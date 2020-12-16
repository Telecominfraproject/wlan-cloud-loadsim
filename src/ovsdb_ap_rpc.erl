%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 29. November 2020 @ 16:01:20
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_rpc).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_ap_tables.hrl").

-export ([eval_req/4,eval_resp/4,publish_monitor/3]).


%%------------------------------------------------------------------------------
%% request handling

-spec eval_req (Method :: binary(), Id :: binary(), Data :: map(), Store :: ets:tid()) -> {ok, ignore} | {ok, Result :: map() | binary()} | generic_error().
eval_req(<<"echo">>, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"echo">>,Id])),
	{ok, make_result(Id,[])};

eval_req(<<"transact">>,Id,#{<<"params">>:=[<<"Open_vSwitch">>|Trans]},Store) ->
	Res = run_transactions(Id,Trans,Store,[]),
	{ok, make_result(Id,Res)};
eval_req(<<"transact">>,Id,#{<<"params">>:=_P},_Store) ->
	{ok, make_result(Id,<<>>)};

eval_req(<<"monitor">>,Id,#{<<"params">>:=[<<"Open_vSwitch">>,NSpace|Tables]},Store) when length(Tables)==1 ->	
	Mon = req_monitor(NSpace,maps:to_list(hd(Tables)),Store),
	Res = make_result(Id,Mon),
	% Json = iolist_to_binary(jiffy:encode(Res,[pretty])),
	% io:format("MONITOR REQUEST (~s):~n~s~n",[NSpace,Json]),
	io:format("MONITOR REQUEST (~s):~n",[NSpace]),
	{ok, Res};
eval_req(<<"monitor">>,Id,P,_) ->
	?L_EA("unrecognized monitor request: ~p",[P]),
	{ok, make_result(Id,#{})};


eval_req(<<"get_schema">>, Id,_,Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"get_schema">>,Id])),
	Schema = read_schema(Store),
	{ok, make_result(Id,Schema)};

eval_req (Method, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[Method,Id])),
	{error,io_lib:format("~s not recognized",[Method])}.

-spec make_result (Id :: binary(), Result :: map() | list() | binary() | null) -> map() | binary().
make_result (Id, <<>>) ->
	iolist_to_binary(io_lib:format("{\"result\":null,\"id\":\"~s\",\"error\":\"internal error\"}",[Id]));
make_result (Id, Res) when is_binary(Res) ->
	iolist_to_binary(io_lib:format("{\"result\":~s,\"id\":\"~s\",\"error\":null}",[Res,Id]));
make_result (Id, Res)  ->
	#{
		<<"result">> => Res,
		<<"id">> => Id,
		<<"error">> => null
	}.

%%------------------------------------------------------------------------------
%% response handling

-spec eval_resp (Id :: binary(), Data :: map(), Queue :: ets:tid(), Store :: ets:tid()) -> ok | generic_error().
eval_resp (Id, _Data, Queue, _Store) ->
	case ets:lookup(Queue,Id) of
		[] ->
			?L_E(?DBGSTR("Result with no corresponding request ~s",[Id])),
			{error,?DBGSTR("Can't find ID ~s",[Id])};
		_ ->
			io:format("handle response to RPC wit ID: ~s~n",[Id]),
			ok
	end.



%%------------------------------------------------------------------------------
%% transaction and monitor handling

-spec run_transactions (Id :: binary(), Transactions :: [#{binary() => term()}], Store :: ets:tid(), Acc :: [#{binary()=>any()}]) -> [#{binary()=>any()}].
run_transactions (_,[],_,Acc) ->
	lists:reverse(Acc);
run_transactions (Id,[Trans|More],Store,Acc) when is_map(Trans) ->
	#{<<"table">>:=T, <<"op">>:=OP} = Trans,
	?L_I(?DBGSTR("=> table: ~s, operation: ~s",[T,OP])),
	Qr = table_query(Trans,Store),
	run_transactions (Id,More,Store,[Qr|Acc]).

-spec req_monitor (NameSpace :: binary(), ToMonitor :: [#{binary()=>term()}], Store :: ets:tid()) -> Result :: #{binary()=>term()}.
req_monitor (NameSpace,[{Table,Operations}|_],Store) ->
	{M,Ret} = monitor (NameSpace,Table,Operations,Store),
	case Table of 
		<<"Wifi_Associated_Clients">> ->
			Res = monitor_result(modify,M,Store),
			timer:apply_after(5000,?MODULE,publish_monitor,[self(),NameSpace,Res]),
			#{};
		<<"DHCP_leased_IP">> ->
			Res = monitor_result(modify,M,Store),
			timer:apply_after(5500,?MODULE,publish_monitor,[self(),NameSpace,Res]),
			#{};
		_ ->
			Ret
	end;
req_monitor (NameSpace,OPS,_) ->
	?L_EA("Monitor request for namespace '~s' with unsupported operatiosn format ~p",[NameSpace,OPS]),
	#{}.

-spec monitor (NameSpace :: binary(), Table :: binary(), Operations :: #{binary()=>term()}, Store :: ets:tid()) -> Result :: #{binary()=>term()}.
monitor (NameSpace, Table, Operations, Store) ->
	Sel = maps:get(<<"select">>,Operations,#{<<"modify">>=>true}),
	M = #monitors{
		namespace = NameSpace,
		table = Table,
		initial = maps:get(<<"initial">>,Sel,false),
		insert = maps:get(<<"insert">>,Sel,false),
		delete = maps:get(<<"delete">>,Sel,false),
		modify = maps:get(<<"modify">>,Sel,false)
	},
	ets:insert(Store, M),
	{M,monitor_result(initial,M,Store)}.

-spec monitor_result (State :: initial | insert | delete | modify, Monitor :: #monitors{}, Store :: ets:tid()) -> Result :: #{binary()=>term()}.
monitor_result (initial,#monitors{table=T, initial=true}, Store) ->
	monitor_table_query(T, new, Store);
monitor_result (insert,#monitors{table=T, insert=true}, Store) ->
	monitor_table_query(T, new, Store);
monitor_result (delete,#monitors{table=T, delete=true}, Store) ->
	monitor_table_query(T, old, Store);
monitor_result (modify,#monitors{table=T, modify=true}, Store) ->
	monitor_table_query(T, both, Store);
monitor_result (_,_,_) ->
	#{}.

-spec monitor_table_query (Table :: binary(), Which :: new | old | both,Store :: ets:tid()) -> Result :: #{binary()=>term()}.
monitor_table_query (Table, Which, Store) ->
	Fields = rec_fields(Table),
	F = fun(X) ->
		M = maps:from_list(lists:zip(Fields,X)),			
		{KeyId,M2} = case Table of
						<<"Wifi_Associated_Clients">> ->
							{KeyId2,Mt} = maps:take(<<"key_id">>,M),
							{KeyId2,Mt#{<<"key_id">>=><<"">>}};
	 					_ ->
							maps:take(<<"key_id">>,M)
					end,
		case Which of
			new -> {KeyId,#{<<"new">>=>M2}};
			old -> {KeyId,#{<<"old">>=>M2}};
			both -> {KeyId,#{<<"new">>=>M2}}  %, <<"old">>=>#{}}}
		end
	end,
	case ets:select(Store,create_match_spec(Table,[])) of		
		[] ->
			#{};
		Res ->
			#{Table=>maps:from_list([F(X) || X <- Res])}
	end.


-spec publish_monitor (AP :: pid(), NameSpace :: binary(), Data :: #{binary()=>term()}) -> ok.
publish_monitor (AP,NameSpace,Data) ->
	RPC = #{
		<<"id">> => null,
		<<"method">> => <<"update">>,
		<<"params">> => [NameSpace,Data]
	},
	Json = iolist_to_binary(jiffy:encode(RPC)),
	io:format("PUBLISHING: ~s~n~s~n",[NameSpace,Json]),
	?L_IA("PUBLISHING: ~s",[NameSpace]),
	ovsdb_ap:rpc_request(AP,RPC).


%%------------------------------------------------------------------------------
%% handling OVSDB tables

-spec table_query (P :: map(), Store :: ets:tid()) -> map().

table_query (#{<<"table">>:=T, <<"op">>:= <<"select">>, <<"columns">>:=C, <<"where">>:=W},S) ->
	Res = ets:select(S,create_match_spec(T,W)),
	#{ <<"rows">> => make_res_rows(T,Res,C,[])};
table_query (#{<<"table">>:=T, <<"op">>:= <<"select">>, <<"where">>:=W},S) ->
	Res = ets:select(S,create_match_spec(T,W)),
	[_|Cols] = rec_fields(T),
	#{ <<"rows">> => make_res_rows(T,Res,Cols,[])};

table_query (#{<<"table">>:=T, <<"op">>:= <<"delete">>, <<"where">>:=W},S) ->
	M = create_match_spec(T,W),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	#{ <<"count">> => D};

table_query (#{<<"table">>:=T, <<"op">>:= <<"insert">>, <<"row">>:=R},S) ->
	Fields = rec_fields(T),
	UUID = utils:uuid_b(),
	Default = maps:from_list(lists:zip(Fields,tl(tuple_to_list(default_record(T))))),
	Rwi = maps:merge(Default,R#{<<"key_id">>=>utils:uuid_b(),<<"_uuid">>=>[<<"uuid">>,UUID]}),
	Rec = list_to_tuple([binary_to_atom(T)|[maps:get(X,Rwi,<<"###WILL_CRASH###">>) || X<-Fields]]),
	% case T of
	% 	<<"Wifi_VIF_Config">> ->
	% 		io:format("INSERT VIF:~nInput=~p~nResult=~p~n",[R,Rec]);
	% 	_ ->
	% 		ok
	% end,
	ets:insert(S,Rec),
	#{uuid=>[uuid,UUID]};

table_query (#{<<"table">>:=T, <<"op">>:= <<"mutate">>, <<"mutations">>:=Mut, <<"where">>:=W},S) ->
	D = mutate_table(T,Mut,W,S),
	#{<<"count">> => D};

table_query (#{<<"table">>:=T, <<"op">>:= <<"update">>, <<"row">>:=R, <<"where">>:=W},S) ->
	M = create_match_spec(T,W),
	Res = ets:select(S,M),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	Upd = update_records(T,R,Res,[]),
	ets:insert(S,Upd),
	check_update_actions(R),
	#{<<"count">> => D}.
	
%--------mutate_table/4------------------hndle mutations to fields in a table row (or rows)

-spec mutate_table (Table :: binary(), Mutations :: [[any()]], Where :: [any()], Store :: ets:tid()) -> RowsAffected :: integer().
mutate_table (Table,Mut,Where,Store) ->
	mutate_table_a (Table,Mut,Where,Store,0).

-spec mutate_table_a (Table :: binary(), Mutations :: [[any()]], Where :: [any()], Store :: ets:tid(), Acc :: integer()) -> RowsAffected :: integer().
mutate_table_a (_,[],_,_,A) ->
	A;
mutate_table_a (Table,[Mut|Tail],Where,Store,A) ->
	MSpec = create_match_spec(Table,Where),
	[ToMutate] = ets:select(Store,MSpec),
	Mutated = apply_mutations(Mut, Table, ToMutate),
	%io:format("MUTATION: MUT=~p~nToMutate=~p~n,Mutated=~p~n",[Mut,ToMutate,Mutated]),
	D = ets:select_delete(Store,[setelement(3,hd(MSpec),[true])]),
	ets:insert(Store,list_to_tuple([binary_to_atom(Table)|Mutated])),
	mutate_table_a (Table,Tail,Where,Store,A+D).

-spec apply_mutations (Mutations :: [], Table :: binary(), ToMutate :: tuple()) -> MutatedRecord :: tuple().
apply_mutations ([Field,<<"insert">>,What],Table,Record) ->
	RecMap = lists:zip(rec_fields(Table),Record),
	case proplists:get_value(Field,RecMap) of
		undefined ->
			Record;
		[T,L] ->
			F = fun (X,_) when X=:=Field -> [T,[What|L]]; (_,V) -> V end,
			[F(Key,Val) || {Key,Val}<-RecMap]
	end.







%--------check_update_actions/1----------special handling for some updates that need to trigger actions

-spec check_update_actions (Updates :: #{binary()=>any()}) -> ok.
check_update_actions (#{<<"manager_addr">>:=_}) ->
	timer:apply_after(250,ovsdb_ap,reset_comm,[self()]);   % delay to give a chance to empty buffer before we close socket
check_update_actions (#{<<"mqtt_settings">>:=[<<"map">>,MQTT]}) ->
	Map = maps:from_list([{K,V}||[K,V]<-MQTT]),
	ovsdb_ap:mqtt_conf(self(),Map);
check_update_actions (_) ->
	ok.

%--------make_res_rows/3-----------------formats query results into proper rows map
-spec make_res_rows (Record :: binary(), Res :: [[{binary(),any()}]], Cols :: [binary()], Acc :: [#{}]) -> [#{}].
make_res_rows (R,Res,[],Acc) ->
	make_res_rows (R,Res,rec_fields(R),Acc);
make_res_rows (_,[],_,Acc) ->
	lists:reverse(Acc);
make_res_rows (R,[H|T],C,Acc) ->
	M =  maps:from_list([{F,V}|| {F,V}<-lists:zip(rec_fields(R),H), lists:member(F,C)]),
	make_res_rows(R,T,C,[M|Acc]).

%--------create_match_spec/3-------------creates proper match specification from RPC command for ETS search
-spec create_match_spec (TableName :: binary(), Where :: []) -> [{tuple(),list(),list()}].
create_match_spec (R,W) ->
	Op = #{<<"==">>=>'==', <<"!=">>=>'/=', <<"<=">>=>'<=', <<"<">>=>'<', <<">=">>=>'>=', <<">">>=>'>'},
	Fields = rec_fields(R),
	MP = [binary_to_atom(list_to_binary([$$,integer_to_list(X)])) || X<-lists:seq(1,length(Fields))],
	C = [{maps:get(O,Op,'=='),field_idx(A1,Fields,1),field_idx(A2,Fields,1)}|| [A1,O,A2] <- W],
	[{list_to_tuple([binary_to_atom(R)|MP]),C,['$$']}].

field_idx (F,[],_) -> F;
field_idx (F,[F|_],N) -> binary_to_atom(list_to_binary([$$,integer_to_list(N)]));
field_idx (F,[_|T],N) -> field_idx(F,T,N+1).

%--------update_records/4-----------------create an updated record to be inserted into ETS from RCP call
-spec update_records (TableName :: binary(), NewValues :: #{binary():=any()}, Records :: [[any()]], Acc :: [[any()]]) -> [tuple()].
update_records (_,_,[],Acc) ->
	Acc;
update_records (T,V,[R|Rest],Acc)  ->
	Fields = rec_fields(T),
	Default = maps:from_list(lists:zip(Fields,tl(tuple_to_list(default_record(T))))),
	Cand = maps:from_list(lists:zip(Fields,R)),
	DefCand = maps:merge(Default,Cand),
	ResMap = maps:merge(DefCand,V),
	Rec = list_to_tuple([binary_to_atom(T)|[maps:get(X,ResMap,<<"###WILL_CRASH###">>) || X<-Fields]]),
	update_records(T,V,Rest,[Rec|Acc]).

%--------read_schema/1-------------------reads the schema from disk for a particular device type
-spec read_schema(Store :: ets:tid()) -> binary().
read_schema (Store) ->
	[#'AWLAN_Node'{model=Model}|_] = ets:match_object(Store,#'AWLAN_Node'{_='_'}),
	FName = filename:join([utils:priv_dir(),"templates",iolist_to_binary([string:uppercase(Model),"_schema.json"])]),
	case filelib:is_regular(FName) of
		true ->
			{ok, Schema} = file:read_file(FName),
			Schema;
		false ->
			?L_EA("Cannot read schmea file for model: ~s",[Model]),
			<<>>
	end.


%%------------------------------------------------------------------------------
%% record convertion helpers
-spec rec_fields (RecordName :: binary()) -> Fieldnames :: [binary()].
rec_fields (<<"Wifi_Inet_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_Config')];
rec_fields (<<"Wifi_Radio_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_Config')];
rec_fields (<<"Wifi_VIF_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_VIF_Config')];
rec_fields (<<"Wifi_VIF_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_VIF_State')];
rec_fields (<<"Wifi_Associated_Clients">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Associated_Clients')];
rec_fields (<<"Command_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Command_State')];
rec_fields (<<"DHCP_leased_IP">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'DHCP_leased_IP')];
rec_fields (<<"Wifi_RRM_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_RRM_Config')];
rec_fields (<<"Hotspot20_Icon_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Hotspot20_Icon_Config')];
rec_fields (<<"Hotspot20_OSU_Providers">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Hotspot20_OSU_Providers')];
rec_fields (<<"Hotspot20_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Hotspot20_Config')];
rec_fields (<<"Wifi_Stats_Config">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Stats_Config')];
rec_fields (<<"Wifi_Radio_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_State')];
rec_fields (<<"Wifi_Inet_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_State')];
rec_fields (<<"AWLAN_Node">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'AWLAN_Node')].

-spec default_record(RecordName::binary) -> Record :: tuple().
default_record (<<"Wifi_Inet_Config">>) ->
	#'Wifi_Inet_Config'{};
default_record (<<"Wifi_Radio_Config">>) ->
	#'Wifi_Radio_Config'{};
default_record (<<"Wifi_VIF_Config">>) ->
	#'Wifi_VIF_Config'{};
default_record (<<"Wifi_VIF_State">>) ->
	#'Wifi_VIF_State'{};
default_record (<<"Wifi_Associated_Clients">>) ->
	#'Wifi_Associated_Clients'{};
default_record (<<"Command_State">>) ->
	#'Command_State'{};
default_record (<<"DHCP_leased_IP">>) ->
	#'DHCP_leased_IP'{};
default_record (<<"Wifi_RRM_Config">>) ->
	#'Wifi_RRM_Config'{};
default_record (<<"Hotspot20_Icon_Config">>) ->
	#'Hotspot20_Icon_Config'{};
default_record (<<"Hotspot20_OSU_Providers">>) ->
	#'Hotspot20_OSU_Providers'{};
default_record (<<"Hotspot20_Config">>) ->
	#'Hotspot20_Config'{};
default_record (<<"Wifi_Stats_Config">>) ->
	#'Wifi_Stats_Config'{};
default_record (<<"Wifi_Radio_State">>) ->
	#'Wifi_Radio_State'{};
default_record (<<"Wifi_Inet_State">>) ->
	#'Wifi_Inet_State'{};
default_record (<<"AWLAN_Node">>) ->
	#'AWLAN_Node'{}.
