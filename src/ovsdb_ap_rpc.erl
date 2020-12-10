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

-export ([eval_req/4,eval_resp/4]).


%%------------------------------------------------------------------------------
%% request handling

-spec eval_req (Method :: binary(), Id :: binary(), Data :: map(), Store :: ets:tid()) -> {ok, ignore} | {ok, Result :: map()} | generic_error().
eval_req(<<"echo">>, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"echo">>,Id])),
	{ok, make_result(Id,#{})};

eval_req(<<"transact">>,Id,#{<<"params">>:=P},Store) ->
	[DB,#{<<"table">>:=T, <<"op">>:=OP}] = P,
	?L_I(?DBGSTR("RPC request: ~s (~s) => DB: ~s, table: ~s, operation: ~s",[<<"transact">>,Id,DB,T,OP])),
	Qr = table_query(P,Store),
	{ok, make_result(Id,Qr)};

eval_req(<<"get_schema">>, Id,_,Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"get_schema">>,Id])),
	Schema = read_schema(Store),
	{ok, make_result(Id,Schema)};

eval_req (Method, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[Method,Id])),
	{error,io_lib:format("~s not recognized",[Method])}.

-spec make_result (Id :: binary(), Result :: map() | binary()) -> map().
make_result (Id, <<>>) ->
	iolist_to_binary(io_lib:format("{\"result\":null,\"id\":\"~s\",\"error\":\"internal error\"}",[Id]));
make_result (Id, Res) when is_binary(Res) ->
	iolist_to_binary(io_lib:format("{\"result\":~s,\"id\":\"~s\",\"error\":null}",[Res,Id]));
make_result (Id, Res) when map_size(Res) == 0 ->
	#{
		<<"result">> => [],
		<<"id">> => Id,
		<<"error">> => null
	};
make_result (Id, Res) when is_map_key(error,Res) ->
	#{
		<<"result">> => null,
		<<"id">> => Id,
		<<"error">> => maps:get(error,Res)
	};
make_result (Id, Res) ->
	#{
		<<"result">> => [Res],
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
%% handling OVSDB tables

-spec table_query (P :: list(), Store :: ets:tid()) -> map().

table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"select">>, <<"columns">>:=C, <<"where">>:=W}],S) ->
	Res = ets:select(S,create_match_spec(T,W)),
	#{ <<"rows">> => make_res_rows(T,Res,C,[])};

table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"delete">>, <<"where">>:=W}],S) ->
	M = create_match_spec(T,W),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	#{ <<"count">> => D};
	
table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"update">>, <<"row">>:=C, <<"where">>:=W}],S) ->
	M = create_match_spec(T,W),
	Res = ets:select(S,M),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	ets:insert(S,update_records(T,C,Res,[])),
	check_update_actions(C),
	#{<<"count">> => D}.
	

%--------check_update_actions/1----------special handling for some updates that need to trigger actions

-spec check_update_actions (Updates :: #{binary()=>any()}) -> ok.
check_update_actions (#{<<"manager_addr">>:=_}) ->
	timer:apply_after(250,ovsdb_ap,reset_comm,[self()]);   % delay to give a chance to empty buffer before we close socket
check_update_actions (#{<<"mqtt_settings">>:=[<<"map">>,MQTT]}) ->
	Map = maps:from_list([{K,V}||[K,V]<-MQTT]),
	ovsdb_ap:mqtt_conf(self(),Map).

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

%--------update_records/4-----------------create an updated record to eb inserted into ETS from RCP call

-spec update_records (TableName :: binary(), NewValues :: #{binary():=any()}, Records :: [[any()]], Acc :: [[any()]]) -> [tuple()].
update_records (T,_,[],Acc) ->
	[list_to_tuple([binary_to_atom(T)|X]) || X <- Acc];

update_records (T,V,[R|Rest],Acc)  ->
	Cand = lists:zip(rec_fields(T),R),
	Updt = [Uv || {F,Ov} <- Cand, case maps:is_key(F,V) of true -> Uv=maps:get(F,V), true; _ -> Uv=Ov, true end],
	update_records(T,V,Rest,[Updt|Acc]).

%--------read_schema/1-------------------reads the schema from disk for a particular device type
-spec read_schema(Store :: ets:tid()) -> binary().
read_schema (Store) ->
	[#'AWLAN_Node'{model=Model}|_] = ets:match_object(Store,#'AWLAN_Node'{_='_'}),
	FName = filename:join([code:priv_dir(?OWLS_APP),"templates",iolist_to_binary([string:uppercase(Model),"_schema.json"])]),
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
rec_fields (<<"Wifi_Radio_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_State')];
rec_fields (<<"Wifi_Inet_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_State')];
rec_fields (<<"AWLAN_Node">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'AWLAN_Node')].
