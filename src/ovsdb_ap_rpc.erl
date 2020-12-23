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
	?L_IA("MONITOR REQUEST: (~s) for AP ~s",[NSpace,Id]),
	Mon = ovsdb_ap_monitor:req_monitor(NSpace,maps:to_list(hd(Tables)),Store),
	Res = make_result(Id,Mon),
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
			?L_IA("handle response to RPC wit ID: ~s",[Id]),
			ok
	end.



%%------------------------------------------------------------------------------
%% transaction handling

-spec run_transactions (Id :: binary(), Transactions :: [#{binary() => term()}], Store :: ets:tid(), Acc :: [#{binary()=>any()}]) -> [#{binary()=>any()}].
run_transactions (_,[],_,Acc) ->
	lists:reverse(Acc);
run_transactions (Id,[Trans|More],Store,Acc) when is_map(Trans) ->
	#{<<"table">>:=T, <<"op">>:=OP} = Trans,
	?L_I(?DBGSTR("=> table: ~s, operation: ~s",[T,OP])),
	Qr = table_query(Trans,Store),
	run_transactions (Id,More,Store,[Qr|Acc]).


%%------------------------------------------------------------------------------
%% handling OVSDB tables

-spec table_query (P :: map(), Store :: ets:tid()) -> map().
table_query (#{<<"table">>:=T, <<"op">>:= <<"select">>, <<"columns">>:=C, <<"where">>:=W},S) ->
	Res = ovsdb_dba:select(T,W,S),
	#{ <<"rows">> => make_res_rows(Res,C,[])};
table_query (#{<<"table">>:=T, <<"op">>:= <<"select">>, <<"where">>:=W},S) ->
	Res = ovsdb_dba:select(T,W,S),
	#{ <<"rows">> => make_res_rows(Res,all,[])};

table_query (#{<<"table">>:=T, <<"op">>:= <<"delete">>, <<"where">>:=W},S) ->
	D = ovsdb_dba:delete(T,W,S),
	#{ <<"count">> => D};

table_query (#{<<"table">>:=T, <<"op">>:= <<"insert">>, <<"row">>:=R},S) ->
	UUIDs = ovsdb_dba:insert(T,R,S),
	#{uuid=>[uuid|UUIDs]};

table_query (#{<<"table">>:=T, <<"op">>:= <<"mutate">>, <<"mutations">>:=Mut, <<"where">>:=W},S) ->
	D = ovsdb_dba:mutate_table(T,Mut,W,S),
	#{<<"count">> => D};

table_query (#{<<"table">>:=T, <<"op">>:= <<"update">>, <<"row">>:=R, <<"where">>:=W},S) ->
	D = ovsdb_dba:update(T,R,W,S),
	check_update_actions(R),
	#{<<"count">> => D}.
	

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
-spec make_res_rows (Res :: [#{binary()=>any()}], Cols :: all | [binary()], Acc :: [#{binary()=>any()}]) -> [#{binary()=>any()}].
make_res_rows (Res,all,_) ->
	Res;
make_res_rows ([],_,Acc) ->
	lists:reverse(Acc);
make_res_rows ([H|T],C,Acc) ->
	M =  maps:from_list([{F,V}|| {F,V}<-maps:to_list(H), lists:member(F,C)]),
	make_res_rows(T,C,[M|Acc]).

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
