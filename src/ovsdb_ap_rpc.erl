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
-include("ovsdb_ap_tables.hrl").

-export ([eval_req/4,eval_resp/4]).



%%------------------------------------------------------------------------------
%% request handling

-spec eval_req (Method, Id, Data, Store) -> {ok, ignore} | {ok, Result} | {error, Reason} when
		Method :: binary(),
		Id :: binary(),
		Data :: map(),
		Store :: ets:tid(),
		Result :: map(),
		Reason :: term().

eval_req(<<"echo">>, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"echo">>,Id])),
	{ok, make_result(Id,#{})};

eval_req(<<"transact">>,Id,#{<<"params">>:=P},Store) ->
	[DB,#{<<"table">>:=T, <<"op">>:=OP}] = P,
	?L_I(?DBGSTR("RPC request: ~s (~s) => DB: ~s, table: ~s, operation: ~s",[<<"transact">>,Id,DB,T,OP])),
	Qr = table_query(P,Store),
	{ok, make_result(Id,Qr)};

eval_req(<<"get_schema">>, Id,_,_Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[<<"get_schema">>,Id])),
	{ok, ignore};

eval_req (Method, Id, _Data, _Store) ->
	?L_I(?DBGSTR("RPC request: ~s (~s)",[Method,Id])),
	{error,io_lib:format("~s not recognized",[Method])}.



-spec make_result (Id :: binary(), Result :: map()) -> map().

make_result (Id, Res) when map_size(Res) == 0 ->
	#{
		<<"result">> => [],
		<<"id">> => Id,
		<<"error">> => null
	};

make_result (Id, Res) ->
	#{
		<<"result">> => [Res],
		<<"id">> => Id,
		<<"error">> => null
	}.


%%------------------------------------------------------------------------------
%% response handling

-spec eval_resp (Id, Data, Queue, Store) -> ok | {error, Reason} when
		Id :: binary(),
		Data :: map(),
		Queue :: ets:tid(),
		Store :: ets:tid(),
		Reason :: term().

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
	Res = ets:select(S,create_match_spec(T,C,W)),
	#{ <<"rows">> => make_res_rows(Res,C,[])};

table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"delete">>, <<"where">>:=W}],S) ->
	M = create_match_spec(T,[],W),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	#{ <<"count">> => D};
	
table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"update">>, <<"row">>:=C, <<"where">>:=W}],S) ->
	M = create_match_spec(T,[],W),
	Res = ets:select(S,M),
	D = ets:select_delete(S,[setelement(3,hd(M),[true])]),
	ets:insert(S,update_records(T,C,Res,[])),
	case network_update(C) of
		true ->
			ovsdb_ap:reset_comm(self()),
			#{<<"count">> => D};
		_ ->
			#{<<"count">> => D}
	end.
	


%--------network_update/1----------------special handling if any network address hase changed

-spec network_update (Updates :: #{binary()=>any()}) -> true | false.

network_update (#{<<"manager_addr">>:=_}) -> true;
network_update (#{<<"redirector_addr">>:=_}) -> true;
network_update (_) -> false.



%--------make_res_rows/3-----------------formats query results into proper rows map

-spec make_res_rows (Res :: [[any()]], Cols :: [binary()], Acc :: [#{}]) -> [#{}].

make_res_rows ([],_,Acc) ->
	lists:reverse(Acc);

make_res_rows ([H|T],C,Acc) when length(H)==length(C) ->
	M = [{X,Y} || {X,Y} <- lists:zip(C,H), Y=/=undefined],
	R = maps:from_list(M),
	make_res_rows(T,C,[R|Acc]);

make_res_rows ([_|T],C,Acc) ->
	make_res_rows(T,C,Acc).



%--------create_match_spec/3-------------creates proper match specification from RPC command for ETS search

-spec create_match_spec (TableName :: binary(), Columns :: [binary()], Where :: []) -> [{tuple(),list(),list()}].

create_match_spec (T,[],W) ->	%% an empty list of desired columns means all columns
	create_match_spec(T,rec_fields(T),W);

create_match_spec (T,C,W) ->
	Fxy = fun(X,true) -> list_to_atom(lists:flatten([$$,integer_to_list(X)])); (_,false) -> list_to_atom("_") end,
	Fields = rec_fields(T),
	MP = [Fxy(I,Y) || {I,X} <- lists:zip(lists:seq(1,length(Fields)),Fields), case lists:member(X,C) of true -> Y=true; _ -> Y=false, true end],
	Clauses = [make_match_clause(T,list_to_tuple(X))||X<-W],
	[{list_to_tuple([binary_to_atom(T)|MP]),Clauses,['$$']}].
	


-spec make_match_clause (TableName :: binary(), {Arg1 :: binary(), Op :: binary(), Arg2 :: binary()}) -> {atom(),any(),any()}.

make_match_clause (T,{A1,Op,A2}) ->
	Fields = rec_fields(T),
	FieldMap = lists:zip(lists:seq(1,length(Fields)),Fields),
	OpMap = #{<<"==">>=>'==', <<"!=">>=>'/=', <<"<=">>=>'<=', <<"<">>=>'<', <<">=">>=>'>=', <<">">>=>'>'},
	{maps:get(Op,OpMap,'=='),mk_fp(A1,FieldMap),mk_fp(A2,FieldMap)}.


mk_fp (Val,IndexFields) ->
	case lists:keyfind(Val,2,IndexFields) of
		false -> Val;
		{N,_} -> list_to_atom(lists:flatten([$$,integer_to_list(N)]))
	end.



%--------update_records/4-----------------create an updated record to eb inserted into ETS from RCP call

-spec update_records (TableName :: binary(), NewValues :: #{binary():=any()}, Records :: [[any()]], Acc :: [[any()]]) -> [tuple()].

update_records (T,_,[],Acc) ->
	[list_to_tuple([binary_to_atom(T)|X]) || X <- Acc];

update_records (T,V,[R|Rest],Acc)  ->
	Cand = lists:zip(rec_fields(T),R),
	Updt = [Uv || {F,Ov} <- Cand, case maps:is_key(F,V) of true -> Uv=maps:get(F,V), true; _ -> Uv=Ov, true end],
	update_records(T,V,Rest,[Updt|Acc]).



%%------------------------------------------------------------------------------
%% record convertion helpers


-spec rec_fields (RecordName :: binary()) -> Fieldnames :: [binary()].

rec_fields (<<"Wifi_Radio_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_State')];

rec_fields (<<"Wifi_Inet_State">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_State')];

rec_fields (<<"AWLAN_Node">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'AWLAN_Node')].



