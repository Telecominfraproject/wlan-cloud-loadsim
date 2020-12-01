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
	?DBGTRC("RPC request: ~s (~s)~n",[<<"echo">>,Id]),
	{ok, make_result(Id,#{})};

eval_req(<<"transact">>,Id,#{<<"params">>:=P},Store) ->
	?DBGTRC("RPC request: ~s (~s)~n",[<<"transact">>,Id]),
	Qr = table_query(P,Store),
	{ok, make_result(Id,Qr)};

eval_req(<<"get_schema">>,Id,_,_Store) ->
	?DBGTRC("RPC request: ~s (~s)~n",[<<"get_schema">>,Id]),
	{ok, ignore};

eval_req (Method, Id, _Data, _Store) ->
	io:format("RPC request: ~s (~s)~n",[Method,Id]),
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
	

table_query ([_,#{<<"table">>:=T, <<"op">>:= <<"update">>, <<"row">>:=C}],S) ->
	#{}.



-spec make_res_rows (Res :: [tuple()], Cols :: [binary()], Acc :: [#{}]) -> [#{}].

make_res_rows ([],_,Acc) ->
	lists:reverse(Acc);

make_res_rows ([H|T],C,Acc) when length(H)==length(C) ->
	M = [{X,Y} || {X,Y} <- lists:zip(C,H), Y=/=undefined],
	R = maps:from_list(M),
	make_res_rows(T,C,[R|Acc]);

make_res_rows ([_|T],C,Acc) ->
	make_res_rows(T,C,Acc).



-spec create_match_spec (TableName :: binary(), Columns :: [binary()], Where :: []) -> [{tuple(),list(),list()}].

create_match_spec (T,[],W) ->	%% an empty list of desired columns means all columns
	create_match_spec(T,rec_fields(T),W);

create_match_spec (T,C,_W) ->
	Fxy = fun(X,true) -> list_to_atom(lists:flatten([$$,integer_to_list(X)])); (_,false) -> list_to_atom("_") end,
	Fields = rec_fields(T),
	MP = [Fxy(I,Y) || {I,X} <- lists:zip(lists:seq(1,length(Fields)),Fields), case lists:member(X,C) of true -> Y=true; _ -> Y=false, true end],
	[{list_to_tuple([binary_to_atom(T)|MP]),[],['$$']}].
	





%%------------------------------------------------------------------------------
%% record convertion helpers


-spec rec_fields (RecordName :: binary()) -> Fieldnames :: [binary()].

rec_fields (<<"AWLAN_Node">>) ->
	[atom_to_binary(X)||X<-record_info(fields,'AWLAN_Node')].






	
