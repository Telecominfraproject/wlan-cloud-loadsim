%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 17. December 2020 @ 11:05:46
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_monitor).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_ap_tables.hrl").


 -export ([req_monitor/3,maybe_publish_data/6,publish_unpublished/1,refresh_publications/1]).


-spec req_monitor (NameSpace :: binary(), ToMonitor :: [#{binary()=>term()}], Store :: ets:tid()) -> Result :: #{binary()=>term()}.
req_monitor (NameSpace,[{Table,Operations}|_],Store) ->
	monitor (NameSpace,Table,Operations,Store);
	% case Table of 
	% 	<<"Wifi_Associated_Clients">> ->
	% 		QRes = ovsdb_dba:select_with_key(Table,[],Store),
	% 		timer:apply_after(5000,?MODULE,publish_monitor,[self(),NameSpace,monitor_result(Table,QRes,[])]),
	% 		#{};
	% 	<<"DHCP_leased_IP">> ->
	% 		QRes = ovsdb_dba:select_with_key(Table,[],Store),
	% 		timer:apply_after(5500,?MODULE,publish_monitor,[self(),NameSpace,monitor_result(Table,QRes,[])]),
	% 		#{};
	% 	_ ->
	% 		Ret
	% end;
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
	case should_return_value(initial,Table,Store) of
		true ->
			QRes = ovsdb_dba:select_with_key(Table,[],Store),
			monitor_result(Table,QRes,[]);
			% R = monitor_result(Table,QRes,[]),
			% io:format("MONITOR RESULT:~n~p~n",[R]),
			% R;
		false ->
			M2 = M#monitors{published=false},
			ets:delete_object(Store,M),
			ets:insert(Store, M2),
			#{}
	end.

-spec should_return_value(State :: initial | insert | delete | modify, TableName :: binary(), Store :: ets:tid()) -> boolean().
should_return_value (initial,Table,Store) ->
	length(ets:match_object(Store,#monitors{table=Table, initial=true, _='_'})) =/= 0;
should_return_value (insert,Table,Store) ->
	length(ets:match_object(Store,#monitors{table=Table, initial=true, _='_'})) =/= 0;
should_return_value (delete,Table,Store) ->
	length(ets:match_object(Store,#monitors{table=Table, initial=true, _='_'})) =/= 0;
should_return_value (modify,Table,Store) ->
	length(ets:match_object(Store,#monitors{table=Table, initial=true, _='_'})) =/= 0.

-spec monitor_result (TableName :: binary(), NewRows :: [{Key :: binary(), #{binary()=>any()}}], OldRows :: [{Key :: binary(), #{binary()=>any()}}]) -> #{binary()=>any()}.
monitor_result (_,[],[]) ->
	#{};
monitor_result (T,NewRows,[]) ->
	L = [{K,#{<<"new">>=>M}} || {K,M} <- NewRows],
	#{T => maps:from_list(L)};
monitor_result (T,NewRows,OldRows) ->
	OldMap = maps:from_list(OldRows),
	F = fun ({K,Map}) ->
			case maps:is_key(K,OldMap) of
				true ->
					{K,#{<<"new">>=>Map, <<"old">>=>maps:get(K,OldMap)}};
				false ->
					{K,#{<<"new">>=>Map}}
			end
		end,
	L = [F(X) || X <- NewRows],
	#{T => maps:from_list(L)}.


dump_data(FileName,Data)->
	{ok,IoDev}=file:open(FileName,[append]),
	io:fwrite(IoDev,"~p~n~n~n",[Data]),
	_=file:close(IoDev).

-spec publish_monitor (NameSpace :: binary(), Data :: #{binary()=>term()}) -> ok.
publish_monitor (NameSpace,Data) ->
	RPC = #{
		<<"id">> => null,
		<<"method">> => <<"update">>,
		<<"params">> => [NameSpace,Data]
	},
	Json = iolist_to_binary(jiffy:encode(RPC)),
	case binary:match(NameSpace,<<"DHCP_leased">>) of
		{_,_} ->
			dump_data("dhcp_lease_traces.txt",Json);
		_ ->
			ok
	end,
	io:format("PUBLISHING: ~s~n~s~n",[NameSpace,Json]),
	?L_IA("PUBLISHING: ~s~n~s",[NameSpace,Json]),
	ovsdb_ap:rpc_request(self(),RPC).

-spec maybe_publish_data (NameSpace :: binary(), 
						  Tablename :: binary(), 
						  Operation :: insert | delete | modify,
						  NewRows :: [{Key :: binary(), #{binary()=>any()}}], 
						  OldRows :: [{Key :: binary(), #{binary()=>any()}}],
						  Store :: ets:tid()) -> ok.
maybe_publish_data (NS,T,Op,New,Old,Store) ->
	case should_return_value(Op,T,Store) of
		true ->
			Res = monitor_result(T,New,Old),
			publish_monitor(NS,Res),
			ok;
		false ->
			ok
	end.

-spec publish_unpublished (Store :: ets:tid()) -> ok.
publish_unpublished (Store) ->
	ToPublish = ets:match_object(Store,#monitors{published=false, modify=true, _='_'}),
	%ToPublish = ets:match_object(Store,#monitors{_='_'}),
	%io:format ("UNPUBLISHED:~n~p~n",[ToPublish]),
	F = fun (#monitors{namespace=NS, table=T}=P) when is_binary(T) ->
			QRes = ovsdb_dba:select_with_key(T,[],Store),
			publish_monitor(NS,monitor_result(T,QRes,[])),
			P#monitors{published=true}
		end,
	N = [F(X) || X <- ToPublish],
	[ets:delete_object(Store,X) || X <- ToPublish],
	ets:insert(Store,N).


-spec refresh_publications (Store::ets:tid()) -> ok.
refresh_publications (Store) ->
	ToPublish = ets:match_object(Store,#monitors{modify=true, _='_'}),
	F = fun (#monitors{namespace=NS, table=T}=P) when is_binary(T) ->
			QRes = ovsdb_dba:select_with_key(T,[],Store),
			publish_monitor(NS,monitor_result(T,QRes,[])),
			P#monitors{published=true}
		end,
	[F(X) || X <- ToPublish].




