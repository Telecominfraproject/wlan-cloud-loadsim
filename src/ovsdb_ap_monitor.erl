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

-export ([req_monitor/3,maybe_publish_data/6,publish_monitored/1,create_pub_entry/3,create_pub_entry/5]).

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
	% case maps:keys(Data) of
	% 	[<<"DHCP_leased_IP">>] ->
	% 		Pr = iolist_to_binary(jiffy:encode(RPC,[pretty])),
	% 		io:format("PUBLISHING: ~s/~s~n~s~n",[NameSpace,"DHCP",Pr]);   %% Json
	% 	_ ->
	% 		io:format("PUBLISHING: ~s~n~p~n",[NameSpace,Data])   %% Json
	% end,
	?L_IA("PUBLISHING: ~s",[NameSpace]),
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

-spec publish_monitored (Store :: ets:tid()) -> {ok, done} | {ok, more}.
publish_monitored (Store) ->
	case ets:match_object(Store,#to_publish{_='_'},3) of
		{ToPublish,_} ->
			[ should_publish(X,Store) || X <- ToPublish ],
			[ ets:delete_object(Store,X) || X <- ToPublish ],
			{ok, more};
		_ ->
			{ok, done}
	end.

-spec should_publish (Publish :: #to_publish{}, Store :: ets:tid()) -> ok.
should_publish(#to_publish{table=T, row_key=Key, new_values=New, old_values=Old}, Store) ->
	case ets:match_object(Store,#monitors{table=T, _='_'}) of
		[#monitors{namespace=NS, initial=I, modify=M}] when I=:=true orelse M=:=true ->
			NewList = case New of
				ND when is_map(ND) andalso map_size(ND) > 0 ->
					[{Key, ND}];
				_ ->
					[]
			end,
			OldList = case Old of
				OD when is_map(OD) andalso map_size(OD) > 0 ->
					[{Key, OD}];
				_ ->
					[]
			end,
			%io:format("SHOULD_PUB: ~s~nTP=~p~nOld=~p~nNew=~p~nNewMapList=~p~n",[T,TP,Old,New,NewList]);
			publish_monitor(NS,monitor_result(T,NewList,OldList));
		[#monitors{table=T}] ->
			?L_IA("Publishing request for ~s but not monitored, throwing away",[T]);
		_ ->
			?L_EA("publishing error: ~p",[T])
	end.

-spec create_pub_entry (TableName :: binary(), RowKey :: binary(), Store :: ets:tid()) -> ok.
create_pub_entry (Table, Key, Store) ->
	case ovsdb_dba:select_with_key(Table,[[<<"**key_id**">>,<<"=">>,Key]],Store) of	
		[{Key,Data}] ->
			[<<"uuid">>,Version] = maps:get(<<"_version">>,Data,[<<"uuid">>,utils:uuid_b()]),
			ets:insert(Store,#to_publish{
				table = Table,
				new_version = Version,
				row_key = Key,
				new_values = Data
			});
		_ ->
			?L_E("CBE error")
	end.

-spec create_pub_entry (TableName :: binary(), RowKey :: binary(), OldValue :: #{binary()=>any()}, NewValue :: #{binary()=>any()}, Store :: ets:tid()) -> ok.
create_pub_entry (Table, Key, Old, New, Store) ->
	[<<"uuid">>,OldVer] = maps:get(<<"_version">>,Old,[<<"uuid">>,utils:uuid_b()]),
	[<<"uuid">>,NewVer] = maps:get(<<"_version">>,New,[<<"uuid">>,utils:uuid_b()]),
	ets:insert(Store,#to_publish{
		table = Table,
		new_version = NewVer,
		old_version = OldVer,
		row_key = Key,
		new_values = New,
		old_values = Old
	}).




-spec dump_data(FileName::string(),Data::binary())->ok.
dump_data(FileName,Data)->
	case filelib:is_file("keep-dhcp-traces") of
		true ->
			{ok,IoDev}=file:open(FileName,[append]),
			io:fwrite(IoDev,"~s~n~n~n",[Data]),
			_=file:close(IoDev),
			ok;
		false ->
			ok
	end.





