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
-include("../include/ovsdb_definitions.hrl").

-export ([req_monitor/3,maybe_publish_data/6,publish_monitored/1,create_pub_entry/3,create_pub_entry/5]).

-export([make_table_and_publish/2]).

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

%skip_table(NameSpace) ->
%	TablesToSkip = [ <<"Wifi_Associated_Clients">> ],
%	lists:foldl(fun(E,A) ->
%								case binary:match(NameSpace,E) of
%									{_,_} -> A+1;
%											_ -> A
%								end
%	            end, 0,TablesToSkip) > 0.

-spec publish_monitor (NameSpace :: binary(), Data :: #{binary()=>term()}) -> ok.
publish_monitor (NameSpace,Data) ->
	case binary:match(NameSpace,<<"Wifi_Associated_Clients">>) of
		nomatch ->
			io:format("PUBLISHING: ~p.~n",[NameSpace]),
			TableUpdate = #{
				<<"id">> => null,
				<<"method">> => <<"update">>,
				<<"params">> => [NameSpace,Data]
			},
			ovsdb_ap:publish(TableUpdate);
		_ ->
			io:format("Skipping table.~n")
	end,
	ok.

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
%%			io:format(">>publish_monitored: more~n"),
			{ok, more};
		_ ->
%%			io:format(">>publish_monitored: done~n"),
			{ok, done}
	end.

-spec should_publish (Publish :: #to_publish{}, Store :: ets:tid()) -> ok.
should_publish(#to_publish{table=T, row_key=Key, new_values=New, old_values=Old}, Store) ->
%%	io:format(">>>should_publish: more~n"),
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
%%			io:format("SHOULD_PUB: ~s~nTP=~p~nOld=~p~nNew=~p~nNewMapList=~p~n",[T,NS,Old,New,NewList]),
			publish_monitor(NS,monitor_result(T,NewList,OldList));
		[#monitors{table=T}] ->
			io:format("Publishing request for ~s but not monitored, throwing away.~n",[T]);
		_ ->
			io:format("Publishing error: ~p.~n",[T])
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

-spec create_pub_entry(TableName::binary(),RowKey::binary(),OldValue::#{binary()=>any()},NewValue::#{binary()=>any()},Store::ets:tid()) -> ok.
create_pub_entry(Table,RowKey,Old,New,Store) ->
	[<<"uuid">>,OldVer] = maps:get(<<"_version">>,Old,[<<"uuid">>,utils:uuid_b()]),
	[<<"uuid">>,NewVer] = maps:get(<<"_version">>,New,[<<"uuid">>,utils:uuid_b()]),
	ets:insert(Store,#to_publish{
		table = Table,
		new_version = NewVer,
		old_version = OldVer,
		row_key = RowKey,
		new_values = New,
		old_values = Old
	}), ok.

make_a_mac(Mac)->
	#{<<"new">> =>
	  #{ <<"version">> => [<<"uuid">>,utils:uuid_b()], <<"capabilities">> =>
	  [<<"set">>,[<<"11ac">>,<<"11n">>,<<"11ab">>]],
	     <<"kick">> => [<<"map">>,[]], <<"mac">> => Mac, <<"oftag">> => [<<"set">>,[]],
	     <<"state">> => <<"active">>,
	     <<"uapsd">> => [<<"set">>,[]]}}.

make_macs([],Acc)->
	Acc;
make_macs([H|T],Acc)->
	make_macs(T, maps:put(utils:uuid_b(),make_a_mac(H),Acc)).

-spec make_table_and_publish(Name::binary(),APS::ap_state()) -> ok.
make_table_and_publish(<<"Wifi_Associated_Clients">>,APS)->
	%% io:format("Publishing my own Wifi_Associated_Clients~n"),
	Name = binary:list_to_bin([<<"Wifi_Associated_Clients_Open_AP_">>, APS#ap_state.id]),
	WiFiClients = [ X || {_Index,_Band,_SSID,X,_Vendor} <- APS#ap_state.details#client_info.wifi_clients ],
	NewAssociations = #{  <<"id">> => null,
      <<"method">> => <<"update">>,

      <<"params">> =>
      [ Name, #{ <<"Wifi_Associated_Clients">> => make_macs(WiFiClients,#{})}]},
	ovsdb_ap:publish(NewAssociations),
	ok.





