%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 26. Dec 2020 10:27 a.m.
%%%-------------------------------------------------------------------
-module(ovsdb_process).
-author("stephb").


%% [rows=[Row [columns={freq_band=2.4G}], Row [columns={freq_band=5GU}], Row [columns={freq_band=5GL}]]]

%% [rows=[Row [columns={freq_band=2.4G}]], [Row [columns={freq_band=5GU}]], Row [columns={freq_band=5GL}]]]


-compile({parse_transform, lager_transform}).

-include("../include/ovsdb_definitions.hrl").
-include("../include/ovsdb_ap_tables.hrl").

%% API
-export([do/2,prepare_monitor_report/2]).

-spec do( Request::#{ binary() => term() }, APS:: ap_state()) -> { reply , Response::binary(), NewState::ap_state()} |
																												{ noreply, error , Reason::term(), NewState::ap_state()} |
																												{ noreply, NewState::ap_state()}.
do(#{ <<"method">> := <<"echo">>, <<"id">> := ID, <<"params">> := Params } = _Request, APS ) ->
	Response = #{ <<"id">> => ID, <<"result">> => Params, <<"error">> => null},
	{reply, jiffy:encode(Response) , APS#ap_state{ echo = APS#ap_state.echo+1 }};
do(#{ <<"method">> := <<"get_schema">>, <<"id">> := ID, <<"params">> := _Params } = _Request, APS ) ->
	Response = binary:list_to_bin([<<"{ \"id\":\"">>,ID,<<"\", \"error\": null, \"result\": ">>,read_schema(APS),<<" }">>]),
	{reply, Response, APS};
do(#{ <<"method">> := <<"monitor">>, <<"id">> := ID, <<"params">> := Params } = _Request, APS ) ->
	[{TableName,Parameters}] = process_monitor(Params),
	NewMonitoredTables = maps:put( TableName, Parameters, APS#ap_state.monitored_tables ),
	% TableNames = maps:fold(fun(K,_V,A) -> [K|A] end,[],NewMonitoredTables),
	% io:format("~p: Now monitoring: ~p~n",[APS#ap_state.id,TableNames]),
	NewState = APS#ap_state{ monitored_tables = NewMonitoredTables},
	case Parameters of
		#{ <<"select">> := #{ <<"initial">> := true }} ->
			{ ResponseDetails, NewState2 } = report_monitored_table(TableName,NewState),
			Response = #{ <<"id">> => ID, <<"result">> => ResponseDetails, <<"error">> => null },
			{ reply , jiffy:encode(Response), NewState2 };
		_ ->
			Response = #{ <<"id">> => ID, <<"result">> => #{}, <<"error">> => null },
			{ reply , jiffy:encode(Response), NewState }
	end;
do(#{ <<"method">> := <<"transact">> , <<"id">> := ID, <<"params">> := [<<"Open_vSwitch">> | Operations] } = _Request, APS ) ->
	%% ?L_IA("~p: Transact request: ~p",[APS#ap_state.id,Request]),
	transact(ID,Operations,APS,[]);
do(#{ <<"id">> := ID, <<"result">> := _Result, <<"error">> := _Error } = _Request, APS )->
	?L_IA("~p: OVSDB-REQUEST:Received answer (ignoring): ~p.~n,",[APS#ap_state.id,ID]),
	{noreply,APS};
do(Request,APS)->
	?L_IA("~p: OVSDB-REQUEST: unknown Request: ~p.~n,",[APS#ap_state.id,Request]),
	{ noreply,APS }.

% log_transact(ID,Params,APS)->
%	io:format("~p: TRANSACTION(~p): ~p~n",[APS#ap_state.id,ID,Params]).

process_monitor([_NameSpace, _LocalTableName, TableParameters] = _Params) ->
	MonRes = maps:fold(fun(K,V,A) ->
							[{ K, V}|A]
						end,[],TableParameters),
	% io:format("MONITOR: ~p, ~p, ~p =~p~n",[NameSpace, LocalTableName, TableParameters,MonRes]),
	MonRes.

-spec report_monitored_table(TableName::binary(),APS::ap_state()) -> { Response::#{}, NewState::ap_state()}.
%% {"id":"162","result":{"Wifi_Inet_State":{"8394af1b-d230-4c60-a801-a14e74711fd2":{"new":{"dhcpd":["map",[]],"if_name":"wwan","upnp_mode":["set",[]],"softwds_mac_addr":["set",[]],"if_type":"eth","enabled":false,"softwds_wrap":false,"vlan_id":["set",[]],"netmask":["set",[]],"NAT":false,"gre_remote_inet_addr":["set",[]],"if_uuid":"","inet_addr":["set",[]],"_version":["uuid","80447384-eefe-4b35-9a4b-7daf3dd789ff"],"hwaddr":"","network":false,"mtu":["set",[]],"parent_ifname":["set",[]],"dns":["map",[]],"broadcast":["set",[]],"gre_ifname":["set",[]],"dhcpc":["map",[]],"ip_assign_scheme":"dhcp","gateway":["set",[]],"inet_config":["uuid","cccc75a7-e427-4e2c-9268-de3e384c3d19"],"gre_local_inet_addr":["set",[]]}},"89a02462-02b6-4f20-840b-ca8f4713916c":{"new":{"dhcpd":["map",[]],"if_name":"wan","upnp_mode":["set",[]],"softwds_mac_addr":["set",[]],"if_type":"bridge","enabled":true,"softwds_wrap":false,"vlan_id":["set",[]],"netmask":"255.255.255.0","NAT":true,"gre_remote_inet_addr":["set",[]],"if_uuid":"","inet_addr":"10.20.0.113","_version":["uuid","7dabbe2f-560d-4307-985a-c6de9e3cbd1b"],"hwaddr":"58:ef:68:62:e7:f1","network":true,"mtu":1500,"parent_ifname":["set",[]],"dns":["map",[["primary","10.20.0.1"]]],"broadcast":["set",[]],"gre_ifname":["set",[]],"dhcpc":["map",[]],"ip_assign_scheme":"dhcp","gateway":"10.20.0.1","inet_config":["uuid","01524ea7-3d40-42bd-8875-aa8f36eece37"],"gre_local_inet_addr":["set",[]]}},"87eb6ee0-0f16-41d8-abc9-d2dedf804454":{"new":{"dhcpd":["map",[["lease_time","12h"],["start","100"],["stop","150"]]],"if_name":"lan","upnp_mode":["set",[]],"softwds_mac_addr":["set",[]],"if_type":"bridge","enabled":true,"softwds_wrap":false,"vlan_id":["set",[]],"netmask":"255.255.255.0","NAT":false,"gre_remote_inet_addr":["set",[]],"if_uuid":"","inet_addr":"192.168.1.1","_version":["uuid","f45ab5d5-aa80-4771-86e4-3648641b5b26"],"hwaddr":"58:ef:68:62:e7:f0","network":true,"mtu":1500,"parent_ifname":["set",[]],"dns":["map",[]],"broadcast":["set",[]],"gre_ifname":["set",[]],"dhcpc":["map",[]],"ip_assign_scheme":"static","gateway":["set",[]],"inet_config":["uuid","df8af6c1-0a3c-4064-825d-0d28d300092d"],"gre_local_inet_addr":["set",[]]}},"b605a1a6-b9bf-469b-a39b-851739b306af":{"new":{"dhcpd":["map",[]],"if_name":"wan6","upnp_mode":["set",[]],"softwds_mac_addr":["set",[]],"if_type":"eth","enabled":false,"softwds_wrap":false,"vlan_id":["set",[]],"netmask":["set",[]],"NAT":false,"gre_remote_inet_addr":["set",[]],"if_uuid":"","inet_addr":["set",[]],"_version":["uuid","8ff7bdc8-5824-4572-9857-0716da75d2e6"],"hwaddr":"","network":false,"mtu":["set",[]],"parent_ifname":["set",[]],"dns":["map",[]],"broadcast":["set",[]],"gre_ifname":["set",[]],"dhcpc":["map",[]],"ip_assign_scheme":["set",[]],"gateway":["set",[]],"inet_config":["uuid","3c501c0b-bc2b-41f6-8d15-753775fc20a8"],"gre_local_inet_addr":["set",[]]}}}},"error":null}
report_monitored_table(<<"DHCP_leased_IP">>,APS) ->
	{#{},APS};
report_monitored_table(<<"Wifi_Associated_Clients">>,APS) ->
	{#{},APS};
report_monitored_table(TableName,APS) ->
	case maps:get(TableName,APS#ap_state.tables,undefined) of
		undefined ->
			{#{},APS};
		TableData ->
			Res = maps:fold(  fun(K,V,A) ->
													maps:put(K,#{ <<"new">> => maps:remove(<<"_uuid">>,V) },A)
												end,#{}, TableData),
			{#{ TableName => Res },APS}
	end.



-spec transact( Id::integer(), Params::[], APS::ap_state(), Response::list() ) ->
								{ reply, binary(),NewState::ap_state() } |
								{ noreply , NewState::ap_state() }.
transact( Id, [] ,APS, ResponseAcc )->
	Response = #{ <<"id">> => Id, <<"error">> => null, <<"result">> => lists:reverse(ResponseAcc)},
	ResponseJson = iolist_to_binary(jiffy:encode(Response)),
	%% ?L_IA("~p: JSON Response: ~p.",[APS#ap_state.id,ResponseJson]),
	{ reply, ResponseJson,APS};
transact( Id, [#{<<"op">> := <<"select">>, <<"table">> := Table , <<"where">> := Where } = OperationParams | MoreOperations ] ,APS, ResponseAcc )->
	Columns = maps:get(<<"columns">>,OperationParams,[]),
	case lists:member(Table,APS#ap_state.known_table_names) of
		false ->
			return_error(Id,<<"Invalid table name.">>,APS);
		true ->
			TableData = maps:get(Table,APS#ap_state.tables),
			Rows = maps:fold( fun(K,V,Acc) ->
													case where(Where,V,K) of
														true ->
															case Columns of
																[] -> [ V | Acc ];
																_  -> [ columns(Columns,V,#{},K) | Acc ]
															end;
														false ->
															Acc
													end
												end,[],TableData),
			Response = #{ <<"rows">> => Rows },
			transact(Id, MoreOperations, APS, [Response|ResponseAcc])
	end;
transact( Id, [#{ <<"row">> := Row, <<"op">> := <<"update">>, <<"table">> := Table , <<"where">> := Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	case lists:member(Table,APS#ap_state.known_table_names) of
		false ->
			return_error(Id,<<"Invalid table name.">>,APS);
		true ->
			TableData = maps:get(Table,APS#ap_state.tables),
			{ NewTable, Count } = maps:fold( fun(K,V,{Acc,Count}) ->
													case where(Where,V,K) of
														true ->
															NewRow = process_row(Row,V),
															FinalRow = maps:put(<<"_version">>,utils:create_version(),NewRow),
															{ maps:put(K,FinalRow,Acc) , Count+1 };
														false ->
															{ maps:put(K,V,Acc), Count }
													end
			                  end,{#{},0},TableData),
			%% io:format("~n~n>>>> NEW TABLE: ~n~p~n~n",[NewTable]),
			Response = #{ <<"count">> => Count },
			check_for_special_values(Table,Row),
			transact(Id, MoreOperations, APS#ap_state{ tables = maps:put(Table,NewTable,APS#ap_state.tables), check_monitor_tick = 0 }, [Response|ResponseAcc])
	end;
transact( Id, [#{ <<"op">> := <<"delete">>, <<"table">> := Table , <<"where">> := Where }| MoreOperations ] = _Params ,APS, ResponseAcc )->
	case lists:member(Table,APS#ap_state.known_table_names) of
		false ->
			return_error(Id,<<"Invalid table name.">>,APS);
		true ->
			TableData = maps:get(Table,APS#ap_state.tables),
			{ NewTable, Count } = maps:fold( fun(K,V,{Acc,Count}) ->
																					case where(Where,V,K) of
																						true ->
																							{ Acc , Count+1 };
																						false ->
																							{ maps:put(K,V,Acc), Count }
																					end
			                                 end,{#{},0},TableData),
			%% io:format("~n~n>>>> NEW TABLE: ~n~p~n~n",[NewTable]),
			Response = #{ <<"count">> => Count },
			transact(Id, MoreOperations,APS#ap_state{ tables = maps:put(Table,NewTable,APS#ap_state.tables), check_monitor_tick = 0 },[Response|ResponseAcc])
	end;
transact( Id, [#{ <<"row">> := Row, <<"op">> := <<"insert">>, <<"table">> := Table } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	case lists:member(Table,APS#ap_state.known_table_names) of
		false ->
			return_error(Id,<<"Invalid table name.">>,APS);
		true ->
			TableData = maps:get(Table,APS#ap_state.tables),
			NewRow = maps:put(<<"_version">>,utils:create_version(),Row),
			UUID = utils:uuid_b(),
			NewTable = maps:put(UUID,NewRow,TableData),
			Response = #{ <<"uuid">> => [ <<"uuid">>,UUID ]},
			transact(Id, MoreOperations,APS#ap_state{ tables = maps:put(Table,NewTable,APS#ap_state.tables), check_monitor_tick = 0 },[Response|ResponseAcc])
	end;
transact( Id, [#{ <<"mutations">> := Mutations, <<"op">> := <<"mutate">>, <<"table">> := Table , <<"where">> := Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	case lists:member(Table,APS#ap_state.known_table_names) of
		false ->
			return_error(Id,<<"Invalid table name.">>,APS);
		true ->
			TableData = maps:get(Table,APS#ap_state.tables),
			{NewTable,Count} = maps:fold( fun(K,OriginalRow,{A,C}) ->
															case where(Where,OriginalRow,K) of
																true ->
																	NewRow = mutate(Mutations,OriginalRow),
																	{maps:put(K,NewRow,A),C+1};
																false -> { maps:put(K,OriginalRow,A), C }
															end
														end,{#{},0},TableData),
			% io:format("~p: Mutated object: ~p~n",[APS#ap_state.id,NewTable]),
			Response = #{ <<"count">> => Count },
			transact(Id, MoreOperations, APS#ap_state{ tables = maps:put(Table,NewTable,APS#ap_state.tables)}, [Response|ResponseAcc])
	end;
transact( Id, [#{ <<"row">> := _Row, <<"op">> := <<"wait">>, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	Response = #{ <<"error">> =><<"wait not supported">> },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]);
transact( Id, [#{ <<"row">> := _Row, <<"op">> := <<"commit">>, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	Response = #{ <<"error">> =><<"commit not supported">> },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]);
transact( Id, [#{ <<"row">> := _Row, <<"op">> := <<"abort">>, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	Response = #{ <<"error">> =><<"abort not supported">> },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]);
transact( Id, [#{ <<"row">> := _Row, <<"op">> := <<"assert">>, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	Response = #{ <<"error">> =><<"assert not supported">> },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]);
transact( Id, [#{ <<"row">> := _Row, <<"op">> := <<"comment">>, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	Response = #{ <<"error">> =><<"comment not supported">> },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]);
transact( Id, [#{ <<"row">> := _Row, <<"op">> := Operation, <<"table">> := _Table , <<"where">> := _Where } | MoreOperations ] = _Params ,APS, ResponseAcc )->
	P = binary:list_to_bin(<<"Unsupported operation:">>, Operation),
	Response = #{ <<"error">> => P },
	transact(Id,MoreOperations,APS,[Response|ResponseAcc]).

where([],_V,_UUID)->
	true;
where([[Field,Operation,Value]|T],V,UUID)->
	case maps:get(Field,V,undefined) of
		undefined ->
			case Field of
				<<"_uuid">> ->
					[<<"uuid">>,ValueUUID] = Value,
					case Operation of
						<<"==">> ->
							case UUID == ValueUUID of
								true -> where(T,V,UUID);
								false-> false
							end;
						<<"!=">> ->
							case UUID == ValueUUID of
								true -> false;
								false-> where(T,V,UUID)
							end
					end;
				_ ->
					false
			end ;
		FieldValue ->
			case Operation of
				<<"==">> ->
					case (FieldValue == Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<"<">> ->
					case (FieldValue < Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<"<=">> ->
					case (FieldValue =< Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<"!=">> ->
					case (FieldValue =/= Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<">=">> ->
					case (FieldValue >= Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<">">> ->
					case (FieldValue > Value) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<"includes">> ->
					case lists:member(Value,FieldValue) of
						true -> where(T,V,UUID);
						false -> false
					end;
				<<"excludes">> ->
					case lists:member(Value,FieldValue) of
						false -> where(T,V,UUID);
						true -> false
					end;
				_ ->
					false
			end
	end.

% Rule: if columns are present in the request, you can only return those columns
columns([],_Row,Acc,_UUID)->
%	Version = maps:get(<<"_version">>,Row),
%	NewMap = maps:put(<<"_version">>,Version,Acc),
%	maps:put(<<"_uuid">>,[<<"uuid">>,UUID],NewMap);
	Acc;
columns([Name|T],Row,Acc,UUID)->
	case maps:get(Name,Row,undefined) of
		undefined ->
			columns(T,Row,Acc,UUID);
		Value ->
			NewAcc=maps:put(Name,Value,Acc),
			columns(T,Row,NewAcc,UUID)
	end.

process_row(NewRow,OldRow)->
	maps:fold(  fun(K,V,A) ->
								maps:put(K,V,A)
							end,OldRow,NewRow).

mutate([],NewRow)->
	maps:put(<<"_version">>,[<<"uuid">>,utils:uuid_b()],NewRow);
mutate([[Field,Operation,NewValue]|T],NewRow)->
	ModifiedRow = case Operation of
									<<"insert">> ->
										case maps:get(Field,NewRow,undefined) of
											undefined ->
												maps:put(Field,[<<"set">>,[NewValue]],NewRow);
											[<<"set">>,OldField] ->
												maps:put(Field,[<<"set">>,OldField ++ [NewValue]],NewRow);
											_ ->
												maps:put(Field,[<<"set">>,[NewValue]],NewRow)
										end;
									<<"delete">> ->
										case maps:get(Field,NewRow,undefined) of
											undefined ->
												NewRow;
											OldField ->
												maps:put(Field,lists:delete(NewValue,OldField),NewRow)
										end
								end,
	mutate(T,ModifiedRow).

return_error(Id,Error,APS)->
	Response = #{ <<"id">> => Id, <<"error">> => Error },
	{ reply, jiffy:encode(Response), APS}.

check_for_special_values (<<"AWLAN_Node">>, #{ <<"manager_addr">> := Manager}) ->
	% io:format("Found Manager: ~p~n",[Manager]),
	ovsdb_ap:set_ovsdb_manager(self(),Manager);
check_for_special_values (<<"AWLAN_Node">>,#{ <<"mqtt_settings">> := [<<"map">>,MQTT]}) ->
  % io:format("Found MQTT settings: ~p~n",[MQTT]),
	Map = maps:from_list([{K,V}||[K,V]<-MQTT]),
	ovsdb_ap:set_mqtt_conf(self(),Map);
check_for_special_values (_T,_UpdateData) ->
	ok.

prepare_monitor_report(TableName,APS)->
	{ ResponseDetails, APS1 } = report_monitored_table(TableName,APS),
	Response = #{ <<"id">> => null,
	              <<"method">> => <<"update">>,
	              <<"params">> => [ binary:list_to_bin([TableName,<<"_Open_AP_">>,APS#ap_state.id]), ResponseDetails]},
	{jiffy:encode(Response), APS1}.

-spec read_schema(APS::ap_state()) -> binary().
read_schema (APS) ->
	Model=binary_to_list(APS#ap_state.hardware#hardware_info.model),
	FName = filename:join([utils:priv_dir(),"templates",iolist_to_binary([string:uppercase(Model),"_schema.json"])]),
	case filelib:is_regular(FName) of
		true ->
			{ok, Schema} = file:read_file(FName),
			Schema;
		false ->
			DefaultSchema = filename:join([utils:priv_dir(),"templates","ap_default_schema.json"]),
			{ok, Schema} = file:read_file(DefaultSchema),
			Schema
	end.