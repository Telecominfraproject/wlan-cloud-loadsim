%%%----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2020 12:44 p.m.
%%%----------------------------------------------------------------------------
-module(ovsdb_ap).
-author("helge").

-compile({parse_transform, lager_transform}).

-include("../include/common.hrl").
-include("../include/ovsdb_definitions.hrl").
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/opensync_stats.hrl").

%% API
-export([start/4]).
-export([start_ap/1,stop_ap/1,pause_ap/1,cancel_ap/1]).

%% comm API
-export([ set_mqtt_conf/2,set_ssid/2,publish/1,set_ovsdb_manager/2]).

%%%============================================================================
%%% API
%%%============================================================================
-spec start (CAName :: string() | binary(), ID::binary(), Options :: #{atom() => term()}, Manager::pid()) -> ok.
start(CAName, ID, Options, ManagerPID ) ->
	#{ sim_name:= SimName, ovsdb_server_name:= Server, ovsdb_server_port:= Port} = Options,
	%% io:format("OVSDB_AP: ID=~p~n",[ID]),
	{ ok, ClientInfo } = inventory:get_record(#client_info{name = ID}),
	{ok,[HardwareInfo]} = hardware:get_by_id(ClientInfo#client_info.id),
	{ok, ReportTimer } = timer:send_interval(15000,send_report),
	Redirector = maps:get(redirector,Options,<<"">>),
	CurrentState = #ap_state{
		id = ID,
		caname = CAName,
		simname = SimName,
		details = ClientInfo,
		manager_pid = ManagerPID,
		status = ready,
		hardware = HardwareInfo,
		ovsdb_server_port = Port,
		ovsdb_server_name = Server,
		original_ovsdb_server_name = Server,
		original_ovsdb_server_port = Port,
		redirector = Redirector,
		wan_addr = make_wan_addr(),
		reporting = ReportTimer,
		mqtt_update_timer = none,
		stats = #ap_statistics{ start_stamp = os:system_time() }
	},
	ovsdb_client_handler:set_ap_status( ready , ID),
	StartingState = ovsdb_ap_config:configure(CurrentState),
	_=message_loop(StartingState),
	ok.

%%%============================================================================
%%% HANDLER API Implementation
%%%============================================================================
-spec start_ap (APPid :: pid()) -> ok.
start_ap( APPid ) ->
	APPid ! ap_start, ok.

-spec stop_ap (APPid :: pid()) -> ok.
stop_ap(APPid) ->
	APPid ! ap_stop, ok.

-spec pause_ap (APPid :: pid()) -> ok.
pause_ap(APPid) ->
	APPid ! ap_pause, ok.

-spec cancel_ap (APPid :: pid()) -> ok.
cancel_ap (APPid) ->
	APPid ! ap_cancel, ok .

-spec set_mqtt_conf (APPid :: pid(), Config :: map()) -> ok.
set_mqtt_conf (APPid, Conf) ->
	APPid ! {set_mqtt_conf,Conf}, ok.

-spec set_ovsdb_manager(APPid::pid(),Manager::#{ atom() => term()})-> ok.
set_ovsdb_manager(APPid,Manager)->
	APPid ! {set_ovsdb_manager,Manager}, ok.

-spec set_ssid(APPid :: pid(), SSID :: binary()) -> ok.
set_ssid (APPid,SSID) ->
	APPid ! {set_ssid,SSID}, ok.

publish( Request ) ->
	self() ! {publish,Request}, ok.

-spec message_loop(APS::ap_state()) -> any().
message_loop(APS) ->
	receive

		dump_state ->
			io:format("~p: ~n~p~n",[APS#ap_state.id,APS]),
			message_loop(APS);

		ap_start ->
			case APS#ap_state.status of
				 ready -> message_loop(start_connection(APS));
				paused -> message_loop(APS#ap_state{ status = running});
						 _ -> message_loop(APS)
			end;

		ap_stop ->
			case APS#ap_state.status of
				paused -> message_loop(disconnect(APS));
				running-> message_loop(disconnect(APS));
						 _ -> message_loop(APS)
			end;

		ap_pause ->
			case APS#ap_state.status of
				running -> 	ovsdb_client_handler:set_ap_status(paused,APS#ap_state.id),
										message_loop(APS#ap_state{ status = paused });
							_ -> message_loop(APS)
			end;

		ap_cancel ->
			disconnect(APS);

		connect ->
			NewState = start_connection(APS),
			% post_event(self(),comm_event,{<<"started">>},<<>>),
			message_loop(NewState);

		re_connect ->
			NewState = start_connection(APS),
			% post_event(self(),comm_event,{<<"started">>},<<>>),
			message_loop(NewState);

		{set_ovsdb_manager,Manager } ->
			?L_IA("~p: Redirecting to ~p.~n",[APS#ap_state.id,Manager]),
			log_packet(binary:list_to_bin([<<"Redirecting to ">>,Manager]),APS),
			[_Protocol,NewHost,NewPort] = string:tokens(binary_to_list(Manager),":"),
			_ = sslclose(APS#ap_state.socket),
			APS1 = stop_timers(APS),
			APS2 = APS1#ap_state{   ovsdb_server_name = list_to_binary(NewHost),
		                          ovsdb_server_port = list_to_integer(NewPort),
	                            socket = none,
	                            trail_data = <<>>,
	                            status = ready,
		                          redirected = true},
			message_loop(start_connection(APS2#ap_state{ normal_reconnect = true, manager_addr = Manager }));

		{set_mqtt_conf,Conf} ->
			?L_IA("~p: Just got MQTT settings: ~p~n",[APS#ap_state.id,Conf]),
			message_loop(check_mqtt(Conf,APS));

		{set_ssid,SSID} = M ->
			_ = case APS#ap_state.mqtt == running of
				true ->     mqtt_client_manager:set_ssid(APS#ap_state.caname,APS#ap_state.id,SSID);
				false-> _ = timer:send_after( 5000 , M ) %% resent a notification in 5 seconds if MQTT is not running
			end,
			message_loop(APS#ap_state{ ssid = SSID });

		send_report ->
			message_loop(report_statistics(APS));

		check_mqtt_updates ->
			message_loop(request_mqtt_updates(APS));

		check_publish_monitor ->
			% io:format("~p: Publish tick=~p. Echos=~p~n",[APS#ap_state.id,APS#ap_state.check_monitor_tick,APS#ap_state.echo]),
			case (APS#ap_state.echo>0) of
				false ->
					message_loop(APS);
				true ->
					case APS#ap_state.check_monitor_tick of
						2 ->
							?L_IA("~p: Resetting Wifi_Associated_Clients table.~n",[APS#ap_state.id]),
							NewState = send_associated_clients_table(false,APS),
							message_loop(NewState#ap_state{ check_monitor_tick = NewState#ap_state.check_monitor_tick+1 });
						4 ->
							?L_IA("~p: Resetting DHCP_Leased_IP table.~n",[APS#ap_state.id]),
							NewState = send_dhcp_lease_table(false,APS),
							message_loop(NewState#ap_state{ check_monitor_tick = NewState#ap_state.check_monitor_tick+1 });
						3 ->
							?L_IA("~p: Sending Wifi_Associated_Clients table.~n",[APS#ap_state.id]),
							NewState = send_associated_clients_table(true,APS),
							message_loop(NewState#ap_state{ check_monitor_tick = NewState#ap_state.check_monitor_tick+1 });
						5 ->
							?L_IA("~p: Sending DHCP_Leased_IP table.~n",[APS#ap_state.id]),
							NewState = send_dhcp_lease_table(true,APS),
							message_loop(NewState#ap_state{ check_monitor_tick = NewState#ap_state.check_monitor_tick+1 });
						_ ->
							message_loop(APS#ap_state{ check_monitor_tick = APS#ap_state.check_monitor_tick+1 })
					end
			end;

		{ssl, S, Data} ->
			case S == APS#ap_state.socket of
				true ->
					NewStats = APS#ap_state.stats#ap_statistics{ rx_bytes = APS#ap_state.stats#ap_statistics.rx_bytes + size(Data)},
					NewState = process_received_data( Data,APS#ap_state{stats = NewStats}),
					message_loop(NewState);
				false->
					message_loop(APS)
			end;

		{ssl_closed, S} ->
			case S == APS#ap_state.socket of
				true ->
					?L_IA("~p: Socket closed by server.~n",[APS#ap_state.id]),
					log_packet(binary:list_to_bin([<<"Socket closed by server.">>]),APS),
					APS1 = disconnect(APS),
					message_loop(try_reconnect(APS1));
				false ->
					message_loop(APS)
			end;

		{ssl_error, S, Reason} ->
			case S == APS#ap_state.socket of
				true ->
					?L_IA("~p: socket error:~p. ",[APS#ap_state.id,Reason]),
					message_loop(try_reconnect(APS));
				false ->
					message_loop(APS)
			end;

		{client_stats,_AP,_Statistics} ->
			%% io:format(">>>>MUST UPDATE STATS!!!~n"),
			message_loop(APS);

		{send_raw, _AP, RawData} ->
			% io:format("~p: Data sent back: ~p~n",[APS#ap_state.id,RawData]),
			?L_IA("~p: Sending internal ~p bytes.~n",[APS#ap_state.id,size(RawData)]),
			log_packet(RawData,APS),
			NewState = sslsend(RawData,APS),
			message_loop(NewState);

		{publish,TableData} when is_map(TableData)->
			?L_IA("~p: Sending monitored data.~n",[APS#ap_state.id]),
			Data = jiffy:encode(TableData),
			NewState = sslsend(Data,APS),
			message_loop(NewState);

		{down, _AP} ->
			% post_event(self(),comm_event,{<<"shutdown">>},<<>>),
			sslclose(APS#ap_state.socket);

		Message ->
			io:format("~p: unknown message: ~p~n",[APS#ap_state.id,Message]),
			message_loop(APS)
	end.

%%%============================================================================
%%% internal functions
%%%============================================================================

-spec sslsend(Data::binary(),State::ap_state()) -> NewState::ap_state().
sslsend(Data,APS)->
	try
		case APS#ap_state.socket == none of
			false ->
				_=ssl:send(APS#ap_state.socket,Data),
				NewStats = APS#ap_state.stats#ap_statistics{ tx_bytes = APS#ap_state.stats#ap_statistics.tx_bytes + size(Data) },
				APS#ap_state{ stats = NewStats };
			true ->
				APS
		end
	catch
		_:_ ->
			APS
	end.

sslclose(none)-> ok;
sslclose(S)->
	try
	    case ssl:close(S,2000) of
				ok ->
					ok;
				Error ->
					?L_IA(">>>>Error closing socket: ~p.~n",[Error])
	    end
	catch
	   _:_  ->
		   ok
	end.

stop_timer(none) -> ok;
stop_timer(T)    -> _=timer:cancel(T).

-spec start_timers(APS::ap_state()) -> NewAPS::ap_state().
start_timers(APS)->
	{ok, MqttUpdateTimer}       = timer:send_interval(20000,check_mqtt_updates),
	{ok, CheckPublishMonitor }  = timer:send_interval(10000,check_publish_monitor),
	APS#ap_state{ mqtt_update_timer = MqttUpdateTimer, publish_timer = CheckPublishMonitor}.

-spec stop_timers(APS::ap_state()) -> NewAPS::ap_state().
stop_timers(APS)->
	_ = stop_timer(APS#ap_state.publish_timer),
	_ = stop_timer(APS#ap_state.mqtt_update_timer),
	APS#ap_state{ mqtt_update_timer = none, publish_timer = none}.


%--------cancel_simulation/1----------------shutdown and exit simulation (AP exits)
-spec process_received_data(Data::binary(),APS::ap_state()) -> NewState::ap_state().
process_received_data (<<>>, APS) ->
	APS;
process_received_data (Data, APS) ->
	try
		FullData = << (APS#ap_state.trail_data)/binary, Data/binary>>,
		{JSONToProcess,TrailingData} = case jiffy:decode(FullData,[return_maps,copy_strings,return_trailer]) of
											{has_trailer,Map,Tail} ->
												{Map,iolist_to_binary(Tail)};
											Map ->
												{Map,<<"">>}
										end,
		case ovsdb_process:do(JSONToProcess,APS) of
			{reply,ResponseData,NewState} ->
				?L_IA("~p: Sending back ~p bytes.~n",[APS#ap_state.id,size(ResponseData)]),
				log_packet(ResponseData,NewState),
				NewState2 = sslsend(ResponseData,NewState),
				process_received_data(TrailingData,NewState2#ap_state{ trail_data = <<>> });
			{noreply,NewState} ->
				process_received_data(TrailingData,NewState#ap_state{ trail_data = <<>>})
		end
	catch
		error:{N,Error} ->
			?L_IA("JSON decode error: '~p' after ~p bytes",[Error,N]),
			APS#ap_state{ trail_data = Data };
		_:_ ->
			APS#ap_state{ trail_data = <<>>}
	end.

%--------ctrl_connect/1------------------connect to either the tip redirector or manager based on state / old connections are closed if open
%--------ctrl_disconnect/1---------------disconnect and closes communication port but otherwise does not chenge status
-spec start_connection(APS::#ap_state{}) -> NewState::#ap_state{}.
start_connection(APS0) ->
	Opts = [{cacerts, [APS0#ap_state.details#client_info.cacert]},
	        {cert,APS0#ap_state.details#client_info.cert},
	        {key,APS0#ap_state.details#client_info.key},
	        {versions, ['tlsv1.2','tlsv1.3']},
	        {session_tickets,auto},
	        {mode,binary},
	        {keepalive, true},
	        {packet,raw},
	        {active,true},
	        {recbuf, 500000},
	        {sndbuf, 500000}],
	% opn normal reconnect, you should not reset the AWLAN table...
	APS = case APS0#ap_state.normal_reconnect of
		true ->
			APS0;
		false ->
			ovsdb_ap_config:create_table(<<"AWLAN_Node">>,APS0)
	end,
	?L_IA("~p: AP connecting to ~s:~B",[APS#ap_state.id,APS#ap_state.ovsdb_server_name,APS#ap_state.ovsdb_server_port]),
	NewState = case ssl:connect( binary_to_list(APS#ap_state.ovsdb_server_name), APS#ap_state.ovsdb_server_port, Opts, 5000) of
		{ok, Socket} ->
			ovsdb_client_handler:set_ap_status(running,APS#ap_state.id),
			NewAPS = APS#ap_state{ socket = Socket, reconnect_timer = none, reconnecting = false , normal_reconnect = false },
			start_timers(NewAPS);
		{error, Reason} ->
			?L_IA("~p: Connecting to ~s:~B failed with reason: ~p",[APS#ap_state.id,APS#ap_state.ovsdb_server_name,APS#ap_state.ovsdb_server_port,Reason]),
			try_reconnect(APS#ap_state{ reconnecting = false, reconnect_timer = none ,socket = none, echo = 0, normal_reconnect = false })
	end,
	NewState.

disconnect(APS)->
	_ = sslclose(APS#ap_state.socket),
	APS1 = stop_timers(APS),
	ovsdb_client_handler:set_ap_status( paused , APS1#ap_state.id),
	APS1#ap_state{ status = ready, socket = none, mqtt_update_timer = none, reconnect_timer = none, normal_reconnect = false }.

random_reconnect_timer(APS)->
	(rand:uniform(APS#ap_state.max_backoff-APS#ap_state.min_backoff)+APS#ap_state.min_backoff)*1000.

-spec try_reconnect (State :: #ap_state{}) -> NewState :: #ap_state{}.
try_reconnect (#ap_state{ reconnecting = true }=State) ->
	State;
try_reconnect (#ap_state{ reconnecting = false, retries = Retries }=APS0) when Retries > 5 ->
	Reconnect_ms = random_reconnect_timer(APS0),
	{ok,ReconnectionTimer} = timer:send_after(Reconnect_ms, re_connect ),
	?L_I(?DBGSTR("socket closed by server, trying to reconnect in ~B seconds.",[Reconnect_ms div 1000])),
	_ = sslclose(APS0#ap_state.socket),
	APS1 = stop_timers(APS0),
	APS2 = case APS1#ap_state.redirected of
		true ->
			APS1#ap_state{ ovsdb_server_port = APS1#ap_state.original_ovsdb_server_port ,
			               ovsdb_server_name = APS1#ap_state.original_ovsdb_server_name ,
			               redirected = false, echo = 0 , normal_reconnect = false,
										 manager_addr = <<>>};
		false ->
			APS1
		end,
	ovsdb_client_handler:set_ap_status( reconnecting , APS2#ap_state.id),
	APS3 = ovsdb_ap_config:create_table(<<"AWLAN_Node">>,APS2),
	APS3#ap_state{socket=none, reconnecting = true, reconnect_timer = ReconnectionTimer, mqtt_update_timer = none, retries = 0 , echo = 0,check_monitor_tick = 0};
try_reconnect (#ap_state{ reconnecting = false, retries = Retries}=APS0) ->
	Reconnect_ms = random_reconnect_timer(APS0),
	{ok,ReconnectionTimer} = timer:send_after(Reconnect_ms, re_connect ),
	_ = sslclose(APS0#ap_state.socket),
	APS = stop_timers(APS0),
	?L_I(?DBGSTR("socket closed by server, trying to reconnect in ~B seconds.",[Reconnect_ms div 1000])),
	ovsdb_client_handler:set_ap_status( reconnecting , APS#ap_state.id),
	APS#ap_state{socket=none, reconnecting = true, echo = 0, reconnect_timer = ReconnectionTimer, mqtt_update_timer = none, retries = Retries+1 ,
		check_monitor_tick = 0 , normal_reconnect = false}.

%%==============================================================================
%% managing mqtt
-spec check_mqtt (Config :: #{binary():=binary()}, #ap_state{}) -> NewState :: #ap_state{}.
check_mqtt (NewConfig,#ap_state{caname=CAName, id=ID, mqtt_config = OldMqttConfig }=APS) ->
	?L_I(?DBGSTR("AP->MQTT check configuration")),
	case mqtt_client_manager:is_running(CAName,ID) of
		{ok,_} ->
			case NewConfig =/= OldMqttConfig of
				true ->
					_ = stop_mqtt(APS),
					start_mqtt(NewConfig,APS);
				false ->
					APS
			end;
		_ ->
			start_mqtt(NewConfig,APS)
	end.

-spec start_mqtt(gen_configuration(), APS::ap_state()) -> NewState :: ap_state().
start_mqtt (Cfg,#ap_state{caname=CAName, id=ID, mqtt=idle}=APS) ->
	_ = mqtt_client_manager:start_client(CAName,ID,Cfg),
	APS#ap_state{mqtt=running,mqtt_config = Cfg};
start_mqtt (_,APS) ->
	?L_E(?DBGSTR("MQTT start request, but client already running!")),
	APS.

-spec stop_mqtt (APS::ap_state()) -> NewState::ap_state().
stop_mqtt(#ap_state{mqtt=idle}=APS) ->
	APS;
stop_mqtt(#ap_state{caname=CAName, id=ID}=APS) ->
	_ = mqtt_client_manager:stop_client(CAName,ID),
	APS#ap_state{mqtt=idle}.

-spec request_mqtt_updates (APS::ap_state()) -> NewState ::ap_state().
request_mqtt_updates (#ap_state{ caname = CAName, id = ID} = APS) ->
	Mqtt = mqtt_client_manager:get_client_pid(CAName,ID),
	Mqtt ! {send_stats, self()},
	APS.

%%==============================================================================
%% managing statistics of access point
%--------report_statistics/1-------------generate an AP specific statistics report and send it to the handler
-spec report_statistics (APS :: ap_state()) -> NewState :: ap_state().
report_statistics (#ap_state{status=ready}=APS) ->
	APS;
report_statistics (APS) ->
	ovsdb_client_handler:push_ap_stats(APS#ap_state.stats#ap_statistics{ end_stamp = os:system_time()},APS#ap_state.id),
	NewStats = #ap_statistics{ start_stamp = os:system_time() },
	APS#ap_state{ stats = NewStats }.

-spec make_wan_addr() -> IPAddr :: binary().
make_wan_addr() ->
	A = rand:uniform(30) + 60,
	B = rand:uniform(200) + 20,
	C = rand:uniform(50) + 100,
	D = rand:uniform(230) + 10,
	list_to_binary(io_lib:format("~B.~B.~B.~B",[A,B,C,D])).

send_response(TableName,Response,APS) ->
	FullResponse = #{ <<"id">> => null,
	              <<"method">> => <<"update">>,
	              <<"params">> =>
	              [ binary:list_to_bin([TableName,<<"_Open_AP_">>, APS#ap_state.id]),
	                #{ TableName => Response }]},
	self() ! {send_raw, APS#ap_state.id, jsx:encode(FullResponse)}.

send_dhcp_lease_table(DevState,APS) ->
	TableName = <<"DHCP_leased_IP">>,
	TableData = maps:get(TableName,APS#ap_state.tables),
	{NewLeaseTable,_ResponseTable} =
		maps:fold(fun(UUID,#{ <<"_version">> := CurrentVersion }=V,{TmpNewTable,TmpResponse}) ->
							NewVersion = utils:create_version(),
							case DevState of
								true ->
									NewUUID = utils:uuid_b(),
									NewLeaseVersion = V#{ <<"_version">> => NewVersion,
									                      <<"_uuid">> => [<<"uuid">>,NewUUID ]},
									NewRow = #{ <<"new">> => V#{ <<"_version">> => NewVersion,
																							<<"_uuid">> => [<<"uuid">>,NewUUID ]},
									            <<"old">> => #{<<"_version">> => CurrentVersion }} ,
									% io:format("~p: DHCP_LEASE_TRANSACTION (on): ~p~n",[APS#ap_state.id,NewRow]),
									_=send_response( TableName, #{ NewUUID => maps:remove(<<"_uuid">>,NewRow)}, APS ),
									{ maps:put(NewUUID,NewLeaseVersion,TmpNewTable),
										maps:put(NewUUID,NewRow,TmpResponse) };
								false->
									NewRow = #{ UUID => #{<<"old">> => maps:remove(<<"_uuid">>,V) } },
									% io:format("~p: DHCP_LEASE_TRANSACTION (off): ~p~n",[APS#ap_state.id,NewRow]),
									_=send_response( TableName, NewRow, APS ),
									{ maps:put(UUID,V,TmpNewTable),
									  maps:put(UUID,NewRow,TmpResponse) }
							end
						end,{#{},#{}},TableData),
	APS#ap_state{ tables = maps:put(TableName,NewLeaseTable,APS#ap_state.tables)}.

send_associated_clients_table(DevState,APS) ->
	APS1 = send_assoc_table(DevState,APS),
	send_vif_state(DevState,APS1).

send_assoc_table(DevState,APS)->
	% make sure you update the associated list
	TableName = <<"Wifi_Associated_Clients">>,
	TableData = maps:get(TableName,APS#ap_state.tables),
	{New_Wifi_Associated_Clients,Response} = maps:fold(fun(K,OldRow,{NewAssocClientsTable,PartialResponse}) ->
									#{ <<"_version">> := OldVersion } = OldRow,
									NewRow = case DevState of
														 true ->
															 OldRow#{ <<"state">> => <<"active">>, <<"_version">> => utils:create_version() };
														 false->
															 OldRow#{ <<"state">> => <<"idle">>, <<"_version">> => utils:create_version() }
									         end,
									OldEntry = case DevState of
															 true ->
																 #{ <<"_version">> => OldVersion, <<"state">> => <<"idle">> };
															 false->
																 #{ <<"_version">> => OldVersion, <<"state">> => <<"active">> }
									           end,
									ResponseRow = #{ <<"new">> => NewRow,
									                 <<"old">> => OldEntry},
									{ maps:put(K,NewRow,NewAssocClientsTable), maps:put(K,ResponseRow,PartialResponse) }
	              end,{#{},#{}},TableData),

	FullResponse = #{ <<"id">> => null,
	                 <<"method">> => <<"update">>,
	                 <<"params">> =>
	                 [ binary:list_to_bin([TableName,<<"_Open_AP_">>, APS#ap_state.id]),
	                   #{ TableName => Response } ]},

	self() ! {send_raw, APS#ap_state.id, jsx:encode(FullResponse)},

	NewTables = maps:put(TableName,New_Wifi_Associated_Clients, APS#ap_state.tables),
	APS#ap_state{ tables = NewTables }.


% make sure you update the VIF state
	% look in the radio DB for the same if_name, this will give you the band
	% with the band, add the client list to to VIF state and send a monitor update
send_vif_state(DevState,APS)->
	TableName = <<"Wifi_VIF_State">>,
	Wifi_VIF_State_Table = maps:get(TableName,APS#ap_state.tables),
	{New_Wifi_VIF_State_Table,VIF_Response} = maps:fold(fun(K,Wifi_VIF_State_Row,{TempNewVifTable,TempVifResponse }) ->
							% io:format("~p: Band=~p Clients=~p~n",[APS#ap_state.id,K,Wifi_VIF_State_Row]),
							case maps:get(<<"if_name">>,Wifi_VIF_State_Row,undefined) of
								undefined ->
									?L_IA("~p: Wifi_VIF_State_Table: could not find if_name~n",[APS#ap_state.id]),
									{maps:put(K,Wifi_VIF_State_Row,TempNewVifTable),TempVifResponse};
								IFName ->
									Band = ovsdb_ap_config:find_band_for_interface(IFName,APS),
									case maps:get(Band,APS#ap_state.associated_clients,undefined) of
										undefined ->
											?L_IA("~p: skipping Wifi_VIF_State update:~p~n",[APS#ap_state.id,Wifi_VIF_State_Row]),
											{maps:put(K,Wifi_VIF_State_Row,TempNewVifTable),TempVifResponse};
										MacUUIDList ->
											#{ <<"_version">> := OldVersion, <<"associated_clients">> := OldSet } = Wifi_VIF_State_Row,
											NewOldSet = case DevState of
																			true -> OldSet;
																			false -> [<<"set">>,[]]
											end,
											NewSet = case DevState of
																 true ->
																	 [ <<"set">>, [ [<<"uuid">>,X] || X <- MacUUIDList]];
																 false ->
																	 [<<"set">>,[]]

											end,
											OldRecord = #{ <<"associated_clients">> => NewOldSet,
											               <<"_version">> => OldVersion },
											NewRecord = Wifi_VIF_State_Row#{ <<"associated_clients">> => NewSet,
											                                 <<"_version">> => utils:create_version()},
											ResponseRow = #{ <<"new">> => NewRecord, <<"old">> => OldRecord},
											{ maps:put(K,NewRecord,TempNewVifTable), maps:put(K,ResponseRow,TempVifResponse)}
									end
							end
						end,{#{},#{}},Wifi_VIF_State_Table),

	FullVifResponse = #{ <<"id">> => null,
	                  <<"method">> => <<"update">>,
	                  <<"params">> =>
	                    [ binary:list_to_bin([TableName,<<"_Open_AP_">>, APS#ap_state.id]),
	                          #{ TableName => VIF_Response }]},
	self() ! {send_raw, APS#ap_state.id, jsx:encode(FullVifResponse)},

	NewTables = maps:put(TableName,New_Wifi_VIF_State_Table,APS#ap_state.tables),
	APS#ap_state{ tables = NewTables }.

log_packet(Data,APS)->
	case filelib:is_file("keep_packets") of
		true ->
			{ok,IoDev}=file:open("packets.log",[append]),
			io:fwrite(IoDev,"~p: (~p) ~p~n",
			          [ % calendar:system_time_to_rfc3339(erlang:system_time(seconds)),
				          os:system_time(),
									APS#ap_state.id,
									Data]),
			_=file:close(IoDev),
			ok;
		false->
			ok
	end.


%% Tables we should report...
%%% [<<"Wifi_VIF_State">>,
%%   <<"Wifi_Radio_State">>,
%%   <<"Wifi_Inet_State">>,
%%   <<"Wifi_Associated_Clients">>,
%%   <<"DHCP_leased_IP">>,
%%   <<"Command_State">>,
%%   <<"AWLAN_Node">>]


