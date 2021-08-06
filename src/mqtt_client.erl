%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2020 10:51 a.m.
%%%-------------------------------------------------------------------
-module(mqtt_client).
-author("stephb").

-compile({parse_transform, lager_transform}).

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/mqtt_definitions.hrl").
-include("../include/opensync_stats.hrl").

%% API
-export([start/4,send_ping/1,c_cfg/0,s_cfg/0,t1/0,t2/0]).

-record(client_state,{id = <<>> :: binary(),
											caname :: binary(),
                      manager_pid :: pid(),
											broker :: binary(),
											port :: integer(),
                      topics = <<>> :: binary(),
                      compress = <<>> :: binary(),
                      configuration = #{} :: gen_configuration(),
                      details :: client_info(),
                      current_state = not_connected :: atom(),
                      keep_alive_ref = undefined :: undefined | timer:tref(),
											send_running_stats_timer = undefined :: undefined | timer:tref(),
                      errors=0::integer(),
											t1 = 0 :: integer(),
											connect_time = 0 :: integer(),
											messages=0::integer(),
											start_time = 0 :: number(),
											send_report_timer,
											last_report = 0,
											running_stats = #mqtt_stats{} :: mqtt_stats(),
											mac_stats = #{} :: #{ binary() => #'Client.Stats'{} }}).

-spec start(CAName::binary(),Serial::binary(),Configuration::gen_configuration_b(), ManagerPid::pid()) -> no_return().
start(CAName,Serial,Configuration,ManagerPid)->
	%% io:format("STARTING CLIENT: ~p~n",[Serial]),
	#{ <<"broker">> := Broker, <<"compress">> := Compress, <<"port">> := Port, <<"topics">> := Topics } = Configuration,
	%% io:format(">>>>Trying topic: ~p  connect to: ~p:~p~n",[Topics,Broker,Port]),
	NewConfig = #{ broker => <<"opensync-mqtt-broker.wlan.local">>, compress => Compress,
	            port => list_to_integer(binary_to_list(Port)), topics => Topics },
	{ok,DeviceConfiguration} = inventory:get_record(#client_info{name = Serial}),

	MacStats = prepare_mac_stats(DeviceConfiguration),

	{ok,SendRunningStatsTimer} = timer:send_interval(2000,send_running_stats),

	full_start(#client_state{
		id = Serial,
		caname = CAName,
		manager_pid = ManagerPid,
		details = DeviceConfiguration,
		configuration = NewConfig,
		broker = Broker,
		port = list_to_integer(binary_to_list(Port)),
		topics = Topics,
		keep_alive_ref = undefined,
		send_report_timer = undefined,
		mac_stats = MacStats,
		last_report = os:system_time(),
		send_running_stats_timer = SendRunningStatsTimer,
		compress = Compress}).

-spec full_start(State::#client_state{})->no_return().
full_start(State)->
	NewState = case ssl:connect(binary_to_list(State#client_state.broker),
		                  State#client_state.port,
	                     [{session_tickets,auto},
	                      {versions, ['tlsv1.2','tlsv1.3']},
	                      {cert,State#client_state.details#client_info.cert},
												{key,State#client_state.details#client_info.key},
												{cacerts,[State#client_state.details#client_info.cacert]},
												{active,false},
												{keepalive,true},
												{packet,raw},
												{mode,binary}]) of
								{ok,SSLSocket} ->
									?L_IA("MQTT(~p): Connecting.~n",[State#client_state.details#client_info.serial]),
									NewStats = State#client_state.running_stats#mqtt_stats{ connects = State#client_state.running_stats#mqtt_stats.connects + 1},
									RS = run_client(SSLSocket,State#client_state{ running_stats = NewStats}),
									utils:do(State#client_state.keep_alive_ref =/= undefined,{timer,cancel,[State#client_state.keep_alive_ref]}),
									utils:do(State#client_state.send_report_timer =/= undefined,{timer,cancel,[State#client_state.send_report_timer]}),
									_ = ssl:close(SSLSocket),
									NewStats2 = State#client_state.running_stats#mqtt_stats{ disconnects = State#client_state.running_stats#mqtt_stats.disconnects + 1},
									RS#client_state{ running_stats = NewStats2,
										keep_alive_ref = undefined,
										send_report_timer = undefined };
								{error,_}=Error->
									?L_IA("MQTT(~p): Cannot connect: ~p.~n",[State#client_state.details#client_info.serial,Error]),
									State
							end,
	timer:sleep(5000),
	full_start(NewState).

-spec run_client(Socket::ssl:sslsocket(),CS::#client_state{}) -> #client_state{}.
run_client(Socket,CS)->
	%% "/ap/sim1-1-000050_SIM1000050/opensync"
	%%RealSerial = case string:tokens(binary_to_list(CS#client_state.topics),"/") of
	%%	             [_,Serial,_] -> list_to_binary(Serial);
	%%							 _ -> CS#client_state.details#client_info.serial
	%%            end,

	C = #mqtt_connect_variable_header_v4{
		username_flag = 0,
		password_flag = 0,
		will_retain_flag = 0,
		will_qos_flag = 0,
		will_flag = 0 ,
		clean_start_flag = 1,
		client_identifier = CS#client_state.details#client_info.serial,
		keep_alive = 180
		},
	M = #mqtt_msg{ variable_header = C},
	ConnectMessage = mqtt_message:encode(M),
	_Res = ssl:setopts(Socket,[{active,true}]),
	NewState = case ssl:send(Socket,ConnectMessage) of
		ok ->
			?L_IA("~p: MQTT_CLIENT: Sent connection message...~n",[CS#client_state.details#client_info.serial]),
			CS#client_state.manager_pid ! { stats, connection , 1 },
			NewStats = CS#client_state.running_stats#mqtt_stats{
				start_stamp = os:system_time(),
				end_stamp = 0,
				tx_bytes = CS#client_state.running_stats#mqtt_stats.tx_bytes+size(ConnectMessage) },
			manage_connection(Socket,CS#client_state{ current_state = waiting_for_hello,
			                                          running_stats = NewStats  });
		Error ->
			?L_IA("MQTT(~p): SSL error ~p while connecting.~n",[CS#client_state.details#client_info.serial,Error]),
			CS
	end,
	NewState#client_state.manager_pid ! { stats, connection , -1 },
	NewState.

-spec manage_connection(Socket::ssl:sslsocket(),CS::#client_state{}) -> no_return().
manage_connection(Socket,CS) ->
	receive
		{ssl,Socket,Data} ->
			%% io:format("MQTT_CLIENT: Received ~p bytes: ~p.~n",[size(Data),Data]),
			case manage_state(Data,CS) of
				{ none, NewState } ->
					NewStats = CS#client_state.running_stats#mqtt_stats{ rx_bytes = CS#client_state.running_stats#mqtt_stats.rx_bytes+size(Data) },
					manage_connection(Socket,NewState#client_state{ running_stats = NewStats });
				{ Response, NewState } ->
					_=ssl:send(Socket,Response),
					NewStats = CS#client_state.running_stats#mqtt_stats{ rx_bytes = CS#client_state.running_stats#mqtt_stats.rx_bytes+size(Data),
					                                                     tx_bytes = CS#client_state.running_stats#mqtt_stats.tx_bytes+size(Response)},
					manage_connection(Socket,NewState#client_state{running_stats = NewStats})
			end;
		{ssl_closed,Socket} ->
			?L_IA("MQTT(~p): Closing.~n",[CS#client_state.details#client_info.serial]),
			CS;
		send_report ->
			ThisReport = os:system_time(),
			NewMacStats = increase_stats(CS#client_state.mac_stats,(ThisReport-CS#client_state.last_report) div 1000000000 ),
			OpenSyncReport = mqtt_os_gen:gen_report(CS#client_state.start_time,CS#client_state.details,NewMacStats),
			Data = mqtt_message:publish(rand:uniform(60000),CS#client_state.topics,zlib:compress(OpenSyncReport),?MQTT_PROTOCOL_VERSION_3_11),
			_ = ssl:send(Socket,Data),
			%% Data2 = mqtt_message:decode(Data,?MQTT_PROTOCOL_VERSION_3_11),
			?L_IA("~p: MQTT: Sent an MQTT report.~n",[CS#client_state.details#client_info.serial]),
			manage_connection(Socket,CS#client_state{ mac_stats = NewMacStats, last_report = ThisReport });
		{set_ssid,SSID}->
			CI = CS#client_state.details,
			?L_IA("~p: MQTT: Setting SSID to ~p.",[CI#client_info.serial,SSID]),
			WifiClients = CI#client_info.wifi_clients,
			NewWifiClients = [{Index,Band,SSID,MAC,Vendor} || {Index,Band,_,MAC,Vendor} <- WifiClients],
			NewCI = CI#client_info{ wifi_clients = NewWifiClients},
			manage_connection(Socket,CS#client_state{ details = NewCI });
		{dump_client,all}->
			?L_IA("MQTT(~p)): ~p~n",[CS#client_state.details#client_info.serial,CS]),
			manage_connection(Socket,CS);
		{ send_stats , Pid }->
			Pid ! {client_stats,CS#client_state.details#client_info.serial,CS#client_state.mac_stats},
			manage_connection(Socket,CS);
		{ send_data, Data } ->
			%% io:format("MQTT_CLIENT: Received a message to return some data: ~p~n",[Data]),
			_ = ssl:send(Socket,Data),
			manage_connection(Socket,CS);
		send_running_stats ->
			mqtt_client_manager:update_running_stats(CS#client_state.id,CS#client_state.running_stats),
			manage_connection(Socket,CS#client_state{ running_stats = #mqtt_stats{start_stamp = os:system_time() }});
		Anything ->
			?L_IA("MQTT(~p): Unprocessed message (~p).~n",[CS#client_state.details#client_info.serial,Anything]),
			manage_connection(Socket,CS#client_state{ errors = CS#client_state.errors+1 })
	end.

-spec manage_state(Data::binary(),CS::#client_state{}) -> { Response::binary() , NewState::#client_state{} } | { none , NewState::#client_state{} }.
manage_state(Data,CS)->
	try
	    case mqtt_message:decode(Data,?MQTT_PROTOCOL_VERSION_3_11) of
		    {ok,Msg} ->
			    process( Msg#mqtt_msg.variable_header, CS );
		    {error,Reason} ->
			    ?L_IA("Could not decode packet. Reason=~p",[Reason]),
			    {none,CS#client_state{ errors = 1+CS#client_state.errors }}
	    end
	catch
		_:_ = Error ->
			?L_IA("Packet decoding exception. Reason=~p",[Error]),
			{none,CS#client_state{ errors = 1+CS#client_state.errors }}
	end.

%% this is responsible for sending the ping at 1 minutes interval.
send_ping(Pid)->
	Blob = mqtt_message:encode(#mqtt_msg{ variable_header = #mqtt_pingreq_variable_header_v4{} }),
	Pid ! { send_data, Blob }.

process( M, CS ) when is_record(M,mqtt_connack_variable_header_v4) ->
	case M#mqtt_connack_variable_header_v4.connect_reason_code of
		0 ->
			{ok,TRef} = timer:apply_interval(60*1000,?MODULE,send_ping,[self()]),
			{ok,TReportTimer} = timer:send_interval(60*1000,self(),send_report),
			ConnectTime = os:system_time() - CS#client_state.t1,
			CS#client_state.manager_pid ! { stats, connect_time , ConnectTime },
			{none,CS#client_state{ start_time = os:system_time(),
			                       messages = 1+CS#client_state.messages,
			                       current_state = connected ,
			                       keep_alive_ref = TRef,
			                       connect_time = ConnectTime,
			                       send_report_timer = TReportTimer,
			                       t1 = 0 }};
		Error ->
			?L_IA("MQTT(~p): Cannot connect. Rejected by server:~p .~n",[CS#client_state.details#client_info.serial,Error]),
			{none,CS#client_state{ messages = 1+CS#client_state.messages, errors = 1+CS#client_state.errors }}
	end;
process( M, CS ) when is_record(M,mqtt_pingresp_variable_header_v4) ->
	Response = mqtt_message:encode(#mqtt_msg{ variable_header = #mqtt_pingresp_variable_header_v4{} }),
	{ Response, CS#client_state{ messages = 1+CS#client_state.messages }};
process( M, CS ) ->
	?L_IA("MQTT_CLIENT: Unknown message: ~p~n",[M]),
	{none,CS}.

c_cfg()->
	#{<<"broker">> => <<"opensync-mqtt-broker.debfarm1-node-a.arilia.com">>,
	  <<"compress">> => <<"zlib">>,
	  <<"port">> => <<"1883">>,
	  <<"qos">> => <<"0">>,
	  <<"remote_log">> => <<"1">>,
	  <<"topics">> => <<"/ap/Open_AP_SIM1001A110000100/opensync">>}.

s_cfg()->
	#{port => 1883,
		num_listeners => 10,
		secure => true }.

t1()->
	mqtt_server_manager:start_server(<<"sim1">>,<<"mqtt-1">>,s_cfg()).

t2()->
	mqtt_client_manager:start_client(<<"sim1">>,<<"SIM1001A11000010">>,c_cfg()).

-spec prepare_mac_stats( CI::client_info()) -> #{ MAC::binary() => Stats::#'Client.Stats'{} }.
prepare_mac_stats(CI)->
	M1 = [ MAC || { _Index, _Port,MAC,_Vendor } <- CI#client_info.lan_clients],
	M2 = [ MAC || { _Index, _Band,_SSID,MAC,_Vendor } <- CI#client_info.wifi_clients],
	M = lists:flatten(M1 ++ M2),
	MacStats = lists:foldl( fun(E,A)->
														maps:put(E,#'Client.Stats'{		rx_bytes = 0 ,
														                               tx_bytes = 0 ,
														                               rx_frames = 0,
														                               tx_frames = 0,
														                               tx_retries = 0,
														                               rx_retries = 0,
														                               rx_rate = 0.0,
														                               tx_rate = 0.0,
														                               rx_errors = 0,
														                               tx_errors = 0,
														                               rssi = make32bit(-40) }, A)
													end,#{},M),
	MacStats.

-spec increase_stats( #{ binary() => #'Client.Stats'{} }, Delta::non_neg_integer()) -> #{ binary() => #'Client.Stats'{} }.
increase_stats(MacStats,_Delta)->
	maps:fold(  fun(K,V,M) ->
									NewRxBytes = V#'Client.Stats'.rx_bytes + rand:uniform(7500000),
									NewTxBytes = V#'Client.Stats'.tx_bytes + rand:uniform(2000000),
									NewStats = #'Client.Stats'{ rx_bytes = NewRxBytes,
																							tx_bytes = NewTxBytes,
									                            rx_frames = V#'Client.Stats'.rx_frames + rand:uniform(200),
									                            tx_frames = V#'Client.Stats'.tx_frames + rand:uniform(40),
									                            tx_retries = V#'Client.Stats'.tx_retries + rand:uniform(10),
									                            rx_retries = V#'Client.Stats'.rx_retries + rand:uniform(10),
									                            rx_errors = V#'Client.Stats'.rx_errors + rand:uniform(2)-1,
									                            tx_errors = V#'Client.Stats'.tx_errors + rand:uniform(2)-1,
									                            rx_rate = mid_rand( 700000.0, 1000000.0),
									                            tx_rate = mid_rand( 300000.0, 500000.0),
									                            rssi = make32bit(-1 * (rand:uniform(20)+40))
									},
									maps:put(K,NewStats,M)
							end, #{}, MacStats).

-spec make32bit(integer()) -> non_neg_integer().
make32bit(A)->
	V1 = <<A:32>>,
	<<V2:32>> = V1,
	V2.

-spec mid_rand(float(),float()) -> float().
mid_rand(Middle,Max)->
	Delta=Max-Middle,
	(Middle-Delta)+rand:uniform(trunc(2*Delta)).
