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

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/mqtt_definitions.hrl").

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
                      errors=0::integer(),
                      internal_messages=0::integer(),
											disconnects = 0 :: integer(),
											connects = 0 :: integer(),
											t1 = 0 :: integer(),
											connect_time = 0 :: integer(),
											messages=0::integer()}).

-spec start(CAName::binary(),Id::binary(),Configuration::gen_configuration_b(), ManagerPid::pid()) -> no_return().
start(CAName,Id,Configuration,ManagerPid)->
	#{ <<"broker">> := Broker, <<"compress">> := Compress, <<"port">> := Port, <<"topics">> := Topics } = Configuration,
	%% io:format(">>>>Trying topic: ~p  connect to: ~p:~p~n",[Topics,Broker,Port]),
	NewConfig = #{ broker => Broker, compress => Compress,
	            port => list_to_integer(binary_to_list(Port)), topics => Topics },
	{ok,DeviceConfiguration} = inventory:get_client(CAName,Id),
	full_start(#client_state{
		id = Id,
		caname = CAName,
		manager_pid = ManagerPid,
		details = DeviceConfiguration,
		configuration = NewConfig,
		broker = Broker,
		port = list_to_integer(binary_to_list(Port)),
		topics = Topics,
		compress = Compress}).

-spec full_start(State::#client_state{})->no_return().
full_start(State)->
	T1 = os:system_time(),
	NewState = case ssl:connect(binary_to_list(State#client_state.broker),
		                  State#client_state.port,
	                     [{session_tickets,auto},{versions, ['tlsv1.2','tlsv1.3']},
	                      {cert,State#client_state.details#client_info.cert},
												{key,State#client_state.details#client_info.key},
												{cacerts,[State#client_state.details#client_info.cacert]},
												{active,false},binary]) of
								{ok,SSLSocket} ->
									io:format(">>>Connected~n"),
									RS = run_client(SSLSocket,State#client_state{ connects = State#client_state.connects+1, t1 = T1 }),
									utils:do(State#client_state.keep_alive_ref =/= undefined,{timer,cancel,[State#client_state.keep_alive_ref]}),
									ssl:close(SSLSocket),
									RS#client_state{ disconnects = State#client_state.disconnects +1 };
								{error,_}=Error->
									io:format(">>>Cannot connect: ~p~n",[Error]),
									?L_IA("MQTT Client cannot connect: ~p.",[Error]),
									State
							end,
	timer:sleep(5000),
	full_start(NewState).

-spec run_client(Socket::ssl:sslsocket(),CS::#client_state{}) -> #client_state{}.
run_client(Socket,CS)->
	%% "/ap/sim1-1-000050_SIM1000050/opensync"
	RealSerial = case string:tokens(binary_to_list(CS#client_state.topics),"/") of
		             [_,Serial,_] -> list_to_binary(Serial);
								 _ -> CS#client_state.details#client_info.serial
	             end,

	C = #mqtt_connect_variable_header{
		protocol_version = ?MQTT_PROTOCOL_VERSION_3_11,
		username_flag = 0,
		password_flag = 0,
		will_retain_flag = 0,
		will_qos_flag = 0,
		will_flag = 0 ,
		clean_start_flag = 1,
		client_identifier = RealSerial,
		keep_alive = 180	},
	M = #mqtt_msg{ variable_header = C},
	ConnectMessage = mqtt_message:encode(M),
	io:format(">>>>CONNECTMESSAGE: ~p~n",[ConnectMessage]),
	_=case ssl:send(Socket,ConnectMessage) of
		ok ->
			Res = ssl:setopts(Socket,[{active,true}]),
			io:format("Sent connection message. Res=~p..~n",[Res]),
			CS#client_state.manager_pid ! { stats, connection , 1 },
			manage_connection(Socket,CS#client_state{ current_state = waiting_for_hello });
		Error ->
			io:format("Failed connection message: ~p...~n",[Error]),
			?L_IA("MQTT_CONNECTION for ID=~p failed (~p)",[CS#client_state.id,Error])
	end,
	CS#client_state.manager_pid ! { stats, connection , -1 },
	CS.

-spec manage_connection(Socket::ssl:sslsocket(),CS::#client_state{}) -> no_return().
manage_connection(Socket,CS) ->
	receive
		{ssl,Socket,Data} ->
			io:format("Received ~p bytes: ~p.~n",[size(Data),Data]),
			case manage_state(Data,CS) of
				{ none, NewState } ->
					manage_connection(Socket,NewState);
				{ Response, NewState } ->
					_=ssl:send(Socket,Response),
					manage_connection(Socket,NewState)
			end;
		{ssl_closed,Socket} ->
			?L_I("MQTT socket closed by server"),
			io:format("Closing socket.~n");
		{ send_data,Data } ->
			io:format("Received a message to return some data: ~p~n",[Data]),
			_ = ssl:send(Socket,Data),
			manage_connection(Socket,CS#client_state{ internal_messages = 1+CS#client_state.internal_messages });
		Anything ->
			io:format("Unknown message: ~p.~n",[Anything]),
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
	io:format("SENDING PING...~n"),
	Blob = mqtt_message:encode(#mqtt_msg{ variable_header = #mqtt_pingreq_variable_header_v4{} }),
	Pid ! { send_data, Blob }.

process( M, CS ) when is_record(M,mqtt_connack_variable_header_v4) ->
	case M#mqtt_connack_variable_header_v4.connect_reason_code of
		0 ->
			{ok,TRef} = timer:apply_interval(60*1000,?MODULE,send_ping,[self()]),
			ConnectTime = os:system_time() - CS#client_state.t1,
			CS#client_state.manager_pid ! { stats, connect_time , ConnectTime },
			{none,CS#client_state{ messages = 1+CS#client_state.messages, current_state = connected , keep_alive_ref = TRef, connect_time = ConnectTime, t1 = 0 }};
		Error ->
			io:format("Device ~p gets error ~p when trying to connect.",[CS#client_state.id,Error]),
			?L_IA("Device ~p gets error ~p when trying to connect.",[CS#client_state.id,Error]),
			{none,CS#client_state{ messages = 1+CS#client_state.messages, errors = 1+CS#client_state.errors }}
	end;
process( M, CS ) when is_record(M,mqtt_pingreq_variable_header_v4) ->
	Response = mqtt_message:encode(#mqtt_msg{ variable_header = #mqtt_pingresp_variable_header_v4{} }),
	{ Response, CS#client_state{ messages = 1+CS#client_state.messages }}.

c_cfg()->
	#{<<"broker">> => <<"renegademac.arilia.com">>,
	  <<"compress">> => <<"zlib">>,
	  <<"port">> => <<"1883">>,
	  <<"qos">> => <<"0">>,
	  <<"remote_log">> => <<"1">>,
	  <<"topics">> => <<"/ap/Open_AP_21P10C69717951/opensync">>}.

s_cfg()->
	#{port => 1883,
		num_listeners => 10,
		secure => true }.

t1()->
	mqtt_server_manager:start_server(<<"sim1">>,<<"mqtt-1">>,s_cfg()).

t2()->
	mqtt_client_manager:start_client(<<"sim1">>,<<"sim1-1-000010">>,c_cfg()).

