%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 04. Dec 2020 3:37 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_server).
-author("stephb").

-compile({parse_transform, lager_transform}).

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-export([start/4,mqttserver_processor_init/2,mqttserver_processor_init_secure/2,mqttserver_processor/2,
	start_listen/4,start_listen_secure/4]).

-spec start(CAName::binary(),Id::binary(),Configuration::gen_configuration(), ManagerPid::pid())->ok.
start(_CAName,Id,#{ secure := true }=Configuration,ManagerPid)->
	#{ num_listeners := NumListeners, port := Port } = Configuration,
	{ok,ServiceConfiguration} = inventory:get_record(#server_info{name = Id}),
	SocketOptions = [
		{backlog,NumListeners},
		{session_tickets,stateless},
		{mode,binary},
		{versions,['tlsv1.2','tlsv1.3']},
		{active,false},
		{reuseaddr,true},
		{cert,ServiceConfiguration#server_info.cert},
		{key,ServiceConfiguration#server_info.key}],
		Pid = spawn_link(?MODULE,start_listen_secure,[Id, Port, SocketOptions,ManagerPid]),
		mqtt_server_manager:register_pids(Id,[Pid]);
start(_CAName,Id,#{ secure := false }=Configuration,ManagerPid)->
	#{ num_listeners := NumListeners, port := Port } = Configuration,
	SocketOptions = [
		{backlog,NumListeners},
    {active,false},
    {reuseaddr,true},
    {mode,binary}],
	Pid = spawn_link(?MODULE,start_listen,[Id, Port, SocketOptions,ManagerPid]),
	mqtt_server_manager:register_pids(Id,[Pid]).

start_listen( Id, Port, SocketOptions ,ParentId)->
	{ ok , ListenSocket } = gen_tcp:listen(Port,SocketOptions),
	listen_loop(Id,ListenSocket,ParentId).

start_listen_secure( Id, Port, SocketOptions ,ParentId)->
	{ ok , ListenSocket } = ssl:listen(Port,SocketOptions),
	listen_loop_secure(Id,ListenSocket,ParentId).

listen_loop(Id,ListenSock,ParentPid)->
	?L_IA("Server ~p starting to listen.",[Id]),
	case gen_tcp:accept(ListenSock) of
		{ok,Socket} ->
			%% io:format("Received connection...~n"),
		    mqtt_server_manager:increase_session(ParentPid,ListenSock),
		    Pid = spawn(?MODULE,mqttserver_processor_init,
		                [Socket,#mqtt_processor_state{listener_pid = self(),
		                                                 parent_pid = ParentPid,
		                                                 peer_ip = inet:peername(Socket),
		                                                 secure = true,
		                                                 bytes_left = <<>>,
		                                                 module = ssl,
		                                                 socket = Socket,
		                                                 version = ?MQTT_PROTOCOL_VERSION_3_11
		                }]),
		    _=gen_tcp:controlling_process(Socket,Pid),
		    listen_loop(Id,ListenSock,ParentPid);
    Error ->
	    io:format("SSL Handshake Error: ~p~n",[Error]),
	    listen_loop(Id,ListenSock,ParentPid)
  end.

listen_loop_secure(Id,ListenSock,ParentPid)->
	?L_IA("Server ~p starting to listen.",[Id]),
	case ssl:transport_accept(ListenSock) of
		{ok,Socket} ->
			%% io:format("Received connection...~n"),
			_ = case ssl:handshake(Socket) of
				    { ok, SslSocket } ->
					    mqtt_server_manager:increase_session(ParentPid,ListenSock),
					    Pid = spawn(?MODULE,mqttserver_processor_init_secure,
					                [SslSocket,#mqtt_processor_state{listener_pid = self(),
                           parent_pid = ParentPid,
                           peer_ip = ssl:peername(SslSocket),
                           secure = true,
                           bytes_left = <<>>,
                           module = ssl,
                           socket = SslSocket,
                           version = ?MQTT_PROTOCOL_VERSION_3_11
					    }]),
					    _=ssl:controlling_process(SslSocket,Pid),
							listen_loop_secure(Id,ListenSock,ParentPid);
						Error ->
							io:format("SSL Handshake Error: ~p~n",[Error]),
							listen_loop_secure(Id,ListenSock,ParentPid)
					end;
		Error ->
			?L_IA("accept failed - server shutting down: ~p~n",[Error]),
			listen_loop_secure(Id,ListenSock,ParentPid)
	end.

mqttserver_processor_init(Socket,#mqtt_processor_state{ secure = false }=State)->
	_=inet:setopts(Socket,[{active,true}]),
	mqttserver_processor(Socket,State).

mqttserver_processor_init_secure(Socket,State)->
	_=ssl:setopts(Socket,[{active,true}]),
	mqttserver_processor_secure(Socket,State).

mqttserver_processor(Socket,State)->
	receive
		{tcp,Socket,Data} ->
			%% io:format("Received ~p bytes.~n",[size(Data)]),
			FullData = <<(State#mqtt_processor_state.bytes_left)/binary,Data/binary>>,
			{ ok, NewState } = mqtt_process:process(State#mqtt_processor_state{ bytes_left = FullData }),
			mqtt_server_manager:set_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self(),NewState#mqtt_processor_state.stats),
			mqttserver_processor(Socket,NewState);
		{tcp_closed,Socket} ->
			mqtt_server_manager:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			mqtt_server_manager:delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
			?L_IA("Socket ~w closed [~w]~n",[Socket,self()]),
			ok;
		Anything ->
			io:format("Anything(not secure): ~p~n",[Anything]),
			mqttserver_processor(Socket,State)
	end.

mqttserver_processor_secure(Socket,State)->
	receive
		{ssl,Socket,Data} ->
			io:format("Received ~p bytes.~n",[size(Data)]),
			FullData = <<(State#mqtt_processor_state.bytes_left)/binary,Data/binary>>,
			{ ok, NewState } = mqtt_process:process(State#mqtt_processor_state{ bytes_left = FullData }),
			mqtt_server_manager:set_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self(),NewState#mqtt_processor_state.stats),
			mqttserver_processor_secure(Socket,NewState);
		{ssl_closed,Socket} ->
			mqtt_server_manager:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			mqtt_server_manager:delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
			?L_IA("Socket ~w closed [~w]~n",[Socket,self()]),
			ok;
		Anything ->
			io:format("Anything ->~p~n",[Anything]),
			mqttserver_processor_secure(Socket,State)
	end.

