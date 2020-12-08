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

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-export([start/4,mqttserver_worker/2,mqttserver_worker_secure/2,mqttserver_processor_init/2,mqttserver_processor/2]).

-spec start(CAName::binary(),Id::binary(),Configuration::gen_configuration(), ManagerPid::pid())->ok.
start(CAName,Id,Configuration,ManagerPid)->
	#{ num_listeners := NumListeners, port := Port , secure := Secure } = Configuration,
	{ok,ServiceConfiguration} = inventory:get_server(CAName,Id),
	{ ok , ListenSocket } = case Secure of
		                        false ->
			                        gen_tcp:listen(Port,[
				                        {active,false},
				                        {reuseaddr,true},
				                        {mode,binary} ]);
		                        true ->
			                        ssl:listen(Port,[
				                        %% {log_level,debug},
				                        {session_tickets,stateless},
				                        {mode,binary},
				                        {versions,['tlsv1.2','tlsv1.3']},
				                        {active,false},
				                        {reuseaddr,true},
				                        {cert,ServiceConfiguration#server_info.cert},
				                        {key,ServiceConfiguration#server_info.key}])
	                        end,
	Pids = case Secure of
		       false -> start_listeners(NumListeners,ListenSocket,ManagerPid);
		       true -> start_listeners_secure(NumListeners,ListenSocket,ManagerPid)
	       end,
	mqtt_server_manager:register_pids(Id,Pids).

start_listeners(NumListeners,ListenSocket,ParentPid)->
	start_listeners(NumListeners,ListenSocket,[],ParentPid).

start_listeners(0,_,Pids,_)->
	Pids;
start_listeners(NumListeners,ListenSock,Pids,ParentPid)->
	Pid = spawn(?MODULE,mqttserver_worker,[ListenSock,ParentPid]),
	start_listeners(NumListeners-1,ListenSock,[Pid|Pids],ParentPid).

start_listeners_secure(NumListeners,ListenSocket,ParentPid)->
	start_listeners_secure(NumListeners,ListenSocket,[],ParentPid).

start_listeners_secure(0,_,Pids,_)->
	Pids;
start_listeners_secure(NumListeners,ListenSock,Pids,ParentPid)->
	%% io:format("Starting listener...~n"),
	Pid = spawn(?MODULE,mqttserver_worker_secure,[ListenSock,ParentPid]),
	start_listeners_secure(NumListeners-1,ListenSock,[Pid|Pids],ParentPid).

mqttserver_worker(ListenSock,ParentPid)->
	?L_IA("Server ~p starting to listen.",[self()]),
	case gen_tcp:accept(ListenSock) of
		{ok,Socket} ->
			%% io:format("Received connection...~n"),
			mqtt_server_manager:increase_session(ParentPid,ListenSock),
			Pid = spawn(?MODULE,mqttserver_processor_secure_init,[Socket,#mqtt_processor_state{listener_pid = self(),
			                                                                                   parent_pid = ParentPid,
			                                                                                   peer_ip = inet:peername(Socket),
			                                                                                   secure = false,
			                                                                                   bytes_left = <<>>,
			                                                                                   module = gen_tcp,
			                                                                                   socket = Socket,
			                                                                                   version = undefined
			}]),
			_ = gen_tcp:controlling_process(Socket,Pid),
			mqttserver_worker(ListenSock,ParentPid);
		Error ->
			?L_IA("accept failed - server shutting down: ~p~n",[Error]),
			ok
	end.

mqttserver_worker_secure(ListenSock,ParentPid)->
	?L_IA("Server ~p starting to listen.",[self()]),
	case ssl:transport_accept(ListenSock) of
		{ok,Socket} ->
			%% io:format("Accepted..."),
			_=case ssl:handshake(Socket) of
				  { ok, SslSocket } ->
					  mqtt_server_manager:increase_session(ParentPid,ListenSock),
					  Pid = spawn(?MODULE,mqttserver_processor_init,[SslSocket,#mqtt_processor_state{listener_pid = self(),
					                                                                                 parent_pid = ParentPid,
					                                                                                 peer_ip = ssl:peername(SslSocket),
					                                                                                 secure = true,
					                                                                                 bytes_left = <<>>,
					                                                                                 module = ssl,
					                                                                                 socket = SslSocket,
					                                                                                 version = undefined
					  }]),
					  ssl:controlling_process(SslSocket,Pid);
				  Error ->
					  io:format("Handshake error: ~p~n...",[Error]),
					  ?L_IA("SSL handshake failed. ~p",[Error]),
					  ssl:close(Socket)
			  end,
			mqttserver_worker_secure(ListenSock,ParentPid);
		Error ->
			%% io:format("Accepted error: ~p~n...",[Error]),
			?L_IA("accept failed - server shutting down: ~p~n",[Error]),
			ok
	end.

mqttserver_processor_init(Socket,#mqtt_processor_state{ secure = false }=State)->
	_=inet:setopts(Socket,[{active,true}]),
	mqttserver_processor(Socket,State);
mqttserver_processor_init(Socket,#mqtt_processor_state{ secure = true }=State)->
	_=ssl:setopts(Socket,[{active,true}]),
	mqttserver_processor(Socket,State).

mqttserver_processor(Socket,#mqtt_processor_state{ secure = false }=State)->
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
	end;
mqttserver_processor(Socket,#mqtt_processor_state{ secure = true }=State)->
	receive
		{ssl,Socket,Data} ->
			%% io:format("Received ~p bytes.~n",[size(Data)]),
			FullData = <<(State#mqtt_processor_state.bytes_left)/binary,Data/binary>>,
			{ ok, NewState } = mqtt_process:process(State#mqtt_processor_state{ bytes_left = FullData }),
			mqtt_server_manager:set_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self(),NewState#mqtt_processor_state.stats),
			mqttserver_processor(Socket,NewState);
		{ssl_closed,Socket} ->
			mqtt_server_manager:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			mqtt_server_manager:delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
			?L_IA("Socket ~w closed [~w]~n",[Socket,self()]),
			ok;
		Anything ->
			io:format("Anything ->~p~n",[Anything]),
			mqttserver_processor(Socket,State)
	end.

