%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 9:28 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_server).
-author("stephb").

-behaviour(gen_server).

-include("../include/mqtt_definitions.hrl").
-include("../include/internal.hrl").

%% API
-export([start_link/0,creation_info/0,increase_session/2,decrease_session/2,set_session_stats/4,delete_session_stats/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% internal exports for spawn
-export([mqttserver_worker/2,mqttserver_worker_secure/2,mqttserver_processor_init/2]).

-define(SERVER, ?MODULE).
-define(DBG, io:format("~s: ~p~n",[?FUNCTION_NAME,?LINE])).

-record(mqtt_server_state, { server_port, num_servers, num_listeners, server_pids, main_listen_socket, session_count = maps:new() , secure , stats = maps:new() }).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

increase_session(Pid,ListenerPid)->
	gen_server:cast(Pid,{increase,ListenerPid}).
decrease_session(Pid,ListenerPid)->
	gen_server:cast(Pid,{decrease,ListenerPid}).
set_session_stats(Pid,ListenerPid,ChildPid,Stats)->
	gen_server:cast(Pid,{set_stats,ListenerPid,ChildPid,Stats}).
delete_session_stats(Pid,ListenerPid,ChildPid)->
	gen_server:cast(Pid,{delete_stats,ListenerPid,ChildPid}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #mqtt_server_state{}} | {ok, State :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	ServerPort = application:get_env(?OWLS_APP,server_port,?MQTT_DEFAULT_SERVER_PORT),
	NumListeners = application:get_env(?OWLS_APP,num_listeners,10),
	NumServers = application:get_env(?OWLS_APP,num_servers,10),
	Secure = application:get_env(?OWLS_APP,secure,false),
	CertFile = application:get_env(?OWLS_APP,certfile,""),
	_CaCert = application:get_env(?OWLS_APP,cacertfile,""),
	KeyFile = application:get_env(?OWLS_APP,keyfile,""),
	{ ok , ListenSocket } = case Secure of
														false ->
															gen_tcp:listen(ServerPort,[
																		   {active,false},
																		   {reuseaddr,true},
																		   {mode,binary} ]);
														true ->
															ssl:listen(ServerPort,[
																%% {log_level,debug},
													      {session_tickets,stateless},
													      {mode,binary},
																{versions,['tlsv1.2','tlsv1.3']},
																{active,false},
																{reuseaddr,true},
																{certfile,CertFile},
																{keyfile,KeyFile}])
	                        end,
	Pids = case Secure of
					 false -> start_listeners(NumListeners,ListenSocket,self());
					 true -> start_listeners_secure(NumListeners,ListenSocket,self())
	       end,
	{ok, #mqtt_server_state{ server_port = ServerPort,
		num_servers = NumServers,
		num_listeners = NumListeners,
		main_listen_socket = ListenSocket,
		server_pids = Pids,
		secure = Secure }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #mqtt_server_state{}) ->
	{reply, Reply :: term(), NewState :: #mqtt_server_state{}} |
	{reply, Reply :: term(), NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{noreply, NewState :: #mqtt_server_state{}} |
	{noreply, NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #mqtt_server_state{}} |
	{stop, Reason :: term(), NewState :: #mqtt_server_state{}}).
handle_call(_Request, _From, State = #mqtt_server_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_server_state{}) ->
	{noreply, NewState :: #mqtt_server_state{}} |
	{noreply, NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_state{}}).
handle_cast({set_stats,ListenerPid,ChildPid,Stats}, State) ->
	io:format("Setting stats: ~p~n",[Stats]),
	NewStats = maps:put( {ListenerPid,ChildPid},Stats,State#mqtt_server_state.stats),
	{noreply, State#mqtt_server_state{stats = NewStats}};
handle_cast({delete_stats,ListenerPid,ChildPid}, State) ->
	io:format("Deleting stats.~n"),
	NewStats = maps:remove( {ListenerPid,ChildPid},State#mqtt_server_state.stats),
	{noreply, State#mqtt_server_state{stats = NewStats}};
handle_cast({decrease,Pid}, State) ->
	io:format("Removing session~n"),
	Counter = maps:get(Pid,State#mqtt_server_state.session_count,0),
	M2 = maps:put(Pid,Counter+1,State#mqtt_server_state.session_count),
	{noreply, State#mqtt_server_state{session_count = M2}};
handle_cast({increase,Pid}, State) ->
	io:format("Adding session~n"),
	Counter = maps:get(Pid,State#mqtt_server_state.session_count,1),
	M2 = maps:put(Pid,Counter-1,State#mqtt_server_state.session_count),
	{noreply, State#mqtt_server_state{session_count = M2}}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_server_state{}) ->
	{noreply, NewState :: #mqtt_server_state{}} |
	{noreply, NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_state{}}).
handle_info(_Info, State = #mqtt_server_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #mqtt_server_state{}) -> term()).
terminate(_Reason, _State = #mqtt_server_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mqtt_server_state{},
		Extra :: term()) ->
	{ok, NewState :: #mqtt_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mqtt_server_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
	Pid = spawn(?MODULE,mqttserver_worker_secure,[ListenSock,ParentPid]),
	start_listeners_secure(NumListeners-1,ListenSock,[Pid|Pids],ParentPid).

mqttserver_worker(ListenSock,ParentPid)->
	lager:info("Server ~p starting to listen.",[self()]),
	case gen_tcp:accept(ListenSock) of
		{ok,Socket} ->
			mqtt_server:increase_session(ParentPid,ListenSock),
			Pid = spawn(?MODULE,mqttserver_processor_secure_init,[Socket,#mqtt_processor_state{listener_pid = self(),
				parent_pid = ParentPid,
				peer_ip = inet:peername(Socket),
				secure = false,
				bytes_left = <<>>,
				module = gen_tcp,
				socket = Socket,
				version = undefined
			}]),
			gen_tcp:controlling_process(Socket,Pid),
			mqttserver_worker(ListenSock,ParentPid);
		Error ->
			lager:info("accept failed - server shutting down: ~p~n",[Error]),
			ok
	end.

mqttserver_worker_secure(ListenSock,ParentPid)->
	lager:info("Server ~p starting to listen.",[self()]),
	case ssl:transport_accept(ListenSock) of
		{ok,Socket} ->
			case ssl:handshake(Socket) of
				{ ok, SslSocket } ->
					mqtt_server:increase_session(ParentPid,ListenSock),
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
					lager:info("SSL handshake failed. ~p",[Error]),
					ssl:close(Socket)
			end,
			mqttserver_worker_secure(ListenSock,ParentPid);
		Error ->
			lager:info("accept failed - server shutting down: ~p~n",[Error]),
			ok
	end.

mqttserver_processor_init(Socket,#mqtt_processor_state{ secure = false }=State)->
	inet:setopts(Socket,[{active,true}]),
	mqttserver_processor(Socket,State);
mqttserver_processor_init(Socket,#mqtt_processor_state{ secure = true }=State)->
	ssl:setopts(Socket,[{active,true}]),
	mqttserver_processor(Socket,State).

mqttserver_processor(Socket,#mqtt_processor_state{ secure = false }=State)->
	receive
		{tcp,Socket,Data} ->
			io:format("Received ~p bytes.~n",[size(Data)]),
			FullData = <<(State#mqtt_processor_state.bytes_left)/binary,Data/binary>>,
			case mqttserver_process:process(State#mqtt_processor_state{ bytes_left = FullData }) of
				{ ok, NewState } ->
					set_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self(),NewState#mqtt_processor_state.stats),
					mqttserver_processor(Socket,NewState);
				Error ->
					io:format("~p Error=~p~n",[?FUNCTION_NAME,Error]),
					gen_tcp:close(Socket)
			end;
		{tcp_closed,Socket} ->
			mqtt_server:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
			lager:info("Socket ~w closed [~w]~n",[Socket,self()]),
			ok;
		Anything ->
			io:format("Anything(not secure): ~p~n",[Anything]),
			mqttserver_processor(Socket,State)
	end;
mqttserver_processor(Socket,#mqtt_processor_state{ secure = true }=State)->
	receive
		{ssl,Socket,Data} ->
			io:format("Received ~p bytes.~n",[size(Data)]),
			FullData = <<(State#mqtt_processor_state.bytes_left)/binary,Data/binary>>,
			case mqttserver_process:process(State#mqtt_processor_state{ bytes_left = FullData }) of
				{ ok, NewState } ->
					set_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self(),NewState#mqtt_processor_state.stats),
					mqttserver_processor(Socket,NewState);
				Error ->
					io:format("~p Error=~p~n",[?FUNCTION_NAME,Error]),
					delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
					ssl:close(Socket)
			end;
		{ssl_closed,Socket} ->
			mqtt_server:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			delete_session_stats(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid,self()),
			lager:info("Socket ~w closed [~w]~n",[Socket,self()]),
			ok;
		Anything ->
			io:format("Anything ->~p~n",[Anything]),
			mqttserver_processor(Socket,State)
	end.


