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
-export([start_link/0,creation_info/0,increase_session/2,decrease_session/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

%% internal exports for spawn
-export([mqttserver_worker/2,mqttserver_processor/2]).

-define(SERVER, ?MODULE).

-record(mqtt_server_state, { server_port, num_servers, server_pids, main_listen_socket, session_count = maps:new() }).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[		#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

increase_session(Pid,ListenerPid)->
	gen_server:cast(Pid,{increase,ListenerPid}).
decrease_session(Pid,ListenerPid)->
	gen_server:cast(Pid,{decrease,ListenerPid}).

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
	ServerPort = application:get_env(?MQTT_APP,server_port,?MQTT_DEFAULT_SERVER_PORT),
	NumServers = application:get_env(?MQTT_APP,num_servers,10),

	{ ok , ListenSocket } = gen_tcp:listen(ServerPort,[{active,false},{packet,2}]),

	Pids = start_servers(NumServers,ListenSocket,self()),

	{ok, #mqtt_server_state{ server_port = ServerPort, num_servers = NumServers, main_listen_socket = ListenSocket, server_pids = Pids}}.

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
handle_cast({decrease,Pid}, State) ->
	Counter = maps:get(Pid,State#mqtt_server_state.session_count,0),
	M2 = maps:put(Pid,Counter+1,State#mqtt_server_state.session_count),
	{noreply, State#mqtt_server_state{session_count = M2}};
handle_cast({increase,Pid}, State) ->
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
start_servers(NumServers,ListenSocket,ParentPid)->
	start_servers(NumServers,ListenSocket,[],ParentPid).

start_servers(0,_,Pids,_)->
	Pids;
start_servers(NumServers,ListenSock,Pids,ParentPid)->
	Pid = spawn(?MODULE,mqttserver_worker,[ListenSock,ParentPid]),
	start_servers(NumServers-1,ListenSock,[Pid|Pids],ParentPid).

mqttserver_worker(ListenSock,ParentPid)->
	lager:info("Server ~p starting to listen.",[self()]),
	case gen_tcp:accept(ListenSock) of
		{ok,Socket} ->
			mqtt_server:increase_session(ParentPid,ListenSock),
			Pid = spawn(?MODULE,mqttserver_processor,[Socket,#mqtt_processor_state{listener_pid = self(), parent_pid = ParentPid, peer_ip = inet:peername(Socket)}]),
			gen_tcp:controlling_process(Socket,Pid),
			mqttserver_worker(ListenSock,ParentPid);
		Error ->
			lager:info("accept failed - server shutting down: ~p~n",[Error]),
			ok
	end.

mqttserver_processor(Socket,State)->
	inet:setopts(Socket,[{active,once}]),
	receive
		{tcp,Socket,Data} ->
			Answer = mqttserver_process:process(Data,State), % Not implemented in this example
			gen_tcp:send(Socket,Answer),
			mqttserver_processor(Socket,State);
		{tcp_closed,Socket} ->
			mqtt_server:increase_session(State#mqtt_processor_state.parent_pid,State#mqtt_processor_state.listener_pid),
			lager:info("Socket ~w closed [~w]~n",[Socket,self()]),
			ok
	end.
