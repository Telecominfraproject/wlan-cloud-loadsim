%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2020 10:39 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_client).
-author("stephb").

-behaviour(gen_server).

-include("../include/mqtt_definitions.hrl").
-include("../include/common.hrl").

-define(DBG, io:format("~s: ~p~n",[?FUNCTION_NAME,?LINE])).

%% API
-export([start_link/0,start/1,stop/1,creation_info/0,start_client/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(mqtt_client_state, { cacertfile, certfile, keyfile, server_ip, server_port, clients = maps:new() }).

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

start(Id)->
	gen_server:call(?MODULE,{start_client,Id}).

stop(Id)->
	gen_server:call(?MODULE,{stop_client,Id}).


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
	{ok, State :: #mqtt_client_state{}} | {ok, State :: #mqtt_client_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	ServerIP = application:get_env(?OWLS_APP,client_server_ip,"localhost"),
	ServerPort = application:get_env(?OWLS_APP,client_server_port,?MQTT_DEFAULT_SERVER_PORT),
	CaCertFile = application:get_env(?OWLS_APP,client_cacertfile,"cacertfile.pem"),
	CertFile = application:get_env(?OWLS_APP,client_certfile,"cliemt.pem"),
	KeyFile = application:get_env(?OWLS_APP,client_keyfile,"client_dec.key"),

	{ok, #mqtt_client_state{ server_ip = ServerIP, server_port = ServerPort, cacertfile = CaCertFile ,
		certfile = CertFile, keyfile = KeyFile}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #mqtt_client_state{}) ->
	{reply, Reply :: term(), NewState :: #mqtt_client_state{}} |
	{reply, Reply :: term(), NewState :: #mqtt_client_state{}, timeout() | hibernate} |
	{noreply, NewState :: #mqtt_client_state{}} |
	{noreply, NewState :: #mqtt_client_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #mqtt_client_state{}} |
	{stop, Reason :: term(), NewState :: #mqtt_client_state{}}).
handle_call({start_client,Id}, _From, State = #mqtt_client_state{}) ->
	io:format("Starting client ~p~n",[Id]),
	Pid = spawn(?MODULE,start_client,[State,Id,self()]),
	Ref = erlang:monitor(process,Pid),
	Clients = maps:put(Id,{ Ref, Pid },State#mqtt_client_state.clients),
	{reply, ok, State#mqtt_client_state{clients = Clients}};
handle_call({stop_client,Id}, _From, State = #mqtt_client_state{}) ->
	case maps:get(Id,State#mqtt_client_state.clients,undefined) of
		undefined ->
			io:format("Unknown client ~pcannot be removed.~n",[Id]),
			{ reply, ok, State};
		{Ref,Pid} ->
			io:format("Stopping client ~p~n",[Id]),
			erlang:demonitor(Ref),
			exit(Pid,kill),
			NewClients = maps:remove(Id,State#mqtt_client_state.clients),
			{ reply, ok , State#mqtt_client_state{ clients = NewClients}}
	end;
handle_call(_Request, _From, State = #mqtt_client_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_client_state{}) ->
	{noreply, NewState :: #mqtt_client_state{}} |
	{noreply, NewState :: #mqtt_client_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_state{}}).
handle_cast(_Request, State = #mqtt_client_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_client_state{}) ->
	{noreply, NewState :: #mqtt_client_state{}} |
	{noreply, NewState :: #mqtt_client_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_state{}}).
handle_info({'EXIT',FromPid,Reason},State)->
	io:format("EXIT message: ~p ~p~n",[FromPid,Reason]),
	{noreply,State};
handle_info({'DOWN', Ref, process, Pid2, Reason},State)->
	io:format("DOWN message: ~p ~p ~p~n",[Ref,Pid2,Reason]),
	{noreply,State};
handle_info(_Info, State = #mqtt_client_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #mqtt_client_state{}) -> term()).
terminate(_Reason, _State = #mqtt_client_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mqtt_client_state{},
		Extra :: term()) ->
	{ok, NewState :: #mqtt_client_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mqtt_client_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_client(State,Id,ParentPid)->
	io:format("Client ~p starting: ~p...~n",[Id,State]),
	{ok,S}=gen_tcp:connect(State#mqtt_client_state.server_ip,State#mqtt_client_state.server_port,[],infinity),
	?DBG,
	{ok,SSL}=ssl:connect(S,[{cacertfile,State#mqtt_client_state.cacertfile},
		{certfile,State#mqtt_client_state.certfile},
		{keyfile,State#mqtt_client_state.keyfile}]),
	?DBG,
	manage_client(SSL,#{ id => Id, socket => SSL , parent_pid => ParentPid }).

manage_client(_SSL,_State)->
	io:format("Managing client...~n"),
	ok.
