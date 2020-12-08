%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 9:28 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_server_manager).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-export([start_link/0,creation_info/0,increase_session/2,decrease_session/2,set_session_stats/4,delete_session_stats/3,
         start_server/3,stop_server/2,register_pids/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-define(DBG, io:format("~s: ~p~n",[?FUNCTION_NAME,?LINE])).

-record(mqtt_server_state, { server_port = 0 :: integer(),
                             num_listeners = 0 :: integer(),
														 server_pids = #{} :: #{ },
														 listener_pids = #{} :: #{ term() => [pid()]},
														 session_count = maps:new() ,
														 secure , stats = maps:new() }).

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

-spec start_server(CAName::binary()|string(),Id::binary()|string(),Configuration::gen_configuration() ) -> ok | generic_error().
start_server(CAName,Id,Configuration)->
	gen_server:call(?SERVER,{start_server,utils:safe_binary(CAName),utils:safe_binary(Id),Configuration}).

-spec stop_server(CAName::binary()|string(),Id::binary()|string()) -> ok | generic_error().
stop_server(CAName,Id)->
	gen_server:call(?SERVER,{start_server,utils:safe_binary(CAName),utils:safe_binary(Id)}).

-spec register_pids(Id::binary(),Pids::[pid()])-> ok.
register_pids(Id,Pids) ->
	gen_server:call(?SERVER,{register_pids,Id,Pids}).

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
	ServerPort = utils:app_env(server_port,?MQTT_DEFAULT_SERVER_PORT),
	NumListeners = utils:app_env(num_listeners,10),
	Secure = utils:app_env(secure,true),
	{ok, #mqtt_server_state{ server_port = ServerPort,
		num_listeners = NumListeners,
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
handle_call({register_pids,Id,Pids},_From,State=#mqtt_server_state{})->
	{reply,ok,State#mqtt_server_state{ listener_pids = maps:put( Id , Pids , State#mqtt_server_state.listener_pids )}};
handle_call({start_server,CAName,Id,Configuration},_From,State=#mqtt_server_state{})->
	Pid=spawn_link(mqtt_server,start,[CAName,Id,Configuration,self()]),
	{reply,ok,State#mqtt_server_state{ server_pids = maps:put(Id,Pid,State#mqtt_server_state.server_pids)}};
handle_call({stop_server,_CAName,_Id},_From,State=#mqtt_server_state{})->
	{reply,ok,State};
handle_call(_Request, _From, State = #mqtt_server_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_server_state{}) ->
	{noreply, NewState :: #mqtt_server_state{}} |
	{noreply, NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_state{}}).
handle_cast({set_stats,ListenerPid,ChildPid,Stats}, State) ->
	%% io:format("Setting stats: ~p~n",[Stats]),
	NewStats = maps:put( {ListenerPid,ChildPid},Stats,State#mqtt_server_state.stats),
	{noreply, State#mqtt_server_state{stats = NewStats}};
handle_cast({delete_stats,ListenerPid,ChildPid}, State) ->
	%% io:format("Deleting stats.~n"),
	NewStats = maps:remove( {ListenerPid,ChildPid},State#mqtt_server_state.stats),
	{noreply, State#mqtt_server_state{stats = NewStats}};
handle_cast({decrease,Pid}, State) ->
	%% io:format("Removing session~n"),
	Counter = maps:get(Pid,State#mqtt_server_state.session_count,0),
	M2 = maps:put(Pid,Counter+1,State#mqtt_server_state.session_count),
	{noreply, State#mqtt_server_state{session_count = M2}};
handle_cast({increase,Pid}, State) ->
	%% io:format("Adding session~n"),
	Counter = maps:get(Pid,State#mqtt_server_state.session_count,1),
	M2 = maps:put(Pid,Counter-1,State#mqtt_server_state.session_count),
	{noreply, State#mqtt_server_state{session_count = M2}}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_server_state{}) ->
	{noreply, NewState :: #mqtt_server_state{}} |
	{noreply, NewState :: #mqtt_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_state{}}).
handle_info({'DOWN', _Ref, process, Pid, _Why},State)->
	{noreply,State#mqtt_server_state{ server_pids = maps:remove(Pid,State#mqtt_server_state.server_pids)}};
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
