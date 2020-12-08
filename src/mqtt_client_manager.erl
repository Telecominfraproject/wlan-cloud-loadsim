%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2020 10:36 a.m.
%%%-------------------------------------------------------------------
-module(mqtt_client_manager).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3,creation_info/0,start_client/3,stop_client/2,is_running/2]).

-define(SERVER, ?MODULE).

-record(mqtt_client_manager_state, {
	client_configurations = #{} :: #{ binary() => { pid(),#{} }},
	client_pids = #{} :: #{ pid() => binary()} }).

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

-spec start_client(CAName::string()|binary(),Id::string()|binary(),Configuration::gen_configuration()) -> ok | generic_error().
start_client(CAName,Id,Configuration)->
	gen_server:call(?SERVER,{start_client,utils:safe_binary(CAName),utils:safe_binary(Id),Configuration}).

-spec stop_client(CAName::string()|binary(),Id::string()|binary()) -> ok | generic_error().
stop_client(CAName,Id)->
	gen_server:call(?SERVER,{stop_client,utils:safe_binary(CAName),utils:safe_binary(Id)}).

-spec is_running(CAName::string()|binary(),Id::string()|binary()) -> { ok , { Pid::pid, Configuration::gen_configuration()}} | generic_error().
is_running(CAName,Id)->
	gen_server:call(?SERVER,{is_running,utils:safe_binary(CAName),utils:safe_binary(Id)}).

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
	{ok, State :: #mqtt_client_manager_state{}} | {ok, State :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #mqtt_client_manager_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #mqtt_client_manager_state{}) ->
	                 {reply, Reply :: term(), NewState :: #mqtt_client_manager_state{}} |
	                 {reply, Reply :: term(), NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #mqtt_client_manager_state{}} |
	                 {noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #mqtt_client_manager_state{}} |
	                 {stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_call({start_client,CAName,Id,Configuration}, _From, State = #mqtt_client_manager_state{}) ->
	NewState = start_client_process(CAName,Id,Configuration,State),
	{reply, ok, NewState};
handle_call({stop_client,_CAName,Id}, _From, State = #mqtt_client_manager_state{}) ->
	case maps:get(Id,State#mqtt_client_manager_state.client_configurations,none) of
		none ->
			{reply, ok, State};
		{Pid,_} ->
			exit(Pid,kill),
			{reply, ok, State}
	end;
handle_call({is_running,_CAName,Id}, _From, State = #mqtt_client_manager_state{}) ->
	case maps:get(Id,State#mqtt_client_manager_state.client_configurations,none) of
		none ->
			{reply, { error, not_running }, State};
		{_Pid,_Configuration}=Client ->
			{reply, {ok,Client} , State}
	end;
handle_call(_Request, _From, State = #mqtt_client_manager_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_client_manager_state{}) ->
	{noreply, NewState :: #mqtt_client_manager_state{}} |
	{noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_cast(_Request, State = #mqtt_client_manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_client_manager_state{}) ->
	{noreply, NewState :: #mqtt_client_manager_state{}} |
	{noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_info({'DOWN', _Ref, process, Pid, _Why},State)->
	case maps:get(Pid,State#mqtt_client_manager_state.client_pids,none) of
		none ->
			{noreply,State};
		Id ->
			NewState = State#mqtt_client_manager_state{
				client_configurations = maps:remove(Id,State#mqtt_client_manager_state.client_configurations),
				client_pids = maps:remove(Pid,State#mqtt_client_manager_state.client_pids ) },
			{noreply,NewState}
	end;
handle_info(_Info, State = #mqtt_client_manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #mqtt_client_manager_state{}) -> term()).
terminate(_Reason, _State = #mqtt_client_manager_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mqtt_client_manager_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #mqtt_client_manager_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mqtt_client_manager_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_client_process(CAName::binary(),Id::binary(),Configuration::map(),State::#mqtt_client_manager_state{}) -> NewState::#mqtt_client_manager_state{}.
start_client_process(CAName,Id,Configuration,State)->
	io:format("MQTT-Client ~p starting.~n",[Id]),
	?L_IA("MQTT-Client ~p starting.",[Id]),
	Pid = spawn_link(mqtt_client,start,[CAName,Id,Configuration,self()]),
	NewState = State#mqtt_client_manager_state{
		client_configurations = maps:put(Id,{ Pid,Configuration} ,State#mqtt_client_manager_state.client_configurations),
		client_pids = maps:put(Pid,Id,State#mqtt_client_manager_state.client_pids )
	},
	io:format("MQTT-Client ~p starting. Already ~p running.~n",[Id,maps:size(NewState#mqtt_client_manager_state.client_pids)]),
	NewState.





