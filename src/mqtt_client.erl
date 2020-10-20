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

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(mqtt_client_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

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
	{ok, #mqtt_client_state{}}.

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