%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 18. Nov 2020 1:04 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_server_handler).
-author("stephb").

-behaviour(gen_server).
-behaviour(gen_sim_client).

-include("../include/common.hrl").

%% API
-export([start_link/0,creation_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([set_configuration/1,start/2,stop/2,cancel/2,pause/2,restart/2]).

-define(SERVER, ?MODULE).

-record(mqtt_server_handler_state, {}).

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

-spec set_configuration( Configuration :: #{} ) -> ok | { error, Reason::term() }.
set_configuration( _Configuration ) ->
	ok.

-spec pause( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | { error, Reason::term() }.
pause( _UIDS,_Attributes ) ->
	ok.

-spec restart( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | { error, Reason::term() }.
restart( _UIDS,_Attributes ) ->
	ok.

-spec start( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | { error, Reason::term() }.
start( _UIDS,_Attributes ) ->
	ok.

-spec stop( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | { error, Reason::term() }.
stop( _UIDS,_Attributes ) ->
	ok.

-spec cancel( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | { error, Reason::term() }.
cancel( _UIDS,_Attributes ) ->
	ok.

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #mqtt_server_handler_state{}} | {ok, State :: #mqtt_server_handler_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #mqtt_server_handler_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #mqtt_server_handler_state{}) ->
	                 {reply, Reply :: term(), NewState :: #mqtt_server_handler_state{}} |
	                 {reply, Reply :: term(), NewState :: #mqtt_server_handler_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #mqtt_server_handler_state{}} |
	                 {noreply, NewState :: #mqtt_server_handler_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #mqtt_server_handler_state{}} |
	                 {stop, Reason :: term(), NewState :: #mqtt_server_handler_state{}}).
handle_call(_Request, _From, State = #mqtt_server_handler_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_server_handler_state{}) ->
	{noreply, NewState :: #mqtt_server_handler_state{}} |
	{noreply, NewState :: #mqtt_server_handler_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_handler_state{}}).
handle_cast(_Request, State = #mqtt_server_handler_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_server_handler_state{}) ->
	{noreply, NewState :: #mqtt_server_handler_state{}} |
	{noreply, NewState :: #mqtt_server_handler_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_server_handler_state{}}).
handle_info(_Info, State = #mqtt_server_handler_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #mqtt_server_handler_state{}) -> term()).
terminate(_Reason, _State = #mqtt_server_handler_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mqtt_server_handler_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #mqtt_server_handler_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mqtt_server_handler_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
