%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 18. November 2020 @ 15:29:05
%%%-----------------------------------------------------------------------------
-module(ovsdb_client_handler).
-author("helge").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([set_configuration/1, start/0, stop/0, pause/0, resume/0, cancel/0, report/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-record(hdl_state, {
	clients = [] :: [{Id::term(),pid()}],
	config :: term()
}).



%%%============================================================================
%%% API
%%%============================================================================


-spec start_link () -> {ok, Pid} | {error, Reason} when
		Pid :: pid(),
		Reason :: term().

start_link () ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).



-spec set_configuration (Cfg) -> {ok, set} | {error, Reason} when
		Cfg :: term(),
		Reason :: term().

set_configuration (Cfg) ->
	gen_server:call(?SERVER,{set_config, Cfg}).



-spec start () -> {ok, started} | {timeout, Reason} | {error, Reason} when
		Reason :: term().

start () ->
	gen_server:call(?SERVER,start_sim).



-spec stop () -> ok.

stop () ->
	gen_server:cast(?SERVER,stop_sim).



-spec pause () -> {ok, paused} | {timeout, Reason} | {error, Reason} when
		Reason :: term().

pause () ->
	gen_server:call(?SERVER,pause_sim).



-spec resume () -> {ok, resumed} | {timeout, Reason} | {error, Reason} when
		Reason :: term().

resume () ->
	gen_server:call(?SERVER,resume_sim).



-spec cancel () -> {ok, cancelled} | {timeout, Reason} | {error, Reason} when
		Reason :: term().

cancel () ->
	gen_server:call(?SERVER,cancel_sim).



-spec report () -> {ok, Report} | {error, Reason} when
		Report :: term(),
		Reason :: term().

report () ->
	gen_server:call(?SERVER,get_report).



%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init (Args) -> {ok, State} when
		Args :: term(),
		State :: #hdl_state{}.

init (_) ->
	{ok, #hdl_state{}}.




-spec handle_cast (Request, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Request :: term(),
		State :: #hdl_state{},
		NewState :: #hdl_state{},
		Reason :: term().

handle_cast (_,State) ->
	{noreply, State}.




-spec handle_call (Request, From, State) -> {reply, Reply, NewState} | {stop, Reason, Reply, NewState} when
		Request :: term(),
		From :: {pid(),Tag::term()},
		State :: #hdl_state{},
		Reply :: term(),
		Reason :: term(),
		NewState :: #hdl_state{}.

handle_call (_, _, State) ->
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} when
		Msg :: term(),
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

handle_info(_, State) ->
	{noreply, State}.




-spec terminate (Reason, State) -> ok when
		Reason :: shutdown | {shutdown, term()} | normal,
		State :: #hdl_state{}.

terminate (_Reason, _State) ->
	ok.




-spec code_change (OldVersion, OldState, Extra) -> {ok, NewState} when
		OldVersion :: term(),
		OldState ::#hdl_state{},
		Extra :: term(),
		NewState :: #hdl_state{}.

code_change (_,OldState,_) ->
	{ok, OldState}.




%%%============================================================================
%%% internal functions
%%%============================================================================

