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
-behaviour(gen_sim_client).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([set_configuration/1, start/1, stop/1, pause/1, resume/1, cancel/1, report/0]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-record(hdl_state, {
	clients = [] :: [{Id::term(),pid()}],
	config = #{} :: #{}
}).



%%%============================================================================
%%% API
%%%============================================================================


-spec start_link () -> {ok, Pid} | {error, Reason} when
		Pid :: pid(),
		Reason :: term().

start_link () ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).



-spec set_configuration (Cfg) -> ok | {error, Reason} when
		Cfg :: #{},
		Reason :: term().

set_configuration (Cfg) ->
	gen_server:call(?SERVER,{set_config, Cfg}).



-spec start (What) -> ok | {error, Reason} when
		What :: all | [UUID::string()],
		Reason :: term().

start (What) ->
	gen_server:call(?SERVER,{start_sim, What}).



-spec stop (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

stop (What) ->
	gen_server:call(?SERVER,{stop_sim, What}).



-spec pause (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

pause (What) ->
	gen_server:call(?SERVER,{pause_sim, What}).



-spec resume (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

resume (What) ->
	gen_server:call(?SERVER,{resume_sim, What}).



-spec cancel (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

cancel (What) ->
	gen_server:call(?SERVER,{cancel_sim, What}).



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

