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

-include("../include/common.hrl").

-define(SERVER, ?MODULE).
-define(MAX_STARTUP_TIME,10000).

%% API
-export([start_link/0]).
-export([set_configuration/1, start/1, stop/1, pause/1, resume/1, cancel/1, report/0]).
-export([ap_status/2]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-type client_states() :: #{UUID::string() := {available | dead | ovsdb_ap:ap_status(), none | pid()}}.

-record (command,{
	cmd :: start | stop | pause | cancel,
	refs :: [UUID::string()]
}).

-type command() :: #command{}.

-record (hdl_state, {
	clients = #{} :: client_states(),
	client_pid_map = #{} :: #{pid() := UUID::string()},
	cmd_queue = [] :: [command()],
	config = #{} :: #{},
	timer :: owls_timers:tms()
}).



%%%============================================================================
%%% HANDLER - API
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
	gen_server:call(?SERVER,{cmd_start, What}).



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
%%% CLIENT CALLBACK API
%%%============================================================================

-spec ap_status (Status, Id) -> ok when
		Status :: ovsdb_ap:ap_status(),
		Id :: UUID::string().

ap_status (Status, Id) ->
	gen_server:cast(?SERVER,{status,Status,Id}).




%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init (Args) -> {ok, State} when
		Args :: term(),
		State :: #hdl_state{}.

init (_) ->
	process_flag(trap_exit, true),
	{ok, #hdl_state{timer=owls_timers:new(millisecond)}}.




-spec handle_cast (Request, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Request :: term(),
		State :: #hdl_state{},
		NewState :: #hdl_state{},
		Reason :: term().

handle_cast (execute, State) ->
	{noreply, execute_cmd(State)};

handle_cast ({status,Status,Id}, State) ->
	{noreply, update_client_status(Status,Id,State)};

handle_cast (_,State) ->
	{noreply, State}.




-spec handle_call (Request, From, State) -> {reply, Reply, NewState} | {stop, Reason, Reply, NewState} when
		Request :: term(),
		From :: {pid(),Tag::term()},
		State :: #hdl_state{},
		Reply :: term(),
		Reason :: term(),
		NewState :: #hdl_state{}.

handle_call ({set_config, Cfg},_,State) ->
	{reply, ok, apply_config(State,Cfg)};
	
handle_call ({cmd_start, How},_,State) ->
	{reply, ok, cmd_startup_sim(State,How)};

handle_call (_, _, State) ->
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} when
		Msg :: term(),
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

handle_info({'EXIT',From,normal},State) ->
	case maps:find(From,State#hdl_state.client_pid_map) of
		{ok, Id} ->
			Cpm = maps:remove(From,State#hdl_state.client_pid_map),
			{noreply, update_client_status(available,Id,State#hdl_state{client_pid_map=Cpm})};
		error ->
			State
	end;

handle_info({'EXIT',From,_},State) ->
	case maps:find(From,State#hdl_state.client_pid_map) of
		{ok, Id} ->
			Cpm = maps:remove(From,State#hdl_state.client_pid_map),
			{noreply, update_client_status(dead,Id,State#hdl_state{client_pid_map=Cpm})};
		error ->
			State
	end;
	
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

%--------apply_config/2------------------translates configuration into state

-spec apply_config (State, Cfg) -> NewState when
		State :: #hdl_state{},
		Cfg :: #{},
		NewState :: #hdl_state{}.

apply_config (State, _Cfg) ->
	State#hdl_state{clients=#{"a68d41fa-dd12-4fb7-bc44-e834667280b4" => {available,none}}}.




%--------update_client_status/3-----------update the state of a client in the clients map

-spec update_client_status (ClientState, ClientId, HandlerState) -> NewHandlerSate when
		ClientState :: available | dead | ovsdb_ap:ap_status(),
		ClientId :: string(),
		HandlerState :: #hdl_state{},
		NewHandlerSate :: #hdl_state{}.

update_client_status (ClS, Id, #hdl_state{clients=C}=State) ->
	case maps:find(Id,C) of
		{ok, {_,P}} ->
			State#hdl_state{clients=C#{Id:={ClS,P}}};
		error ->
			State
	end.




%--------get_clients_in_state/3----------filter all clients with state

-spec get_clients_in_state (State, Clients, Candidates) -> Available when
		State :: available | dead | ovsdb_ap:ap_status(),
		Clients :: client_states(),
		Candidates :: all | [UUID::string()],
		Available :: [UUID::string()].

get_clients_in_state (State, Clients, all) ->
	[K || {K,{V,_}} <- maps:to_list(Clients), (V=:=State)];

get_clients_in_state (State, Clients, Candidates) ->
	[K || {K,{V,_}} <- maps:to_list(Clients), (V=:=State) and lists:member(K,Candidates)].



%%-----------------------------------------------------------------------------
%% command queue handling


%--------execute_cmd/1-------------------executes the first command in queue

-spec execute_cmd (State) -> NewState when
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

execute_cmd (#hdl_state{cmd_queue=[]}=State) ->
	State;

execute_cmd (#hdl_state{cmd_queue=Q}=State) ->
	[Cmd|RemCmds] = lists:reverse(Q),
	AltState = State#hdl_state{cmd_queue=lists:reverse(RemCmds)},
	case Cmd of	
		#command{cmd=start, refs=R} ->
			cmd_start_clients(R, AltState);

		#command{cmd=Ukn} ->
			?L_E(?DBGSTR("Unknown command '~s' in queue",[Ukn])),
			AltState
	end.
	



%--------cmd_startup_sim/2--------------lauches the start-up sequence of simulation clients

-spec cmd_startup_sim (State, How) -> NewState when
		State :: #hdl_state{},
		How :: all | [UUID::string()],
		NewState :: #hdl_state{}.

cmd_startup_sim (State, How) ->
	NewState = #hdl_state{timer=owls_timers:mark("startup",State#hdl_state.timer)},
	ToLaunch = get_clients_in_state (available,State#hdl_state.clients, How) ++ 
				get_clients_in_state (dead,State#hdl_state.clients, How),
	ToStart = get_clients_in_state (ready,State#hdl_state.clients, How),
	if
		length(ToLaunch) + length(ToStart) > 0 ->
			gen_server:cast(self(),execute),
			Cmds = [#command{cmd=start,refs=ToStart ++ ToLaunch} | NewState#hdl_state.cmd_queue],
			cmd_lauch(ToLaunch,NewState#hdl_state{cmd_queue=Cmds});
		true ->
			?L_E(?DBGSTR("start command issued with no clients available or ready to start")),
			NewState
	end.




%--------cmd_launch/2--------------------lauch processes for clients (synchrounsly)

-spec cmd_lauch (ToLauch,State) -> NewState when
		ToLauch :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

cmd_lauch (ToLauch, State) ->
	%% @TODO: pass in the real manager
	L = [ {V, ovsdb_ap:launch(V,self())} || V <- ToLauch],
	Pm = maps:merge(State#hdl_state.client_pid_map, maps:from_list([{P,Id} || {Id, {ok, P}} <- L])),
	C = maps:merge(State#hdl_state.clients, maps:from_list([{V,{init,P}} || {V, {ok, P}} <- L])),
	T = owls_timers:mark("launched",State#hdl_state.timer),
	State#hdl_state{clients=C, client_pid_map=Pm, timer=T}.




%--------cmd_start_clients/2-------------starts the simulation of the cliens (in ready state)

-spec cmd_start_clients (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

cmd_start_clients (Refs, #hdl_state{timer=T,cmd_queue=Q}=State) ->	 
	Elapsed = owls_timers:delta("launched",T),
	if
		Elapsed > ?MAX_STARTUP_TIME * length(Refs) ->
			?L_E(?DBGSTR("Getting ~B clients ready took too long (~s)",[length(Refs),owls_timers:fmt_duration(Elapsed,T)])),
			?L_I(?DBGSTR("Cancelling simulation start request")),
			Cmds = lists:reverse([#command{cmd=cancel, refs=Refs}|lists:reverse(Q)]),
			gen_server:cast(self(),execute),
			State#hdl_state{cmd_queue=Cmds};
		true ->
			Ready = get_clients_in_state (ready,State#hdl_state.clients,Refs),
			check_client_startup(Ready,Refs,State)
	end.

	


-spec check_client_startup (Ready, ToStart, State) -> NewState when
		Ready :: [UUID::string()],
		ToStart :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

check_client_startup (Ready, ToStart, #hdl_state{clients=C,timer=T,cmd_queue=Q}=State) ->
	io:format("checking for ready clients~n"),
	if
		length(Ready) == length(ToStart) ->
			lists:foreach(fun(I) -> #{I:={ready,P}}=C, ovsdb_ap:start_ap(P) end, Ready),
			State#hdl_state{timer=owls_timers:mark("sim_started",T)};
		true ->
			Cmds = lists:reverse([#command{cmd=start, refs=ToStart}|lists:reverse(Q)]),
			{ok, _} = timer:apply_after(1000,gen_server,cast,[self(),execute]),
			State#hdl_state{cmd_queue=Cmds}
	end.

