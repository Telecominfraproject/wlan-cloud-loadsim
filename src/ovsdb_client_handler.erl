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
-define(STATS_INTERVAL,30000).

%% API
-export([start_link/0]).
-export([set_configuration/1, start/1, stop/1, pause/1, resume/1, cancel/1, report/0]).
-export([ap_status/2]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-type client_status() :: available | dead | ovsdb_ap:ap_status().
-type client_states() :: #{UUID::string() := {client_status(), none | pid()}}.
-type client_transition() :: {client_status(), TimeStamp::integer()}.

-record (command,{
	cmd :: clients_start | clients_stop | clients_pause | clients_resume | clients_cancel,
	refs :: [UUID::string()]
}).

-type command() :: #command{}.

-record (hdl_state, {
	clients = #{} :: client_states(),
	client_pid_map = #{} :: #{pid() := UUID::string()},
	client_trans = #{} :: #{UUID::string() := [client_transition(),...]},
	cmd_queue = [] :: [command()],
	config = #{} :: #{},
	timer :: owls_timers:tms()
}).


-record (statistics, {
	seq :: non_neg_integer(),				%% sequence number of record
	stamp :: integer(), 					%% os timestmap
	configured :: non_neg_integer(),		%% configured clients
	running :: non_neg_integer(),			%% actively running clients
	paused :: non_neg_integer(),			%% currently paused clients
	dropped :: non_neg_integer(),			%% number of dropped connections since last interval
	avg_latency :: non_neg_integer(),		%% avg. latency in last interval
	avg_speed :: non_neg_integer()			%% avg. communication speed during last interval
}).


%%%============================================================================
%%% HANDLER - API
%%%============================================================================


-spec start_link () -> {ok, Pid} | {error, Reason} when
		Pid :: pid(),
		Reason :: term().

start_link () ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).



%%% gen_sim_clients behaviour


-spec set_configuration (Cfg) -> ok | {error, Reason} when
		Cfg :: #{},
		Reason :: term().

set_configuration (Cfg) ->
	gen_server:call(?SERVER,{set_config, Cfg}).



-spec start (What) -> ok | {error, Reason} when
		What :: all | [UUID::string()],
		Reason :: term().

start (What) ->
	gen_server:call(?SERVER,{api_cmd_start, What}).



-spec stop (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

stop (What) ->
	gen_server:call(?SERVER,{api_cmd_stop, What}).



-spec pause (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

pause (What) ->
	gen_server:call(?SERVER,{api_cmd_pause, What}).



-spec resume (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

resume (What) ->
	gen_server:call(?SERVER,{api_cmd_resume, What}).



-spec cancel (What) -> ok | {error, Reason} when 
		What :: all | [UUID::string()],
		Reason :: term().

cancel (What) ->
	gen_server:call(?SERVER,{api_cmd_cancel, What}).



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

-spec init (Args) -> {ok, State} | {stop, Reason} when
		Args :: term(),
		State :: #hdl_state{},
		Reason :: term().

init (_) ->
	process_flag(trap_exit, true),
	case prepare_statistics() of
		{error, Reason} ->
			{stop, Reason};
		ok ->
			{ok, _} = timer:apply_after(?STATS_INTERVAL,gen_server,call,[self(),update_stats]),
			{ok, #hdl_state{timer=owls_timers:new(millisecond)}}
	end.




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
	{reply, ok, apply_config(Cfg, State)};
	
handle_call ({api_cmd_start, Which},_,State) ->
	{reply, ok, cmd_startup_sim(State,Which)};

handle_call ({api_cmd_stop, Which},_,State) ->
	NewState = trigger_execute (0, queue_command (clients_stop, Which,State)),
	{reply, ok, NewState};

handle_call ({api_cmd_pause, Which},_,State) ->
	NewState = trigger_execute (0, queue_command (clients_pause, Which,State)),
	{reply, ok, NewState};

handle_call ({api_cmd_resume, Which},_,State) ->
	NewState = trigger_execute (0, queue_command (clients_resume, Which,State)),
	{reply, ok, NewState};

handle_call ({api_cmd_cancel, Which},_,State) ->
	NewState = trigger_execute (0, queue_command (clients_cancel, Which,State)),
	{reply, ok, NewState};

handle_call (update_stats, _From, State) ->
	NewState = update_client_statistics(State),
	{reply, ok, NewState};

handle_call (_, _, State) ->
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} when
		Msg :: term(),
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

handle_info({'EXIT',From,Reason}, #hdl_state{clients=C, client_trans=T, client_pid_map=M}=State) when is_map_key(From,M) ->
	#{From := Id} = M,
	#{Id := L} = T,
	Cpm = maps:remove(From,M),
	{Cl, R} = case Reason of
			normal	-> {C#{Id := {available, none}}, cancelled};
				_ 	-> {C#{Id := {dead, none}}, crashed}
	end,
	{noreply, State#hdl_state{client_pid_map=Cpm, clients=Cl, client_trans=T#{Id := [{R,os:system_time()}|L]}}};

	
handle_info(_, State) ->
	{noreply, State}.




-spec terminate (Reason, State) -> ok when
		Reason :: shutdown | {shutdown, term()} | normal,
		State :: #hdl_state{}.

terminate (_Reason, _State) ->
	ok = dets:close(?MODULE),
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

%--------trigger_execute/2---------------trigger execute after a delay of D milliseconds (0 = immedeately)

-spec trigger_execute (Delay, State) -> State when
		Delay :: non_neg_integer(),
		State :: #hdl_state{}.

trigger_execute (0, State) ->
	gen_server:cast(self(),execute),
	State;

trigger_execute (D, State) ->
	{ok,_} = timer:apply_after(D,gen_server,cast,[self(),execute]),
	State.


%--------queue_command/3-----------------que command into state 

-spec queue_command (Where,Command, Refs, State) -> NewState when
		Where :: front | back,
		Command :: clients_start | clients_stop | clients_pause | clients_resume | clients_cancel,
		Refs :: all | [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

queue_command (Where,Cmd, all, #hdl_state{clients=Clients}=State) ->
	queue_command (Where,Cmd,[K || {K,_} <- maps:to_list(Clients)],State);

queue_command (Where,Cmd,Refs,#hdl_state{cmd_queue=Q}=State) ->
	NewQueue = case Where of
		back -> 
			[#command{cmd=Cmd, refs=Refs}|Q];
		front -> 
			Q ++ [#command{cmd=Cmd, refs=Refs}]
	end,
	State#hdl_state{cmd_queue=NewQueue}.
	

-spec queue_command (Command, Refs, State) -> NewState when
		Command :: clients_start | clients_stop | clients_pause | clients_resume | clients_cancel,
		Refs :: all | [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

queue_command (Command, Refs, State) ->
	queue_command (back,Command, Refs, State).





%--------apply_config/2------------------translates configuration into state

-spec apply_config (Cfg, State) -> NewState when
		State :: #hdl_state{},
		Cfg :: #{},
		NewState :: #hdl_state{}.

apply_config (Cfg, State) when is_map_key(file,Cfg) ->
	#{file := CfgFile} = Cfg,
	Path = case filelib:is_regular(CfgFile) of
		true -> CfgFile;
		_ -> filename:join([code:priv_dir(?OWLS_APP),CfgFile])
	end,
	case file:consult(Path) of
		{ok, [ClientIds|_]} ->
			Clients = maps:from_list([{X,{available,none}}||X<-ClientIds]),
			Transitions = maps:from_list([{X,{available,os:system_time()}}||X<-ClientIds]),
			State#hdl_state{clients=Clients, client_trans=Transitions};

		{error, Err} ->
			?L_E(?DBGSTR("invalid config file at '~s' with error: '~p'",[Path,Err])),
			State
	end;

apply_config (Cfg, State) when is_map_key(internal,Cfg) ->
	Clients = #{"a68d41fa-dd12-4fb7-bc44-e834667280b4" => {available,none}},
	Transitions = #{"a68d41fa-dd12-4fb7-bc44-e834667280b4" => [{available,os:system_time()}]},
	State#hdl_state{clients=Clients, client_trans=Transitions};

apply_config (_Cfg, State) ->
	State.




%--------update_client_status/3-----------update the state of a client in the clients map

-spec update_client_status (ClientState, ClientId, HandlerState) -> NewHandlerSate when
		ClientState :: available | dead | ovsdb_ap:ap_status(),
		ClientId :: string(),
		HandlerState :: #hdl_state{},
		NewHandlerSate :: #hdl_state{}.

update_client_status (ClS, Id, #hdl_state{clients=C, client_trans=T}=State) when is_map_key(Id,C), is_map_key(Id,T) ->
	#{Id := {_,P}} = C,
	#{Id := L} = T,
	State#hdl_state{clients=C#{Id := {ClS,P}}, client_trans=T#{Id := [{ClS,os:system_time()}|L]}};
	
update_client_status (_, _, State) ->
	State.



%--------get_clients_in_state/3----------filter all clients with state

-spec get_clients_in_state (State, Clients, Candidates) -> Available when
		State :: available | dead | ovsdb_ap:ap_status(),
		Clients :: client_states(),
		Candidates :: all | [UUID::string()],
		Available :: [UUID::string()].

get_clients_in_state (State, Clients, Which) ->
	get_clients_with_sate_pred (fun(X) -> X=:=State end, Clients, Which).

get_clients_with_sate_pred (Pred, Clients, all) ->
	[K || {K,{V,_}} <- maps:to_list(Clients), Pred(V)];

get_clients_with_sate_pred (Pred, Clients, Candidates) ->
	[K || {K,{V,_}} <- maps:to_list(Clients), Pred(V) and lists:member(K,Candidates)].

%--------cmd_startup_sim/2--------------lauches the start-up sequence of simulation clients

-spec cmd_startup_sim (State, Which) -> NewState when
		State :: #hdl_state{},
		Which :: all | [UUID::string()],
		NewState :: #hdl_state{}.

cmd_startup_sim (#hdl_state{timer=T, clients=Clients}=State, Which) ->
	T2 = owls_timers:mark("startup",T),
	ToLaunch = get_clients_with_sate_pred (fun(X) -> (X=:=available) orelse (X=:=dead) end, Clients, Which),
	ToStart = get_clients_in_state (ready,Clients, Which),
	if
		length(ToLaunch) + length(ToStart) > 0 ->
			NewState = cmd_launch_clients(ToLaunch,State#hdl_state{timer=T2}),
			trigger_execute (0, queue_command(front,clients_start,ToStart ++ ToLaunch,NewState));
		true ->
			?L_E(?DBGSTR("start command issued with no clients available or ready to start")),
			State
	end.




%--------cmd_launch_clients/2--------------------lauch processes for clients (synchrounsly)

-spec cmd_launch_clients (ToLauch,State) -> NewState when
		ToLauch :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

cmd_launch_clients (ToLauch, State) ->
	%% @TODO: pass in the real manager
	L = [ {V, ovsdb_ap:launch(V,{global,manager})} || V <- ToLauch],
	Pm = maps:merge(State#hdl_state.client_pid_map, maps:from_list([{P,Id} || {Id, {ok, P}} <- L])),
	C = maps:merge(State#hdl_state.clients, maps:from_list([{V,{init,P}} || {V, {ok, P}} <- L])),
	T = owls_timers:mark("launched",State#hdl_state.timer),
	State#hdl_state{clients=C, client_pid_map=Pm, timer=T}.




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
		#command{cmd=clients_start, refs=R} ->
			clients_start(R, AltState);
		
		#command{cmd=clients_pause, refs=R} ->
			clients_pause(R, AltState);
		
		#command{cmd=clients_resume, refs=R} ->
			clients_resume(R, AltState);

		#command{cmd=clients_stop, refs=R} ->
			clients_stop(R, AltState);

		#command{cmd=clients_cancel, refs=R} ->
			clients_cancel(R, AltState)
	end.




%--------clients_start/2-------------starts the simulation of the cliens (in ready state)

-spec clients_start (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_start (Refs, #hdl_state{timer=T}=State) ->	 
	Elapsed = owls_timers:delta("launched",T),
	if
		Elapsed > ?MAX_STARTUP_TIME ->
			?L_E(?DBGSTR("Getting ~B clients ready took too long (~s)",[length(Refs),owls_timers:fmt_duration(Elapsed,T)])),
			?L_I(?DBGSTR("Cancelling simulation start request")),
			trigger_execute (0, queue_command (front,clients_cancel,Refs,State));
		true ->
			Ready = get_clients_in_state (ready,State#hdl_state.clients,Refs),
			check_client_startup(Ready,Refs,State)
	end.

	
-spec check_client_startup (Ready, ToStart, State) -> NewState when
		Ready :: [UUID::string()],
		ToStart :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

check_client_startup (Ready, ToStart, #hdl_state{clients=C,timer=T}=State) ->
	if
		length(Ready) == length(ToStart) ->
			lists:foreach(fun(I) -> #{I:={ready,P}}=C, ovsdb_ap:start_ap(P) end, Ready),
			State#hdl_state{timer=owls_timers:mark("sim_started",T)};
		true ->
			trigger_execute (500, queue_command (front,clients_start,ToStart,State))
	end.




%--------clients_stop/2-------------stops the simulation in specified clients

-spec clients_stop (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_stop (Refs, #hdl_state{clients=C, timer=T}=State) ->
	T2 = owls_timers:mark("stop_called",T),
	ToStop = get_clients_with_sate_pred (fun(X) -> (X=:=running) or (X=:=paused) end, C, Refs),
	lists:foreach(fun(I) -> #{I:={_,P}}=C, ovsdb_ap:stop_ap(P) end, ToStop),
	State#hdl_state{timer=owls_timers:mark("stop_executed",T2)}.



%--------clients_pause/2-------------pauses clients that are in running state

-spec clients_pause (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_pause (Refs, #hdl_state{clients=C, timer=T}=State) ->
	T2 = owls_timers:mark("pause_called",T),
	ToPause = get_clients_in_state (running,C, Refs),
	lists:foreach(fun(I) -> #{I:={_,P}}=C, ovsdb_ap:pause_ap(P) end, ToPause),
	State#hdl_state{timer=owls_timers:mark("pause_executed",T2)}.



%--------clients_resume/2-------------resume paused clients

-spec clients_resume (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_resume (Refs, #hdl_state{clients=C, timer=T}=State) ->
	T2 = owls_timers:mark("resume_called",T),
	ToResume = get_clients_in_state (paused,C, Refs),
	lists:foreach(fun(I) -> #{I:={_,P}}=C, ovsdb_ap:start_ap(P) end, ToResume),
	State#hdl_state{timer=owls_timers:mark("resume_executed",T2)}.



%--------clients_cancel/2-------------cancel clients regardless of state

-spec clients_cancel (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_cancel (Refs, #hdl_state{clients=C, timer=T}=State) ->
	T2 = owls_timers:mark("cancel_called",T),
	ToCancel = get_clients_with_sate_pred (fun(X) -> (X=/=available) and (X=/=dead) end, C, Refs),
	lists:foreach(fun(I) -> #{I:={_,P}}=C, ovsdb_ap:cancel_ap(P) end, ToCancel),
	State#hdl_state{timer=owls_timers:mark("cancel_executed",T2)}.





%%-----------------------------------------------------------------------------
%% handling statistics

-spec prepare_statistics () -> ok | {error, Reason::term()}.

prepare_statistics () ->
	File = filename:join([code:priv_dir(?OWLS_APP),"ovsdb","handler_stats.dets"]),
	Exists = filelib:is_file(File),
	case dets:open_file(?MODULE,[{file,File},{type,set},{keypos,2}]) of
		{ok, ?MODULE} ->
			case Exists of
				true -> ok;
				false -> ok = dets:insert(?MODULE,{seq,seq,0})
			end;
		{error, Reason} ->
			?L_E(?DBGSTR("Can't open statistics table: '~p'",[File])),
			{error, Reason}
	end.


-spec update_client_statistics (State) -> NewState  when
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

update_client_statistics (#hdl_state{clients=C}=State) ->
	[{_,_,Seq}] = dets:lookup(?MODULE,seq),
	Stats = #statistics{
			seq = Seq + 1,
			stamp = os:system_time(),
			configured = maps:size(C),
			running = length(get_clients_in_state(running,C,all)),
			paused = length(get_clients_in_state(paused,C,all)),
			dropped = 0,
			avg_latency = 0,
			avg_speed = 0
		},
	ok = dets:insert(?MODULE,[{seq,seq,Seq+1},Stats]),
	{ok, _} = timer:apply_after(?STATS_INTERVAL,gen_server,call,[self(),update_stats]),
	State.