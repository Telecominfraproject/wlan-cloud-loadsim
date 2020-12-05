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
-define(AP_STATS_INTERVAL,2000).
-define(AP_REPORT_INTERVAL,10000).
-define(MGR_REPORT_INTERVAL,20000).

%% API
-export([start_link/0]).
-export([set_configuration/1, start/1, stop/1, pause/1, resume/1, cancel/1, report/0]).
-export([ap_status/2,dump_clients/0]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-type client_status() :: available | dead | ovsdb_ap:ap_status().

-record (command,{
	cmd :: clients_start | clients_stop | clients_pause | clients_resume | clients_cancel,
	refs :: [UUID::string()]
}).

-type command() :: #command{}.


-record (ap_client, {						%% don't modif -- if thern check all ets:match operations
	id :: UUID::string(),
	ca_name :: string() | binary(),
	status :: client_status(),
	process :: none | pid(),
	transitions :: [{client_status(), TimeStamp::integer()}]
}).

-record (ap_proc_map, {
	process :: pid(),
	id :: UUID::string()
}).

-record (hdl_state, {
	clients = ets:tid(),
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


dump_clients () ->
	gen_server:call(?SERVER,dump_clients).

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
			{ok, _} = timer:apply_after(?MGR_REPORT_INTERVAL,gen_server,call,[self(),update_stats]),
			Tid = ets:new(ovsdb_clients,[ordered_set,private,{keypos, 2}]),
			{ok, #hdl_state{timer=owls_timers:new(millisecond), clients=Tid}}
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

handle_call (dump_clients,_From, #hdl_state{clients=Clients}=State) ->
	C = ets:match_object(Clients,'$1'),
	{reply,C,State};

handle_call (_, _, State) ->
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, State} when
		Msg :: term(),
		State :: #hdl_state{}.

handle_info({'EXIT',From,Reason}, #hdl_state{clients=CTid}=State) ->
	case get_client_with_pid(CTid,From) of
		{ok, C} ->
			T = C#ap_client.transitions,
			UpdC = case Reason of
				normal -> 
					C#ap_client{process=none, transitions=[{cancelled,erlang:system_time()}|T]};
				_ ->
					C#ap_client{process=none, transitions=[{crashed,erlang:system_time()}|T]}
			end,
			ets:insert(CTid,UpdC),
			ets:match_delete(CTid,{ap_proc_map,From,'_'}),
			{noreply, State};
		_ ->
			{noreply, State}
	end;
	
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
	queue_command (Where,Cmd,[ID || #ap_client{id=ID} <- ets:match_object(Clients,'$1')],State);

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

apply_config (Cfg, #hdl_state{clients=Clients}=State) when is_map_key(file,Cfg) ->
	#{file := CfgFile} = Cfg,
	Path = case filelib:is_regular(CfgFile) of
		true -> CfgFile;
		_ -> filename:join([code:priv_dir(?OWLS_APP),CfgFile])
	end,
	_ = case file:consult(Path) of
		{ok, [Refs]} ->
			C = [#ap_client{id=ID,ca_name=CA,status=available,process=none,transitions=[{available,erlang:system_time()}]} || {CA,ID}<-Refs],
			ets:insert(Clients,C);
			
		{error, Err} ->
			?L_E(?DBGSTR("invalid config file at '~s' with error: '~p'",[Path,Err]))	
	end,
	State;

apply_config (Cfg, #hdl_state{clients=Clients}=State) when is_map_key(internal,Cfg) ->
	C = #ap_client{
		id="a68d41fa-dd12-4fb7-bc44-e834667280b4",
		ca_name="i_am_the_boss",
		status = available,
		process = none,
		transitions = [{available,erlang:system_time()}]
	},
	ets:insert(Clients,C),
	State;

apply_config (Cfg, State) ->
	io:format("GOT CONFIG!!! ~p~n~n",[Cfg]),
	State.




%--------update_client_status/3-----------update the state of a client in the clients map

-spec update_client_status (ClientState, ClientId, HandlerState) -> NewHandlerSate when
		ClientState :: available | dead | ovsdb_ap:ap_status(),
		ClientId :: string(),
		HandlerState :: #hdl_state{},
		NewHandlerSate :: #hdl_state{}.

update_client_status (ClS, Id, #hdl_state{clients=Clients}=State) ->
	{ok, C} = get_client_with_id(Clients,Id),
	ets:insert(Clients,C#ap_client{status=ClS}),
	State.
	



%--------get_clients_in_state/3----------filter all clients with state

-spec get_client_ids_in_state (Clients :: ets:tid(), State :: client_status() | tuple(), Refs :: all | [UUID::string()]) ->  [UUID::string()].
							
get_client_ids_in_state (Tid, State, Refs) when is_atom(State) ->
	get_client_ids_in_state (Tid,{State},Refs);

get_client_ids_in_state (Tid, States, Refs) ->
	MSpec = [{{ap_client,'_','_',X,'_','_'},[],['$_']}||X<-tuple_to_list(States)],
	Clients = ets:select(Tid,MSpec),
	Cids = [ID||#ap_client{id=ID}<-Clients],
	%io:format("MSPEC: ~p~nCLIENTS: ~p~nREFS: ~p~n, CIDS: ~p~n",[MSpec,Clients,Refs,Cids]),
	case Refs of
		all ->
			Cids;
		Cand when is_list(Cand) ->
			[ID || ID <- Cids, lists:member(ID,Cand)]
	end.


-spec get_clients_with_ids (Clients::ets:tid(),Ids::[UUID::string()]) -> [#ap_client{}].

get_clients_with_ids (CTid, Ids) ->
	get_clients_with_ids (CTid, Ids, []).

get_clients_with_ids (_,[],Acc) ->
	Acc;

get_clients_with_ids (CTid,[ID|Rest],Acc) ->
	case ets:match_object(CTid,{ap_client,ID,'_','_','_','_'}) of
		[R] ->
			get_clients_with_ids (CTid,Rest,[R|Acc]);
		_ ->
			io:format("ID> ~s~n",[ID]),
			[]
	end.






 -spec get_client_with_pid (Clients :: ets:tid(), Pid :: pid()) -> {ok, #ap_client{}} | {error, not_found}.

get_client_with_pid (Tid, Pid) ->
	case ets:match_object(Tid,{ap_proc_map,Pid,'_'}) of
		[] ->
			{error, not_found};
		[{_,_,Id}] ->
			get_client_with_id(Tid,Id)
	end.


-spec get_client_with_id (Clients :: ets:tid(), Id :: string()) -> {ok, #ap_client{}} | {error, not_found}.

get_client_with_id (Tid, Id) ->
	case ets:match_object(Tid,{ap_client,Id,'_','_','_','_'}) of
		[] ->
			{error, not_found};
		[R|_] ->
			{ok, R}
	end.






%--------cmd_startup_sim/2--------------lauches the start-up sequence of simulation clients

-spec cmd_startup_sim (State, Which) -> NewState when
		State :: #hdl_state{},
		Which :: all | [UUID::string()],
		NewState :: #hdl_state{}.

cmd_startup_sim (#hdl_state{timer=T, clients=Clients}=State, Which) ->
	T2 = owls_timers:mark("startup",T),
	ToLaunch = get_client_ids_in_state (Clients, {available, dead}, Which),
	ToStart = get_client_ids_in_state (Clients, ready, Which),
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

cmd_launch_clients (ToLauch, #hdl_state{clients=Clients}=State) ->
	Opt = [{report_int,?AP_REPORT_INTERVAL},{stats_int,?AP_STATS_INTERVAL}],
	F = fun (#ap_client{id=ID,ca_name=CAName}=C) -> 
			{ok, Pid} = ovsdb_ap:launch(CAName,ID,Opt), 
			[#ap_proc_map{id=ID, process=Pid},C#ap_client{process=Pid}]
		end,
	L = [F(C) || C <- get_clients_with_ids(Clients,ToLauch)],
	ets:insert(Clients,lists:flatten(L)),
	T = owls_timers:mark("launched",State#hdl_state.timer),
	State#hdl_state{timer=T}.




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

clients_start (Refs, #hdl_state{clients=Clients, timer=T}=State) ->	 
	Elapsed = owls_timers:delta("launched",T),
	if
		Elapsed > ?MAX_STARTUP_TIME ->
			?L_E(?DBGSTR("Getting ~B clients ready took too long (~s)",[length(Refs),owls_timers:fmt_duration(Elapsed,T)])),
			?L_I(?DBGSTR("Cancelling simulation start request")),
			trigger_execute (0, queue_command (front,clients_cancel,Refs,State));
		true ->
			Ready = get_client_ids_in_state (Clients,ready,Refs),
			check_client_startup(Ready,Refs,State)
	end.

	
-spec check_client_startup (Ready, ToStart, State) -> NewState when
		Ready :: [UUID::string()],
		ToStart :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

check_client_startup (Ready, ToStart, #hdl_state{clients=Clients,timer=T}=State) ->
	if
		length(Ready) == length(ToStart) ->			
			[ovsdb_ap:start_ap(P) || #ap_client{process=P} <- get_clients_with_ids(Clients,Ready)],
			State#hdl_state{timer=owls_timers:mark("sim_started",T)};
		true ->
			trigger_execute (500, queue_command (front,clients_start,ToStart,State))
	end.




%--------clients_stop/2-------------stops the simulation in specified clients

-spec clients_stop (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_stop (Refs, #hdl_state{clients=Clients, timer=T}=State) ->
	T2 = owls_timers:mark("stop_called",T),
	ToStop = get_clients_with_ids(Clients,get_client_ids_in_state(Clients,{running,paused},Refs)),
	[ovsdb_ap:stop_ap(P) || #ap_client{process=P} <- ToStop],
	State#hdl_state{timer=owls_timers:mark("stop_executed",T2)}.



%--------clients_pause/2-------------pauses clients that are in running state

-spec clients_pause (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_pause (Refs, #hdl_state{clients=Clients, timer=T}=State) ->
	T2 = owls_timers:mark("pause_called",T),
	ToPause = get_clients_with_ids(Clients,get_client_ids_in_state(Clients,running,Refs)),
	[ovsdb_ap:pause_ap(P) || #ap_client{process=P} <- ToPause],
	State#hdl_state{timer=owls_timers:mark("pause_executed",T2)}.



%--------clients_resume/2-------------resume paused clients

-spec clients_resume (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_resume (Refs, #hdl_state{clients=Clients, timer=T}=State) ->
	T2 = owls_timers:mark("resume_called",T),
	ToResume = get_clients_with_ids(Clients,get_client_ids_in_state(Clients,paused,Refs)),
	[ovsdb_ap:start_ap(P) || #ap_client{process=P} <- ToResume],
	State#hdl_state{timer=owls_timers:mark("resume_executed",T2)}.



%--------clients_cancel/2-------------cancel clients regardless of state

-spec clients_cancel (Refs, State) -> NewState when
		Refs :: [UUID::string()],
		State :: #hdl_state{},
		NewState :: #hdl_state{}.

clients_cancel (Refs, #hdl_state{clients=Clients, timer=T}=State) ->
	T2 = owls_timers:mark("cancel_called",T),
	ToCancel = get_clients_with_ids(Clients,get_client_ids_in_state(Clients,{running,paused,stopped},Refs)),
		
	[ovsdb_ap:cancel_ap(P) || #ap_client{process=P} <- ToCancel],
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
			configured = length(ets:match_object(C,{ap_client,'_','_','_','_','_'})),
			running = length(get_client_ids_in_state(C,running,all)),
			paused = length(get_client_ids_in_state(C,paused,all)),
			dropped = 0,
			avg_latency = 0,
			avg_speed = 0
		},
	ok = dets:insert(?MODULE,[{seq,seq,Seq+1},Stats]),
	{ok, _} = timer:apply_after(?MGR_REPORT_INTERVAL,gen_server,call,[self(),update_stats]),
	State.


