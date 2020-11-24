%%%----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2020 12:44 p.m.
%%%----------------------------------------------------------------------------
-module(ovsdb_ap).
-author("helge").

-behaviour(gen_server).

-include("../include/common.hrl").

-define(SERVER, ?MODULE).



%%-----------------------------------------------------------------------------
%% @doc The SPEC for starting a AP node is a proplist with the following fileds.
%%
%% {manager, ::pid()} - the pid of the simmanager used to get configuration.
%% {ap_serial, ::string()} - the serial number of the AP.
%% {ap_type, ::binary()} - type of AP to simulate e.g. EA8300.
%% {uuid, ::term()} - arbitrary unique id to identify this simnode AP.
%% @end
%%-----------------------------------------------------------------------------


%% API
-export([launch/2]).
-export([uuid/1,start_ap/1,stop_ap/1,pause_ap/1,cancel_ap/1]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-type ap_status() :: init | ready | running | paused | stopped.
-export_type([ap_status/0]).

-record(ap_state, { 	
	sim_manager :: pid(), 	% manager to be contacted to get configuration
	serial = "" :: string(),		% serial number of the access point
	type = <<"">> :: binary(),		% device type e.g. EA8300
	uuid :: string(),			% unique ID for this AP node
	timer :: owls_timers:tms(),
	status = init :: ap_status(),	% internal status
	config = #{} :: map(),
	statistics = #{} :: map()
}).




%%%============================================================================
%%% API
%%%============================================================================


-spec launch (Id, SimMan) -> {ok, Pid} | {error, Reason} when
		Id :: UUID::string(),
		SimMan :: pid(),
		Pid :: pid(),
		Reason :: term().

launch (Id, SimMan) ->
	gen_server:start_link(?MODULE, {Id, SimMan}, []).




-spec uuid (Node) -> Uuid when
		Node :: pid(),
		Uuid :: term().

uuid (Node) ->
	gen_server:call(Node,ap_uuid).


%%%============================================================================
%%% HANDLER API Implementation
%%%============================================================================

-spec start_ap (Node :: pid()) -> ok.

start_ap (Node) -> 
	gen_server:cast(Node,ap_start).


-spec stop_ap (Node :: pid()) -> ok.

stop_ap (Node) -> 
	gen_server:cast(Node,ap_stop).


-spec pause_ap (Node :: pid()) -> ok.

pause_ap (Node) -> 
	gen_server:cast(Node,ap_pause).


-spec cancel_ap (Node :: pid()) -> ok.

cancel_ap (Node) -> 
	gen_server:cast(Node,ap_cancel).




%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init ({Id, SimMan}) -> {ok, State}  when
		Id :: UUID::string(),
		SimMan :: pid(),
		State :: #ap_state{}.

init ({Id,SimMan}) ->
	process_flag(trap_exit, true),
	InitialState = prepare_state(Id,SimMan),
	gen_server:cast(self(),start_up),
	{ok, InitialState}.




-spec handle_cast (Request, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Request :: term(),
		State :: #ap_state{},
		NewState :: #ap_state{},
		Reason :: string().

handle_cast (start_up, #ap_state{status=init}=State) ->
	{noreply, startup_ap(State)};

handle_cast (start_up, State) ->
	?L_E(?DBGSTR("can not start up client if not in init state")),	
	{noreply, State};

handle_cast (ap_start, #ap_state{status=ready}=State) ->
	{noreply, run_simulation(State)};

handle_cast (ap_start, #ap_state{status=paused}=State) ->
	{noreply, run_simulation(State)};

handle_cast (ap_start, State) ->
	?L_E(?DBGSTR("can not run simulation if not in ready or paused state")),
	{noreply, State};

handle_cast (ap_stop, #ap_state{status=paused}=State) ->
	{noreply, stop_simulation(State)};

handle_cast (ap_stop, #ap_state{status=running}=State) ->
	{noreply, stop_simulation(State)};

handle_cast (ap_stop, State) ->
	?L_E(?DBGSTR("can not stop simulation that is not running or paused")),
	{noreply, State};

handle_cast (ap_pause, #ap_state{status=running}=State) ->
	{noreply, pause_simulation(State)};

handle_cast (ap_pause, State) ->
	?L_E(?DBGSTR("can not pause simulation that is not running")),
	{noreply, State};

handle_cast (ap_cancel, State) ->	
	_ = cancel_simulation(State),
	{stop, normal, State};

handle_cast (R,State) ->
	?L_E(?DBGSTR("got unknown request: ~p",[R])),
	{noreply, State}.




-spec handle_call (Request, From, State) -> {reply, Reply, NewState} | {stop, Reason, Reply, NewState} when
		Request :: term(),
		From :: {pid(),Tag::term()},
		State :: #ap_state{},
		Reply :: term(),
		Reason :: term(),
		NewState :: #ap_state{}.

handle_call (ap_uuid,_,State) ->
	{reply, State#ap_state.uuid, State};

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Msg :: term(),
		State :: #ap_state{},
		Reason :: term(),
		NewState :: #ap_state{}.


handle_info({'EXIT', _Pid, _Reason}, State) ->
	%% @TODO: implement proper restart strategy for linked processes
	{noreply, State};


handle_info (Msg,State) ->
	?L_E(?DBGSTR("got unexpected info message ~p",[Msg])),
	{noreply, State}.




-spec terminate (Reason, State) -> ok when
		Reason :: shutdown | normal,
		State :: #ap_state{}.

terminate (_Reason, _State) ->
	ok.




-spec code_change (OldVersion, OldState, Extra) -> {ok, NewState} when
		OldVersion :: term(),
		OldState ::#ap_state{},
		Extra :: term(),
		NewState :: #ap_state{}.

code_change (_,OldState,_) ->
	?L_E(?DBGSTR("code change requested")),
	{ok, OldState}.




%%%============================================================================
%%% internal functions
%%%============================================================================


%---------prepare_state/2----------------convert Spec proplist into internal state 

-spec prepare_state (ID, SimMan) -> State when
		ID :: UUID::string(),
		SimMan :: pid(),
		State :: #ap_state{}.

prepare_state (ID, SimMan) ->
	#ap_state{
		sim_manager = SimMan,
		uuid = ID,
		timer = owls_timers:new(millisecond)
	}.




%--------set_status/1--------------------sets internal status + broadcast status to handler

-spec set_status (Status, State) -> NewState when
		Status :: ap_status(),
		State :: #ap_state{},
		NewState :: #ap_state{}.

set_status (Status, #ap_state{uuid=Id}=State) ->
	ovsdb_client_handler:ap_status(Status,Id),
	State#ap_state{status=Status}.




%--------startup_ap/1--------------------initiate startup sequence

-spec startup_ap (State :: #ap_state{}) -> NewState :: #ap_state{}.

startup_ap (#ap_state{status=init}=State) ->
	%% @TODO: implement self configuration phase
	set_status(ready,State).




%--------run_simulation/1----------------start or resume simulation

-spec run_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

run_simulation (State) ->
	%% @TODO: implement simulation start
	set_status(running, State).




%--------stop_simulation/1----------------stops a simulation (clears internal state to ready)

-spec stop_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

stop_simulation (State) ->
	set_status(ready, State).




%--------pause_simulation/1----------------halts simulation (tear down of connections) but keeps internal state

-spec pause_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

pause_simulation (State) ->
	set_status(paused, State).




%--------cancel_simulation/1----------------shutdown and exit simulation (AP exits)

-spec cancel_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

cancel_simulation (State) ->
	State.