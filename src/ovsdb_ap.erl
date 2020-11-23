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
-export([uuid/1,start_ap/1]).


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



-spec start_ap (Node) -> ok when
		Node :: pid().

start_ap (Node) ->
	gen_server:cast(Node,ap_start).




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

handle_cast (start_up, State) ->
	{noreply, startup_ap(State)};

handle_cast (ap_start, State) ->
	{noreply, run_simulation(State)};


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




-spec handle_info (Msg, State) -> {noreply, NewState} when
		Msg :: term(),
		State :: #ap_state{},
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
	set_status(ready,State);

startup_ap (State) ->
	?L_E(?DBGSTR("can nott start_up client if not in init state")),
	State.




%--------run_simulation/1----------------start or resume simulation

-spec run_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

run_simulation (State) ->
	set_status(running,State).
