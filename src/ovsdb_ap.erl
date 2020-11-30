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



%% API
-export([launch/2]).
-export([start_ap/1,stop_ap/1,pause_ap/1,cancel_ap/1]).

%% comm API
-export([rpc_cmd/2,reset_comm/1]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-type ap_status() :: init | ready | running | paused | stopped.
-export_type([ap_status/0]).

-record(ap_state, { 	
	sim_manager :: tuple(), 	% manager to be contacted to get configuration	
	timer :: owls_timers:tms(),
	status = init :: ap_status(),	% internal status
	config :: ovsdb_ap_config:cfg(),
	comm = none :: none | pid(),
	store :: ets:tid(),
	req_queue :: ets:tid(),
	stats_ets :: ets:tid()
}).




%%%============================================================================
%%% API
%%%============================================================================


-spec launch (Id, SimMan) -> {ok, Pid} | {error, Reason} when
		Id :: UUID::string(),
		SimMan :: tuple(),
		Pid :: pid(),
		Reason :: term().

launch (Id, SimMan) ->
	gen_server:start_link(?MODULE, {Id, SimMan}, []).




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
%%% Comm module API
%%%============================================================================


-spec rpc_cmd (Node :: pid(), Rpc :: term()) -> ok | invalid.

rpc_cmd (Node,Rpc) ->
	gen_server:call(Node,{exec_rpc,Rpc}).


-spec reset_comm (Node :: pid()) -> ok.

reset_comm (Node) ->
	gen_server:call(Node,reset_comm).



%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init ({Id, SimMan}) -> {ok, State}  when
		Id :: UUID::string(),
		SimMan :: tuple(),
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

handle_call ({exec_rpc, RPC}, _From, State) when is_map(RPC) andalso
												 is_map_key(<<"method">>,RPC) andalso
												 is_map_key(<<"id">>,RPC) ->
	case ovasdb_ap_rpc:eval_req(maps:get(<<"method">>,RPC),
								maps:get(<<"id">>,RPC),
								RPC,
								State#ap_state.store) of
		ok ->
			{reply,ok,State};

		{ok, Result} when is_map(Result) andalso is_map_key(<<"result">>,RPC) ->
			ovasdb_ap_comm:send_term(State#ap_state.comm,Result),
			{reply,ok,State};

		{error, Reason} ->
			?L_E(?DBGSTR("RPC call '~s' failed with reason: ~p",[maps:get(<<"method">>,RPC),Reason])),
			{reply,invalid,State}
	end;
	
handle_call ({exec_rpc, RPC}, _From, State) when is_map(RPC) andalso
												 is_map_key(<<"result">>,RPC) andalso
												 is_map_key(<<"id">>,RPC) ->
	case ovasdb_ap_rpc:eval_resp(maps:get(<<"id">>,RPC),
								 RPC,
								 State#ap_state.req_queue,
								 State#ap_state.store) of
		ok ->
			{reply,ok,State};

		{error, Reason} ->
			?L_E(?DBGSTR("RPC response to request '~B' failed with reason: ~p",[maps:get(<<"id">>,RPC),Reason])),
			{reply,invalid,State}
	end;
	
handle_call ({exec_rpc, RPC}, _From, State) ->
	io:format("invalid RPC: ~p~n",[RPC]),
	{reply,invalid, State};

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Msg :: term(),
		State :: #ap_state{},
		Reason :: term(),
		NewState :: #ap_state{}.


handle_info({'EXIT', Pid, Reason}, State) ->
	?L_E(?DBGSTR("Abnormal exit from ~p with reason: ~p",[Pid,Reason])),
	%% @TODO: implement proper restart strategy for linked processes
	{noreply, State};


handle_info (Msg,State) ->
	?L_E(?DBGSTR("got unexpected info message ~p",[Msg])),
	{noreply, State}.




-spec terminate (Reason, State) -> ok when
		Reason :: shutdown | normal,
		State :: #ap_state{}.

terminate (_Reason, #ap_state{stats_ets=Tab}) ->
	ets:delete(Tab),
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
		SimMan :: tuple(),
		State :: #ap_state{}.

prepare_state (ID, SimMan) ->
	Store = ets:new(ovsdb_ap,[bag,private,{keypos, 1}]),
	#ap_state{
		sim_manager = SimMan,
		config = ovsdb_ap_config:new(ID,Store),
		timer = owls_timers:new(millisecond),
		status = init,
		store = Store,
		req_queue = ets:new(ovsdb_ap_req,[ordered_set,private,{keypos, 1}]),
		stats_ets = ets:new(ovsdb_ap_stats,[set,private,{keypos, 2}])
	}.




%--------set_status/1--------------------sets internal status + broadcast status to handler

-spec set_status (Status, State) -> NewState when
		Status :: ap_status(),
		State :: #ap_state{},
		NewState :: #ap_state{}.

set_status (Status, #ap_state{config=Cfg}=State) ->
	ovsdb_client_handler:ap_status(Status,ovsdb_ap_config:id(Cfg)),
	State#ap_state{status=Status}.




%--------startup_ap/1--------------------initiate startup sequence

-spec startup_ap (State :: #ap_state{}) -> NewState :: #ap_state{}.

startup_ap (#ap_state{status=init, config=Cfg, sim_manager=Man}=State) ->
	NewCfg =  ovsdb_ap_config:configure(Man,Cfg),
	Opts = [
		{host, ovsdb_ap_config:tip(host,NewCfg)},
		{port, ovsdb_ap_config:tip(port,NewCfg)},
		{ca, ovsdb_ap_config:ca_certs(NewCfg)},
		{cert, ovsdb_ap_config:client_cert(NewCfg)}
	],
	{ok, Comm} = ovsdb_ap_comm:start_link(Opts),
	set_status(ready,State#ap_state{config=NewCfg, comm=Comm}).




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