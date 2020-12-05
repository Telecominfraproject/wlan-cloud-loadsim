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
-export([rpc_cmd/2,reset_comm/1,mqtt_conf/2]).

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
	mqtt = idle :: idle | running,
	store :: ets:tid(),
	req_queue :: ets:tid(),
	stats_ets :: ets:tid()
}).

%%%============================================================================
%%% API
%%%============================================================================
-spec launch (Id::string(), SimMan::tuple()) -> {ok, Pid::pid()} | {error, Reason::term()}.
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
	gen_server:cast(Node, reset_comm).

-spec mqtt_conf (Node :: pid(), Config :: map()) -> ok.
mqtt_conf (Node, Conf) ->
	gen_server:cast(Node, {mqtt_conf,Conf}).


%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init ({Id::string(), SimMan :: tuple()}) -> {ok, State :: #ap_state{}}.
init ({Id,SimMan}) ->
	process_flag(trap_exit, true),
	InitialState = prepare_state(Id,SimMan),
	gen_server:cast(self(),start_up),
	{ok, InitialState}.

-spec handle_cast (Request :: term(), State :: #ap_state{}) -> {noreply, NewState :: #ap_state{}} | {stop, Reason :: string(), NewState :: #ap_state{}}.
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

handle_cast (reset_comm, State) ->
	{noreply,ctrl_connect(State)};

handle_cast (ctlr_start_comm, State) ->
	{noreply, ctlr_start_comm(State)};

handle_cast ({mqtt_conf,Conf}, State) ->
	{noreply, configure_mqtt(Conf,State)};

handle_cast (R,State) ->
	?L_E(?DBGSTR("got unknown request: ~p",[R])),
	{noreply, State}.

-spec handle_call (Request::term(), From::{pid(),Tag::term()}, State::#ap_state{}) -> {reply, Reply:: ok | invalid | ignored, NewState:: #ap_state{}} | {stop, Reason:: term(), Reply:: ok | invalid | ignored, NewState:: #ap_state{}}.
handle_call ({exec_rpc, RPC}, _From, #ap_state{status=paused}=State) when is_map(RPC) andalso
												 						  is_map_key(<<"method">>,RPC) andalso
												 						  is_map_key(<<"id">>,RPC) ->
	case  maps:get(<<"method">>,RPC) of
		<<"echo">> -> 
			case ovsdb_ap_rpc:eval_req(<<"echo">>, maps:get(<<"id">>,RPC), RPC, State#ap_state.store) of
				{ok, Result} when is_map(Result) andalso is_map_key(<<"result">>,Result) ->
						ok = ovsdb_ap_comm:send_term(State#ap_state.comm,Result),
						{reply,ok,State};
				_ ->
						{reply, ignored, State}
			end;
		_ ->
			{reply, ignored, State}
	end;

handle_call ({exec_rpc, _RPC}, _From, #ap_state{status=paused}=State) ->
	{reply, ignored, State};

handle_call ({exec_rpc, RPC}, _From, State) when is_map(RPC) andalso
												 is_map_key(<<"method">>,RPC) andalso
												 is_map_key(<<"id">>,RPC) ->
	case ovsdb_ap_rpc:eval_req(maps:get(<<"method">>,RPC),
								maps:get(<<"id">>,RPC),
								RPC,
								State#ap_state.store) of
		{ok, ignore} ->
			{reply,ok,State};

		{ok, Result} when is_map(Result) andalso is_map_key(<<"result">>,Result) ->
			ok = ovsdb_ap_comm:send_term(State#ap_state.comm,Result),
			{reply,ok,State};

		{error, Reason} ->
			?L_E(?DBGSTR("RPC call '~s' failed with reason: ~s",[maps:get(<<"method">>,RPC),Reason])),
			{reply,invalid,State}
	end;
	
handle_call ({exec_rpc, RPC}, _From, State) when is_map(RPC) andalso
												 is_map_key(<<"result">>,RPC) andalso
												 is_map_key(<<"id">>,RPC) ->
	case ovsdb_ap_rpc:eval_resp(maps:get(<<"id">>,RPC),
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
	?L_E(?DBGSTR("invalid RPC: ~p~n",[RPC])),
	{reply,invalid, State};

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.

-spec handle_info (Msg::term(), State::#ap_state{}) -> {noreply, NewState::#ap_state{}} | {stop, Reason::term(), NewState:: #ap_state{}}.
handle_info({'EXIT', _Pid, normal}, State) ->
	{noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
	?L_E(?DBGSTR("Abnormal exit from ~p with reason: ~p",[Pid,Reason])),
	{noreply, State};
handle_info (Msg,State) ->
	?L_E(?DBGSTR("got unexpected info message ~p",[Msg])),
	{noreply, State}.

-spec terminate (Reason :: shutdown | normal, State :: #ap_state{}) -> ok.
terminate (_Reason, #ap_state{stats_ets=Tab}) ->
	ets:delete(Tab),
	ok.

-spec code_change (OldVersion :: term(), OldState ::#ap_state{}, Extra :: term()) -> {ok, NewState :: #ap_state{}}.
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

set_status (Status, #ap_state{status=OldStatus, config=Cfg}=State) ->
	ovsdb_client_handler:ap_status(Status,ovsdb_ap_config:id(Cfg)),
	?L_I(?DBGSTR("AP ~p : status change := ~p -> ~p",[self(),OldStatus,Status])),
	State#ap_state{status=Status}.




%--------startup_ap/1--------------------initiate startup sequence

-spec startup_ap (State :: #ap_state{}) -> NewState :: #ap_state{}.

startup_ap (#ap_state{status=init, config=Cfg, sim_manager=Man}=State) ->
	NewCfg =  ovsdb_ap_config:configure(Man,Cfg),
	set_status(ready,State#ap_state{config=NewCfg, comm=none}).




%--------run_simulation/1----------------start or resume simulation

-spec run_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

run_simulation (#ap_state{status=ready}=State) ->
	NewState = ctrl_connect(State),
	set_status(running,NewState);

run_simulation (#ap_state{status=paused}=State) ->
	set_status(running,State).
	



%--------stop_simulation/1----------------stops a simulation (clears internal state to ready)

-spec stop_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

stop_simulation (State) ->
	NewState = ctrl_disconnect(State),
	set_status(ready, NewState).




%--------pause_simulation/1----------------halts simulation (tear down of connections) but keeps internal state

-spec pause_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

pause_simulation (State) ->
	set_status(paused, State).




%--------cancel_simulation/1----------------shutdown and exit simulation (AP exits)

-spec cancel_simulation (State :: #ap_state{}) -> NewState :: #ap_state{}.

cancel_simulation (State) ->
	State.




%--------ctrl_connect/1------------------connect to either the tip redirector or manager based on state / old connections are closed if open

-spec ctrl_connect (State :: #ap_state{}) -> NewState :: #ap_state{}.

ctrl_connect (#ap_state{comm=none, status=ready, config=Cfg}=State) ->
	Opts = [
		{host, ovsdb_ap_config:tip_redirector(host,Cfg)},
		{port, ovsdb_ap_config:tip_redirector(port,Cfg)},
		{ca, ovsdb_ap_config:ca_certs(Cfg)},
		{cert, ovsdb_ap_config:client_cert(Cfg)}
	],
	{ok, Comm} = ovsdb_ap_comm:start_link(Opts),
	gen_server:cast(self(),ctlr_start_comm),
	State#ap_state{comm=Comm};

ctrl_connect (#ap_state{comm=none, status=running, config=Cfg}=State) ->
	O = case ovsdb_ap_config:tip_manager(host,Cfg) of
		[] ->
			[{host, ovsdb_ap_config:tip_redirector(host,Cfg)},
			 {port, ovsdb_ap_config:tip_redirector(port,Cfg)}];
		_ ->
			[{host, ovsdb_ap_config:tip_manager(host,Cfg)},
			 {port, ovsdb_ap_config:tip_manager(port,Cfg)}]
	end,
	Opts = [{ca, ovsdb_ap_config:ca_certs(Cfg)},{cert, ovsdb_ap_config:client_cert(Cfg)}|O],
	{ok, Comm} = ovsdb_ap_comm:start_link(Opts),
	gen_server:cast(self(),ctlr_start_comm),
	State#ap_state{comm=Comm};

ctrl_connect (#ap_state{comm=Comm}=State) ->
	ovsdb_ap_comm:end_comm(Comm),
	ctrl_connect (State#ap_state{comm=none}).




%--------ctrl_disconnect/1---------------disconnect and closes communication port but otherwise does not chenge status

-spec ctrl_disconnect (State :: #ap_state{}) -> NewState :: #ap_state{}.

ctrl_disconnect (#ap_state{comm=none}=State) ->
	State;

ctrl_disconnect (#ap_state{comm=Comm}=State) ->
	ovsdb_ap_comm:end_comm(Comm),
	State#ap_state{comm=none}.



%--------ctlr_start_comm/1---------------asychrounously starts the connection after comm is created

-spec ctlr_start_comm (State :: #ap_state{}) -> NewState :: #ap_state{}.

ctlr_start_comm (#ap_state{comm=Comm}=State) ->
	ovsdb_ap_comm:start_comm(Comm),
	State.



%--------configure_mqtt/2----------------send configuration to MQTT client to establish a connection

-spec configure_mqtt (Config :: #{binary():=binary()}, #ap_state{}) -> NewState :: #ap_state{}.

configure_mqtt (Cfg,State) ->
	?L_I(?DBGSTR("AP->MQTT set configuration to: ~w",[Cfg])),
	State#ap_state{mqtt=running}.
