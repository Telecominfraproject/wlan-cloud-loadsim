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
-include("../include/ovsdb_definitions.hrl").
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/opensync_stats.hrl").


-define(SERVER, ?MODULE).



%% API
-export([launch/3]).
-export([start_ap/1,stop_ap/1,pause_ap/1,cancel_ap/1]).

%% comm API
-export([rpc_cmd/2,rpc_request/2,reset_comm/1,mqtt_conf/2,post_event/4,post_event/3,check_publish_monitor/1,check_for_mqtt_updates/1,set_ssid/2]).

%% debug API
-export([dbg_status/1,dbg_details/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

%% data structures
-type ap_status() :: init | ready | running | paused.
-export_type([ap_status/0]).

-record(dbg_info,{
	substate = initial :: initial | active | idle,
	recons = 0 :: integer(),
	published = 0 :: integer(),
	last_msg_down = <<>> :: binary(),
	last_msg_up = <<>> :: binary(),
	last_recon_msg_down = <<>> :: binary(),
	last_recon_msg_up = <<>> :: binary()
}).

-record(ap_state, { 	
	id :: UUID::binary(),				% the ID of the access point we cary around
	ca_name :: string() | binary(),		% ???
	status = init :: ap_status(),		% internal status
	config :: ovsdb_ap_config:cfg(),	% simulated AP configuration (model, serial etc.)
	comm = none :: none | pid(),		% OVSDB SSL communication
	mqtt = idle :: idle | running,		% mqtt status (external process)
	store :: ets:tid(),					% the tables where OVSDB server stores info
	req_queue :: ets:tid(),				% not used at the moment ... used to que request IDs
	reporting :: timer:tref(),			% statistics reporting interval timer reference
	stats_ets :: ets:tid(),				% statistics table
	recons = 0 :: integer(),			% number of reconnection events
	dbg_info = #dbg_info{} :: #dbg_info{},	% debug info of the AP state
	updates = none :: none | timer:tref()	% timer reference for periodic updates from MQTT while running
}).

-record (ap_events, {
	stamp = {0,0} :: {Time :: integer(), UMI :: integer()},
	event = none :: atom(),
	args = {} :: tuple(),
	comment = <<>> :: binary()
}).

-record (stats_vars, {
	dummy = 0 :: integer (),  % keypos shoudl alwys be first entry
	recon_backoff = 250 :: integer(),
	stats_count = 0 :: integer()
}).

%%%============================================================================
%%% API
%%%============================================================================
-spec launch (CAName :: string() | binary(), Id::binary(), Options :: [{atom(),term()}]) -> {ok, Pid :: pid()} | generic_error().
launch (CAName, Id, Options) ->
	gen_server:start_link(?MODULE, {CAName, Id, Options}, []).

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
%%% Internal module API
%%%============================================================================
-spec rpc_cmd (Node :: pid(), Rpc :: term()) -> ok.
rpc_cmd (Node,Rpc) ->
	gen_server:cast(Node,{exec_rpc,Rpc}).

-spec rpc_request (Node :: pid(), Rpc :: term()) -> ok.
rpc_request (Node,Rpc) ->
	gen_server:cast(Node,{exec_rpc_req,Rpc}).

-spec reset_comm (Node :: pid()) -> ok.
reset_comm (Node) ->
	gen_server:cast(Node, reset_comm).

-spec mqtt_conf (Node :: pid(), Config :: map()) -> ok.
mqtt_conf (Node, Conf) ->
	gen_server:cast(Node, {mqtt_conf,Conf}).

-spec post_event (Node :: pid(), Event :: atom(), Args :: tuple(), Comment :: binary() | string()) -> ok.
post_event (Node, Event, Args, Comment) ->
	gen_server:cast(Node,{stats_update, {Event, Args, Comment}}).

-spec post_event (Event :: atom(), Args :: tuple(), Comment :: binary() | string()) -> ok.
post_event (Event, Args, Comment) ->
	post_event(self(),Event,Args,Comment).

-spec check_publish_monitor (Node :: pid()) -> ok.
check_publish_monitor (Node) ->
	gen_server:cast(Node,check_publish_monitor).

-spec check_for_mqtt_updates (Node :: pid()) -> ok.
check_for_mqtt_updates (Node) ->
	gen_server:cast(Node,check_mqtt_updates).

-spec set_ssid(Node :: pid(), SSID :: binary()) -> ok.
set_ssid (Node,SSID) ->
	gen_server:cast(Node,{set_ssid,SSID}).

-spec dbg_status (Node :: pid()) -> ok.
dbg_status (Node) ->
	gen_server:call(Node,dbg_status).

-spec dbg_details (Node :: pid()) -> ok.
dbg_details (Node) ->
	gen_server:call(Node,dbg_details).


%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================
-spec init ({CAName :: string() | binary(), Id::binary(), Options :: [{atom(),term()}]}) -> {ok, State :: #ap_state{}}.
init ({CAName, Id, Options}) ->
	process_flag(trap_exit, true),
	InitialState = prepare_state(CAName,Id,Options),
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
	?L_E(?DBGSTR("can not run simulation if not in ready or paused  state")),
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
	{noreply, check_mqtt(Conf,State)};

handle_cast ({stats_update,Event},State) ->
	true = update_statistics(Event,State#ap_state.stats_ets),
	NewState = update_debug_state(Event,State),
	{noreply, NewState};

handle_cast (check_mqtt_updates, State) ->
	S = request_mqtt_updates(State),
	{noreply, S};

handle_cast (check_publish_monitor, State) ->
	case ovsdb_ap_monitor:publish_monitored(State#ap_state.store) of
		{ok, more} ->
			timer:apply_after(500,?MODULE,check_publish_monitor,[self()]);
		{ok, done} ->
			ok
	end,
	{noreply, State};

handle_cast (send_report,State) ->
	{noreply, report_statistics(State)};

handle_cast ({exec_rpc, RPC}, #ap_state{status=paused}=State) when is_map(RPC) andalso
												 					is_map_key(<<"method">>,RPC) andalso
												 					is_map_key(<<"id">>,RPC) ->
	case  maps:get(<<"method">>,RPC) of
		<<"echo">> -> 
			case ovsdb_ap_rpc:eval_req(<<"echo">>, maps:get(<<"id">>,RPC), RPC, State#ap_state.store) of
				{ok, Result} when is_map(Result) andalso is_map_key(<<"result">>,Result) ->
						ok = ovsdb_ap_comm:send_term(State#ap_state.comm,Result),
						NewState = update_debug_state(#ap_events{event=set_idle},State),
						{reply,ok,NewState};
				_ ->
						{reply, ignored, State}
			end;
		_ ->
			{noreply, State}
	end;
handle_cast ({exec_rpc, _RPC}, #ap_state{status=paused}=State) ->
	{noreply, State};
handle_cast ({exec_rpc, RPC}, State) when is_map(RPC) andalso
										  is_map_key(<<"method">>,RPC) andalso
										  is_map_key(<<"id">>,RPC) ->
	NewState = case  maps:get(<<"method">>,RPC) of
		<<"echo">> -> 
			update_debug_state(#ap_events{event=set_idle},State);
		_ ->
			update_debug_state(#ap_events{event=set_active},State)
	end,
	case ovsdb_ap_rpc:eval_req(maps:get(<<"method">>,RPC),
								maps:get(<<"id">>,RPC),
								RPC,
								State#ap_state.store) of
		
		{ok, Result} when is_map(Result) andalso is_map_key(<<"result">>,Result) ->
			R= io_lib:format("~p",[Result]),
			Bytes = length(lists:flatten(R)),
			?L_I(?DBGSTR("RPC RESULT (~s): ~Bbytes",[maps:get(<<"id">>,RPC),Bytes])),
			ok = ovsdb_ap_comm:send_term(State#ap_state.comm,Result),
			{noreply, NewState};

		{ok, Result} when is_binary(Result) ->
			?L_I(?DBGSTR("RPC RAW RESULT (~s): ~Bbytes",[maps:get(<<"id">>,RPC),byte_size(Result)])),
			ok = ovsdb_ap_comm:send_term(State#ap_state.comm,Result),
			{noreply, NewState};

		{error, Reason} ->
			?L_E(?DBGSTR("RPC call '~s' failed with reason: ~s",[maps:get(<<"method">>,RPC),Reason])),
			{norepl,NewState}
	end;
handle_cast ({exec_rpc, RPC},  State) when is_map(RPC) andalso
										   is_map_key(<<"result">>,RPC) andalso
										   is_map_key(<<"id">>,RPC) ->
	case ovsdb_ap_rpc:eval_resp(maps:get(<<"id">>,RPC),
								 RPC,
								 State#ap_state.req_queue,
								 State#ap_state.store) of
		ok ->
			{noreply,State};

		{error, Reason} ->
			?L_E(?DBGSTR("RPC response to request '~B' failed with reason: ~p",[maps:get(<<"id">>,RPC),Reason])),
			{noreply,State}
	end;
handle_cast ({exec_rpc, RPC},State) ->
	?L_E(?DBGSTR("invalid RPC: ~p~n",[RPC])),
	{reply,noreply, State};

handle_cast ({exec_rpc_req, _RPC}, #ap_state{status=paused}=State) ->
	{noreply, State};
handle_cast ({exec_rpc_req, RPC}, State) when is_map(RPC) andalso
										      is_map_key(<<"method">>,RPC) andalso
										      is_map_key(<<"id">>,RPC) ->
	R= io_lib:format("~p",[RPC]),
	Bytes = length(lists:flatten(R)),
	?L_I(?DBGSTR("RPC UPSTREAM REQUEST (~s): ~Bbytes",[maps:get(<<"id">>,RPC),Bytes])),
	ok = ovsdb_ap_comm:send_term(State#ap_state.comm,RPC),
	{noreply, State};

handle_cast ({set_ssid,SSID},#ap_state{config=Cfg}=State) ->
	%% io:format("SETTING SSID: ~s~n",[SSID]),
	mqtt_client_manager:set_ssid(ovsdb_ap_config:caname(Cfg),ovsdb_ap_config:serial(Cfg),SSID),
	{noreply,State};

handle_cast (R,State) ->
	?L_E(?DBGSTR("got unknown request: ~p",[R])),
	{noreply, State}.

-spec handle_call (Request :: term(), From :: {pid(),Tag::term()}, State :: #ap_state{}) -> {reply, Reply :: ok | invalid | ignored, NewState :: #ap_state{}} | {stop, Reason :: term(), Reply :: ok | invalid | ignored, NewState :: #ap_state{}}.
handle_call (dbg_status, _, State) ->
	{reply, assemble_dbg_status(State), State};

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.

-spec handle_info (Msg :: term(), State :: #ap_state{}) -> {noreply, NewState :: #ap_state{}} | {stop, Reason :: term(), NewState :: #ap_state{}}.
handle_info({'EXIT', _Pid, normal}, State) ->
	{noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
	?L_E(?DBGSTR("Abnormal exit from ~p with reason: ~p",[Pid,Reason])),
	{noreply, State};

handle_info({client_stats,Serial,Stats},State) ->
	S = handle_mqtt_stats_update(Serial,Stats,State),
	{noreply, S};

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

-spec prepare_state (CAName :: string() | binary(), ID :: binary(), Options :: [{atom(),term()}]) -> State :: #ap_state{}.
prepare_state (CAName, ID, Options) ->
	Store = ets:new(ovsdb_ap,[bag,private,{keypos, 1}]),
	Stats = ets:new(ovsdb_ap_stats,[ordered_set,private,{keypos, 2}]),
	{ok, Tref} = timer:apply_interval(proplists:get_value(report_int,Options,?AP_REPORT_INTERVAL),
										gen_server,cast,[self(),send_report]),
	Redirector = proplists:get_value(redirector,Options,<<"">>),
	update_statistics({report_mark,{},<<>>},Stats),
	ets:insert(Stats,#stats_vars{}),
	#ap_state{
		id = ID,
		ca_name = CAName,
		config = ovsdb_ap_config:new(CAName,ID,Store,Redirector),
		status = init,
		store = Store,
		req_queue = ets:new(ovsdb_ap_req,[ordered_set,private,{keypos, 1}]),
		reporting = Tref,
		stats_ets = Stats
	}.

%--------set_status/1--------------------sets internal status + broadcast status to handler
-spec set_status (Status :: ap_status(), State :: #ap_state{}) -> NewState :: #ap_state{}.
set_status (Status, #ap_state{status=OldStatus, config=Cfg}=State) ->
	ovsdb_client_handler:ap_status(Status,ovsdb_ap_config:id(Cfg)),
	post_event(status_change,{OldStatus,Status},io_lib:format("status change (~s) := ~p -> ~p",[ovsdb_ap_config:id(Cfg),OldStatus,Status])),
	?L_I(?DBGSTR("AP ~p : status change (~s) := ~p -> ~p",[self(),ovsdb_ap_config:id(Cfg),OldStatus,Status])),
	start_stop_mqtt_updates(State#ap_state{status=Status}).

-spec start_stop_mqtt_updates (State :: #ap_state{}) -> NewState :: #ap_state{}.
start_stop_mqtt_updates(#ap_state{status=running, updates=none}=State) ->
	{ok, Ref} = timer:apply_interval(60000,?MODULE,check_for_mqtt_updates,[self()]),
	State#ap_state{updates=Ref};
start_stop_mqtt_updates(#ap_state{status=running}=State) ->
	State;
start_stop_mqtt_updates(#ap_state{updates=U}=State) when U =/= none->
	{ok, cancel} = timer:cancel(U),
	State#ap_state{updates=none};
start_stop_mqtt_updates(State) ->
	State.

%--------startup_ap/1--------------------initiate startup sequence
-spec startup_ap (State :: #ap_state{}) -> NewState :: #ap_state{}.
startup_ap (#ap_state{status=init, config=Cfg}=State) ->
	NewCfg =  ovsdb_ap_config:configure(Cfg),
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
	_ = timer:cancel(State#ap_state.reporting),
	State.

%--------ctrl_connect/1------------------connect to either the tip redirector or manager based on state / old connections are closed if open
-spec ctrl_connect (State :: #ap_state{}) -> NewState :: #ap_state{}.
ctrl_connect (#ap_state{comm=none, status=ready, config=Cfg, id=ID}=State) ->
	Opts = [
		{host, ovsdb_ap_config:tip_redirector(host,Cfg)},
		{port, ovsdb_ap_config:tip_redirector(port,Cfg)},
		{ca, ovsdb_ap_config:ca_certs(Cfg)},
		{cert, ovsdb_ap_config:client_cert(Cfg)},
		{id, ID},
		{key, ovsdb_ap_config:client_key(Cfg)}
	],
	{ok, Comm} = ovsdb_ap_comm:start_link(Opts),
	gen_server:cast(self(),ctlr_start_comm),
	post_event(tip_connect,{<<"redirector">>},<<"connecting to the TIP controller (redirector)">>),
	State#ap_state{comm=Comm};

ctrl_connect (#ap_state{comm=none, status=running, config=Cfg, id=ID}=State) ->
	O = case ovsdb_ap_config:tip_manager(host,Cfg) of
		[] ->
			post_event(tip_connect,{<<"redirector">>},<<"connecting to the TIP controller (redirector)">>),
			[{host, ovsdb_ap_config:tip_redirector(host,Cfg)},
			 {port, ovsdb_ap_config:tip_redirector(port,Cfg)}];
		_ ->
			post_event(tip_connect,{<<"manager">>},<<"connecting to the TIP controller (manager)">>),
			[{host, ovsdb_ap_config:tip_manager(host,Cfg)},
			 {port, ovsdb_ap_config:tip_manager(port,Cfg)}]
	end,
	Opts = [{cacert, ovsdb_ap_config:ca_certs(Cfg)},
	        {cert, ovsdb_ap_config:client_cert(Cfg)},
	        {key,ovsdb_ap_config:client_key(Cfg)},
			{id, ID} |O],
	{ok, Comm} = ovsdb_ap_comm:start_link(Opts),
	gen_server:cast(self(),ctlr_start_comm),
	State#ap_state{comm=Comm};

ctrl_connect (#ap_state{comm=none}=State) ->
	State;

ctrl_connect (#ap_state{comm=Comm}=State) ->
	ovsdb_ap_comm:end_comm(Comm),
	post_event(tip_connect,{<<"down">>},<<"TIP contoller connection relinquished">>),
	ctrl_connect (State#ap_state{comm=none}).

%--------ctrl_disconnect/1---------------disconnect and closes communication port but otherwise does not chenge status
-spec ctrl_disconnect (State :: #ap_state{}) -> NewState :: #ap_state{}.
ctrl_disconnect (#ap_state{comm=none}=State) ->
	State;

ctrl_disconnect (#ap_state{comm=Comm}=State) ->
	ovsdb_ap_comm:end_comm(Comm),
	post_event(tip_connect,{<<"down">>},<<"TIP contoller connection relinquished">>),
	stop_mqtt(State#ap_state{comm=none}).

%--------ctlr_start_comm/1---------------asychrounously starts the connection after comm is created
-spec ctlr_start_comm (State :: #ap_state{}) -> NewState :: #ap_state{}.
ctlr_start_comm (#ap_state{comm=Comm, store=Store}=State) ->
	ovsdb_ap_comm:start_comm(Comm),
	post_event(tip_connect,{<<"start_comm">>},<<"TIP contoller start communication">>),
	case ets:match(Store,#'AWLAN_Node'{mqtt_settings='$1',_='_'}) of
		[[[<<"map">>,MQTT]]] when is_list(MQTT) andalso length(MQTT)>0 ->
			Map = maps:from_list([{K,V}||[K,V]<-MQTT]),
			check_mqtt(Map,State);
		_ ->
			State
	end.

%%==============================================================================
%% managing mqtt
-spec check_mqtt (Config :: #{binary():=binary()}, #ap_state{}) -> NewState :: #ap_state{}.
check_mqtt (Cfg,#ap_state{ca_name=CAName, id=ID}=State) ->
	?L_I(?DBGSTR("AP->MQTT check configuration")),
	case mqtt_client_manager:is_running(CAName,ID) of
		{ ok , {_, MQTTCfgCurr}} ->
			case mqtt_needs_restart(MQTTCfgCurr,Cfg) of
				true ->
					_ = stop_mqtt(State),
					start_mqtt(Cfg,State);
				false ->
					State#ap_state{mqtt=running}
			end;
		_ -> 
			start_mqtt(Cfg,State)
	end.

-spec mqtt_needs_restart (ActiveConfig :: gen_configuration(), NewConfig :: gen_configuration()) -> boolean().
mqtt_needs_restart (#{broker:=CurBroker, port:=CurPort},#{broker:=NewBroker, port:=NewPort}) when CurBroker==NewBroker andalso CurPort==NewPort ->
	false;
mqtt_needs_restart (_,_) ->
	true.

-spec start_mqtt(gen_configuration(), #ap_state{}) -> NewState :: #ap_state{}.
start_mqtt (Cfg,#ap_state{ca_name=CAName, id=ID, mqtt=idle}=State) ->
	post_event(mqtt,{<<"set_config">>},<<"start an MQTT client">>),
	_ = mqtt_client_manager:start_client(CAName,ID,Cfg),
	State#ap_state{mqtt=running};
start_mqtt (_,State) ->
	?L_E(?DBGSTR("MQTT start request, but client already running!")),
	State.

-spec stop_mqtt (State::#ap_state{}) -> NewState::#ap_state{}.
stop_mqtt(#ap_state{mqtt=idle}=State) ->
	State;
stop_mqtt(#ap_state{ca_name=CAName, id=ID}=State) ->
	_ = mqtt_client_manager:stop_client(CAName,ID),
	State#ap_state{mqtt=idle}.

-spec request_mqtt_updates (State :: #ap_state{}) -> NewState :: #ap_state{}.
request_mqtt_updates (#ap_state{config=Cfg} = State) ->
	CA = ovsdb_ap_config:caname(Cfg),
	Serial = ovsdb_ap_config:serial(Cfg),
	Mqtt = mqtt_client_manager:get_client_pid(CA,Serial),
	Mqtt ! {send_stats, self()},
	State.

-spec handle_mqtt_stats_update (Serial :: binary(), Stats :: #{binary() => #'Client.Stats'{}}, State :: #ap_state{}) -> NewState :: #ap_state{}.
handle_mqtt_stats_update (_Serial,_Stats,State) ->
	%% io:format("GOT MQTT STATS: ->republishing changes~n"),
	%%ovsdb_ap_monitor:refresh_publications(State#ap_state.store),
	ovsdb_ap_simop:update_wifi_clients(State#ap_state.store),
	ovsdb_ap_simop:update_dhcp_leases(State#ap_state.store),
	check_publish_monitor(self()),
	State.
	
%%==============================================================================
%% managing statistics of access point
-spec update_statistics (Event :: tuple(), Stats :: ets:tid()) -> true.
update_statistics ({Event,Args,Comment},Stats) ->
	ETag = {erlang:system_time(),erlang:unique_integer([monotonic])},
	ets:insert(Stats,#ap_events{
			stamp = ETag,
			event = Event,
			args = Args,
			comment = case Comment of
						A when is_binary(A) -> A;
						A when is_list(A) -> list_to_binary(A);
						_ -> <<>>
					end
		}).

%--------report_statistics/1-------------generate an AP specific statistics report and send it to the handler
-spec report_statistics (State :: #ap_state{}) -> NewState :: #ap_state{}.
report_statistics (#ap_state{status=ready}=State) ->
	State;
report_statistics (#ap_state{stats_ets=S,id=ID}=State) ->
	% D = ets:match(Stats,'$1'),
	% io:format("all DATA: ~n~p~n",[D]),
	Ri = report_interval(S),
	RX = received_bytes(S),
	TX = sent_bytes(S),
	Stats = #ap_statistics{
			stamp = erlang:system_time(),
			interval = Ri,
			rx_bytes = RX,
			rx_bps = RX / Ri,
			tx_bytes = TX,
			tx_bps = TX / Ri,
			dropped = comm_dropped(S),
			restarts  = comm_restart(S),
			latency = 0
		},
	ets:delete_all_objects(S),
	ovsdb_client_handler:push_ap_stats(Stats,ID),
	update_statistics({report_mark,{},<<>>},S),
	State.

-spec report_interval(S :: ets:tid()) -> IntervalInMs :: integer().
report_interval (S) ->
	case ets:match(S,{ap_events,{'$1','_'},report_mark,'_','_'}) of
		[[R]] ->
			erlang:convert_time_unit(erlang:system_time()-R, native, millisecond);
		_ -> 
			?AP_REPORT_INTERVAL
	end.

-spec received_bytes (S :: ets:tid()) -> Bytes :: integer().
received_bytes (S) ->
	lists:foldl(fun(X,A) -> X+A end,0,[V||[V]<-ets:match(S,{ap_events,'_',comm_event,{<<"RX">>,'$1'},'_'})]).
	
-spec sent_bytes (S :: ets:tid()) -> Bytes :: integer().
sent_bytes (S) ->
	lists:foldl(fun(X,A) -> X+A end,0,[V||[V]<-ets:match(S,{ap_events,'_',comm_event,{<<"TX">>,'$1'},'_'})]).
	
-spec comm_dropped (S :: ets:tid()) -> ErrorCount :: integer().
comm_dropped (S) ->
	length(ets:match_object(S,{ap_events,'_',comm_error,{<<"socket_closed">>},'_'})).
	
-spec comm_restart(S::ets:tid()) -> RestartCount::integer().
comm_restart(S) ->
	length(ets:match_object(S,{ap_events,'_',tip_connect,{<<"start_comm">>},'_'})). 


-spec assemble_dbg_status(State :: #ap_state{}) -> DbgInfo :: #status_info{}.
assemble_dbg_status (#ap_state{store=Store}=State) ->
	#status_info{
		id = State#ap_state.id,
		status = State#ap_state.status,
		substate = State#ap_state.dbg_info#dbg_info.substate,
		mqtt = State#ap_state.mqtt,
		recons = State#ap_state.dbg_info#dbg_info.recons,
		clients = length(ets:match_object(Store,#'DHCP_leased_IP'{_='_'})),
		monitors = length(ets:match_object(Store,#monitors{_='_'})),
		published = State#ap_state.dbg_info#dbg_info.published
	}.

-spec update_debug_state(Event :: tuple(), State :: #ap_state{}) -> NewState :: #ap_state{}.
update_debug_state (#ap_events{event=sock_recon}, #ap_state{dbg_info=Info}=State) ->
	New = Info#dbg_info{recons=Info#dbg_info.recons+1},
	State#ap_state{dbg_info=New};
update_debug_state (#ap_events{event=set_idle}, #ap_state{dbg_info=Info}=State) ->
	New = Info#dbg_info{substate=idle},
	State#ap_state{dbg_info=New};
update_debug_state (#ap_events{event=set_active}, #ap_state{dbg_info=Info}=State) ->
	New = Info#dbg_info{substate=active},
	State#ap_state{dbg_info=New};
update_debug_state (_, State) ->
	State.
