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

-compile({parse_transform, lager_transform}).

-behaviour(gen_server).
-behaviour(gen_sim_client).

-include("../include/common.hrl").
-include("../include/ovsdb_definitions.hrl").
-include("../include/inventory.hrl").

-define(SERVER, ?MODULE).


%% API
-export([start_link/0,creation_info/0]).
-export([set_configuration/1, start/1, start/2, restart/2, stop/1, stop/2, pause/1, pause/2, cancel/1, cancel/2, resume/1, report/0]).
-export([push_ap_stats/2,dump_clients/0,list_ids/0,all_ready/0,register/2,set_state/1,dump_status/0]).
-export([create_aps/2,start_aps/3,stop_aps/3,pause_aps/3,cancel_aps/3,update_stats/0,set_ap_status/2,
					get_client_state/1]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).


%% data structures
-record (state, {
	clients_serial  = #{}   :: #{ Serial::binary() => Pid::pid() },
	clients_pid     = #{}   :: #{ Pid::pid() => Serial::binary() },
	clients_status  = #{}   :: ovsdb_client_status_map(),
	config          = #{}   :: #{atom()=>term()},
	running_stats           :: #ap_statistics{},
	running_count   = 0     :: non_neg_integer(),
	total_stats             :: #ap_statistics{},
	total_count     = 0     :: non_neg_integer(),
	timer                   :: owls_timers:tms(),
	update_stats_timer = none :: none | timer:tref() ,
	state_num = 0           :: non_neg_integer(),
	state = undefined       :: atom()
}).

-record (ovsdb_statistics_report, {
	stamp       :: non_neg_integer(), 		%% os timestmap
	configured  :: non_neg_integer(),		  %% configured clients
	running     :: non_neg_integer(),			%% actively running clients
	paused      :: non_neg_integer(),			%% currently paused clients
	reconnecting:: non_neg_integer(),     %% aps in reconnecting mode
	ready       :: non_neg_integer(),
	dropped     :: non_neg_integer(),			%% number of dropped connections since last interval
	restarts    :: non_neg_integer(),
	average_tx_rate   :: non_neg_integer(),			%% average trasnmitted bytes (of all access points)
	average_rx_rate   :: non_neg_integer(),			%% average received bytes (of all access points)
	tx_bytes          :: non_neg_integer(),			%% maximum bytes transmitted
	rx_bytes          :: non_neg_integer()			%% maximum bytes received
}).


%%%============================================================================
%%% HANDLER - API
%%%============================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

dump_clients () ->
	gen_server:call(?SERVER,dump_clients).

dump_status()->
	gen_server:call(?SERVER,dump_status).

list_ids() ->
	gen_server:call(?SERVER,list_ids).

all_ready() ->
	gen_server:call(?SERVER,all_ready).

update_stats() ->
	gen_server:call(?SERVER,update_stats).

%%% gen_sim_clients behaviour
-spec restart( all | [UUID::binary()], Attributes::#{ atom() => term() }) -> ok | generic_error().
restart( _UIDS,_Attributes ) ->
	ok.

-spec set_configuration (Cfg:: #{any() => any()}) -> ok | generic_error().
set_configuration (Cfg) ->
	gen_server:call(?SERVER,{set_config, Cfg}).

-spec start(What :: all | [UUID::binary()]) -> ok | generic_error().
start (What) ->
	start(What,#{}).

-spec start(What :: all | [UUID::binary()],Options :: #{atom()=>term()}) -> ok | generic_error().
start (What,Options) ->
	gen_server:call(?SERVER,{api_cmd_start, What, Options}).

-spec stop(What :: all | [UUID::binary()]) -> ok | generic_error().
stop (What) ->
	stop (What,#{}).

-spec stop(What :: all | [UUID::binary()],Options :: #{atom()=>term()}) -> ok | generic_error().
stop (What,Options) ->
	gen_server:call(?SERVER,{api_cmd_stop, What, Options}).

-spec pause(What :: all | [UUID::binary()]) -> ok | generic_error().
pause (What) ->
	pause (What,#{}).

-spec pause(What :: all | [UUID::binary()],Options :: #{atom()=>term()}) -> ok | generic_error().
pause (What,Options) ->
	gen_server:call(?SERVER,{api_cmd_pause, What, Options}).

-spec cancel(What :: all | [UUID::binary()]) -> ok | generic_error().
cancel (What) ->
	cancel (What,#{}).

-spec cancel(What :: all | [UUID::binary()],Options :: #{atom()=>term()}) -> ok | generic_error().
cancel (What, Options) ->
	gen_server:call(?SERVER,{api_cmd_cancel, What, Options}).

-spec report () -> {ok, Report :: term()} | generic_error().
report () ->
	gen_server:call(?SERVER,get_report).

resume (_) ->
	ok.

register(ID,Pid)->
	gen_server:cast(?SERVER,{register,ID,Pid}).

set_state(NewState)->
	gen_server:cast(?SERVER,{set_state,NewState} ).

get_client_state(Id)->
	gen_server:call(?SERVER,{get_client_state,Id}).

%%%============================================================================
%%% CLIENT CALLBACK API
%%%============================================================================
-spec set_ap_status (Status::ovsdb_client_status(), Id::binary()) -> ok.
set_ap_status (Status, Id) ->
	gen_server:cast(?SERVER,{set_ap_status,Status,Id}).

-spec push_ap_stats (APStatistics :: #ap_statistics{}, Id :: string() |  binary()) -> ok.
push_ap_stats (Stats, Id) ->
	gen_server:cast(?SERVER,{ap_stats,Stats,utils:safe_binary(Id)}).

%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================
-spec start_link () -> {ok, Pid :: pid()} | generic_error().
start_link () ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).

-spec init (Args :: term()) -> {ok, State :: #state{}} | {stop, Reason :: term()}.
init (_) ->
	process_flag(trap_exit, true),
	{ok, #state{timer=owls_timers:new(millisecond),
	                clients_pid = #{} ,
	                clients_serial = #{},
	                state = init ,
	                clients_status = #{},
                  running_stats = #ap_statistics{},
	                running_count = 0,
	                total_stats = #ap_statistics{},
	                total_count = 0,
									update_stats_timer = none}}.

-spec handle_cast (Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} | {stop, Reason :: term(), NewState :: #state{}}.
handle_cast ({status,Status,Id}, State) ->
	NewClientStatus = maps:put(Id,Status,State#state.clients_status),
	{noreply, State#state{ clients_status = NewClientStatus }};
handle_cast ({ap_stats,NewStats,_Id},State) ->
	{noreply, State#state{ running_count = State#state.running_count+1,
		total_count = State#state.total_count+1,
		running_stats = add_ap_statistics(State#state.running_stats,NewStats),
		total_stats = add_ap_statistics(State#state.total_stats,NewStats)}};
handle_cast ({set_state,NewState},#state{}=State) ->
	case NewState of
		cancelled ->
			utils:select( State#state.update_stats_timer == none, ok, timer:cancel(State#state.update_stats_timer) ),
			{ noreply, State#state{ state = init, config = #{}, clients_pid = #{}, clients_serial = #{}, update_stats_timer = none }};
		started ->
			{ok,TRef} = timer:apply_interval(2000,?MODULE,update_stats,[]),
			{ noreply, State#state{ update_stats_timer = TRef }};
		paused->
			utils:select( State#state.update_stats_timer == none, ok, timer:cancel(State#state.update_stats_timer) ),
			{noreply, State#state{state=NewState,update_stats_timer = none}};
  	stopped->
		  utils:select( State#state.update_stats_timer == none, ok, timer:cancel(State#state.update_stats_timer) ),
		  {noreply, State#state{state=NewState,update_stats_timer = none}};
		configured ->
			{noreply, State#state{state=NewState}};
		_ ->
			io:format("OVSDB_HANDLER: unknown state: ~p~n",[NewState]),
			{noreply, State#state{state=NewState}}
end;

handle_cast ({register,Id,Pid},#state{}=State) ->
	NewClientPids = maps:put( Pid, Id, State#state.clients_pid),
	NewClientSerials = maps:put( Id, Pid, State#state.clients_serial),
	{noreply, State#state{ clients_pid = NewClientPids, clients_serial = NewClientSerials }};

handle_cast ({set_ap_status,Status,Id},#state{}=State) ->
	NewClientStatus = maps:put( Id, Status, State#state.clients_status),
	{noreply, State#state{ clients_status = NewClientStatus }};

handle_cast (_,State) ->
	{noreply, State}.

-spec handle_call (Request :: term(), From :: {pid(),Tag::term()}, State :: #state{}) -> {reply, Reply :: term(), NewState :: #state{}} | {stop, Reason :: term(), Reply :: term(), NewState :: #state{}}.
handle_call ({set_config, Cfg},_From,#state{ state = init }=State) ->
	spawn_link(?MODULE,create_aps,[Cfg,self()]),
	{reply, ok, State#state{ config = Cfg }};

handle_call ({api_cmd_start,Which,Options},_From, State) ->
	case State#state.state of
		configured ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = starting }};
		paused ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = starting }};
		stopped ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = starting }};
		started ->
			{reply, ok, State};
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_stop, Which, Options},_,State) ->
	io:format("Actively stopping:~n"),
	case State#state.state of
		configured ->
			io:format(">>>1~n"),
			spawn_link(?MODULE,stop_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = stopping }};
		paused ->
			io:format(">>>2~n"),
			spawn_link(?MODULE,stop_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = stopping }};
		stopped ->
			io:format(">>>3~n"),
			{reply, ok, State};
		started ->
			io:format(">>>4~n"),
			spawn_link(?MODULE,stop_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = stopping }};
		cancelled ->
			io:format(">>>5~n"),
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			io:format(">>>6: state: ~p~n",[State#state.state]),
			{reply, ok, State}
	end;

handle_call ({api_cmd_pause, Which, Options},_,State) ->
	case State#state.state of
		configured ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = pausing }};
		paused ->
			{reply, ok, State};
		stopped ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = pausing }};
		started ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = pausing }};
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_resume, Which, Options},_,State) ->
	case State#state.state of
		paused ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = starting }};
		stopped ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = starting }};
		started ->
			{reply, ok, State };
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_cancel, Which, Options},_,State) ->
	case State#state.state of
		configured ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = cancelling }};
		paused ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = cancelling }};
		stopped ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = cancelling }};
		started ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#state.clients_pid]),
			{reply, ok, State#state{ state = cancelling }};
		cancelled ->
			{reply, ok,State};
		_ ->
			{reply, ok, State}
	end;

handle_call (update_stats, _From, State) ->
	NewState = update_statistics(State),
	{reply, ok, NewState };

handle_call (dump_clients,_From, #state{ clients_serial = Clients}=State) ->
	C = maps:fold(fun(K,_V,A) ->
									[K|A]
								end, [], Clients),
	{reply,C,State};

handle_call (dump_status,_From, #state{ clients_status = Clients}=State) ->
	{reply,Clients,State};

handle_call ({get_client_state,Id},_From, #state{ clients_serial = Clients}=State) ->
	case maps:get(Id,Clients,undefined) of
		undefined -> { reply, {error,unknown_serial_number} , State };
		Pid ->
			Pid ! dump_state,
			{ reply, ok , State }
	end;

handle_call (list_ids,_From, #state{ clients_serial = Clients}=State) ->
	C = maps:fold(fun(K,_V,A) ->
		[K|A]
	              end, [], Clients),
	{reply,C,State};

handle_call (all_ready,_From, #state{ clients_status = Clients }=APS) ->
	Ready = maps:fold(fun(_K,V,A) ->
											case V == ready of
												true -> A+1;
												false-> A
											end
										end,0,Clients),
	{ reply,Ready == maps:size(Clients),APS };

handle_call (_, _, State) ->
	{reply, invalid, State}.

-spec handle_info (Msg :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info({'EXIT',Pid,_Reason}, #state{ clients_pid = Clients } = APS) ->
	case maps:get(Pid,Clients,undefined) of
		undefined ->
			{ noreply, APS};
		ID ->
			NewClientsPid = maps:remove(Pid,APS#state.clients_pid),
			NewClientStatus = maps:remove(ID,APS#state.clients_status),
			NewClientSerial = maps:remove(ID,APS#state.clients_serial),
			{ noreply , APS#state{ clients_serial = NewClientSerial, clients_status = NewClientStatus, clients_pid = NewClientsPid}}
	end;
	
handle_info(_, State) ->
	{noreply, State}.

-spec terminate (Reason :: shutdown | {shutdown, term()} | norma, State :: #state{}) -> ok.
terminate (_Reason, _State) ->
	ok.

-spec code_change (OldVersion :: term(), OldState ::#state{}, Extra :: term()) -> {ok, Extra :: term()}.
code_change (_,OldState,_) ->
	{ok, OldState}.

%%%============================================================================
%%% internal functions
%%%============================================================================
-spec add_ap_statistics(X::ap_statistics(),Y::ap_statistics())->Z::ap_statistics().
add_ap_statistics(X,Y)->
	#ap_statistics{
		start_stamp = X#ap_statistics.start_stamp + Y#ap_statistics.start_stamp,
		end_stamp = X#ap_statistics.end_stamp + Y#ap_statistics.end_stamp,
		rx_bytes = X#ap_statistics.rx_bytes + Y#ap_statistics.rx_bytes,
		tx_bytes = X#ap_statistics.tx_bytes + Y#ap_statistics.tx_bytes,
		dropped = X#ap_statistics.dropped + Y#ap_statistics.dropped,
		restarts = X#ap_statistics.restarts + Y#ap_statistics.restarts
	}.


%--------trigger_execute/2---------------trigger execute after a delay of D milliseconds (0 = immedeately)

create_aps(Cfg,ManagerPid) ->
	#{ sim_ca := CAName, clients := Clients, callback := { CallBackPid, CallBackMessage } } = Cfg,
	_ = lists:foldl(  fun(C,A) ->
									Pid = spawn_link(ovsdb_ap,start,[CAName,C,Cfg,ManagerPid]),
									ovsdb_client_handler:register(C,Pid),
									[{C,Pid}|A]
								end, [], Clients),
	ovsdb_client_handler:set_state(configured),
	?L_IA("CONFIG APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

start_aps(Which,Options,ClientPids)->
	#{ callback := { CallBackPid, CallBackMessage } } = Options,
	_ = maps:fold(fun(K,_V,A) ->
			ovsdb_ap:start_ap(K), [K|A]
		end, [], utils:select( Which == all , ClientPids , Which )),
	ovsdb_client_handler:set_state(started),
	io:format("STARTING APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

stop_aps(Which,Options,ClientPids)->
	#{ callback := { CallBackPid, CallBackMessage } } = Options,
	_ = maps:fold(fun(K,_V,A) ->
		ovsdb_ap:stop_ap(K), [K|A]
	              end, [], utils:select( Which == all , ClientPids , Which )),
	ovsdb_client_handler:set_state(stopped),
	io:format("STOP APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

pause_aps(Which,Options,ClientPids)->
	#{ callback := { CallBackPid, CallBackMessage } } = Options,
	_ = maps:fold(fun(K,_V,A) ->
		ovsdb_ap:pause_ap(K), [K|A]
	              end, [], utils:select( Which == all , ClientPids , Which )),
	ovsdb_client_handler:set_state(paused),
	?L_IA("PAUSE APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

cancel_aps(Which,Options,ClientPids)->
	#{ callback := { CallBackPid, CallBackMessage } } = Options,
	_ = maps:fold(fun(K,_V,A) ->
									ovsdb_ap:cancel_ap(K), [K|A]
	              end, [], utils:select( Which == all , ClientPids , Which )),
	ovsdb_client_handler:set_state(cancelled),
	?L_IA("CANCEL APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

%%------------------------------------------------------------------------------
%% external API
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% internal API
%%------------------------------------------------------------------------------
-spec update_statistics(State::#state{}) -> NewState::#state{}.
update_statistics(State) ->
	{Running,Paused,Reconnecting,Ready} =
	maps:fold(fun(K,V,{R,P,T,RD})->
								case V of
									running -> {R+1,P,T,RD};
									paused -> {R,P+1,T,RD};
									reconnecting -> {R,P,T+1,RD};
									ready -> {R,P,T,RD+1};
									_ ->
										io:format("~p: STATS: unknown status : ~p~n",[K,V]),
										{R,P,T,RD}
								end
	            end,{0,0,0,0},State#state.clients_status),

	LatestStats = #ovsdb_statistics_report{
		stamp = os:system_time(),
		configured = maps:size(State#state.clients_pid),
		running = Running,
		paused = Paused,
		reconnecting = Reconnecting,
		ready = Ready,
		dropped = State#state.running_stats#ap_statistics.dropped ,
		restarts = State#state.running_stats#ap_statistics.restarts,
		tx_bytes = State#state.running_stats#ap_statistics.tx_bytes,
		rx_bytes = State#state.running_stats#ap_statistics.rx_bytes,
		average_tx_rate =
				State#state.running_stats#ap_statistics.tx_bytes div (1+(State#state.running_stats#ap_statistics.start_stamp-State#state.running_stats#ap_statistics.end_stamp)),
		average_rx_rate =
				State#state.running_stats#ap_statistics.rx_bytes div (1+(State#state.running_stats#ap_statistics.start_stamp-State#state.running_stats#ap_statistics.end_stamp))
	},
	post_statistics(LatestStats),
	State#state{ running_stats = #ap_statistics{}, running_count = 0 }.

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------
-spec post_statistics(Entry::#ovsdb_statistics_report{}) -> ok.
post_statistics(StatsRecord) ->
	Fields = record_info(fields,ovsdb_statistics_report),
	[_|Values] = tuple_to_list(StatsRecord),
	Map = maps:from_list(lists:zip(Fields,Values)),
	statistics:submit_report(ovsdb_clients,maps:remove(seq,Map)).
