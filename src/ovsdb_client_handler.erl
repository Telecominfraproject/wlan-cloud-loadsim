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
-record (hdl_state, {
	clients_serial = #{}  :: #{ Serial::binary() => Pid::pid() },
	clients_pid = #{}     :: #{ Pid::pid() => Serial::binary() },
	clients_status = #{}  :: ovsdb_client_status_map(),
	config = #{}          :: #{atom()=>term()},
	ap_statistics = []    :: [ovsdb_ap_statistics()],
	timer :: owls_timers:tms(),
	update_stats_timer = none :: none | timer:tref() ,
	state_num = 0 :: non_neg_integer(),
	state = undefined :: atom()
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
	gen_server:cast(?SERVER,{ap_stats,Stats,Id}).

%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================
-spec start_link () -> {ok, Pid :: pid()} | generic_error().
start_link () ->
	gen_server:start_link({local, ?SERVER},?MODULE, [], []).

-spec init (Args :: term()) -> {ok, State :: #hdl_state{}} | {stop, Reason :: term()}.
init (_) ->
	process_flag(trap_exit, true),
	ovsdb_client_stats:prepare_statistics(),
	{ok, #hdl_state{timer=owls_timers:new(millisecond),
	                clients_pid = #{} ,
	                clients_serial = #{},
	                state = init ,
	                clients_status = #{},
									update_stats_timer = none}}.

-spec handle_cast (Request :: term(), State :: #hdl_state{}) -> {noreply, NewState :: #hdl_state{}} | {stop, Reason :: term(), NewState :: #hdl_state{}}.
handle_cast ({status,Status,Id}, State) ->
	NewClientStatus = maps:put(Id,Status,State#hdl_state.clients_status),
	{noreply, State#hdl_state{ clients_status = NewClientStatus }};
handle_cast ({ap_stats,NewStats,_Id},#hdl_state{ap_statistics=ApStats}=State) ->
	{noreply, State#hdl_state{ap_statistics=[NewStats|ApStats]}};
handle_cast ({set_state,NewState},#hdl_state{}=State) ->
	case NewState of
		cancelled ->
			utils:select( State#hdl_state.update_stats_timer == none, ok, timer:cancel(State#hdl_state.update_stats_timer) ),
			{ noreply, State#hdl_state{ state = init, config = #{}, clients_pid = #{}, clients_serial = #{}, update_stats_timer = none }};
		started ->
			{ok,TRef} = timer:apply_interval(?MGR_REPORT_INTERVAL,?MODULE,update_stats,[]),
			{ noreply, State#hdl_state{ update_stats_timer = TRef }};
		paused->
			utils:select( State#hdl_state.update_stats_timer == none, ok, timer:cancel(State#hdl_state.update_stats_timer) ),
			{noreply, State#hdl_state{state=NewState,update_stats_timer = none}};
  	stopped->
		  utils:select( State#hdl_state.update_stats_timer == none, ok, timer:cancel(State#hdl_state.update_stats_timer) ),
		  {noreply, State#hdl_state{state=NewState,update_stats_timer = none}};
		configured ->
			{noreply, State#hdl_state{state=NewState}};
		_ ->
			io:format("OVSDB_HANDLER: unknown state: ~p~n",[NewState]),
			{noreply, State#hdl_state{state=NewState}}
end;

handle_cast ({register,Id,Pid},#hdl_state{}=State) ->
	NewClientPids = maps:put( Pid, Id, State#hdl_state.clients_pid),
	NewClientSerials = maps:put( Id, Pid, State#hdl_state.clients_serial),
	{noreply, State#hdl_state{ clients_pid = NewClientPids, clients_serial = NewClientSerials }};

handle_cast ({set_ap_status,Status,Id},#hdl_state{}=State) ->
	NewClientStatus = maps:put( Id, Status, State#hdl_state.clients_status),
	{noreply, State#hdl_state{ clients_status = NewClientStatus }};

handle_cast (_,State) ->
	{noreply, State}.

-spec handle_call (Request :: term(), From :: {pid(),Tag::term()}, State :: #hdl_state{}) -> {reply, Reply :: term(), NewState :: #hdl_state{}} | {stop, Reason :: term(), Reply :: term(), NewState :: #hdl_state{}}.
handle_call ({set_config, Cfg},_From,#hdl_state{ state = init }=State) ->
	spawn_link(?MODULE,create_aps,[Cfg,self()]),
	{reply, ok, State#hdl_state{ config = Cfg }};

handle_call ({api_cmd_start,Which,Options},_From, State) ->
	case State#hdl_state.state of
		configured ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = starting }};
		paused ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = starting }};
		stopped ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = starting }};
		started ->
			{reply, ok, State};
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_stop, Which, Options},_,State) ->
	case State#hdl_state.state of
		configured ->
			spawn_link(?MODULE,stop_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = stopping }};
		paused ->
			spawn_link(?MODULE,stop_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = stopping }};
		stopped ->
			{reply, ok, State};
		started ->
			spawn_link(?MODULE,stop_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = stopping }};
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_pause, Which, Options},_,State) ->
	case State#hdl_state.state of
		configured ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = pausing }};
		paused ->
			{reply, ok, State};
		stopped ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = pausing }};
		started ->
			spawn_link(?MODULE,pause_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = pausing }};
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_resume, Which, Options},_,State) ->
	case State#hdl_state.state of
		paused ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = starting }};
		stopped ->
			spawn_link(?MODULE,start_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = starting }};
		started ->
			{reply, ok, State };
		cancelled ->
			{reply, {error,simulation_most_receive_new_configuration},State};
		_ ->
			{reply, ok, State}
	end;

handle_call ({api_cmd_cancel, Which, Options},_,State) ->
	case State#hdl_state.state of
		configured ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = cancelling }};
		paused ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = cancelling }};
		stopped ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = cancelling }};
		started ->
			spawn_link(?MODULE,cancel_aps,[Which,Options,State#hdl_state.clients_pid]),
			{reply, ok, State#hdl_state{ state = cancelling }};
		cancelled ->
			{reply, ok,State};
		_ ->
			{reply, ok, State}
	end;

handle_call (update_stats, _From, #hdl_state{ clients_status = ClientStatuses , ap_statistics = Statistics }=State) ->
	ovsdb_client_stats:update_statistics(ClientStatuses,Statistics),
	{reply, ok, State#hdl_state{ap_statistics=[]}};

handle_call (dump_clients,_From, #hdl_state{ clients_serial = Clients}=State) ->
	C = maps:fold(fun(K,_V,A) ->
									[K|A]
								end, [], Clients),
	{reply,C,State};

handle_call (dump_status,_From, #hdl_state{ clients_status = Clients}=State) ->
	{reply,Clients,State};

handle_call ({get_client_state,Id},_From, #hdl_state{ clients_serial = Clients}=State) ->
	case maps:get(Id,Clients,undefined) of
		undefined -> { reply, {error,unknown_serial_number} , State };
		Pid ->
			Pid ! dump_state,
			{ reply, ok , State }
	end;

handle_call (list_ids,_From, #hdl_state{ clients_serial = Clients}=State) ->
	C = maps:fold(fun(K,_V,A) ->
		[K|A]
	              end, [], Clients),
	{reply,C,State};

handle_call (all_ready,_From, #hdl_state{ clients_status = Clients }=APS) ->
	Ready = maps:fold(fun(_K,V,A) ->
											case V == ready of
												true -> A+1;
												false-> A
											end
										end,0,Clients),
	{ reply,Ready == maps:size(Clients),APS };

handle_call (_, _, State) ->
	{reply, invalid, State}.

-spec handle_info (Msg :: term(), State :: #hdl_state{}) -> {noreply, NewState :: #hdl_state{}}.
handle_info({'EXIT',Pid,_Reason}, #hdl_state{ clients_pid = Clients } = APS) ->
	case maps:get(Pid,Clients,undefined) of
		undefined ->
			{ noreply, APS};
		ID ->
			NewClientsPid = maps:remove(Pid,APS#hdl_state.clients_pid),
			NewClientStatus = maps:remove(ID,APS#hdl_state.clients_status),
			NewClientSerial = maps:remove(ID,APS#hdl_state.clients_serial),
			{ noreply , APS#hdl_state{ clients_serial = NewClientSerial, clients_status = NewClientStatus, clients_pid = NewClientsPid}}
	end;
	
handle_info(_, State) ->
	{noreply, State}.

-spec terminate (Reason :: shutdown | {shutdown, term()} | norma, State :: #hdl_state{}) -> ok.
terminate (_Reason, _State) ->
	ovsdb_client_stats:close().

-spec code_change (OldVersion :: term(), OldState ::#hdl_state{}, Extra :: term()) -> {ok, Extra :: term()}.
code_change (_,OldState,_) ->
	{ok, OldState}.

%%%============================================================================
%%% internal functions
%%%============================================================================

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
	?L_IA("STARTING APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
	CallBackPid ! CallBackMessage.

stop_aps(Which,Options,ClientPids)->
	#{ callback := { CallBackPid, CallBackMessage } } = Options,
	_ = maps:fold(fun(K,_V,A) ->
		ovsdb_ap:stop_ap(K), [K|A]
	              end, [], utils:select( Which == all , ClientPids , Which )),
	ovsdb_client_handler:set_state(stopped),
	?L_IA("STOP APS: Sending message back: ~p ~p~n",[CallBackPid, CallBackMessage]),
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
