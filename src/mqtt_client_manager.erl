%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2020 10:36 a.m.
%%%-------------------------------------------------------------------
-module(mqtt_client_manager).
-author("stephb").

-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, creation_info/0, start_client/3, stop_client/2, is_running/2,
         get_stats/0, update_stats_daemon/0, set_ssid/3, dump_client/2, get_client_pid/2,
         get_pid_map/1, update_running_stats/2]).

-define(SERVER, ?MODULE).

-record(mqtt_client_traffic_report, {
	stamp           = 0 :: non_neg_integer(),
	connected       = 0 :: non_neg_integer(),
	reconnecting    = 0 :: non_neg_integer(),
	dropped         = 0 :: non_neg_integer(),
	restarts        = 0 :: non_neg_integer(),
	tx_bytes        = 0 :: non_neg_integer(),
	rx_bytes        = 0 :: non_neg_integer(),
	average_tx_rate = 0 :: non_neg_integer(),
	average_rx_rate = 0 :: non_neg_integer()
}).

-record(mqtt_client_manager_state, {
	client_configurations = #{} :: #{ Serial::binary() => { ClientPid::pid(),Configuration::#{} }},
	client_pids = #{} :: #{ ClientPid::pid() => Serial::binary()} ,
	connect_avg_time,
	connect_avg_time_hwm = 0.0 :: number(),
	current_connections = 0 :: integer(),
	connections_hwm = 0 :: integer(),
	errors =0 :: integer(),
	running_stats = #mqtt_stats{} :: #mqtt_stats{},
	total_stats = #mqtt_stats{} :: #mqtt_stats{},
	stats_updater :: timer:tref() } ).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
	       start => { ?MODULE , start_link, [] },
	       restart => permanent,
	       shutdown => 100,
	       type => worker,
	       modules => [?MODULE]} ].

-spec start_client(CAName::string()|binary(),Serial::string()|binary(),Configuration::gen_configuration()) -> ok | generic_error().
start_client(CAName,Serial,Configuration)->
	gen_server:call(?SERVER,{start_client,utils:safe_binary(CAName),utils:safe_binary(Serial),Configuration}).

-spec stop_client(CAName::string()|binary(),Serial::string()|binary()) -> ok | generic_error().
stop_client(CAName,Serial)->
	gen_server:call(?SERVER,{stop_client,utils:safe_binary(CAName),utils:safe_binary(Serial)}).

-spec is_running(CAName::string()|binary(),Serial::string()|binary()) -> { ok , { Pid::pid, Configuration::gen_configuration()}} | generic_error().
is_running(CAName,Serial)->
	gen_server:call(?SERVER,{is_running,utils:safe_binary(CAName),utils:safe_binary(Serial)}).

-spec get_stats()-> {ok,#{ atom() => term() }}.
get_stats()->
	gen_server:call(?SERVER,get_stats).

-spec set_ssid(CAName::string()|binary(),Serial::string()|binary(),SSID::binary())->ok.
set_ssid(CAName,Serial,SSID)->
	gen_server:cast(?SERVER,{set_ssid,utils:safe_binary(CAName),utils:safe_binary(Serial),utils:safe_binary(SSID)}).

-spec dump_client(CAName::string()|binary(),Serial::string()|binary())->ok.
dump_client(CAName,Serial)->
	gen_server:cast(?SERVER,{dump_client,utils:safe_binary(CAName),utils:safe_binary(Serial)}).

-spec get_client_pid(CAName::string()|binary(),Serial::string()|binary())->pid().
get_client_pid(CAName,Serial)->
	gen_server:call(?SERVER,{get_client_pid,utils:safe_binary(CAName),utils:safe_binary(Serial)}).

-spec get_pid_map(CAName::binary()|string()) -> {ok,#{ ClientPid::pid() => Serial::binary() }} | generic_error().
get_pid_map(CAName)->
	gen_server:call(?SERVER,{get_pid_map,CAName}).

update_stats_daemon()->
	gen_server:cast(?SERVER,update_stats_daemon).

update_running_stats(Id,Stats)->
	gen_server:cast(?SERVER,{update_running_stats,Id,Stats}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #mqtt_client_manager_state{}} | {ok, State :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok,TRef} = timer:apply_interval(2000,?MODULE,update_stats_daemon,[]),
	{ok, #mqtt_client_manager_state{
		stats_updater = TRef,
		connect_avg_time = utils:new_avg() }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #mqtt_client_manager_state{}) ->
	                 {reply, Reply :: term(), NewState :: #mqtt_client_manager_state{}} |
	                 {reply, Reply :: term(), NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #mqtt_client_manager_state{}} |
	                 {noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #mqtt_client_manager_state{}} |
	                 {stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_call({start_client,CAName,Id,Configuration}, _From, State = #mqtt_client_manager_state{}) ->
	NewState = start_client_process(CAName,Id,Configuration,State),
	{reply, ok, NewState};
handle_call({stop_client,_CAName,Serial}, _From, State = #mqtt_client_manager_state{}) ->
	case maps:get(Serial,State#mqtt_client_manager_state.client_configurations,none) of
		none ->
			{reply, ok, State};
		{Pid,_} ->
			exit(Pid,kill),
			{reply, ok, State}
	end;
handle_call({is_running,_CAName,Serial}, _From, State = #mqtt_client_manager_state{}) ->
	case maps:get(Serial,State#mqtt_client_manager_state.client_configurations,none) of
		none ->
			{reply, { error, not_running }, State};
		{_Pid,_Configuration}=Client ->
			{reply, {ok,Client} , State}
	end;
handle_call(get_stats,_From,State = #mqtt_client_manager_state{}) ->
	{reply, {ok,extract_stats(State)} , State};
handle_call({get_client_pid,_CAName,Serial},_From,State = #mqtt_client_manager_state{}) ->
	case maps:get(Serial,State#mqtt_client_manager_state.client_configurations,unknown) of
		unknown ->
			?L_IA("MQTT_CLIENT_MANAGER: attempt to get PID for device ~p failed.",[Serial]),
			{ reply, {error,client_unknown}, State};
		{Pid,_} ->
			{reply, Pid , State}
	end;
handle_call({get_pid_map,_CAName}, _From, State = #mqtt_client_manager_state{}) ->
	{reply, {ok,State#mqtt_client_manager_state.client_pids}, State};
handle_call(_Request, _From, State = #mqtt_client_manager_state{}) ->
	{reply, ok, State}.

extract_stats(State)->
	#{
		current_connections => State#mqtt_client_manager_state.current_connections,
		connections_hwm => State#mqtt_client_manager_state.connections_hwm,
		errors => State#mqtt_client_manager_state.errors,
		connection_avg_time => utils:get_avg(State#mqtt_client_manager_state.connect_avg_time),
		connection_avg_time_hwm => State#mqtt_client_manager_state.connect_avg_time_hwm
	}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mqtt_client_manager_state{}) ->
	{noreply, NewState :: #mqtt_client_manager_state{}} |
	{noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_cast({set_ssid,_CAName,Serial,SSID}, State = #mqtt_client_manager_state{}) ->
	_=case maps:get(Serial,State#mqtt_client_manager_state.client_configurations,unknown) of
		unknown ->
			?L_IA("MQTT_CLIENT_MANAGER: attempt to set SSID ~p to device ~p failed.",[SSID,Serial]);
		{Pid,_} ->
			?L_IA("MQTT_CLIENT_MANAGER: sending set SSID ~p to device ~p to pid ~p.",[SSID,Serial,Pid]),
			Pid ! {set_ssid,SSID}
	end,
	{noreply, State};
handle_cast({dump_client,_CAName,Serial}, State = #mqtt_client_manager_state{}) ->
	_=case maps:get(Serial,State#mqtt_client_manager_state.client_configurations,unknown) of
		unknown ->
			?L_IA("MQTT_CLIENT_MANAGER: attempt to show config for device ~p failed.",[Serial]);
		{Pid,_} ->
			?L_IA("MQTT_CLIENT_MANAGER: sending show config for device ~p failed to ~p.",[Serial,Pid]),
			Pid ! {dump_client,all}
	end,
	{noreply, State};
handle_cast({update_running_stats,_Id,Stats}, State = #mqtt_client_manager_state{}) ->
	NewRunningStats = add_mqtt_stats( State#mqtt_client_manager_state.running_stats, Stats ),
	NewTotalStats = add_mqtt_stats( State#mqtt_client_manager_state.total_stats, Stats ),
	{noreply, State#mqtt_client_manager_state{ running_stats = NewRunningStats, total_stats = NewTotalStats }};
handle_cast(update_stats_daemon, State = #mqtt_client_manager_state{}) ->
	ReportRecord = #mqtt_client_traffic_report{
		stamp = os:system_time(),
		connected = State#mqtt_client_manager_state.current_connections,
		tx_bytes = State#mqtt_client_manager_state.running_stats#mqtt_stats.tx_bytes,
		rx_bytes = State#mqtt_client_manager_state.running_stats#mqtt_stats.rx_bytes,
		restarts = State#mqtt_client_manager_state.running_stats#mqtt_stats.connects,
		dropped = State#mqtt_client_manager_state.running_stats#mqtt_stats.disconnects,
		average_tx_rate =
			State#mqtt_client_manager_state.running_stats#mqtt_stats.tx_bytes div (1+(State#mqtt_client_manager_state.running_stats#mqtt_stats.start_stamp-State#mqtt_client_manager_state.running_stats#mqtt_stats.end_stamp)),
		average_rx_rate =
			State#mqtt_client_manager_state.running_stats#mqtt_stats.rx_bytes div (1+(State#mqtt_client_manager_state.running_stats#mqtt_stats.start_stamp-State#mqtt_client_manager_state.running_stats#mqtt_stats.end_stamp))
		},
	Report = prepare_statistics_report(ReportRecord),
	statistics:submit_report(mqtt_client_stats,maps:remove(seq,Report)),
	statistics:submit_report(mqtt_client_handler, extract_stats(State)),
	{noreply, State#mqtt_client_manager_state{ running_stats = #mqtt_stats{} }};
handle_cast(_Request, State = #mqtt_client_manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mqtt_client_manager_state{}) ->
	{noreply, NewState :: #mqtt_client_manager_state{}} |
	{noreply, NewState :: #mqtt_client_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #mqtt_client_manager_state{}}).
handle_info({'DOWN', _Ref, process, Pid, _Why},State)->
	case maps:get(Pid,State#mqtt_client_manager_state.client_pids,none) of
		none ->
			{noreply,State};
		Id ->
			NewState = State#mqtt_client_manager_state{
				client_configurations = maps:remove(Id,State#mqtt_client_manager_state.client_configurations),
				client_pids = maps:remove(Pid,State#mqtt_client_manager_state.client_pids ) },
			{noreply,NewState}
	end;
handle_info({stats,Type,Value}=_Info, State = #mqtt_client_manager_state{}) ->
	%% io:format("MQTT_CLIENT_MANAGER: processing message: ~p~n",[Info]),
	State1 = case Type of
		connection ->
			CurConns = State#mqtt_client_manager_state.current_connections + Value,
			State#mqtt_client_manager_state{ current_connections = CurConns, connections_hwm = max(CurConns,State#mqtt_client_manager_state.connections_hwm) };
		error ->
			State#mqtt_client_manager_state{ errors = State#mqtt_client_manager_state.errors+1};
	  connect_time ->
		  NewAvg = utils:compute_avg(Value,State#mqtt_client_manager_state.connect_avg_time),
		  NewAvgHWM = max(State#mqtt_client_manager_state.connect_avg_time_hwm,utils:get_avg(NewAvg)),
		  State#mqtt_client_manager_state{ connect_avg_time= NewAvg, connect_avg_time_hwm = NewAvgHWM };
	  _ ->
		  io:format(">>>>MQTTCLIENTMANAGER: unknow stats type = ~p~n",[Type]),
		  State
	end,
%%	io:format("MQTT Clients: ~p Concurrent: ~p~n",[maps:size(State#mqtt_client_manager_state.client_pids),State1#mqtt_client_manager_state.current_connections]),
	{noreply, State1};

handle_info(Info, State = #mqtt_client_manager_state{}) ->
	io:format("MQTT_CLIENT_MANAGER: unprocessed message: ~p~n",[Info]),
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #mqtt_client_manager_state{}) -> term()).
terminate(_Reason, State = #mqtt_client_manager_state{}) ->
	_ = timer:cancel(State#mqtt_client_manager_state.stats_updater),
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mqtt_client_manager_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #mqtt_client_manager_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mqtt_client_manager_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_client_process(CAName::binary(),Serial::binary(),Configuration::map(),State::#mqtt_client_manager_state{}) -> NewState::#mqtt_client_manager_state{}.
start_client_process(CAName,Serial,Configuration,State)->
	?L_IA("MQTT-Client ~p starting.",[Serial]),
	Pid = spawn_link(mqtt_client,start,[CAName,Serial,Configuration,self()]),
	NewState = State#mqtt_client_manager_state{
		client_configurations = maps:put(Serial,{ Pid,Configuration} ,State#mqtt_client_manager_state.client_configurations),
		client_pids = maps:put(Pid,Serial,State#mqtt_client_manager_state.client_pids )
	},
	% io:format("MQTT-Client ~p starting at pid ~p. Already ~p running.~n",[Serial,Pid,maps:size(NewState#mqtt_client_manager_state.client_pids)]),
	NewState.

-spec add_mqtt_stats(X::mqtt_stats(),Y::mqtt_stats())->Z::mqtt_stats().
add_mqtt_stats(X,Y)->
	#mqtt_stats{
		start_stamp   = X#mqtt_stats.start_stamp + Y#mqtt_stats.start_stamp,
		end_stamp     = X#mqtt_stats.end_stamp + Y#mqtt_stats.end_stamp,
		rx_bytes      = X#mqtt_stats.rx_bytes + Y#mqtt_stats.rx_bytes,
		tx_bytes      = X#mqtt_stats.tx_bytes + Y#mqtt_stats.tx_bytes,
		disconnects   = X#mqtt_stats.disconnects + Y#mqtt_stats.disconnects,
		connects      = X#mqtt_stats.connects + Y#mqtt_stats.connects,
		errors        = X#mqtt_stats.start_stamp + Y#mqtt_stats.errors
	}.

prepare_statistics_report(StatsRecord) ->
	Fields = record_info(fields,mqtt_client_traffic_report),
	[_|Values] = tuple_to_list(StatsRecord),
	maps:from_list(lists:zip(Fields,Values)).



