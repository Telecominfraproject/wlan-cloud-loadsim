%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2020 10:19 p.m.
%%%-------------------------------------------------------------------
-module(simengine).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/errors.hrl").
-include("../include/simengine.hrl").
-include("../include/inventory.hrl").

-compile([{parse_transform, rec2json}]).

%% API
-export([start_link/0,creation_info/0,create/1,create_tables/0,get/1,list/0,
         prepare/2,prepare_assets/2,run_batch/4,start/2,stop/2,cancel/2,pause/2,restart/2,
         push/2,sim_exists/1,
         push_assets/2,stop_assets/2,cancel_assets/2,pause_assets/2,restarts_assets/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(sim_state,{
	created = false :: boolean(),
	prepared = false :: boolean(),
	current_op_pid = none :: none | pid(),
	current_op = none :: none | preparing | pushing | starting | pausing | stopping | restarting | cancelling ,
	state = created :: created | prepared | pushed | started | paused | stopped | restarted | cancelled ,
	start = none :: none | erlang:timestamp(),
	current_cb = none :: none | notification_cb(),
	sim_info :: simulation()
	}).

-record(simengine_state, {
	sim_states = #{} :: #{ SimName::binary() => #sim_state{} }
}).

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

-spec create(SimInfo::simulation())-> ok | generic_error().
create(SimInfo) when is_record(SimInfo,simulation) ->
	gen_server:call(?SERVER,{create_simulation,SimInfo}).

-spec get(SimName::string()|binary()) -> {ok,simulation()} | generic_error().
get(SimName)->
	gen_server:call(?SERVER,{get,utils:safe_binary(SimName)}).

-spec push(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
push(SimName,Notification)->
	gen_server:call(?SERVER,{push,utils:safe_binary(SimName),Notification}).

-spec start(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
start(SimName,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{start,utils:safe_binary(SimName),Notification}).

-spec stop(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
stop(SimName,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{stop,utils:safe_binary(SimName),Notification}).

-spec pause(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
pause(SimName,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{pause,utils:safe_binary(SimName),Notification}).

-spec cancel(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
cancel(SimName,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{cancel,utils:safe_binary(SimName),Notification}).

-spec restart(SimName::string()|binary(), Notification::notification_cb() )-> ok | generic_error().
restart(SimName,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{restart,utils:safe_binary(SimName),Notification}).

-spec list() -> {ok,[string()]} | generic_error().
list() ->
	gen_server:call(?SERVER,list_simulations).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link(?START_SERVER, ?MODULE, [], []).

-spec prepare(SimName::string(), NotificationCB::notification_cb())-> ok.
prepare(SimName,{M,F,A}=Notification) when is_list(SimName), is_atom(M), is_atom(F), is_list(A) ->
	gen_server:call(?SERVER,{prepare,list_to_binary(SimName),Notification}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #simengine_state{}} | {ok, State :: #simengine_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #simengine_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #simengine_state{}) ->
	                 {reply, Reply :: term(), NewState :: #simengine_state{}} |
	                 {reply, Reply :: term(), NewState :: #simengine_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #simengine_state{}} |
	                 {noreply, NewState :: #simengine_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #simengine_state{}} |
	                 {stop, Reason :: term(), NewState :: #simengine_state{}}).

handle_call({create_simulation,SimInfo}, _From, State = #simengine_state{}) ->
	case sim_exists(SimInfo#simulation.name) of
		true ->
			{reply,?ERROR_SIM_ALREADY_EXISTS};
		false->
			?L_IA("Creating simulation ~s.",[binary_to_list(SimInfo#simulation.name)]),
			case create_sim(SimInfo) of
				ok ->
					SimState = #sim_state{ sim_info = SimInfo, created = true },
					{reply, ok, State#simengine_state{ sim_states = maps:put(SimInfo#simulation.name,SimState,State#simengine_state.sim_states )}};
				Error -> { reply, {error,Error} , State}
			end
	end;
handle_call({prepare,SimName,Notification},_From,State = #simengine_state{}) ->
	case get_sim(SimName) of
		[]->
			{reply,?ERROR_SIM_UNKNOWN,State};
		[SimInfo] ->
			case SimInfo#simulation.assets_created of
				true ->
					{ok,?ERROR_SIM_ASSETS_ALREADY_CREATED,State};
				false->
					S = maps:get(SimName,State#simengine_state.sim_states),
					case is_pid(S#sim_state.current_op_pid) of
						true->
							{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
						false->
							OpPid=spawn_link(?MODULE,prepare_assets,[SimInfo,Notification]),
							{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = preparing },State#simengine_state.sim_states) }}
					end
			end
	end;
handle_call({push,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,push_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = pushing },State#simengine_state.sim_states) }}
			end
	end;
handle_call({start,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,start_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = starting },State#simengine_state.sim_states) }}
			end
	end;
handle_call({stop,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,stop_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = stopping },State#simengine_state.sim_states) }}
			end
	end;
handle_call({pause,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,pause_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = pausing },State#simengine_state.sim_states) }}
			end
	end;
handle_call({cancel,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,cancel_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = cancelling },State#simengine_state.sim_states) }}
			end
	end;
handle_call({restart,SimName,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case is_pid(S#sim_state.current_op_pid) of
				true->
					{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
				false->
					OpPid=spawn_link(?MODULE,restart_assets,[SimInfo,Callback]),
					{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = restarting },State#simengine_state.sim_states) }}
			end
	end;
handle_call({get,SimName}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			{reply, {ok,SimInfo}, State}
	end;
handle_call(list_simulations, _From, State = #simengine_state{}) ->
	{ reply, {ok, list_sims()}, State };
handle_call(_Request, _From, State = #simengine_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #simengine_state{}) ->
	{noreply, NewState :: #simengine_state{}} |
	{noreply, NewState :: #simengine_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #simengine_state{}}).
handle_cast(_Request, State = #simengine_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #simengine_state{}) ->
	{noreply, NewState :: #simengine_state{}} |
	{noreply, NewState :: #simengine_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #simengine_state{}}).
handle_info({'DOWN', _Ref, process, Pid, _Why}, State = #simengine_state{}) ->
	N1 = maps:fold(fun(K,E,A)->
		case E#sim_state.current_op_pid==Pid of
			true ->
				NewState = case E#sim_state.current_op of
										 preparing -> prepared;
					           pushing -> pushed;
										 starting -> started;
					           stopping -> stopped;
										 pausing -> paused;
										 cancelling -> cancelled
				           end,
				maps:put(K,E#sim_state{current_op_pid = none, current_op = none , state = NewState },A);
			false->
				maps:put(K,E,A)
		end end,maps:new(),State#simengine_state.sim_states),
	{noreply, State#simengine_state{ sim_states = N1 }};

handle_info(_Info, State = #simengine_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #simengine_state{}) -> term()).
terminate(_Reason, _State = #simengine_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #simengine_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #simengine_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #simengine_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_tables()->
	{atomic,ok} = mnesia:create_table(simulations,[{disc_copies,[node()]}, {record_name,simulation}, {attributes,record_info(fields,simulation)}]),
	ok.

sim_exists(SimName)->
	case get_sim(SimName) of
		[] -> false;
		_ -> true
	end.

create_sim(SimInfo) when is_record(SimInfo,simulation) ->
	{atomic,Result}=mnesia:transaction(fun() ->
			mnesia:dirty_write( simulations, SimInfo )
		end),
	Result.

get_sim(SimName) ->
	{atomic,Result} = mnesia:transaction( fun() ->
		mnesia:read(simulations,SimName)
											end),
	Result.

list_sims()->
	{atomic,Result} = mnesia:transaction( fun()->
																					mnesia:foldr( fun(E,A)->
																						[ binary_to_list(E#simulation.name) | A ]
																												end, [], simulations)
																				end),
	Result.

-spec set_assets_created(SimInfo::simulation(), Value::boolean())->ok.
set_assets_created(SimInfo,Value)->
	_=mnesia:transaction( fun()->
												[Sim]=mnesia:read(simulations,SimInfo#simulation.name),
												_=mnesia:dirty_write(simulations,Sim#simulation{ assets_created = Value})
											end),
	ok.

-spec prepare_assets(SimInfo::simulation(), Notification::notification_cb())->ok.
prepare_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	split_build_clients(SimInfo,utils:noop_mfa()),
	split_build_servers(SimInfo,utils:noop_mfa()),
	set_assets_created(SimInfo,true),
	apply(M,F,A),
	ok.

-spec push_assets(SimName::binary(), Notification::notification_cb())->ok.
push_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	apply(M,F,A),
	ok.

-spec stop_assets(SimName::binary(), Notification::notification_cb())->ok.
stop_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Stopping all assets.",[binary_to_list(SimInfo#simulation.name)]),
	apply(M,F,A),
	ok.

-spec pause_assets(SimName::binary(), Notification::notification_cb())->ok.
pause_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Pausing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	apply(M,F,A),
	ok.

-spec cancel_assets(SimName::binary(), Notification::notification_cb())->ok.
cancel_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Cancelling all assets.",[binary_to_list(SimInfo#simulation.name)]),
	apply(M,F,A),
	ok.

-spec restarts_assets(SimName::binary(), Notification::notification_cb())->ok.
restarts_assets(SimInfo,{M,F,A}=_Notification)->
	?L_IA("~s: Restarting all assets.",[binary_to_list(SimInfo#simulation.name)]),
	apply(M,F,A),
	ok.

-spec split_build_clients( Sim::simulation(), NotificationCB::notification_cb())->ok.
split_build_clients(SimInfo,_Notification)->
	{ok,HardwareDefinitions} = hardware:get_definitions(),
	Ids = [ X#hardware_info.id || X <- HardwareDefinitions ],
	BatchSize = SimInfo#simulation.num_devices div length(Ids),
	_=run_batch(Ids,BatchSize,SimInfo),
	ok.

run_batch(Ids,BatchSize,SimInfo)->
	run_batch(Ids,BatchSize,SimInfo,1).

run_batch([],_,SimInfo,_)->
	io:format("~n~s: done creating ~p clients.~n",[binary_to_list(SimInfo#simulation.name),SimInfo#simulation.num_devices]),
	ok;
run_batch([H|T],BatchSize,SimInfo,BatchNumber)->
	io:format("~n~s: creating ~p of ~p clients.~n",[binary_to_list(SimInfo#simulation.name),BatchSize,SimInfo#simulation.num_devices]),
	BatchName = binary:list_to_bin([SimInfo#simulation.name,<<"-">>,integer_to_binary(BatchNumber)]),
	Attributes = #{ id => H, name => BatchName, serial => H, mac => <<>> },
	_ = inventory:make_clients(SimInfo#simulation.ca,
	                       1,
	                       min(BatchSize,SimInfo#simulation.num_devices - (BatchSize * (BatchNumber-1))),
													Attributes,
                         {?MODULE,run_batch,[T,BatchSize,SimInfo,BatchNumber+1]}).

%% Create the servers - only if they are pon automatic mode
split_build_servers(SimInfo,_Notification)->
	_ = generate_server(SimInfo,mqtt_server,SimInfo#simulation.mqtt_servers),
	_ = generate_server(SimInfo,ovsdb_server,SimInfo#simulation.ovsdb_servers),
	ok.

generate_server(SimInfo,mqtt_server,auto)->
	inventory:make_server(SimInfo#simulation.ca,"mqtt-1",mqtt_server);
generate_server(SimInfo,ovsdb_server,auto)->
	inventory:make_server(SimInfo#simulation.ca,"ovsdb-1",ovsdb_server);
generate_server(_,_,_)->
	ok.




