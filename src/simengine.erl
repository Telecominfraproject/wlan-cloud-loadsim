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
-export([start_link/0,creation_info/0,create/1,create_tables/0,get/1,list/0,run_batch/4,
         prepare/3,start/3,stop/3,cancel/3,pause/3,restart/3,push/3,
         sim_exists/1,prepare_assets/4,push_assets/4,start_assets/4,stop_assets/4,cancel_assets/4,
				 pause_assets/4,restarts_assets/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(sim_state,{
	pushed = false :: boolean(),
	current_op_pid = none :: none | pid(),
	current_op = none :: none | preparing | pushing | starting | pausing | stopping | restarting | cancelling ,
	state = created :: created | prepared | pushed | started | paused | stopped | restarted | cancelled ,
	start = none :: none | erlang:timestamp(),
	current_cb = none :: none | notification_cb(),
	outstanding_nodes = [] :: [node()],
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

-spec prepare(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
prepare(SimName,Attributes,{M,F,A}=Notification) when is_list(SimName), is_atom(M), is_atom(F), is_list(A) ->
	gen_server:call(?SERVER,{prepare,list_to_binary(SimName),Attributes,Notification}).

-spec push(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
push(SimName,Attributes,Notification)->
	gen_server:call(?SERVER,{push,utils:safe_binary(SimName),Attributes,Notification}).

-spec start(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
start(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{start,utils:safe_binary(SimName),Attributes,Notification}).

-spec stop(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
stop(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{stop,utils:safe_binary(SimName),Attributes,Notification}).

-spec pause(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
pause(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{pause,utils:safe_binary(SimName),Attributes,Notification}).

-spec cancel(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
cancel(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{cancel,utils:safe_binary(SimName),Attributes,Notification}).

-spec restart(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> ok | generic_error().
restart(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{restart,utils:safe_binary(SimName),Attributes,Notification}).

-spec list() -> {ok,[string()]} | generic_error().
list() ->
	gen_server:call(?SERVER,list_simulations).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link(?START_SERVER, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #simengine_state{}} | {ok, State :: #simengine_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	_ = mnesia:wait_for_tables([simulations],20000),
	Simulations = list_sims_full(),
	NewStates = lists:foldl( fun(E,A)->
			maps:put( E#simulation.name, #sim_state{
				pushed = false,
				state = utils:select(E#simulation.assets_created,prepared,created),
				sim_info = E },A )
		end,maps:new(),Simulations),
	{ok, #simengine_state{sim_states = NewStates}}.

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
					SimState = #sim_state{ sim_info = SimInfo },
					{reply, ok, State#simengine_state{ sim_states = maps:put(SimInfo#simulation.name,SimState,State#simengine_state.sim_states )}};
				Error -> { reply, {error,Error} , State}
			end
	end;
handle_call({prepare,SimName,Attributes,Notification},_From,State = #simengine_state{}) ->
	case get_sim(SimName) of
		[]->
			{reply,?ERROR_SIM_UNKNOWN,State};
		[SimInfo] ->
			case SimInfo#simulation.assets_created of
				true ->
					{ok,?ERROR_SIM_ASSETS_ALREADY_CREATED,State};
				false->
					S = maps:get(SimName,State#simengine_state.sim_states),
					case length(S#sim_state.outstanding_nodes)>0 of
						true->
							{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
						false->
							OpPid=spawn_link(?MODULE,prepare_assets,[SimInfo,Attributes,self(),Notification]),
							{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,
							                                                       S#sim_state{current_op_pid = OpPid, current_op = preparing , outstanding_nodes = [node()] },
							                                                       State#simengine_state.sim_states) }}
					end
			end
	end;

handle_call({push,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case (SimInfo#simulation.assets_created) of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_ALREADY_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									OpPid=spawn_link(?MODULE,push_assets,[SimInfo,Attributes,self(),Callback]),
									{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = pushing , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }}
							end
					end
			end
	end;

handle_call({start,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case S#sim_state.state of
										started->
											{ reply, ?ERROR_SIM_ALREADY_STARTED };
										_ ->
											OpPid=spawn_link(?MODULE,start_assets,[SimInfo,Attributes,self(),Callback]),
											{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = starting , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }}
									end
							end
					end
			end
	end;

handle_call({stop,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==started) or (S#sim_state.state==paused))  of
										true->
											OpPid=spawn_link(?MODULE,stop_assets,[SimInfo,Attributes,self(),Callback]),
											{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = stopping , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED_OR_PAUSED }
									end
							end
					end
			end
	end;

handle_call({pause,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case (S#sim_state.state==started) of
										true->
											OpPid=spawn_link(?MODULE,pause_assets,[SimInfo,Attributes,self(),Callback]),
											{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = pausing , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED }
									end
							end
					end
			end
	end;

handle_call({cancel,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==started) or (S#sim_state.state==paused) or (S#sim_state.state==stopped)) of
										true->
											OpPid=spawn_link(?MODULE,cancel_assets,[SimInfo,Attributes,self(),Callback]),
											{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = cancelling , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED_OR_PAUSED_OR_STOPPED }
									end
							end
					end
			end
	end;

handle_call({restart,SimName,Attributes,Callback}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		[] ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		[SimInfo] ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==paused) or (S#sim_state.state==stopped)) of
										true->
											OpPid=spawn_link(?MODULE,restart_assets,[SimInfo,Attributes,self(),Callback]),
											{reply,ok,State#simengine_state{ sim_states = maps:put(SimName,S#sim_state{current_op_pid = OpPid, current_op = restarting , outstanding_nodes = SimInfo#simulation.nodes },State#simengine_state.sim_states) }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_PAUSED_OR_STOPPED }
									end
							end
					end
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

handle_info({ SimName,Node,MsgType,TimeStamp}=_Msg, State = #simengine_state{}) ->
	NS = try
		%% io:format("INFO MESSAGE RECEIVED: ~p~n",[Msg]),
		SimState = maps:get(SimName,State#simengine_state.sim_states,undefined),
		NewNodes = lists:delete(Node,SimState#sim_state.outstanding_nodes),
		Now = erlang:timestamp(),
		Elapsed = timer:now_diff(Now,TimeStamp) / 1000000,
		%% io:format("NewNodes: ~p SimState: ~p Elapsed: ~p~n",[NewNodes,SimState,Elapsed]),
		NewSimState = case MsgType of
			prepare_done->
				io:format("Node ~p prepared. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes, state = prepared };
			push_done ->
				io:format("Node ~p push done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes , pushed = true , state = pushed };
			start_done ->
				io:format("Node ~p start done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes, state = started };
			stop_done ->
				io:format("Node ~p stop done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes , state = stopped };
			pause_done ->
				io:format("Node ~p pause done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes, state = paused };
			cancel_done ->
				io:format("Node ~p cancel done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes, state = cancelled };
			restart_done ->
				io:format("Node ~p restart done. Took ~p seconds.~n",[Node,Elapsed]),
				SimState#sim_state{ outstanding_nodes = NewNodes, state = started }
		end,
		State#simengine_state{ sim_states = maps:put(SimName,NewSimState,State#simengine_state.sim_states)}
	catch
		_:_ = Error ->
			io:format("Failed ~p processing INFO MESSAGE~n",[Error]),
			State
	end,
	{noreply,NS};

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

handle_info(Info, State = #simengine_state{}) ->
	io:format("SIMENGINE MESSAGE: ~p~n",[Info]),
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

list_sims_full()->
	{atomic,Result} = mnesia:transaction( fun()->
		mnesia:foldr( fun(E,A)->
			[ E | A ]
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

-spec prepare_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
prepare_assets(SimInfo,_Attributes,SimEnginePid,{M,F,A}=_Notification)->
	StartedAt = erlang:timestamp(),
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	split_build_clients(SimInfo,utils:noop_mfa()),
	split_build_servers(SimInfo,utils:noop_mfa()),
	set_assets_created(SimInfo,true),
	SimEnginePid ! {SimInfo#simulation.name,node(),prepare_done,StartedAt},
	erlang:apply(M,F,A),
	ok.

-spec push_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
push_assets(SimInfo,_Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	%% devise how many batches
	{ok,Clients} = inventory:list_clients(SimInfo#simulation.ca),
	Splits = utils:split_into( SimInfo#simulation.nodes, Clients),
	Results = lists:reverse(lists:foldl(fun({N,C},Acc) ->
													Config = #{ sim_name => SimInfo#simulation.name,
													            sim_ca => SimInfo#simulation.ca,
													            clients => C,
																			ovsdb_server_name => SimInfo#simulation.servers#sim_entry.opensync_server_name,
																			ovsdb_server_port => SimInfo#simulation.servers#sim_entry.opensync_server_port,
																			callback => { SimEnginePid, {SimInfo#simulation.name, N,push_done,erlang:timestamp()} }},
													io:format("Pushing to ~p...~n",[N]),
													R = rpc:call(N,simnode,set_configuration,[Config]),
													[R|Acc]
												end,[],Splits)),
	io:format("Results of push: ~p.~n",[Results]),
	erlang:apply(M,F,A),
	ok.

-spec start_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
start_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,start,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,start_done,erlang:timestamp()}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec stop_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
stop_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Stopping all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,stop,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,start_done,erlang:timestamp()}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec pause_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
pause_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Pausing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,pause,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,pause_done,erlang:timestamp()}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec cancel_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
cancel_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Cancelling all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,cancel,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,cancel_done,erlang:timestamp()}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec restarts_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb())->ok.
restarts_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification)->
	?L_IA("~s: Restarting all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,restart,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,restart_done,erlang:timestamp()}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec split_build_clients( SimInfo::simulation(), NotificationCB::notification_cb())->ok.
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
	_ = generate_server(SimInfo,mqtt_server,SimInfo#simulation.servers),
	_ = generate_server(SimInfo,ovsdb_server,SimInfo#simulation.servers),
	ok.

generate_server(SimInfo,mqtt_server,auto)->
	inventory:make_server(SimInfo#simulation.ca,"mqtt-1",mqtt_server);
generate_server(SimInfo,ovsdb_server,auto)->
	inventory:make_server(SimInfo#simulation.ca,"ovsdb-1",ovsdb_server);
generate_server(_,_,_)->
	ok.




