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
-export([start_link/0,creation_info/0,create/1,get/1,list_simulations/0,run_batch/5,
         prepare/3,start/3,stop/3,cancel/3,pause/3,restart/3,push/3,
         prepare_assets/5,push_assets/5,start_assets/5,stop_assets/5,cancel_assets/5,
				 pause_assets/5,restarts_assets/5,update/1,list_actions/0,get_action/1,
				 sim_action_to_json/1,delete/2,list_simulation_states/0,get_simulation_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(simengine_state, {
			sim_states = #{} :: #{ SimName::binary() => #sim_state{}},
			sim_actions = #{} :: #{ ID::binary() => #sim_action{}}
}).

-type sim_operation_res() :: { ok , JobId:: binary() }.
-type sim_state() :: #sim_state{}.

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

-spec delete(SimName::binary()|string(),CAName::binary()|string())-> ok | generic_error().
delete(SimName,CAName)->
	gen_server:call(?SERVER,{delete_simulation,utils:safe_binary(SimName),utils:safe_binary(CAName)}).

-spec update(SimInfo::simulation())-> ok | generic_error().
update(SimInfo) when is_record(SimInfo,simulation) ->
	gen_server:call(?SERVER,{update_simulation,SimInfo}).

-spec get(SimName::string()|binary()) -> {ok,simulation()} | generic_error().
get(SimName)->
	gen_server:call(?SERVER,{get,utils:safe_binary(SimName)}).

-spec prepare(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
prepare(SimName,Attributes,Notification) ->
	gen_server:call(?SERVER,{prepare,utils:safe_binary(SimName),Attributes,Notification}).

-spec push(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
push(SimName,Attributes,Notification)->
	gen_server:call(?SERVER,{push,utils:safe_binary(SimName),Attributes,Notification}).

-spec start(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
start(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{start,utils:safe_binary(SimName),Attributes,Notification}).

-spec stop(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
stop(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{stop,utils:safe_binary(SimName),Attributes,Notification}).

-spec pause(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
pause(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{pause,utils:safe_binary(SimName),Attributes,Notification}).

-spec cancel(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
cancel(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{cancel,utils:safe_binary(SimName),Attributes,Notification}).

-spec restart(SimName::string()|binary(), Attributes::attribute_list(), Notification::notification_cb() )-> sim_operation_res() | generic_error().
restart(SimName,Attributes,{_M,_F,_A}=Notification)->
	gen_server:call(?SERVER,{restart,utils:safe_binary(SimName),Attributes,Notification}).

-spec list_actions() -> {ok,[sim_action()]}.
list_actions()->
	gen_server:call(?SERVER,list_actions).

-spec list_simulation_states() -> {ok,#{ SimName::binary() => #sim_state{}}}.
list_simulation_states()->
	gen_server:call(?SERVER,list_simulation_states).

-spec get_simulation_state(SimName::string()|binary()) -> {ok,sim_state()} | generic_error().
get_simulation_state(SimName)->
	gen_server:call(?SERVER,{get_simulation_state,utils:safe_binary(SimName)}).

-spec get_action(JobID::binary()|string()) -> {ok,sim_action()}| generic_error().
get_action(JobID)->
	gen_server:call(?SERVER,{get_action,utils:safe_binary(JobID)}).

-spec list_simulations() -> {ok,[string()]} | generic_error().
list_simulations() ->
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
	Simulations = inventory:list_records(#simulation{}),
	NewStates = lists:foldl( fun(E,A)->
			maps:put( E#simulation.name, #sim_state{
				pushed = false,
				start = os:system_time(),
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

handle_call({get_action,JobID}, _From, State = #simengine_state{}) ->
	case maps:get(JobID,State#simengine_state.sim_actions,undefined) of
		undefined ->
			{reply,?ERROR_SIM_ACTION_UNKNOWN,State};
		Job->
			{reply,{ok,Job},State}
	end;

handle_call(list_actions, _From, State = #simengine_state{}) ->
	ListOfActions = maps:fold(fun(_,V,A) -> [V|A] end,[],State#simengine_state.sim_actions),
	{reply,{ok,ListOfActions},State};

handle_call({create_simulation,SimInfo}, _From, State = #simengine_state{}) ->
	case inventory:exists(SimInfo) of
		true ->
			{reply,?ERROR_SIM_ALREADY_EXISTS};
		false->
			?L_IA("Creating simulation ~s.",[binary_to_list(SimInfo#simulation.name)]),
			case inventory:add_record(SimInfo) of
				ok ->
					SimState = #sim_state{
						start = os:system_time(),
						state = created,
						current_op = none,
						pushed = false,
						sim_info = SimInfo },
					{reply, ok, State#simengine_state{
						sim_states = maps:put(SimInfo#simulation.name,SimState,State#simengine_state.sim_states )}};
				Error -> { reply, {error,Error} , State}
			end
	end;

handle_call({delete_simulation,SimName,CAName}, _From, State = #simengine_state{}) ->
	case inventory:exists(#simulation{ name = SimName} ) of
		false ->
			{reply,?ERROR_SIM_UNKNOWN,State};
		true->
			case inventory:get_record(#ca_info{name = CAName}) of
				{ok,CAInfo} ->
					?L_IA("Deleting simulation ~s.",[binary_to_list(SimName)]),
					_=inventory:del_record(#simulation{name = SimName}),
					CADir = binary_to_list(CAInfo#ca_info.dir_name),
					Dirs = [ "clients" , "servers" , "newcerts" ],
					Files = [ "index.txt", "index.txt.attr", "index.txt.old", "index.txt.attr.old", "serial.txt", "serial.txt.old" ],
					_ = [ file:del_dir_r(filename:join([CADir,X])) || X<-Dirs ],
					_ = [ file:make_dir(filename:join([CADir,X])) || X<-Dirs ],
					_ = [ file:delete(filename:join([CADir,X])) || X<-Files ],
					ok = file:write_file( filename:join([CADir, "index.txt"]),<<>>),
					ok = file:write_file( filename:join([CADir, "serial.txt"]),<<$0,$1>>),
					inventory:delete_all_records(clients),
					inventory:delete_all_records(servers),
					{reply,ok,State#simengine_state{ sim_states = maps:remove(SimName,State#simengine_state.sim_states)}};
				_ ->
					{reply,?ERROR_CA_UNKNOWN,State}
			end
	end;

handle_call({update_simulation,SimInfo}, _From, State = #simengine_state{}) ->
	case inventory:exists(SimInfo) of
		false ->
			{reply,?ERROR_SIM_UNKNOWN,State};
		true->
			?L_IA("Updating simulation ~s.",[binary_to_list(SimInfo#simulation.name)]),
			case inventory:add_record(SimInfo) of
				ok ->
					SimState = #sim_state{ sim_info = SimInfo },
					{reply, ok, State#simengine_state{ sim_states = maps:put(SimInfo#simulation.name,SimState,State#simengine_state.sim_states )}};
				Error ->
					{ reply, {error,Error} , State}
			end
	end;

handle_call({prepare,SimName,Attributes,Notification},_From,State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name=SimName}) of
		{error,_}->
			{reply,?ERROR_SIM_UNKNOWN,State};
		{ok,SimInfo} ->
			case SimInfo#simulation.assets_created of
				true ->
					{reply,?ERROR_SIM_ASSETS_ALREADY_CREATED,State};
				false->
					S = maps:get(SimName,State#simengine_state.sim_states),
					case length(S#sim_state.outstanding_nodes)>0 of
						true->
							{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
						false->
							JobId = utils:uuid_b(),
							OpPid=spawn_link(?MODULE,prepare_assets,[SimInfo,Attributes,self(),Notification,JobId]),
							SimAction = #sim_action{
								id = JobId,
								action = <<"prepare">>,
								created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
								simulation = SimName,
								parameters = Attributes,
								status = <<"started">>,
								completed = <<>>,
								target_count = 1,
								done_count = 0,
								start_os_time = os:system_time()
								},
							NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
							{reply,{ok,JobId},State#simengine_state{
								sim_states = maps:put(SimName,
								                      S#sim_state{current_op_pid = OpPid,
								                                  current_op = preparing ,
								                                  start = os:system_time(),
								                                  outstanding_nodes = [node()] },
							                                                       State#simengine_state.sim_states),
								sim_actions = NewActions }}
					end
			end
	end;

handle_call({push,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name=SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case (SimInfo#simulation.assets_created) of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_ALREADY_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									JobId = utils:uuid_b(),
									OpPid=spawn_link(?MODULE,push_assets,[SimInfo,Attributes,self(),Notification,JobId]),
									SimAction = #sim_action{
										id = JobId,
										action = <<"push">>,
										created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
										simulation = SimName,
										parameters = Attributes,
										status = <<"started">>,
										completed = <<>>,
										target_count = length(SimInfo#simulation.nodes),
										start_os_time = os:system_time(),
										done_count = 0
									},
									NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
									{reply,{ok,JobId},State#simengine_state{ sim_states = maps:put(SimName,
									                                                               S#sim_state{current_op_pid = OpPid, current_op = pushing ,
			                                                                           start = os:system_time(),
			                                                                           outstanding_nodes = SimInfo#simulation.nodes },
									                                                               State#simengine_state.sim_states),
									                                         sim_actions = NewActions }}
							end
					end
			end
	end;

handle_call({start,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name=SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case S#sim_state.state of
										started->
											{ reply, ?ERROR_SIM_ALREADY_STARTED };
										_ ->
											JobId = utils:uuid_b(),
											OpPid=spawn_link(?MODULE,start_assets,[SimInfo,Attributes,self(),Notification,JobId]),
											SimAction = #sim_action{
												action = <<"start">>,
												id = JobId,
												created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
												simulation = SimName,
												parameters = Attributes,
												status = <<"started">>,
												completed = <<>>,
												target_count = length(SimInfo#simulation.nodes),
												done_count = 0,
												start_os_time = os:system_time()
											},
											NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
											{reply,{ok,JobId},State#simengine_state{ sim_states = maps:put(SimName,
											                                                               S#sim_state{current_op_pid = OpPid, current_op = starting ,
											                                                                           start = os:system_time(),
											                                                                           outstanding_nodes = SimInfo#simulation.nodes },
											                                                               State#simengine_state.sim_states),
											                                         sim_actions = NewActions }}
									end
							end
					end
			end
	end;

handle_call({stop,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name = SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==started) or (S#sim_state.state==paused))  of
										true->
											JobId = utils:uuid_b(),
											OpPid=spawn_link(?MODULE,stop_assets,[SimInfo,Attributes,self(),Notification,JobId]),
											SimAction = #sim_action{
												id = JobId,
												action = <<"stop">>,
												created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
												simulation = SimName,
												parameters = Attributes,
												status = <<"started">>,
												completed = <<>>,
												target_count = length(SimInfo#simulation.nodes),
												done_count = 0,
												start_os_time = os:system_time()
											},
											NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
											{reply,{ok,JobId},State#simengine_state{ sim_states = maps:put(SimName,
											                                                               S#sim_state{current_op_pid = OpPid,
											                                                                           current_op = stopping ,
											                                                                           outstanding_nodes = SimInfo#simulation.nodes },
											                                                               State#simengine_state.sim_states),
											                                         sim_actions = NewActions }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED_OR_PAUSED }
									end
							end
					end
			end
	end;

handle_call({pause,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name = SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case (S#sim_state.state==started) of
										true->
											JobId = utils:uuid_b(),
											OpPid=spawn_link(?MODULE,pause_assets,[SimInfo,Attributes,self(),Notification,JobId]),
											SimAction = #sim_action{
												id = JobId,
												action = <<"pause">>,
												created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
												simulation = SimName,
												parameters = Attributes,
												status = <<"started">>,
												completed = <<>>,
												target_count = length(SimInfo#simulation.nodes),
												done_count = 0,
												start_os_time = os:system_time()
											},
											NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
											{reply,{ok,JobId},State#simengine_state{ sim_states = maps:put(SimName,
											                                                               S#sim_state{current_op_pid = OpPid,
											                                                                           current_op = pausing ,
											                                                                           outstanding_nodes = SimInfo#simulation.nodes },
											                                                               State#simengine_state.sim_states),
											                                         sim_actions = NewActions }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED }
									end
							end
					end
			end
	end;

handle_call({cancel,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name = SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==started) or (S#sim_state.state==paused) or (S#sim_state.state==stopped)) of
										true->
											JobId = utils:uuid_b(),
											OpPid=spawn_link(?MODULE,cancel_assets,[SimInfo,Attributes,self(),Notification,JobId]),
											SimAction = #sim_action{
												id = JobId,
												action = <<"cancel">>,
												created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
												simulation = SimName,
												parameters = Attributes,
												status = <<"started">>,
												completed = <<>>,
												target_count = length(SimInfo#simulation.nodes),
												done_count = 0,
												start_os_time = os:system_time()
											},
											NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
											{reply,{ok,JobId},State#simengine_state{ sim_states = maps:put(SimName,
											                                                               S#sim_state{current_op_pid = OpPid,
											                                                                           current_op = cancelling ,
											                                                                           outstanding_nodes = SimInfo#simulation.nodes },
											                                                               State#simengine_state.sim_states),
											                                         sim_actions = NewActions }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_STARTED_OR_PAUSED_OR_STOPPED }
									end
							end
					end
			end
	end;

handle_call({restart,SimName,Attributes,Notification}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name=SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			S = maps:get(SimName,State#simengine_state.sim_states),
			case SimInfo#simulation.assets_created of
				false->
					{reply,?ERROR_SIM_NO_ASSETS_EXIST,State};
				true->
					case not S#sim_state.pushed of
						true ->
							{reply,?ERROR_SIM_ASSETS_NOT_PUSHED,State};
						false->
							case length(S#sim_state.outstanding_nodes)>0 of
								true->
									{reply,?ERROR_SIM_OPERATION_IN_PROGRESS,State};
								false->
									case ((S#sim_state.state==paused) or (S#sim_state.state==stopped)) of
										true->
											JobId = utils:uuid_b(),
											OpPid=spawn_link(?MODULE,restarting_assets,[SimInfo,Attributes,self(),Notification,JobId]),
											SimAction = #sim_action{
												id = JobId,
												action = <<"restart">>,
												created = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
												simulation = SimName,
												parameters = Attributes,
												status = <<"started">>,
												completed = <<>>,
												target_count = length(SimInfo#simulation.nodes),
												done_count = 0,
												start_os_time = os:system_time()
											},
											NewActions = maps:put(JobId,SimAction,State#simengine_state.sim_actions),
											{reply,{ok,JobId},State#simengine_state{
												sim_states = maps:put(SimName,
                                                S#sim_state{current_op_pid = OpPid, current_op = restarting , outstanding_nodes = [node()] },
                                                State#simengine_state.sim_states),
		                                         sim_actions = NewActions }};
										false->
											{ reply, ?ERROR_SIM_MUST_BE_PAUSED_OR_STOPPED }
									end
							end
					end
			end
	end;

handle_call({get,SimName}, _From, State = #simengine_state{}) ->
	case inventory:get_record(#simulation{name=SimName}) of
		{error,_} ->
			{ reply, ?ERROR_SIM_UNKNOWN, State };
		{ok,SimInfo} ->
			{reply, {ok,SimInfo}, State}
	end;

handle_call({get_simulation_state,SimName}, _From, State = #simengine_state{}) ->
	case maps:get(SimName,State#simengine_state.sim_states,undefined) of
		undefined ->
			{ reply, {error,?ERROR_SIM_UNKNOWN},State};
		SimState ->
			{ reply, {ok,SimState},State}
	end;

handle_call(list_simulations, _From, State = #simengine_state{}) ->
	{reply,inventory:list_records_names(#simulation{}),State};

handle_call(list_simulation_states, _From, State = #simengine_state{}) ->
	{ reply, {ok, State#simengine_state.sim_states}, State };

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

handle_info({ SimName,Node,MsgType,TimeStamp,JobId}=_Msg, State = #simengine_state{}) ->
	try
		SimState = maps:get(SimName,State#simengine_state.sim_states,undefined),
		NewNodes = lists:delete(Node,SimState#sim_state.outstanding_nodes),
		Now = erlang:timestamp(),
		Elapsed = timer:now_diff(Now,TimeStamp) / 1000000,
		% io:format("SIMENGINE-MSG: SimName:~p MsgType:~p~n",[SimName,MsgType]),
		NewSimState = case MsgType of
				prepare_done->
					?L_IA("Node ~p prepared. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes, state = prepared, current_op = none };
				push_done ->
					?L_IA("Node ~p push done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes , pushed = true , state = pushed, current_op = none };
				start_done ->
					?L_IA("Node ~p start done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes, state = started, current_op = none };
				stop_done ->
					?L_IA("Node ~p stop done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes , state = stopped, current_op = none  };
				pause_done ->
					?L_IA("Node ~p pause done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes, state = paused, current_op = none  };
				cancel_done ->
					?L_IA("Node ~p cancel done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes, state = cancelled, current_op = none  };
				restart_done ->
					?L_IA("Node ~p restart done. Took ~p seconds.~n",[Node,Elapsed]),
					SimState#sim_state{ outstanding_nodes = NewNodes, state = started, current_op = none  }
			end,
			SimAction = maps:get(JobId,State#simengine_state.sim_actions,undefined),
			NewCount = SimAction#sim_action.done_count+1,
			NewAction = case NewCount == SimAction#sim_action.target_count of
				true ->
					SimAction#sim_action{ target_count = NewCount,
					                      done_count = NewCount,
					                      completed = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))) ,
					                      status = <<"completed">>,
																end_os_time = os:system_time()};
				false ->
					SimAction#sim_action{ done_count = NewCount}
			end,
			NewState = State#simengine_state{ sim_states = maps:put(SimName,NewSimState,State#simengine_state.sim_states),
			                                  sim_actions = maps:put(JobId,NewAction,State#simengine_state.sim_actions)},
		  {noreply,NewState}
	catch
		_:_ = Error ->
			io:format("Failed ~p processing INFO MESSAGE~n",[Error]),
			{ noreply, State }
	end;

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
-spec set_assets_created(SimInfo::simulation(), Value::boolean())->ok.
set_assets_created(SimInfo,Value)->
	_=mnesia:transaction( fun()->
												[Sim]=mnesia:read(simulations,SimInfo#simulation.name),
												_=mnesia:dirty_write(simulations,Sim#simulation{ assets_created = Value})
											end),
	ok.

-spec prepare_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
prepare_assets(SimInfo,_Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	StartedAt = erlang:timestamp(),
	?L_IA("~s: Preparing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	timer:sleep(2000),    %% wait 2 seconds... this will allow calling process some time to complete
	split_build_clients(SimInfo,utils:noop_mfa()),
	split_build_servers(SimInfo,utils:noop_mfa()),
	set_assets_created(SimInfo,true),
	SimEnginePid ! {SimInfo#simulation.name,node(),prepare_done,StartedAt,JobId},
	erlang:apply(M,F,A),
	?L_IA("~s: Prepared all assets.",[binary_to_list(SimInfo#simulation.name)]),
	ok.

-spec push_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
push_assets(SimInfo,_Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Pushing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	timer:sleep(2000),    %% wait 2 seconds... this will allow calling process some time to complete
	{ok,Clients} = inventory:list_sim_clients(SimInfo#simulation.name),
	Splits = utils:split_into( SimInfo#simulation.nodes, Clients),
	?L_IA("~s: Nodes: ~p.",[binary_to_list(SimInfo#simulation.name),SimInfo#simulation.nodes]),
	?L_IA("~s: Splits: ~p.",[binary_to_list(SimInfo#simulation.name),Splits]),
	_ = lists:reverse(lists:foldl(fun({N,C},Acc) ->
													Config = #{ sim_name => SimInfo#simulation.name,
													            sim_ca => SimInfo#simulation.ca,
													            clients => C,
																			ovsdb_server_name => SimInfo#simulation.opensync_server_name,
																			ovsdb_server_port => SimInfo#simulation.opensync_server_port,
																			callback => { SimEnginePid, {SimInfo#simulation.name, N,push_done,erlang:timestamp(),JobId} }},
													?L_IA("SIMENGINE: Pushing ~p entries to ~p.",[length(C),N]),
													R = rpc:call(N,simnode,set_configuration,[Config]),
													[R|Acc]
												end,[],Splits)),
	erlang:apply(M,F,A),
	?L_IA("~s: Pushed all assets.",[binary_to_list(SimInfo#simulation.name)]),
	ok.

-spec start_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
start_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Starting all assets.",[binary_to_list(SimInfo#simulation.name)]),
	timer:sleep(2000),    %% wait 2 seconds... this will allow calling process some time to complete
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,start,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,start_done,erlang:timestamp(),JobId}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	?L_IA("~s: Started all assets.",[binary_to_list(SimInfo#simulation.name)]),
	ok.

-spec stop_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
stop_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Stopping all assets.",[binary_to_list(SimInfo#simulation.name)]),
	timer:sleep(2000),    %% wait 2 seconds... this will allow calling process some time to complete
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,stop,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,stop_done,erlang:timestamp(),JobId}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	io:format("Stopping assets~n"),
	ok.

-spec pause_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
pause_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Pausing all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,pause,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,pause_done,erlang:timestamp(),JobId}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec cancel_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
cancel_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Cancelling all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,cancel,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,cancel_done,erlang:timestamp(),JobId}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec restarts_assets(SimInfo::simulation(), Attributes::#{atom()=>term()}, ManagerPis::pid(),Notification::notification_cb(),JobId::binary())->ok.
restarts_assets(SimInfo,Attributes,SimEnginePid,{M,F,A}=_Notification,JobId)->
	?L_IA("~s: Restarting all assets.",[binary_to_list(SimInfo#simulation.name)]),
	_Results = lists:reverse(lists:foldl(fun(Node,Acc) ->
		R = rpc:call(Node,simnode,restart,[all,Attributes#{ callback => {SimEnginePid,{SimInfo#simulation.name, Node,restart_done,erlang:timestamp(),JobId}} }]),
		[R|Acc] end,[],SimInfo#simulation.nodes)),
	apply(M,F,A),
	ok.

-spec split_build_clients( SimInfo::simulation(), NotificationCB::notification_cb())->ok.
split_build_clients(SimInfo,Notification)->
	{ok,HardwareDefinitions} = hardware:get_definitions(),
	Ids = [ X#hardware_info.id || X <- HardwareDefinitions ],
	{ok,CAInfo} = inventory:get_ca(SimInfo#simulation.ca),
	_ = run_batch(Ids,SimInfo,CAInfo,0,Notification),
	ok.

random_list_item(L)->
	lists:nth(rand:uniform(length(L)),L).

run_batch(_Ids,SimInfo,_CAInfo,Index,{M,F,A}=_Notification) when SimInfo#simulation.num_devices == Index->
	apply(M,F,A);
run_batch(Ids,SimInfo,CAInfo,Total,Notification)->
	Id = random_list_item(Ids),
	Attributes = #{ id => Id, name => <<"SIM">>, serial => <<"SIM">>, mac => <<>> },
	inventory:generate_single_client(Id,CAInfo,SimInfo#simulation.name,Total,Attributes),
	run_batch(Ids,SimInfo,CAInfo,Total+1,Notification).

%% Create the servers - only if they are pon automatic mode
split_build_servers(SimInfo,_Notification)->
	_ = generate_server(SimInfo,ovsdb_server),
	ok.

%% generate_server(#simulation{ internal = true } = SimInfo,mqtt_server)->
%%  	inventory:make_server(SimInfo#simulation.ca,"mqtt-1",mqtt_server);
generate_server(#simulation{ internal = true } = SimInfo,ovsdb_server )->
	inventory:make_server(SimInfo#simulation.ca,SimInfo#simulation.name,"ovsdb-1",ovsdb_server);
generate_server(_,_)->
	ok.

-spec sim_action_to_json(#sim_action{}) -> binary().
sim_action_to_json(SimAction)->
	Parameters = case maps:get(stagger,SimAction#sim_action.parameters,undefined) of
		             { D , T } ->
			              #{ name => stagger , value => list_to_binary(integer_to_list(D) ++ "/" ++ integer_to_list(T))};
		             undefined ->
										#{}
	             end,
	jiffy:encode(#{
			id => SimAction#sim_action.id,
			operation => SimAction#sim_action.action,
			status => SimAction#sim_action.status,
			created => SimAction#sim_action.created,
			completed => SimAction#sim_action.completed,
			parameters => [Parameters],
			done_count => SimAction#sim_action.done_count,
			target_count => SimAction#sim_action.target_count,
			execution_ms => (SimAction#sim_action.end_os_time-SimAction#sim_action.start_os_time) div 1000000
	}).


