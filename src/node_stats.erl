%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 10. Dec 2020 10:23 p.m.
%%%-------------------------------------------------------------------
-module(node_stats).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").

%% API
-export([start_link/0,creation_info/0,update_stats/0,node_type/0,find_manager/2,connect/1,disconnect/0,
	connected/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% -define(SERVER, {global,?MODULE}).
%% -define(START_SERVER,{global,?MODULE}).

-define(SERVER, ?MODULE).
-define(START_SERVER,{local,?MODULE}).

-record(node_state, {
	node_type,
	nodeid,
	updater_timer = undefined,
	node_finder_timer = undefined,
	manager }).

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

update_stats()->
	gen_server:cast(?SERVER,update_stats).

node_type()->
	gen_server:call(?SERVER,node_type).

-spec connect(NodeName::atom())-> {ok, none | node()}.
connect(NodeName) ->
	gen_server:call(?SERVER,{connect,NodeName}).

-spec disconnect() -> ok.
disconnect() ->
	gen_server:call(?SERVER,{disconnect,node()}).

-spec connected()->{ok,none|node()}.
connected()->
	gen_server:call(?SERVER,connected).

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
	{ok, State :: #node_state{}} | {ok, State :: #node_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	NodeId = utils:app_env(node_id,1),
	NodeType = utils:app_env(role,undefined),
	case NodeType of
		manager ->
			NodeFinder = undefined ;
		_ ->
			{ok,NodeFinder} = timer:apply_interval(5000,?MODULE,find_manager,[self(),NodeId])
	end,
	{ok,TRef} = timer:apply_interval(2000,?MODULE,update_stats,[]),
	{ok, #node_state{ node_type = NodeType, updater_timer = TRef, node_finder_timer = NodeFinder, nodeid = NodeId }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #node_state{}) ->
	                 {reply, Reply :: term(), NewState :: #node_state{}} |
	                 {reply, Reply :: term(), NewState :: #node_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #node_state{}} |
	                 {noreply, NewState :: #node_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #node_state{}} |
	                 {stop, Reason :: term(), NewState :: #node_state{}}).
handle_call({disconnect,_},_From,State=#node_state{})->
	manager:disconnect(),
	{ reply, ok, State#node_state{ manager = none }};
handle_call({connect,NodeName}, _From, State = #node_state{}) ->
	NewState = case State#node_state.manager of
		           none ->
			           try_connecting(NodeName,State);
		           _ ->
			           State
	           end,
	{ reply, {ok,NewState#node_state.manager}, NewState };
handle_call(connected, _From, State = #node_state{}) ->
	{ reply, { ok, State#node_state.manager } , State };
handle_call(node_type, _From, State = #node_state{}) ->
	{reply, {ok,State#node_state.node_type} , State};
handle_call(_Request, _From, State = #node_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #node_state{}) ->
	{noreply, NewState :: #node_state{}} |
	{noreply, NewState :: #node_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_state{}}).
handle_cast(update_stats, State = #node_state{}) ->
	statistics:submit_report(os_details,create_os_stats_report()),
	{noreply, State};
handle_cast( {manager_found,NodeName}, State = #node_state{}) ->
	%% io:format("Trying to connect now to ~p~n",[NodeName]),
	case State#node_state.manager == NodeName of
		true ->
			{noreply, State};
		false ->
			NewState=try_connecting(NodeName,State),
			{noreply, NewState}
	end;
handle_cast(_Request, State = #node_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #node_state{}) ->
	{noreply, NewState :: #node_state{}} |
	{noreply, NewState :: #node_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_state{}}).
handle_info({nodedown,Node},State=#node_state{})->
%%	io:format("Node ~p is going down.~n",[Node]),
	manager:report_event(nodedown,#{ nodename => Node }),
	{noreply,State#node_state{ manager = none }};
handle_info(_Info, State = #node_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #node_state{}) -> term()).
terminate(_Reason, State = #node_state{}) ->
	_ = timer:cancel(State#node_state.updater_timer),
	_=case State#node_state.node_type of
		manager ->
			ok;
		_ ->
			timer:cancel(State#node_state.node_finder_timer)
	end,
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #node_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #node_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #node_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

volumes_to_tuples([],A)->
	A;
volumes_to_tuples([{Name,Size,_}|T],A)->
	volumes_to_tuples(T,[#{ name => list_to_binary(Name), size => Size }|A]).

cpu_details_to_tuples([],A)->
	A;
cpu_details_to_tuples([{Cpu,Busy,Idle,_}|T],A)->
%%	io:format("CPU: ~p BUSY: ~p IDLE: ~p~n",[Cpu,Busy,Idle]),
	cpu_details_to_tuples(T,[[#{cpu=>Cpu , busy=>Busy, idle=>Idle}]|A]).

create_os_stats_report() ->
	{X1,X2,{_,X3}} = memsup:get_memory_data(),
	MemoryData = #{total => X1, allocated=>X2, biggest=>X3 },
	SystemMemoryData = memsup:get_system_memory_data(),
	{ Cpus, DetailCpu, NonBusy, _ } = cpu_sup:util([detailed]),

%%	io:format("CPUS: ~p~n",[cpu_sup:util([per_cpu])]),

	Report = #{
		cpu_utilization => cpu_sup:util(),
		cpu_avg1  => 		cpu_sup:avg1(),
		cpu_avg5  => 		cpu_sup:avg5(),
		cpu_avg15 => 		cpu_sup:avg15(),
		number_of_processes => cpu_sup:nprocs(),
		sysmem_high_watermark => memsup:get_sysmem_high_watermark(),
		procmem_high_watermark => memsup:get_procmem_high_watermark(),
		mem_check_interval => memsup:get_check_interval(),
		mem_helper_timeout => memsup:get_helper_timeout(),
		disk_check_interval => disksup:get_check_interval(),
		memory_data => MemoryData,
		number_of_cpus => length(Cpus),
		kernel_utilization => proplists:get_value(kernel,DetailCpu,0.0),
		nice_user => proplists:get_value(nice_user,DetailCpu,0.0),
		user => proplists:get_value(user,DetailCpu,0.0),
		idle => proplists:get_value(idle,NonBusy,0.0),
		disk_almost_full_threshold => disksup:get_almost_full_threshold(),
		free_memory => proplists:get_value(free_memory,SystemMemoryData,0),
		total_memory => proplists:get_value(total_memory,SystemMemoryData,0),
		system_total_memory => proplists:get_value(system_total_memory,SystemMemoryData,0),
		cpu_details => cpu_details_to_tuples(cpu_sup:util([per_cpu]),[]),
		disk_details => volumes_to_tuples(disksup:get_disk_data(),[])
	},
	Report.

find_manager(Pid,Id) ->
	%% io:format("Looking for manager~n"),
	case node_finder:receiver(Id) of
		{error,_Reason} ->
			%% io:format("No manager~n"),
			ok;
		{ok,NodeName} ->
			%% io:format("Found manager~n"),
			gen_server:cast( Pid , { manager_found, NodeName}),
			ok
	end.

try_connecting(NodeName,State)->
	case NodeName == State#node_state.manager of
		true ->
			State;
		false ->
			case net_adm:ping(NodeName) of
				pong ->
					%% io:format("got pong~n"),
					_=global:sync(),
					manager:connect(State#node_state.node_type),
					erlang:monitor_node(NodeName,true),
					?L_IA("Adding new manager ~p node.",[NodeName]),
					State#node_state{ manager = NodeName };
				pang ->
					?L_IA("Manager node ~p unresponsive.",[NodeName]),
					State
			end
	end.
