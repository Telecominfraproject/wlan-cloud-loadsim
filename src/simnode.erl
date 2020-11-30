%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2020 12:10 p.m.
%%%-------------------------------------------------------------------
-module(simnode).
-author("stephb").

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/sim_commands.hrl").

-compile({parse_transform, lager_transform}).
-dialyzer(no_match).

-behaviour(gen_server).

%% API
-export([start_link/0,creation_info/0,connect/1,disconnect/0,find_manager/2,connected/0,
	set_configuration/1,reset_configuration/1,set_operation_state/2,execute/1,set_client/2,
	get_configuration/0,set_configuration/2,get_configuration/1,register_handler/2,
	update_stats/3,send_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(simnode_state, {
	node_finder = none :: timer:tref(),
	os_stats_updater = none :: timer:tref(),
	mqtt_client_handler,
	ovsdb_client_handler,
	mqtt_server_handler,
	ovsdb_server_handler,
	node_id = 0 :: integer(),
	manager = undefined :: atom(),
	sim_configuration = #{} :: #{ atom() => string()},
	seq_commands = [] :: list( sim_operation() ),
	par_commands = #{} :: #{ string() => sim_operation() },
	executing = #{} ::  #{ string() => sim_operation_state() },
	client_pids = #{} :: #{ string() => pid() }}).

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

-spec connect(NodeName::atom())-> {ok, none | node()}.
connect(NodeName) ->
	gen_server:call(?SERVER,{connect,NodeName}).

-spec disconnect() -> ok.
disconnect() ->
	gen_server:call(?SERVER,{disconnect,node()}).

-spec connected()->{ok,none|node()}.
connected()->
	gen_server:call(?SERVER,connected).

set_configuration(Configuration)->
	gen_server:call(?SERVER,{set_configuration,Configuration}).

get_configuration()->
	gen_server:call(?SERVER,get_configuration).

-spec set_configuration(Node::node(),Configuration::term())->ok.
set_configuration(Node,Configuration)->
	gen_server:call({?SERVER,Node},{set_configuration,Configuration}).

-spec get_configuration(Node::node())->{ok,Configuration::term()}.
get_configuration(Node)->
	gen_server:call({?SERVER,Node},get_configuration).

reset_configuration(State)->
	gen_server:call(?SERVER,{reset_configuration,State}).

-spec execute( sim_operation() | [sim_operation()] ) -> ok | { error , Reason::term }.
execute(Command) when is_record(Command,sim_operation)->
	gen_server:call(?SERVER,{execute,[Command]});
execute(Commands) when is_list(Commands)->
	gen_server:call(?SERVER,{execute,Commands}).

-spec register_handler( ClientType::any_role() , Module::module() ) -> ok.
register_handler( ClientType, Module )->
	gen_server:call(?SERVER,{register_handle,ClientType,Module}).

set_operation_state(Operation,NewState)->
	gen_server:cast(?SERVER,{set_state,Operation,NewState}).

-spec set_client( Client::string(), ClientPid:: none |  pid())->ok.
set_client(Client,Pid)->
	gen_server:cast(?SERVER,{set_client_pid,Client,Pid}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec update_stats( Client::any_role(), Role::any_role(), Stats::#{})-> ok.
update_stats(Client,Role,Stats)->
	gen_server:cast(?SERVER,{update_stats,Client,Role,Stats}).

send_stats()->
	manager:send_os_stats_report(create_os_stats_report()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #simnode_state{}} | {ok, State :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	NodeId = utils:app_env(node_id,1),
	{ok,NodeFinder} = timer:apply_interval(20000,?MODULE,find_manager,[self(),NodeId]),
	{ok,StatsUpdater} = timer:apply_interval(5000,?MODULE,send_stats,[]),
	{ok,#simnode_state{ node_finder = NodeFinder, os_stats_updater = StatsUpdater , node_id = NodeId, manager = none }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #simnode_state{}) ->
	{reply, Reply :: term(), NewState :: #simnode_state{}} |
	{reply, Reply :: term(), NewState :: #simnode_state{}, timeout() | hibernate} |
	{noreply, NewState :: #simnode_state{}} |
	{noreply, NewState :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #simnode_state{}} |
	{stop, Reason :: term(), NewState :: #simnode_state{}}).
handle_call({disconnect,_},_From,State=#simnode_state{})->
	manager:disconnect(),
	{ reply, ok, State#simnode_state{ manager = none }};
handle_call({connect,NodeName}, _From, State = #simnode_state{}) ->
	NewState = case State#simnode_state.manager of
		none ->
			try_connecting(NodeName,State);
	  _ ->
		  State
	end,
	{ reply, {ok,NewState#simnode_state.manager}, NewState };
handle_call(connected, _From, State = #simnode_state{}) ->
	{ reply, { ok, State#simnode_state.manager } , State };
handle_call({set_configuration,Configuration}, _From, State = #simnode_state{}) ->
	{ reply, ok , State#simnode_state{ sim_configuration = Configuration } };
handle_call(get_configuration, _From, State = #simnode_state{}) ->
	{ reply, {ok , State#simnode_state.sim_configuration} ,State };
handle_call({register_handle,ClientType,Module}, _From, State = #simnode_state{}) ->
	NewState = case ClientType of
		           mqtt_client -> State#simnode_state{ mqtt_client_handler = Module };
							 ovsdb_client -> State#simnode_state{ ovsdb_client_handler = Module };
							 mqtt_server -> State#simnode_state{ mqtt_server_handler = Module };
							 ovsdb_server -> State#simnode_state{ ovsdb_server_handler = Module }
	end,
	{ reply, ok , NewState };
handle_call({reset_configuration,_NewAttributes}, _From, State = #simnode_state{}) ->
	{ reply, ok , State };
handle_call({execute,Commands}, _From, State = #simnode_state{}) ->
	{NewSeq,NewPar,Rejected} = lists:foldl(fun(Element,{Seqs,Pars,Rejected}) ->
													case Element#sim_operation.type of
														parallel->
															{ Seqs, maps:put(Element#sim_operation.uuid,Element,Pars),Rejected };
														sequential ->
															{ Seqs ++ [ Element ], Pars, Rejected};
														_ ->
															{ Seqs , Pars, Rejected ++ [Element]}
													end
												end,{State#simnode_state.seq_commands,State#simnode_state.par_commands,[]},Commands),
	{ reply, { ok, Rejected } , State#simnode_state{ seq_commands = NewSeq, par_commands = NewPar } };
handle_call(_Request, _From, State = #simnode_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #simnode_state{}) ->
	{noreply, NewState :: #simnode_state{}} |
	{noreply, NewState :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #simnode_state{}}).
handle_cast( {manager_found,NodeName}, State = #simnode_state{}) ->
	NewState=try_connecting(NodeName,State),
	{noreply, NewState};
handle_cast({set_client_pid,Client,none}, State = #simnode_state{}) ->
	NewPids = maps:remove(Client,State#simnode_state.client_pids),
	{noreply, State#simnode_state{ client_pids = NewPids }};
handle_cast({set_client_pid,Client,Pid}, State = #simnode_state{}) when is_pid(Pid)->
	NewPids = maps:put(Client,Pid,State#simnode_state.client_pids),
	{noreply, State#simnode_state{ client_pids = NewPids }};
handle_cast({set_state,Operation,NewState}, State = #simnode_state{}) ->
	NewOpStates = maps:put( Operation , NewState, State#simnode_state.executing ),
	{noreply, State#simnode_state{ executing = NewOpStates}};
handle_cast({update_stats,_Client,_Role,_Stats}, State = #simnode_state{}) ->
	{noreply,State};
handle_cast(_Request, State = #simnode_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #simnode_state{}) ->
	{noreply, NewState :: #simnode_state{}} |
	{noreply, NewState :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #simnode_state{}}).
handle_info({nodedown,Node},State=#simnode_state{})->
	io:format("Manager ~p is going down.~n",[Node]),
	_=lager:info("Manager ~p is going down.",[Node]),
	{noreply,State#simnode_state{ manager = none }};
handle_info(_Info, State = #simnode_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #simnode_state{}) -> term()).
terminate(_Reason, State = #simnode_state{}) ->
	_=timer:cancel(State#simnode_state.os_stats_updater),
	_=timer:cancel(State#simnode_state.node_finder),
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #simnode_state{},
		Extra :: term()) ->
	{ok, NewState :: #simnode_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #simnode_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_manager(Pid,Id) ->
	case node_finder:receiver(Id) of
		{error,_Reason} ->
			ok;
		{ok,NodeName} ->
			gen_server:cast( Pid , { manager_found, NodeName}),
			ok
	end.

try_connecting(NodeName,State)->
	case NodeName == State#simnode_state.manager of
		true ->
			State;
		false ->
			case net_adm:ping(NodeName) of
				pong ->
					_=global:sync(),
					manager:connect(),
					erlang:monitor_node(NodeName,true),
					_=lager:info("Adding new manager ~p node.",[NodeName]),
					State#simnode_state{ manager = NodeName };
				pang ->
					_=lager:info("Node ~p unresponsive.",[NodeName]),
					State
			end
	end.

create_os_stats_report() ->
	CpuSup = #{ avg1 => cpu_sup:avg1() , avg5 => cpu_sup:avg5(), avg15 => cpu_sup:avg15(),
	            nprocs => cpu_sup:nprocs(), util => cpu_sup:util(), detailed => cpu_sup:util([detailed]), per_cpu => cpu_sup:util([per_cpu])},
	DiskSup = #{ disk_data => disksup:get_disk_data(), check_interval => disksup:get_check_interval(),
	             almost_full_threshold => disksup:get_almost_full_threshold()},
	MemSup = #{ check_interval => memsup:get_check_interval(), procmem_high_watermark => memsup:get_procmem_high_watermark(),
	            sysmem_high_watermark => memsup:get_sysmem_high_watermark(), memory_data => memsup:get_memory_data(),
	            helper_timeout => memsup:get_helper_timeout(), system_memory_data => memsup:get_system_memory_data()},
	#{ cpu_sup => CpuSup,disk_sup => DiskSup, memsup => MemSup}.