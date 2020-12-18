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
-include("../include/statistics.hrl").

-compile({parse_transform, lager_transform}).
-dialyzer(no_match).

-behaviour(gen_server).

%%-define(SERVER, {global,?MODULE}).
%%-define(START_SERVER,{global,?MODULE}).

-define(SERVER, ?MODULE).
-define(START_SERVER,{local,?MODULE}).

%% API
-export([start_link/1,creation_info/0,set_configuration/1,reset_configuration/1,
	 get_configuration/0,set_configuration/2,get_configuration/1,update_stats/3,start/2,restart/2,
   pause/2,cancel/2,stop/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-record(simnode_state, {
	node_finder = none :: timer:tref(),
	os_stats_updater = none :: timer:tref(),
	ap_client_handler = undefined :: atom(),
	mqtt_server_handler = undefined :: atom(),
	ovsdb_server_handler = undefined :: atom(),
	node_id = 0 :: integer(),
	manager = undefined :: atom(),
	sim_configuration = #{} :: #{ atom() => string()},
	client_pids = #{} :: #{ string() => pid() }}).

%% ovsdb_server
%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [[{ap_client,ovsdb_client_handler},{mqtt_server,mqtt_server_handler}]] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

%% ovsdb_server

-spec pause( all | [UUID::binary()], Attributes::attribute_list()) -> ok | generic_error().
pause(UIDS,Attributes ) ->
	gen_server:call(?SERVER,{pause,UIDS,Attributes}).

-spec restart( all | [UUID::binary()], Attributes::attribute_list()) -> ok | generic_error().
restart(UIDS,Attributes ) ->
	gen_server:call(?SERVER,{restart,UIDS,Attributes}).

-spec start( all | [UUID::binary()], Attributes::attribute_list()) -> ok | generic_error().
start( UIDS,Attributes ) ->
	gen_server:call(?SERVER,{start,UIDS,Attributes}).

-spec stop( all | [UUID::binary()], Attributes::attribute_list()) -> ok | generic_error().
stop( UIDS,Attributes ) ->
	gen_server:call(?SERVER,{stop,UIDS,Attributes}).

-spec cancel( all | [UUID::binary()], Attributes::attribute_list()) -> ok | generic_error().
cancel( UIDS,Attributes ) ->
	gen_server:call(?SERVER,{cancel,UIDS,Attributes}).

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

-spec update_stats( Client::any_role(), Role::any_role(), Stats::#{})-> ok.
update_stats(Client,Role,Stats)->
	gen_server:cast(?SERVER,{update_stats,Client,Role,Stats}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc Spawns the server and registers the local name (unique)
-spec start_link(Config::term()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Config) ->
	gen_server:start_link(?START_SERVER, ?MODULE, [Config], []).

%% @private
%% @doc Initializes the server
-spec init(Args :: proplists:proplist()) -> {ok, State :: #simnode_state{}}.
	%%| {ok, State :: #simnode_state{}, timeout() | hibernate} |
	%%{stop, Reason :: term()} | ignore).
init([Config]) ->
	_=utils:priv_dir(),
	{ ok, #simnode_state{ node_id = utils:app_env(node_id,1),
		                    ap_client_handler = proplists:get_value(ap_client,Config,undefined),
		                    mqtt_server_handler = proplists:get_value(mqtt_server,Config,undefined),
		                    ovsdb_server_handler = proplists:get_value(ovsdb_server,Config,undefined),
		                    manager = none }}.

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
handle_call({set_configuration,Configuration}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, set_configuration, [Configuration]),
	safe_execute( State#simnode_state.mqtt_server_handler, set_configuration, [Configuration]),
	safe_execute( State#simnode_state.ovsdb_server_handler, set_configuration, [Configuration]),
	?L_I("Configuration sent to all handlers."),
	{ reply, ok , State#simnode_state{ sim_configuration = Configuration } };

handle_call({start,UIDs,Attributes}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, start, [UIDs,Attributes]),
	safe_execute( State#simnode_state.mqtt_server_handler, start, [UIDs,Attributes]),
	safe_execute( State#simnode_state.ovsdb_server_handler, start, [UIDs,Attributes]),
	?L_I("START sent to all handlers."),
	{ reply, ok , State};

handle_call({stop,UIDs,Attributes}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, stop, [UIDs,Attributes]),
	safe_execute( State#simnode_state.mqtt_server_handler, stop, [UIDs,Attributes]),
	safe_execute( State#simnode_state.ovsdb_server_handler, stop, [UIDs,Attributes]),
	?L_I("STOP sent to all handlers."),
	{ reply, ok , State};

handle_call({pause,UIDs,Attributes}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, pause, [UIDs,Attributes]),
	safe_execute( State#simnode_state.mqtt_server_handler, pause, [UIDs,Attributes]),
	safe_execute( State#simnode_state.ovsdb_server_handler, pause, [UIDs,Attributes]),
	?L_I("PAUSE sent to all handlers."),
	{ reply, ok , State};

handle_call({restart,UIDs,Attributes}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, restart, [UIDs,Attributes]),
	safe_execute( State#simnode_state.mqtt_server_handler, restart, [UIDs,Attributes]),
	safe_execute( State#simnode_state.ovsdb_server_handler, restart, [UIDs,Attributes]),
	?L_I("RESTART sent to all handlers."),
	{ reply, ok , State};

handle_call({cancel,UIDs,Attributes}, _From, State = #simnode_state{}) ->
	safe_execute( State#simnode_state.ap_client_handler, cancel, [UIDs,Attributes]),
	safe_execute( State#simnode_state.mqtt_server_handler, cancel, [UIDs,Attributes]),
	safe_execute( State#simnode_state.ovsdb_server_handler, cancel, [UIDs,Attributes]),
	?L_I("CANCEL sent to all handlers."),
	{ reply, ok , State};

handle_call(get_configuration, _From, State = #simnode_state{}) ->
	{ reply, {ok , State#simnode_state.sim_configuration} ,State };
handle_call({reset_configuration,_NewAttributes}, _From, State = #simnode_state{}) ->
	{ reply, ok , State };
handle_call(_Request, _From, State = #simnode_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #simnode_state{}) ->
	{noreply, NewState :: #simnode_state{}} |
	{noreply, NewState :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #simnode_state{}}).
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
terminate(_Reason, _State = #simnode_state{}) ->
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
safe_execute(undefined,_F,_A)->
	ok;
safe_execute(M,F,A)->
%%	io:format("Executing: ~p:~p(Config)~n",[M,F]),
	apply(M,F,A).
