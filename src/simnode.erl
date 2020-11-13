%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2020 12:10 p.m.
%%%-------------------------------------------------------------------
-module(simnode).
-author("stephb").

-include("../include/internal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,creation_info/0,connect/1,disconnect/0,find_manager/2,connected/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(simnode_state, { node_finder, stats_updater, node_id, manager }).

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

connect(NodeName) ->
	gen_server:call(?SERVER,{connect,NodeName}).

disconnect() ->
	gen_server:call(?SERVER,{disconnect,node()}).

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
	{ok, State :: #simnode_state{}} | {ok, State :: #simnode_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	NodeId = application:get_env(?OWLS_APP,node_id,1),
	{ok,NodeFinder} = timer:apply_interval(20000,?MODULE,find_manager,[self(),NodeId]),
	{ok,StatsUpdater} = timer:apply_interval(5000,manager,send_stats_report,[]),
	{ok,#simnode_state{ node_finder = NodeFinder, stats_updater = StatsUpdater , node_id = NodeId, manager = none }}.

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
	NewState = try_connecting(NodeName,State),
	{ reply, ok, NewState };
handle_call(connected, _From, State = #simnode_state{}) ->
	{ reply, { ok, State#simnode_state.manager } , State };
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
	{noreply,NewState};
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
	timer:cancel(State#simnode_state.stats_updater),
	timer:cancel(State#simnode_state.node_finder),
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
		unknown ->
			ok;
		NodeName ->
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
					global:sync(),
					manager:connect(),
					erlang:monitor_node(NodeName,true),
					lager:info("Adding new manager node."),
					State#simnode_state{ manager = NodeName };
				pang ->
					lager:info("Node ~p unresponsive.",[NodeName]),
					State
			end
	end.