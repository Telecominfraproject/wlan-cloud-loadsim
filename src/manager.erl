%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 19. Oct 2020 10:40 p.m.
%%%-------------------------------------------------------------------
-module(manager).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").

%% API
-export([start_link/0,creation_info/0,connect/0,disconnect/0,send_os_stats_report/1,connected_nodes/0]).
-export([log_info/1,log_info/2,log_error/1,log_error/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(manager_state, { nodes, stats }).

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

connect()->
	gen_server:call({global,?SERVER},{connect,node()}).

disconnect()->
	gen_server:call({global,?SERVER},{disconnect,node()}).

connected_nodes()->
	gen_server:call({global,?SERVER},connected_nodes).

send_os_stats_report(Report)->
	gen_server:cast({global,?SERVER},{stats_report,node(),Report}).

log_info(Message)->
	gen_server:cast({global,?SERVER},{log_info,node(),Message}).

log_info(Message,Args)->
	gen_server:cast({global,?SERVER},{log_info,node(),Message,Args}).

log_error(Message)->
	gen_server:cast({global,?SERVER},{log_error,node(),Message}).

log_error(Message,Args)->
	gen_server:cast({global,?SERVER},{log_error,node(),Message,Args}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #manager_state{}} | {ok, State :: #manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	startdb(),
	{ok, #manager_state{ nodes = sets:new(), stats = maps:new() }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #manager_state{}) ->
	{reply, Reply :: term(), NewState :: #manager_state{}} |
	{reply, Reply :: term(), NewState :: #manager_state{}, timeout() | hibernate} |
	{noreply, NewState :: #manager_state{}} |
	{noreply, NewState :: #manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #manager_state{}} |
	{stop, Reason :: term(), NewState :: #manager_state{}}).
handle_call({connect,NodeName}, _From, State = #manager_state{}) ->
	case sets:is_element(NodeName,State#manager_state.nodes) of
		true ->
			{reply,ok,State};
		false ->
			NewNodes = sets:add_element(NodeName,State#manager_state.nodes),
			erlang:monitor_node(NodeName,true),
			?L_IA("Node ~p is connecting.",[NodeName]),
			Result = rpc:call(NodeName,utils,get_addr2,[]),
			io:format(">>Node ~p at address ~p~n",[NodeName,Result]),
			{reply, ok, State#manager_state{ nodes = NewNodes }}
	end;
handle_call({disconnect,NodeName}, _From, State = #manager_state{}) ->
	case sets:is_element(NodeName,State#manager_state.nodes) of
		false ->
			{reply,ok,State};
		true ->
			NewNodes = sets:del_element(NodeName,State#manager_state.nodes),
			NewStats = maps:remove(NodeName,State#manager_state.stats),
			erlang:monitor_node(NodeName,false),
			?L_IA("Node ~p is disconnecting.",[NodeName]),
			{reply, ok, State#manager_state{ nodes = NewNodes , stats = NewStats }}
	end;
handle_call(connected_nodes, _From, State = #manager_state{}) ->
	{reply,{ok,sets:to_list(State#manager_state.nodes)},State};
handle_call(_Request, _From, State = #manager_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #manager_state{}) ->
	{noreply, NewState :: #manager_state{}} |
	{noreply, NewState :: #manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #manager_state{}}).
handle_cast({stats_report,NodeName,Report},State=#manager_state{})->
	%% io:format("Received stats from ~p.~n",[NodeName]),
	{noreply,State#manager_state{ stats = maps:put(NodeName,Report,State#manager_state.stats)}};
handle_cast({log_info,NodeName,Message}, State = #manager_state{}) ->
	_=lager:info("~p: "++Message,[NodeName]),
	{noreply, State};
handle_cast({log_info,NodeName,Message,Args}, State = #manager_state{}) ->
	_=lager:info("~p: "++Message,[NodeName|Args]),
	{noreply, State};
handle_cast({log_error,NodeName,Message}, State = #manager_state{}) ->
	_=lager:error("~p: "++Message,[NodeName]),
	{noreply, State};
handle_cast({log_error,NodeName,Message,Args}, State = #manager_state{}) ->
	_=lager:error("~p: "++Message,[NodeName|Args]),
	{noreply, State};
handle_cast(_Request, State = #manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #manager_state{}) ->
	{noreply, NewState :: #manager_state{}} |
	{noreply, NewState :: #manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #manager_state{}}).
handle_info({nodedown,Node},State=#manager_state{})->
	io:format("Node ~p is going down.~n",[Node]),
	NewNodes = sets:del_element(Node,State#manager_state.nodes),
	NewStats = maps:remove( Node, State#manager_state.stats),
	{noreply,State#manager_state{ nodes = NewNodes, stats = NewStats }};
handle_info(_Info, State = #manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #manager_state{}) -> term()).
terminate(_Reason, _State = #manager_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #manager_state{},
		Extra :: term()) ->
	{ok, NewState :: #manager_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #manager_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
startdb()->
	_ = case filelib:is_file(filename:join([utils:priv_dir(),"mnesia","schema.DAT"])) of
		    true ->
			    _ = mnesia:start(),
			    ?L_I("Reloading MNESIA.");
		    false ->
			    ?L_I("Starting MNESIA from scratch."),
			    ok=mnesia:create_schema([node()]),
			    _ = mnesia:start(),
			    create_tables()
	    end,
	ok.

create_tables()->
	inventory:create_tables(),
	simengine:create_tables(),
	ok.
