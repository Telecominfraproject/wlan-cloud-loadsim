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
-export([start_link/0,creation_info/0,connect/1,disconnect/0,connected_nodes/0]).
-export([log_info/1,log_info/2,log_error/1,log_error/2,report_event/2]).

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

connect(Type)->
	gen_server:call({global,?SERVER},{connect,node(),Type}).

disconnect()->
	gen_server:call({global,?SERVER},{disconnect,node()}).

connected_nodes()->
	gen_server:call({global,?SERVER},connected_nodes).

log_info(Message)->
	gen_server:cast({global,?SERVER},{log_info,node(),Message}).

log_info(Message,Args)->
	gen_server:cast({global,?SERVER},{log_info,node(),Message,Args}).

log_error(Message)->
	gen_server:cast({global,?SERVER},{log_error,node(),Message}).

log_error(Message,Args)->
	gen_server:cast({global,?SERVER},{log_error,node(),Message,Args}).

-spec report_event(Event::atom(),EventData::#{ atom() => term()})->ok.
report_event(Event,EventData)->
	gen_server:cast({global,?SERVER},{event,node(),Event,EventData}).


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
	{ok, #manager_state{ nodes = maps:new(), stats = maps:new() }}.

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
handle_call({connect,NodeName,Type}, _From, State = #manager_state{}) ->
	case maps:is_key(NodeName,State#manager_state.nodes) of
		true ->
			{reply,ok,State};
		false ->
			erlang:monitor_node(NodeName,true),
			Result = rpc:call(NodeName,utils,get_addr,[]),
			NewNodes = maps:put(NodeName,Type,State#manager_state.nodes),
			manager:report_event(node_connect,#{ connecting_node => NodeName, address => Result}),
			?L_IA("Node ~p is connecting (~p).",[NodeName,Type]),
			report_event(nodeup,#{ nodename => NodeName }),
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
			manager:report_event(node_disconnect,#{ disconnecting_node => NodeName }),
			{reply, ok, State#manager_state{ nodes = NewNodes , stats = NewStats }}
	end;
handle_call(connected_nodes, _From, State = #manager_state{}) ->
	{reply,{ok,maps:to_list(State#manager_state.nodes)},State};
handle_call(_Request, _From, State = #manager_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #manager_state{}) ->
	{noreply, NewState :: #manager_state{}} |
	{noreply, NewState :: #manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #manager_state{}}).
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
handle_cast({event,NodeName,Event,EventData}, State = #manager_state{}) ->
	try
		JSON = jiffy:encode( #{ type => event, name => Event , node => NodeName , data => EventData} ),
		web_socket_handler:send_frame( JSON )
%%		_ = file:write_file(filename:join([utils:priv_dir(),"events.json"]),JSON,[append]),
%%		_ = file:write_file(filename:join([utils:priv_dir(),"events.json"]),<<"\n">>,[append]),
%%		_ = file:write_file(filename:join([utils:priv_dir(),"events.json"]),<<"\n">>,[append])
	catch
		_:_ ->
			io:format("Bad event: ~p from node: ~p~nData:~p~n",[Event,NodeName,EventData])
	end,
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
	NewNodes = maps:remove(Node, State#manager_state.nodes),
	NewStats = maps:remove(Node, State#manager_state.stats),
	report_event(nodedown,#{ nodename => Node }),
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
	statistics:create_tables(),
	ok.
