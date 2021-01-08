%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Dec 2020 10:37 p.m.
%%%-------------------------------------------------------------------
-module(statistics).
-author("stephb").

-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/statistics.hrl").

%% API
-export([start_link/0,creation_info/0,submit_report/2,create_tables/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(statistics_state, { last_reports = #{} }).

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

submit_report(Type,Report)->
	io:format(">STATS submitting report~n"),
	gen_server:cast(?SERVER,{stats_report,node(),Type,Report}).

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
	{ok, State :: #statistics_state{}} | {ok, State :: #statistics_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #statistics_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #statistics_state{}) ->
	                 {reply, Reply :: term(), NewState :: #statistics_state{}} |
	                 {reply, Reply :: term(), NewState :: #statistics_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #statistics_state{}} |
	                 {noreply, NewState :: #statistics_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #statistics_state{}} |
	                 {stop, Reason :: term(), NewState :: #statistics_state{}}).
handle_call(_Request, _From, State = #statistics_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #statistics_state{}) ->
	{noreply, NewState :: #statistics_state{}} |
	{noreply, NewState :: #statistics_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #statistics_state{}}).
handle_cast({stats_report,NodeName,Type,Report},State=#statistics_state{})->
%%	io:format("Received ~p stats from ~p.~n",[Type,NodeName]),
	_=add_new_report(NodeName,Type,Report),
	{noreply,State#statistics_state{ last_reports = maps:put({NodeName,Type},Report,State#statistics_state.last_reports)}};
handle_cast(_Request, State = #statistics_state{}) ->
	{noreply,State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #statistics_state{}) ->
	{noreply, NewState :: #statistics_state{}} |
	{noreply, NewState :: #statistics_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #statistics_state{}}).
handle_info(_Info, State = #statistics_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #statistics_state{}) -> term()).
terminate(_Reason, _State = #statistics_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #statistics_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #statistics_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #statistics_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_tables()->
	{atomic,ok} = mnesia:create_table(stats,    [{disc_copies,[node()]}, {record_name,stat_report}, {index,[node,timestamp]}, {attributes,record_info(fields,stat_report)}]),
	ok.

-spec add_new_report(Node::node(),Type::atom(),Report::#{})->ok.
add_new_report(Node,Type,Report)->
	_ = mnesia:transaction( fun() ->
												mnesia:dirty_write(stats,#stat_report{ uuid = utils:uuid_b(),
												                                       timestamp = erlang:timestamp(),
												                                       node = Node,
												                                       type = Type,
												                                       report = Report })
	                    end ),
	try
	  JSON = jiffy:encode( #{ type => report, name => Type , node => Node, data => Report } ),
		web_socket_handler:send_frame( JSON ),
	  io:format("NODE: ~p stats.~n",[Node])
%%	  _ = file:write_file(filename:join([utils:priv_dir(),"stats.json"]),JSON,[append]),
%%	  _ = file:write_file(filename:join([utils:priv_dir(),"stats.json"]),<<"\n">>,[append]),
%%	  _ = file:write_file(filename:join([utils:priv_dir(),"stats.json"]),<<"\n">>,[append])
	catch
		_:_ ->
			io:format("FAILED REPORT: ~p~n ~p~n",[Type,Report])
	end,
	ok.
