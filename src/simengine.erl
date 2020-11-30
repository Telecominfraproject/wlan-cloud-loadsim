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
-include("../include/simengine.hrl").

-compile([{parse_transform, rec2json}]).

%% API
-export([start_link/0,creation_info/0,create/1,create_tables/0,get/1,list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(simengine_state, {}).

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

-spec get(SimName::string()) -> {ok,simulation()} | generic_error().
get(SimName) when is_list(SimName)->
	gen_server:call(?SERVER,{get,list_to_binary(SimName)}).

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
	case create_sim(SimInfo) of
		ok -> {reply, ok, State};
		Error -> { reply, {error,Error} , State}
	end;
handle_call({get,SimName}, _From, State = #simengine_state{}) ->
	case get_sim(SimName) of
		{ok,[Record]} -> { reply, {ok, Record}, State };
		Error-> { reply, {error, Error}, State }
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


