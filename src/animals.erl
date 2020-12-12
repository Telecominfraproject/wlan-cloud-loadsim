%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 12. Dec 2020 9:27 a.m.
%%%-------------------------------------------------------------------
-module(animals).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0,creation_info/0,get_an_animal/0,get_some_animals/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(animals_state, {animals = [] :: [string()],length=0::integer()}).

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

-spec get_an_animal()-> string().
get_an_animal()->
	gen_server:call(?SERVER,get_an_animal).

-spec get_an_animals( Size::integer())-> string().
get_some_animals(Size)->
	gen_server:call(?SERVER,{get_some_animals,Size}).

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
	{ok, State :: #animals_state{}} | {ok, State :: #animals_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	_ = rand:seed(exs1024s),
	Animals = try
							{ok,A0} = file:read_file(filename:join([utils:priv_dir(),"templates","animals.txt"])),
							A1 = string:split(binary_to_list(A0),"\n",all),
							[ lists:flatten(lists:join("_",string:split(X," ",all))) || X <- A1 ]
						catch
						 _:_ ->
							 ["cat","dog","fish","mouse"]
	         end,
	{ok, #animals_state{ animals = Animals, length = length(Animals)}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #animals_state{}) ->
	                 {reply, Reply :: term(), NewState :: #animals_state{}} |
	                 {reply, Reply :: term(), NewState :: #animals_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #animals_state{}} |
	                 {noreply, NewState :: #animals_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #animals_state{}} |
	                 {stop, Reason :: term(), NewState :: #animals_state{}}).
handle_call(get_an_animal, _From, State = #animals_state{}) ->
	Animal = lists:nth(rand:uniform(State#animals_state.length),State#animals_state.animals),
	{reply, Animal, State};
handle_call({get_some_animals,Size}, _From, State = #animals_state{}) ->
	Fun = fun() -> lists:nth(rand:uniform(State#animals_state.length),State#animals_state.animals) end,
	Animals = apply_ntimes(Size,Fun),
	{reply, Animals, State};
handle_call(_Request, _From, State = #animals_state{}) ->
	{reply, ok, State}.

apply_ntimes(Size,Fun)->
	apply_ntimes(Size,Fun,[]).
apply_ntimes(0,_,Acc)->
	lists:reverse(Acc);
apply_ntimes(Size,Fun,Acc)->
	apply_ntimes(Size-1,Fun,[Fun()|Acc]).

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #animals_state{}) ->
	{noreply, NewState :: #animals_state{}} |
	{noreply, NewState :: #animals_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #animals_state{}}).
handle_cast(_Request, State = #animals_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #animals_state{}) ->
	{noreply, NewState :: #animals_state{}} |
	{noreply, NewState :: #animals_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #animals_state{}}).
handle_info(_Info, State = #animals_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #animals_state{}) -> term()).
terminate(_Reason, _State = #animals_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #animals_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #animals_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #animals_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
