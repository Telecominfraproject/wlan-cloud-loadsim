%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2021, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2021 2:26 p.m.
%%%-------------------------------------------------------------------
-module(tip_stats).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0,creation_info/0,create_sim_report/0,run_sim_report/1,register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3,lookup/1,list_reporters/0]).

-record(tip_stats_state, { last_reports = #{},
                           reporting_pids = #{} }).

%%-define(SERVER, {global,?MODULE}).
%%-define(START_SERVER,{global,?MODULE}).
-define(SERVER, ?MODULE).
-define(START_SERVER,{local,?MODULE}).

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

%%%===================================================================
%%% API
%%%===================================================================
register( SimName, Pid ) ->
	gen_server:call(?SERVER,{register_reporting_pid,SimName,Pid}).

lookup( SimName ) ->
	gen_server:call(?SERVER,{lookup,SimName}).

list_reporters()->
	gen_server:call(?SERVER,list_reporters).


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
	{ok, State :: #tip_stats_state{}} | {ok, State :: #tip_stats_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ok, #tip_stats_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #tip_stats_state{}) ->
	                 {reply, Reply :: term(), NewState :: #tip_stats_state{}} |
	                 {reply, Reply :: term(), NewState :: #tip_stats_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #tip_stats_state{}} |
	                 {noreply, NewState :: #tip_stats_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #tip_stats_state{}} |
	                 {stop, Reason :: term(), NewState :: #tip_stats_state{}}).
handle_call(list_reporters, _From, State = #tip_stats_state{}) ->
	{reply, {ok,State#tip_stats_state.reporting_pids}, State};
handle_call({lookup,SimName}, _From, State = #tip_stats_state{}) ->
	case maps:find(SimName,State#tip_stats_state.reporting_pids,undefined) of
		undefined ->
			{reply,error,State};
		Pid ->
			{reply,{ok,Pid},State}
	end;
handle_call({register_reporting_pid,SimName,Pid}, _From, State = #tip_stats_state{}) ->
	{reply, ok, State#tip_stats_state{ reporting_pids = maps:put( SimName, Pid, State#tip_stats_state.reporting_pids )}};
handle_call(_Request, _From, State = #tip_stats_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #tip_stats_state{}) ->
	{noreply, NewState :: #tip_stats_state{}} |
	{noreply, NewState :: #tip_stats_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #tip_stats_state{}}).
handle_cast(_Request, State = #tip_stats_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #tip_stats_state{}) ->
	{noreply, NewState :: #tip_stats_state{}} |
	{noreply, NewState :: #tip_stats_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #tip_stats_state{}}).
handle_info(_Info, State = #tip_stats_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #tip_stats_state{}) -> term()).
terminate(_Reason, _State = #tip_stats_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #tip_stats_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #tip_stats_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #tip_stats_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_sim_report()->
	% Look at running sims, for each running sim, spawn a thread to process the sim.
	% if there is no running sim_report for the simulation, start one. If there is one,
	% do not run it.
	case simengine:list_simulations() of
		{ ok , [] } ->
			% make sure we come back here again
			timer:apply_after(2000,?MODULE,create_sim_report,[]);
		{ ok , SimulationList } ->
			create_missing_reports(SimulationList);
		_ ->
			timer:apply_after(2000,?MODULE,create_sim_report,[])
	end,
	ok.

run_sim_report(SimName) ->
	% figure out which host is hosting the simulation
	% do calls to get the number of equipment, sessions, etc
	% compute the number for this simulation: APS, clients.
	%
	?MODULE:register(SimName,self()),
	ok.

create_missing_reports([])->
	ok;
create_missing_reports([H|T])->
	case lookup(H) of
		{ok,_Pid} ->
			create_missing_reports(T);
		_ ->
			spawn_link(?MODULE,run_sim_report,[H]),
			create_missing_reports(T)
	end.

