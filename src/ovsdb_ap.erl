%%%----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2020 12:44 p.m.
%%%----------------------------------------------------------------------------
-module(ovsdb_ap).
-author("helge").

-behaviour(gen_server).

-include("../include/common.hrl").

-define(SERVER, ?MODULE).



%%-----------------------------------------------------------------------------
%% @doc The SPEC for starting a AP node is a proplist with the following fileds.
%%
%% {manager, ::pid()} - the pid of the simmanager used to get configuration.
%% {ap_serial, ::string()} - the serial number of the AP.
%% {ap_type, ::binary()} - type of AP to simulate e.g. EA8300.
%% {uuid, ::term()} - arbitrary unique id to identify this simnode AP.
%% @end
%%-----------------------------------------------------------------------------


%% API
-export([start/2]).
-export([uuid/1]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).



%% data structures

-record(ap_state, { 
	sim_node :: pid(),  	% controlling simnode
	sim_manager :: pid(), 	% manager to be contacted to get configuration
	serial :: string(),		% serial number of the access point
	type :: binary(),		% device type e.g. EA8300
	uuid :: term(),			% unique ID for this AP node
	timer :: owls_timers:tms(),
	status = init :: init | ready | running | stopped,	% internal status
	config = #{} :: map(),
	statistics = #{} :: map()
}).




%%%============================================================================
%%% API
%%%============================================================================


-spec start (Handler, Spec) -> {ok, Pid} | {error, Reason} when
		Handler :: pid(),
		Spec :: proplists:proplist(),
		Pid :: pid(),
		Reason :: term().

start (Handler, Spec) ->
	gen_server:start(?MODULE, {Handler, Spec}, []).




-spec uuid (Node) -> Uuid when
		Node :: pid(),
		Uuid :: term().

uuid (Node) ->
	gen_server:call(Node,ap_uuid).




%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init ({Handler, Spec}) -> {ok, State}  when
		Handler :: pid(),
		Spec :: proplists:proplist(),
		State :: #ap_state{}.

init ({Handler,Spec}) ->
	process_flag(trap_exit, true),
	InitialState = prepare_state(Handler,Spec),
	gen_server:cast(self(),start_up),
	{ok, InitialState}.




-spec handle_cast (Request, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Request :: term(),
		State :: #ap_state{},
		NewState :: #ap_state{},
		Reason :: string().

handle_cast (start_up, State) ->
	{ok, NewState} = startup_ap(State),
	State#ap_state.sim_node ! {ready, {self(), State#ap_state.uuid}},
	{noreply, NewState};

handle_cast (R,State) ->
	?L_E(?DBGSTR("got unknown request: ~p",[R])),
	{noreply, State}.




-spec handle_call (Request, From, State) -> {reply, Reply, NewState} | {stop, Reason, Reply, NewState} when
		Request :: term(),
		From :: {pid(),Tag::term()},
		State :: #ap_state{},
		Reply :: term(),
		Reason :: term(),
		NewState :: #ap_state{}.

handle_call (ap_uuid,_,State) ->
	{reply, State#ap_state.uuid, State};

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.




-spec handle_info (Msg, State) -> {noreply, NewState} when
		Msg :: term(),
		State :: #ap_state{},
		NewState :: #ap_state{}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
	%% @TODO: implement proper restart strategy for linked processes
	{noreply, State};


handle_info (Msg,State) ->
	?L_E(?DBGSTR("got unexpected info message ~p",[Msg])),
	{noreply, State}.




-spec terminate (Reason, State) -> ok when
		Reason :: shutdown | normal,
		State :: #ap_state{}.

terminate (_Reason, _State) ->
	ok.




-spec code_change (OldVersion, OldState, Extra) -> {ok, NewState} when
		OldVersion :: term(),
		OldState ::#ap_state{},
		Extra :: term(),
		NewState :: #ap_state{}.

code_change (_,OldState,_) ->
	?L_E(?DBGSTR("code change requested")),
	{ok, OldState}.




%%%============================================================================
%%% internal functions
%%%============================================================================


%---------prepare_state/2----------------convert Spec proplist into internal state 

-spec prepare_state (Node, Spec) -> State when
		Node :: pid(),
		Spec :: proplists:proplist(),
		State :: #ap_state{}.

prepare_state (Node,Spec) ->
	#ap_state{
		sim_node = Node,
		sim_manager = proplists:get_value(manager,Spec,Node),
		serial = proplists:get_value(ap_serial,Spec,"1P000000000"),
		type = proplists:get_value(ap_type,Spec,<<"EA8300">>),
		uuid = proplists:get_value(uuid,Spec,erlang:phash2({node(),erlang:system_time()})),
		timer = owls_timers:new(millisecond)
	}.




%--------startup_ap/1--------------------initiate startup sequence
-spec startup_ap (State) -> {ok, NewState} | {error, Reason} when
		State :: #ap_state{},
		NewState :: #ap_state{},
		Reason :: string().

startup_ap (#ap_state{status=init}=State) ->
	{ok,State#ap_state{status=ready}};

startup_ap (_) ->
	{error,"not in initial state, cant start-up"}.