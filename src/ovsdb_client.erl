%%%-------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2020 12:44 p.m.
%%%-------------------------------------------------------------------
-module(ovsdb_client).
-author("helge").

-behaviour(gen_server).

-include("../include/common.hrl").

-define(SERVER, ?MODULE).



%% API
-export([start_link/2]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3]). %, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



%% data structures

-record(client_state, { 
	sim_node :: pid(),  	% controlling simnode
	sim_manager :: pid(), 	% manager to be contacted to get configuration
	serial :: string(),		% serial number of the access point
	type :: binary(),		% device type e.g. EA8300
	timer :: owls_timers:tms(),
	satus = init :: init | ready | running | stopped,	% internal status
	config = #{} :: map(),
	statistics = #{} :: map()
}).




%%%============================================================================
%%% API
%%%============================================================================


-spec start_link(CallbackNode::pid(), Spec::proplists:proplist()) ->
			{ok, Pid :: pid()}.

start_link(Node,Spec) ->
	gen_server:start_link(?MODULE, {Node,Spec}, []).




%%%============================================================================
%%% GEN_SERVER callbacks
%%%============================================================================

-spec init ({Node, Spec}) -> {ok, State}  when
		Node :: pid(),
		Spec :: proplists:proplist(),
		State :: #client_state{}.

init ({Node,Spec}) ->
	InitialState = prepare_state(Node,Spec),
	gen_server:cast(self(),start_up),
	{ok, InitialState}.




-spec handle_cast (Request, State) -> {noreply, NewState} | {stop, Reason, NewState} when
		Request :: term(),
		State :: #client_state{},
		NewState :: #client_state{},
		Reason :: string().

handle_cast (start_up, State) ->
	%TODO: startup
	{noreply, State};

handle_cast (R,State) ->
	?L_E(?DBGSTR("got unknown request: ~p",[R])),
	{noreply, State}.




-spec handle_call (Request, From, State) -> {reply, Reply, NewState} | {stop, Reason, Reply, NewState} when
		Request :: term(),
		From :: {pid(),Tag::term()},
		State :: #client_state{},
		Reply :: term(),
		Reason :: term(),
		NewState :: #client_state{}.

handle_call (Request, From, State) ->
	?L_E(?DBGSTR("got unknow request ~p from ~p",[Request,From])),
	{reply, invalid, State}.





%%%============================================================================
%%% internal functions
%%%============================================================================


%---------prepare_state------------------convert Spec proplist into internal state 

-spec prepare_state (Node, Spec) -> State when
		Node :: pid(),
		Spec :: proplists:proplist(),
		State :: #client_state{}.

prepare_state (Node,Spec) ->
	#client_state{
		sim_node = Node,
		sim_manager = proplists:get_value(manager,Spec,Node),
		serial = proplists:get_value(ap_serial,Spec,"1P000000000"),
		type = proplists:get_value(ap_type,Spec,<<"EA8300">>),
		timer = owls_timers:new(millisecond)
	}.
