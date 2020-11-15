%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 3:11 p.m.
%%%-------------------------------------------------------------------
-module(user_default).
-author("stephb").

-include("../include/internal.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).

-define(SIM_APIKEY,sim_api_key).


-spec help() -> ok.
help()->
	case application:get_env(?OWLS_APP,role,none) of
		none ->
			io:format("Invalid application role. Please verify your configuration.~n");
		node ->
			node_help();
		manager ->
			manager_help()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Node commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec node_help() -> ok.
node_help()->
	io:format("login(ApiKey).    ApiKey ,ust be a string.~n"),
	io:format("logout().~n"),
	io:format("connect(Node).    Node must be a string.~n"),
	io:format("connected().~n").

-spec login( ApiKey :: string()) -> ok.
login(ApiKey)->
	persistent_term:put(?SIM_APIKEY,ApiKey).

-spec logout() -> ok.
logout()->
	persistent_term:erase(?SIM_APIKEY),
	ok.

-spec connected() -> { ok , none } | { ok , Manager::node() }.
connected()->
	simnode:connected().

-spec connect(NodeName::string()) -> ok | { error , Reason::atom() }.
connect(NodeName) ->
	Node = list_to_atom(NodeName),
	simnode:connect(Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Manager commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec manager_help() -> ok.
manager_help()->
	io:format("refresh_ouis().~n"),
	io:format("connected_nodes().~n").

-spec refresh_ouis()-> ok.
refresh_ouis()->
	oui_server:refresh().

-spec connected_nodes() -> {ok,[node()]}.
connected_nodes()->
	manager:connected_nodes().

