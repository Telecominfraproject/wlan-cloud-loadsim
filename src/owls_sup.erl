%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2020 11:28 p.m.
%%%-------------------------------------------------------------------
-module(owls_sup).
-behaviour(supervisor).

-include("../include/common.hrl").
-include_lib("../deps/lager/include/lager.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Processes = case utils:app_env(role,undefined) of
		manager ->
			?L_I("Simulation Manager starting."),
			node_finder:creation_info() ++
			manager:creation_info() ++
			manager_rest_api:creation_info() ++
			oui_server:creation_info() ++
			hardware:creation_info() ++
			inventory:creation_info() ++
      simengine:creation_info();
		node ->
			?L_I("Simulation Node starting."),
			mqtt_client_manager:creation_info() ++
			simnode:creation_info() ++
      node_rest_api:creation_info();
		undefined ->
			lager:error("No role has been defined in configuration (must be manager or node)")
	end,
	{ok, {{one_for_one, 1, 5}, Processes}}.
