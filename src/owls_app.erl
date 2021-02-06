%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2020 11:28 p.m.
%%%-------------------------------------------------------------------
-module(owls_app).
-behaviour(application).

-include("../include/common.hrl").

-export([start/2,start/0,load_cli/0]).
-export([stop/1]).

start(_Type, _Args) ->
	ok = app_settings(),
	owls_sup:start_link().

stop(_State) ->
	ok.

start() ->
	ok = app_settings(),
	case init:get_argument(sim) of
		{ok,[[Script]]} ->
			timer:apply_after(30*1000,user_default,run_script,[Script]);
		_ ->
			ok
	end,
	application:ensure_all_started(owls).

load_cli()->
	code:purge(user_default),
	_=code:load_file(user_default),
	ok.

app_settings()->
	utils:app_name(?OWLS_APP),
	lager:start(),
	ok = load_cli(),
	_ = inets:start(),
	_ = application:start(sasl),
	_ = application:start(os_mon),
	disksup:set_check_interval(5),
	disksup:set_almost_full_threshold(0.90),
	_ = application:ensure_all_started(ssl),
	_ = application:start(yamerl),
	% _ = application:start(cecho),
	ok.
