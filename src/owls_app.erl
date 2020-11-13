-module(owls_app).
-behaviour(application).

-export([start/2,start/0,load_cli/0]).
-export([stop/1]).

start(_Type, _Args) ->
	app_settings(),
	application:ensure_all_started(ssl),
	owls_sup:start_link().

stop(_State) ->
	ok.

start() ->
	app_settings(),
	application:ensure_all_started(ssl),
	application:ensure_all_started(owls).

load_cli()->
	code:purge(user_default),
	code:load_file(user_default).

app_settings()->
	load_cli(),
	inets:start(),
	lager:start(),
	application:start(sasl),
	application:start(os_mon),
	disksup:set_check_interval(5),
	disksup:set_almost_full_threshold(0.90).
