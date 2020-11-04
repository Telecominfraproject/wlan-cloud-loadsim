-module(owls_app).
-behaviour(application).

-export([start/2,start/0,load_cli/0]).
-export([stop/1]).

start(_Type, _Args) ->
	load_cli(),
	inets:start(),
	lager:start(),
	application:ensure_all_started(ssl),
	owls_sup:start_link().

stop(_State) ->
	ok.

start() ->
	load_cli(),
	inets:start(),
	lager:start(),
	application:ensure_all_started(ssl),
	application:ensure_all_started(owls).

load_cli()->
	code:purge(user_default),
	code:load_file(user_default).
