-module(mqttsim_app).
-behaviour(application).

-export([start/2,start/0]).
-export([stop/1]).

start(_Type, _Args) ->
	inets:start(),
	lager:start(),
	application:ensure_all_started(ssl),
	mqttsim_sup:start_link().

stop(_State) ->
	ok.

start() ->
	inets:start(),
	lager:start(),
	application:ensure_all_started(ssl),
	application:ensure_all_started(mqttsim).