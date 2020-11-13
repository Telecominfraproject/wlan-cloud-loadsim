-module(ovsdb_server).
-author("helge").

-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/ovsdb_definitions.hrl").

%% API
-export([start_link/0,creation_info/0]).


%% OVSDB_SERVER API

%-export ([]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,code_change/3]).


-define(SERVER, ?MODULE).



%==============================================================================
% OSVDB specific API



%==============================================================================
% API

-spec creation_info () -> Info when
            Info :: [#{}].

creation_info () ->
	[
        #{	
            id => ?MODULE ,
		    start => { ?MODULE , start_link, [] },
		    restart => permanent,
		    shutdown => 100,
		    type => worker,
		    modules => [?MODULE]
        } 
    ].




-spec start_link () -> {ok, Pid} | ignore | {error, Reason} when
            Pid :: pid(),
            Reason :: string().

start_link () ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).




-spec init(Args) -> {ok, State} | {ok, State, timeout() | hibernate} | {stop, Reason} | ignore when
            Args :: term(),
            State :: #ovsdb_state{},
            Reason :: string().

init([]) ->
    Cfg = create_config(),
	{ok, #ovsdb_state{config = Cfg}}.


%----------SYNC CALLS----------------------------------------------------------

-spec handle_call (Request,From,State) -> {reply, Result, State} when
            Request :: term(),
            From :: term(),
            State :: #{},
            Result :: ok.

handle_call(_, _, State) ->
	{reply, ok, State}.




%--------ASYNC CALLS-----------------------------------------------------------

handle_cast(_Request, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.






%==============================================================================
% internals

-spec create_config () -> Config when
            Config :: ovsdb_cfg().

create_config () ->
    ?DBGTRC("creating configuration"),
    Rport = application:get_env(?OWLS_APP,osvdb_rport,?OVSDB_DEFAULT_REFLECTOR_PORT),
    Port = application:get_env(?OWLS_APP,osvdb_port,?OVSDB_DEFAULT_SERVER_PORT),
    MaxClients = application:get_env(?OWLS_APP,osvdb_max_clients,?OVSDB_DEFAULT_MAX_CLIENTS),
    #ovsdb_cfg{reflector_port = Rport, ovsdb_port = Port, max_clients = MaxClients}.