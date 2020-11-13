%%%-------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2020 12:04 p.m.
%%%-------------------------------------------------------------------
-author("helge").

-ifndef(__OVSDBDEFS_HRL__).
-define(__OVSDBDEFS_HRL__,1).

-record( ovsdb_cfg, {
	reflector_port = 6643 :: integer(),
	ovsdb_port = 6640 :: integer(),
	max_clients = 100 :: integer(),
    clint_certs = [] :: list()
}).

-type ovsdb_cfg() :: #ovsdb_cfg{}.




-record( ovsdb_state, {
    config :: ovsdb_cfg(),
    server :: pid(),
    clients = [] :: list()
}).

-type ovsdb_state() :: #ovsdb_state{}.



-define(OVSDB_DEFAULT_REFLECTOR_PORT,6643).
-define(OVSDB_DEFAULT_SERVER_PORT,6640).
-define(OVSDB_DEFAULT_MAX_CLIENTS,100).



-endif.