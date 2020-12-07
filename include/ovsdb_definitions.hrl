%%%-------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
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
    server = undefined :: undefined | pid(),
    clients = [] :: list()
}).

-type ovsdb_state() :: #ovsdb_state{}.
-export_type([ovsdb_state/0]).

-define(OVSDB_DEFAULT_REFLECTOR_PORT,6643).
-define(OVSDB_DEFAULT_SERVER_PORT,6640).
-define(OVSDB_DEFAULT_MAX_CLIENTS,100).


-record (ap_statistics, {
	stamp :: integer(),		% erlang system time
	interval :: integer(),  % evaluation period in ms
	rx_bytes :: integer(),  % received bytes
	tx_bytes :: integer(),  % transmitted bytes
	rx_bps :: float(),		% received bytes per sec.
	tx_bps :: float(),		% transmitted bytes per sec.
	dropped :: integer(),	% number of dropped connections
	restarts :: integer(),  % number of resarts 
	latency :: integer()	% network latency from HB in ms		
}).


-type client_status() :: available | dead | ovsdb_ap:ap_status().

-record (ap_client, {						
	id :: ets_dont_care() | binary() | string(),				% this is the index position and must be there
	ca_name :: ets_dont_care() | string() | binary(),
	redirector = <<"">> :: ets_dont_care() | binary(),
	status :: ets_dont_care() | client_status(),
	process :: ets_dont_care() | none | pid(),
	transitions :: ets_dont_care() | [{client_status(), TimeStamp::integer()}]
}).


-define(MAX_STARTUP_TIME,10000).
-define(AP_STATS_INTERVAL,1000).
-define(AP_REPORT_INTERVAL,4000).
-define(MGR_REPORT_INTERVAL,10000).


-endif.
