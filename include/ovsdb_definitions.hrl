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

-include("../include/inventory.hrl").

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
	start_stamp   = 0 :: integer(),	% os:system_time() in microseconds
	end_stamp     = 0 :: integer(), % evaluation period in ms
	rx_bytes      = 0 :: integer(), % received bytes
	tx_bytes      = 0 :: integer(), % transmitted bytes
	dropped       = 0 :: integer(),	% number of dropped connections
	restarts      = 0 :: integer() % number of resarts
}).

-type ap_statistics()::#ap_statistics{}.

-record(ap_state, {
	id = <<>> ::  binary(),				% the ID of the access point we cary around
	caname = <<>> :: string() | binary(),		% ???
	simname = <<>> ::binary(),
	ovsdb_server_name = <<>> :: binary(),
	ovsdb_server_port = 6643 :: non_neg_integer(),
	original_ovsdb_server_name = <<>> :: binary(),
	original_ovsdb_server_port = 6643 :: non_neg_integer(),
	redirected = false :: boolean(),
	retries = 0 :: non_neg_integer(),
	config,
	socket = none :: none | ssl:sslsocket(),
	details :: #client_info{},
	hardware :: #hardware_info{},
	lan_addr = <<"192.168.1.1">> :: binary(),
	wan_addr = <<>> :: binary(),
	trail_data = <<>> :: binary(),
	manager_pid :: pid(),
	reconnecting = false :: boolean(),
	redirector = <<>> :: binary(),
	manager_addr = <<>> :: binary(),
	monitored_tables = #{} :: #{ TableName::binary() => TableParameters::#{} },
	tables = #{} :: #{ TableName::binary() => term() },
	known_table_names = [] :: [binary()],
	associated_clients = #{} :: #{ Band::wifi_band() => [ MAC::binary()]},
	associated_clients_uids = #{} :: #{ Band::wifi_band() => [ MAC::binary()]},
	normal_reconnect = false :: boolean(),
	ssid = <<>> :: binary(),
	mqtt_config = #{} :: #{},
	mqtt = idle :: idle | running,		% mqtt status (external process)
	status = init :: atom(),		% internal status
	min_backoff = 30 :: non_neg_integer(),
	max_backoff = 60 :: non_neg_integer(),
	reporting = none :: none | timer:tref(),			% statistics reporting interval timer reference
	reconnect_timer = none  :: none | timer:tref(),
	mqtt_update_timer = none :: none | timer:tref(),
	publish_timer = none :: none | timer:tref(),
	echo = 0 :: non_neg_integer(),
	stats = #ap_statistics{} :: ap_statistics(),
	check_monitor_tick = 0 :: non_neg_integer()
}).

-type ap_state()::#ap_state{}.
-export_type([ap_state/0,ap_statistics/0]).

-type ovsdb_client_status() :: ready | paused | running | reconnecting.
-type ovsdb_ap_statistics() :: #ap_statistics{}.
-type ovsdb_client_status_map() :: #{Serial::binary() => ovsdb_client_status()}.

-export_type([ovsdb_client_status/0,ovsdb_ap_statistics/0,ovsdb_client_status_map/0]).

-define(MAX_STARTUP_TIME,10000).
-define(AP_STATS_INTERVAL,1000).
-define(AP_REPORT_INTERVAL,4000).
-define(MGR_REPORT_INTERVAL,10000).

-endif.
