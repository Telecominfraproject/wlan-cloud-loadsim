%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 24. November 2020 @ 13:55:04
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_config).
-author("helge").

-include("../include/common.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	id :: string(),
	serial = "" :: string(),		% serial number of the access point
	type = <<"">> :: binary(),		% device type e.g. EA8300
	tip_host = "" :: string(),			% host of tip controller
	tip_port = 0 :: integer(),			% port at tip controller
	ca_certs = <<"">> :: binary(),		% pem file (in memory) of the server certificate chain
	client_cert = <<"">> :: binary()	% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.


-export_type([cfg/0]).


-export([new/1,configure/2]).
-export ([id/1,tip/2,ca_certs/1,client_cert/1]).


%%------------------------------------------------------------------------------
%% API


-spec new (Id :: string()) -> Config :: cfg().

new (Id) ->
	#cfg{id=Id}.


-spec configure (Manager :: tuple(), Config :: cfg()) -> NewConfig :: cfg().

configure (_Manager,Config) ->
	%% @TODO: remote procisioned configuration
	%% in the meantime read a sample config from a file
	File = filename:join([code:priv_dir(?OWLS_APP),"ovsdb","test_ap.cfg"]),
	{ok, [M]} = file:consult(File),
	APC = maps:get(Config#cfg.id,M),
	Config#cfg{
		serial = proplists:get_value(serial,APC),
		type = proplists:get_value(type,APC),
		tip_host = proplists:get_value(tip_host,APC),
		tip_port = proplists:get_value(tip_port,APC),
		ca_certs = proplists:get_value(ca_certs,APC),
		client_cert = proplists:get_value(client_cert,APC)
	}.


%%------------------------------------------------------------------------------
%% accessor API

-spec id (Config :: cfg()) -> Id :: string().
id (Cfg) -> Cfg#cfg.id.

-spec tip (Part :: host | port, Config :: cfg()) -> string() | integer().
tip (host,Cfg) -> Cfg#cfg.tip_host;
tip (port,Cfg) -> Cfg#cfg.tip_port.

-spec ca_certs (Config :: cfg()) -> binary().
ca_certs (Cfg) -> Cfg#cfg.ca_certs.

-spec client_cert (Config :: cfg()) -> binary().
client_cert (Cfg) -> Cfg#cfg.client_cert.


