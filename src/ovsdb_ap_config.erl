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


%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	id :: string(),
	serial = "" :: string(),		% serial number of the access point
	type = <<"">> :: binary()		% device type e.g. EA8300
}).

-opaque cfg() :: #cfg{}.


-export_type([cfg/0]).



%%------------------------------------------------------------------------------
%% API

-export([new/1,configure/2,get_id/1]).



-spec new (Id :: string()) -> Config :: cfg().

new (Id) ->
	#cfg{id=Id}.


-spec configure (Manager :: tuple(), Config :: cfg()) -> NewConfig :: cfg().

configure (_Manager,Config) ->
	Config#cfg{}.



-spec get_id (Config :: cfg()) -> Id :: string().

get_id (Cfg) ->
	Cfg#cfg.id.