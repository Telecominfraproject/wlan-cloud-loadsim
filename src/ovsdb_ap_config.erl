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
-include("ovsdb_ap_tables.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	ca_name :: string() | binary(),
	id :: string(),
	store_ref :: ets:tid(),
	ca_certs = <<"">> :: binary(),		% pem file (in memory) of the server certificate chain
	client_cert = <<"">> :: binary()	% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.
-export_type([cfg/0]).


-export([new/3,configure/1]).
-export ([id/1,ca_certs/1,client_cert/1,tip_redirector/2,tip_manager/2]).


%%------------------------------------------------------------------------------
%% API


-spec new (CAName :: string() | binary(), Id :: string(), Store :: ets:tid()) -> Config :: cfg().
new (CAName,Id,Store) ->
	#cfg{ca_name=CAName, id=Id, store_ref = Store}.

-spec configure (Config :: cfg()) -> NewConfig :: cfg().
configure (Config) ->
	%% @TODO: remote provisioned configuration
	%% in the meantime read a sample config from a file
	File = filename:join([code:priv_dir(?OWLS_APP),"ovsdb","test_ap.cfg"]),
	{ok, [M]} = file:consult(File),
	APC = maps:get(Config#cfg.id,M),
	initialize_ap_tables(Config#cfg.store_ref,APC),
	Config#cfg{
		ca_certs = proplists:get_value(ca_certs,APC),
		client_cert = proplists:get_value(client_cert,APC)
	}.

-spec initialize_ap_tables (Store :: ets:tid(), APConfig :: proplists:proplist()) -> true.
initialize_ap_tables (Store, APC) ->
	create_table('AWLAN_Node',APC,Store),
	create_table('Wifi_Radio_State',APC,Store),
	create_table('Wifi_Inet_State',APC,Store).
	
%%------------------------------------------------------------------------------
%% accessor API - direct config settings

-spec id (Config :: cfg()) -> Id :: string().
id (Cfg) ->
	Cfg#cfg.id.

-spec ca_certs (Config :: cfg()) -> binary().
ca_certs (Cfg) ->
	Cfg#cfg.ca_certs.

-spec client_cert (Config :: cfg()) -> binary().
client_cert (Cfg) ->
	Cfg#cfg.client_cert.

%%------------------------------------------------------------------------------
%% accessor API from Store tables

-spec tip_redirector (Part :: host | port, Config :: cfg()) -> string() | integer().
tip_redirector (Part,#cfg{store_ref=Store}) ->
	[#'AWLAN_Node'{redirector_addr=R}|_] = ets:lookup(Store,'AWLAN_Node'),
	get_host_or_port(Part,R).

-spec tip_manager (Part :: host | port, Config :: cfg()) -> string() | integer().
tip_manager (Part,#cfg{store_ref=Store}) ->
	[#'AWLAN_Node'{manager_addr=R}|_] = ets:lookup(Store,'AWLAN_Node'),
	get_host_or_port(Part,R).

-spec get_host_or_port (Part :: host | port, Addr :: binary()) -> string() | integer().
get_host_or_port (Part, Addr) when is_binary(Addr) ->
	Parts = string:split(Addr,":",all),
	case Part of
		host -> case Parts of
					[_,H,_] -> binary_to_list(H);
					[H,_]   -> binary_to_list(H);
						  _ -> ""
				end;
		port -> case Parts of
					[_,_,P] -> binary_to_integer(P);
					[_,P]   -> binary_to_integer(P);
						  _ -> 0
				end
	end.

%%------------------------------------------------------------------------------
%% table creation

-spec create_table (Table :: atom(), AP_Config :: [{atom(),term()}], Store :: ets:tid()) -> true.
create_table ('Wifi_Radio_State',_APC,Store) ->
	ets:insert(Store, #'Wifi_Radio_State'{
		row_idx = 0,
		freq_band = <<"5GU">>,
		if_name = <<"radio0">>
	}),
	ets:insert(Store, #'Wifi_Radio_State'{
		row_idx = 1,
		freq_band = <<"2.4G">>,
		if_name = <<"radio1">>
	}),
	ets:insert(Store, #'Wifi_Radio_State'{
		row_idx = 2,
		freq_band = <<"5GL">>,
		if_name = <<"radio2">>
	});

create_table ('Wifi_Inet_State',APC,Store) -> 
	ets:insert(Store, #'Wifi_Inet_State'{
		row_idx = 0,
		inet_addr = proplists:get_value(wan_addr,APC),
		hwaddr = proplists:get_value(wan_mac,APC),
		if_name = <<"wan">>,
		if_type = <<"bridge">>
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		row_idx = 1,
		inet_addr = proplists:get_value(lan_addr,APC),
		hwaddr = proplists:get_value(lan_mac,APC),
		if_name = <<"lan">>,
		if_type = <<"bridge">>
	});

create_table ('AWLAN_Node',APC,Store) -> 
	ets:insert(Store, #'AWLAN_Node'{
		row_idx = 0,
		redirector_addr = proplists:get_value(tip_redirector,APC),									
		serial_number = proplists:get_value(serial,APC),
		model = proplists:get_value(type,APC),
		revision = <<"1">>,
		platform_version = <<"OPENWRT_EA8300">>,
		firmware_version = <<"0.1.0">>,
		version_matrix = [<<"map">>,[
							[<<"DATE">>,<<"Mon Nov  2 09">>],
							[<<"FIRMWARE">>,<<"0.1.0-0-notgit-development">>],
							[<<"FW_BUILD">>,<<"0">>],
							[<<"FW_COMMIT">>,<<"notgit">>],
							[<<"FW_IMAGE_ACTIVE">>,<<"ea8300-2020-11-02-pending-97ebe9d">>],
							[<<"FW_IMAGE_INACTIVE">>,<<"unknown">>],
							[<<"FW_PROFILE">>,<<"development">>],
							[<<"FW_VERSION">>,<<"0.1.0">>],
							[<<"HOST">>,<<"runner@72477083da86">>],
							[<<"OPENSYNC">>,<<"2.0.5.0">>],
							[<<"core">>,<<"2.0.5.0/0/notgit">>],
							[<<"vendor/tip">>,<<"0.1.0/0/notgit">>]
						 ]]
	}).
