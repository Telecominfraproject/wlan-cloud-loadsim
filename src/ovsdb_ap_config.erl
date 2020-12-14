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
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/inventory.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	ca_name :: string() | binary(),
	redirector :: binary(),
	id :: binary(),
	store_ref :: ets:tid(),
	cacert    = <<>> :: binary(),		% pem file (in memory) of the server certificate chain
	cert      = <<>> :: binary(),
	key       = {none,<<>>} :: {atom(), binary()}% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.
-export_type([cfg/0]).


-export([new/4,configure/1]).
-export ([id/1,ca_certs/1,client_cert/1,client_key/1,tip_redirector/2,tip_manager/2]).


%%------------------------------------------------------------------------------
%% API


-spec new (CAName :: string() | binary(), Id :: binary(), Store :: ets:tid(), Redirector :: binary()) -> Config :: cfg().
new (CAName,Id,Store,Redirector) ->
	#cfg{ca_name=CAName, id=Id, store_ref = Store, redirector=Redirector}.

-spec configure (Config :: cfg()) -> NewConfig :: cfg().
configure (#cfg{ca_name=CAName, id=ID, redirector=R}=Config) ->
	{ok,Info} = inventory:get_client(CAName,ID),
	APC = [
		{serial,Info#client_info.serial},
		{type,Info#client_info.type},
		{wan_addr,make_ip_addr(ID)},
		{wan_mac,Info#client_info.wan_mac0},
		{lan_addr,<<"192.168.1.1">>},
		{lan_mac,Info#client_info.lan_mac0},
		{tip_redirector,R}
		% {serial,<<"21P10C69717951">>},
		% {type,<<"EA8300">>},
		% {wan_addr,<<"10.20.0.113">>},
		% {wan_mac,<<"58:ef:68:62:e7:f1">>},
		% {lan_addr,<<"192.168.1.1">>},
		% {lan_mac,<<"58:ef:68:62:e7:f0">>},
		% {tip_redirector,<<"ssl:opensync-controller.wlan.local:6643">>}
	],
	initialize_ap_tables(Config#cfg.store_ref,validate_config(APC)),
	Config#cfg{
		cacert    = Info#client_info.cacert,
		cert = Info#client_info.cert,
		key  = Info#client_info.key
	}.

-spec validate_config(APC :: [{atom(),term()}]) -> CorrAPC :: [{atom(),term()}].
validate_config (APC) ->
	File = filename:join([utils:priv_dir(),"templates","default_ap.cfg"]),
	{ok, [Defaults]} = file:consult(File),
	F = fun({K,V}) ->
		case V of 
			<<"">> ->
				{K,proplists:get_value(K,Defaults)};
			_ ->
				{K,V}
		end
	end,
	[F(X)||X<-APC].


-spec make_ip_addr(ID::binary()) -> IPAddr :: binary().
make_ip_addr(_ID) ->
	A = rand:uniform(30) + 60,
	B = rand:uniform(200) + 20,
	C = rand:uniform(50) + 100,
	D = rand:uniform(230) + 10,
	list_to_binary(io_lib:format("~B.~B.~B.~B",[A,B,C,D])).

-spec initialize_ap_tables (Store :: ets:tid(), Config :: proplists:proplist()) -> true.
initialize_ap_tables (Store, APC) ->
	create_table('AWLAN_Node',APC,Store),
	create_table('Wifi_Radio_State',APC,Store),
	create_table('Wifi_Inet_State',APC,Store).
	
%%------------------------------------------------------------------------------
%% accessor API - direct config settings

-spec id (Config :: cfg()) -> Id :: binary().
id(Cfg) ->
	Cfg#cfg.id.

-spec ca_certs (Config :: cfg()) -> binary().
ca_certs (Cfg) ->
	Cfg#cfg.cacert.

-spec client_cert (Config :: cfg()) -> binary().
client_cert (Cfg) ->
	Cfg#cfg.cert.

-spec client_key (Config :: cfg()) -> {atom(),binary()}.
client_key (Cfg) ->
	Cfg#cfg.key.


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
		if_name = <<"radio0">>,
		allowed_channels = [<<"set">>,[100,104,108,112,116,120,124,128,132,136,140,144,149,153,157,161,165]]
	}),
	ets:insert(Store, #'Wifi_Radio_State'{
		row_idx = 1,
		freq_band = <<"2.4G">>,
		if_name = <<"radio1">>,
		allowed_channels = [<<"set">>,[1,2,3,4,5,6,7,8,9,10,11]]
	}),
	ets:insert(Store, #'Wifi_Radio_State'{
		row_idx = 2,
		freq_band = <<"5GL">>,
		if_name = <<"radio2">>,
		allowed_channels = [<<"set">>,[36,40,44,48,52,56,60,64]]
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
