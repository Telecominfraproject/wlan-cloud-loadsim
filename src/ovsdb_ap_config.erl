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
		{tip_redirector,R},
		{wifi_clients,Info#client_info.wifi_clients}
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
	create_table('Wifi_Radio_Config',APC,Store),
	create_table('Wifi_Radio_State',APC,Store),
	create_table('Wifi_Inet_Config',APC,Store),
	create_table('Wifi_Inet_State',APC,Store),
	create_table('Wifi_Associated_Clients',APC,Store).
	
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


modify_mac (MAC) ->
	[_,_|F] = lists:reverse(string:split(MAC,":",all)),
	HD1 = list_to_binary(integer_to_list(rand:uniform(255),16)),
	HD2 = list_to_binary(integer_to_list(rand:uniform(255),16)),
	list_to_binary(lists:join(<<":">>,lists:reverse([HD1,HD2|F]))).

%%------------------------------------------------------------------------------
%% table creation



-spec create_table (Table :: atom(), AP_Config :: [{atom(),term()}], Store :: ets:tid()) -> true.
create_table ('Wifi_Radio_State',APC,Store) ->
	ets:insert(Store, #'Wifi_Radio_State'{
		key_id = utils:uuid_b(),
		if_name = <<"radio0">>,
		mac = modify_mac(proplists:get_value(lan_mac,APC)),
		bcn_int = 100,
		allowed_channels = [<<"set">>,[100,104,108,112,116,120,124,128,132,136,140,144,149,153,157,161,165]],
		radio_config = [<<"uuid">>,<<"830bd195-7114-4e99-9b51-5622e47ce221">>],
		vif_states = [<<"uuid">>,<<"87f75538-67d0-408a-9c8b-018665754d48">>],
		country = <<"US">>,
		radar = [<<"map">>,[]],
		tx_chainmask = 3,
		channel = 149,
		tx_power = 18,
		ht_mode = <<"HT80">>,
		hw_mode = <<"11ac">>,
		enabled = true,
		'_version' = [<<"uuid">>,<<"c325d603-ac42-43b5-a2e0-0b65c73888c6">>],
		freq_band = <<"5GU">>
	}),
	ets:insert(Store, #'Wifi_Radio_State' {
		key_id = utils:uuid_b(),
		if_name = <<"radio1">>,
		mac = modify_mac(proplists:get_value(lan_mac,APC)),
		bcn_int = 100,
		allowed_channels = [<<"set">>,[1,2,3,4,5,6,7,8,9,10,11]],
		radio_config = [<<"uuid">>,<<"fb11d840-cbe9-4e32-9744-ebcda9162e52">>],
		vif_states = [<<"set">>,[]],
		hw_config = [<<"map">>,[]],
		country = <<"US">>,
		radar = [<<"map">>,[]],
		tx_chainmask = 3,
		channel = 6,
		tx_power = 18,
		ht_mode = <<"HT80">>,
		hw_mode = <<"11n">>,
		enabled = true,
		'_version' = [<<"uuid">>,<<"0b76545e-c106-41d4-aed2-3d89812f3a11">>],
		freq_band = <<"2.4G">>
	}),
	ets:insert(Store, #'Wifi_Radio_State'{
		key_id = utils:uuid_b(),
		if_name = <<"radio2">>,
		mac = modify_mac(proplists:get_value(lan_mac,APC)),
		bcn_int = 100,
		allowed_channels = [<<"set">>,[36,40,44,48,52,56,60,64]],
		radio_config = [<<"uuid">>,<<"94f9b810-8c71-4961-a9c0-7f3a96869368">>],
		vif_states = ["set",[]],
		country = <<"US">>,
		radar = [<<"map">>,[]],
		tx_chainmask = 3,
		channel = 36,
		tx_power = 18,
		ht_mode = <<"HT80">>,
		hw_mode = <<"11ac">>,
		enabled = true,
		'_version' = [<<"uuid">>,<<"86116d0d-19fc-47db-be17-8eac2ff9bda7">>],
		freq_band = <<"5GL">>
	});

create_table ('Wifi_Radio_Config',_APC,Store) ->
	ets:insert(Store, #'Wifi_Radio_Config'{
		key_id = <<"830bd195-7114-4e99-9b51-5622e47ce221">>,
		'_uuid' = [<<"uuid">>, <<"830bd195-7114-4e99-9b51-5622e47ce221">>],
		freq_band = <<"5GU">>,
		if_name = <<"radio0">>

	}),
	ets:insert(Store, #'Wifi_Radio_Config'{
		key_id = <<"94f9b810-8c71-4961-a9c0-7f3a96869368">>,
		'_uuid' = [<<"uuid">>, <<"94f9b810-8c71-4961-a9c0-7f3a96869368">>],
		freq_band = <<"5GL">>,
		if_name = <<"radio2">>
	}),
	ets:insert(Store, #'Wifi_Radio_Config'{
		key_id = <<"fb11d840-cbe9-4e32-9744-ebcda9162e52">>,
		'_uuid' = [<<"uuid">>, <<"fb11d840-cbe9-4e32-9744-ebcda9162e52">>],
		freq_band = <<"2.4G">>,
		if_name = <<"radio1">>
	});

create_table ('Wifi_Inet_State',APC,Store) -> 
	ets:insert(Store, #'Wifi_Inet_State'{
		key_id = utils:uuid_b(),
		if_name= <<"wwan">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"0b10958d-9bfb-45e5-9c36-ad8327750607">>],
		inet_config = [<<"uuid">>,<<"7e38a63b-526a-4b83-b30e-edd4c17ab3f6">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		key_id = utils:uuid_b(),
		dhcpd = [<<"map">>,[[<<"lease_time">>,<<"12h">>],[<<"start">>,<<"100">>],[<<"stop">>,<<"150">>]]],
		if_name= <<"lan">>,
		if_type = <<"bridge">>,
		enabled = true,
		netmask = <<"255.255.255.0">>,
		inet_addr = proplists:get_value(lan_addr,APC),
		'_version' = [<<"uuid">>,<<"6237745e-3a4d-41a3-858d-7cbce39f5b8c">>],
		hwaddr = proplists:get_value(lan_mac,APC),
		network = true,
		mtu = 1500,
		ip_assign_scheme = <<"static">>,
		inet_config = [<<"uuid">>,<<"19484645-8519-4bd0-98dd-13f1fec83395">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		key_id = utils:uuid_b(),
		if_name= <<"wan6">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"ac171d81-5e5f-41a9-aa71-44a11bb2f72b">>],
		inet_config = [<<"uuid">>,<<"b803af39-e392-437b-8c86-dd87d24f8b49">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		key_id = utils:uuid_b(),
		if_name= <<"wan">>,
		if_type = <<"bridge">>,
		enabled = true,
		netmask = <<"255.255.255.0">>,
		'NAT' = true,
		inet_addr = proplists:get_value(wan_addr,APC),
		'_version' = [<<"uuid">>,<<"325acfc1-ca59-4cbe-8316-7ed307663881">>],
		hwaddr = proplists:get_value(wan_mac,APC),
		network = true,
		mtu = 1500,
		dns = [<<"map">>,[[<<"primary">>,<<"10.20.0.1">>]]],
		ip_assign_scheme = <<"dhcp">>,
		gateway = <<"10.20.0.1">>,
		inet_config = [<<"uuid">>,<<"1a533ecc-90d7-499e-a76c-0d593a446fdb">>]
	});

create_table ('Wifi_Inet_Config',APC,Store) -> 
	ets:insert(Store, #'Wifi_Inet_Config'{
		key_id = <<"1a533ecc-90d7-499e-a76c-0d593a446fdb">>,
		'_uuid' = [<<"uuid">>, <<"1a533ecc-90d7-499e-a76c-0d593a446fdb">>],
		dhcpd = [<<"map">>,[]],
		if_name = <<"wan">>,
		mtu = [<<"set">>,[]],
		network = true,
		dns = [<<"map">>,[]],
		if_type = <<"bridge">>,
		broadcast = [<<"set">>,[]],
		enabled = true,
		vlan_id = [<<"set">>,[]],
		netmask = [<<"set">>,[]],
		gateway = [<<"set">>,[]],
		'NAT' = true,
		ip_assign_scheme = <<"dhcp">>,
		inet_addr = [<<"set">>,[]]
	}),
	ets:insert(Store, #'Wifi_Inet_Config'{
		key_id = <<"b803af39-e392-437b-8c86-dd87d24f8b49">>,
		'_uuid' = [<<"uuid">>, <<"b803af39-e392-437b-8c86-dd87d24f8b49">>],
		if_name = <<"wan6">>,
		network = true,
		if_type = <<"bridge">>,
		enabled = true,
		'NAT' = false
	}),
	ets:insert(Store, #'Wifi_Inet_Config'{
		key_id = <<"7e38a63b-526a-4b83-b30e-edd4c17ab3f6">>,
		'_uuid' = [<<"uuid">>, <<"7e38a63b-526a-4b83-b30e-edd4c17ab3f6">>],
		if_name = <<"wwan">>,
		network = true,
		if_type = <<"eth">>,
		enabled = true,
		'NAT' = false,
		ip_assign_scheme = <<"dhcp">>
	}),
	ets:insert(Store, #'Wifi_Inet_Config'{
		key_id = <<"19484645-8519-4bd0-98dd-13f1fec83395">>,
		'_uuid' = [<<"uuid">>, <<"19484645-8519-4bd0-98dd-13f1fec83395">>],
		dhcpd = [<<"map">>,[[<<"lease_time">>,<<"12h">>],[<<"start">>,<<"100">>],[<<"stop">>,<<"150">>]]],
		if_name = <<"lan">>,
		network = true,
		if_type = <<"bridge">>,
		enabled = true,
		netmask = <<"255.255.255.0">>,
		'NAT' = false,
		ip_assign_scheme = <<"static">>,
		inet_addr = proplists:get_value(lan_addr,APC)
	});

create_table ('Wifi_RRM_Config',_APC,Store) ->
	ets:insert(Store,#'Wifi_RRM_Config'{
		key_id = <<"d1f9874c-d8e7-4426-9d70-c856c4dc6126">>,
		'_version' = [<<"uuid">>,<<"9bbd18e7-ed7e-4ff3-b89d-a54c12b27ed7">>],
		freq_band = <<"2.4G">>,
		min_load = 50,
		'_uuid' = [<<"uuid">>,<<"d1f9874c-d8e7-4426-9d70-c856c4dc6126">>],
		backup_channel = 11,
		snr_percentage_drop = 20
	}),
	ets:insert(Store,#'Wifi_RRM_Config'{
		key_id = <<"8cf973a6-a268-4de4-9bf2-5f7d9222f806">>,
		'_version' = [<<"uuid">>,<<"9bbd18e7-ed7e-4ff3-b89d-a54c12b27ed7">>],
		freq_band = <<"5GL">>,
		min_load = 40,
		'_uuid' = [<<"uuid">>,<<"8cf973a6-a268-4de4-9bf2-5f7d9222f806">>],
		backup_channel = 44,
		snr_percentage_drop = 30
	}),
	ets:insert(Store,#'Wifi_RRM_Config'{
		key_id = <<"44deb01a-a2a8-4b5b-a2be-0bdf04050b97">>,
		'_version' = [<<"uuid">>,<<"9bbd18e7-ed7e-4ff3-b89d-a54c12b27ed7">>],
		freq_band = <<"5GU">>,
		min_load = 40,
		'_uuid' = [<<"uuid">>,<<"844deb01a-a2a8-4b5b-a2be-0bdf04050b97">>],
		backup_channel = 154,
		snr_percentage_drop = 30
	});

create_table ('Wifi_Associated_Clients',_APC,Store) -> 
	%io:format("CONFIGURED WIFI CLIENTS:~n~p~n",[proplists:get_value(wifi_clients,APC)]),
	% F = fun({_,_,[MAC|_]}) ->
	% 	ets:insert(Store, #'Wifi_Associated_Clients'{
	% 		key_id = utils:uuid_b(),
	% 		'_version' = [<<"uuid">>, utils:uuid_b()],
	% 		mac = MAC,
	% 		state = <<"active">>
	% 	})
	% end,
	% [F(X) || X <- proplists:get_value(wifi_clients,APC)];
	ets:insert(Store, #'Wifi_Associated_Clients'{
		key_id = <<"ee49ed4e-5a04-4100-bf6a-ebfbbc54250e">>,
		'_version' = [<<"uuid">>,<<"5bc3eb0f-1cc3-4dae-aae5-af02c8d2f1c7">>],
		mac = <<"52:b6:76:03:6d:f2">>,
		state = <<"active">>,
		uapsd = [<<"set">>,[]],
		capabilities = [<<"set">>,[]],
		kick = [<<"map">>,[]],
		oftag = ["set",[]]
	});

create_table ('AWLAN_Node',APC,Store) -> 
	ets:insert(Store, #'AWLAN_Node'{
		key_id = utils:uuid_b(),
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

