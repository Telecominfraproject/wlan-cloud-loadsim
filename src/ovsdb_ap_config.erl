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
	ca_name   = <<>> :: string() | binary(),
	redirector= <<>> :: binary(),
	serial    = <<>> :: binary(),
	id        = <<>> :: binary(),
	store_ref :: ets:tid(),
	cacert    = <<>> :: binary(),		% pem file (in memory) of the server certificate chain
	cert      = <<>> :: binary(),
	key       = {none,<<>>} :: {atom(), binary()}% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.
-export_type([cfg/0]).


-export([new/4,configure/1]).
-export ([id/1,ca_certs/1,client_cert/1,client_key/1,tip_redirector/2,tip_manager/2,caname/1,serial/1]).


%%------------------------------------------------------------------------------
%% API


-spec new (CAName :: string() | binary(), Id :: binary(), Store :: ets:tid(), Redirector :: binary()) -> Config :: cfg().
new (CAName,Id,Store,Redirector) ->
	#cfg{ca_name=CAName, id=Id, store_ref = Store, redirector=Redirector}.

-spec configure (Config :: cfg()) -> NewConfig :: cfg().
configure (#cfg{ca_name=CAName, id=ID, redirector=R}=Config) ->
	{ok,Info} = inventory:get_client(CAName,ID),
	SSID = case Info#client_info.wifi_clients of
		[{_,_,_,S,_}|_] ->
			S;
		_ ->
			<<"TipWlan-cloud-wifi">>
	end,

	HW = case hardware:get_by_id(Info#client_info.id) of
		{ok,[HardwareInfo]} ->
			HardwareInfo;
		_ ->
			#hardware_info{}
	end,

	APC = [
		{serial,Info#client_info.serial},
		{type,Info#client_info.type},
		{wan_addr,make_ip_addr(ID)},
		{wan_mac,Info#client_info.wan_mac0},
		{lan_addr,<<"192.168.1.1">>},
		{lan_mac,Info#client_info.lan_mac0},
		{tip_redirector,R},
		{wifi_clients,get_all_wifi_macs(Info#client_info.wifi_clients)},
		{name,Info#client_info.name},
		{ssid,SSID},
		{bands,Info#client_info.bands},
		{hardware,HW}

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
		cacert = Info#client_info.cacert,
		serial = Info#client_info.serial,
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
	create_radio_tables(APC,Store),
	create_VIF_tables(APC,Store),
	create_table('AWLAN_Node',APC,Store),
%	create_table('Wifi_Radio_Config',APC,Store),
%	create_table('Wifi_Radio_State',APC,Store),
	create_table('Wifi_Inet_Config',APC,Store),
	create_table('Wifi_Inet_State',APC,Store),
%	create_table('Wifi_RRM_Config',APC,Store),
%	create_table('Wifi_Stats_Config',APC,Store),
	create_table('DHCP_leased_IP',APC,Store),
%	create_table('Wifi_VIF_Config',APC,Store),
%	create_table('Wifi_VIF_State',APC,Store),
	create_table('Wifi_Associated_Clients',APC,Store).
	
%%------------------------------------------------------------------------------
%% accessor API - direct config settings

-spec id (Config :: cfg()) -> Id :: binary().
id(Cfg) ->
	Cfg#cfg.id.

-spec caname (Config :: cfg()) -> CAName :: binary().
caname(Cfg) ->
	Cfg#cfg.ca_name.

-spec serial (Config :: cfg()) -> Serial :: binary().
serial(Cfg) ->
	Cfg#cfg.serial.

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

create_radio_tables(APC,Store)->
	lists:foldl(fun(E,N)->
								Band = convert_band(E),
								RadioConfigUUID = utils:uuid_b(),
								Wifi_RRM_ConfigUUID = utils:uuid_b(),
								IFName = << <<"radio">>/binary , ($0+N) >>,
								Wifi_Stats_ConfigUUID = utils:uuid_b(),
								ets:insert(Store, #'Wifi_Stats_Config'{
									'**key_id**' = Wifi_Stats_ConfigUUID,
									'_uuid' = [<<"uuid">>,Wifi_Stats_ConfigUUID],
									radio_type = Band}),
								ets:insert(Store,#'Wifi_RRM_Config'{
									'**key_id**' = Wifi_RRM_ConfigUUID,
									'_uuid' = [<<"uuid">>,Wifi_RRM_ConfigUUID],
									'_version' = [<<"uuid">>,utils:uuid_b()],
									freq_band = Band,
									min_load = 40,
									backup_channel = get_backup_channel(Band),
									snr_percentage_drop = 30
								}),
								ets:insert(Store, #'Wifi_Radio_Config'{
							    '**key_id**' = RadioConfigUUID,
							    '_uuid' = [<<"uuid">>, RadioConfigUUID],
							    '_version' = [<<"uuid">>,utils:uuid_b()],
							    freq_band = Band,
							    if_name = IFName
								}),
		            ets:insert(Store, #'Wifi_Radio_State'{
									'**key_id**' = utils:uuid_b(),
									'_version' = [<<"uuid">>,utils:uuid_b()],
									if_name = IFName,
									mac = modify_mac(proplists:get_value(wan_mac,APC),0),
									bcn_int = 100,
									allowed_channels = [<<"set">>, get_allowed_channels(Band)],
									radio_config = [<<"uuid">>,RadioConfigUUID],
									vif_states = [<<"set">>,[]], % [<<"uuid">>,<<"87f75538-67d0-408a-9c8b-018665754d48">>],
									country = <<"US">>,
									radar = [<<"map">>,[]],
									tx_chainmask = 3,
									channel = get_default_channel(Band),
									tx_power = 18,
									ht_mode = <<"HT80">>,
									hw_mode = get_hw_mode(Band),
									enabled = true,
									freq_band = Band
								}), N+1
		end,0,proplists:get_value(bands,APC)).

create_VIF_tables(APC,Store)->
	Wifi_VIF_ConfigUUID = utils:uuid_b(),
	ets:insert(Store,#'Wifi_VIF_Config'{
		'**key_id**' = Wifi_VIF_ConfigUUID,
		ssid = proplists:get_value(ssid,APC),
		'_version' = [<<"uuid">>,utils:uuid_b()]
	}),
	ets:insert(Store,#'Wifi_VIF_State'{
		'**key_id**' = utils:uuid_b(),
		mac = proplists:get_value(wan_mac,APC),
		associated_clients = [<<"set">>,proplists:get_value(wifi_clients,APC)],
		vif_config = [<<"uuid">>,Wifi_VIF_ConfigUUID],
		ssid = proplists:get_value(ssid,APC),
		'_version' = [<<"uuid">>,utils:uuid_b()]
	}).

create_table ('Wifi_Inet_State',APC,Store) ->
	ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
		if_name= <<"wwan">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"0b10958d-9bfb-45e5-9c36-ad8327750607">>],
		inet_config = [<<"uuid">>,<<"7e38a63b-526a-4b83-b30e-edd4c17ab3f6">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
		dhcpd = [<<"map">>,[[<<"lease_time">>,<<"12h">>],[<<"start">>,<<"100">>],[<<"stop">>,<<"150">>]]],
		if_name= <<"lan">>,
		if_type = <<"bridge">>,
		enabled = true,
		netmask = <<"255.255.255.0">>,
		inet_addr = proplists:get_value(lan_addr,APC),
		'_version' = [<<"uuid">>,<<"6237745e-3a4d-41a3-858d-7cbce39f5b8c">>],
		hwaddr = proplists:get_value(wan_mac,APC),
		network = true,
		mtu = 1500,
		ip_assign_scheme = <<"static">>,
		inet_config = [<<"uuid">>,<<"19484645-8519-4bd0-98dd-13f1fec83395">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
		if_name= <<"wan6">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"ac171d81-5e5f-41a9-aa71-44a11bb2f72b">>],
		inet_config = [<<"uuid">>,<<"b803af39-e392-437b-8c86-dd87d24f8b49">>]
	}),
	ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
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
	UUID1 = utils:uuid_b(),
	ets:insert(Store, #'Wifi_Inet_Config'{
		'**key_id**' = UUID1,
		'_uuid' = [<<"uuid">>, UUID1],
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
	UUID2 = utils:uuid_b(),
	ets:insert(Store, #'Wifi_Inet_Config'{
		'**key_id**' = UUID2,
		'_uuid' = [<<"uuid">>, UUID2],
		if_name = <<"wan6">>,
		network = true,
		if_type = <<"bridge">>,
		enabled = true,
		'NAT' = false
	}),
	UUID3 = utils:uuid_b(),
	ets:insert(Store, #'Wifi_Inet_Config'{
		'**key_id**' = UUID3,
		'_uuid' = [<<"uuid">>, UUID3],
		if_name = <<"wwan">>,
		network = true,
		if_type = <<"eth">>,
		enabled = true,
		'NAT' = false,
		ip_assign_scheme = <<"dhcp">>
	}),
	UUID4 = utils:uuid_b(),
	ets:insert(Store, #'Wifi_Inet_Config'{
		'**key_id**' = UUID4,
		'_uuid' = [<<"uuid">>, UUID4],
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

create_table ('Wifi_Associated_Clients',APC,Store) ->
	%io:format("CONFIGURED WIFI CLIENTS:~n~p~n",[proplists:get_value(wifi_clients,APC)]),
	lists:foldl( fun(MAC,A)->
									ets:insert(Store, #'Wifi_Associated_Clients'{
										'**key_id**' = utils:uuid_b(),
										'_version' = [<<"uuid">>, utils:uuid_b()],
										mac = MAC,
										state = <<"active">>,
									}), A
								end,[],proplists:get_value(wifi_clients,APC));
	%%end,
	%%[F(X) || X <- proplists:get_value(wifi_clients,APC)];
	% ets:insert(Store, #'Wifi_Associated_Clients'{
	% 	'**key_id**' = <<"ee49ed4e-5a04-4100-bf6a-ebfbbc54250e">>,
	% 	'_version' = [<<"uuid">>,<<"5bc3eb0f-1cc3-4dae-aae5-af02c8d2f1c7">>],
	% 	mac = <<"52:b6:76:03:6d:f2">>,
	% 	state = <<"active">>,
	% 	uapsd = [<<"set">>,[]],
	% 	capabilities = [<<"set">>,[]],
	% 	kick = [<<"map">>,[]],
	% 	oftag = [<<"set">>,[]]
	% });

create_table ('DHCP_leased_IP',APC,Store) ->
	NM = proplists:get_value(name,APC),
	lists:foldl( fun(MAC,N)->
										Vendor = case oui_server:lookup_oui_from_mac(MAC) of
											         {ok,X} -> X;
															 _ -> <<"unknown">>
										         end,

		ets:insert(Store, #'DHCP_leased_IP'{
										'**key_id**' = utils:uuid_b(),
										'_version' = [<<"uuid">>, utils:uuid_b()],
										hostname = iolist_to_binary([proplists:get_value(name,APC),"_",integer_to_list(N)]),
										inet_addr = iolist_to_binary(["192.168.1.",integer_to_list(N+1)]),
										hwaddr = MAC,
										vendor_class = Vendor,
										device_name = iolist_to_binary([NM,".SimClient_",integer_to_list(N+1)])
									}),
									N+1
								end,1,proplists:get_value(wifi_clients,APC));


create_table ('AWLAN_Node',APC,Store) ->
	HW = proplists:get_value(hardware,APC),
	ets:insert(Store, #'AWLAN_Node'{
		'**key_id**' = utils:uuid_b(),
		redirector_addr = proplists:get_value(tip_redirector,APC),
		serial_number = proplists:get_value(serial,APC),
		id = proplists:get_value(serial,APC),
		model = proplists:get_value(type,APC),
		revision = <<"1">>,
		platform_version = <<"OPENWRT_EA8300">>,
		firmware_version = <<"0.1.0">>,
		version_matrix = [<<"map">>,[
							[<<"DATE">>,HW#hardware_info.firmware_date],
							[<<"FIRMWARE">>,HW#hardware_info.firmware],
							[<<"FW_BUILD">>,HW#hardware_info.firmware_build],
							[<<"FW_COMMIT">>,HW#hardware_info.firmware_commit],
							[<<"FW_IMAGE_ACTIVE">>,HW#hardware_info.firmware],
							[<<"FW_IMAGE_INACTIVE">>,HW#hardware_info.firmware_image_inactive],
							[<<"FW_PROFILE">>,HW#hardware_info.firmware_profile],
							[<<"FW_VERSION">>,HW#hardware_info.firmware_version],
							[<<"HOST">>,HW#hardware_info.firmware_host],
							[<<"OPENSYNC">>,HW#hardware_info.opensync],
							[<<"core">>,HW#hardware_info.core],
							[<<"vendor/tip">>,HW#hardware_info.vendor_tip]
						 ]]
	}).

get_all_wifi_macs(Clients)->
	Fl = lists:flatten(Clients),
	[MAC || {_,_,_,MAC,_} <- Fl].
	
convert_band('BAND2G')-> <<"2.4G">>;
convert_band('BAND5GL')-> <<"5GL">>;
convert_band('BAND5GU')-> <<"5GU">>.

modify_mac(MAC,N) ->
	[X1,X2,$:,X3,X4,$:,X5,X6,$:,X7,X8,$:,X9,X10,$:,X11,_X12] = binary_to_list(MAC),
	list_to_binary([X1,X2,$:,X3,X4,$:,X5,X6,$:,X7,X8,$:,X9,X10,$:,X11,N+$0]).

get_allowed_channels(<<"5GU">>)->[100,104,108,112,116,120,124,128,132,136,140,144,149,153,154,157,161,165];
get_allowed_channels(<<"2.4G">>)->[1,2,3,4,5,6,7,8,9,10,11];
get_allowed_channels(<<"5GL">>)->[36,40,44,48,52,56,60,64].

get_default_channel(<<"5GU">>)->149;
get_default_channel(<<"2.4G">>)->6;
get_default_channel(<<"5GL">>)->36.

get_backup_channel(<<"5GU">>)->154;
get_backup_channel(<<"2.4G">>)->11;
get_backup_channel(<<"5GL">>)->44.

get_hw_mode(<<"5GU">>)-><<"11ac">>;
get_hw_mode(<<"2.4G">>)-><<"11n">>;
get_hw_mode(<<"5GL">>)-><<"11ac">>.

