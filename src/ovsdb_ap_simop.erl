%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 20. December 2020 @ 09:19:42
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_simop).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/inventory.hrl").

-export ([create_ap_node/2,create_radios/2,create_clients/2,update_wifi_clients/1,update_dhcp_leases/1]).



%%-----------------------------------------------------------------------------
%% managing the access point
%%-----------------------------------------------------------------------------

-spec create_ap_node(APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_ap_node(APC,Store) ->
    create_AWLAN_Node(APC,Store),
    create_AWLAN_Inet(APC,Store).

create_AWLAN_Node (APC,Store) ->
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

create_AWLAN_Inet (APC,Store) ->
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
    ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
		if_name= <<"wwan">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"0b10958d-9bfb-45e5-9c36-ad8327750607">>],
		inet_config = [<<"uuid">>,UUID1]
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
		inet_config = [<<"uuid">>,UUID2]
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
    ets:insert(Store, #'Wifi_Inet_State'{
		'**key_id**' = utils:uuid_b(),
		if_name= <<"wan6">>,
		if_type = <<"eth">>,
		enabled = false,
		'_version' = [<<"uuid">>,<<"ac171d81-5e5f-41a9-aa71-44a11bb2f72b">>],
		inet_config = [<<"uuid">>,UUID3]
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
		inet_config = [<<"uuid">>,UUID4]
	}).
	

%%-----------------------------------------------------------------------------
%% managing radios for the AP
%%-----------------------------------------------------------------------------

-spec create_radios (APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_radios(APC,Store) ->
    lists:foldl(fun(Band,N)->create_radio(Band,N,APC,Store), N+1 end,
                0,
                proplists:get_value(bands,APC)),
    create_initial_VIF_config(APC,Store).

-spec create_radio (Band :: atom(), N :: integer(), APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_radio(B,N,APC,Store) ->
    Band = band_map(B),
    RadioConfigUUID = utils:uuid_b(),
    Wifi_RRM_ConfigUUID = utils:uuid_b(),
    IFName = << <<"radio">>/binary , ($0+N) >>,
    Wifi_Stats_ConfigUUID = utils:uuid_b(),
    DefChannel = get_default_channel(Band),
    BackupChannel = get_backup_channel(Band,DefChannel),
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
        backup_channel = BackupChannel,
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
        channel = DefChannel,
        tx_power = 18,
        ht_mode = <<"HT80">>,
        hw_mode = get_hw_mode(Band),
        enabled = true,
        freq_band = Band
    }).

-spec create_initial_VIF_config (APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_initial_VIF_config (APC,Store) ->
    Wifi_VIF_ConfigUUID = utils:uuid_b(),
    #{'BAND2G' := SSID} = proplists:get_value(ssids,APC),
    MACs = [ MAC || {_,_,_,MAC,_} <- proplists:get_value(wifi_clients,APC)],
	ets:insert(Store,#'Wifi_VIF_Config'{
		'**key_id**' = Wifi_VIF_ConfigUUID,
		ssid = SSID,
		'_version' = [<<"uuid">>,utils:uuid_b()]
	}),
	ets:insert(Store,#'Wifi_VIF_State'{
		'**key_id**' = utils:uuid_b(),
		mac = proplists:get_value(wan_mac,APC),
		associated_clients = [<<"set">>,MACs],
		vif_config = [<<"uuid">>,Wifi_VIF_ConfigUUID],
		ssid = SSID,
		'_version' = [<<"uuid">>,utils:uuid_b()]
	}).

%%-----------------------------------------------------------------------------
%% managing clients
%%-----------------------------------------------------------------------------

-spec create_clients(APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_clients(APC,Store) ->
    create_ap_wifi_clients(APC,Store),
    create_ap_lan_clients(APC,Store).

-spec create_ap_lan_clients(APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_ap_lan_clients (_APC,_Store) ->
    ok.

-spec create_ap_wifi_clients (APC :: proplists:proplist(), Store :: ets:tid()) -> ok.
create_ap_wifi_clients (APC,Store) ->
    % Cl = proplists:get_value(wifi_clients,APC),
    % io:format("Wifi Clients:~n~p~n",[Cl]).
    %[ create_wifi_client(Cl,APC,Store) || {_,_,_,MAC,_}=Cl <- proplists:get_value(wifi_clients,APC), MAC =/= <<"f8:e5:cf:ef:bf:fa">>].
	[ create_wifi_client(Cl,APC,Store) || Cl <- proplists:get_value(wifi_clients,APC) ].
	
    
create_wifi_client ({Idx,_Band,_SSID,MAC,Vendor},APC,Store) ->
    NM = proplists:get_value(name,APC),
	AssKey = utils:uuid_b(),
	DhcpKey = utils:uuid_b(),
    ets:insert(Store, #'Wifi_Associated_Clients'{
        '**key_id**' = AssKey,
        '_version' = [<<"uuid">>, utils:uuid_b()],
        mac = MAC,
        state = <<"active">>
    }),
    ets:insert(Store, #'DHCP_leased_IP'{
        '**key_id**' = DhcpKey,
        '_version' = [<<"uuid">>, utils:uuid_b()],
        hostname = iolist_to_binary(["H_",NM,"_",integer_to_list(Idx)]),
        inet_addr = iolist_to_binary(["192.168.1.",integer_to_list(Idx+1)]),
        hwaddr = MAC,
        vendor_class = Vendor,
		fingerprint = get_dhcp_fingerprint(),
        device_name = iolist_to_binary([NM,".SimClient_",integer_to_list(Idx+1)])
    }),
	ovsdb_ap_monitor:create_pub_entry(<<"Wifi_Associated_Clients">>,AssKey,Store),
	ovsdb_ap_monitor:create_pub_entry(<<"DHCP_leased_IP">>,DhcpKey,Store).

update_wifi_clients (Store) ->
	F = fun ({Key,Old}) ->
			New = Old#{<<"_version">>=>[<<"uuid">>,utils:uuid_b()],
					   <<"state">>=>get_random_wifi_state()},
			{Key,Old,New}
	end,
	Cl = [ F(X) || X <- ovsdb_dba:select_with_key(<<"Wifi_Associated_Clients">>,[],Store) ],
	[ ovsdb_ap_monitor:create_pub_entry(<<"Wifi_Associated_Clients">>,K,O,N,Store) || {K,O,N} <- Cl ],
	?L_IA("Refreshing Wifi Clients N=~B",[length(Cl)]).

update_dhcp_leases (Store) ->
	F = fun ({Key,Old}) ->
		New = Old#{<<"_version">>=>[<<"uuid">>,utils:uuid_b()]},
		{Key,Old,New}
	end,
	Leases = [ F(X) || X <- ovsdb_dba:select_with_key(<<"DHCP_leased_IP">>,[],Store) ],
	[ ovsdb_ap_monitor:create_pub_entry(<<"DHCP_leased_IP">>,K,O,N,Store) || {K,O,N} <- Leases ],
	?L_IA("Refreshing DHCP leases N=~B",[length(Leases)]).

%%-----------------------------------------------------------------------------
%% helper functions
%%-----------------------------------------------------------------------------

-spec band_map(atom()) -> binary().
band_map('BAND2G') -> <<"2.4G">>;
band_map('BAND5G') -> <<"5GL">>;
band_map('BAND5GL') -> <<"5GL">>;
band_map('BAND5GU') -> <<"5GU">>;
band_map(_) -> <<"2.4G">>.

get_allowed_channels(<<"5GU">>)->[100,104,108,112,116,120,124,128,132,136,140,144,149,153,154,157,161,165];
get_allowed_channels(<<"2.4G">>)->[1,2,3,4,5,6,7,8,9,10,11];
get_allowed_channels(<<"5GL">>)->[36,40,44,48,52,56,60,64].

get_default_channel(Band) ->
    Allowed = list_to_tuple(get_allowed_channels(Band)),
    element(rand:uniform(size(Allowed)),Allowed).

get_backup_channel(Band,N) ->
    case get_default_channel(Band) of
        N ->
            get_backup_channel(Band,N) ;
        Ch ->
            Ch
    end.
    
modify_mac(MAC,N) ->
	[X1,X2,$:,X3,X4,$:,X5,X6,$:,X7,X8,$:,X9,X10,$:,X11,_X12] = binary_to_list(MAC),
	list_to_binary([X1,X2,$:,X3,X4,$:,X5,X6,$:,X7,X8,$:,X9,X10,$:,X11,N+$0]).

get_hw_mode(<<"5GU">>)-><<"11ac">>;
get_hw_mode(<<"2.4G">>)-><<"11n">>;
get_hw_mode(<<"5GL">>)-><<"11ac">>.

get_random_wifi_state() ->
	case rand:uniform(10) of
		N when N > 4 ->
			<<"active">>;
		_ ->
			<<"idle">>
	end.

get_dhcp_fingerprint () ->
	FP = {<<"1,15,3,6,44,46,47,31,33,249,43">>,
		  <<"1,15,3,6,44,46,47,31,33,249,43,252">>,
		  <<"1,15,3,6,44,46,47,31,33,249,43,252,12">>,
		  <<"15,3,6,44,46,47,31,33,249,43">>,
		  <<"15,3,6,44,46,47,31,33,249,43,252">>,
		  <<"28,2,3,15,6,12,44,47">>,
		  <<"1,3,6,15,119,78,79,95,252">>,
          <<"1,3,6,15,119,95,252,44,46,47">>},
	element(rand:uniform(tuple_size(FP)),FP).

