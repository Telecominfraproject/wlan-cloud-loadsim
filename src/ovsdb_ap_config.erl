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
-include("../include/ovsdb_definitions.hrl").
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/inventory.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-export([configure/1,find_inet_config_for_if_name/2,find_radio_config_for_interface/2,find_vif_config/2,find_band_for_interface/2,create_table/2]).

%%------------------------------------------------------------------------------
%% API



-spec configure(APS::ap_state()) -> NewState::ap_state().
configure (APS0) ->
	Tables = [<<"AWLAN_Node">>,<<"Wifi_Inet_Config">>,<<"Wifi_Inet_State">>,<<"Wifi_Associated_Clients">>,<<"DHCP_leased_IP">>,<<"Wifi_VIF_Config">>,
	          <<"Wifi_VIF_State">>,<<"Wifi_Radio_Config">>,<<"Wifi_Radio_State">>,<<"Wifi_Stats_Config">>,<<"Wifi_RRM_Config">>,<<"Hotspot20_Config">>,
						<<"Hotspot20_OSU_Providers">>,<<"Hotspot20_Icon_Config">>,<<"Command_State">>],
	NewState = lists:foldl(fun(Table,APNow) ->
								create_table(Table,APNow)
							end,APS0,Tables),
	NewState#ap_state{ known_table_names = Tables }.

-spec create_table(TableName::binary(),APS::ap_state())->NewState::ap_state().
create_table(<<"AWLAN_Node">> = TableName,APS)->
	HW = APS#ap_state.hardware,
% 	TopicName = binary:list_to_bin([<<"/ap/Open_AP_">>,APS#ap_state.id,<<"/opensync">>]),
	UUID = utils:uuid_b(),
	NewTable = #{ UUID =>
	              #{
		              <<"_uuid">> => [<<"uuid">>,UUID],
		              <<"_version">> => utils:create_version(),
	                <<"device_mode">> => [<<"set">>,[]],
	                <<"firmware_pass">> => <<>>,
	                <<"firmware_url">> => <<>>,
	                <<"firmware_version">> => <<"0.1.0">>,
	                <<"id">> => APS#ap_state.id,
	                <<"led_config">> => [<<"map">>,[]],
%	                <<"manager_addr">> => <<"ssl:opensync-controller.wlan.local:6640">>,
									<<"manager_addr">> => <<>>,
	                <<"max_backoff">> => 60,<<"min_backoff">> => 30,
	                <<"model">> => HW#hardware_info.model,
	                <<"mqtt_headers">> => [<<"map">>,[]],
	                <<"mqtt_settings">> =>
	                [<<"map">>,[]],
%	                 [[<<"broker">>,<<"opensync-mqtt-broker.wlan.local">>],
%	                  [<<"compress">>,<<"zlib">>],
%	                  [<<"port">>,<<"1883">>],
%	                  [<<"qos">>,<<"0">>],
%	                  [<<"remote_log">>,<<"1">>],
%	                  [<<"topics">>,TopicName]],
	                <<"mqtt_topics">> => [<<"map">>,[]],
	                <<"platform_version">> => binary:list_to_bin([<<"OPENWRT_">>,HW#hardware_info.model]),
	                <<"redirector_addr">> =>
	                <<"ssl:opensync-redirector.wlan.local:6643">>,
	                <<"revision">> => <<"1">>,
	                <<"serial_number">> => APS#ap_state.id,
                  <<"sku_number">> => binary:list_to_bin([<<"tip.wlan_">>,APS#ap_state.id]),
%                  <<"sku_number">> => [<<"set">>,[]],
	                <<"upgrade_dl_timer">> => 0,<<"upgrade_status">> => 0,
	                <<"upgrade_timer">> => 0,
	                <<"factory_reset">> => [<<"set">>,[]],
	                <<"version_matrix">> =>
	                [<<"map">>,
	                 [[<<"DATE">>,HW#hardware_info.firmware_date],
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
	                  [<<"vendor/tip">>,HW#hardware_info.vendor_tip]]]}
								},
	NewTables = maps:put(TableName, NewTable, APS#ap_state.tables),
	APS#ap_state{ tables = NewTables };

create_table (<<"Wifi_Inet_Config">> = TableName ,APS) ->
	WanUUID = utils:uuid_b(),
	Wan = #{
		<<"_uuid">> => [<<"uuid">>,WanUUID],
		<<"_version">> => utils:create_version(),
		<<"dhcpd">> => [<<"map">>,[]],
		<<"if_name">> => <<"wan">>,
		<<"mtu">> => [<<"set">>,[]],
		<<"network">> => true,
		<<"dns">> => [<<"map">>,[]],
		<<"if_type">> => <<"bridge">>,
		<<"broadcast">> => [<<"set">>,[]],
		<<"enabled">> => true,
		<<"vlan_id">> => [<<"set">>,[]],
		<<"netmask">> => [<<"set">>,[]],
		<<"gateway">> => [<<"set">>,[]],
		<<"NAT">> => true,
		<<"ip_assign_scheme">> => <<"dhcp">>,
		<<"inet_addr">> => [<<"set">>,[]]
	},

	WWanUUID = utils:uuid_b(),
	WWan = #{
		<<"_uuid">> => [<<"uuid">>,WWanUUID],
		<<"_version">> => utils:create_version(),
		<<"if_name">> => <<"wwan">>,
		<<"network">> => true,
		<<"if_type">> => <<"eth">>,
		<<"enabled">> => true,
		<<"NAT">> => false,
		<<"ip_assign_scheme">> => <<"dhcp">>
	},

	LanUUID = utils:uuid_b(),
	Lan = #{
		<<"_uuid">> => [<<"uuid">>,LanUUID],
		<<"_version">> => utils:create_version(),
		<<"dhcpd">> => [<<"map">>,[[<<"lease_time">>,<<"12h">>],[<<"start">>,<<"100">>],[<<"stop">>,<<"150">>]]],
		<<"if_name">> => <<"lan">>,
		<<"network">> => true,
		<<"if_type">> => <<"bridge">>,
		<<"enabled">> => true,
		<<"netmask">> => <<"255.255.255.0">>,
		<<"NAT">> => false,
		<<"ip_assign_scheme">> => <<"static">>,
		<<"inet_addr">> => APS#ap_state.lan_addr
	},

	M1 = maps:put( WanUUID, Wan , #{} ),
	M2 = maps:put( WWanUUID, WWan, M1 ),
	M3 = maps:put( LanUUID, Lan, M2),

	APS#ap_state{ tables = maps:put(TableName,M3, APS#ap_state.tables)};

create_table (<<"Wifi_Inet_State">> = TableName,APS) ->
	WWanUUID = utils:uuid_b(),
	WWan = #{
		<<"_uuid">> => [<<"uuid">>,WWanUUID],
		<<"_version">> => utils:create_version(),
		<<"if_name">> => <<"wwan">>,
		<<"if_type">> => <<"eth">>,
		<<"enabled">> => false,
		<<"inet_config">> => [<<"uuid">>, find_inet_config_for_if_name(<<"wwan">>,APS)]
	},

	LanUUID = utils:uuid_b(),
	Lan = #{
		<<"_uuid">> => [<<"uuid">>,LanUUID],
		<<"_version">> => utils:create_version(),
		<<"dhcpd">> => [<<"map">>,[[<<"lease_time">>,<<"12h">>],[<<"start">>,<<"100">>],[<<"stop">>,<<"150">>]]],
		<<"if_name">> => <<"lan">>,
		<<"if_type">> => <<"bridge">>,
		<<"enabled">> => true,
		<<"netmask">> => <<"255.255.255.0">>,
		<<"inet_addr">> => APS#ap_state.lan_addr,
		<<"hwaddr">> => APS#ap_state.details#client_info.lan_mac0,
		<<"network">> => true,
		<<"mtu">> => 1500,
		<<"ip_assign_scheme">> => <<"static">>,
		<<"inet_config">> => [<<"uuid">>,find_inet_config_for_if_name(<<"lan">>,APS)]
	},

	WanUUID = utils:uuid_b(),
	Wan = #{
		<<"_uuid">> => [<<"uuid">>,WanUUID],
		<<"_version">> => utils:create_version(),
		<<"if_name">> => <<"wan">>,
		<<"if_type">> => <<"bridge">>,
		<<"enabled">> => true,
		<<"netmask">> => <<"255.255.255.0">>,
		<<"NAT">> => true,
		<<"inet_addr">> => APS#ap_state.wan_addr,
		<<"hwaddr">> => APS#ap_state.details#client_info.wan_mac0,
		<<"network">> => true,
		<<"mtu">> => 1500,
		<<"dns">> => [<<"map">>,[[<<"primary">>,<<"10.20.0.1">>]]],
		<<"ip_assign_scheme">> => <<"dhcp">>,
		<<"gateway">> => <<"10.20.0.1">>,
		<<"inet_config">> => [<<"uuid">>,find_inet_config_for_if_name(<<"wan">>,APS)]
	},

	M1 = maps:put( WanUUID, Wan , #{} ),
	M2 = maps:put( WWanUUID, WWan, M1 ),
	M3 = maps:put( LanUUID, Lan, M2),

	APS#ap_state{ tables = maps:put(TableName,M3, APS#ap_state.tables)};

%% Model for 'Wifi_Associated_Clients'
%% "{\"id\":null,\"method\":\"update\",\"params\":[\"Wifi_Associated_Clients_Open_AP_21P10C69717951\",{\"Wifi_Associated_Clients\":{\"8435a778-2f9b-49f7-989a-92634aa3adfc\":{\"new\":{\"key_id\":\"\",\"_version\":[\"uuid\",\"d4db59ad-1f01-48d9-beba-1b8c15c59769\"],\"mac\":\"f8:ff:c2:43:f9:8c\",\"state\":\"active\",\"uapsd\":[\"set\",[]],\"capabilities\":[\"set\",[]],\"kick\":[\"map\",[]],\"oftag\":[\"set\",[]]}}}}]}"
create_table(<<"Wifi_Associated_Clients">> = TableName ,APS)->
	{ AssociatedList, AssociatedBandList } =
		lists:foldl( fun({_Index,Band,_SSID,MAC,_Vendor},{MacList,MacBandList}) ->
										NewUUID = utils:uuid_b(),
										NewMacList = maps:put( NewUUID,
										             #{<<"_uuid">> => [<<"uuid">>,NewUUID],
											             <<"_version">> => utils:create_version(),
											              <<"capabilities">> => [<<"set">>,[]],
											              <<"key_id">> => <<>>,
											              <<"kick">> => [ <<"map">>,[]],
											              <<"mac">> => MAC ,
											              <<"oftag">> => [<<"set">>,[]],
											              <<"state">> => <<"idle">>,
											              <<"uapsd">> => [<<"set">>,[]]
											             },MacList),
										CurrentBandMacList = maps:get(utils:band_to_json(Band),MacBandList,[]),
										NewMacs = [ NewUUID | CurrentBandMacList],
										NewMacBandList = maps:put(utils:band_to_json(Band),NewMacs,MacBandList),
										{ NewMacList, NewMacBandList }
	                end,{#{},#{}},APS#ap_state.details#client_info.wifi_clients),
		APS#ap_state{ tables = maps:put(TableName,AssociatedList,APS#ap_state.tables), associated_clients = AssociatedBandList};

%% Model for DHCP_leased_IP monitor update
%% "{\"id\":null,\"method\":\"update\",\"params\":[\"DHCP_leased_IP_Open_AP_21P10C69717951\",{\"DHCP_leased_IP\":{\"4bbf3f26-4aba-43da-a03a-dcf7a52bb21e\":{\"new\":{\"db_status\":1,\"subnet_mask\":\"255.255.255.0\",\"hostname\":\"Hypatia\",\"secondary_dns\":\"0.0.0.0/0\",\"inet_addr\":\"192.168.1.211\",\"lease_time\":43200,\"hwaddr\":\"f8:ff:c2:43:f9:8c\",\"_version\":[\"uuid\",\"708d126a-dbc9-4274-ab46-fbd465e22265\"],\"manuf_id\":0,\"vendor_class\":\"\",\"device_type\":0,\"dhcp_server\":\"192.168.1.1\",\"device_name\":\"\",\"fingerprint\":\"1,121,3,6,15,114,119,252,95,44,46\",\"primary_dns\":\"192.168.1.1\",\"gateway\":\"192.168.1.1\"}}}}]}"
create_table(<<"DHCP_leased_IP">> = TableName,APS)->
	NM = APS#ap_state.id,
	Leases = lists:foldl( fun({Index,_Band,_SSID,MAC,Vendor},Acc) ->
													NewUUID = utils:uuid_b(),
													maps:put( NewUUID,
													            #{ <<"_uuid">> => [<<"uuid">>,NewUUID],
													               <<"_version">> => utils:create_version(),
													               <<"db_status">> => 1,
													               <<"device_name">> => <<>>,
													               <<"device_type">> => 0,
													               <<"dhcp_server">> => <<"192.168.1.1">>,
													               <<"fingerprint">> => get_dhcp_fingerprint(),
													               <<"gateway">> => <<"192.168.1.1">>,
													               <<"hostname">> => iolist_to_binary([NM,"_",integer_to_list(Index)]),
													               <<"hwaddr">> => MAC,
													               <<"inet_addr">> => iolist_to_binary(["192.168.1.",integer_to_list(Index+1)]),
													               <<"lease_time">> => 43200,<<"manuf_id">> => 0,
													               <<"primary_dns">> => <<"192.168.1.1">>,
													               <<"secondary_dns">> => <<"8.8.8.8">>,
													               <<"subnet_mask">> => <<"255.255.255.0">>,
													               <<"vendor_class">> => Vendor},Acc)
												end,#{},APS#ap_state.details#client_info.wifi_clients),
	APS#ap_state{ tables = maps:put(TableName,Leases,APS#ap_state.tables)};

create_table(<<"Wifi_VIF_Config">> = TableName ,APS) ->
	{Table,_} = lists:foldl( fun(Band,{Acc,Index})->
								NewUUID = utils:uuid_b(),
								NewAcc = maps:put( NewUUID,
									#{
										<<"_uuid">> => [<<"uuid">>,NewUUID],
										<<"_version">> => utils:create_version(),
										<<"vif_radio_idx">> => Index,
										<<"if_name">> => if_name(Index),
										<<"ap_bridge">> => false,
										<<"wps_pbc">> => true,
										<<"mac_list">> => <<>>,
										<<"ssid">> => <<"TipWlan-cloud-wifi">>,
										<<"uapsd_enable">> => true,
										<<"btm">> => 5,
										<<"wps">> => true,
										<<"mcast2ucast">> => true,
										<<"wds">> => true,
										<<"enabled">> => true,
										<<"dynamic_beacon">> => true,
										<<"rrm">> => 1,
										<<"group_rekey">> => 0,
										<<"vif_dbg_lvl">> => 0,
										<<"wps_pbc_key_id">> => <<>>,
										<<"ft_psk">> => 0,
										<<"ft_mobility_domain">> => 1,
										<<"mode">> => <<"ap">>,
										<<"multi_ap">> => <<"none">>,
										<<"mac_list_type">> => <<"none">>,
										<<"min_hw_mode">> => maps:get(Band,APS#ap_state.hardware#hardware_info.min_hw_mode),
										<<"bridge">> => <<"lan">>,
										<<"ssid_broadcast">> => <<"enabled">>,
										<<"security">> => [<<"map">>,[[<<"encryption">>,<<"WPA-PSK">>],[<<"key">>,<<"w1r3l3ss-fr33d0m">>],[<<"mode">>,<<"2">>]]],
										<<"custom_options">> => [<<"map">>,[]],
										<<"captive_portal">> => [<<"map">>,[]],
										<<"captive_allowlist">> => [<<"set">>,[]],
										<<"vlan_id">> => 1										% unimplemented in default config.
										% credential_configs
										% "parent"

									}, Acc),
								{ NewAcc,Index+1}
							end, {#{},0}, APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Wifi_VIF_State">> = TableName ,APS)->
	{Table,_} = lists:foldl( fun(Band,{Acc,Index})->
%		AssociatedClients = [ [<<"uuid">>,U] || U <- maps:get(utils:band_to_json(Band),APS#ap_state.associated_clients)],
		NewUUID = utils:uuid_b(),
		NewAcc = maps:put( NewUUID,
		          #{
			          <<"_uuid">> => [<<"uuid">>,NewUUID],
			          <<"_version">> => utils:create_version(),
			          <<"vif_radio_idx">> => [<<"set">>,[]],
			          <<"if_name">> => if_name(Index),
			          <<"state">> => [<<"set">>,[]],
			          <<"mac">> => utils:modify_mac( APS#ap_state.details#client_info.wan_mac0, Index ),
			          <<"ssid">> => <<"TipWlan-cloud-wifi">>,
			          <<"uapsd_enable">> => true,
			          <<"ap_bridge">> => true,
			          <<"wps_pbc">> => [<<"set">>,[]],
			          <<"mac_list">> => [<<"set">>,[]],
%%			          <<"associated_clients">> => [ <<"set">>, AssociatedClients ] ,
			          <<"associated_clients">> => [ <<"set">>, [] ] ,
			          <<"vif_config">> => [<<"uuid">>, find_vif_config(if_name(Index),APS)],
			          <<"btm">> => 1,
			          <<"ssid_broadcast">> => <<"enabled">>,
			          <<"wps">> => [<<"set">>,[]],
			          <<"mcast2ucast">> => [<<"set">>,[]],
			          <<"mode">> => <<"ap">>,
			          <<"wds">> => true,
			          <<"enabled">> => true,
			          <<"dynamic_beacon">> => [<<"set">>,[]],
			          <<"rrm">> => 1,
			          <<"group_rekey">> => 0,
			          <<"vif_dbg_lvl">> => 0,
			          <<"wps_pbc_key_id">> => <<>>,
			          <<"ft_psk">> => 0,
			          <<"ft_mobility_domain">> => 1,
			          <<"multi_ap">> => [<<"set">>,[]],
			          <<"mac_list_type">> => <<"none">>,
			          <<"min_hw_mode">> => maps:get(Band,APS#ap_state.hardware#hardware_info.min_hw_mode),
			          <<"channel">> => maps:get(Band,APS#ap_state.hardware#hardware_info.channel_default),
			          <<"bridge">> => <<"lan">>,
			          <<"parent">> => [<<"set">>,[]],
			          <<"ap_vlan_sta_addr">> => [<<"set">>,[]],
			          <<"security">> => [<<"map">>,[[<<"encryption">>,<<"WPA-PSK">>],[<<"key">>,<<"w1r3l3ss-fr33d0m">>],[<<"mode">>,<<"2">>]]],
			          <<"custom_options">> => [<<"map">>,[]],
			          <<"captive_portal">> => [<<"map">>,[]],
			          <<"captive_allowlist">> => [<<"set">>,[]],
			          <<"vlan_id">> => 1

			          % unimplemented in default config.
			          % credential_configs
		          }, Acc),
		{ NewAcc,Index+1}
	                    end, {#{},0}, APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Wifi_Radio_Config">> = TableName,APS)->
	{Table,_} = lists:foldl( fun(Band,{Acc,Index})->
												NewUUID = utils:uuid_b(),
												NewAcc = maps:put( NewUUID,
													#{
														<<"_uuid">> => [<<"uuid">>,NewUUID],
														<<"_version">> => utils:create_version(),
														<<"dfs_demo">> => false,
														<<"if_name">> => if_name(Index),
														<<"tx_power">> => 16,
														<<"thermal_downgrade_temp">> => 65,
														<<"thermal_upgrade_temp">> => 45,
														<<"enabled">> => true,
														<<"bcn_int">> => 100,
														<<"thermal_integration">> => 50,
														<<"freq_band">> => utils:band_to_json(Band),
														<<"channel_mode">> => [<<"set">>,[]],
														<<"country">> => <<"CA">>,
														<<"channel">> => maps:get(Band,APS#ap_state.hardware#hardware_info.channel_default),
														<<"ht_mode">> => maps:get(Band,APS#ap_state.hardware#hardware_info.ht_mode),
														<<"vif_configs">> => [<<"set">>,[]]
													},Acc),
												{ NewAcc,Index+1}
	                    end, {#{},0}, APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Wifi_Radio_State">> = TableName,APS)->
	{Table,_} = lists:foldl( fun(Band,{Acc,Index})->
		NewUUID = utils:uuid_b(),
		NewAcc = maps:put( NewUUID,
		                   #{
			                   <<"_uuid">> => [<<"uuid">>,NewUUID],
			                   <<"_version">> => utils:create_version(),
			                   <<"mac">> => utils:modify_mac(APS#ap_state.details#client_info.wan_mac0,Index),
			                   <<"dfs_demo">> => false,
			                   <<"if_name">> => if_name(Index),
			                   <<"tx_power">> => 16,
			                   <<"thermal_downgrade_temp">> => 65,
			                   <<"thermal_upgrade_temp">> => 45,
			                   <<"enabled">> => true,
			                   <<"bcn_int">> => 100,
			                   <<"thermal_integration">> => 50,
			                   <<"freq_band">> => utils:band_to_json(Band),
			                   <<"channel_mode">> => [<<"set">>,[]],
			                   <<"country">> => <<"CA">>,
			                   <<"allowed_channels">> => [<<"set">>, maps:get(Band,APS#ap_state.hardware#hardware_info.channels)],
			                   <<"channel">> => maps:get(Band,APS#ap_state.hardware#hardware_info.channel_default),
			                   <<"tx_chainmask">> => 3,
			                   <<"radar">> => [<<"map">>,[]],
			                   <<"vif_states">> => [<<"set">>,[]], % [<<"uuid">>,<<"87f75538-67d0-408a-9c8b-018665754d48">>],
			                   <<"ht_mode">> => maps:get(Band,APS#ap_state.hardware#hardware_info.ht_mode),
			                   <<"vif_configs">> => [<<"set">>,[]],
			                   <<"radio_config">> => [<<"uuid">>,find_radio_config_for_interface(if_name(Index),APS)]
		                   },Acc),
												{ NewAcc,Index+1}
	                    end, {#{},0}, APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Wifi_Stats_Config">> = TableName,APS)->
	Table = lists:foldl(fun(Band,Acc) ->
												NewUUID = utils:uuid_b(),
												maps:put( NewUUID,
													#{
														<<"_uuid">> => [<<"uuid">>,NewUUID],
														<<"_version">> => utils:create_version(),
														<<"sampling_interval">> => 20,
														<<"radio_type">> => utils:band_to_json(Band),
														<<"report_type">> => <<"raw">>,
														<<"threshold">> => 2,
														<<"reporting_interval">> => 30,
														<<"survey_interval_ms">> => 50,
														<<"stats_type">> => <<"client">>
													},Acc)
											end,#{},APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Wifi_RRM_Config">> = TableName,APS)->
	Table = lists:foldl(fun(Band,Acc) ->
												NewUUID = utils:uuid_b(),
												maps:put( NewUUID,
												          #{
													          <<"_uuid">> => [<<"uuid">>,NewUUID],
													          <<"_version">> => utils:create_version(),
																		<<"probe_resp_threshold">> => 10,
													          <<"freq_band">> => utils:band_to_json(Band),
													          <<"min_load">> => 5,
													          <<"cell_size">> => 10,
													          <<"client_disconnect_threshold">> => 20,
													          <<"basic_rate">> => 100,
													          <<"backup_channel">> => maps:get(Band,APS#ap_state.hardware#hardware_info.channel_backup),
													          <<"snr_percentage_drop">> => 4
												          },Acc)
	                    end,#{},APS#ap_state.hardware#hardware_info.bands),
	APS#ap_state{ tables = maps:put(TableName,Table,APS#ap_state.tables)};

create_table(<<"Hotspot20_Config">> = TableName,APS)->
	% Table = #{ utils:uuid_b() => #{ <<"_version">> => utils:create_version()} },
	APS#ap_state{ tables = maps:put(TableName,#{},APS#ap_state.tables) };

create_table(<<"Hotspot20_OSU_Providers">> = TableName,APS)->
	% Table = #{ utils:uuid_b() => #{ <<"_version">> => utils:create_version()} },
	APS#ap_state{tables = maps:put(TableName,#{},APS#ap_state.tables)};

create_table(<<"Hotspot20_Icon_Config">> = TableName,APS)->
	% Table = #{ utils:uuid_b() => #{ <<"_version">> => utils:create_version()} },
	APS#ap_state{tables = maps:put(TableName,#{},APS#ap_state.tables)};

create_table(<<"Command_State">> = TableName,APS)->
	% Table = #{ utils:uuid_b() => #{ <<"_version">> => utils:create_version()} },
	APS#ap_state{tables = maps:put(TableName,#{},APS#ap_state.tables)}.

%%-----------------------------------------------------------------------------
%% helper functions
%%-----------------------------------------------------------------------------

-spec if_name(Index::integer()) -> RadioName::binary().
if_name(Index)->
	binary:list_to_bin([<<"radio">>,list_to_binary(integer_to_list(Index))]).

get_dhcp_fingerprint () ->
	FP = [<<"1,15,3,6,44,46,47,31,33,249,43">>,
	      <<"1,15,3,6,44,46,47,31,33,249,43,252">>,
	      <<"1,15,3,6,44,46,47,31,33,249,43,252,12">>,
	      <<"15,3,6,44,46,47,31,33,249,43">>,
	      <<"15,3,6,44,46,47,31,33,249,43,252">>,
	      <<"28,2,3,15,6,12,44,47">>,
	      <<"1,3,6,15,119,78,79,95,252">>,
	      <<"1,3,6,15,119,95,252,44,46,47">>],
	lists:nth(rand:uniform(length(FP)),FP).

-spec find_vif_config(IfName::binary(),APS::ap_state()) -> UUID::binary().
find_vif_config(IfName,APS)->
	VIFConfigTable = maps:get(<<"Wifi_VIF_Config">>,APS#ap_state.tables),
	maps:fold(fun(K,V,A) ->
								#{ <<"if_name">> := Interface } = V,
								case Interface == IfName of
									true -> K;
									false -> A
								end
						end,<<>>,VIFConfigTable).

find_inet_config_for_if_name(IFName,APS)->
	Entries = maps:get(<<"Wifi_Inet_Config">>,APS#ap_state.tables),
	maps:fold(fun(K,V,A) ->
							#{ <<"if_name">> := IF } = V,
							case IF == IFName of
								true -> K;
								false -> A
							end
						end, <<>>, Entries).

find_radio_config_for_interface(IFName,APS)->
	Entries = maps:get(<<"Wifi_Radio_Config">>,APS#ap_state.tables),
	maps:fold(fun(K,V,A) ->
		#{ <<"if_name">> := IF } = V,
		case IF == IFName of
			true -> K;
			false -> A
		end
  end, <<>>, Entries).

find_band_for_interface(IFName,APS)->
	Entries = maps:get(<<"Wifi_Radio_Config">>,APS#ap_state.tables),
	maps:fold(fun(_K,V,A) ->
							case maps:get(<<"if_name">>,V,undefined) of
								undefined ->
									A;
								IF ->
									case IF == IFName of
										true ->
											maps:get(<<"freq_band">>,V,<<>>);
										false ->
											A
									end
							end
	          end, <<>>, Entries).


