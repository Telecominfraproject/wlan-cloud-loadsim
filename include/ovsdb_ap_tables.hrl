%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% Definition of the Access Point (AP) tables to wokr with TIP controller
%%% Note: since this will be converted to JSON all strings *MUST* be binaries
%%% 
%%% @end
%%% Created : 30. November 2020 @ 09:02:43
%%%-----------------------------------------------------------------------------
-author("helge").



%%------------------------------------------------------------------------------
%% the tables


-record ('AWLAN_Node',{
	'**key_id**' = <<>>:: binary() | ets_dont_care(),
	mqtt_settings = [<<"map">>,[]] :: term() | ets_dont_care(),
	sku_number = [<<"set">>,[]] :: term() | ets_dont_care(),
	model = <<>>:: term() | ets_dont_care(),
	version_matrix = [<<"map">>,[]] :: term() | ets_dont_care(),
	id = <<"">> :: term(),
	firmware_version = <<>> :: term() | ets_dont_care(),
	firmware_url = <<"">> :: term(),
	upgrade_dl_timer = 0 :: term(),
	platform_version = <<>> :: term() | ets_dont_care(),
	firmware_pass = <<"">> :: term(),
	upgrade_timer = 0 :: term(),
	max_backoff = 60 :: term(),
	led_config = [<<"map">>,[]] :: term(),
	redirector_addr = <<>> :: term() | ets_dont_care(),
	serial_number = <<>> :: term() | ets_dont_care(),
	'_version' = [<<"uuid">>,<<"33fad8ee-4cd8-4818-a247-a4161ce3f2b0">>] :: term(),
	mqtt_headers = [<<"map">>,[]] :: term(),
	min_backoff = 30 :: term(),
	device_mode = [<<"set">>,[]] :: term(),
	upgrade_status = 0 :: term(),
	revision = <<"1">> :: term() | ets_dont_care(),
	mqtt_topics = [<<"map">>,[]] :: term(),
	manager_addr = <<>> :: term() | ets_dont_care(),
	factory_reset = [<<"set">>,[]] :: term()
}).


-record ('Wifi_Stats_Config', {
	'**key_id**' = <<>>:: binary() | ets_dont_care(),
	'_version' = [<<"uuid">>,<<"4ad2c67d-99d6-4431-a6a7-09a0fa95b8e2">>] :: term(),
	radio_type = <<"2.4G">> :: term() | ets_dont_care(),
	sampling_interval = 10 :: term() | ets_dont_care(),
	report_type = <<"raw">> :: term(),
	threshold = [<<"map">>,[]] :: term(),
	survey_type = [<<"set">>,[]] :: term(),
	'_uuid' = [<<"uuid">>,<<"f84b6834-80d6-4fd6-af73-98e3f4f96033">>] :: term() | ets_dont_care(),
	channel_list = [<<"set">>,[]] :: term(),
	reporting_interval = 60 :: term() | ets_dont_care(),
	survey_interval_ms = 65 :: term() | ets_dont_care(),
	reporting_count = 0 :: term(),
	stats_type = <<"video_voice">> :: term() | ets_dont_care()
}).

-record ('Hotspot20_Config', {
	'**key_id**' = <<>>:: binary() | ets_dont_care()
}).

-record ('Hotspot20_OSU_Providers', {
	'**key_id**' = <<>>:: binary() | ets_dont_care()
}).

-record ('Hotspot20_Icon_Config', {
	'**key_id**' = <<>> :: binary() | ets_dont_care()
}).

-record ('Wifi_RRM_Config', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	'_version' = [<<"uuid">>,<<"9bbd18e7-ed7e-4ff3-b89d-a54c12b27ed7">>] :: term(),
	freq_band = <<"5GU">> :: term(),
	probe_resp_threshold = -90 :: term(),
	min_load = 40 :: term(),
	cell_size = [<<"set">>,[]] :: term(),
	client_disconnect_threshold = -90 :: term(),
	basic_rate = [<<"set">>,[]] :: term(),
	'_uuid' = [<<"uuid">>,<<"44deb01a-a2a8-4b5b-a2be-0bdf04050b97">>] :: term(),
	backup_channel = 154 :: term(),
	snr_percentage_drop = 30 :: term()
}).

-record ('Command_State', {
	'**key_id**' = <<>>:: binary() | ets_dont_care()
}).



-record ('Wifi_VIF_Config', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	vif_radio_idx = [<<"set">>,[]] :: term(),
	if_name = <<"wlan0">> :: term() | ets_dont_care(),
	ap_bridge = <<"">> :: term() | ets_dont_care(),
	wps_pbc = [<<"set">>,[]] :: term(),
	mac_list = <<"">> :: term() | ets_dont_care(),
	ssid = <<"TipWlan-cloud-wifi">> :: term() | ets_dont_care(),
	uapsd_enable = true :: term() | ets_dont_care(),
	btm = <<"">> :: term() | ets_dont_care(),
	'_uuid' = [<<"uuid">>,<<"312e96c1-2797-4551-9a18-37474e4d4b8b">>]:: term() | ets_dont_care(),
	ssid_broadcast = <<"enabled">> :: term() | ets_dont_care(),
	wps = [<<"set">>,[]] :: term(),
	mcast2ucast = [<<"set">>,[]] :: term(),
	mode = <<"ap">> :: term() | ets_dont_care(),
	mac_list_type = <<"">> :: term() | ets_dont_care(),
	wps_pbc_key_id = <<"">> :: term(),
	credential_configs = [<<"set">>,[]] :: term(),
	ft_psk = 0 :: term() | ets_dont_care(),
	parent = [<<"set">>,[]] :: term(),
	multi_ap = [<<"set">>,[]] :: term(),
	security = [<<"map">>,[[<<"encryption">>,<<"WPA-PSK">>],[<<"key">>,<<"w1r3l3ss-fr33d0m">>],[<<"mode">>,<<"2">>]]] :: term(),
	wds = [<<"set">>,[]] :: term(),
	enabled = true :: term() | ets_dont_care(),
	vlan_id = 1 :: term() | ets_dont_care(),
	min_hw_mode = <<"11ac">> :: term() | ets_dont_care(),
	vif_dbg_lvl = [<<"set">>,[]] :: term(),
	custom_options = [<<"map">>,[[<<"client_dl_limit">>,<<"0">>],[<<"client_ul_limit">>,<<"0">>],[<<"dtim_period">>,<<"2">>],[<<"ieee80211k">>,<<"1">>],[<<"rate_limit_en">>,<<"0">>],[<<"rts_threshold">>,<<"65535">>],[<<"ssid_dl_limit">>,<<"0">>],[<<"ssid_ul_limit">>,<<"0">>]]] :: term(),
	'_version' = [<<"uuid">>,<<"5e2e57ef-4212-479f-9c4c-f6f09f333d5f">>],
	captive_portal = [<<"map">>,[]] :: term(),
	bridge = <<"lan">> :: term() | ets_dont_care(),
	group_rekey = 0 :: term() | ets_dont_care(),
    ft_mobility_domain = [<<"set">>,[]] :: term(),
	captive_allowlist = [<<"set">>,[]] :: term(),
	dynamic_beacon = [<<"set">>,[]] :: term(),
	rrm = 1 :: term() | ets_dont_care()
}).


-record ('Wifi_VIF_State', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	if_name = <<"wlan0">> :: term(),
	vif_radio_idx = [<<"set">>,[]] :: term(),
	state = [<<"set">>,[]] :: term(),
	mac = <<>> :: term(),
	uapsd_enable = true :: term(),
	ap_bridge = true :: term(),
	mac_list = [<<"set">>,[]] :: term(),
	ssid = <<"TipWlan-cloud-wifi">> :: term(),
	wps_pbc = [<<"set">>,[]] :: term(),
	associated_clients = [<<"set">>,[]] :: term(),
	btm = 1 :: term(),
	ssid_broadcast = <<"enabled">> :: term(),
	mcast2ucast = [<<"set">>,[]] :: term(),
	wps = [<<"set">>,[]] :: term(),
	mode = <<"ap">> :: term(),
	mac_list_type = <<"none">> :: term(),
	wps_pbc_key_id = <<"">> :: term(),
	ft_psk = 0 :: term(),
	channel = 149 :: term(),
	parent = [<<"set">>,[]] :: term(),
	multi_ap = [<<"set">>,[]] :: term(),
	ap_vlan_sta_addr = [<<"set">>,[]] :: term(),
	security = [<<"map">>,[[<<"encryption">>,<<"WPA-PSK">>],[<<"key">>,<<"w1r3l3ss-fr33d0m">>],[<<"mode">>,<<"2">>]]] :: term(),
	wds = [<<"set">>,[]] :: term(),
	enabled = true :: term(),
	vlan_id = 1 :: term(),
	custom_options = [<<"map">>,[[<<"client_dl_limit">>,<<"0">>],
								[<<"client_ul_limit">>,<<"0">>],
								[<<"dtim_period">>,<<"2">>],
								[<<"ieee80211k">>,<<"1">>],
								[<<"rate_limit_en">>,<<"0">>],
								[<<"rts_threshold">>,<<"65535">>],
								[<<"ssid_dl_limit">>,<<"0">>],
								[<<"ssid_ul_limit">>,<<"0">>]]] :: term(),
	min_hw_mode = <<"11ac">> :: term(),
	vif_config = [<<"uuid">>,<<"bc95a046-00de-4b57-9d41-ef3cca56a1b5">>] :: term(),
	'_version' = [<<"uuid">>,<<"7e213f7e-aee9-45b1-bd3b-6c9f92531d8b">>] :: term(),
	captive_portal = [<<"map">>,[]] :: term(),
	bridge = <<"lan">> :: term(),
	group_rekey = 0 :: term(),
	ft_mobility_domain = [<<"set">>,[]] :: term(),
	captive_allowlist = [<<"set">>,[]] :: term(),
	rrm = 1 :: term(),
	dynamic_beacon = [<<"set">>,[]] :: term()
}).

-record ('Wifi_Associated_Clients', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	'_version' = [<<"uuid">>,<<"5bc3eb0f-1cc3-4dae-aae5-af02c8d2f1c7">>] :: term(),
	mac = <<"">> :: term(),
	state = <<"">> :: term(),
	uapsd= [<<"set">>,[]] :: term(),
	capabilities= [<<"set">>,[<<"11ac">>,<<"11n">>,<<"11ab">>]] :: term(),
	kick = [<<"map">>,[]] :: term(),
	oftag = [<<"set">>,[]] :: term()
}).


-record ('DHCP_leased_IP', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	db_status = 1 :: term(),
	subnet_mask = <<"255.255.255.0">> :: term(),
	hostname = <<"">> :: term(),
	secondary_dns = <<"0.0.0.0">> :: term(),
	inet_addr = <<"">> :: term(),
	lease_time = 43200 :: term(),
	hwaddr = <<"">> :: term(),
	'_version' = [<<"uuid">>,<<"463c9bfd-e539-419c-83d2-6048a1a9e2a7">>] :: term(),
	manuf_id = 0 :: term(),
	vendor_class = <<"">> :: term(),
	device_type = 0 :: term(),
	dhcp_server = <<"192.168.1.1">> :: term(),
	device_name = <<"Simulation">> :: term(),
	fingerprint = <<"1,121,3,6,15,114,119,252">> :: term(),
	primary_dns = <<"192.168.1.1">> :: term(),
	gateway = <<"192.168.1.1">> :: term()
}).

-record ('Wifi_Radio_Config', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	dfs_demo = [<<"set">>,[]] :: term(),
	if_name = <<"radio0">> :: term() | ets_dont_care(),
	temperature_control = [<<"map">>,[]] :: term(),
	tx_power = 18 :: term() | ets_dont_care(),
	thermal_downgrade_temp = [<<"set">>,[]] :: term(),
	ht_mode = <<"HT80">> :: term() | ets_dont_care(),
	bcn_int = 100 :: term() | ets_dont_care(),
	zero_wait_dfs = [<<"set">>,[]] :: term(),
	thermal_tx_chainmask = [<<"set">>,[]] :: term(),
	hw_mode = <<"11ac">> :: term(),
	enabled = true :: term() | ets_dont_care(),
	channel_sync = [<<"set">>,[]] :: term(),
	thermal_shutdown = [<<"set">>,[]] :: term(),
	thermal_upgrade_temp = [<<"set">>,[]] :: term(),
	custom_options = [<<"map">>,[]] :: term(),
	'_uuid' = [<<"uuid">>,<<"5b0c1d91-a642-4356-a6bd-dfc7cab11edc">>] :: term() | ets_dont_care(),
	'_version' = [<<"uuid">>,<<"9274f405-bd49-47c7-a8a3-265358d730a1">>],
	freq_band = <<"5GU">> :: term() | ets_dont_care(),
	hw_type = <<"ath10k">> :: term() | ets_dont_care(),
	thermal_integration = [<<"set">>,[]] :: term(),
	channel_mode = [<<"set">>,[]] :: term(),
	vif_configs = [<<"set">>,[]] :: term(),
	country = <<"US">> :: term() | ets_dont_care(),
	tx_chainmask = [<<"set">>,[]] :: term(),
	fallback_parents = [<<"map">>,[]] :: term(),
	hw_config = [<<"map">>,[[<<"dfs_enable">>,<<"1">>],[<<"dfs_ignorecac">>,<<"0">>],[<<"dfs_usenol">>,<<"1">>]]],
	channel = 149 :: term() | ets_dont_care()
}).

-record ('Wifi_Radio_State',{
	'**key_id**' = <<"">>:: binary() | ets_dont_care(),
	if_name = <<>> :: term(),
	dfs_demo = [<<"set">>,[]] :: term(),
	thermal_downgraded = [<<"set">>,[]] :: term(),
	temperature_control = [<<"map">>,[]] :: term(),
	mac = <<>> :: term(),
	bcn_int = 0 :: term(),
	allowed_channels = [<<"set">>,[]] :: term(),
	radio_config = [<<"uuid">>,<<"830bd195-7114-4e99-9b51-5622e47ce221">>] :: term(),
	thermal_tx_chainmask = [<<"set">>,[]] :: term(),
	channel_sync = [<<"set">>,[]] :: term(),
	thermal_shutdown = [<<"set">>,[]] :: term(),
	hw_type = [<<"set">>,[]] :: term(),
	vif_states = [<<"uuid">>,<<"87f75538-67d0-408a-9c8b-018665754d48">>] ::term(),
	country = <<>> :: term(),
	radar = [<<"map">>,[]] :: term(),
	tx_chainmask = 0 :: term(),
	fallback_parents = [<<"map">>,[]] :: term(),
	hw_config = [<<"map">>,[[<<"dfs_enable">>,<<"1">>],[<<"dfs_ignorecac">>,<<"0">>],[<<"dfs_usenol">>,<<"1">>]]] :: term(),
	channel = 0 :: term(),
	tx_power = 18 :: term(),
	ht_mode = <<"HT80">> :: term(),
	thermal_downgrade_temp = [<<"set">>,[]] :: term(),
	hw_mode = <<"11ac">> :: term(),
	zero_wait_dfs = [<<"set">>,[]] :: term(),
	enabled = true :: term(),
	hw_params = [<<"map">>,[]] :: term(),
	thermal_upgrade_temp = [<<"set">>,[]] :: term(),
	channels = [<<"map">>,[]] :: term(),
	custom_options = [<<"map">>,[]] :: term(),
	'_version' = [<<"uuid">>,<<"c325d603-ac42-43b5-a2e0-0b65c73888c6">>] :: term(),
	freq_band = <<"5GU">> :: term(),
	channel_mode = [<<"set">>,[]] :: term(),
	thermal_integration = [<<"set">>,[]] :: term()
}).

-record ('Wifi_Inet_Config', {
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	if_name = <<"">> :: term(),
	dhcpd = [<<"map">>,[]] :: term(),
	dhcp_sniff = false :: term(),
	upnp_mode = [<<"set">>,[]] :: term(),
	if_type = <<"eth">> :: term(),
	netmask = [<<"set">>,[]] :: term(),
	softwds_wrap = [<<"set">>,[]] :: term(),
	'_uuid' = [<<"uuid">>,<<"70c9636c-e410-4c13-80e5-fa32e6efc299">>] :: term(),
	'NAT' = true :: term(),
    network = true :: term(),
	mtu = [<<"set">>,[]] :: term(),
	igmp_proxy = [<<"set">>,[]] :: term(),
	ppp_options = [<<"map">>,[]] :: term(),
	igmp_tsize = [<<"set">>,[]] :: term(),
	broadcast = [<<"set">>,[]] :: term(),
	gre_ifname = [<<"set">>,[]] :: term(),
	gateway = [<<"set">>,[]] :: term(),
	gre_local_inet_addr = [<<"set">>,[]] :: term(),
	igmp_age = [<<"set">>,[]] :: term(),
	softwds_mac_addr = [<<"set">>,[]] :: term(),
	enabled = true :: term(),
	gre_remote_mac_addr = [<<"set">>,[]] :: term(),
	vlan_id = [<<"set">>,[]] :: term(),
	gre_remote_inet_addr = [<<"set">>,[]] :: term(),
	if_uuid = <<"">>,
	inet_addr = [<<"set">>,[]] :: term(),
	igmp = [<<"set">>,[]] :: term(),
	'_version' = [<<"uuid">>,<<"cc9fd80f-9b01-40bf-80e6-37ca62b86b67">>] :: term(),
	dns = [<<"map">>,[]] :: term(),
	mld_proxy = [<<"set">>,[]] :: term(),
	ip_assign_scheme = <<"dhcp">> :: term()	
}).

-record ('Wifi_Inet_State',{
	'**key_id**' = <<>> :: binary() | ets_dont_care(),
	dhcpd = [<<"map">>,[]] :: term(),
	if_name = <<"">> :: term(),
	upnp_mode = [<<"set">>,[]] :: term(),
	softwds_mac_addr= [<<"set">>,[]] :: term(),
	if_type = <<"eth">> :: term(),
	enabled = false :: term(),
	softwds_wrap = false :: term(),
	vlan_id = [<<"set">>,[]] :: term(),
	netmask = [<<"set">>,[]] :: term(),
	'NAT' = false :: term(),
	gre_remote_inet_addr = [<<"set">>,[]] :: term(),
	if_uuid = <<"">>,
	inet_addr = [<<"set">>,[]] :: term(),
	'_version' = [<<"uuid">>,<<"0b10958d-9bfb-45e5-9c36-ad8327750607">>] :: term(),
	hwaddr = <<"">> :: term(),
	network = false :: term(),
	mtu = [<<"set">>,[]] :: term(),
	parent_ifname = [<<"set">>,[]] :: term(),
	dns = [<<"map">>,[]] :: term(),
	broadcast = [<<"set">>,[]] :: term(),
	gre_ifname = [<<"set">>,[]] :: term(),
	dhcpc = [<<"map">>,[]] :: term(),
	ip_assign_scheme = <<"">> :: term(),
	gateway = [<<"set">>,[]] :: term(),
	inet_config = [<<"uuid">>,<<"7e38a63b-526a-4b83-b30e-edd4c17ab3f6">>] :: term(),
	gre_local_inet_addr = [<<"set">>,[]] :: term()
}).

%%------------------------------------------------------------------------------
%% monitor table

-record (monitors, {
	namespace :: binary() | ets_dont_care(),
	table :: binary() | ets_dont_care(),
	initial :: boolean() | ets_dont_care(),
	insert :: boolean() | ets_dont_care(),
	delete :: boolean() | ets_dont_care(),
	modify :: boolean() | ets_dont_care(),
	published = true :: boolean() | ets_dont_care()
}).

-record (to_publish, {
	table = <<>> :: binary()			| ets_dont_care(),
	old_version = <<>> :: binary()		| ets_dont_care(),
	new_version = <<>> :: binary()		| ets_dont_care(),
	row_key = <<>> :: binary()			| ets_dont_care(),
	old_values =  #{} :: #{binary()=>any}	| ets_dont_care(),
	new_values =  #{} :: #{binary()=>any}	| ets_dont_care()
}).

