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
%% auxiliary types

-record (version_matrix_map, {
	'DATE' = <<"Mon Nov  2 09">> :: term(),
	'FIRMWARE' = <<"0.1.0-0-notgit-development">> :: term(),
	'FW_BUILD' = <<"0">> :: term(),
	'FW_COMMIT' = <<"notgit">> :: term(),
	'FW_IMAGE_ACTIVE' = <<"ea8300-2020-11-02-pending-97ebe9d">> :: term(), 
	'FW_IMAGE_INACTIVE' = <<"unknown">> :: term(),
	'FW_PROFILE' = <<"development">> :: term(),
	'FW_VERSION' = <<"0.1.0">> :: term(),
	'HOST' = <<"runner@72477083da86">> :: term(),
	'OPENSYNC' = <<"2.0.5.0">> :: term(),
	'core' = <<"2.0.5.0/0/notgit">> :: term(),
	'vendor/tip' = <<"0.1.0/0/notgit">> :: term()
}).



%%------------------------------------------------------------------------------
%% the tables


-record ('AWLAN_Node',{
	row_idx = 0 :: integer() | ets_dont_care(),
	mqtt_settings = [<<"map">>,[]] :: term() | ets_dont_care(),
	redirector_addr = <<>> :: term() | ets_dont_care(),
	manager_addr = <<>> :: term() | ets_dont_care(),
	sku_number = [<<"set">>,[]] :: term() | ets_dont_care(),
	serial_number = <<>> :: term() | ets_dont_care(),
	model = <<>>:: term() | ets_dont_care(),
	firmware_version = <<>> :: term() | ets_dont_care(),
	platform_version = <<>> :: term() | ets_dont_care(),
	revision = <<>> :: term() | ets_dont_care(),
	version_matrix = [<<"map">>,[]] :: term() | ets_dont_care()
}).


-record ('Wifi_Inet_State',{
	row_idx = 0 :: integer(),
	inet_addr = <<>> :: term(),
    hwaddr = <<>> :: term(),
    if_name = <<>> :: term(),
    if_type = <<>> :: term()
}).


-record ('Wifi_Radio_State',{
	row_idx = 0 :: integer() | ets_dont_care(),
	freq_band = <<>> :: term(),
	if_name = <<>> :: term(),
	allowed_channels = <<>>  :: term()
}).

-record ('Wifi_Stats_Config', {
	row_idx = 0 :: integer() | ets_dont_care(),
	channel_list = <<"">> :: term() | ets_dont_care(),
	radio_type = <<"">> :: term() | ets_dont_care(),
	reporting_interval = <<"">> :: term() | ets_dont_care(),
	sampling_interval = <<"">> :: term() | ets_dont_care(),
	stats_type = <<"">> :: term() | ets_dont_care(),
	survey_interval_ms = <<"">> :: term() | ets_dont_care(),
	survey_type = <<"">> :: term() | ets_dont_care(),
	threshold = <<"">> :: term() | ets_dont_care(),
	'_uuid' = <<"">> :: term() | ets_dont_care()
}).

-record ('Hotspot20_Config', {
	row_idx = 0 :: integer() | ets_dont_care()
}).

-record ('Hotspot20_OSU_Providers', {
	row_idx = 0 :: integer() | ets_dont_care()
}).

-record ('Hotspot20_Icon_Config', {
	row_idx = 0 :: integer() | ets_dont_care()
}).

-record ('Wifi_RRM_Config', {
	row_idx = 0 :: integer() | ets_dont_care()
}).

-record ('Wifi_VIF_Config', {
	row_idx = 0 :: integer() | ets_dont_care(),
	bridge = <<"">> :: term() | ets_dont_care(),
    ap_bridge = <<"">> :: term() | ets_dont_care(),
	'_uuid' = <<"">> :: term() | ets_dont_care(),
	btm = <<"">> :: term() | ets_dont_care(),
	enabled = <<"">> :: term() | ets_dont_care(),
	ft_psk = <<"">> :: term() | ets_dont_care(),
	ft_mobility_domain = <<"">> :: term() | ets_dont_care(),
	group_rekey = <<"">> :: term() | ets_dont_care(),
	if_name = <<"">> :: term() | ets_dont_care(),
	min_hw_mode = <<"">> :: term() | ets_dont_care(),
	mode = <<"">> :: term() | ets_dont_care(),
	rrm = <<"">> :: term() | ets_dont_care(),
	ssid = <<"">> :: term() | ets_dont_care(),
	ssid_broadcast = <<"">> :: term() | ets_dont_care(),
	uapsd_enable = <<"">> :: term() | ets_dont_care(),
	vif_radio_idx = <<"">> :: term() | ets_dont_care(),
	security = <<"">> :: term() | ets_dont_care(),
	vlan_id = <<"">> :: term() | ets_dont_care(),
	mac_list = <<"">> :: term() | ets_dont_care(),
	mac_list_type = <<"">> :: term() | ets_dont_care()
}).


-record ('Wifi_Radio_Config', {
	row_idx = 0 :: integer() | ets_dont_care(),
	'_uuid' = <<"">> :: term() | ets_dont_care(),
	if_name = <<"">> :: term() | ets_dont_care(),
	bcn_int = <<"">> :: term() | ets_dont_care(),
	channel = <<"">> :: term() | ets_dont_care(),
	channel_mode = <<"">> :: term() | ets_dont_care(),
	country = <<"">> :: term() | ets_dont_care(),
	enabled = <<"">> :: term() | ets_dont_care(),
	ht_mode = <<"">> :: term() | ets_dont_care(),
	tx_power = <<"">> :: term() | ets_dont_care(),
	vif_configs = <<"">> :: term() | ets_dont_care(),
	freq_band = <<"">> :: term() | ets_dont_care(),
	hw_config = <<"">> :: term() | ets_dont_care(),
	hw_type = <<"">> :: term() | ets_dont_care()
}).

-record ('Wifi_Inet_Config', {
	row_idx = 0 :: integer() | ets_dont_care(),
	'NAT' = <<"">> :: term() | ets_dont_care(),
	'_uuid' = <<"">> :: term() | ets_dont_care(),
	broadcast = <<"">> :: term() | ets_dont_care(),
	enabled = <<"">> :: term() | ets_dont_care(),
	if_name = <<"">> :: term() | ets_dont_care(),
	if_type = <<"">> :: term() | ets_dont_care(),
	ip_assign_scheme = <<"">> :: term() | ets_dont_care(),
	network = <<"">> :: term() | ets_dont_care(),
	inet_addr = <<"">> :: term() | ets_dont_care(),
	mtu = <<"">> :: term() | ets_dont_care(),
	netmask = <<"">> :: term() | ets_dont_care(),
	vlan_id = <<"">> :: term() | ets_dont_care(),
	gateway = <<"">> :: term() | ets_dont_care(),
	dns = <<"">> :: term() | ets_dont_care(),
	dhcpd = <<"">> :: term() | ets_dont_care(),
	parent_ifname = <<"">>  :: term() | ets_dont_care()
}).

