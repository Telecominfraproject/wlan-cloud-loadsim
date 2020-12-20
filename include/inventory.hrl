%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 12:16 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-ifndef(__OWLS_INVENTORY_HRL__).
-define(__OWLS_INVENTORY_HRL__,1).

-include("../include/common.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(ca_info,{
	name = <<>> :: binary(),   %% You must leave the NAME field first - do not move it...
	sim_name = <<>> :: binary(),
	description = <<>> :: binary(),
	dir_name = <<>> :: binary(),
	clients_dir_name = <<>> :: binary(),
	servers_dir_name = <<>> :: binary(),
	cert_file_name = <<>> :: binary(),
	key_file_name = <<>> :: binary(),
	config_file_name = <<>> :: binary(),
	key = {none,<<>>} :: {atom(), binary()},
	cert = <<>> :: binary(),
	decrypt_data = <<>> :: binary(),
	config_data = <<>> :: binary(),
	password = <<>> :: binary(),
	attributes = #{} :: attribute_list()
}).

-type client_role() :: none | mqtt_client | ovsdb_client.

-type wifi_band() :: 'BAND2G' | 'BAND5G' | 'BAND5GL' | 'BAND5GU' | undefined.
-type wifi_client() :: { Index::non_neg_integer(), Band::wifi_band(), SSID::binary(), MAC::binary(), VendorClass:: binary() }. %% MAC is XX:XX:XX:XX:XX:XX lowercase
-type lan_client() :: { Index::non_neg_integer(), Port::binary(), MAC::binary(), VendorClass::binary() }.  %% MAC is XX:XX:XX:XX:XX:XX lowercase

-record( client_info, {
	name = <<>> :: binary(),
	ca  = <<>> :: binary(),
	cap = [] :: [client_role()],
	wan_mac0 = <<>> :: binary(),   %% MAC is XX:XX:XX:XX:XX:XX lowercase
	lan_mac0 = <<>> :: binary(),   %% MAC is XX:XX:XX:XX:XX:XX lowercase
	serial = <<>> :: binary(),
	description = <<>> :: binary(),
	type = <<>> :: binary(),
	bands = [] :: [wifi_band()],
	wifi_clients = [] :: [ wifi_client()],
	lan_clients = [] :: [ lan_client()],
	id = <<>> :: binary(),              %% hardware ID
	key = {none,<<>>} :: {atom(), binary()},
	cert = <<>> :: binary(),
	decrypt = {none,<<>>} :: {atom(), binary()},
	csr = <<>> :: binary(),
	cacert = <<>> :: binary(),
	attributes = #{} :: attribute_list()
}).

-type service_role() :: none | mqtt_server | ovsdb_server .

-record( server_info, {
	name  = <<>> :: binary(),
	service = none :: service_role(),
	ca  = <<>> :: binary(),
	description  = <<>> :: binary(),
	type  = <<>> :: binary(),
	version  = <<>> :: binary(),
	ports = [] :: [integer()],
	addresses = [] :: [inet:ip_address()],
	key = {none,<<>>} :: {atom(), binary()},
	cert = <<>> :: binary(),
	decrypt = {none,<<>>} :: {atom(), binary()},
	csr = <<>> :: binary(),
	cacert = <<>> :: binary(),
	attributes = #{} :: attribute_list()
}).

-record( hardware_info, {
	id = <<>> :: binary(),
	description = <<>> :: binary(),
	vendor = <<>> :: binary(),
	model = <<>> :: binary(),
	capabilities = [] :: [ mqtt_client | ovsdb_client ],
	bands = [] :: [ 'BAND2G' | 'BAND5GL' | 'BAND5GU' ],
	channels_2G = [1,2,3,4,5,6,7,8,9,10,11] :: [pos_integer()],
	channels_5GL= [36,40,44,48,52,56,60,64] :: [pos_integer()],
	channels_5GU= [100,104,108,112,116,120,124,128,132,136,140,144,149,153,154,157,161,165] :: [pos_integer()],
	channel_default_2G = 6 :: pos_integer(),
	channel_default_5GU= 149 :: pos_integer(),
	channel_default_5GL= 36 :: pos_integer(),
	channel_backup_2G = 11 :: pos_integer(),
	channel_backup_5GU= 154 :: pos_integer(),
	channel_backup_5GL= 44 :: pos_integer(),
	number_of_radios = 2 :: pos_integer(),
	firmware_profile = <<"development">> :: binary(),
	firmware_date = <<"Mon Nov  2 09">> :: binary(),
	firmware_build = <<"0">> :: binary(),
	firmware_commit = <<"notgit">> :: binary(),
	firmware = <<"0.1.0-0-notgit-development">> :: binary(),
	opensync = <<"2.0.5.0">> :: binary(),
	core = <<"2.0.5.0/0/notgit">> :: binary(),
	firmware_image_active = <<"0.1.0-0-notgit-development">> :: binary(),
	firmware_image_inactive = <<"unknown">> :: binary(),
	firmware_version = <<"0.1.0">> :: binary(),
	firmware_host = <<"runner@72477083da86">> :: binary(),
	vendor_tip = <<"0.1.0/0/notgit">> :: binary()
}).

-type any_role() :: service_role() | client_role().
-type ca_info() :: #ca_info{}.
-type client_info() :: #client_info{}.
-type server_info() :: #server_info{}.
-type hardware_info() :: #hardware_info{}.

-export_type([client_info/0,server_info/0,ca_info/0,client_role/0,service_role/0,
	any_role/0,hardware_info/0]).

-endif.