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
	'DATE' = <<"Mon Nov  2 09">> :: binary(),
	'FIRMWARE' = <<"0.1.0-0-notgit-development">> :: binary(),
	'FW_BUILD' = <<"0">> :: binary(),
	'FW_COMMIT' = <<"notgit">> :: binary(),
	'FW_IMAGE_ACTIVE' = <<"ea8300-2020-11-02-pending-97ebe9d">> :: binary(), 
	'FW_IMAGE_INACTIVE' = <<"unknown">> :: binary(),
	'FW_PROFILE' = <<"development">> :: binary(),
	'FW_VERSION' = <<"0.1.0">> :: binary(),
	'HOST' = <<"runner@72477083da86">> :: binary(),
	'OPENSYNC' = <<"2.0.5.0">> :: binary(),
	'core' = <<"2.0.5.0/0/notgit">> :: binary(),
	'vendor/tip' = <<"0.1.0/0/notgit">> :: binary()
}).



%%------------------------------------------------------------------------------
%% the tables


-record ('AWLAN_Node',{
	row_idx = 0 :: integer(),
	mqtt_settings = [map,[]] :: [map|[proplists:proplist()]],
	redirector_addr = <<>> :: binary(),
	manager_addr = <<>> :: binary(),
	sku_number = [set,[]] :: [set|[proplists:proplist()]],
	serial_number = <<>> :: binary(),
	model = <<>>:: binary(),
	firmware_version = <<>> :: binary(),
	platform_version = <<>> :: binary(),
	revision = <<>> :: binary(),
	version_matrix = [map,[]] :: [map|[proplists:proplist()]]
}).

