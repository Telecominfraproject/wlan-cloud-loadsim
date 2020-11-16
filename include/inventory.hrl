%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 12:16 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-type attribute_list() :: [{string(),string()}].
-type record_index() :: { string() , string() }.

-record(ca_info,{
	name = "" :: string(),   %% You must leave the NAME field first - do not move it...
	description = "" :: string(),
	dir_name = "" :: string(),
	clients_dir_name = "" :: string(),
	servers_dir_name = "" :: string(),
	cert_file_name = "" :: string(),
	key_file_name = "" :: string(),
	config_file_name = "" :: string(),
	cert_data = <<>> :: binary(),
	key_data = <<>> :: binary(),
	config_data = <<>> :: binary(),
	password = "" :: string(),
	attributes = [] :: attribute_list()
}).

-record( client_info, {
	index = {} :: record_index(),    %% should be a tuple { CA, Name }, must be in position 1
	ca = "" :: string(),
	name = "" :: string(),
	mac = "" :: string(),
	description = "" :: string(),
	type = "" :: string(),
	firmware = "" :: string(),
	vendor = "" :: string(),
	device_model = "" :: string(),
	key = "" :: string(),
	cert = "" :: string(),
	decrypt = "" :: string(),
	csr = "" :: string(),
	attributes = [] :: attribute_list()
}).

-record( server_info, {
	index = {} :: record_index(),    %% should be a tuple { CA, Name }, must be in position 1
	ca  = "" :: string(),
	name  = "" :: string(),
	description  = "" :: string(),
	type  = "" :: string(),
	version  = "" :: string(),
	ports = "" :: string(),
	addresses = "" :: list(string()),
	key = "" :: string(),
	cert = "" :: string(),
	decrypt = "" :: string(),
	csr = "" :: string(),
	attributes = [] :: attribute_list()
}).

-type ca_info() :: #ca_info{}.
-type client_info() :: #client_info{}.
-type server_info() :: #server_info{}.

-export_type([client_info/0,server_info/0,ca_info/0,record_index/0,attribute_list/0]).