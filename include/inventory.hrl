%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 12:16 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-record(ca_info,{
	name,   %% You must leave the NAME field first - do not move it...
	dir,
	clients,
	servers,
	cert,
	key,
	config,
	password
}).

-record( client_info, {
	index,    %% should be a tuple { CA, Name }, must be in position 1
	ca,
	name,
	mac,
	description,
	type,
	firmware,
	vendor,
	device_model,
	key,
	cert,
	decrypt,
	csr,
	attributes
}).

-record( server_info, {
	index,    %% should be a tuple { CA, Name }, must be in position 1
	ca,
	name,
	description,
	type,
	version,
	ports,
	addresses,
	key,
	cert,
	decrypt,
	csr,
	attributes
}).

-type ca_info() :: #ca_info{}.
-type client_info() :: #client_info{}.
-type server_info() :: #server_info{}.

-export_type([client_info/0,server_info/0,ca_info/0]).