%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2020 10:25 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-record( sim_entry, {
	name = <<>> :: binary(),
	type = none :: none | clients | mqtt_clients | ovsdb_clients | mqtt_server | ovsdb_server,
	node :: node(),
	ip :: inet:ip_address(),
	port :: integer(),
	port_reflector :: integer(),
	names = [] :: [binary()]
}).

-record( simulation, { name = <<>> :: binary(),
	ca = <<>> :: binary(),
	num_devices = 0 :: integer(),
	creation_date :: calendar:datetime(),
	start_date = undefined :: undefined | calendar:datetime(),
	end_date = undefined :: undefined | calendar:datetime(),
	nodes = [] :: [ node() ],
	mqtt_servers = auto :: auto | sim_entry(),
  ovsdb_servers = auto :: auto | sim_entry()
}).

-type simulation() :: #simulation{}.
-type sim_entry()::#sim_entry{}.

-export_type([simulation/0,sim_entry/0]).