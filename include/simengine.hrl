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
	node :: node(),
	reflector_server_name = auto :: auto | binary(),
	reflector_server_port = 6643 :: integer(),
	opensync_server_name = auto :: auto | binary(),
	opensync_server_port = 6640 :: integer(),
	mqtt_server_name = auto :: auto | binary(),
	mqtt_server_port = 1883 :: integer(),
	names = [] :: [binary()]
}).

-record( simulation, { name = <<>> :: binary(),
	ca = <<>> :: binary(),
	num_devices = 0 :: integer(),
	assets_created = false :: boolean(),
	creation_date :: calendar:datetime(),
	start_date = undefined :: undefined | calendar:datetime(),
	end_date = undefined :: undefined | calendar:datetime(),
	nodes = [] :: [ node() ],
	servers = auto :: auto | sim_entry()
}).

-type simulation() :: #simulation{}.
-type sim_entry()::#sim_entry{}.

-export_type([simulation/0,sim_entry/0]).