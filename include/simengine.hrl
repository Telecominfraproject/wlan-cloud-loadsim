%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2020 10:25 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-record( simulation, { name = <<>> :: binary(),
	ca = <<>> :: binary(),
	num_devices = 0 :: integer(),
	assets_created = false :: boolean(),
	creation_date :: calendar:datetime(),
	start_date = undefined :: undefined | calendar:datetime(),
	end_date = undefined :: undefined | calendar:datetime(),
	nodes = [] :: [ node() ],
	internal = false :: boolean(),
  opensync_server_name = <<>> :: binary(),
  opensync_server_port = 6643 :: integer()
}).

-record(sim_action,{
	id = <<>> :: binary(),
	action = <<>> :: binary(),
	simulation = <<>> :: binary(),
	parameters = <<>> :: binary(),
	status = <<>> :: binary(),
	created,
	done_count = 0 :: integer(),
	target_count :: integer(),
	completed = <<>>}).

-type simulation() :: #simulation{}.
-type sim_action()::#sim_action{}.

-export_type([simulation/0,sim_action/0]).