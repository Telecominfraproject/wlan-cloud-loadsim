%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2020 10:25 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-include("../include/common.hrl").

-record( simulation, { name = <<>> :: binary(),
	ca = <<>> :: binary(),
	num_devices = 0 :: integer(),
	assets_created = false :: boolean(),
	creation_date = <<>> :: binary(),
	start_date = <<>> :: binary(),
	end_date = <<>> :: binary(),
	nodes = [] :: [ node() ],
	internal = false :: boolean(),
  opensync_server_name = <<>> :: binary(),
  opensync_server_port = 6643 :: integer()
}).

-record(sim_action,{
	id = <<>> :: binary(),
	action = <<>> :: binary(),
	simulation = <<>> :: binary(),
	parameters = #{} :: #{atom()=>term()},
	status = <<>> :: binary(),
	created = <<>> :: binary(),
	done_count = 0 :: integer(),
	target_count :: integer(),
	completed = <<>> :: binary(),
	start_os_time = 1 :: pos_integer(),
	end_os_time = 1 :: pos_integer()}).

-record(sim_state,{
	pushed = false :: boolean(),
	current_op_pid = none :: none | pid(),
	current_op = none :: none | preparing | pushing | starting | pausing | stopping | restarting | cancelling ,
	state = created :: created | prepared | pushed | started | paused | stopped | restarted | cancelled ,
	start = 0 :: non_neg_integer(),
	current_cb = none :: none | notification_cb(),
	outstanding_nodes = [] :: [node()],
	sim_info :: simulation()
}).

-type simulation()::#simulation{}.
-type sim_action()::#sim_action{}.

-export_type([simulation/0,sim_action/0]).