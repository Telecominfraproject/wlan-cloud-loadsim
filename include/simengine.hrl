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

-type simulation()::#simulation{}.
-type sim_action()::#sim_action{}.

-export_type([simulation/0,sim_action/0]).