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
	plan = <<>> :: binary(),    %% yaml plan for the simulation
	creation_date :: calendar:datetime(),
	start_date = undefined :: undefined | calendar:datetime(),
	end_date = undefined :: undefined | calendar:datetime(),
	nodes = [] :: [ node() ]
}).

-type simulation() :: #simulation{}.

-export_type([simulation/0]).