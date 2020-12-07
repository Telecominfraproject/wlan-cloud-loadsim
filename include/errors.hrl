%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 06. Dec 2020 12:31 p.m.
%%%-------------------------------------------------------------------
-author("stephb").


-ifndef(__ERRORS_HRL__).
-define(__ERRORS_HRL__,1).

-define(ERROR_SIM_UNKNOWN,{error,unknown_simulation_name}).
-define(ERROR_SIM_ALREADY_EXISTS,{error,simulation_already_exists}).
-define(ERROR_SIM_PREPARE_RUNNING,{error,preparation_already_in_progress}).
-define(ERROR_SIM_PUSH_RUNNING,{error,push_already_in_progress}).
-define(ERROR_SIM_ASSETS_ALREADY_CREATED,{error,assets_already_created}).
-define(ERROR_SIM_OPERATION_IN_PROGRESS,{error,sim_operation_already_in_progress}).


-endif.