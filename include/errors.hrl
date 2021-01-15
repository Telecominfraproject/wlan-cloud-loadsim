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
-define(ERROR_SIM_NO_ASSETS_EXIST,{error,no_simulation_assets}).
-define(ERROR_SIM_ASSETS_ALREADY_PUSHED,{error,assets_already_pushed_to_nodes}).
-define(ERROR_SIM_ASSETS_NOT_PUSHED,{error,assets_not_pushed}).
-define(ERROR_SIM_ALREADY_STARTED,{error,already_starated}).
-define(ERROR_SIM_MUST_BE_STARTED_OR_PAUSED,{error,must_be_started_or_paused}).
-define(ERROR_SIM_MUST_BE_STARTED,{error,must_be_started}).
-define(ERROR_SIM_MUST_BE_STARTED_OR_PAUSED_OR_STOPPED,{error,must_be_started_paused_or_stopped}).
-define(ERROR_SIM_MUST_BE_PAUSED_OR_STOPPED,{error,must_be_paused_or_stopped}).
-define(ERROR_SIM_ACTION_UNKNOWN,{error,sim_action_unknown}).
-define(ERROR_CA_CANNOT_IMPORT_KEY,{error,key_not_imported}).
-define(ERROR_CA_UNKNOWN,{error,ca_unknown}).

-endif.