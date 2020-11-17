%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2020 9:53 a.m.
%%%-------------------------------------------------------------------
-author("stephb").

-type sim_command() :: start | stop | pause | create.
-type sim_operation_type() :: parallel | sequential.

-record( sim_operation, {
	uuid = "" :: string(),
	type = sequential :: sim_operation_type(),
	command = none :: none | sim_command(),
	clients_types = all :: all | inventory:client_role() | inventory:service_type(),
	clients = all :: all | [string()],
	all_commands = [] :: [pid()],
	call_back = none :: mfa() }).

-record( sim_operation_state, {
	pid :: pid(),
	step :: any(),
	start = 1 :: integer(),
	finish = 1 :: integer(),
	progress = 1 :: integer(),
	errors = 0 :: integer(),
	warnings = 0 :: integer() }).

-type sim_operation() :: #sim_operation{}.
-type sim_operation_state() :: #sim_operation_state{}.

-export_type([sim_command/0,sim_operation/0,sim_operation_state/0]).