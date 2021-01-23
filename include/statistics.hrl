%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 05. Dec 2020 10:27 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-ifndef(__STATISTICS_HRL__).
-define(__STATISTICS_HRL__,1).

-record( stat_report, {
	uuid = <<>> :: binary(),
	node :: node(),
	type :: atom(),
	timestamp :: erlang:timestamp(),
	report = #{} :: #{}
	}).

-record( stat_os_report, {
	cpu_avg1 :: number(),
	cpu_avg5 :: number(),
	cpu_avg15:: number(),
	number_of_processes :: number(),
	sysmem_high_watermark :: number(),
	procmem_high_watermark :: number(),
	cpu_utilization :: number(),
	mem_check_interval :: number(),
	mem_helper_timeout :: number(),
	disk_check_interval :: number(),
	memory_data = [] :: [ { atom(), number()} ],
	number_of_cpus :: number(),
	kernel_utilization :: number(),
	nice_user :: number(),
	user :: number(),
	idle :: number(),
	disk_almost_full_threshold :: number(),
	disk_details :: [{string(),number()}],
	system_memory_data :: [{ atom() , number() }],
	cpu_details :: [ [ { string(), number() }]]
}).

-type stat_report() :: #stat_report{}.
-type generic_stat_report()::#{atom()=>term()}.

-export_type([stat_report/0,generic_stat_report/0]).


-endif.