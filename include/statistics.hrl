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

-type stat_report() :: #stat_report{}.

-export_type([stat_report/0]).

-endif.