%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2020 4:26 p.m.
%%%-------------------------------------------------------------------
-module(gen_sim_client).
-author("stephb").

%% API
-export([]).

-callback set_configuration( Configuration :: #{} ) -> ok | { error, Reason::term() }.
-callback pause( all | [UUID::string()]) -> ok | { error, Reason::term() }.
-callback resume( all | [UUID::string()]) -> ok | { error, Reason::term() }.
-callback start( all | [UUID::string()]) -> ok | { error, Reason::term() }.
-callback stop( all | [UUID::string()]) -> ok | { error, Reason::term() }.
-callback cancel( all | [UUID::string()]) -> ok | { error, Reason::term() }.