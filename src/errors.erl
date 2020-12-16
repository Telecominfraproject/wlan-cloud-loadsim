%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2020 11:45 p.m.
%%%-------------------------------------------------------------------
-module(errors).
-author("stephb").

%% API
-export([to_string/1]).

to_string(unknown_ca)-> "Unknown CA.".

