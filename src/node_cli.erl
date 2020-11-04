%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Nov 2020 9:27 p.m.
%%%-------------------------------------------------------------------
-module(node_cli).
-author("stephb").

-compile(export_all).
-compile(nowarn_export_all).

connect(Node)->
	io:format("Connecting to node: ~p~n",[Node]).

disconnect(Node)->
	io:format("Disconnecting from node: ~p~n",[Node]).
