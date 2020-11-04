%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2020 10:43 a.m.
%%%-------------------------------------------------------------------
-module(simmanager_cli).
-author("stephb").

%% API
-compile(export_all).
-compile(nowarn_export_all).

refresh_ouis()->
	oui_server:refresh().
