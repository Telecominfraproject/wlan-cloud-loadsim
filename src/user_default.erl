%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 3:11 p.m.
%%%-------------------------------------------------------------------
-module(user_default).
-author("stephb").

%% API
-compile(export_all).
-compile(nowarn_export_all).

-define(SIM_APIKEY,sim_api_key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Node commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec login( ApiKey :: string()) -> ok.
login(ApiKey)->
	persistent_term:put(?SIM_APIKEY,ApiKey).

-spec logout() -> ok.
logout()->
	persistent_term:erase(?SIM_APIKEY).

-spec connected() -> { ok , none } | { ok , Manager::node() }.
connected()->
	simnode:connected().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Manager commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec refresh_ouis()-> ok.
refresh_ouis()->
	oui_server:refresh().

-spec connected_nodes() -> {ok,[node()]}.
connected_nodes()->
	manager:connected_nodes().

