%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2020 10:51 a.m.
%%%-------------------------------------------------------------------
-module(mqtt_client).
-author("stephb").

-include("../include/common.hrl").
-include("../include/inventory.hrl").

%% API
-export([start/4]).

-record(client_state,{manager_pid, topics, compress, configuration, details}).

start(CAName,Id,Configuration,ManagerPid)->
	#{ <<"broker">> := Broker, <<"compress">> := Compress, <<"port">> := Port, <<"topics">> := Topics } = Configuration,
	{ok,DeviceConfiguration} = inventory:get_client(CAName,Id),
	{ok,SSL}=ssl:connect(binary_to_list(Broker),list_to_integer(binary_to_list(Port)),
	                     [ {cert,DeviceConfiguration#client_info.cert}, {key,DeviceConfiguration#client_info.key}]),
	CS = #client_state{ manager_pid = ManagerPid, topics = Topics, compress = Compress , configuration = Configuration, details = DeviceConfiguration },
	run_client(SSL,CS).

run_client(SSL,CS)->
	ok.
