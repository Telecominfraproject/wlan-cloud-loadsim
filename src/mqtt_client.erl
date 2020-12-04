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
-include("../include/mqtt_definitions.hrl").

%% API
-export([start/4]).

-record(client_state,{id, manager_pid, topics, compress, configuration, details}).

start(CAName,Id,Configuration,ManagerPid)->
	#{ <<"broker">> := Broker, <<"compress">> := Compress, <<"port">> := Port, <<"topics">> := Topics } = Configuration,
	{ok,DeviceConfiguration} = inventory:get_client(CAName,Id),
	{ok,SSL}=ssl:connect(binary_to_list(Broker),list_to_integer(binary_to_list(Port)),
   [
		{session_tickets,auto},
		{versions, ['tlsv1.2','tlsv1.3']},
		{cert,DeviceConfiguration#client_info.cert},
		{key,DeviceConfiguration#client_info.key},
		{active,false },
		binary]),
	CS = #client_state{ id=Id, manager_pid = ManagerPid, topics = Topics, compress = Compress , configuration = Configuration, details = DeviceConfiguration },
	run_client(SSL,CS).

run_client(SSL,CS)->
	C = #mqtt_connect_variable_header{
		protocol_version = ?MQTT_PROTOCOL_VERSION_3_11,
		username_flag = 0,
		password_flag = 0,
		will_retain_flag = 0,
		will_qos_flag = 0,
		will_flag = 0 ,
		clean_start_flag = 1,
		client_identifier = CS#client_state.id,
		keep_alive = 60	},
	M = #mqtt_msg{ variable_header = C},
	ConnectMessage = mqtt_message:encode(M),

	_ = ssl:send(SSL,ConnectMessage),
	ok.
