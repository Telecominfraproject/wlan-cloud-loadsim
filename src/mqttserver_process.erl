%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 10:14 p.m.
%%%-------------------------------------------------------------------
-module(mqttserver_process).
-author("stephb").

-include("../include/mqtt_definitions.hrl").
-include("../include/internal.hrl").

%% API
-export([process/2]).

process(Data,State)->
	io:format("Data (~p bytes)>>> ~p~n",[size(Data),Data]),
	Msg = message:decode(Data),
	io:format("Message: ~p~n",[Msg]),
	answer_msg( Msg, State).

answer_msg( Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED, properties = [] },
	Msg = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Msg),
	{ Blob, State}.
