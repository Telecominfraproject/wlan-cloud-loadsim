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
-export([process/1]).

process(#mqtt_processor_state{ bytes_left = <<>> }=State) ->
	{ ok , State };
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,1:1,V:15,Rest/binary>>}=State) ->
	RemainingLength = mqttlib:dec_varint(<<1:1,V:15>>),
	case size(Rest) >= RemainingLength of
		true ->
			<< CurrentPacket:(RemainingLength+3)/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = message:decode( CurrentPacket ),
			{ ok , NewState } = answer_msg(Msg,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			{ ok, State }
	end;
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,RemainingLength:8,Rest/binary>>}=State) ->
	case size(Rest) >= RemainingLength of
		true ->
			<< CurrentPacket:(RemainingLength+2)/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = message:decode( CurrentPacket ),
			{ ok , NewState } = answer_msg(Msg,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			{ ok, State }
	end;
process(State) ->
	{ok,State}.

answer_msg( Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED, properties = [] },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	{ok,State};

answer_msg( Msg, State ) ->
	io:format("MSG->~p~n",[Msg]),
	{ ok, State }.
