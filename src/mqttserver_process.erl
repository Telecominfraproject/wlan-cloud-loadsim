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

-define(DBG,io:format("F=~p L=~p~n",[?FUNCTION_NAME,?LINE])).

%% API
-export([process/1]).

process(#mqtt_processor_state{ bytes_left = <<>> }=State) ->
	{ ok , State };
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,1:1,V:15,Rest/binary>>}=State) ->
?DBG,
	{ RemainingLength , <<>> } = mqttlib:dec_varint(<<1:1,V:15>>),
	?DBG,
	io:format("Bytes left=~p  Remaining=~p~n",[size(State#mqtt_processor_state.bytes_left),RemainingLength]),
	case size(Rest) >= RemainingLength of
		true ->
			?DBG,
			PacketLength=RemainingLength+3,
			<< CurrentPacket:PacketLength/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = message:decode( CurrentPacket , State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			?DBG,
			process(NewState);
		false ->
			?DBG,
			{ ok, State }
	end;
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,RemainingLength:8,Rest/binary>>}=State) ->
	?DBG,
	io:format("Bytes left=~p  Remaining=~p~n",[size(State#mqtt_processor_state.bytes_left),RemainingLength]),
	case size(Rest) >= RemainingLength of
		true ->
			?DBG,
			PacketLength = RemainingLength+2,
			<< CurrentPacket:PacketLength/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = message:decode( CurrentPacket, State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			?DBG,
			{ ok, State }
	end;
process(State) ->
	?DBG,
	{ok,State}.

answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_3_11 }=Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header_v4{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version }};

answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_5 }=Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header_v5{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version }};

answer_msg( Msg, State ) when is_record(Msg,mqtt_pingreq_variable_header_v4) ->
	VariableHeader = #mqtt_pingresp_variable_header_v4{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	{ok,State};
answer_msg( Msg, State ) when is_record(Msg,mqtt_pingreq_variable_header_v5) ->
	VariableHeader = #mqtt_pingresp_variable_header_v5{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	{ok,State};

answer_msg(Msg,State) when is_record(Msg,mqtt_publish_variable_header_v4) ->
	io:format("PUBLISH: ~p~n",[binary_to_list(Msg#mqtt_publish_variable_header_v4.payload)]),
	{ok,State};

answer_msg( Msg, State ) ->
	io:format("MSG->~p~n",[Msg]),
	{ ok, State }.
