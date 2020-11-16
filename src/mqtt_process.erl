%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 10:14 p.m.
%%%-------------------------------------------------------------------
-module(mqtt_process).
-author("stephb").

-dialyzer(specdiffs).

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").

-define(INCREMENT_STATS1(X,Y),X#mqtt_connection_stats{ Y = X#mqtt_connection_stats.Y+1}).
-define(INCREMENT_STATS2(X,Y,Z),X#mqtt_connection_stats{ Y = X#mqtt_connection_stats.Y+1, Z = X#mqtt_connection_stats.Z+1}).

%% API
-export([process/1]).

-spec process(State::mqtt_processor_state()) -> { ok , NewState::mqtt_processor_state() }.
process(#mqtt_processor_state{ bytes_left = <<>> }=State) ->
	{ ok , State };
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,1:1,V:15,Rest/binary>>}=State) ->
	{ RemainingLength , <<>> } = mqttlib:dec_varint(<<1:1,V:15>>),
	io:format("Bytes left=~p  Remaining=~p~n",[size(State#mqtt_processor_state.bytes_left),RemainingLength]),
	case size(Rest) >= RemainingLength of
		true ->
			PacketLength=RemainingLength+3,
			<< CurrentPacket:PacketLength/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = mqtt_message:decode( CurrentPacket , State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			io:format("Need more data.~n"),
			{ ok, State }
	end;
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,RemainingLength:8,Rest/binary>>}=State) ->
	io:format("Bytes left=~p  Remaining=~p~n",[size(State#mqtt_processor_state.bytes_left),RemainingLength]),
	case size(Rest) >= RemainingLength of
		true ->
			PacketLength = RemainingLength+2,
			<< CurrentPacket:PacketLength/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = mqtt_message:decode( CurrentPacket, State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			io:format("Need more data.~n"),
			{ ok, State }
	end;
process(#mqtt_processor_state{}=State) ->
	io:format("Nothing to process~n"),
	{ok,State}.

-spec answer_msg( Msg :: mqtt_answerable(), State :: mqtt_processor_state()) -> {ok,NewState::mqtt_processor_state()}.
answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_3_11 }=Msg, State )->
	VariableHeader = #mqtt_connack_variable_header_v4{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats#mqtt_connection_stats{ client_identifier = Msg#mqtt_connect_variable_header.client_identifier },
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_connect,msg_connack),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version,
		stats = Stats2 }};

answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_5 }=Msg, State )->
	VariableHeader = #mqtt_connack_variable_header_v5{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats#mqtt_connection_stats{ client_identifier = Msg#mqtt_connect_variable_header.client_identifier },
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_connect,msg_connack),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version,
		stats = Stats2 }};

answer_msg( #mqtt_publish_variable_header_v4{}=Msg, State )->
	case Msg#mqtt_publish_variable_header_v4.qos_level_flag of
		0 ->
			io:format("Publish - no QoS.~n"),
			display_published_payload(Msg#mqtt_publish_variable_header_v4.payload),
			Stats1 = State#mqtt_processor_state.stats,
			Stats2 = ?INCREMENT_STATS1(Stats1,msg_publish),
			{ ok , State#mqtt_processor_state{stats = Stats2}};
		1 ->
			VariableHeader = #mqtt_puback_variable_header_v4{ packet_identifier = Msg#mqtt_publish_variable_header_v4.packet_identifier , reason_code = ?MQTT_RC_SUCCESS },
			Response = #mqtt_msg{ packet_type = ?MQTT_PUBACK , variable_header = VariableHeader },
			Blob = mqtt_message:encode( Response ),
			Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
			io:format("Sending PUBACK response(~p): ~p~n",[Result,Blob]),
			Stats1 = State#mqtt_processor_state.stats,
			Stats2 = ?INCREMENT_STATS2(Stats1,msg_publish,msg_puback),
			{ok,State#mqtt_processor_state{ stats = Stats2} };
		2 ->
			VariableHeader = #mqtt_pubrec_variable_header_v4{ packet_identifier = Msg#mqtt_publish_variable_header_v4.packet_identifier , reason_code = ?MQTT_RC_SUCCESS },
			Response = #mqtt_msg{ packet_type = ?MQTT_PUBREC , variable_header = VariableHeader },
			Blob = mqtt_message:encode(Response),
			Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
			io:format("Sending PUBREC response(~p): ~p~n",[Result,Blob]),
			Stats1 = State#mqtt_processor_state.stats,
			Stats2 = ?INCREMENT_STATS2(Stats1,msg_publish,msg_pubrec),
			{ok,State#mqtt_processor_state{ stats = Stats2} }
	end;

answer_msg( #mqtt_pubrel_variable_header_v4{}=Msg, State )->
	VariableHeader = #mqtt_pubcomp_variable_header_v4{ packet_identifier = Msg#mqtt_pubrel_variable_header_v4.packet_identifier },
	Response = #mqtt_msg{ packet_type = ?MQTT_PUBCOMP , variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PUBCOMP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pubrel,msg_pubcomp),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( #mqtt_subscribe_variable_header_v4{}=Msg, State )->
	VariableHeader = #mqtt_suback_variable_header_v4{
		packet_identifier = Msg#mqtt_subscribe_variable_header_v4.packet_identifier ,
		reason_codes = grant_all_topics(Msg#mqtt_subscribe_variable_header_v4.topic_filter_list)},
	Response = #mqtt_msg{ packet_type = ?MQTT_SUBACK, variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending SUBACK response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_subscribe,msg_suback),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( #mqtt_unsubscribe_variable_header_v4{}=Msg, State )->
	VariableHeader = #mqtt_unsuback_variable_header_v4{
		packet_identifier = Msg#mqtt_unsubscribe_variable_header_v4.packet_identifier},
	Response = #mqtt_msg{ packet_type = ?MQTT_UNSUBACK, variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending UNSUBACK response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_unsubscribe,msg_unsuback),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( #mqtt_pingreq_variable_header_v4{}=_Msg, State )->
	VariableHeader = #mqtt_pingresp_variable_header_v4{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pingreq,msg_pingresp),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( #mqtt_pingreq_variable_header_v5{}=_Msg, State )->
	VariableHeader = #mqtt_pingresp_variable_header_v5{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = mqtt_message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pingreq,msg_pingresp),
	{ok,State#mqtt_processor_state{ stats = Stats2} }.

display_published_payload(Payload)->
	P = zlib:uncompress(Payload),
	R = opensync_stats:decode_msg(P,'Report'),
	io:format("Published data: ~p~n",[R]).

grant_all_topics(List)->
	grant_all_topics(List,[]).
grant_all_topics([],Grants)->
	lists:reverse(Grants);
grant_all_topics([{_Topic,QoS}|Tail],Grants)->
	grant_all_topics(Tail,[QoS|Grants]).
