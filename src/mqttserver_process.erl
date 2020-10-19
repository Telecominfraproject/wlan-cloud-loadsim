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

-define(INCREMENT_STATS1(X,Y),X#mqtt_connection_stats{ Y = X#mqtt_connection_stats.Y+1}).
-define(INCREMENT_STATS2(X,Y,Z),X#mqtt_connection_stats{ Y = X#mqtt_connection_stats.Y+1, Z = X#mqtt_connection_stats.Z+1}).

%% API
-export([process/1,pub2/0]).

process(#mqtt_processor_state{ bytes_left = <<>> }=State) ->
	{ ok , State };
process(#mqtt_processor_state{ bytes_left = <<_Command:4,_Flags:4,1:1,V:15,Rest/binary>>}=State) ->
?DBG,
	{ RemainingLength , <<>> } = mqttlib:dec_varint(<<1:1,V:15>>),
	io:format("Bytes left=~p  Remaining=~p~n",[size(State#mqtt_processor_state.bytes_left),RemainingLength]),
	case size(Rest) >= RemainingLength of
		true ->
			PacketLength=RemainingLength+3,
			<< CurrentPacket:PacketLength/binary, LeftData/binary >> = State#mqtt_processor_state.bytes_left,
			{ ok, Msg } = message:decode( CurrentPacket , State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			?DBG,
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
			{ ok, Msg } = message:decode( CurrentPacket, State#mqtt_processor_state.version ),
			{ ok , NewState } = answer_msg(Msg#mqtt_msg.variable_header,State#mqtt_processor_state{bytes_left = LeftData}),
			process(NewState);
		false ->
			io:format("Need more data.~n"),
			{ ok, State }
	end;
process(State) ->
	io:format("Nothing to process~n"),
	{ok,State}.

answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_3_11 }=Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header_v4{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats#mqtt_connection_stats{ client_identifier = Msg#mqtt_connect_variable_header.client_identifier },
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_connect,msg_connack),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version,
		stats = Stats2 }};

answer_msg( #mqtt_connect_variable_header{ protocol_version = ?MQTT_PROTOCOL_VERSION_5 }=Msg, State ) when is_record(Msg,mqtt_connect_variable_header) ->
	VariableHeader = #mqtt_connack_variable_header_v5{ connect_acknowledge_flag = 0,connect_reason_code = ?MQTT_RC_CONNECTION_ACCEPTED },
	Response = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending CONNECT response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats#mqtt_connection_stats{ client_identifier = Msg#mqtt_connect_variable_header.client_identifier },
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_connect,msg_connack),
	{ok,State#mqtt_processor_state{ version = Msg#mqtt_connect_variable_header.protocol_version,
		stats = Stats2 }};

answer_msg( #mqtt_publish_variable_header_v4{ qos_level_flag = 0} = Msg, State ) when is_record(Msg,mqtt_publish_variable_header_v4) ->
	io:format("Publish - no QoS.~n"),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS1(Stats1,msg_publish),
	{ ok , State#mqtt_processor_state{stats = Stats2}};
answer_msg( #mqtt_publish_variable_header_v4{ qos_level_flag = 1} = Msg, State ) when is_record(Msg,mqtt_publish_variable_header_v4) ->
	VariableHeader = #mqtt_puback_variable_header_v4{ packet_identifier = Msg#mqtt_publish_variable_header_v4.packet_identifier , reason_code = ?MQTT_RC_SUCCESS },
	Response = #mqtt_msg{ packet_type = ?MQTT_PUBACK , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PUBACK response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_publish,msg_puback),
	{ok,State#mqtt_processor_state{ stats = Stats2} };
answer_msg( #mqtt_publish_variable_header_v4{ qos_level_flag = 2} = Msg, State ) when is_record(Msg,mqtt_publish_variable_header_v4) ->
	VariableHeader = #mqtt_pubrec_variable_header_v4{ packet_identifier = Msg#mqtt_publish_variable_header_v4.packet_identifier , reason_code = ?MQTT_RC_SUCCESS },
	Response = #mqtt_msg{ packet_type = ?MQTT_PUBREC , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PUBREC response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_publish,msg_pubrec),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( Msg, State ) when is_record(Msg,mqtt_pubrel_variable_header_v4) ->
	VariableHeader = #mqtt_pubcomp_variable_header_v4{ packet_identifier = Msg#mqtt_pubrel_variable_header_v4.packet_identifier },
	Response = #mqtt_msg{ packet_type = ?MQTT_PUBCOMP , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result = (State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PUBCOMP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pubrel,msg_pubcomp),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( Msg, State ) when is_record(Msg,mqtt_subscribe_variable_header_v4) ->
	VariableHeader = #mqtt_suback_variable_header_v4{
		packet_identifier = Msg#mqtt_subscribe_variable_header_v4.packet_identifier ,
		reason_codes = grant_all_topics(Msg#mqtt_subscribe_variable_header_v4.topic_filter_list)},
	Response = #mqtt_msg{ packet_type = ?MQTT_SUBACK, variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending SUBACK response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_subscribe,msg_suback),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( Msg, State ) when is_record(Msg,mqtt_unsubscribe_variable_header_v4) ->
	VariableHeader = #mqtt_unsuback_variable_header_v4{
		packet_identifier = Msg#mqtt_subscribe_variable_header_v4.packet_identifier},
	Response = #mqtt_msg{ packet_type = ?MQTT_UNSUBACK, variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending UNSUBACK response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_unsubscribe,msg_unsuback),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( Msg, State ) when is_record(Msg,mqtt_pingreq_variable_header_v4) ->
	VariableHeader = #mqtt_pingresp_variable_header_v4{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pingreq,msg_pingresp),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg( Msg, State ) when is_record(Msg,mqtt_pingreq_variable_header_v5) ->
	VariableHeader = #mqtt_pingresp_variable_header_v5{ time = erlang:timestamp() },
	Response = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = VariableHeader },
	Blob = message:encode(Response),
	Result=(State#mqtt_processor_state.module):send(State#mqtt_processor_state.socket,Blob),
	io:format("Sending PINGRESP response(~p): ~p~n",[Result,Blob]),
	Stats1 = State#mqtt_processor_state.stats,
	Stats2 = ?INCREMENT_STATS2(Stats1,msg_pingreq,msg_pingresp),
	{ok,State#mqtt_processor_state{ stats = Stats2} };

answer_msg(Msg,State) when is_record(Msg,mqtt_publish_variable_header_v4) ->
	io:format("PUBLISH: ~p~n",[binary_to_list(Msg#mqtt_publish_variable_header_v4.payload)]),
	{ok,State};

answer_msg( Msg, State ) ->
	io:format("MSG->~p~n",[Msg]),
	{ ok, State }.

pub1() ->
	<<120,156,237,89,75,104,19,65,24,222,217,164,237,154,154,52,173,22,23,241,
		32,193,199,30,106,217,166,113,147,142,30,4,47,61,122,244,154,199,70,139,
		162,165,86,80,16,84,90,170,168,52,133,62,76,219,4,95,125,196,55,88,5,181,
		189,104,69,161,32,42,181,165,136,135,66,169,30,244,160,7,17,31,80,103,102,
		147,221,217,77,98,45,164,233,6,60,126,255,124,243,255,223,255,237,236,204,
		108,98,115,184,107,246,212,136,187,165,186,58,209,43,73,222,138,125,28,
		235,100,248,247,209,150,175,147,213,46,59,183,201,217,93,90,223,243,166,
		155,219,27,3,4,254,88,171,131,55,121,29,28,229,116,48,82,169,131,167,215,
		167,160,200,84,204,173,66,149,0,31,239,37,149,202,208,112,7,43,128,250,
		168,202,23,156,29,73,120,9,160,241,42,4,5,139,18,136,227,113,81,29,39,208,
		227,140,208,208,167,142,246,99,184,83,15,119,169,228,62,64,106,159,89,193,
		218,55,216,84,238,197,106,199,114,94,59,170,246,221,159,247,218,195,255,
		236,121,238,107,183,229,182,118,100,41,181,251,86,176,246,181,21,170,141,
		95,255,46,118,165,186,70,79,123,177,218,236,50,212,70,187,220,73,142,65,
		219,233,183,139,100,147,43,231,138,157,243,107,4,171,200,214,199,49,99,24,
		144,208,39,139,80,108,8,221,218,44,20,25,66,163,149,105,161,49,11,202,5,
		116,161,215,46,154,133,4,12,59,144,2,192,71,163,201,109,22,32,125,66,145,
		66,184,142,187,103,113,128,165,2,22,197,14,18,72,224,128,85,207,176,115,
		69,184,229,212,56,174,122,153,69,29,104,66,236,92,137,202,32,144,83,225,
		16,134,171,104,88,198,217,112,193,100,96,16,7,74,149,197,160,5,0,14,20,83,
		146,114,33,186,135,18,61,148,31,209,90,32,23,162,243,228,244,95,68,95,255,
		55,209,17,86,91,148,3,102,17,173,37,77,100,20,61,128,78,101,211,57,173,19,
		93,70,68,11,122,217,232,112,177,22,160,236,139,133,233,118,148,213,118,
		243,191,239,33,195,102,146,221,75,185,157,40,28,217,3,172,238,236,92,146,
		108,59,145,157,85,52,58,41,143,35,81,12,127,65,249,30,113,114,221,192,57,
		110,71,2,238,39,143,118,18,233,92,103,140,204,110,48,70,62,173,54,70,238,
		166,205,234,162,102,161,210,145,74,84,27,240,35,169,175,174,16,190,68,220,
		87,239,35,251,245,240,160,10,201,141,163,81,63,218,172,135,199,84,72,46,
		47,39,232,81,7,119,10,208,195,14,174,213,128,207,2,61,255,130,1,119,232,
		48,105,237,167,133,106,214,193,245,26,24,113,114,1,177,82,57,174,0,186,31,
		7,55,8,156,237,116,127,33,21,198,11,206,140,47,170,25,177,255,102,196,172,
		121,54,163,157,134,38,51,227,123,190,87,70,22,51,250,205,96,70,187,73,86,
		134,41,204,248,156,239,13,116,73,102,232,50,47,191,25,191,77,253,154,100,
		49,35,190,76,102,156,55,245,107,146,103,51,90,173,5,248,154,44,102,70,204,
		20,151,46,145,113,61,98,201,15,87,147,61,228,70,186,197,86,30,144,96,56,0,
		101,15,244,120,161,167,6,6,252,21,12,127,123,97,97,161,108,227,169,185,87,
		227,191,44,2,35,22,99,94,48,3,239,142,145,183,13,229,243,24,121,37,141,
		114,243,209,67,13,254,36,253,233,7,149,94,139,232,65,232,150,161,199,7,
		101,25,138,62,40,5,43,236,141,254,38,127,168,225,136,188,177,233,112,240,
		0,159,32,147,218,102,219,38,222,149,166,180,248,101,227,36,38,201,59,55,
		219,69,243,164,116,222,80,146,119,233,185,198,11,100,229,205,124,236,76,
		230,19,24,87,59,75,254,217,152,81,190,36,80,175,225,32,148,131,48,228,135,
		225,237,176,198,13,37,183,214,235,180,210,107,203,227,249,199,59,208,244,
		42,215,86,91,185,79,130,190,90,24,116,195,90,100,144,15,122,37,84,230,33,
		225,77,61,153,104,123,217,9,8,17,233,9,203,198,188,12,255,54,67,194,186,
		172,9,167,175,60,163,19,138,25,18,78,17,226,248,220,72,42,97,181,173,220,
		155,150,80,109,40,77,40,50,36,2,200,167,213,213,62,98,72,53,253,240,125,
		198,135,175,248,249,224,197,228,200,40,46,215,13,112,3,218,226,243,169,
		139,74,33,158,191,247,138,38,6,51,16,7,9,113,118,236,77,138,40,48,127,0,6,
		145,210,70>>.

pub2()->
	zlib:uncompress(pub1()).

grant_all_topics(List)->
	grant_all_topics(List,[]).
grant_all_topics([],Grants)->
	lists:reverse(Grants);
grant_all_topics([{_Topic,QoS}|Tail],Grants)->
	grant_all_topics(Tail,[QoS|Grants]).