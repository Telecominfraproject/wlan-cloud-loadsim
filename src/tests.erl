%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 10:29 a.m.
%%%-------------------------------------------------------------------
-module(tests).
-author("stephb").

-include_lib("eunit/include/eunit.hrl").
-include("../include/mqtt_definitions.hrl").

-define( DBGFUNC, ?debugFmt("Testing:~p",[?FUNCTION_NAME])).

simple_test() ->
	?assert(true).

connection_packet_encoding_decoding_test() ->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_connect_variable_header{
		username_flag = 1 ,
		password_flag = 1,
		username = "Stephb",
		password = <<"1234567890">>,
		will_flag = 1,
		will_qos_flag = 1,
		keep_alive = 60,
		client_identifier = "test_device_1",
		will_topic = "topics/a",
		clean_start_flag = 1,
		will_properties = [{will_delay_interval,1000},
			{payload_format_indicator,1},
			{ message_expiry_interval,5000},
			{ content_type, "application/json"},
			{ response_topic, "topic/response"},
			{ user_property, { "uprop1", "user_property_one"}}],
		will_payload = <<"will payload">>
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_CONNECT , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

connack_packet_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_connack_variable_header{
		properties = [],
		connect_acknowledge_flag = 1,
		connect_reason_code = ?MQTT_RC_SUCCESS
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_packet_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_publish_variable_header{
		topic_name = "topic/b/c",
		packet_identifier = 12345,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		],
		payload = <<"Some bla bla data for payload">>,
		dup_flag = 1,
		qos_level_flag = 1,
		retain_flag = 1
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_CONNACK , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_puback_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_puback_variable_header{
		packet_identifier = 12345,
		reason_code = ?MQTT_RC_SUCCESS,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_PUBACK , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_pubrec_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_pubrec_variable_header{
		packet_identifier = 12345,
		reason_code = ?MQTT_RC_SUCCESS,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_PUBREC , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_pubrel_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_pubrel_variable_header{
		packet_identifier = 12345,
		reason_code = ?MQTT_RC_SUCCESS,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_PUBREL , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_pubcomp_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_pubcomp_variable_header{
		packet_identifier = 12345,
		reason_code = ?MQTT_RC_SUCCESS,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_PUBCOMP , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_subscribe_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_subscribe_variable_header{
		packet_identifier = 12345,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		],
		topic_filter_list = [ { "topic1" , ?MQTT_SUBSCRIBE_LEVEL_0 },
			{ "topic2", ?MQTT_SUBSCRIBE_LEVEL_1 },
			{ "topic3", ?MQTT_SUBSCRIBE_LEVEL_2 }]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_SUBSCRIBE , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_suback_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_suback_variable_header{
		packet_identifier = 12345,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		],
		reason_codes = [ ?MQTT_RC_GRANTED_QOS_0, ?MQTT_RC_GRANTED_QOS_1, ?MQTT_RC_GRANTED_QOS_2 ]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_SUBACK , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_unsubscribe_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_unsubscribe_variable_header{
		packet_identifier = 12345,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		],
		topic_list = [ "topic1" , "topic2", "topic3" ]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_UNSUBSCRIBE , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_unsuback_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_unsuback_variable_header{
		packet_identifier = 12345,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		],
		reason_codes = [ ?MQTT_RC_SUCCESS, ?MQTT_RC_NO_SUBSCRIPTION_EXISTED, ?MQTT_RC_SUCCESS ]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_UNSUBACK , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_pingreq_encoding_decoding_test()->
	%% Since there is nothing to encode/decode, we put in the time. Since time changes, we need to fake the time in/out
	%% So this looks funny because it has to
	?DBGFUNC,
	Time = erlang:timestamp(),
	PacketVariableHeader = #mqtt_pingreq_variable_header{ time = Time },
	Msg = #mqtt_msg{ packet_type = ?MQTT_PINGREQ , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	D2 = Decoded#mqtt_msg{ variable_header = #mqtt_pingreq_variable_header{ time = Time }},
	?assert( D2#mqtt_msg.variable_header == PacketVariableHeader ).

publish_pingresp_encoding_decoding_test()->
	%% Since there is nothing to encode/decode, we put in the time. Since time changes, we need to fake the time in/out
	%% So this looks funny because it has to
	?DBGFUNC,
	Time = erlang:timestamp(),
	PacketVariableHeader = #mqtt_pingresp_variable_header{ time = Time },
	Msg = #mqtt_msg{ packet_type = ?MQTT_PINGRESP , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	D2 = Decoded#mqtt_msg{ variable_header = #mqtt_pingresp_variable_header{ time = Time }},
	?assert( D2#mqtt_msg.variable_header == PacketVariableHeader ).

publish_disconnect_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_disconnect_variable_header{
			reason_code = ?MQTT_RC_SUCCESS,
			properties = [
				{ payload_format_indicator, 0 },
				{ message_expiry_interval, 60},
				{ topic_alias , 51 },
				{ response_topic, "response/topic"},
				{ correlation_data, <<"1234567890abcdefghijklmnop">>},
				{ user_property, {"UProp1","UProp1Val"}},
				{ subscription_identifier, 16253 },
				{ content_type, "application/json"}
			]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_DISCONNECT , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).

publish_auth_encoding_decoding_test()->
	?DBGFUNC,
	PacketVariableHeader = #mqtt_auth_variable_header{
		reason_code = ?MQTT_RC_SUCCESS,
		properties = [
			{ payload_format_indicator, 0 },
			{ message_expiry_interval, 60},
			{ topic_alias , 51 },
			{ response_topic, "response/topic"},
			{ correlation_data, <<"1234567890abcdefghijklmnop">>},
			{ user_property, {"UProp1","UProp1Val"}},
			{ subscription_identifier, 16253 },
			{ content_type, "application/json"}
		]
	},
	Msg = #mqtt_msg{ packet_type = ?MQTT_AUTH , variable_header = PacketVariableHeader },
	Blob = message:encode(Msg),
	{ ok, Decoded } = message:decode(Blob),
	?assert( Decoded#mqtt_msg.variable_header == PacketVariableHeader ).