%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2020 11:56 a.m.
%%%-------------------------------------------------------------------
-module(message).
-author("stephb").

-include("../include/mqtt_definitions.hrl").

%% API
-export([decode/1,encode/1]).

-spec decode( Packet::binary() ) -> { error , atom() } | { ok , mqtt_msg()}.
decode(<<PacketType:4,Flags:4,Rest/binary>>)->
	{ RemainingLength, Blob } = mqttlib:dec_varint(Rest),
	case size(Blob) == RemainingLength of
		true ->
			decode_packet(#mqtt_msg{ packet_type = PacketType, flags = Flags, remaining_length = RemainingLength },Blob);
		false ->
			{ error , malformed_packet }
	end.

get_properties_section(Data) ->
	{ PropertiesLength , Rest } = mqttlib:dec_varint(Data),
	<<PropertiesData:PropertiesLength/binary,Rest2/binary>> = Rest,
	{ mqttlib:dec_properties( PropertiesData ), Rest2 }.

set_properties_section(Properties)->
	mqttlib:enc_properties(Properties).

get_topic_filter_list(Data)->
	get_topic_filter_list(Data,[]).
get_topic_filter_list(<<>>,Topics)->
	lists:reverse(Topics);
get_topic_filter_list(Data,Topics)->
	{ String, Rest } = mqttlib:dec_string(Data),
	{ Byte, Rest2 } = mqttlib:dec_byte(Rest),
	get_topic_filter_list(Rest2,[{String,Byte}|Topics]).

set_topic_filter_list(List)->
	set_topic_filter_list(List,<<>>).
set_topic_filter_list([],Blob)->
	Blob;
set_topic_filter_list([{Topic,Filter}|Tail],Blob)->
	set_topic_filter_list(Tail,<< Blob/binary, (mqttlib:enc_string(Topic))/binary, Filter:8>>).

get_reason_code_list(Data)->
	get_reason_code_list(Data,[]).
get_reason_code_list(<<>>,List)->
	lists:reverse(List);
get_reason_code_list(<<V:8,Rest/binary>>,List)->
	get_reason_code_list( Rest , [V|List]).

set_reason_code_list(List)->
	set_reason_code_list(List,<<>>).
set_reason_code_list([],Blob)->
	Blob;
set_reason_code_list([H|Tail],Blob)->
	set_reason_code_list( Tail , << Blob/binary, H:8>>).

get_string_list(Data)->
	get_string_list(Data,[]).
get_string_list(<<>>,List)->
	lists:reverse(List);
get_string_list(Data,List)->
	{ String, Rest} = mqttlib:dec_string(Data),
	get_string_list(Rest,[String|List]).

set_string_list(List)->
	set_string_list(List,<<>>).
set_string_list([],Blob)->
	Blob;
set_string_list([H|T],Blob)->
	set_string_list(T,<<Blob/binary,(mqttlib:enc_string(H))/binary>>).

%% Rules: payload required
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_CONNECT } = Msg,
		<< 0:8 , 4:8 , $M:8, $Q:8 , $T:8, $T:8 , ?MQTT_PROTOCOL_VERSION:8,
			UserNameFlag:1,
			PasswordFlag:1,
			WillRetain:1,
			WillQOS:2,
			WillFlag:1,
			CleanStart:1,0:1,
			KeepAlive:16,Rest/binary>> )->
	{ Properties , Rest2 } = get_properties_section(Rest),
	{ ClientIdentifier , L1 } = mqttlib:dec_string(Rest2),
	{ WillTopic, WillPayload, WillProperties, L4 } = case WillFlag of
												 0 -> { "" , <<>>, [], L1};
												 1 -> { WProperties , R2 } = get_properties_section(L1),
													    { TopicString , R3 } = mqttlib:dec_string(R2) ,
													    { TopicData , R4 } = mqttlib:dec_binary(R3) ,
													    { TopicString , TopicData , WProperties, R4 }
	                     end,
	{ UserName , L5 } = case UserNameFlag of
												0 -> { "" , L4 };
												1 -> mqttlib:dec_string(L4)
	                    end,
	{ Password , _L6 } = case PasswordFlag of
												0 -> { <<>> , L5 };
												1 -> mqttlib:dec_binary(L5)
	                    end,
	VariableHeader = #mqtt_connect_variable_header{
		username_flag = UserNameFlag,
		password_flag = PasswordFlag,
		will_retain_flag = WillRetain,
		will_qos_flag = WillQOS ,
		will_flag = WillFlag,
		clean_start_flag = CleanStart,
		keep_alive = KeepAlive,
		client_identifier = ClientIdentifier ,
		will_payload = WillPayload ,
		will_properties = WillProperties,
		will_topic = WillTopic,
		username = UserName,
		password = Password,
		properties = Properties},
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader }};

%% Rules: payload none,
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_CONNACK }=Msg,<<_:7,ConnectAcknowledgeFlag:1,ConnectReasonCode:8,Rest/binary>>)->
	{ Properties , <<>> } = get_properties_section(Rest),
	VariableHeader = #mqtt_connack_variable_header{ connect_acknowledge_flag = ConnectAcknowledgeFlag ,
																									connect_reason_code = ConnectReasonCode,
																									properties = Properties },
	{ok,Msg#mqtt_msg{variable_header = VariableHeader}};

%% Rules: payload optional
%% packet identifier = if QoS>0 YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PUBLISH }=Msg,Data)->
	<< _:4,DUPFlag:1,QOSLevelFlag:2,RetainFlag:1>> = <<(Msg#mqtt_msg.flags)>>,
	{ TopicName , Rest2 } = mqttlib:dec_string(Data),
	{ PacketIdentifier, Rest3 } = case QOSLevelFlag =:= 0 of
																	true -> { 0 , Rest2 };
																	false -> <<V:16,C1/binary>>=Rest2,
																		{ V, C1 }
	                              end,
	{ Properties , Payload } = get_properties_section( Rest3 ),

	VariableHeader = #mqtt_publish_variable_header{
		dup_flag = DUPFlag ,
		qos_level_flag = QOSLevelFlag ,
		retain_flag = RetainFlag,
		topic_name = TopicName ,
		properties = Properties,
		packet_identifier = PacketIdentifier,
		payload = Payload},
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PUBACK }=Msg,<<PacketIdentifier:16,ReasonCode:8,Rest/binary>>)->
	VariableHeader = case Msg#mqtt_msg.remaining_length > 3 of
											true ->
												{ Properties , <<>> } = get_properties_section(Rest),
												#mqtt_puback_variable_header{ packet_identifier = PacketIdentifier , reason_code = ReasonCode , properties = Properties };
											false ->
												#mqtt_puback_variable_header{ packet_identifier = PacketIdentifier , reason_code = ReasonCode }
										end,
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PUBREC }=Msg,<<PacketIdentifier:16,ReasonCode:8,Rest/binary>>)->
	VariableHeader = case Msg#mqtt_msg.remaining_length > 3 of
											true ->
												{ Properties , _ } = get_properties_section(Rest),
												#mqtt_pubrec_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode, properties = Properties};
											false ->
												#mqtt_pubrec_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode }
										end,
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PUBREL , flags = 2 }=Msg,<<PacketIdentifier:16,ReasonCode:8,Rest/binary>>)->
	VariableHeader = case Msg#mqtt_msg.remaining_length > 3 of
		                 true ->
			                 { Properties , _ } = get_properties_section(Rest),
			                 #mqtt_pubrel_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode, properties = Properties};
		                 false ->
			                 #mqtt_pubrel_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode }
	                 end,
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PUBCOMP }=Msg,<<PacketIdentifier:16,ReasonCode:8,Rest/binary>>)->
	VariableHeader = case Msg#mqtt_msg.remaining_length > 3 of
		                 true ->
			                 { Properties , _ } = get_properties_section(Rest),
			                 #mqtt_pubcomp_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode, properties = Properties};
		                 false ->
			                 #mqtt_pubcomp_variable_header{ packet_identifier = PacketIdentifier, reason_code = ReasonCode }
	                 end,
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload required
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_SUBSCRIBE , flags=2 }=Msg,<<PacketIdentifier:16,Rest/binary>>)->
	VariableHeader = case Msg#mqtt_msg.remaining_length > 2 of
		                 true ->
			                 { Properties , Payload } = get_properties_section(Rest),
			                 TopicFilterList = get_topic_filter_list( Payload ),
			                 #mqtt_subscribe_variable_header{ packet_identifier = PacketIdentifier, properties = Properties, topic_filter_list = TopicFilterList };
		                 false ->
			                 #mqtt_subscribe_variable_header{ packet_identifier = PacketIdentifier }
	                 end,
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload required
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_SUBACK }=Msg,<<PacketIdentifier:16,Data/binary>>)->
	{ Properties , Payload } = get_properties_section(Data),
	ReasonCodeList = get_reason_code_list(Payload),
	VariableHeader = #mqtt_suback_variable_header{ packet_identifier = PacketIdentifier, properties = Properties, reason_codes = ReasonCodeList},
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload required
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_UNSUBSCRIBE , flags = 2 }=Msg,<<PacketIdentifier:16,Data/binary>>)->
	{ Properties , Payload } = get_properties_section(Data),
	StringList = get_string_list(Payload),
	VariableHeader = #mqtt_unsubscribe_variable_header{ packet_identifier = PacketIdentifier , properties = Properties, topic_list = StringList },
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload required
%% packet identifier = YES
decode_packet(#mqtt_msg{ packet_type = ?MQTT_UNSUBACK }=Msg,<<PacketIdentifier:16,Data/binary>>)->
	{ Properties , Payload } = get_properties_section(Data),
	ReasonCodeList = get_reason_code_list(Payload),
	VariableHeader = #mqtt_unsuback_variable_header{ packet_identifier = PacketIdentifier , properties = Properties, reason_codes = ReasonCodeList },
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PINGREQ }=Msg,_)->
	{ok,Msg#mqtt_msg{ variable_header = #mqtt_pingreq_variable_header{ time=erlang:timestamp()}}};

%% Rules: payload none
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_PINGRESP }=Msg,_)->
	{ok,Msg#mqtt_msg{ variable_header = #mqtt_pingresp_variable_header{ time=erlang:timestamp()}}};

%% Rules: payload none
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_DISCONNECT }=Msg,Data)->
	{ ResultCode , Rest } = case Msg#mqtt_msg.remaining_length == 0 of
														 true ->
															 { ?MQTT_RC_SUCCESS, <<>> };
														 false ->
															 <<RC:8,Rest1/binary>> = Data,
															 { RC, Rest1 }
							             end,
	{ Properties, _ } = case Msg#mqtt_msg.remaining_length < 2 of
												true ->
													{ [] , <<>>};
												false ->
													get_properties_section(Rest)
	                    end,
	VariableHeader = #mqtt_disconnect_variable_header{ reason_code = ResultCode, properties = Properties },
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}};

%% Rules: payload none
%% packet identifier = NO
decode_packet(#mqtt_msg{ packet_type = ?MQTT_AUTH }=Msg,Data)->
	{ ResultCode , Rest } = case Msg#mqtt_msg.remaining_length == 0 of
		                        true ->
			                        { ?MQTT_RC_SUCCESS, <<>> };
		                        false ->
			                        <<RC:8,Rest1/binary>> = Data,
			                        { RC, Rest1 }
	                        end,
	{ Properties, _ } = case Msg#mqtt_msg.remaining_length < 2 of
		                    true ->
			                    { [] , <<>>};
		                    false ->
			                    get_properties_section(Rest)
	                    end,
	VariableHeader = #mqtt_auth_variable_header{ reason_code = ResultCode, properties = Properties },
	{ok,Msg#mqtt_msg{ variable_header = VariableHeader}}.


-spec encode( Msg::mqtt_msg() ) -> { error, atom() } | { ok, Data::binary() }.
encode(Msg) when is_record(Msg,mqtt_msg)->
	{ Command, Flags, Blob } = encode( Msg#mqtt_msg.variable_header ),
	Length = mqttlib:enc_varint(size(Blob)),
	<<Command:4,Flags:4,Length/binary,Blob/binary>>;
%% Complete
encode( Header ) when is_record(Header,mqtt_connect_variable_header) ->
	Flags = <<(Header#mqtt_connect_variable_header.username_flag):1,
		(Header#mqtt_connect_variable_header.password_flag):1,
		(Header#mqtt_connect_variable_header.will_retain_flag):1,
		(Header#mqtt_connect_variable_header.will_qos_flag):2,
		(Header#mqtt_connect_variable_header.will_flag):1,
		(Header#mqtt_connect_variable_header.clean_start_flag):1,
		0:1>>,
	PropData=set_properties_section(Header#mqtt_connect_variable_header.properties),
	UserNamePayload = case Header#mqtt_connect_variable_header.username_flag == 1 of
		                  true -> mqttlib:enc_string(Header#mqtt_connect_variable_header.username);
		                  false -> <<>>
	                  end,
	UserPasswordPayload = case Header#mqtt_connect_variable_header.password_flag==1 of
		                      true -> mqttlib:enc_binary(Header#mqtt_connect_variable_header.password);
		                      false -> <<>>
	                      end,
	Payload = << (mqttlib:enc_string(Header#mqtt_connect_variable_header.client_identifier))/binary,
		(set_properties_section((Header#mqtt_connect_variable_header.will_properties)))/binary,
		(mqttlib:enc_string(Header#mqtt_connect_variable_header.will_topic))/binary,
		(mqttlib:enc_binary(Header#mqtt_connect_variable_header.will_payload))/binary,
		UserNamePayload/binary,
		UserPasswordPayload/binary>>,
	Blob = <<0:8,4:8,$M,$Q,$T,$T,?MQTT_PROTOCOL_VERSION,Flags/binary,(Header#mqtt_connect_variable_header.keep_alive):16,PropData/binary,Payload/binary>>,
	{?MQTT_CONNECT, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_connack_variable_header) ->
	Blob = << 0:7, (Header#mqtt_connack_variable_header.connect_acknowledge_flag):1, (Header#mqtt_connack_variable_header.connect_reason_code):8,(set_properties_section(Header#mqtt_connack_variable_header.properties))/binary >>,
	{?MQTT_CONNACK, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_publish_variable_header) ->
	Flags = (Header#mqtt_publish_variable_header.dup_flag bsl 3) bor
					(Header#mqtt_publish_variable_header.qos_level_flag bsl 1) bor
					(Header#mqtt_publish_variable_header.retain_flag),
	Blob = case Header#mqtt_publish_variable_header.qos_level_flag > 0 of
						true ->
							<< (mqttlib:enc_string(Header#mqtt_publish_variable_header.topic_name))/binary,
								(Header#mqtt_publish_variable_header.packet_identifier):16,
								(set_properties_section(Header#mqtt_publish_variable_header.properties))/binary,
								(Header#mqtt_publish_variable_header.payload)/binary>>;
					 false ->
						  << (mqttlib:enc_string(Header#mqtt_publish_variable_header.topic_name))/binary,
							 (set_properties_section(Header#mqtt_publish_variable_header.properties))/binary,
							 (Header#mqtt_publish_variable_header.payload)/binary>>
	       end,
	{?MQTT_PUBLISH, Flags, Blob};

%% Complete
encode( Header ) when is_record(Header,mqtt_puback_variable_header) ->
	Blob = << (Header#mqtt_puback_variable_header.packet_identifier):16,
		(Header#mqtt_puback_variable_header.reason_code):8,
	(set_properties_section(Header#mqtt_puback_variable_header.properties))/binary>>,
	{ ?MQTT_PUBACK, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_pubrec_variable_header) ->
	Blob = << (Header#mqtt_pubrec_variable_header.packet_identifier):16,
		(Header#mqtt_pubrec_variable_header.reason_code):8,
		(set_properties_section(Header#mqtt_pubrec_variable_header.properties))/binary>>,
	{ ?MQTT_PUBREC, 0, Blob};

% complete
encode( Header ) when is_record(Header,mqtt_pubrel_variable_header) ->
	Blob = << (Header#mqtt_pubrel_variable_header.packet_identifier):16,
		(Header#mqtt_pubrel_variable_header.reason_code):8,
		(set_properties_section(Header#mqtt_pubrel_variable_header.properties))/binary>>,
	{ ?MQTT_PUBREL, 2, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_pubcomp_variable_header) ->
	Blob = << (Header#mqtt_pubcomp_variable_header.packet_identifier):16,
		(Header#mqtt_pubcomp_variable_header.reason_code):8,
		(set_properties_section(Header#mqtt_pubcomp_variable_header.properties))/binary>>,
	{ ?MQTT_PUBCOMP, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_subscribe_variable_header) ->
	Blob = << (Header#mqtt_subscribe_variable_header.packet_identifier):16,
		(set_properties_section(Header#mqtt_subscribe_variable_header.properties))/binary,
		(set_topic_filter_list(Header#mqtt_subscribe_variable_header.topic_filter_list))/binary>>,
	{ ?MQTT_SUBSCRIBE, 2, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_suback_variable_header) ->
	Blob = << (Header#mqtt_suback_variable_header.packet_identifier):16,
		(set_properties_section(Header#mqtt_suback_variable_header.properties))/binary,
		(set_reason_code_list(Header#mqtt_suback_variable_header.reason_codes))/binary>>,
	{ ?MQTT_SUBACK, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_unsubscribe_variable_header) ->
	Blob = << (Header#mqtt_unsubscribe_variable_header.packet_identifier):16,
		(set_properties_section(Header#mqtt_unsubscribe_variable_header.properties))/binary,
		(set_string_list(Header#mqtt_unsubscribe_variable_header.topic_list))/binary>>,
	{ ?MQTT_UNSUBSCRIBE, 2, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_unsuback_variable_header) ->
	Blob = << (Header#mqtt_unsuback_variable_header.packet_identifier):16,
		(set_properties_section(Header#mqtt_unsuback_variable_header.properties))/binary,
		(set_reason_code_list(Header#mqtt_unsuback_variable_header.reason_codes))/binary>>,
	{ ?MQTT_UNSUBACK, 0, Blob};

%% Complete
encode( Header ) when is_record(Header,mqtt_pingreq_variable_header) ->
	{ ?MQTT_PINGREQ, 0, <<>>};

%% Complete
encode( Header ) when is_record(Header,mqtt_pingresp_variable_header) ->
	{ ?MQTT_PINGRESP, 0, <<>>};

%% Complete
encode( Header ) when is_record(Header,mqtt_disconnect_variable_header) ->
	Blob = case length(Header#mqtt_disconnect_variable_header.properties) > 0 of
					 true ->
						 <<(Header#mqtt_disconnect_variable_header.reason_code):8,
							 (set_properties_section(Header#mqtt_disconnect_variable_header.properties))/binary >>;
					 false ->
						 <<(Header#mqtt_disconnect_variable_header.reason_code):8>>
				 end,
	{ ?MQTT_DISCONNECT, 0, Blob};

%% complete
encode( Header ) when is_record(Header,mqtt_auth_variable_header) ->
	Blob = case length(Header#mqtt_auth_variable_header.properties) >0 of
					 true ->
						 << (Header#mqtt_auth_variable_header.reason_code):8,
							 (set_properties_section(Header#mqtt_auth_variable_header.properties))/binary>>;
					 false ->
						 << (Header#mqtt_auth_variable_header.reason_code):8 >>
	       end,
	{ ?MQTT_AUTH, 0, Blob}.
