%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2020 9:42 a.m.
%%%-------------------------------------------------------------------
-module(mqttlib).
-author("stephb").

-include("../include/mqtt_definitions.hrl").

%% API
-export([dec_varint/1,enc_varint/1,dec_2byteinteger/1,dec_4byteinteger/1,enc_2byteinteger/2,enc_4byteinteger/2]).
-export([dec_binary/1,enc_binary/1,dec_string/1,enc_string/1,dec_stringpair/1,enc_stringpair/1,dec_byte/1,enc_byte/1]).
-export([atom_to_prop/1,prop_to_atom/1]).
-export([enc_properties/1,dec_properties/1]).

dec_binary(<<L:16,Data:L/binary,Rest/binary>>)->
	{ Data, Rest }.

enc_binary(Data)->
	<<(size(Data)):16,Data/binary>>.

enc_string(S)->
	<<(length(S)):16,(list_to_binary(S))/binary>>.

dec_byte(<<V:8,Rest/binary>>)->
	{ V,Rest }.

enc_byte(V)->
	<<V:8>>.

dec_string(<<L:16,String:L/binary,Rest/binary>>)->
	{ binary_to_list(String) , Rest }.

enc_stringpair({String1,String2})->
	<<(enc_string(String1))/binary,(enc_string(String2))/binary>>.

dec_stringpair(Blob)->
	{ String1 , Blob1 } = dec_string(Blob),
	{ String2 , Blob3 } = dec_string(Blob1),
	{{String1,String2},Blob3}.

dec_varint( <<>> ) ->
	{error,<<>>};
dec_varint( Blob ) when is_binary(Blob)->
	dec_varint(Blob, 1, 0).

dec_varint(<<>>,_,Value) ->
	{Value,<<>>};
dec_varint( <<0:1,V:7,Rest/binary>>,Multiplier,Value) ->
	{ (V*Multiplier) + Value , Rest };
dec_varint( <<1:1,V:7,Rest/binary>>, Multiplier,Value) ->
	dec_varint(Rest,Multiplier bsl 7,(V*Multiplier) + Value).

dec_2byteinteger(<<A:16,Rest/binary>>) ->
	{A,Rest}.

dec_4byteinteger(<<A:32,Rest/binary>>)->
	{A,Rest}.

enc_varint(0)->
	<<0>>;
enc_varint(X) when is_integer(X)->
	enc_varint(X,<<>>).

enc_varint(X,Value) when X < 128 ->
	<<Value/binary,X>>;
enc_varint(X,Value) ->
	B = X rem 128,
	enc_varint( X div 128, <<Value/binary,1:1,B:7>>).

enc_2byteinteger(Blob,A) ->
	<<Blob/binary,A:16>>.

enc_4byteinteger(Blob,A)->
	<<Blob/binary,A:32>>.

dec_properties(Data)->
	dec_properties(Data,[]).

dec_properties(<<>>,Props)->
	lists:reverse(Props);

dec_properties(<<Tag:8,Rest/binary>>,Props)->
	{ PropertyAtom , Format } = property_decoding( Tag ),
	{ Value , Rest2 }= case Format of
		encoding_byte ->
			<<V:8,R2/binary>> = Rest, { V,R2};
		encoding_2byte ->
      <<V:16,R2/binary>> = Rest, { V,R2};
		encoding_4byte ->
			<<V:32,R2/binary>> = Rest, { V,R2};
		encoding_varint ->
			dec_varint(Rest);
		encoding_string ->
			dec_string(Rest);
		encoding_stringpair ->
			dec_stringpair(Rest);
		encoding_binary ->
			dec_binary(Rest)
	end,
	dec_properties( Rest2 , [{PropertyAtom,Value}|Props]).

enc_properties(Properties)->
	enc_properties(Properties,<<>>).

enc_properties([],Data)->
	<<(enc_varint(size(Data)))/binary,Data/binary>>;
enc_properties([{Property,Value}|Rest],Data)->
	{ Tag , Format } = property_encoding( Property ),
	EncodedData = case Format of
		                   encoding_byte ->
			                   <<Value:8>>;
		                   encoding_2byte ->
			                   <<Value:16>>;
		                   encoding_4byte ->
			                   <<Value:32>>;
		                   encoding_varint ->
			                   enc_varint(Value);
		                   encoding_string ->
			                   enc_string(Value);
		                   encoding_stringpair ->
			                   enc_stringpair(Value);
		                   encoding_binary ->
			                   enc_binary(Value)
	                   end,
	enc_properties( Rest , <<Data/binary,Tag:8,EncodedData/binary>>).

property_decoding(?MQTT_PROP_PAYLOAD_FORMAT_INDICATOR)-> { payload_format_indicator , encoding_byte };
property_decoding(?MQTT_PROP_MESSAGE_EXPIRY_INTERVAL)-> { message_expiry_interval , encoding_4byte };
property_decoding(?MQTT_PROP_CONTENT_TYPE)-> { content_type, encoding_string };
property_decoding(?MQTT_PROP_RESPONSE_TOPIC)-> { response_topic, encoding_string };
property_decoding(?MQTT_PROP_CORRELATION_DATA)-> { correlation_data, encoding_binary };
property_decoding(?MQTT_PROP_SUBSCRIPTION_IDENTIFIER)-> { subscription_identifier, encoding_varint };
property_decoding(?MQTT_PROP_SESSION_EXPIRY_INTERVAL)-> { session_expiry_interval, encoding_4byte };
property_decoding(?MQTT_PROP_ASSIGNED_CLIENT_IDENTIFIER)-> { assigned_client_identifier, encoding_string };
property_decoding(?MQTT_PROP_SERVER_KEEP_ALIVE)-> { server_keep_alive , encoding_2byte} ;
property_decoding(?MQTT_PROP_AUTHENTICATION_METHOD)-> { authentication_method, encoding_string};
property_decoding(?MQTT_PROP_AUTHENTICATION_DATA)-> { authentication_data, encoding_binary};
property_decoding(?MQTT_PROP_REQUEST_PROBLEM_INFORMATION)-> {request_problem_information,encoding_byte};
property_decoding(?MQTT_PROP_WILL_DELAY_INTERVAL)-> {will_delay_interval,encoding_4byte};
property_decoding(?MQTT_PROP_REQUEST_RESPONSE_INFORMATION)-> {request_response_information,encoding_byte};
property_decoding(?MQTT_PROP_RESPONSE_INFORMATION)-> {response_information,encoding_string};
property_decoding(?MQTT_PROP_SERVER_REFERENCE)-> {server_reference,encoding_string};
property_decoding(?MQTT_PROP_REASON_STRING)-> {reason_string,encoding_string};
property_decoding(?MQTT_PROP_RECEIVE_MAXIMUM)-> {receive_maximum,encoding_2byte};
property_decoding(?MQTT_PROP_TOPIC_ALIAS_MAXIMUM)-> {topic_alias_maximum,encoding_2byte};
property_decoding(?MQTT_PROP_TOPIC_ALIAS)-> {topic_alias,encoding_2byte};
property_decoding(?MQTT_PROP_MAXIMUM_QOS)-> {maximum_qos,encoding_byte};
property_decoding(?MQTT_PROP_RETAIN_AVAILABLE)-> {retain_available,encoding_byte};
property_decoding(?MQTT_PROP_USER_PROPERTY)-> {user_property,encoding_stringpair};
property_decoding(?MQTT_PROP_MAXIMUM_PACKET_SIZE)-> {maximum_packet_size,encoding_4byte};
property_decoding(?MQTT_PROP_WILDCARD_SUBSCRIPTION_AVAILABLE)-> {wildcard_subscription_available,encoding_byte};
property_decoding(?MQTT_PROP_SUBSCRIPTION_IDENTIFIER_AVAILABLE)-> {subscription_identifier_available,encoding_byte};
property_decoding(?MQTT_PROP_SHARED_SUBSCRIPTION_AVAILABLE)-> {shared_subscription_available,encoding_byte}.

property_encoding(payload_format_indicator)-> { ?MQTT_PROP_PAYLOAD_FORMAT_INDICATOR , encoding_byte };
property_encoding(message_expiry_interval)-> { ?MQTT_PROP_MESSAGE_EXPIRY_INTERVAL , encoding_4byte };
property_encoding(content_type)-> { ?MQTT_PROP_CONTENT_TYPE, encoding_string };
property_encoding(response_topic)-> { ?MQTT_PROP_RESPONSE_TOPIC, encoding_string };
property_encoding(correlation_data)-> {?MQTT_PROP_CORRELATION_DATA , encoding_binary };
property_encoding(subscription_identifier)-> { ?MQTT_PROP_SUBSCRIPTION_IDENTIFIER, encoding_varint };
property_encoding(session_expiry_interval)-> { ?MQTT_PROP_SESSION_EXPIRY_INTERVAL, encoding_4byte };
property_encoding(assigned_client_identifier)-> {?MQTT_PROP_ASSIGNED_CLIENT_IDENTIFIER , encoding_string };
property_encoding(server_keep_alive)-> { ?MQTT_PROP_SERVER_KEEP_ALIVE , encoding_2byte} ;
property_encoding(authentication_method)-> { ?MQTT_PROP_AUTHENTICATION_METHOD, encoding_string};
property_encoding(authentication_data)-> { ?MQTT_PROP_AUTHENTICATION_DATA, encoding_binary};
property_encoding(request_problem_information)-> {?MQTT_PROP_REQUEST_PROBLEM_INFORMATION,encoding_byte};
property_encoding(will_delay_interval)-> {?MQTT_PROP_WILL_DELAY_INTERVAL,encoding_4byte};
property_encoding(request_response_information)-> {?MQTT_PROP_REQUEST_RESPONSE_INFORMATION,encoding_byte};
property_encoding(response_information)-> {?MQTT_PROP_RESPONSE_INFORMATION,encoding_string};
property_encoding(server_reference)-> {?MQTT_PROP_SERVER_REFERENCE,encoding_string};
property_encoding(reason_string)-> {?MQTT_PROP_REASON_STRING,encoding_string};
property_encoding(receive_maximum)-> {?MQTT_PROP_RECEIVE_MAXIMUM,encoding_2byte};
property_encoding(topic_alias_maximum)-> {?MQTT_PROP_TOPIC_ALIAS_MAXIMUM,encoding_2byte};
property_encoding(topic_alias)-> {?MQTT_PROP_TOPIC_ALIAS,encoding_2byte};
property_encoding(maximum_qos)-> {?MQTT_PROP_MAXIMUM_QOS,encoding_byte};
property_encoding(retain_available)-> {?MQTT_PROP_RETAIN_AVAILABLE,encoding_byte};
property_encoding(user_property)-> {?MQTT_PROP_USER_PROPERTY,encoding_stringpair};
property_encoding(maximum_packet_size)-> {?MQTT_PROP_MAXIMUM_PACKET_SIZE,encoding_4byte};
property_encoding(wildcard_subscription_available)-> {?MQTT_PROP_WILDCARD_SUBSCRIPTION_AVAILABLE,encoding_byte};
property_encoding(subscription_identifier_available)-> {?MQTT_PROP_SUBSCRIPTION_IDENTIFIER_AVAILABLE,encoding_byte};
property_encoding(shared_subscription_available)-> {?MQTT_PROP_SHARED_SUBSCRIPTION_AVAILABLE,encoding_byte}.

prop_to_atom(?MQTT_PROP_PAYLOAD_FORMAT_INDICATOR)-> payload_format_indicator;
prop_to_atom(?MQTT_PROP_MESSAGE_EXPIRY_INTERVAL)-> message_expiry_interval;
prop_to_atom(?MQTT_PROP_CONTENT_TYPE)-> content_type;
prop_to_atom(?MQTT_PROP_RESPONSE_TOPIC)-> response_topic;
prop_to_atom(?MQTT_PROP_CORRELATION_DATA)-> correlation_data;
prop_to_atom(?MQTT_PROP_SUBSCRIPTION_IDENTIFIER)-> subscription_identifier;
prop_to_atom(?MQTT_PROP_SESSION_EXPIRY_INTERVAL)-> session_expiry_interval;
prop_to_atom(?MQTT_PROP_ASSIGNED_CLIENT_IDENTIFIER)-> assigned_client_identifier;
prop_to_atom(?MQTT_PROP_SERVER_KEEP_ALIVE)-> server_keep_alive;
prop_to_atom(?MQTT_PROP_AUTHENTICATION_METHOD)-> authentication_method;
prop_to_atom(?MQTT_PROP_AUTHENTICATION_DATA)-> authentication_data;
prop_to_atom(?MQTT_PROP_REQUEST_PROBLEM_INFORMATION)-> request_problem_information;
prop_to_atom(?MQTT_PROP_WILL_DELAY_INTERVAL)-> will_delay_interval;
prop_to_atom(?MQTT_PROP_REQUEST_RESPONSE_INFORMATION)-> request_response_information;
prop_to_atom(?MQTT_PROP_RESPONSE_INFORMATION)-> response_information;
prop_to_atom(?MQTT_PROP_SERVER_REFERENCE)-> server_reference;
prop_to_atom(?MQTT_PROP_REASON_STRING)-> reason_string;
prop_to_atom(?MQTT_PROP_RECEIVE_MAXIMUM)-> receive_maximum;
prop_to_atom(?MQTT_PROP_TOPIC_ALIAS_MAXIMUM)-> topic_alias_maximum;
prop_to_atom(?MQTT_PROP_TOPIC_ALIAS)-> topic_alias;
prop_to_atom(?MQTT_PROP_MAXIMUM_QOS)-> maximum_qos;
prop_to_atom(?MQTT_PROP_RETAIN_AVAILABLE)-> retain_available;
prop_to_atom(?MQTT_PROP_USER_PROPERTY)-> user_property;
prop_to_atom(?MQTT_PROP_MAXIMUM_PACKET_SIZE)-> maximum_packet_size;
prop_to_atom(?MQTT_PROP_WILDCARD_SUBSCRIPTION_AVAILABLE)-> wildcard_subscription_available;
prop_to_atom(?MQTT_PROP_SUBSCRIPTION_IDENTIFIER_AVAILABLE)-> subscription_identifier_available;
prop_to_atom(?MQTT_PROP_SHARED_SUBSCRIPTION_AVAILABLE)-> shared_subscription_available.

atom_to_prop(payload_format_indicator)-> ?MQTT_PROP_PAYLOAD_FORMAT_INDICATOR;
atom_to_prop(message_expiry_interval)-> ?MQTT_PROP_MESSAGE_EXPIRY_INTERVAL;
atom_to_prop(content_type)->?MQTT_PROP_CONTENT_TYPE ;
atom_to_prop(response_topic)-> ?MQTT_PROP_RESPONSE_TOPIC;
atom_to_prop(correlation_data)->?MQTT_PROP_CORRELATION_DATA ;
atom_to_prop(subscription_identifier)->?MQTT_PROP_SUBSCRIPTION_IDENTIFIER ;
atom_to_prop(session_expiry_interval)-> ?MQTT_PROP_SESSION_EXPIRY_INTERVAL;
atom_to_prop(assigned_client_identifier)-> ?MQTT_PROP_ASSIGNED_CLIENT_IDENTIFIER;
atom_to_prop(server_keep_alive)-> ?MQTT_PROP_SERVER_KEEP_ALIVE;
atom_to_prop(authentication_method)-> ?MQTT_PROP_AUTHENTICATION_METHOD;
atom_to_prop(authentication_data)-> ?MQTT_PROP_AUTHENTICATION_DATA;
atom_to_prop(request_problem_information)-> ?MQTT_PROP_REQUEST_PROBLEM_INFORMATION;
atom_to_prop(will_delay_interval)-> ?MQTT_PROP_WILL_DELAY_INTERVAL;
atom_to_prop(request_response_information)-> ?MQTT_PROP_REQUEST_RESPONSE_INFORMATION;
atom_to_prop(response_information)-> ?MQTT_PROP_RESPONSE_INFORMATION;
atom_to_prop(server_reference)-> ?MQTT_PROP_SERVER_REFERENCE;
atom_to_prop(reason_string)-> ?MQTT_PROP_REASON_STRING;
atom_to_prop(receive_maximum)-> ?MQTT_PROP_RECEIVE_MAXIMUM;
atom_to_prop(topic_alias_maximum)-> ?MQTT_PROP_TOPIC_ALIAS_MAXIMUM;
atom_to_prop(topic_alias)-> ?MQTT_PROP_TOPIC_ALIAS;
atom_to_prop(maximum_qos)-> ?MQTT_PROP_MAXIMUM_QOS;
atom_to_prop(retain_available)-> ?MQTT_PROP_RETAIN_AVAILABLE;
atom_to_prop(user_property)-> ?MQTT_PROP_USER_PROPERTY;
atom_to_prop(maximum_packet_size)-> ?MQTT_PROP_MAXIMUM_PACKET_SIZE;
atom_to_prop(wildcard_subscription_available)-> ?MQTT_PROP_WILDCARD_SUBSCRIPTION_AVAILABLE;
atom_to_prop(subscription_identifier_available)-> ?MQTT_PROP_SUBSCRIPTION_IDENTIFIER_AVAILABLE;
atom_to_prop(shared_subscription_available)-> ?MQTT_PROP_SHARED_SUBSCRIPTION_AVAILABLE.




