%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2020 12:04 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

-ifndef(__MQTTDEFS_HRL__).
-define(__MQTTDEFS_HRL__,1).

%% As defined in https://docs.oasis-open.org/mqtt/mqtt/v5.0/os/mqtt-v5.0-os.html#_Toc3901001

-define(MQTT_PROTOCOL_VERSION_5,5).
-define(MQTT_PROTOCOL_VERSION_3_11,4).

-define(OUI_FILENAME,"oui_db.txt").
-define(OUI_DEFAULT_DOWNLOAD_LINK,"https://linuxnet.ca/ieee/oui.txt").

-define(MQTT_DEFAULT_SERVER_PORT,1883).

-define(MQTT_CONNECT,1).
-define(MQTT_CONNACK,2).
-define(MQTT_PUBLISH,3).
-define(MQTT_PUBACK,4).
-define(MQTT_PUBREC,5).
-define(MQTT_PUBREL,6).
-define(MQTT_PUBCOMP,7).
-define(MQTT_SUBSCRIBE,8).
-define(MQTT_SUBACK,9).
-define(MQTT_UNSUBSCRIBE,10).
-define(MQTT_UNSUBACK,11).
-define(MQTT_PINGREQ,12).
-define(MQTT_PINGRESP,13).
-define(MQTT_DISCONNECT,14).
-define(MQTT_AUTH,15).

%%
%% MQTT Properties
%%
-define(MQTT_PROP_PAYLOAD_FORMAT_INDICATOR,1).
-define(MQTT_PROP_MESSAGE_EXPIRY_INTERVAL,2).
-define(MQTT_PROP_CONTENT_TYPE,3).
-define(MQTT_PROP_RESPONSE_TOPIC,8).
-define(MQTT_PROP_CORRELATION_DATA,9).
-define(MQTT_PROP_SUBSCRIPTION_IDENTIFIER,11).
-define(MQTT_PROP_SESSION_EXPIRY_INTERVAL,17).
-define(MQTT_PROP_ASSIGNED_CLIENT_IDENTIFIER,18).
-define(MQTT_PROP_SERVER_KEEP_ALIVE,19).
-define(MQTT_PROP_AUTHENTICATION_METHOD,21).
-define(MQTT_PROP_AUTHENTICATION_DATA,22).
-define(MQTT_PROP_REQUEST_PROBLEM_INFORMATION,23).
-define(MQTT_PROP_WILL_DELAY_INTERVAL,24).
-define(MQTT_PROP_REQUEST_RESPONSE_INFORMATION,25).
-define(MQTT_PROP_RESPONSE_INFORMATION,26).
-define(MQTT_PROP_SERVER_REFERENCE,28).
-define(MQTT_PROP_REASON_STRING,31).
-define(MQTT_PROP_RECEIVE_MAXIMUM,33).
-define(MQTT_PROP_TOPIC_ALIAS_MAXIMUM,34).
-define(MQTT_PROP_TOPIC_ALIAS,35).
-define(MQTT_PROP_MAXIMUM_QOS,36).
-define(MQTT_PROP_RETAIN_AVAILABLE,37).
-define(MQTT_PROP_USER_PROPERTY,38).
-define(MQTT_PROP_MAXIMUM_PACKET_SIZE,39).
-define(MQTT_PROP_WILDCARD_SUBSCRIPTION_AVAILABLE,40).
-define(MQTT_PROP_SUBSCRIPTION_IDENTIFIER_AVAILABLE,41).
-define(MQTT_PROP_SHARED_SUBSCRIPTION_AVAILABLE,42).

%%
%% The reason codes.
%%
-define(MQTT_RC_SUCCESS,0).
-define(MQTT_RC_NORMAL_DISCONNECTION,0).
-define(MQTT_RC_GRANTED_QOS_0,0).
-define(MQTT_RC_GRANTED_QOS_1,1).
-define(MQTT_RC_GRANTED_QOS_2,2).
-define(MQTT_RC_DISCONNECT_WITH_WILL_MESSAGE,4).
-define(MQTT_RC_NO_MATCHING_SUBSCRIBERS,16).
-define(MQTT_RC_NO_SUBSCRIPTION_EXISTED,17).
-define(MQTT_RC_CONTINUE_AUTHENTICATION,24).
-define(MQTT_RC_RE_AUTHENTICATE,25).
-define(MQTT_RC_UNSPECIFIED_ERROR,128).
-define(MQTT_RC_MALFORMED_PACKET,129).
-define(MQTT_RC_PROTOCOL_ERROR,130).
-define(MQTT_RC_IMPLEMENTATION_SPECIFIC_ERROR,131).
-define(MQTT_RC_UNSUPPORTED_PROTOCOL_VERSION,132).
-define(MQTT_RC_CLIENT_IDENTIFIER_NOT_VALID,133).
-define(MQTT_RC_BAD_USERNAME_OR_PASSWORD,134).
-define(MQTT_RC_NOT_AUTHORIZED,135).
-define(MQTT_RC_SERVER_UNAVAILABLE,136).
-define(MQTT_RC_SERVER_BUSY,137).
-define(MQTT_RC_BANNED,138).
-define(MQTT_RC_SERVER_SHUTTING_DOWN,139).
-define(MQTT_RC_BAD_AUTHENTICATION_METHOD,140).
-define(MQTT_RC_KEEP_ALIVE_TIMEOUT,141).
-define(MQTT_RC_SESSION_TAKEN_OVER,142).
-define(MQTT_RC_TOPIC_FILTER_INVALID,143).
-define(MQTT_RC_TOPIC_NAME_INVALID,144).
-define(MATT_RC_PACKET_IDENTIFIER_IN_USE,145).
-define(MQTT_RC_PACKET_IDENTIFIER_NOT_FOUND,146).
-define(MQTT_RC_RECEIVE_MAXIMUM_EXCEEDED,147).
-define(MQTT_RC_TOPIC_ALIAS_INVALID,148).
-define(MQTT_RC_PACKET_TOO_LARGE,149).
-define(MQTT_RC_MESSAGE_RATE_TOO_HIGH,150).
-define(MQTT_RC_QUOTA_EXCEEDED,151).
-define(MQTT_RC_ADMINISTRATIVE_ACTION,152).
-define(MQTT_RC_PAYLOAD_FORMAT_INVALID,153).
-define(MQTT_RC_RETAIN_NOT_SUPPORTED,154).
-define(MQTT_RC_QOS_NOT_SUPPORTED,155).
-define(MQTT_RC_USE_ANOTHER_SERVER,156).
-define(MQTT_RC_SERVER_MOVED,157).
-define(MQTT_RC_SHARED_SUBSCRIPTIONS_NOT_SUPPORTED,158).
-define(MQTT_RC_CONNECTION_RATE_EXCEEDED,159).
-define(MQTT_RC_MAXIMUM_CONNECT_TIME,160).
-define(MQTT_RC_SUBSCRIPTION_IDENTIFIERS_NOT_SUPPORTED,161).
-define(MQTT_RC_WILDCARD_SUBSCRIPTIONS_NOT_SUPPORTED,162).

-define(MQTT_SUBSCRIBE_LEVEL_0,0).  %% Send retained messages at the time of the subscribe
-define(MQTT_SUBSCRIBE_LEVEL_1,0).  %% Send retained messages at subscribe only if the subscription does not currently exist
-define(MQTT_SUBSCRIBE_LEVEL_2,0).  %% Do not send retained messages at the time of the subscribe

-define(MQTT_RC_CONNECTION_ACCEPTED,0).
-define(MQTT_RC_CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION,1).
-define(MQTT_RC_CONNECTION_REFUSED_IDENTIFIER_REJECTED,2).
-define(MQTT_RC_CONNECTION_REFUSED_SERVER_UNAVAILABLE,3).
-define(MQTT_RC_CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD,4).
-define(MQTT_RC_CONNECTION_REFUSED_NOT_AUTHORIZED,5).

%%
%% Connect flags
%% All have the form: MQTT_CF_...
%%
-define(MQTT_CF_USER_NAME,  2#10000000).
-define(MQTT_CF_PASSWORD,   2#01000000).
-define(MQTT_CF_WILL_RETAIN,2#00100000).
-define(MQTT_CF_WILL_QOS,   2#00011000).
-define(MQTT_CF_WILL,       2#00000100).
-define(MQTT_CF_CLEAN_START,2#00000010).

%%
%% PUBACK Reason Codes
%%
-define(MQTT_PUB_RC_SUCCESS,0).
-define(MQTT_PUB_RC_NO_MATCHING_SUBSCRIBERS,16).
-define(MQTT_PUB_RC_UNSPECIFIED_ERROR,128).
-define(MQTT_PUB_RC_IMPLEMENTATION_SPECIFIC_ERROR,131).
-define(MQTT_PUB_RC_NOT_AUTHORIZED,135).
-define(MQTT_PUB_RC_TOPIC_NAME_INVALID,144).
-define(MQTT_PUB_RC_PACKET_IDENTIFIER_IN_USE,145).
-define(MQTT_PUB_RC_QUOTA_EXCEDDED,151).
-define(MATT_PUB_RC_PAYLOAD_FORMAT_INVALID,153).

-record( mqtt_connect_variable_header, {
	protocol_name = <<0,4,$M:8,$Q:8,$T:8,$T:8>> :: binary(),
	protocol_version = 0 :: integer(),
	username_flag = 0 :: integer(),
	password_flag = 0 :: integer(),
	will_retain_flag = 0 :: integer(),
	will_qos_flag = 0 :: integer(),
	will_flag = 0 :: integer() ,
	clean_start_flag = 0 :: integer(),
	flags = 0 :: integer(),
	keep_alive = 0 :: integer(),
	client_identifier = <<>> :: binary(),
	will_topic = <<>> :: binary(),
	will_message = <<>> :: binary(),
	will_properties = [] :: list(),
	will_payload = <<>> :: binary(),
	username = <<>> :: binary(),
	password = <<>> :: binary()}).

-record( mqtt_connack_variable_header_v4, {
	connect_acknowledge_flag = 0 :: integer(),
	connect_reason_code = 0 :: integer()} ).

-record( mqtt_connack_variable_header_v5, {
	connect_acknowledge_flag = 0 :: integer(),
	connect_reason_code = 0 :: integer(),
	properties = [] :: list() }).

-record( mqtt_publish_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	dup_flag = 0 :: integer(),
	qos_level_flag = 0 :: integer(),
	retain_flag = 0 :: integer(),
	topic_name = "" :: string(),
	payload = <<>> :: binary()}).

-record( mqtt_publish_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	dup_flag = 0 :: integer(),
	qos_level_flag = 0 :: integer(),
	retain_flag = 0 :: integer(),
	topic_name = "" :: string(),
	properties = [] :: list(),
	payload = <<>> :: binary()}).

-record( mqtt_puback_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	reason_code = 0 :: integer()}).

-record( mqtt_puback_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	reason_code =0 :: integer(),
	properties = [] :: list()}).

-record( mqtt_pubrec_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer()}).

-record( mqtt_pubrec_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer(),
	properties = [] :: list()}).

-record( mqtt_pubrel_variable_header_v4, {
	packet_identifier = 0 :: integer()}).

-record( mqtt_pubrel_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer(),
	properties = [] :: list()}).

-record( mqtt_pubcomp_variable_header_v4, {
	packet_identifier = 0 :: integer()}).

-record( mqtt_pubcomp_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer(),
	properties = [] :: list()}).

-record( mqtt_subscribe_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer(),
	topic_filter_list = [] :: [ { string() , integer() }]}).

-record( mqtt_subscribe_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	reason_code= 0 :: integer(),
	properties = [] :: list(),
	topic_filter_list = [] :: [ { string() , integer() }]}).

-record( mqtt_suback_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	reason_codes = [] :: [ integer() ]}).

-record( mqtt_suback_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	properties = [] :: list(),
	reason_codes = [] :: [ integer() ]}).

-record( mqtt_unsubscribe_variable_header_v4, {
	packet_identifier = 0 :: integer(),
	topic_list = [] :: [ string() ]}).

-record( mqtt_unsubscribe_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	properties = [] :: list(),
	topic_list = [] :: [ string() ]}).

-record( mqtt_unsuback_variable_header_v4, {
	packet_identifier = 0 :: integer()}).

-record( mqtt_unsuback_variable_header_v5, {
	packet_identifier = 0 :: integer(),
	properties = [] :: list(),
	reason_codes = [] :: [ integer() ]}).

-record( mqtt_msg, {
	packet_type = 0 :: integer(),
	flags = 0 :: integer(),
	remaining_length = 0 :: integer(),
	variable_header = none :: none | mqtt_msg_any() }).

-record(mqtt_connection_stats,{
	client_identifier = <<>> :: binary(),
	msg_connect = 0 :: integer(),
	msg_connack = 0 :: integer(),
	msg_publish = 0 :: integer(),
	msg_pubrec = 0 :: integer(),
	msg_puback = 0 :: integer(),
	msg_pubrel = 0 :: integer(),
	msg_pubcomp = 0 :: integer(),
	msg_subscribe = 0 :: integer(),
	msg_suback = 0 :: integer(),
	msg_unsubscribe = 0 :: integer(),
	msg_unsuback = 0 :: integer(),
	msg_pingreq = 0 :: integer(),
	msg_pingresp = 0 :: integer(),
	msg_disconnect = 0 :: integer(),
	msg_auth = 0 :: integer() }).

-type mqtt_connection_stats() :: #mqtt_connection_stats{}.

-record(mqtt_processor_state, {
	listener_pid,     %% parent listener PID
	parent_pid,       %% gen_server PID to send stats to
	peer_ip,          %% connect client IP & port
	connection_state, %% properties about the connection
	secure,           %% TLS or straight TCP,
	bytes_left,       %% bytes left in stream from previous packets
	module,           %% where should I get the functions: ssl or gen_tcp
	socket,
	stats = #mqtt_connection_stats{} :: mqtt_connection_stats(),
	version = undefined :: undefined | integer() }).

-record( mqtt_pingreq_variable_header_v4, { time }).
-record( mqtt_pingreq_variable_header_v5, { time }).

-record( mqtt_pingresp_variable_header_v4, { time }).
-record( mqtt_pingresp_variable_header_v5, { time }).

-record( mqtt_disconnect_variable_header_v4, { reason_code = 0 :: integer(), properties = [] :: list()}).
-record( mqtt_disconnect_variable_header_v5, { reason_code = 0 :: integer(), properties = [] :: list()}).

-record( mqtt_auth_variable_header_v4, { reason_code = 0 :: integer(), properties = [] :: list()}).
-record( mqtt_auth_variable_header_v5, { reason_code = 0 :: integer(), properties = [] :: list()}).

-type mqtt_processor_state() :: #mqtt_processor_state{}.

-type mqtt_msg() :: #mqtt_msg{}.
-type mqtt_connect_variable_header() :: #mqtt_connect_variable_header{}.

-type mqtt_connack_variable_header_v4() :: #mqtt_connack_variable_header_v4{}.
-type mqtt_connack_variable_header_v5() :: #mqtt_connack_variable_header_v5{}.

-type mqtt_publish_variable_header_v4() :: #mqtt_publish_variable_header_v4{}.
-type mqtt_publish_variable_header_v5() :: #mqtt_publish_variable_header_v5{}.

-type mqtt_puback_variable_header_v5() :: #mqtt_puback_variable_header_v5{}.
-type mqtt_puback_variable_header_v4() :: #mqtt_puback_variable_header_v4{}.

-type mqtt_pubrec_variable_header_v4() :: #mqtt_pubrec_variable_header_v4{}.
-type mqtt_pubrec_variable_header_v5() :: #mqtt_pubrec_variable_header_v5{}.

-type mqtt_pubrel_variable_header_v4() :: #mqtt_pubrel_variable_header_v4{}.
-type mqtt_pubrel_variable_header_v5() :: #mqtt_pubrel_variable_header_v5{}.

-type mqtt_pubcomp_variable_header_v4() :: #mqtt_pubcomp_variable_header_v4{}.
-type mqtt_pubcomp_variable_header_v5() :: #mqtt_pubcomp_variable_header_v5{}.

-type mqtt_subscribe_variable_header_v4() :: #mqtt_subscribe_variable_header_v4{}.
-type mqtt_subscribe_variable_header_v5() :: #mqtt_subscribe_variable_header_v5{}.

-type mqtt_suback_variable_header_v4() :: #mqtt_suback_variable_header_v4{}.
-type mqtt_suback_variable_header_v5() :: #mqtt_suback_variable_header_v5{}.

-type mqtt_unsubscribe_variable_header_v4() :: #mqtt_unsubscribe_variable_header_v4{}.
-type mqtt_unsubscribe_variable_header_v5() :: #mqtt_unsubscribe_variable_header_v5{}.

-type mqtt_unsuback_variable_header_v4() :: #mqtt_unsuback_variable_header_v4{}.
-type mqtt_unsuback_variable_header_v5() :: #mqtt_unsuback_variable_header_v5{}.

-type mqtt_disconnect_variable_header_v4() :: #mqtt_disconnect_variable_header_v4{}.
-type mqtt_disconnect_variable_header_v5() :: #mqtt_disconnect_variable_header_v5{}.

-type mqtt_auth_variable_header_v4() :: #mqtt_auth_variable_header_v4{}.
-type mqtt_auth_variable_header_v5() :: #mqtt_auth_variable_header_v5{}.

-type mqtt_pingreq_variable_header_v4() :: #mqtt_pingreq_variable_header_v4{}.
-type mqtt_pingreq_variable_header_v5() :: #mqtt_pingreq_variable_header_v5{}.

-type mqtt_pingresp_variable_header_v4() :: #mqtt_pingresp_variable_header_v4{}.
-type mqtt_pingresp_variable_header_v5() :: #mqtt_pingresp_variable_header_v5{}.

-type mqtt_msg_any() :: mqtt_connect_variable_header() | mqtt_connack_variable_header_v4() | mqtt_connack_variable_header_v5() | mqtt_publish_variable_header_v4() | mqtt_publish_variable_header_v5() | mqtt_puback_variable_header_v4() | mqtt_puback_variable_header_v5() | mqtt_pubrec_variable_header_v4() | mqtt_pubrec_variable_header_v5() | mqtt_pubrel_variable_header_v4() | mqtt_pubrel_variable_header_v5() |
		mqtt_pubcomp_variable_header_v4() | mqtt_pubcomp_variable_header_v5() | mqtt_subscribe_variable_header_v4() | mqtt_subscribe_variable_header_v5() | mqtt_suback_variable_header_v4() | mqtt_suback_variable_header_v5() | mqtt_unsubscribe_variable_header_v4() | mqtt_unsubscribe_variable_header_v5() | mqtt_unsuback_variable_header_v4() | mqtt_unsuback_variable_header_v5() | mqtt_disconnect_variable_header_v4() | mqtt_disconnect_variable_header_v5() |
		mqtt_auth_variable_header_v4() | mqtt_auth_variable_header_v5() | mqtt_pingreq_variable_header_v4() | mqtt_pingreq_variable_header_v5() | mqtt_pingresp_variable_header_v4() | mqtt_pingresp_variable_header_v5().

-type mqtt_answerable() :: mqtt_connect_variable_header() | mqtt_publish_variable_header_v4() | mqtt_pubrel_variable_header_v4() |
	mqtt_subscribe_variable_header_v4() | mqtt_unsubscribe_variable_header_v4() | mqtt_pingreq_variable_header_v4() | mqtt_pingreq_variable_header_v5().

-export_type([mqtt_connection_stats/0,mqtt_processor_state/0]).

-export_type([mqtt_msg_any/0,mqtt_msg/0,mqtt_connect_variable_header/0,
	mqtt_connack_variable_header_v4/0,mqtt_connack_variable_header_v5/0,
	mqtt_publish_variable_header_v4/0,mqtt_publish_variable_header_v5/0,
	mqtt_puback_variable_header_v4/0,mqtt_puback_variable_header_v5/0,
	mqtt_pubrec_variable_header_v4/0,mqtt_pubrec_variable_header_v5/0,
	mqtt_pubrel_variable_header_v4/0,mqtt_pubrel_variable_header_v5/0,
	mqtt_pubcomp_variable_header_v4/0,mqtt_pubcomp_variable_header_v5/0,
	mqtt_subscribe_variable_header_v4/0,mqtt_subscribe_variable_header_v5/0,
	mqtt_suback_variable_header_v4/0,mqtt_suback_variable_header_v5/0,
	mqtt_unsubscribe_variable_header_v4/0,mqtt_unsubscribe_variable_header_v5/0,
	mqtt_unsuback_variable_header_v4/0,mqtt_unsuback_variable_header_v5/0,
	mqtt_disconnect_variable_header_v4/0,mqtt_disconnect_variable_header_v5/0,
	mqtt_auth_variable_header_v4/0,mqtt_auth_variable_header_v5/0,
	mqtt_pingreq_variable_header_v4/0,mqtt_pingreq_variable_header_v5/0,
	mqtt_pingresp_variable_header_v4/0,mqtt_pingresp_variable_header_v5/0,
	mqtt_answerable/0]).

-endif.
