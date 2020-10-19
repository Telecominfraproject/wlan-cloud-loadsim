%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 11:34 p.m.
%%%-------------------------------------------------------------------
-author("stephb").

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
	msg_auth = 0 :: integer()
}).

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
	version = 0 :: integer()
}).

-type mqtt_processor_state() :: #mqtt_processor_state{}.

-export_type([mqtt_connection_stats/0,mqtt_processor_state/0]).
