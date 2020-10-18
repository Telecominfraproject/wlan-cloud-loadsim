%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 11:34 p.m.
%%%-------------------------------------------------------------------
-author("stephb").


-record(mqtt_processor_state, {
	listener_pid,   %% parent listener PID
	parent_pid,       %% gen_server PID to send stats to
	peer_ip,          %% connect client IP & port
	connection_state, %% properties about the connection
	secure,           %% TLS or straight TCP,
	bytes_left,       %% bytes left in stream from previous packets
	module,            %% where should I get the functions: ssl or gen_tcp
	socket,
	remaining_length
}).
