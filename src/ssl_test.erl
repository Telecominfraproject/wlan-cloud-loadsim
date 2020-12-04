%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 03. Dec 2020 11:50 a.m.
%%%-------------------------------------------------------------------
-module(ssl_test).
-author("stephb").

%% API
-export([start/0,server/0,client/0]).

-define(S_CERT,"certs/test_certs/server-mqtt-1--cert.pem").
-define(S_KEY ,"certs/test_certs/server-mqtt-1--key.pem").
-define(S_DKEY,"certs/test_certs/server-mqtt-1--key_dec.pem").

-define(C_CERT,"certs/test_certs/client-sim1-1-000032-cert.pem").
-define(C_KEY ,"certs/test_certs/client-sim1-1-000032-key.pem").
-define(C_DKEY,"certs/test_certs/client-sim1-1-000032-key_dec.pem").
-define(C_CA  ,"certs/test_certs/sim1_cert.pem").

-define(PORT,11000).

start()->
	_=ssl:start(),
	spawn(?MODULE,server,[]),
	timer:sleep(2000),
	spawn(?MODULE,client,[]).

server()->
	{ ok , ListenSocket } = ssl:listen(?PORT,[
%%				                        {log_level,debug},
				                        {session_tickets,stateless},
				                        {mode,binary},
				                        {versions,['tlsv1.2','tlsv1.3']},
				                        {active,false},
				                        {reuseaddr,true},
				                        {certfile,?S_CERT},
				                        {keyfile,?S_KEY}
]),
	accept(ListenSocket).

accept(ListenSocket) ->
	_=case ssl:transport_accept(ListenSocket) of
		{ok,Socket} ->
			case ssl:handshake(Socket) of
				  { ok, SslSocket } ->
					  _=ssl:setopts(SslSocket,[{active,true}]),
					  server_processor_active(SslSocket),
					  io:format("Client is done...~n");
				  Error ->
					  io:format("Client error is done: ~p...~n",[Error]),
					  ssl:close(Socket)
			end;
		Error ->
			io:format("Client error done: ~p...~n",[Error])
	end,
	accept(ListenSocket).

%% server_processor_passive(Socket)->
%%	case ssl:recv(Socket,10) of
%%		{ok,Data} ->
%%			io:format("Received ~p bytes: ~p.~n",[size(Data),Data]);
%%		Error ->
%%			io:format("Error ~p.~n",[Error])
%%	end,
%%	server_processor_passive(Socket).

server_processor_active(Socket)->
	receive
		{ssl,Socket,Data} ->
			io:format("Received ~p bytes: ~p.~n",[size(Data),Data]),
			server_processor_active(Socket);
		{ssl_closed,Socket} ->
			io:format("Closing socket.~n");
		Anything ->
			io:format("Unknown message: ~p.~n",[Anything]),
			server_processor_active(Socket)
	end.

client()->
%%	{ok,Cacerts}=file:read_file(?C_CA),
	{ok,SSL}=ssl:connect("renegademac.arilia.com",?PORT,
	                     [
		                     %% {log_level,debug},
		                     {session_tickets,auto},
		                     {versions, ['tlsv1.2','tlsv1.3']},
		                     {certfile,?C_CERT},
		                     {keyfile,?C_KEY},
		                     {active,false },
		                     binary]),
	send_data(SSL).

send_data(SSL)->
	_ = ssl:send(SSL,<<"foo">>),
	%% io:format("Sending: ~p~n",[E]),
	timer:sleep(1000),
	send_data(SSL).