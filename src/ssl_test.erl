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

-define(S_CERT,"certs_db/sim1/servers/server-mqtt-1-cert.pem").
-define(S_KEY ,"certs_db/sim1/servers/server-mqtt-1-key.pem").
-define(S_DKEY,"certs_db/sim1/servers/server-mqtt-1-key_dec.pem").

-define(C_CERT,"certs_db/sim1/clients/client-sim1-1-000010-cert.pem").
-define(C_KEY ,"certs_db/sim1/clients/client-sim1-1-000010-key.pem").
-define(C_DKEY,"certs_db/sim1/clients/client-sim1-1-000010-key_dec.pem").
-define(C_CA  ,"certs_db/sim1/sim1_cert.pem").

-define(PORT,11000).

start()->
	_=ssl:start(),
	spawn(?MODULE,server,[]),
	timer:sleep(2000),
	spawn(?MODULE,client,[]).

server()->
	{ok,Cert}=utils:pem_to_cert(?S_CERT),
	{ok,Key}=utils:pem_to_key(?S_KEY),

	{ ok , ListenSocket } = ssl:listen(?PORT,[
%%				                        {log_level,debug},
				                        {session_tickets,stateless},
				                        {mode,binary},
				                        {versions,['tlsv1.2','tlsv1.3']},
				                        {active,false},
				                        {reuseaddr,true},
				                        {cert,Cert},
				                        {key,Key}
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
	{ok,Cert}=utils:pem_to_cert(?C_CERT),
	{ok,Key}=utils:pem_to_key(?C_KEY),
	{ok,Cacert}=utils:pem_to_cert(?C_CA),
	{ok,SSL}=ssl:connect("renegademac.arilia.com",?PORT,
	                     [
		                     %% {log_level,debug},
		                     {session_tickets,auto},
		                     {versions, ['tlsv1.2','tlsv1.3']},
		                     {cacerts,[Cacert]},
		                     {cert,Cert},
		                     {key,Key},
		                     {active,false },
		                     binary]),
	send_data(SSL).

send_data(SSL)->
	_ = ssl:send(SSL,<<"foo">>),
	%% io:format("Sending: ~p~n",[E]),
	timer:sleep(1000),
	send_data(SSL).