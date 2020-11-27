%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 25. November 2020 @ 17:07:25
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_comm).
-author("helge").

-include("../include/common.hrl").


%%-----------------------------------------------------------------------------
%% types and specifications

-record (c_state, {
	socket :: gen_tcp:socket(),
	ap :: pid()
}).


-type options() :: [
	{host, string()} | 		% tip controller host name
	{port, integer()} | 	% port to connect
	{certs, binary()}		% in memory PEM file of all certs (CA,Cert, Key .. in that order)
].



-export_type([options/0]).


%%------------------------------------------------------------------------------
%% API

-export ([start_link/1,create_comm/2]).


-spec start_link (Options :: options()) -> {ok, pid()}.

start_link (Options) ->
	Pid = spawn_link(?MODULE,create_comm,[Options,self()]),
	{ok, Pid}.
	





%%------------------------------------------------------------------------------
%% internals

-spec create_comm (Options :: options(), AP :: pid()) -> ok.

create_comm (Opts, AP) ->
	H = proplists:get_value(host,Opts),
	P = proplists:get_value(port,Opts),
	C = proplists:get_value(certs,Opts),
	[{'Certificate',CA,not_encrypted},
	 {'Certificate',Cert,not_encrypted},
	 {KeyType,Key,not_encrypted}] = public_key:pem_decode(C),
	State = #c_state{
		socket = connect_to_server(H,P,CA,Cert,{KeyType,Key}),
		ap = AP
	},
	comm_loop(State).




-spec comm_loop (State :: #c_state{}) -> ok.

comm_loop (_State) ->
	%% @TODO: proper socket handling ...
	io:format("connected ... loop ... exit"),
	ok.



-spec connect_to_server (Host, Port, CA, Cert, Key) -> Socket when
		Host :: string(),					%% host name to connect to (can be IP address in string format)
		Port :: integer(),					%% port to connect to
		CA :: public_key:der_encoded(),		%% server certificate
		Cert :: public_key:der_encoded(),	%% client certificate
		Key :: {atom(),public_key:der_encoded()},	%% private key for client cert
		Socket :: gen_tcp:socket().			


connect_to_server (Host, Port, CA, Cert, Key) -> 
	Opts = [{cacerts, [CA]},
			{cert,Cert},
			{key,Key},
			{versions, ['tlsv1.2','tlsv1.3']},
			{session_tickets,auto},
			{mode,binary}],
	case ssl:connect(Host, Port, Opts) of
		{ok, Socket} -> Socket;
		{error, Reason} -> 
			?L_E(?DBGSTR("connecting to ~s:~B failed with reason: ~p",[Host,Port,Reason])),
			exit(Reason)
	end.


