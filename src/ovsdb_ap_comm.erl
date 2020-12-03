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

-type options() :: [
	{host, string()} | 		% tip controller host name
	{port, integer()} | 	% port to connect
	{ca, binary()}	|		% in memory PEM file of the server CA chain
	{cert, binary()}		% 
].

-export_type([options/0]).

-record (c_state, {
	options :: options(),
	socket :: none | ssl:sslsocket(),
	ap :: pid(),
	status :: active | idle,
	restarts = 0 :: integer(),
	max_restarts = 50 :: integer(),
	rxb = <<"">> :: binary()
}).


%%------------------------------------------------------------------------------
%% API

-export ([start_link/1,create_comm/2,send_term/2,end_comm/1,start_comm/1]).


-spec start_link (Options :: options()) -> {ok, pid()}.

start_link (Options) ->
	Pid = spawn_link(?MODULE,create_comm,[Options,self()]),
	{ok, Pid}.
	


-spec send_term(Comm :: pid(), Data :: term()) -> ok | {error, Reason :: string()}.

send_term (Comm, Data) when is_pid(Comm) andalso is_map(Data) ->
	Comm ! {send, self(), Data},
	ok;

send_term (_,_) ->
	{error,"Data needs to be a map"}.



-spec end_comm (Comm :: pid()) -> ok.

end_comm (Comm) ->
	Comm ! {down, self()},
	ok.


-spec start_comm (Comm :: pid()) -> ok.

start_comm (Comm) ->
	Comm ! {start, self()},
	ok.



%%------------------------------------------------------------------------------
%% internals

-spec create_comm (Options :: options(), AP :: pid()) -> ok.

create_comm (Opts, AP) ->
	State = #c_state{
		options = Opts,
		socket = none,
		ap = AP,
		status = idle
	},
	comm_loop(State).




-spec comm_loop (State :: #c_state{}) -> ok.

comm_loop (#c_state{socket=S, rxb=Rx, ap=AP}=State) ->
	receive 
		{ssl, S, Data} ->
			Buffer = process_rx_data(<<Rx/binary,Data/binary>>,AP),
			case ssl:setopts(S,[{active,once}]) of
				ok ->
					comm_loop(State#c_state{status=active, rxb=Buffer});
				{error,closed} ->
					comm_loop(try_reconnect(State))
			end;

		{ssl_closed, S} ->
			comm_loop(try_reconnect(State));

    	{ssl_error, S, Reason} ->
			?L_E(?DBGSTR("socket error ~p",[Reason])),
			ssl:close(S);

		{send, AP, Data} ->
			case S of
				none ->
					?L_E(?DBGSTR("trying to send data with no socker"));
				_ ->
					ToSend = jiffy:encode(Data),
					?L_I(?DBGSTR("Sending: ~B bytes of date",[size(ToSend)])),
					case ssl:send(S,ToSend) of
						ok ->
							comm_loop(State#c_state{status=active});
						{error, closed} ->
							comm_loop(try_reconnect(State))
					end
			end;

		{start, AP} ->
			NewState = start_connection(State),
			comm_loop(NewState);

		{down, AP} ->
			case S of 
				none -> ok; 
				_ -> ssl:close(S)
			end

	after
		10000 ->
			comm_loop(State#c_state{status=idle})
	end.



-spec start_connection (State :: #c_state{}) -> NewState :: #c_state{}.

start_connection (#c_state{options=Opts}=State) ->
	H = proplists:get_value(host,Opts),
	P = proplists:get_value(port,Opts),
	CAs = [X || {'Certificate',X,not_encrypted} <- public_key:pem_decode(proplists:get_value(ca,Opts))],
	[{'Certificate',Cert,not_encrypted},
	 {KeyType,Key,not_encrypted}] = public_key:pem_decode(proplists:get_value(cert,Opts)),
	State#c_state{
		socket = connect_to_server(H,P,CAs,Cert,{KeyType,Key}),
		status = active
	}.



-spec try_reconnect (State :: #c_state{}) -> NewState :: #c_state{}.

try_reconnect (#c_state{restarts=R, max_restarts=MR, ap=AP}=State) when R < MR ->
	?L_I(?DBGSTR("socket closed by server, trying to reconnect (~B of ~B)",[R+1,MR])),
	_ = timer:send_after(1000,{start, AP}),
	State#c_state{socket=none, restarts=R+1};

try_reconnect (#c_state{ap=AP}=State) ->
	?L_E(?DBGSTR("socket closed by server too many times, bailing out :(")),
	self() ! {down, AP},
	State#c_state{socket=none}.




-spec connect_to_server (Host, Port, CAs, Cert, Key) -> Socket when
		Host :: string(),					%% host name to connect to (can be IP address in string format)
		Port :: integer(),					%% port to connect to
		CAs:: [public_key:der_encoded()],	%% server certificate
		Cert :: public_key:der_encoded(),	%% client certificate
		Key :: {atom(),public_key:der_encoded()},	%% private key for client cert
		Socket :: ssl:sslsocket().			


connect_to_server (Host, Port, CAs, Cert, Key) -> 
	Opts = [{cacerts, CAs},
			{cert,Cert},
			{key,Key},
			{versions, ['tlsv1.2','tlsv1.3']},
			{session_tickets,auto},
			{mode,binary},
			{keepalive, true},
			{packet,raw},
			{active,once}],
	?L_I(?DBGSTR("AP connecting to ~s:~B",[Host,Port])),
	case ssl:connect(Host, Port, Opts) of
		{ok, Socket} -> Socket;
		{error, Reason} -> 
			?L_E(?DBGSTR("connecting to ~s:~B failed with reason: ~p",[Host,Port,Reason])),
			exit(Reason)
	end.




%--------process_rx_data/2---------------process data in buffer and ensures only valid decoded JSON is sent to AP

-spec process_rx_data (Data :: binary(), AP :: pid()) -> Buffer :: binary().

process_rx_data (Data, AP) ->
	try jiffy:decode(Data,[return_maps,copy_strings,return_trailer]) of
		{has_trailer,Map,Tail} ->
			ovsdb_ap:rpc_cmd(AP,Map),
			iolist_to_binary(Tail);
		Map ->
			ovsdb_ap:rpc_cmd(AP,Map),
			<<"">>
	catch
		error:{_,truncated_json} ->
			Data;
		_:_ ->
			<<"">>
	end.
