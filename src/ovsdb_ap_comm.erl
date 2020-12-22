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
	{id, binary()}	|		% for tagging messages the ID of the AP node
	{cacert, binary()}	|		% in memory PEM file of the server CA chain
	{cert, binary()} |
	{key, {atom(),binary() } }
].

-export_type([options/0]).

-record (c_state, {
	options :: options(),
	id :: binary(),
	socket :: none | ssl:sslsocket(),
	ap :: pid(),
	status :: active | idle | error,
	restart = 250 :: integer(),
	rxb = <<"">> :: binary()
}).


%%------------------------------------------------------------------------------
%% API

-export ([start_link/1,create_comm/2,send_term/2,end_comm/1,start_comm/1]).


-spec start_link (Options :: options()) -> {ok, pid()}.
start_link (Options) ->
	Pid = spawn_link(?MODULE,create_comm,[Options,self()]),
	{ok, Pid}.

-spec send_term(Comm :: pid(), Data :: term()) -> ok | generic_error().
send_term (Comm, Data) when is_pid(Comm) andalso is_map(Data) ->
	Comm ! {send, self(), Data},
	ok;
send_term (Comm, Data) when is_pid(Comm) andalso is_binary(Data) ->
	Comm ! {send_raw, self(), Data},
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
		id = proplists:get_value(id,Opts,<<"unknown AP">>),
		status = idle
	},
	comm_loop(State).

-spec comm_loop (State :: #c_state{}) -> ok.
comm_loop (#c_state{socket=S, rxb=Rx, ap=AP, id=ID, options=Opts}=State) ->
	receive 
		{ssl, S, Data} ->
			{Buffer, JSON} = process_rx_data(<<Rx/binary,Data/binary>>,AP),
			?L_I(?DBGSTR("GOT REQUEST: from ~s:~B to ~s -> ~Bbytes",[proplists:get_value(host,Opts,"unknown"),proplists:get_value(port,Opts,0),ID,byte_size(JSON)])),
			ovsdb_ap:post_event(AP,comm_event,{<<"RX">>,size(Data)},io_lib:format("receive ~Bbytes",[size(Data)])),
			case ssl:setopts(S,[{active,once}]) of
				ok ->
					comm_loop(State#c_state{status=active, rxb=Buffer});
				{error,closed} ->
					comm_loop(try_reconnect(State))
			end;

		{ssl_closed, S} ->
			ovsdb_ap:post_event(AP,comm_error,{<<"socket_closed">>},<<>>),
			?L_E(?DBGSTR("~p: Socket closed by server.",[ID])),
			comm_loop(try_reconnect(State));

		{ssl_error, S, Reason} ->
			?L_E(?DBGSTR("~p: socket error:~p. ",[ID,Reason])),
			ovsdb_ap:post_event(AP,comm_error,{<<"undefined">>},io_lib:format("got SSL error: ~p",[Reason])),
			_ = ssl:close(S),
			comm_loop(try_reconnect(State));

		{send, AP, Data} ->
			case S of
				none ->
					?L_E(?DBGSTR("trying to send data with no socket")),
					ovsdb_ap:post_event(AP,comm_error,{<<"send_error">>},<<"trying to send data with no socket">>),
					comm_loop(State#c_state{status=error});
				_ ->
					ToSend = iolist_to_binary(jiffy:encode(Data)),
					%ToSendP = jiffy:encode(Data,[pretty]),
					%?L_I(?DBGSTR("Sending: ~B bytes of date",[size(ToSend)])),
					?L_I(?DBGSTR("SENDING RESPONSE: from ~s to ~s:~B -> ~Bbytes",[ID,proplists:get_value(host,Opts,"unknown"),proplists:get_value(port,Opts,0),byte_size(ToSend)])),
					%?L_I(?DBGSTR("SENDING RESPONSE: from ~s to ~s:~B -> ~n~s~n",[ID,proplists:get_value(host,Opts,"unknown"),proplists:get_value(port,Opts,0),ToSendP])),
					case ssl:send(S,[ToSend,<<"\n">>]) of
						ok ->
							ovsdb_ap:post_event(AP,comm_event,{<<"TX">>,size(ToSend)},io_lib:format("sending ~Bbytes",[size(ToSend)])),
							comm_loop(State#c_state{status=active});
						{error, closed} ->
							ovsdb_ap:post_event(AP,comm_error,{<<"send_error">>},<<"trying to send data with no socket">>),
							?L_E(?DBGSTR("trying to send data with closed socket")),
							comm_loop(try_reconnect(State))
					end
			end;

		{send_raw, AP, Data} ->
			case S of 
				none ->
					?L_E(?DBGSTR("trying to send raw data with no socket")),
					ovsdb_ap:post_event(AP,comm_error,{<<"send_error">>},<<"trying to send raw data with no socket">>),
					comm_loop(State#c_state{status=error});
				_ ->
					?L_I(?DBGSTR("SENDING RAW RESPONSE: from ~s to ~s:~B -> ~Bbytes",[ID,proplists:get_value(host,Opts,"unknown"),proplists:get_value(port,Opts,0),byte_size(Data)])),
					case ssl:send(S,Data) of
						ok ->
							ovsdb_ap:post_event(AP,comm_event,{<<"TX">>,byte_size(Data)},io_lib:format("sending ~Bbytes",[byte_size(Data)])),
							comm_loop(State#c_state{status=active});
						{error, closed} ->
							ovsdb_ap:post_event(AP,comm_error,{<<"send_error">>},<<"trying to send raw data with no socket">>),
							?L_E(?DBGSTR("trying to send raw data with closed socket")),
							comm_loop(try_reconnect(State))
					end
			end;

		{start, AP} ->
			NewState = start_connection(State),
			ovsdb_ap:post_event(AP,comm_event,{<<"started">>},<<>>),
			comm_loop(NewState);

		{down, AP} ->
			ovsdb_ap:post_event(AP,comm_event,{<<"shutdown">>},<<>>),
			case S of 
				none -> ok; 
				_ -> ssl:close(S)
			end

	after
		10000 ->
			ovsdb_ap:post_event(AP,comm_event,{<<"idle">>},<<>>),
			comm_loop(State#c_state{status=idle})
	end.

-spec start_connection (State :: #c_state{}) -> NewState :: #c_state{}.
start_connection (#c_state{options=Opts}=State) ->
	H = proplists:get_value(host,Opts),
	P = proplists:get_value(port,Opts),
	CAs = proplists:get_value(ca,Opts),
	Cert = proplists:get_value(cert,Opts),
	Key = proplists:get_value(key,Opts),
	State#c_state{
		socket = connect_to_server(H,P,CAs,Cert,Key,State#c_state.ap),
		status = active
	}.

%--------try_reconnect/1-----------------reconnect to broken sockets with exponential back-off and random jitter
-spec try_reconnect (State :: #c_state{}) -> NewState :: #c_state{}.
try_reconnect (#c_state{restart=R, ap=AP}=State) ->
	Rj = R + rand:uniform(250) - 125,
	?L_I(?DBGSTR("socket closed by server, trying to reconnect in ~.2fsec",[Rj/1000])),
	_ = timer:send_after(Rj,{start, AP}),  %% should be more a reset of the AP!
	ovsdb_ap:post_event(AP,sock_recon,{Rj},io_lib:format("socket abnormally closed -> reconnect attempt in ~.2fsec",[Rj/1000])),
	State#c_state{socket=none, status=error, restart=min(R*2,32000)}.




%% Host :: string(),					%% host name to connect to (can be IP address in string format)
%% Port :: integer(),					%% port to connect to
%% CAs:: [public_key:der_encoded()],	%% server certificate
%% Cert :: public_key:der_encoded(),	%% client certificate
%% Key :: {atom(),public_key:der_encoded()},	%% private key for client cert
%% Socket :: ssl:sslsocket().
-spec connect_to_server(Host::string(), Port::integer(), CAs::binary(), Cert::binary(), Key::{atom(),binary()},AP::binary()) -> Socket::ssl:sslsocket().
connect_to_server (Host, Port, CAs, Cert, Key,AP) ->
	Opts = [{cacerts, [CAs]},
					{cert,Cert},
					{key,Key},
					{versions, ['tlsv1.2','tlsv1.3']},
					{session_tickets,auto},
					{mode,binary},
					{keepalive, true},
					{packet,raw},
					{active,once},
					{recbuf, 250000},
    				{sndbuf, 250000}],
	%% io:format(">>>~p: Trying to connect.",[AP]),
	?L_I(?DBGSTR("~p: AP connecting to ~s:~B",[AP,Host,Port])),
	case ssl:connect(Host, Port, Opts) of
		{ok, Socket} ->
			Socket;
		{error, Reason} -> 
			?L_E(?DBGSTR("~p: Connecting to ~s:~B failed with reason: ~p",[AP,Host,Port,Reason])),
			exit(Reason)
	end.

%--------process_rx_data/2---------------process data in buffer and ensures only valid decoded JSON is sent to AP
-spec process_rx_data (Data :: binary(), AP :: pid()) -> {Buffer :: binary(), PrettyJSON :: binary()}.
process_rx_data (Data, AP) ->
	try jiffy:decode(Data,[return_maps,copy_strings,return_trailer]) of
		{has_trailer,Map,Tail} ->
			ovsdb_ap:rpc_cmd(AP,Map),
			{iolist_to_binary(Tail),iolist_to_binary(jiffy:encode(Map))};
		Map ->
			ovsdb_ap:rpc_cmd(AP,Map),
			{<<"">>,iolist_to_binary(jiffy:encode(Map))}
	catch
		error:{N,Error} ->
			?L_IA("JSON decode error: '~s' after ~B bytes",[Error,N]),
			Data;
		_:_ ->
			<<"">>
	end.
