%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2020 3:36 p.m.
%%%-------------------------------------------------------------------
-module(node_finder).
-author("stephb").

-behaviour(gen_server).

%% API
-export([start_link/0,broadcaster/1,creation_info/0,receiver/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(node_finder_state, { broadcaster }).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #node_finder_state{}} | {ok, State :: #node_finder_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{ ok , Ref } = timer:apply_interval(10000,?MODULE,broadcaster,[self()]),
	{ok, #node_finder_state{ broadcaster = Ref }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #node_finder_state{}) ->
	{reply, Reply :: term(), NewState :: #node_finder_state{}} |
	{reply, Reply :: term(), NewState :: #node_finder_state{}, timeout() | hibernate} |
	{noreply, NewState :: #node_finder_state{}} |
	{noreply, NewState :: #node_finder_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #node_finder_state{}} |
	{stop, Reason :: term(), NewState :: #node_finder_state{}}).
handle_call(_Request, _From, State = #node_finder_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #node_finder_state{}) ->
	{noreply, NewState :: #node_finder_state{}} |
	{noreply, NewState :: #node_finder_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_finder_state{}}).
handle_cast(_Request, State = #node_finder_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #node_finder_state{}) ->
	{noreply, NewState :: #node_finder_state{}} |
	{noreply, NewState :: #node_finder_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_finder_state{}}).
handle_info(_Info, State = #node_finder_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #node_finder_state{}) -> term()).
terminate(_Reason, State = #node_finder_state{}) ->
	_=timer:cancel(State#node_finder_state.broadcaster),
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #node_finder_state{},
		Extra :: term()) ->
	{ok, NewState :: #node_finder_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #node_finder_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
send_payload(_,_,_,-1)->
	ok;
send_payload(Socket,Payload,StartPort,HowMany)->
	SockAddr = #{ family => inet, port => StartPort+HowMany, addr => broadcast },
	_=socket:sendto(Socket,Payload,SockAddr),
	send_payload(Socket,Payload,StartPort,HowMany-1).

broadcaster(_Pid)->
	Cookie = erlang:get_cookie(),
	Key = crypto:hash(sha256,atom_to_binary(Cookie)),
	Data = term_to_binary({atom_to_list(Cookie),atom_to_list(node())},[compressed]),
	Payload = crypto:crypto_one_time(aes_256_ctr,Key,<<0:128>>,Data,true),
	{ ok , Socket } = socket:open(inet,dgram,udp),
	_=socket:setopt(Socket,socket,broadcast,true),
	send_payload( Socket, Payload, 19000,100 ),
	socket:close(Socket).

receiver(Id)->
	case socket:open(inet,dgram,udp) of
		{ok,S} ->
			io:format("Opening a socket...~n"),
			_ = socket:bind(S,#{ family => inet, addr => any, port => 19000+Id}),
			R = case socket:recv(S,2000) of
						{ok,Data} ->
							Cookie = erlang:get_cookie(),
							Key = crypto:hash(sha256,atom_to_binary(Cookie)),
							Payload = crypto:crypto_one_time(aes_256_ctr,Key,<<0:128>>,Data,false),
							try
				        { _ , Node } = erlang:binary_to_term(Payload,[safe]),
								list_to_atom(Node)
							catch
								_:_ ->
									{ error , unknown }
							end;
						{error,_Reason} = Error ->
							Error
					end,
				_ = socket:close(S),
				io:format("Closing a socket...~n"),
			R;
		{error,_Reason} = Error ->
			Error
	end.
