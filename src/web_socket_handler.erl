%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2020 11:28 p.m.
%%%-------------------------------------------------------------------
-module(web_socket_handler).
-author("stephb").

%% API
-export([init/2,websocket_init/1,websocket_handle/2,websocket_info/2,terminate/3,send_frame/1]).

-type in_frame() :: ping | pong | {text | binary | ping | pong, binary()}.
-type out_frame() :: cow_ws:frame().
-type call_result() :: {ok, ws_state()}
		| {ok, ws_state(), hibernate}
		| {reply, out_frame() | [out_frame()], ws_state()}
		| {reply, out_frame() | [out_frame()], ws_state(), hibernate}
		| {stop, ws_state()}.
-type ws_state() :: any().

-record( conn_state, { pid, keep_alive :: timer:tref() }).

-spec init( Req :: cowboy_req:req(), State ::ws_state() ) -> { cowboy_websocket, cowboy_req:req(), State::ws_state() }.
init(Req, State) ->
	%% io:format("Web socket init.~p..~n",[self()]),
	Opts = #{compress => true},
	{cowboy_websocket,Req,State}.

-spec websocket_init(State::ws_state())-> call_result().
websocket_init(_State)->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	NewPids = sets:add_element(self(),Pids),
	persistent_term:put(web_socket_pids,NewPids),
	{ok,TRef} = timer:send_interval(20000,ping),
	%% io:format("Web socket starting. ~p..~n",[self()]),
	{ok,#conn_state{ pid = self(), keep_alive = TRef }}.

-spec websocket_handle(InFrame :: in_frame(),State::ws_state())-> call_result().
websocket_handle(pong,State)->
	{ok,State};
websocket_handle(InFrame,State)->
	io:format("Web socket: ~p~n",[InFrame]),
	{ok,State}.

-spec websocket_info(Info::any(),State::ws_state())-> call_result().
websocket_info({frame,Format,Data},State)->
	%% io:format("Web socket message.~p..2~n",[self()]),
	{reply,{Format,Data},State};
websocket_info(ping,State)->
	%% io:format("Web socket message.~p..2~n",[self()]),
	{reply,ping,State};
websocket_info(_Info,State)->
	%% io:format("Web socket starting: ~p..3.~n",[self()]),
	{ok,State}.

-spec terminate(Reason::term(), PartialReq::#{}, State::ws_state() ) -> ok.
terminate(_Reason,_PartialReq,_State)->
%%	io:format("Web socket closing: ~p..~n",[self()]),
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	NewPids = sets:del_element(self(),Pids),
	persistent_term:put(web_socket_pids,NewPids),
	ok.

-spec send_frame(Data::binary()|string())->ok.
send_frame(Data)->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	sets:fold( fun(E,A) ->
%%						io:format("sending to ~p~n",[E]),
							E ! { frame, text, Data }, A
	           end,[], Pids ),
	ok.
