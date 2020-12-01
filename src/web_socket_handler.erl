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

-spec init( Req :: cowboy_req:req(), State ::ws_state() ) -> { cowboy_websocket, cowboy_req:req(), State::ws_state() }.
init(Req, State) ->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	NewPids = sets:add_element(self(),Pids),
	persistent_term:put(web_socket_pids,NewPids),
	{cowboy_websocket,Req,State}.

-spec websocket_init(State::ws_state())-> call_result().
websocket_init(State)->
	{ok,State}.

-spec websocket_handle(InFrame :: in_frame(),State::ws_state())-> call_result().
websocket_handle(_InFrame,State)->
	{ok,State}.

-spec websocket_info(Info::any(),State::ws_state())-> call_result().
websocket_info({frame,Format,Data},State)->
	{reply,{Format,Data},State};
websocket_info(_Info,State)->
	{ok,State}.

-spec terminate(Reason::term(), PartialReq::#{}, State::ws_state() ) -> ok.
terminate(_Reason,_PartialReq,_State)->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	NewPids = sets:del_element(self(),Pids),
	persistent_term:put(web_socket_pids,NewPids),
	ok.

send_frame( Data ) when is_binary(Data)->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	sets:fold( fun(E,A) ->
							E ! { frame, binary , Data }, A
						 end,[], Pids ),
	ok;
send_frame( Data ) when is_list(Data)->
	Pids = persistent_term:get(web_socket_pids,sets:new()),
	sets:fold( fun(E,A) ->
							E ! { frame, text, Data }, A
	           end,[], Pids ),
	ok.
