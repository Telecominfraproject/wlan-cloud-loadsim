%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2020 3:38 p.m.
%%%-------------------------------------------------------------------
-module(api_rest_handler).
-author("stephb").

%% API
-export([ init/2,allowed_methods/2,is_authorized/2 ]).
-export([ content_types_provided/2, content_types_accepted/2 ]).
-export([ db_to_json/2 , json_to_db/2 ,resource_exists/2,delete_resource/2 ]).

-record(request_state,{ resource, method }).

init(Req, State) ->
	{cowboy_rest,Req,State#request_state{
		resource = cowboy_req:binding( restype , Req , undefined ),
		method = cowboy_req:method(Req)}}.

allowed_methods(Req, State) ->
	{[<<"GET">>,<<"HEAD">>,<<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, db_to_json}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, json_to_db}], Req, State}.

is_authorized(Req, State) ->
	{true, Req, State }.

delete_resource(Req, State) ->
	{ true , Req , State }.

resource_exists(Req, State) ->
	{true, Req, State}.

json_to_db(Req, State) ->
	do( State#request_state.method , Req , State ).

db_to_json(Req, State) ->
	do( State#request_state.method , Req , State ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do( <<"GET">> , Req , State ) ->
	_QsVals = cowboy_req:parse_qs(Req),
	cowboy_req:reply(200,maps:new(),<<>>,Req),
	{ stop , Req , State }.




