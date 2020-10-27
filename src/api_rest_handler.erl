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
-export([ content_types_provided/2, content_types_accepted/2,options/2 ]).
-export([ db_to_json/2 , json_to_db/2 ,resource_exists/2,delete_resource/2,test_get_ouis/0 ]).

-record(request_state,{ resource, method }).

init(Req, _State) ->
	io:format("Request~n"),
	{cowboy_rest,Req,#request_state{
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

options(Req0, State) ->
	io:format("Options~n"),
	Req1 = cowboy_req:set_resp_header(
		<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req0),
	Req2 = cowboy_req:set_resp_header(
		<<"access-control-allow-origin">>, <<"*">>, Req1),
	Req3 = cowboy_req:set_resp_header(
		<<"access-control-allow-headers">>, <<"authorization">>, Req2),
	{ok, Req3, State}.

json_to_db(Req, State) ->
	do( State#request_state.method , Req , State ).

db_to_json(Req, State) ->
	do( State#request_state.method , Req , State ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do( <<"GET">> , Req , #request_state{ resource = <<"ouis">> } = State ) ->
	{ok,OUIs}=oui_server:get_ouis(),
	JSON = binary:list_to_bin([<<"{ \"OUIlist\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]),
	{JSON,add_CORS(Req),State};

do( <<"GET">> , Req , #request_state{ resource = <<"oui">> } = State ) ->
	QsVals = cowboy_req:parse_qs(Req),
	OUI = proplists:get_value( <<"id">> , QsVals , <<>> ),
	case oui_server:lookup_oui(binary_to_list(OUI)) of
		{ok,Maker} ->
			JSON = binary:list_to_bin([<<"{ \"OUI\" : \"">> , OUI, <<"\" , \"Manufacturer\" : \"">>, Maker, <<"\" }">>]),
			{JSON,add_CORS(Req),State};
		{error,Reason} ->
			JSON = generate_error(101,Reason),
			{JSON,add_CORS(Req),State}
	end;

do( <<"GET">> , Req , #request_state{ resource = <<"makers">> } = State ) ->
	{ok,OUIs}=oui_server:get_makers(),
	JSON = binary:list_to_bin([<<"{ \"Manufacturers\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]),
	{JSON,add_CORS(Req),State};

do( <<"GET">> , Req , #request_state{ resource = <<"maker">> } = State ) ->
	QsVals = cowboy_req:parse_qs(Req),
	Maker = proplists:get_value( <<"id">> , QsVals , <<>> ),
	case oui_server:lookup_maker(binary_to_list(Maker)) of
		{ok,OUIs} ->
			JSON = binary:list_to_bin([<<"{ \"Manufacturer\" : \"">> , Maker, <<"\" , \"OUIs\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]),
			{JSON,add_CORS(Req),State};
		{error,Reason} ->
			JSON = generate_error(101,Reason),
			{JSON,add_CORS(Req),State}
	end;

do( <<"HEAD">> , Req , State) ->
	{<<>>,Req,State};

do( <<"OPTIONS">> , Req , State) ->
	{<<>>,Req,State}.


%%% Helper functions
dump_string_array(A)->
	dump_string_array(A,<<>>).

dump_string_array([],Blob)->
	Blob;
dump_string_array([H1,H2|T],Blob)->
	%% io:format("OUIs: ~p (~p)~n",[H1,Blob]),
	dump_string_array([H2|T],binary:list_to_bin([Blob, $" , H1, $", $,]));
dump_string_array([H1|_],Blob)->
	binary:list_to_bin([ Blob, $" , H1, $" ]).


test_get_ouis()->
	{ok,OUIs}=oui_server:get_ouis(),
	binary:list_to_bin([<<"{ \"OUIlist\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]).

add_CORS(Req)->
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).

generate_error(Error,Reason)->
	binary:list_to_bin(["{ \"ErrorCode\" : ", integer_to_binary(Error), ", \"ErrorDescription\" : \"",
		Reason, "\" }"]).
