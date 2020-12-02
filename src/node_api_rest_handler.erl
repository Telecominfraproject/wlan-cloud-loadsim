%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2020 3:38 p.m.
%%%-------------------------------------------------------------------
-module(node_api_rest_handler).
-author("stephb").

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").

-define(HTTP_GET,<<"GET">>).
-define(HTTP_POST,<<"POST">>).
-define(HTTP_PUT,<<"PUT">>).
-define(HTTP_DELETE,<<"DELETE">>).
-define(HTTP_OPTIONS,<<"OPTIONS">>).
-define(HTTP_HEAD,<<"HEAD">>).

%% API
-export([ init/2,allowed_methods/2,is_authorized/2 ]).
-export([ content_types_provided/2, content_types_accepted/2,options/2 ]).
-export([ db_to_json/2 , json_to_db/2 ,resource_exists/2,delete_resource/2]).

-record(request_state,{resource,id,method,time_in}).

init(Req, _State) ->
	Res =cowboy_req:binding( restype , Req , nothing ),
	Id = cowboy_req:binding( resid , Req , nothing ),
	io:format("REQUEST: ~p ~n  RES: ~p~n  ID: ~p~n",[cowboy_req:method(Req),Res,Id]),
	{ cowboy_rest,Req,#request_state{
		resource = Res,
		id = Id,
		method = cowboy_req:method(Req),
		time_in = os:system_time() }}.

allowed_methods(Req, State) ->
	{[<<"GET">>,<<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, db_to_json},
		{{<<"text">>,<<"html">>,<<"charset=ISO-8859-1">>},db_to_html}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, json_to_db}], Req, State}.

is_authorized(Req,#request_state{ method = <<"OPTIONS">> }=State)->
	{true,Req,State};
is_authorized(Req, State) ->
	Answer = case restutils:get_access_token(Req) of
		{ok,Token} ->
			case restutils:validate_token(Token) of
				true ->
					{true, Req, State };
				false ->
					io:format("Access not granted: token=~p~n",[Token]),
					{{false, <<"Bearer">>}, Req, State}
			end;
    _ ->
	    io:format("No access.~n"),
	    {{false, <<"Bearer">>}, Req, State}
	end,
	Answer.

delete_resource(Req, State) ->
	{ true , Req , State }.

resource_exists(Req, State) ->
	{true, Req, State}.

options(Req0, State) ->
	%% io:format("Calling OPTIONS/2~n"),
	Req1 = case State#request_state.resource of
						<<"cas">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
					  <<"ouis">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
						<<"makers">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
					  _ -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0)
	       end,
	Req2 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
	Req3 = cowboy_req:set_resp_header(
		<<"Pragma">>, <<"no-cache">>, Req2),
	Req4 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Credentials">>, <<"true">>, Req3),
	Req5 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Headers">>, <<"*">>, Req4),
	%% io:format("REQ5=~p~n",[Req5]),
	{ok, Req5, State}.

json_to_db(Req, State) ->
	do( State#request_state.method , Req , State ).

db_to_json(Req, State) ->
	do( State#request_state.method , Req , State ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do( ?HTTP_GET , Req , #request_state{ resource = <<"ouis">> , id = nothing } = State ) ->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,OUIs}=oui_server:get_ouis(),
	{ SubList, PaginationInfo }  = restutils:paginate(PaginationParameters,OUIs),
	JSON = restutils:create_paginated_return( "OUIs" , SubList, PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"ouis">> } = State ) ->
	%% io:format(">>>>OUIS...~n"),
	OUI = State#request_state.id,
	case oui_server:lookup_oui(binary_to_list(OUI)) of
		{ok,Maker} ->
			JSON = binary:list_to_bin([<<"{ \"OUI\" : \"">> , OUI, <<"\" , \"Manufacturer\" : \"">>, Maker, <<"\" }">>]),
			{JSON,restutils:add_CORS(Req),State};
		{error,Reason} ->
			JSON = restutils:generate_error(101,Reason),
			{JSON,restutils:add_CORS(Req),State}
	end;

do( ?HTTP_GET , Req , #request_state{ resource = <<"makers">> , id = nothing } = State ) ->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,Makers}=oui_server:get_vendors(),
	{ SubList , PaginationInfo } = restutils:paginate(PaginationParameters,Makers),
	JSON = restutils:create_paginated_return("Manufacturers",SubList,PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"makers">> } = State ) ->
	Maker = State#request_state.id,
	case oui_server:lookup_vendor(binary_to_list(Maker)) of
		{ok,OUIs} ->
			JSON = binary:list_to_bin([<<"{ \"Manufacturer\" : \"">> , Maker, <<"\" , \"OUIs\" : [ ">>, restutils:dump_string_array(OUIs), <<" ] }">>]),
			{JSON,restutils:add_CORS(Req),State};
		{error,Reason} ->
			JSON = restutils:generate_error(101,Reason),
			{JSON,restutils:add_CORS(Req),State}
	end;

%% list CAs
do( ?HTTP_GET ,Req,#request_state{resource = <<"cas">>,id=nothing}=State)->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,CAs}=inventory:get_cas(),
	{SubList,PaginationInfo} = restutils:paginate(PaginationParameters,CAs),
	JSON = restutils:create_paginated_return("CAs",SubList,PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_HEAD , Req , State) ->
	io:format("HEAD~n"),
	{<<>>,Req,State}.


