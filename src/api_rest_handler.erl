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

-include("../include/internal.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-export([ init/2,allowed_methods/2,is_authorized/2 ]).
-export([ content_types_provided/2, content_types_accepted/2,options/2 ]).
-export([ db_to_json/2 , json_to_db/2 ,resource_exists/2,delete_resource/2,test_get_ouis/0 ]).

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
	Answer = case get_access_token(Req) of
		{ok,Token} ->
			case validate_token(Token) of
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
	Req1 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0),
	Req2 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Origin">>, <<"*">>, Req1),
	Req3 = cowboy_req:set_resp_header(
		<<"Pragma">>, <<"no-cache">>, Req2),
	Req4 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Credentials">>, <<"true">>, Req3),
	Req5 = cowboy_req:set_resp_header(
		<<"Access-Control-Allow-Headers">>, <<"*">>, Req4),
	{ok, Req5, State}.

json_to_db(Req, State) ->
	do( State#request_state.method , Req , State ).

db_to_json(Req, State) ->
	do( State#request_state.method , Req , State ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do( <<"GET">> , Req , #request_state{ resource = <<"ouis">> , id = nothing } = State ) ->
	PaginationParameters = get_pagination_parameters(Req),
	{ok,OUIs}=oui_server:get_ouis(),
	SubList = paginate(PaginationParameters,OUIs),
	JSON = binary:list_to_bin([<<"{ \"OUIs\" : [ ">>, dump_string_array(SubList), <<" ] }">>]),
	{JSON,add_CORS(Req),State};

do( <<"GET">> , Req , #request_state{ resource = <<"ouis">> } = State ) ->
	io:format(">>>>OUIS...~n"),
	OUI = State#request_state.id,
	case oui_server:lookup_oui(binary_to_list(OUI)) of
		{ok,Maker} ->
			JSON = binary:list_to_bin([<<"{ \"OUI\" : \"">> , OUI, <<"\" , \"Manufacturer\" : \"">>, Maker, <<"\" }">>]),
			{JSON,add_CORS(Req),State};
		{error,Reason} ->
			JSON = generate_error(101,Reason),
			{JSON,add_CORS(Req),State}
	end;

do( <<"GET">> , Req , #request_state{ resource = <<"makers">> , id = nothing } = State ) ->
	PaginationParameters = get_pagination_parameters(Req),
	{ok,Makers}=oui_server:get_makers(),
	SubList = paginate(PaginationParameters,Makers),
	JSON = binary:list_to_bin([<<"{ \"Manufacturers\" : [ ">>, dump_string_array(SubList), <<" ] }">>]),
	{JSON,add_CORS(Req),State};

do( <<"GET">> , Req , #request_state{ resource = <<"makers">> } = State ) ->
	Maker = State#request_state.id,
	case oui_server:lookup_maker(binary_to_list(Maker)) of
		{ok,OUIs} ->
			JSON = binary:list_to_bin([<<"{ \"Manufacturer\" : \"">> , Maker, <<"\" , \"OUIs\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]),
			{JSON,add_CORS(Req),State};
		{error,Reason} ->
			JSON = generate_error(101,Reason),
			{JSON,add_CORS(Req),State}
	end;

%% list CAs
do(<<"GET">>,Req,#request_state{resource = <<"cas">>,id=nothing}=State)->
	PaginationParameters = get_pagination_parameters(Req),
	{ok,CAs}=inventory:get_cas(),
	SubList = paginate(PaginationParameters,CAs),
	JSON = binary:list_to_bin([<<"{ \"CAs\" : [ ">>, dump_string_array(SubList), <<" ] }">>]),
	{JSON,add_CORS(Req),State};

do( <<"HEAD">> , Req , State) ->
	io:format("HEAD~n"),
	{<<>>,Req,State};

do( <<"OPTIONS">> , Req , State) ->
	io:format("OPTIONS~n"),
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

get_access_token(Req) ->
	Result = case cowboy_req:header(<<"x-api-key">>, Req) of
						 undefined ->
							 QsVals = cowboy_req:parse_qs(Req),
							 case proplists:get_value(<<"access_token">>, QsVals,undefined) of
								 undefined ->
									 {error,"No API Key present."};
								 Token ->
									 { ok , Token }
							 end;
						Token->
								{ok, Token}
					 end,
	Result.

%% tests
test_get_ouis()->
	{ok,OUIs}=oui_server:get_ouis(),
	binary:list_to_bin([<<"{ \"OUIlist\" : [ ">>, dump_string_array(OUIs), <<" ] }">>]).

add_CORS(Req)->
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).

generate_error(Error,Reason)->
	binary:list_to_bin(["{ \"ErrorCode\" : ", integer_to_binary(Error), ", \"ErrorDescription\" : \"",
		Reason, "\" }"]).

validate_token(Token)->
	application:get_env(?OWLS_APP,rest_api_token,"") == binary_to_list(Token).

get_pagination_parameters(Req) ->
	#{ start := Start , limit := Limit, filter := Filter } = cowboy_req:match_qs([{start,int,1},{limit,int,0},{filter,[],<<>>}],Req),
	{ Start, Limit, Filter }.

paginate( { Start, Limit, Filter}, List ) ->
	%% filter the list
	List1 = case Filter == <<>> of
						true ->
							List;
						false ->
							F1 = binary_to_list(Filter),
							lists:reverse(lists:foldl(
								fun(E,A) ->
									case string:find(E,F1,leading) of
										nomatch -> A;
										_ -> [E|A]
									end
							  end, [], List))
	        end,
	case Limit == 0 of
		true -> lists:nthtail(Start-1,List1);
		false -> lists:sublist(List1,Start,Limit)
	end.
