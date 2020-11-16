%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2020 11:32 a.m.
%%%-------------------------------------------------------------------
-module(restutils).
-author("stephb").

-include("../include/common.hrl").

%% API
-export([create_paginated_return/3,dump_string_array/1,get_access_token/1,add_CORS/1,generate_error/2,get_pagination_parameters/1,paginate/2,validate_token/1]).

-record(pagination_info,{limit=0, offset=0, previous_offset=0,
	next_offset=0, current_page=0, page_count=0, total_count=0}).

%%% create paginated return
create_paginated_return(Header,List,PaginationInfo )->
	binary:list_to_bin(
		[ "{ \"Items\": { \"" ++ Header ++ "\" : [ ",
			dump_string_array(List),
				" ] }, " ++
				dump_pagination_info(PaginationInfo),"} "]).

dump_pagination_info(PI) ->
	"\"Meta\": {
		\"Limit\": " ++ integer_to_list(PI#pagination_info.limit) ++ ", " ++
		"\"Offset\" : " ++ integer_to_list(PI#pagination_info.offset) ++ ", " ++
		"\"PreviousOffset\": " ++ integer_to_list(PI#pagination_info.previous_offset) ++ ", " ++
		"\"NextOffset\": " ++ integer_to_list(PI#pagination_info.next_offset) ++ ", " ++
		"\"CurrentPage\": " ++ integer_to_list(PI#pagination_info.current_page) ++ ", " ++
		"\"PageCount\": " ++ integer_to_list(PI#pagination_info.page_count) ++ ", " ++
		"\"TotalCount\": " ++ integer_to_list(PI#pagination_info.total_count) ++ " }".

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

add_CORS(Req)->
	cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req).

generate_error(Error,Reason)->
	binary:list_to_bin(["{ \"ErrorCode\" : ", integer_to_binary(Error), ", \"ErrorDescription\" : \"",
		Reason, "\" }"]).

validate_token(Token)->
	application:get_env(?OWLS_APP,rest_api_token,"") == binary_to_list(Token).

get_pagination_parameters(Req) ->
	#{ offset := Offset , limit := Limit, filter := Filter } = cowboy_req:match_qs([{offset,int,1},{limit,int,0},{filter,[],<<>>}],Req),
	{ Offset, Limit, Filter }.

paginate( { Offset, Limit, Filter}, List ) ->
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
	TotalCount = length(List1),
	{ ReturnList, NextOffset, CurrentPage } = case Limit == 0 of
		                                          true -> {lists:nthtail(Offset-1,List1),0,1} ;
		                                          false -> {lists:sublist(List1,Offset,Limit),Offset+Limit,1+(Offset div Limit) }
	                                          end,
	PageCount = case Limit == 0 of
		            true ->1;
		            false -> TotalCount div Limit
	            end,
	{ReturnList,#pagination_info{
		limit = Limit , offset = Offset , previous_offset = Offset , total_count = TotalCount,
		next_offset = NextOffset, current_page = CurrentPage,
		page_count = PageCount }}.
