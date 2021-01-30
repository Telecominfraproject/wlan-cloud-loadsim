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
-export([ create_paginated_return/4,dump_string_array/1,get_access_token/1,
          add_CORS/1,generate_error/2,get_pagination_parameters/1,paginate/2,
					get_access_token_not_secure/1,get_parameter/3,paginate_record_list/2,create_json_string_array/2]).

-record(pagination_info,{limit=0, offset=0, previous_offset=0,
	next_offset=0, current_page=0, page_count=0, total_count=0}).

%%% create paginated return
create_paginated_return(Header,List,PaginationInfo,stringlist )->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
			dump_string_array(List),
				" ] }, " ++
				dump_pagination_info(PaginationInfo),"} "]);
create_paginated_return(Header,List,PaginationInfo,nodes )->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
			  utils:json_node_info(List),
		  " ] }, " ++
		  dump_pagination_info(PaginationInfo),"} "]);
create_paginated_return(Header,List,PaginationInfo, hardware_info )->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
		  dump_record_list(hardware_info,List,<<>>),
		  " ] }, " ++
		  dump_pagination_info(PaginationInfo),"} "]);
create_paginated_return(Header,List,PaginationInfo, sim_action )->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
		  dump_record_list(sim_action,List,<<>>),
		  " ] }, " ++
		  dump_pagination_info(PaginationInfo),"} "]);
create_paginated_return(Header,List,PaginationInfo,Type )->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
		  dump_record_array(List,Type),
		  " ] }, " ++
		  dump_pagination_info(PaginationInfo),"} "]).

create_json_string_array(Header,List)->
	binary:list_to_bin(
		[ "{ \"Data\": { \"" ++ Header ++ "\" : [ ",
		  dump_string_array(List),
		  " ] }" ]).

dump_record_list(_,[],Acc)->
	Acc;
dump_record_list(hardware_info,[H1,H2|T],Acc)->
	dump_record_list(hardware_info,[H2|T],<<Acc/binary, (jiffy:encode(hardware_info:to_json(H1)))/binary, (<<" ,">>)/binary >>);
dump_record_list(hardware_info,[H1],Acc)->
	<<Acc/binary, (jiffy:encode(hardware_info:to_json(H1)))/binary >>;
dump_record_list(sim_action,[H1,H2|T],Acc)->
	dump_record_list(sim_action,[H2|T],<<Acc/binary, (simengine:sim_action_to_json(H1))/binary, (<<" ,">>)/binary >>);
dump_record_list(sim_action,[H1],Acc)->
	<<Acc/binary, (simengine:sim_action_to_json(H1))/binary >>.

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

dump_record_array(L,Type)->
	dump_record_array(L,Type,<<>>).

dump_record_array([],_,Blob)->
	Blob;
dump_record_array([H1,H2|T],Type,Blob)->
	dump_record_array([H2|T],binary:list_to_bin([Blob, ${ , Type:to_json(H1), $}, $,]));
dump_record_array([H1|_],Type,Blob)->
	binary:list_to_bin([ Blob, ${ , Type:to_json(H1), $} ]).

-spec get_access_token_not_secure(Req::term()) -> { ok, Token::binary()} | { error, Reason::term()}.
get_access_token_not_secure(_Req) ->
	{ok,<<"1234567890">>}.

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
	cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, <<"*">>, Req).

generate_error(Error,Reason) when is_list(Reason)->
	generate_error(Error,list_to_binary(Reason));
generate_error(Error,Reason)->
	E = #{ 'ErrorCode' =>  integer_to_binary(Error), 'ErrorDescription' => Reason },
	jiffy:encode(E).

get_pagination_parameters(Req) ->
	#{ offset := Offset , limit := Limit, filter := Filter } = cowboy_req:match_qs([{offset,int,1},{limit,int,0},{filter,[],<<>>}],Req),
	{ Offset, Limit, Filter }.

-spec get_parameter(Parameter::atom(),Default::any(),Req :: cowboy_req:req()) -> any().
get_parameter(Parameter, Default, Req) when is_integer(Default) ->
	#{ Parameter := Value } = cowboy_req:match_qs([{Parameter,int,Default}],Req),
	Value.

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

paginate_record_list( { Offset, Limit, _Filter}, List ) ->
	%% filter the list
	TotalCount = length(List),
	{ ReturnList, NextOffset, CurrentPage } = case Limit == 0 of
		                                          true -> {lists:nthtail(Offset-1,List),0,1} ;
		                                          false -> {lists:sublist(List,Offset,Limit),Offset+Limit,1+(Offset div Limit) }
	                                          end,
	PageCount = case Limit == 0 of
		            true ->1;
		            false -> TotalCount div Limit
	            end,
	{ReturnList,#pagination_info{
		limit = Limit , offset = Offset , previous_offset = Offset , total_count = TotalCount,
		next_offset = NextOffset, current_page = CurrentPage,
		page_count = PageCount }}.
