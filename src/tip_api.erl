%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 21. Dec 2020 1:34 p.m.
%%%-------------------------------------------------------------------
-module(tip_api).
-author("stephb").

-include("../include/simengine.hrl").

%% API
-export([login/1,tip_locations/0,clients/0,equipment/0,clients_old/0]).


login(SimName)->
	_=inets:start(),
	try
		{ok,Sim} = simengine:get(SimName),
		io:format("Trying to log into: ~p...~n",[Sim#simulation.opensync_server_name]),
		ServerName = binary_to_list(Sim#simulation.opensync_server_name),
		LoginURIBase = "https://" ++ ServerName ++ ":" ++ integer_to_list(9051),
		LoginURI =  LoginURIBase ++ "/management/v1/oauth2/token",
		LoginPassword = <<"{ \"userId\": \"support@example.com\", \"password\": \"support\" }">>,
		{ ok, { {_,200,_},_Headers,Body}} = httpc:request(post, {LoginURI, [],["application/json"],LoginPassword}, [], []),
		Map = jiffy:decode(Body,[return_maps]),
		%% io:format("R=~p  ~p~n",[ResultCode,Map]),
		persistent_term:put(tip_access_token,binary_to_list(maps:get(<<"access_token">>,Map))),
		persistent_term:put(tip_uri_base,LoginURIBase),
		io:format("TIP Logged in.~n")
	catch
		_:_ -> io:format("Could not log into TIP.~n")
	end.

token()->
	persistent_term:get(tip_access_token).

uri_base()->
	persistent_term:get(tip_uri_base).

tip_locations()->
	URI = uri_base() ++ "/portal/customer?customerId=2",
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Details = maps:get(<<"details">>,M),
	Auto = maps:get(<<"autoProvisioning">>,Details),
	LocationId = maps:get(<<"locationId">>,Auto),
	LocationId.

create_pagination_context(#{})->
	uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 500 }"}]);
create_pagination_context(Context)->
	JSON=jiffy:encode(Context),
	uri_string:compose_query([{"paginationContext",binary_to_list(JSON)}]).

get_all(BaseURI)->
	get_all(BaseURI,#{},[]).

get_all(_BaseURI,#{ <<"lastPage">> := LastPage } = _Context, Acc) when LastPage == true ->
	Acc;
get_all(BaseURI,Context,Acc)->
	PC = create_pagination_context(Context),
	URI = uri_base() ++ "/portal/equipment/forCustomer?customerId=2&" ++ PC,
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Array = maps:get(<<"items">>,M),
	NewContext = maps:get(<<"context">>,M),
	io:format("Returned ~p items NewContext = ~p.~n",[length(Array),NewContext]),
	get_all(BaseURI,NewContext,[length(Array)|Acc]).


equipment()->
	PC = uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 500 }"}]),
	URI = uri_base() ++ "/portal/equipment/forCustomer?customerId=2&" ++ PC,
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Array = maps:get(<<"items">>,M),
	Res = lists:foldl(fun(E,A)->
		%% Details = maps:get(<<"details">>,E),
		InventoryID = maps:get( <<"inventoryId">>,E),
		[InventoryID|A]
	                  end,[],Array),
	Res.

clients()->
	get_all("/portal/client/session/forCustomer?customerId=2&").

clients_old()->
	PC = uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 500 }"}]),
	URI = uri_base() ++ "/portal/client/session/forCustomer?customerId=2&" ++ PC,
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Array = maps:get(<<"items">>,M),
	Context = maps:get(<<"context">>,M),
	Cursor = maps:get(<<"cursor">>,Context),
	LastPage = maps:get(<<"lastPage">>,Context),
	io:format("Got ~p sessions last=~p context=~p ~n",[length(Array),LastPage,Context]),
	PC2 = uri_string:compose_query([
		                               {"paginationContext","{ \"cursor\" : \"" ++ binary_to_list(Cursor) ++ "\", \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 500 }"}]),
	URI2 = uri_base() ++ "/portal/client/session/forCustomer?customerId=2&" ++ PC2,
	{ok,{{_,200,_},_Headers2,Body2}} = httpc:request(get,{URI2,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M2 = jiffy:decode(Body2,[return_maps]),
	Array2 = maps:get(<<"items">>,M2),
	Context2 = maps:get(<<"context">>,M2),
	% Cursor2 = maps:get(<<"cursor">>,Context2),
	LastPage2 = maps:get(<<"lastPage">>,Context2),
	io:format("Got ~p sessions last=~p context=~p~n",[length(Array2),LastPage2,Context2]),
	io:format("CONTEXT: ~p~n",[Context2]).
