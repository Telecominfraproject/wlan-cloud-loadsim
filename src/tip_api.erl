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
-export([login/1,tip_locations/0,clients/0,equipments/0]).

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

create_pagination_context("")->
	uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 100 }"}]);
create_pagination_context(Cursor)->
	uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"cursor\" : \"" ++ Cursor ++ "\" , \"maxItemsPerPage\": 100 }"}]).

get_all(BaseURI)->
	get_all(BaseURI,"",0).

get_all(BaseURI,Context,Acc)->
	PC = create_pagination_context(Context),
	io:format("Context: ~s~n",[Context]),
	URI = uri_base() ++ BaseURI ++ PC,
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Array = maps:get(<<"items">>,M),
	NewContext = maps:get(<<"context">>,M),
	L = length(Array),
	case  maps:get(<<"lastPage">>,NewContext) of
		true ->
			io:format("Total elements: ~p~n",[Acc+length(Array)]);
		false ->
			Cursor = binary_to_list(maps:get(<<"cursor">>,NewContext)),
			io:format("New cursor: ~p~n",[Cursor]),
			io:format("Just got ~p elements so far ~p ~n",[L,Acc+L]),
			get_all(BaseURI,Cursor,Acc+L)
	end.

equipments()->
	get_all("/portal/equipment/forCustomer?customerId=2&").

clients()->
	get_all("/portal/client/session/forCustomer?customerId=2&").
