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
-export([login/1,tip_locations/0,clients/0,equipments/0,client_ids/0,client_ids_for_equipment/1,equipment_ids/0,
	client_macs_for_equipment/1]).

loginEndpoint() ->
	case init:get_argument(tipauth) of
		{ok,[["1"]]} ->
			"/management/v1/oauth2/token";
		{ok,[["2"]]} ->
			"/management/cmap/oauth2/token";
		_ ->
			"/management/v1/oauth2/token"
	end.

loginUserAndPassword() ->
	case init:get_argument(tipauth) of
		{ok,[["1"]]} ->
			<<"{ \"userId\": \"support@example.com\", \"password\": \"support\" }">>;
		{ok,[["2"]]} ->
			<<"{ \"userId\": \"support@example.com\", \"password\": \"Support!\" }">>;
		_ ->
			<<"{ \"userId\": \"support@example.com\", \"password\": \"support\" }">>
	end.

tip_port() ->
	case init:get_argument(tipauth) of
		{ok,[["1"]]} ->
			30251;
		{ok,[["2"]]} ->
			30151;
		_ ->
			30251
	end.

% Options foe SSL: {cacerts, client_cacerts()}
% {server_name_indication, sni()} ,where sni() is disable.

login(SimName)->
	_=inets:start(),
	try
		{ok,Sim} = simengine:get(SimName),
		ServerName = binary_to_list(Sim#simulation.opensync_server_name),
		LoginURIBase = "https://" ++ ServerName ++ ":" ++ integer_to_list(tip_port()),
		LoginURI =  LoginURIBase ++ loginEndpoint(),
		LoginPassword = loginUserAndPassword(),
		_ = case httpc:request(post, {LoginURI, [],["application/json"],LoginPassword}, [], []) of
			{ ok, { {_,200,_},_Headers,Body}} ->
				Map = jiffy:decode(Body,[return_maps]),
				persistent_term:put(tip_access_token,binary_to_list(maps:get(<<"access_token">>,Map))),
				persistent_term:put(tip_uri_base,LoginURIBase),
				ok;
			Error ->
				?L_IA("OWLS cannot access the TIP controller. URI='~p' Error=~p",[LoginURI, Error])
		end,
		ok
	catch
		_:_ ->
			?L_I("Could not log into TIP."),
			error
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
	uri_string:compose_query([{"paginationContext","{ \"model_type\": \"PaginationContext\", \"maxItemsPerPage\": 250 }"}]);
create_pagination_context(Context)->
	NewContext = binary_to_list(jiffy:encode(Context)),
	uri_string:compose_query([{"paginationContext",NewContext}]).

get_all(BaseURI)->
	get_all(BaseURI,"",[]).

get_all(BaseURI,Cursor,Acc)->
	PC = create_pagination_context(Cursor),
	% io:format("Context: ~s~n",[PC]),
	URI = uri_base() ++ BaseURI ++ PC,
	{ok,{{_,200,_},_Headers,Body}} = httpc:request(get,{URI,[{"Authorization","Bearer " ++ token()}]},[],[]),
	M = jiffy:decode(Body,[return_maps]),
	Array = maps:get(<<"items">>,M),
	NewContext = maps:get(<<"context">>,M),
	% L = length(Array),
	case  maps:get(<<"lastPage">>,NewContext) of
		true ->
			% io:format("Total elements: ~p~n",[length(Acc)+length(Array)]),
			Acc ++ Array;
		false ->
			% io:format("Just got ~p elements so far ~p ~n",[L,length(Acc)+L]),
			get_all(BaseURI,NewContext,Acc ++ Array)
	end.

equipments()->
	get_all("/portal/equipment/forCustomer?customerId=2&").

equipment_ids()->
	E = equipments(),
	[ X || #{ <<"id">> := X } <- E ].

clients()->
	get_all("/portal/client/session/forCustomer?customerId=2&").

client_ids()->
	C = clients(),
	[ X || #{ <<"equipmentId">> := X } <- C ].

client_ids_for_equipment(E)->
	Query = uri_string:compose_query([{"equipmentIds",integer_to_list(E)},{"customerId","2"}]),
	get_all("/portal/client/session/forCustomer?" ++ Query ++ "&" ).

client_macs_for_equipment(E)->
	Query = uri_string:compose_query([{"equipmentIds",integer_to_list(E)},{"customerId","2"}]),
	C = get_all("/portal/client/session/forCustomer?" ++ Query ++ "&" ),
	[ X || #{ <<"macAddress">> := #{ <<"addressAsString">> := X }} <- C ].
