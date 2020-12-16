%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2020 3:38 p.m.
%%%-------------------------------------------------------------------
-module(manager_api_rest_handler).
-author("stephb").

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").
-include("../include/inventory.hrl").
-include("../include/simengine.hrl").

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

-record(request_state,{
					resource = nothing :: nothing | binary(),
					id = nothing :: nothing | binary(),
					method :: binary(),
					looked_up :: any(),
					time_in}).

init(Req, _State) ->
	Res =cowboy_req:binding( restype , Req , nothing ),
	Id = cowboy_req:binding( resid , Req , nothing ),
	%% io:format("REQUEST: ~p ~p ~n  RES: ~p~n  ID: ~p~n",[Req,cowboy_req:method(Req),Res,Id]),
	Method = cowboy_req:method(Req),
	{ cowboy_rest,Req,#request_state{
		resource = Res,
		id = Id,
		method = Method,
		looked_up = undefined,
		time_in = os:system_time() }}.

allowed_methods(Req, State) ->
	{[?HTTP_GET,?HTTP_OPTIONS,?HTTP_POST,?HTTP_DELETE], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, db_to_json},
		{{<<"text">>,<<"html">>,<<"charset=ISO-8859-1">>},db_to_html}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, json_to_db}], Req, State}.

is_authorized(Req,#request_state{ method = <<"OPTIONS">> }=State)->
	{true,Req,State};
is_authorized(Req, State) ->
	Answer = case restutils:get_access_token_not_secure(Req) of
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

resource_exists(Req, #request_state{ method = ?HTTP_GET, id=nothing}=State) ->
	{true, Req, State};
resource_exists(Req, #request_state{ method = ?HTTP_GET, resource = <<"cas">>}=State) ->
	case inventory:get_ca(State#request_state.id) of
		{ok,Record}  -> 	{true, Req, State#request_state{ looked_up = Record }};
		{error,Reason}   ->  create_error(101,Reason,Req,State)
	end;
resource_exists(Req, #request_state{ method = ?HTTP_GET, resource = <<"simulations">>}=State) ->
	case simengine:get(State#request_state.id) of
		{ok,Record}  -> 	{true, Req, State#request_state{ looked_up = Record }};
		{error,Reason}   ->  create_error(101,Reason,Req,State)
	end;
resource_exists(Req, #request_state{ method = ?HTTP_GET, resource = <<"actions">>}=State) ->
	case simengine:get_action(State#request_state.id) of
		{ok,Record}  -> 	{true, Req, State#request_state{ looked_up = Record }};
		{error,Reason}   ->  create_error(101,Reason,Req,State)
	end;
resource_exists(Req, #request_state{ method = ?HTTP_POST, resource = <<"simulations">>}=State) ->
	case simengine:get(State#request_state.id) of
		{ok,_Record}  -> 	{true, Req, State};
		_             ->  {false, Req, State}
	end;
resource_exists(Req, #request_state{ method = ?HTTP_GET, resource = <<"ouis">>}=State) ->
	case oui_server:lookup_oui(binary_to_list(State#request_state.id)) of
		{ok,Record}  -> 	{true, Req, State#request_state{ looked_up = Record }};
		{error,Reason}   ->  create_error(101,Reason,Req,State)
	end;
resource_exists(Req, #request_state{ method = ?HTTP_GET, resource = <<"vendors">>}=State) ->
	case oui_server:lookup_vendor(binary_to_list(State#request_state.id)) of
		{ok,Record}  -> 	{true, Req, State#request_state{ looked_up = Record }};
		{error,Reason}   ->  create_error(101,Reason,Req,State)
	end;
resource_exists(Req,State)->
	{true, Req, State}.

options(Req0, State) ->
	%% io:format("Calling OPTIONS/2~n"),
	Req1 = case State#request_state.resource of
						<<"cas">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
					  <<"ouis">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
						<<"vendors">> -> cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, OPTIONS">>, Req0);
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
	Maker = State#request_state.looked_up,
	JSON = binary:list_to_bin([<<"{ \"OUI\" : \"">> , State#request_state.id, <<"\" , \"Vendor\" : \"">>, Maker, <<"\" }">>]),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"vendors">> , id = nothing } = State ) ->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,Vendors}=oui_server:get_vendors(),
	{ SubList , PaginationInfo } = restutils:paginate(PaginationParameters,Vendors),
	JSON = restutils:create_paginated_return("Vendors",SubList,PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"vendors">> } = State ) ->
	Maker = State#request_state.id,
	JSON = binary:list_to_bin([<<"{ \"Vendor\" : \"">> , Maker, <<"\" , \"OUIs\" : [ ">>,
	                           restutils:dump_string_array(State#request_state.looked_up), <<" ] }">>]),
	{JSON,restutils:add_CORS(Req),State};

%% list CAs
do( ?HTTP_GET ,Req,#request_state{resource = <<"cas">>,id=nothing}=State)->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,CAs}=inventory:get_cas(),
	{SubList,PaginationInfo} = restutils:paginate(PaginationParameters,CAs),
	JSON = restutils:create_paginated_return("CAs",SubList,PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET ,Req,#request_state{resource = <<"cas">>}=State)->
	CA = State#request_state.looked_up,
	{ _ , RawKey } = CA#ca_info.key,
	CAInfo = #{ name => CA#ca_info.name,
		key => list_to_binary(base64:encode_to_string(RawKey)),
		cert => list_to_binary(base64:encode_to_string(CA#ca_info.cert)),
		location => CA#ca_info.dir_name,
		configuration => CA#ca_info.config_data },
	JSON = jiffy:encode(CAInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"nodes">> , id = nothing } = State ) ->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,AllNodes}=manager:connected_nodes(),
	{ SubList, PaginationInfo }  = restutils:paginate(PaginationParameters,[{node(),manager}|AllNodes]),
	JSON = case restutils:get_parameter(details,0,Req) of
		0 -> NamesOnly = [ atom_to_list(X) || {X,Role} <- SubList, Role == node  ],
				 restutils:create_paginated_return( "Nodes" , NamesOnly, PaginationInfo);
		1 -> restutils:create_paginated_return( "Nodes" , SubList, PaginationInfo,nodes)
	end,
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET ,Req,#request_state{resource = <<"hardware_definitions">>,id=nothing}=State)->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,Definitions}=hardware:get_definitions(),
	{SubList,PaginationInfo} = restutils:paginate_record_list(PaginationParameters,Definitions),
	JSON = restutils:create_paginated_return("HardwareDefinitions",SubList,PaginationInfo,hardware_info),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET ,Req,#request_state{resource = <<"simulations">>,id=nothing}=State)->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,Simulations}=simengine:list_simulations(),
	{SubList,PaginationInfo} = restutils:paginate(PaginationParameters,Simulations),
	JSON = restutils:create_paginated_return("Simulations",SubList,PaginationInfo),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"simulations">> } = State ) ->
	S = State#request_state.looked_up,
	{ServerName,Port} = case S#simulation.servers of
												undefined -> {<<"unknown">>,0};
												Server ->  {Server#sim_entry.opensync_server_name, Server#sim_entry.opensync_server_port}
											end,
	Sim = #{ name => S#simulation.name, caname => S#simulation.ca, num_devices => S#simulation.num_devices, nodes => S#simulation.nodes,
		server => ServerName, port=> Port , assets_created => S#simulation.assets_created },
	{jiffy:encode(Sim),restutils:add_CORS(Req),State};

do( ?HTTP_POST , Req , #request_state{ resource = <<"simulations">> } = State ) ->
	{ok,Data,Req1} = cowboy_req:read_body(Req),
	Res = jiffy:decode(Data,[return_maps]),
	case validate(simulations,Res) of
		true ->
			#{ <<"num_devices">> := NumDevices, <<"port">> := Port, <<"server">> := Server , <<"nodes">> := Nodes, <<"caname">> := CAName } = Res,
			case simengine:get(State#request_state.id) of
				{ok,Simulation}-> %% we are updating a simulation
					NewSim = Simulation#simulation{
						ca = CAName,
						name = State#request_state.id,
						num_devices = NumDevices,
						servers = #sim_entry{ opensync_server_port = Port, opensync_server_name = Server },
						nodes = utils:to_atom_list(Nodes) },
					simengine:update(NewSim),
					URI = <<  <<"/api/v1/simulations/">>/binary, (State#request_state.id)/binary >>,
					io:format("URI: ~p~n",[URI]),
					Req2 = cowboy_req:set_resp_header(<<"location">>, URI, Req1),
					{true,restutils:add_CORS(Req2),State};
				_ ->  %% we are creating a new simulation
					NewSim = #simulation{
						ca = CAName,
						name = State#request_state.id,
						num_devices = NumDevices,
						servers = #sim_entry{ opensync_server_port = Port, opensync_server_name = Server },
						nodes = utils:to_atom_list(Nodes),
						assets_created = false },
					simengine:create(NewSim),
					URI = <<  <<"/api/v1/simulations/">>/binary, (State#request_state.id)/binary >>,
					%% io:format("URI: ~p~n",[URI]),
					Req2 = cowboy_req:set_resp_header(<<"location">>, URI, Req1),
					{true,restutils:add_CORS(Req2),State}
			end;
		false ->
			create_error(102,"Some fields are invalid or missing. Must have at least 11 valid node, port must not be 0, caname must exist",Req1,State)
	end;

do( ?HTTP_GET ,Req,#request_state{resource = <<"actions">>,id=nothing}=State)->
	PaginationParameters = restutils:get_pagination_parameters(Req),
	{ok,Actions}=simengine:list_actions(),
	{SubList,PaginationInfo} = restutils:paginate_record_list(PaginationParameters,Actions),
	JSON = restutils:create_paginated_return("Actions",SubList,PaginationInfo,sim_action),
	{JSON,restutils:add_CORS(Req),State};

do( ?HTTP_GET , Req , #request_state{ resource = <<"actions">> } = State ) ->
	S = State#request_state.looked_up,
	{simengine:sim_action_to_json(S),restutils:add_CORS(Req),State};

do( ?HTTP_POST , Req , #request_state{ resource = <<"actions">> } = State ) ->
	{ok,Data,Req1} = cowboy_req:read_body(Req),
	Res = jiffy:decode(Data,[return_maps]),
	case validate(action,Res) of
		true ->
			#{ <<"action">> := Action, <<"simulation">> := SimName, <<"parameters">> := AttributesRaw} = Res,
			Attributes = process_attributes(AttributesRaw),
			{ok,Id} = case Action of
				<<"prepare">> ->
					simengine:prepare(SimName,Attributes,utils:noop_mfa());
				<<"push">> ->
					simengine:push(SimName,Attributes,utils:noop_mfa());
				<<"start">> ->
					simengine:start(SimName,Attributes,utils:noop_mfa());
				<<"stop">> ->
					simengine:stop(SimName,Attributes,utils:noop_mfa());
				<<"pause">> ->
					simengine:pause(SimName,Attributes,utils:noop_mfa());
				<<"cancel">> ->
					simengine:cancel(SimName,Attributes,utils:noop_mfa());
				<<"restart">> ->
					simengine:restart(SimName,Attributes,utils:noop_mfa())
			end,
			URI = <<  <<"/api/v1/actions/">>/binary, Id/binary >>,
			%% io:format("URI: ~p~n",[URI]),
			Body = #{ action => Action, simulation => SimName, id => Id },
			Req2 = cowboy_req:set_resp_header(<<"location">>, URI, Req1),
			Req3 = cowboy_req:set_resp_body( jiffy:encode(Body) , Req2),
			{true,restutils:add_CORS(Req3),State};
		false ->
			create_error(102,"Some fields are invalid or missing. Must have at least 11 valid node, port must not be 0, caname must exist",Req1,State)
	end;

do( ?HTTP_HEAD , Req , State) ->
	io:format("HEAD~n"),
	{<<>>,Req,State}.

process_attributes(AttributesRaw)->
	process_attributes(AttributesRaw,#{}).

process_attributes([],Acc)->
	Acc;
process_attributes([H|T],Acc)->
	#{ <<"name">> := Name, <<"value">> := Value} = H,
	case Name of
		<<"stagger">> ->
			[ Devices , Interval ] = string:tokens( binary_to_list(Value), "/" ),
			process_attributes( T, maps:put(stagger,{ list_to_integer(string:trim(Devices)) , list_to_integer(string:trim(Interval))}, Acc) );
		_ ->
			process_attributes(T,Acc)
	end.


create_error(Error,Reason,Req,State) ->
	Req1 = cowboy_req:set_resp_body(restutils:generate_error(Error,Reason), Req),
	{false, restutils:add_CORS(Req1),State}.

validate(simulations,Data)->
	try
		#{ <<"caname">> := CAName, <<"nodes">> := Nodes, <<"num_devices">> := NumDevices,
		   <<"server">> := _Server, <<"port">> := Port } = Data,
		true = (length(Nodes)>0) andalso
		       validate_name(cas,CAName) andalso
		       validate_name(nodes,Nodes) andalso
		       (Port>0) andalso
		       (NumDevices>0),
		true
	catch
		_:_ ->
			false
	end;
validate(action,Data)->
	ValidActions = [ <<"push">>, <<"start">>, <<"stop">>, <<"pause">>, <<"cancel">>, <<"restart">>, <<"prepare">> ],
	try
		#{ <<"action">> := Action, <<"simulation">> := SimName, <<"parameters">> := _Attributes} = Data,
		lists:member(Action,ValidActions) andalso validate_name(simulations,SimName)
	catch
		_:_ ->
			false
	end.

validate_name(cas,Name)->
	case inventory:get_ca(Name) of
		{ok,_}  -> true;
					_ -> false
	end;
validate_name(simulations,Name)->
	case simengine:get(Name) of
		{ok,_} -> true;
		_ -> false
	end;
validate_name(nodes,[])->
	true;
validate_name(nodes,[H|T])->
	NodeName=binary_to_atom(H),
	case lists:member(NodeName,nodes()) of
		true ->
			validate_name(nodes,T);
		false ->
			false
	end.
