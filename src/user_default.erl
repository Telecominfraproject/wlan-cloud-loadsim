%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 29. Oct 2020 3:11 p.m.
%%%-------------------------------------------------------------------
-module(user_default).
-author("stephb").

-compile({parse_transform, lager_transform}).

-include("../include/common.hrl").
-include("../include/errors.hrl").
-include("../include/inventory.hrl").
-include("../include/simengine.hrl").
-include("../include/mqtt_definitions.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).

-define(SIM_APIKEY,sim_api_key).

-spec help() -> ok.
help()->
	case utils:app_env(role,none) of
		none ->
			io:format("Invalid application role. Please verify your configuration.~n");
		node ->
			node_help();
		manager ->
			manager_help()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Node commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec node_help() -> ok.
node_help()->
	io:format("login(ApiKey).    ApiKey ,ust be a string.~n"),
	io:format("logout().~n"),
	io:format("connect(Node).    Node must be a string.~n"),
	io:format("connected().~n").

-spec login( ApiKey :: string()) -> ok.
login(ApiKey)->
	persistent_term:put(?SIM_APIKEY,ApiKey).

-spec logout() -> ok.
logout()->
	persistent_term:erase(?SIM_APIKEY),
	ok.

-spec connected() -> boolean().
connected()->
	nodes() > 0.

-spec connect(NodeName::string()) -> { ok , none | node() }.
connect(NodeName) ->
	Node = list_to_atom(NodeName),
	manager:connect(Node).

-spec configuration() -> { ok , Configuration::term() }.
configuration()->
	simnode:get_configuration().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Manager commands
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec manager_help() -> ok.
manager_help()->
	io:format("refresh_ouis().~n"),
	io:format("show_nodes().~n"),
	io:format("get_node_configuration( Node ).~n"),
	io:format("set_node_configuration( Node, Configuration ).~n").

-spec create_simulation(SimName::string())-> generic_result().
create_simulation(SimName)->
	CAName = SimName,
	_=create_ca(CAName),
	create_simulation(SimName,CAName).

-spec create_simulation(SimName::string(),CAName::string())-> generic_result().
create_simulation(SimName,CAName) when is_list(SimName),is_list(CAName) ->
	{ ok , Nodes } = manager:connected_nodes(),
	%% Only keep the non-pseudo nodes
	GoodNodes = [ X || {X,Role} <- Nodes, Role == node ],
	io:format("Creation simulation: ~s~n",[SimName]),
	io:format("  -nodes(~p): ~p~n",[length(GoodNodes),GoodNodes]),
	MaxDevices = length(GoodNodes) * 10000,
	NumberOfDevices = input("Number of devices (max:" ++ integer_to_list(MaxDevices) ++ ") ", integer_to_list(MaxDevices div 2)),
	RealNumberOfDevices = utils:select( length(Nodes)==0 , list_to_integer(NumberOfDevices) , utils:adjust(list_to_integer(NumberOfDevices),length(GoodNodes))),
	{ ServerName, ServerPort } = select_servers(),
	Simulation = #simulation{ name = list_to_binary(SimName),
	                          ca = list_to_binary(CAName),
	                          num_devices = RealNumberOfDevices,
	                          opensync_server_port = ServerPort,
	                          opensync_server_name = ServerName,
	                          nodes = GoodNodes },
	Yes = input("Confirm: [Y]n","Y"),
	case Yes == "Y" of
		true ->  _ = simengine:create(Simulation), ok;
		false -> io:format("Creation aborted.~n"), { error, creation_aborted }
	end.

-spec show_simulation(SimName::string())-> {ok,Attributes::attribute_list()} | generic_error().
show_simulation(SimName) when is_list(SimName) ->
	simengine:get(SimName).

-spec prepare_simulation( SimName::string()|binary() )->ok | generic_error().
prepare_simulation(SimName)->
	simengine:prepare(SimName,#{},utils:noop_mfa()).

-spec push_simulation(SimName::string()|binary())-> ok | generic_error().
push_simulation(SimName)->
	simengine:push(SimName,#{},utils:noop_mfa()).

-spec start_simulation(SimName::string()|binary())-> ok | generic_error().
start_simulation(SimName)->
	simengine:start(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec restart_simulation(SimName::string()|binary())-> ok | generic_error().
restart_simulation(SimName)->
	simengine:restart(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec stop_simulation(SimName::string()|binary())-> ok | generic_error().
stop_simulation(SimName)->
	simengine:stop(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec pause_simulation(SimName::string()|binary())-> ok | generic_error().
pause_simulation(SimName)->
	simengine:pause(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec cancel_simulation(SimName::string()|binary())-> ok | generic_error().
cancel_simulation(SimName)->
	simengine:cancel(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec list_simulations() -> {ok,SimulationList::[string()]} | generic_error().
list_simulations() ->
	simengine:list_simulations().

-spec analyze_nodes()-> ok.
analyze_nodes()->
	{ok,Nodes}=show_nodes(),
	utils:print_nodes_info([{node(),manager}|Nodes]).

-spec show_plan(SimName::string()) -> {ok,Attributes::attribute_list()} | generic_error().
show_plan(_SimName)->
	{ok,#{}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  CA Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_ca(CAName::string())-> generic_result().
create_ca(CAName) when is_list(CAName)->
	create_ca(CAName,"").

-spec create_ca(CAName::string(),Password::string())-> generic_result().
create_ca(CAName,Password) when is_list(CAName),is_list(Password)->
	inventory:make_ca(CAName,Password).

-spec import_ca(CAName::string(),Password::string(),KeyFileName::string(),CertFileName::string())->ok | generic_error().
import_ca(CAName,Password,KeyFileName,CertFileNAme) when is_list(CAName), is_list(Password), is_list(KeyFileName), is_list(CertFileNAme) ->
	case utils:pem_key_is_encrypted(KeyFileName) of
		true ->
			TmpKeyFileName = KeyFileName ++ "-tmp",
			case utils:remove_pem_key_password(Password,KeyFileName,TmpKeyFileName) of
				true ->
					Res = inventory:import_ca(CAName,#{ password => "", keyfilename => TmpKeyFileName, certfilename => CertFileNAme}),
					_=file:delete(TmpKeyFileName),
					Res;
				false->
					{ error , ?ERROR_CA_CANNOT_IMPORT_KEY }
			end;
		false ->
			inventory:import_ca(CAName,#{ password => Password, keyfilename => KeyFileName, certfilename => CertFileNAme})
	end.

-spec remove_ca(CAName::string())->generic_result().
remove_ca(CAName) when is_list(CAName) ->
	inventory:delete_ca(CAName).

-spec show_ca(CAName::string())-> { ok, Attributes::attribute_list() } | generic_error().
show_ca(CAName) when is_list(CAName) ->
	case inventory:get_ca(CAName) of
		{ok,CAInfo} ->
			{ok,ca_info:to_json(CAInfo)};
		Error ->
			{error,Error}
	end.

-spec list_cas() -> { ok , [string()]} | generic_error().
list_cas()->
	inventory:get_cas().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec show_client(SimName::string(),Client::string())-> {ok,Client::client_info()}.
show_client(SimName,Client)->
	inventory:get_client(SimName,Client).

list_clients(CAName)->
	{ok,Clients} = inventory:list_clients(CAName),
	{ok,[binary_to_list(X) || X <- Clients] }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec show_nodes() -> {ok,[node()]}.
show_nodes()->
	manager:connected_nodes().

-spec set_node_configuration( Node::node() , Configuration::term() ) -> ok.
set_node_configuration( Node, Configuration ) ->
	simnode:set_configuration(Node,Configuration).

-spec get_node_configuration( Node::node() ) -> { ok, Configuration::term() }.
get_node_configuration(Node) ->
	simnode:get_configuration(Node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec create_server(SimName::string(),Type::service_role()) -> generic_result().
create_server(SimName,"mqtt_server") when is_list(SimName)->
	ok;
create_server(SimName,"ovsdb_server") when is_list(SimName)->
	ok;
create_server(SimName,all) when is_list(SimName)->
	ok;
create_server(SimName,Name) when is_list(SimName)->
	io:format("create_server: invalid server type ~p. Must be mqtt_server, ovsdb_server, all.~n",[Name]),
	{ error , unknown_server_type }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  AccessPoint management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec ap_create_clients(SimName::string(),Number::integer()) -> generic_result().
ap_create_clients(SimName,Number) ->
	_ = case whereis(ovsdb_client_handler) of
		undefined ->
			ovsdb_client_handler:start_link();
		_ ->
			true
	end,
	ovsdb_client_handler:set_configuration(#{internal=>SimName,clients=>Number}).

-spec ap_start_clients(ClientIDs::all|[binary()]) -> generic_result().
ap_start_clients(ClientsToStart) ->
	ovsdb_client_handler:start(ClientsToStart).

-spec ap_stop_clients() -> generic_result().
ap_stop_clients() ->
	ovsdb_client_handler:stop(all).

-spec ap_client_stats(NumberOfRecords::non_neg_integer()) -> generic_result().
ap_client_stats(N) ->
	ovsdb_client_stats:show_statistics(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hardware Simulations Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_hardware_definitions()->
	{ok,Definitions} = hardware:get_definitions(),
	Definitions.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Server Management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actions()->
	simengine:list_actions().

action(ActionID)->
	simengine:get_action(ActionID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Misc management functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec refresh_ouis()-> ok.
refresh_ouis()->
	oui_server:refresh().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Local utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input(Prompt,Default)->
	InputData=string:trim(io:get_line( Prompt ++ " [" ++ Default ++ "] :")),
	case InputData=="" of
		true -> Default;
		false -> InputData
	end.

-spec select_servers() -> { binary(), integer()}.
select_servers() ->
	get_server(ovsdb_server).

-spec get_server( ovsdb_server )-> { binary(), integer()}.
get_server(ovsdb_server)->
	io:format("Please enter the OpenSync Server configuration:~n"),
	ServerName = input("  IP Address or hostname: ",""),
	Port  =      input("  Port:","6643"),
	{ list_to_binary(ServerName),list_to_integer(Port)}.

s1(X)->
	_ = import_ca("sim1","mypassword","tip2-cakey.pem","tip2-cacert.pem"),
	c1(X).

p1()->
	prepare_simulation("sim1").

c1(X)->
	Simulation = #simulation{ name = <<"sim1">>,
	                          ca = <<"sim1">>,
	                          num_devices = X,
	                          opensync_server_port = 6643,
	                          opensync_server_name = <<"debfarm1-node-a.arilia.com">>,
	                          nodes = ['simnode1@renegademac.arilia.com']  },
%%	                          nodes = ['simnode1@debfarm1-node-c.arilia.com'] },
		simengine:create(Simulation).

t1_key_h() ->
	_ = import_ca("sim1","mypassword","tip2-cakey.pem","tip2-cacert.pem"),
	Simulation = #simulation{ name = <<"sim1">>,
	                          ca = <<"sim1">>,
	                          num_devices = 50,
	                          opensync_server_port = 6643,
	                          opensync_server_name = <<"10.20.0.118">>,
	                          nodes = ['simnode1@hypatia.syramo.com'] },
	simengine:create(Simulation).

t1_key_hz() ->
	import_ca("sim1","mypassword","tip2-cakey.pem","tip2-cacert.pem").

r1(X)->
	w(X),
	_ = push_simulation("sim1"),
	timer:sleep(1000),
	start_simulation("sim1").

w(X)->
	case length(nodes()) of
		X -> ok;
		_ -> timer:sleep(1000), w(X)
	end.

number_of_clients()->
	{ ok , AllClients } = list_clients("sim1"),
	lists:foldl(fun(Client,A)->
								case inventory:get_client("sim1",Client) of
									{ok,ClientInfo} ->
										A + length(ClientInfo#client_info.wifi_clients);
									_ ->
										A
								end
							end,0,AllClients).

get_all_sim_clients_macs()->
	{ ok , AllClients } = list_clients("sim1"),
	lists:foldl(fun(Client,A)->
								case inventory:get_client("sim1",Client) of
									{ok,ClientInfo} ->
										A ++ [ MAC || {_Index,_Band,_SSID,MAC,_Vendor} <- ClientInfo#client_info.wifi_clients] ;
									_ ->
										A
								end
	            end,[],AllClients).

%% Get all serial numbers in the simulation data
get_all_sim_clients_serials()->
	{ ok , AllClients } = list_clients("sim1"),
	lists:sort(lists:foldl(fun(Client,A)->
								case inventory:get_client("sim1",Client) of
									{ok,ClientInfo} ->
										[ ClientInfo#client_info.serial | A] ;
									_ ->
										A
								end
	            end,[],AllClients)).

compare_clients()->
	tip_api:login("sim1"),
	Clients = tip_api:clients(),
	TipClientMacs = lists:sort(lists:foldl( fun(C,A) ->
														MacEntry = maps:get(<<"macAddress">>,C),
														Mac = maps:get(<<"addressAsString">>,MacEntry),
														[Mac|A]
													 end,[],Clients)),
	SimClientMacs = lists:sort(get_all_sim_clients_macs()),
	{ SimClientMacs, TipClientMacs, lists:subtract(SimClientMacs,TipClientMacs) }.

compare_equipments()->
	tip_api:login("sim1"),
	SimSerials = get_all_sim_clients_serials(),
	Equipments = tip_api:equipments(),
	TipSerials = lists:sort(lists:foldl( fun(E,A) ->
															[ maps:get(<<"serial">>,E) | A ]
														end,[],Equipments)),
	{ SimSerials, TipSerials, lists:subtract(SimSerials,TipSerials)}.

reconnecting()->
	{ok,SimInfo} = simengine:get("sim1"),
	[Node] = SimInfo#simulation.nodes,
	Statuses = rpc:call(Node,ovsdb_client_handler,dump_status,[]),
	maps:fold(fun(_K,V,A) ->
							case V of
								reconnecting -> A+1;
								_ -> A
							end
						end,0,Statuses).

calculate_client_delta()->
	{_S,_T,D} = compare_clients(),
	{ok,SerialNumbers} = list_clients("sim1"),
	%% for each MAC in D, find it's client
	lists:foldl(fun(Element,Acc)->
								SN = lists:foldl(  fun(Serial,Res) ->
																{ok,Client} = show_client("sim1",Serial),
																case lists:keyfind(Element,4,Client#client_info.wifi_clients) of
																	false -> Res;
																	Row ->
																		Row
																end
															end,{},SerialNumbers),
								[{Element,SN}|Acc]
							end,[],D).


xx() ->
	L = [<<"6c:41:6a:2f:c8:51">>,<<"50:48:eb:05:5f:b9">>,
	     <<"34:e7:0b:b7:77:f1">>,<<"34:96:72:40:bc:90">>,
	     <<"1c:f0:3e:8a:66:b9">>,<<"1c:3a:60:c6:75:2f">>,
	     <<"18:b3:ba:cb:4b:f5">>,<<"18:7e:b9:1b:05:20">>,
	     <<"10:c0:7c:e3:3f:98">>,<<"04:d6:aa:ab:bd:e7">>,
	     <<"04:65:65:38:3a:60">>,<<"00:c0:9e:77:0e:e1">>,
	     <<"00:90:0d:a0:44:af">>,<<"00:30:fe:cb:69:e5">>,
	     <<"00:24:36:9a:fd:a7">>,<<"00:1d:17:5d:25:d2">>,
	     <<"00:16:50:ec:69:b3">>,<<"00:14:cc:7e:bf:93">>,
	     <<"00:0d:fb:df:03:7e">>,<<"00:0d:ba:5a:3e:80">>,
	     <<"00:0b:64:5f:e3:f8">>,<<"00:09:ef:93:ac:56">>,
	     <<"00:08:b2:99:a6:d9">>,<<"00:01:f8:93:d1:51">>],
	BL = [mac_to_int(X)||X<-L],
	[unicode:characters_to_list(binary_to_list(X))||X<-BL].

mac_to_int(M)->
	Tokens = string:tokens(binary_to_list(M),":"),
	Ints = [ list_to_integer(X,16) || X<-Tokens],
	binary:list_to_bin(Ints).


