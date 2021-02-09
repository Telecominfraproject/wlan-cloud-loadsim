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
	case net_adm:ping(Node) of
		pong ->
			{ok,Role} = node_stats:node_type(),
			_ = global:sync(),
			timer:sleep(2000),
			manager:connect(Role);
		_ ->
			io:format("Cannot connect to node ~p. Please verify your network cookie and the FQDN is pingable.~n",[NodeName])
	end.

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

remove_simulation(SimName,CAName)->
	simengine:delete(SimName,CAName).

-spec show_simulation(SimName::string())-> { ok,Simulation::simulation() } | generic_error().
show_simulation(SimName) when is_list(SimName) ->
	simengine:get(SimName).

-spec prepare_simulation( SimName::string()|binary() )-> {ok,ActionId::binary()} | generic_error().
prepare_simulation(SimName)->
	simengine:prepare(SimName,#{},utils:noop_mfa()).

-spec push_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
push_simulation(SimName)->
	simengine:push(SimName,#{},utils:noop_mfa()).

-spec start_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
start_simulation(SimName)->
	simengine:start(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec restart_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
restart_simulation(SimName)->
	simengine:restart(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec stop_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
stop_simulation(SimName)->
	simengine:stop(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec pause_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
pause_simulation(SimName)->
	simengine:pause(SimName,#{stagger=>{5,2000}},utils:noop_mfa()).

-spec cancel_simulation(SimName::string()|binary())-> {ok,ActionId::binary()} | generic_error().
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
	inventory:import_raw_ca(CAName,Password,KeyFileName,CertFileNAme).

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
show_client(_SimName,Client)->
	inventory:get_record(#client_info{name = utils:safe_binary(Client)}).

list_clients(SimName)->
	{ok,Clients} = inventory:list_sim_clients(SimName),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

w(X)->
	case length(nodes()) of
		X -> ok;
		_ -> timer:sleep(1000), w(X)
	end.

number_of_clients()->
	{ ok , AllClients } = list_clients("sim1"),
	lists:foldl(fun(Client,A)->
								case inventory:get_record(#client_info{name = utils:safe_binary(Client)}) of
									{ok,ClientInfo} ->
										A + length(ClientInfo#client_info.wifi_clients);
									_ ->
										A
								end
							end,0,AllClients).

get_all_sim_clients_macs()->
	{ ok , AllClients } = list_clients("sim1"),
	lists:foldl(fun(Client,A)->
								case inventory:get_record(#client_info{name = utils:safe_binary(Client)}) of
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
													case inventory:get_record(#client_info{name = utils:safe_binary(Client)}) of
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

filter_nodes()->
	filter_nodes(nodes(),[]).

filter_nodes([],Result)->
	Result;
filter_nodes([H|T],Result)->
	try
		case rpc:call(H,node_stats,node_type,[]) of
			{ok,node} ->
				filter_nodes(T,[H|Result]);
			_ ->
				filter_nodes(T,Result)
		end
	catch
		_:_ ->
			filter_nodes(T,Result)
	end.

wait_for_nodes()->
	wait_for_nodes(20).

wait_for_nodes(0)->
	throw("Cannot create a simulation without at least 1 other node. Please check your configurations and the documentation");
wait_for_nodes(Seconds)->
	case filter_nodes()  of
		[] ->
			io:format("."),
			timer:sleep(1000),
			wait_for_nodes(Seconds-1);
		NodeList ->
			NodeList
	end.

wait_job_id(Id)->
	wait_job_id(Id,600).

wait_job_id(_,0)->
	throw("operation did not complete...~n");
wait_job_id(Id,Timer)->
	{ok,Jobs}=simengine:list_actions(),
	Done = lists:foldl( fun(E,A)->
												case ((Id == E#sim_action.id) andalso (E#sim_action.end_os_time > E#sim_action.start_os_time)) of
													true ->
														true;
													false ->
														% io:format("End:~p Start:~p~n",[E#sim_action.end_os_time,E#sim_action.start_os_time]),
														A
												end
											end,false,Jobs),
	case Done of
		true -> ok;
		false ->
			io:format("."),
			timer:sleep(1000),
			wait_job_id(Id,Timer-1)
	end.

clear() ->
	io:format("\033[2J").

get_currrent_simulation()->
	persistent_term:get(current_simulation).

auto()->
	auto(10).

auto(NumberOfDevices)->
	SimName = "sim1",
	CAName = "tip1",
	clear(),
	io:format("Waiting for nodes to come on-line...~n"),
	Nodes = wait_for_nodes(),
	persistent_term:put(current_simulation,SimName),
	BinSim = list_to_binary(SimName),
	BinCAName = list_to_binary(CAName),
	_ = remove_ca(CAName),
	_ = remove_simulation(CAName,SimName),
	io:format("~nImporting TIP certificate...~n"),
	_ = import_ca("tip1","mypassword","tip-cakey.pem","tip-cacert.pem"),
	io:format("Simulation will be built for nodes: ~p.~n",[Nodes]),
	Simulation = #simulation{ name = BinSim,
	                          ca = BinCAName,
	                          num_devices = NumberOfDevices,
	                          opensync_server_port = 6643,
	                          opensync_server_name = <<"opensync-controller.wlan.local">>,
	                          nodes = Nodes  },
	_ = simengine:create(Simulation),
	io:format("Creating assets for simulation..."),
	{ok,ID} = simengine:prepare(SimName,#{},utils:noop_mfa()),
	wait_job_id(ID),
	io:format("~nAll assets created and ready to run the simulation. Type \"run().\" to start.~n~n").

-spec run() -> any().
run()->
	run("sim1",20).

-spec run(SimName::binary()|string(),DelayInSeconds::non_neg_integer()) -> any().
run(_SimName,0)->
	throw("Nodes are not on-line, simulation cannot proceed.");
run(SimName,Delay)->
	io:format("Making sure all nodes are on-line first..."),
	case inventory:get_record(#simulation{name = utils:safe_binary(SimName)}) of
		{ok,SimInfo} ->
			Nodes = wait_for_nodes(),
			case lists:subtract(SimInfo#simulation.nodes,Nodes) of
				[] ->
					io:format("~n.~nAll nodes on-line.~nPushing simulation assets to all nodes..."),
					case push_simulation(SimName) of
						{ok,ID1} ->
							wait_job_id(ID1),
							io:format(".Done.~n"),
							io:format("Starting simulation assets on all nodes..."),
							case start_simulation(SimName) of
								{ok,ID2} ->
									wait_job_id(ID2),
									io:format(".Done.~n");
								_ ->
									io:format("~nSimulation could not be started. Please try again.~n")
							end;
						_ ->
							io:format("~nSimulation could not be pushed to remote nodes. Please try again.~n")
					end;
				_ ->
					io:format("."),
					timer:sleep(1000),
					run(SimName,Delay-1)
			end,
			io:format("~nSimulation ~s is now running.~n",[SimName]);
		Error ->
			io:format("~nAne error occured while trying to start the simulation. ~p~n",[Error])
	end.

run_script(ScriptName) ->
	case filelib:is_file(ScriptName) of
		true ->
			try
				[ Script ] = yamerl_constr:file(ScriptName),
				%% verify that we have everything
				{"simulation", SimInfo} = proplists:lookup("simulation",Script),
				{"name", SimName} = proplists:lookup("name",SimInfo),
				{"server", SimServer} = proplists:lookup("server",SimInfo),
				{"port", SimPort} = proplists:lookup("port", SimInfo),
				{"devices", NumDevices} = proplists:lookup("devices",SimInfo),
				{"ca",CAInfo} = proplists:lookup("ca",SimInfo),
				{"name",CAName} = proplists:lookup("name",CAInfo),
				{"cert",CACert} = proplists:lookup("cert",CAInfo),
				{"key",CAKey} = proplists:lookup("key",CAInfo),
				{"password",CAPassword} = proplists:lookup("password",CAInfo),
				io:format("SIM: ~p: ~p/~p/~p CA: ~p: ~p/~p/~p",[SimName,SimServer,SimPort,NumDevices,CAName,CACert,CAKey,CAPassword]),
				case filelib:is_file(CACert) andalso filelib:is_file(CAKey) of
					true ->
						try
							_ = inventory:import_raw_ca(CAName,CAPassword,CAKey,CACert),
							Nodes = wait_for_nodes(),
							Simulation = #simulation{ name = utils:safe_binary(SimName),
							                          ca = utils:safe_binary(CAName),
							                          num_devices = NumDevices,
							                          opensync_server_port = 6643,
							                          opensync_server_name = utils:safe_binary(SimServer),
							                          nodes = Nodes  },
							_ = simengine:create(Simulation),
							{ok,ID} = simengine:prepare(SimName,#{},utils:noop_mfa()),
							wait_job_id(ID),
							run(SimName,120),
							?L_IA("Simulation ~p is now running.",[SimName])
						catch
							_:_ ->
								?L_IA("Simultion ~p cannot be started. Please check your certificate/key/password.",[ScriptName])
						end;
					false ->
						?L_IA("Simulation script: ~p, KeyFile: ~p or Certfile: ~p is not present.",[ScriptName,CAKey,CACert])
				end
			catch
				_:_ ->
					?L_IA("Cannot run script: ~p. Please look at the template simulation.yaml",[ScriptName])
			end;
		false ->
			?L_IA("Scrip ~p does not exist.",[ScriptName])
	end.
