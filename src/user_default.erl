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

-include("../include/common.hrl").
-include("../include/inventory.hrl").
-include("../include/simengine.hrl").

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

-spec connected() -> { ok , none } | { ok , Manager::node() }.
connected()->
	simnode:connected().

-spec connect(NodeName::string()) -> { ok , none | node() }.
connect(NodeName) ->
	Node = list_to_atom(NodeName),
	simnode:connect(Node).

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
	io:format("Creation simulation: ~s~n",[SimName]),
	io:format("  -nodes(~p): ~p~n",[length(Nodes),Nodes]),
	MaxDevices = length(Nodes) * 10000,
	NumberOfDevices = input("Number of devices (max:" ++ integer_to_list(MaxDevices) ++ ") ", integer_to_list(MaxDevices div 2)),
	{ MQTTServers , OVSDBServers } = select_servers(),
	Simulation = #simulation{ name = list_to_binary(SimName),
	                          ca = list_to_binary(CAName),
	                          num_devices = list_to_integer(NumberOfDevices),
	                          creation_date = calendar:local_time(),
	                          mqtt_servers = MQTTServers,
	                          ovsdb_servers = OVSDBServers,
	                          start_date = undefined,
	                          end_date = undefined,
	                          nodes = Nodes },
	simengine:create(Simulation).

-spec show_simulation(SimName::string())-> {ok,Attributes::attribute_list()} | generic_error().
show_simulation(SimName) when is_list(SimName) ->
	simengine:get(SimName).

-spec prepare_simulation( SimName::string() )->ok.
prepare_simulation(SimName)->
	simengine:prepare(SimName,{?MODULE,preparation_completion,[SimName]}),
	ok.

preparation_completion(SimName,CompletionStatus)->
	io:format("Prepararion completion status for simulation ~s. Completion status: ~p~n",[SimName,CompletionStatus]).

-spec list_simulations() -> {ok,SimulationList::[string()]} | generic_error().
list_simulations() ->
	simengine:list().

-spec analyze_nodes()-> ok.
analyze_nodes()->
	{ok,Nodes}=show_nodes(),
	utils:print_nodes_info([node()|Nodes]).

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
			io:format("Key is encrypted..trying to remove encryption...~n"),
			TmpKeyFileName = "tmp-"++KeyFileName,
			case utils:remove_pem_key_password(Password,KeyFileName,TmpKeyFileName) of
				true ->
					io:format("Key was decrypted and can be imported...~n"),
					inventory:import_ca(CAName,#{ password => "", keyfilename => TmpKeyFileName, certfilename => CertFileNAme}),
					file:delete(TmpKeyFileName);
				false->
					io:format("Key was not decrypted and will not be imported. Please supply the right password.~n")
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
	inventory:list_clients(CAName).

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
%%  Node Management functions
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

-spec select_servers() -> { MQTTServer:: auto | #sim_entry{} , OVSDBServer:: auto | #sim_entry{} }.
select_servers() ->
	case input("Do you want to automatically create servers? (yes/no)", "yes") of
		"yes" ->
			{ auto, auto } ;
		_ ->
			MQTTServer = case input("Do you want to automatically create MQTT server? (yes/no)", "yes") of
										 "yes" ->
											 auto;
										 _ ->
											 get_server(mqtt_server)
			             end,
			OVSDBServer = case input("Do you want to automatically create OVSDB server? (yes/no)", "yes") of
				              "yes" ->
					              auto;
				              _ ->
					              get_server(ovsdb_server)
			              end,
			{ MQTTServer, OVSDBServer }
	end.

-spec get_server( mqtt_server | ovsdb_server )-> auto | #sim_entry{}.
get_server(mqtt_server)->
	try
		io:format("Please enter the MQTT Server configuration:~n"),
		Name =      input("  Name: ",""),
		{ok,IPAddress} = inet:parse_address(input("  IP Address: ","")),
		Port =      input("  TCP Port","1883"),
		#sim_entry{ name = list_to_binary(Name),
		            type = mqtt_server,
								ip = IPAddress,
								port = list_to_integer(Port),
								port_reflector = 0 }
	catch
		_:_ ->
			io:format("Invalid information entered. Please try again.~n"),
			auto
	end;
get_server(ovsdb_server)->
	try
		io:format("Please enter the OVSDB Server configuration:~n"),
		Name =      input("  Name: ",""),
		{ok,IPAddress} = inet:parse_address(input("  IP Address: ","")),
		Port1 =      input("  TCP Port (reflector)","6643"),
		Port2 =      input("  TCP Port (service)","6640"),
		#sim_entry{ name = list_to_binary(Name),
		            type = ovsdb_server,
		            ip   = IPAddress,
		            port_reflector = list_to_integer(Port1),
		            port = list_to_integer(Port2) }
	catch
		_:_ ->
			io:format("Invalid information entered. Please try again.~n"),
			auto
	end.

t1_key()->
	import_ca("sim1","mypassword","tip2-cakey.pem","tip2-cacert.pem").

t2_key()->
	import_ca("sim1","","sim1_key.pem","sim1_cert.pem").


