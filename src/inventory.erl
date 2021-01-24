%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2020 8:51 a.m.
%%%-------------------------------------------------------------------
-module(inventory).
-author("stephb").

-compile({parse_transform, lager_transform}).

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").
-include("../include/inventory.hrl").
-include("../include/errors.hrl").
-include("../include/simengine.hrl").

%% API
-export([start_link/0,creation_info/0,create_tables/0,
	make_ca/2,get_ca/1,get_cas/0,delete_ca/1,import_ca/2,
	make_server/4,get_server/3,make_servers/4,delete_server/3,
	make_client/3,make_clients/6,generate_client_batch/7,get_client/3,
	all_files_exist/1,valid_ca_name/1,valid_password/1,
	list_clients/1,generate_single_client/5,list_sim_clients/1,
	delete_all_records/1,import_raw_ca/4,ca_in_use/1,
	list_records_names/1,exists/1,add_record/1,del_record/1,get_record/1,list_records/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(inventory_state, {
						status = undefined,
						inventory_db_dir, %% where to store anything about inventory
						inventory_file_name,
						cert_db_dir,       %% where to put all certs
						batch_generation_pid = undefined
		}).

%%%===================================================================
%%% API
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

-spec make_ca( CAName::string(), Password::string() ) -> ok | { error,Reason::term() }.
make_ca(CAName,Password)->
	case (valid_ca_name(CAName) and valid_password(Password) )of
		true->
			gen_server:call(?SERVER,{make_ca,utils:safe_binary(CAName),utils:safe_binary(Password),self()});
		false->
			{error,invalid_ca_name_or_password}
	end.

-spec import_ca(Name::string(),Attributes::attribute_list()) -> ok | generic_error().
import_ca(CAName,Attributes)->
	gen_server:call(?SERVER,{import_ca,utils:safe_binary(CAName),Attributes,self()}).

-spec get_ca( Name::string()|binary() )-> {ok,ca_info()} | generic_error().
get_ca(Name)->
	gen_server:call(?SERVER,{get_ca,utils:safe_binary(Name),self()}).

-spec delete_ca( Name::string()|binary() ) -> ok | { error, in_use, [string()]} | generic_error().
delete_ca(Name)->
	gen_server:call(?SERVER,{delete_ca,utils:safe_binary(Name),self()}).

-spec get_cas() -> { ok , [ CAName::string() ]} | generic_error().
get_cas()->
	gen_server:call(?SERVER,{get_cas,self()}).

-spec make_server(CAName::string()|binary(),SimName::string()|binary(),Name::string()|binary(),Type::service_role())-> { ok , SI::server_info() } | generic_error().
make_server(CAName,SimName,Name, mqtt_server )->
	gen_server:call(?SERVER,{make_server,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:safe_binary(Name),mqtt_server,self()});
make_server(CAName,SimName,Name, ovsdb_server )->
	gen_server:call(?SERVER,{make_server,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:safe_binary(Name),ovsdb_server,self()}).

-spec make_servers(CAName::string()|binary(),SimName::string()|binary(),Servers::list(ServerName::string()),Type::service_role())-> { ok, list({ ServerName::string(),SI::server_info()})} | generic_error().
make_servers(CAName,SimName,ServerList,mqtt_server)->
	gen_server:call(?SERVER,{make_servers,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:to_binary_list(ServerList,[]),mqtt_server,self()});
make_servers(CAName,SimName,ServerList,ovsdb_server)->
	gen_server:call(?SERVER,{make_servers,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:to_binary_list(ServerList,[]),ovsdb_server,self()}).

-spec get_server(CAName::string()|binary(),SimName::binary()|string(),Id::string()|binary())-> { ok, SI::server_info()} | generic_error().
get_server(CAName,SimName,Id)->
	gen_server:call(?SERVER,{get_server,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:safe_binary(Id),self()}).

-spec delete_server(CAName::string()|binary(),SimName::string|binary(),Id::string()|binary())-> { ok, SI::server_info()} | generic_error().
delete_server(CAName,SimName,Id)->
	gen_server:call(?SERVER,{delete_server,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:safe_binary(Id),self()}).

-spec make_client(CAName::string()|binary(),SimName::string()|binary(),Attributes::#{ atom() => term() })-> {ok,Client::client_info()} | generic_error().
make_client(CAName,SimName,Attributes)->
	case validate_attributes(Attributes) of
		true ->
			gen_server:call(?SERVER,{make_client,utils:safe_binary(CAName),utils:safe_binary(SimName),Attributes});
		false ->
			{error,missing_attributes}
	end.

-spec make_clients(CAName::string()|binary(),SimName::string()|binary(),Start::integer(),HowMany::integer(),Attributes::#{ atom() => term() }, Notification::notification_cb() ) -> {ok,HowManyDone::integer()} | generic_error().
make_clients(CAName,SimName,Start,HowMany,Attributes,Notification) ->
	case validate_attributes(Attributes) of
		true -> gen_server:call(?SERVER,{make_many_clients,utils:safe_binary(CAName),utils:safe_binary(SimName),Start,HowMany,Attributes,Notification});
		false -> { error, missing_attributes }
	end.

validate_attributes(Attrs) when is_map(Attrs)->
	maps:is_key(serial,Attrs) and maps:is_key(name,Attrs) and maps:is_key(mac,Attrs) and maps:is_key(id,Attrs).

-spec get_client(CAName::string()|binary(),SimName::binary()|string(),Id::string()|binary())-> { ok , Client::client_info() } | generic_error().
get_client(CAName,SimName,Id)->
	gen_server:call(?SERVER,{get_client,utils:safe_binary(CAName),utils:safe_binary(SimName),utils:safe_binary(Id)}).

-spec list_clients(SimName::string()|binary())-> { ok , [Client::binary()] } | generic_error().
list_clients(SimName)->
	gen_server:call(?SERVER,{list_clients,utils:safe_binary(SimName)}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link(?START_SERVER, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #inventory_state{}} | {ok, State :: #inventory_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	InventoryDbDir = utils:app_env(inventory_db_dir,""),
	ok = utils:make_dir(InventoryDbDir),
	CertsDbDir = utils:app_env(cert_db_dir,""),
	ok = utils:make_dir(CertsDbDir),

	{ok, #inventory_state{
		status = started,
		inventory_db_dir = InventoryDbDir,
		cert_db_dir = CertsDbDir
		}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #inventory_state{}) ->
	{reply, Reply :: term(), NewState :: #inventory_state{}} |
	{reply, Reply :: term(), NewState :: #inventory_state{}, timeout() | hibernate} |
	{noreply, NewState :: #inventory_state{}} |
	{noreply, NewState :: #inventory_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #inventory_state{}} |
	{stop, Reason :: term(), NewState :: #inventory_state{}}).
handle_call({make_ca,CAName,Password,Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,_CAInfo} ->
			{reply,{error,ca_already_exists},State};
		_ ->
			{ok,NewState}=create_ca(CAName,Password,State,Pid),
			{reply, ok, NewState}
	end;

handle_call({import_ca,CAName,Attributes,Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,_CAInfo} ->
			{reply,{error,ca_already_exists},State};
		_ ->
			{ok,NewState}=import_a_ca(CAName,Attributes,State,Pid),
			{reply, ok, NewState}
	end;

handle_call({delete_ca,CAName,Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			case ca_in_use(CAInfo#ca_info.name) of
				[] ->
					{ok,NewState}=delete_ca(CAInfo,State,Pid),
					{reply, ok, NewState};
				SimulationList ->
					{reply, {error,in_use,SimulationList},State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({get_ca,CAName,_Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			{reply, {ok, CAInfo}, State};
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({get_cas,_Pid}, _From, State = #inventory_state{}) ->
	{ reply,list_records_names(#ca_info{}), State};

handle_call({make_server,CAName,SimName,Id,Type,Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			{ok, NewState}=create_server(CAInfo,SimName,Id,Type,State,Pid),
			{reply, ok, NewState};
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({make_servers,CAName,SimName,ServerList,Type,Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			{ok , NewState}=create_servers(CAInfo,SimName,ServerList,Type,State,Pid),
			{reply, ok, NewState};
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({get_server,CAName,_SimName,Id,_Pid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			Server = #server_info{ name = Id },
			case get_record(Server) of
				{ok,Record} ->
					case Record#server_info.ca == CAInfo#ca_info.name of
						true ->
							{reply,{ok,Record},State};
						false ->
							{reply,{error,unknown_client},State}
					end;
				Error ->
					{reply,{error,Error},State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({delete_server,CAName,_SimName,Id,_ParentPid}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			Server = #server_info{ name = Id },
			case get_record(Server) of
				{ok,Record} ->
					case Record#server_info.ca == CAInfo#ca_info.name of
						true ->
							_ = del_record(Record),
							{ reply, ok,State};
						false ->
							{ reply , {error, unknown_server},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({make_client,CAName,SimName,Attributes}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			case create_client(CAInfo,SimName,Attributes) of
				ok ->
					{reply, ok, State};
				{error,Reason} ->
					{reply,{error,Reason},State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({make_many_clients,CAName,SimName,Start,HowMany,Attributes,Notification}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			JobPid = spawn(?MODULE,generate_client_batch,[CAInfo,SimName,Start,HowMany,Attributes,Notification,State]),
			{ reply, ok , State#inventory_state{batch_generation_pid = JobPid} };
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({get_client,CAName,Id}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			Client = #client_info{ name = Id },
			case get_record(Client) of
				{ok,Record} ->
					case Record#client_info.ca == CAInfo#ca_info.name of
						true ->
							{ reply, {ok,Record},State};
						false ->
							{ reply , {error, unknown_client},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({list_clients,SimName}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = SimName }) of
		{ok,_CAInfo} ->
			case list_sim_clients(SimName) of
				{atomic,Records} ->
					{ reply, {ok,Records},State};
				Error ->
					{ reply, {error, Error}, State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call({delete_client,CAName,Id}, _From, State = #inventory_state{}) ->
	case get_record(#ca_info{ name = CAName }) of
		{ok,CAInfo} ->
			Client = #client_info{ name = Id },
			case get_record(Client) of
				{ok,Record} ->
					case Record#client_info.ca == CAInfo#ca_info.name of
						true ->
							_ = del_record(Record),
							{ reply, ok,State};
						false ->
							{ reply , {error, unknown_client},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end;
		_ ->
			{reply,{error,unknown_ca},State}
	end;

handle_call(_Request, _From, State = #inventory_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #inventory_state{}) ->
	{noreply, NewState :: #inventory_state{}} |
	{noreply, NewState :: #inventory_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #inventory_state{}}).
handle_cast(_Request, State = #inventory_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #inventory_state{}) ->
	{noreply, NewState :: #inventory_state{}} |
	{noreply, NewState :: #inventory_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #inventory_state{}}).
handle_info({'EXIT',_FromPid,_Reason},State)->
	{noreply,State};
handle_info(_Info, State = #inventory_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #inventory_state{}) -> term()).
terminate(Reason, _State = #inventory_state{}) ->
	%% _ = mnesia:stop(),
	?L_IA("Inventory exiting (~p).",[Reason]).

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #inventory_state{},
		Extra :: term()) ->
	{ok, NewState :: #inventory_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #inventory_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_ca(CAName::binary(),Password::binary(),State::#inventory_state{},Pid::pid()) -> {ok,NewState::#inventory_state{}}.
create_ca(CaName,Password,State,_Pid)->
	%% Make all the directories
	CaDir = filename:join([State#inventory_state.cert_db_dir,binary_to_list(CaName)]),
	ok = utils:make_dir(CaDir),
	ok = file:write_file( filename:join([CaDir, "index.txt"]),<<>>),
	ok = file:write_file( filename:join([CaDir, "serial.txt"]),<<$0,$1>>),
	ok = utils:make_dir(filename:join([CaDir,"certs"])),
	ok = utils:make_dir(filename:join([CaDir,"newcerts"])),
	ok = utils:make_dir(filename:join([CaDir,"crl"])),
	ok = utils:make_dir(filename:join([CaDir,"private"])),
	CaClientsDir = filename:join([CaDir,"clients"]),
	CaServersDir = filename:join([CaDir,"servers"]),
	ok = utils:make_dir(CaClientsDir),
	ok = utils:make_dir(CaServersDir),

	CaTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-ca.cnf"]),
	CaConfigFileName = filename:join([CaDir,"ssl-ca.cnf"]),
	utils:search_replace(CaTemplateFileName,CaConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	ClientTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-client.cnf"]),
	ClientConfigFileName = filename:join([CaDir,"ssl-client.cnf"]),
	utils:search_replace(ClientTemplateFileName,ClientConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	ServerTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-server.cnf"]),
	ServerConfigFileName = filename:join([CaDir,"ssl-server.cnf"]),
	utils:search_replace(ServerTemplateFileName,ServerConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateCaScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-ca.sh"]),
	CreateCaScriptFileName = filename:join([CaDir,"ssl-create-ca.sh"]),
	utils:search_replace(CreateCaScriptTemplate,CreateCaScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateClientScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-client.sh"]),
	CreateClientScriptFileName = filename:join([CaDir,"ssl-create-client.sh"]),
	utils:search_replace(CreateClientScriptTemplate,CreateClientScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateServerScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-server.sh"]),
	CreateServerScriptFileName = filename:join([CaDir,"ssl-create-server.sh"]),
	utils:search_replace(CreateServerScriptTemplate,CreateServerScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	_ = file:change_mode(CreateCaScriptFileName,8#0777),
	_ = file:change_mode(CreateClientScriptFileName,8#0777),
	_ = file:change_mode(CreateServerScriptFileName,8#0777),

	_Result = os:cmd(CreateCaScriptFileName),
	%% io:format("RESULT: ~p~n",[Result]),

	CaKeyCertFileName = filename:join([CaDir,"cacert.pem"]),
	CaKeyFileName = filename:join([CaDir,"cakey.pem"]),
	{ok,CertData}=utils:pem_to_cert(CaKeyCertFileName),
	{ok,KeyData}=utils:pem_to_key(CaKeyFileName),

	_ = file:change_mode(CaKeyFileName,8#0400),
	_ = file:change_mode(CaKeyCertFileName,8#0400),

	NewCa = #ca_info{ name = CaName,
										dir_name = list_to_binary(CaDir),
										clients_dir_name = list_to_binary(CaClientsDir),
										servers_dir_name = list_to_binary(CaServersDir),
										cert_file_name = list_to_binary(CaKeyCertFileName),
										key_file_name =  list_to_binary(CaKeyFileName),
										config_file_name = list_to_binary(CaConfigFileName),
										cert = CertData,
	                  key = KeyData,
										password = Password
			},
	_ = add_record(NewCa),
	{ ok, State#inventory_state{ status = created } }.

-spec import_raw_ca(CAName::string(),Password::string(),KeyFileName::string(),CertFileName::string())->ok | generic_error().
import_raw_ca(CAName,Password,KeyFileName,CertFileNAme) when is_list(CAName), is_list(Password), is_list(KeyFileName), is_list(CertFileNAme) ->
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

-spec import_a_ca(CAName::binary(),Attributes::attribute_list(),State::#inventory_state{},Pid::pid()) -> {ok,NewState::#inventory_state{}}.
import_a_ca(CaName,Attributes,State,_Pid)->
	%% Make all the directories
	#{ password := OPassword , keyfilename := OKeyFileNAme , certfilename := OCertFileName } = Attributes,
	CaDir = filename:join([State#inventory_state.cert_db_dir,binary_to_list(CaName)]),
	CaDir = filename:join([State#inventory_state.cert_db_dir,binary_to_list(CaName)]),
	ok = utils:make_dir(CaDir),
	ok = file:write_file( filename:join([CaDir, "index.txt"]),<<>>),
	ok = file:write_file( filename:join([CaDir, "serial.txt"]),<<$0,$1>>),
	ok = utils:make_dir(filename:join([CaDir,"certs"])),
	ok = utils:make_dir(filename:join([CaDir,"newcerts"])),
	ok = utils:make_dir(filename:join([CaDir,"crl"])),
	ok = utils:make_dir(filename:join([CaDir,"private"])),
	CaClientsDir = filename:join([CaDir,"clients"]),
	CaServersDir = filename:join([CaDir,"servers"]),
	ok = utils:make_dir(CaClientsDir),
	ok = utils:make_dir(CaServersDir),

	CaTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-ca.cnf"]),
	CaConfigFileName = filename:join([CaDir,"ssl-ca.cnf"]),
	utils:search_replace(CaTemplateFileName,CaConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	ClientTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-client.cnf"]),
	ClientConfigFileName = filename:join([CaDir,"ssl-client.cnf"]),
	utils:search_replace(ClientTemplateFileName,ClientConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	ServerTemplateFileName = filename:join([utils:priv_dir(),"templates","ssl-server.cnf"]),
	ServerConfigFileName = filename:join([CaDir,"ssl-server.cnf"]),
	utils:search_replace(ServerTemplateFileName,ServerConfigFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateCaScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-ca.sh"]),
	CreateCaScriptFileName = filename:join([CaDir,"ssl-create-ca.sh"]),
	utils:search_replace(CreateCaScriptTemplate,CreateCaScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateClientScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-client.sh"]),
	CreateClientScriptFileName = filename:join([CaDir,"ssl-create-client.sh"]),
	utils:search_replace(CreateClientScriptTemplate,CreateClientScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	CreateServerScriptTemplate =	filename:join([utils:priv_dir(),"templates","ssl-create-server.sh"]),
	CreateServerScriptFileName = filename:join([CaDir,"ssl-create-server.sh"]),
	utils:search_replace(CreateServerScriptTemplate,CreateServerScriptFileName,[{"$$ROOT_DIR$$",CaDir}]),

	_ = file:change_mode(CreateCaScriptFileName,8#0777),
	_ = file:change_mode(CreateClientScriptFileName,8#0777),
	_ = file:change_mode(CreateServerScriptFileName,8#0777),

	CaKeyFileName = filename:join([CaDir,"cakey.pem"]),
	CaKeyCertFileName = filename:join([CaDir,"cacert.pem"]),
	CaConfigFileName = filename:join([CaDir,"ssl-ca.cnf"]),

	_R1 = file:copy( OKeyFileNAme , CaKeyFileName ),
	_R2 = file:copy( OCertFileName, CaKeyCertFileName ),

	_ = file:change_mode(CaKeyFileName,8#0400),
	_ = file:change_mode(CaKeyCertFileName,8#0400),

	{ok,CertData}=utils:pem_to_cert(CaKeyCertFileName),
	{ok,KeyData}=utils:pem_to_key(CaKeyFileName),

	NewCa = #ca_info{ name = CaName,
	                  dir_name = list_to_binary(CaDir),
	                  clients_dir_name = list_to_binary(CaClientsDir),
	                  servers_dir_name = list_to_binary(CaServersDir),
	                  cert_file_name = list_to_binary(CaKeyCertFileName),
	                  key_file_name =  list_to_binary(CaKeyFileName),
	                  config_file_name = list_to_binary(CaConfigFileName),
	                  cert = CertData,
	                  key = KeyData,
	                  password = list_to_binary(OPassword)
	},
	_ = add_record(NewCa),
	{ ok, State#inventory_state{ status = created } }.

delete_ca(CAInfo,State,_Pid)->
	_ = del_record(CAInfo),
	{ ok , State }.

%% -subj "/C=US/ST=Utah/L=Lehi/O=Your Company, Inc./OU=IT/CN=yourdomain.com"

%% openssl req -batch -config mqtt-server.cnf -newkey rsa:2048 -sha256 -out mqttservercert.csr -outform PEM
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_server -out mqttservercert.pem -infiles mqttservercert.csr
%% openssl rsa -passin pass:apassword -in mqttserverkey.pem -out mqttserverkey_dec.pem
-spec create_server(CAInfo::ca_info(),SimName::binary(),Name::binary(),Type::service_role(),State::#inventory_state{},ParentPid::pid()) -> { ok , NewState::#inventory_state{} } | generic_error().
create_server(CAInfo,SimName,Name,Type,State,_Pid)->
	BaseDir = binary_to_list(CAInfo#ca_info.servers_dir_name),
	ServerKeyPem = filename:join([BaseDir,binary_to_list(Name) ++ "-key.pem"]),
	ServerKeyDec = filename:join([BaseDir,binary_to_list(Name) ++ "-key_dec.pem"]),
	ServerCertCsr = filename:join([BaseDir,binary_to_list(Name) ++ "-cert.csr"]),
	ServerCertPem = filename:join([BaseDir,binary_to_list(Name) ++ "-cert.pem"]),

	CaBase = binary_to_list(CAInfo#ca_info.dir_name),
	CreateServerScriptFileName = filename:join([CaBase,"ssl-create-server.sh"]),
	_Result = os:cmd(CreateServerScriptFileName),
	%% io:format("RESULT: ~p~n",[Result]),

	_ = file:rename( filename:join([CaBase,"servercert.csr"]),ServerCertCsr ),
	_ = file:rename( filename:join([CaBase,"servercert.pem"]),ServerCertPem ),
	_ = file:rename( filename:join([CaBase,"serverkey.pem"]),ServerKeyPem ),
	_ = file:rename( filename:join([CaBase,"serverkey_dec.pem"]),ServerKeyDec ),

	{ok,KeyPemData} = utils:pem_to_key(ServerKeyPem),
	{ok,ServerCertPemData} = utils:pem_to_cert(ServerCertPem),
	{ok,ServerKeyDecPemData} = utils:pem_to_key(ServerKeyDec),
	{ok,ServerCertCsrPemData} = file:read_file(ServerCertCsr),

	NewServerInfo = #server_info{
		name = Name,
		service = Type,
		ca = CAInfo#ca_info.name,
		sim_name = SimName,
		key = KeyPemData,
		cert = ServerCertPemData,
		decrypt = ServerKeyDecPemData,
		csr = ServerCertCsrPemData,
		cacert = CAInfo#ca_info.cert
	},

	_ = add_record(NewServerInfo),

	{ ok, State }.

create_servers(_CAInfo,_SimName,[],_,State,_Pid)->
	{ok,State};
create_servers(CAInfo,SimName,[H|T],Type,State,Pid)->
	_ = create_server(CAInfo,SimName,H,Type,State,Pid),
	create_servers(CAInfo,SimName,T,Type,State,Pid).

%% openssl req -batch -config openssl-client.cnf -newkey rsa:2048 -sha256 -out clientcert.csr -outform PEM -nodes
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_client -out clientcert.pem -infiles clientcert.csr
%% openssl rsa -passin pass:apassword -in clientkey.pem -out clientkey_dec.pem
-spec create_client( CAInfo :: ca_info(), SimName::binary(),Attributes::#{ atom() => term() }) ->
			ok | { error , Reason :: term()}.
create_client(CAInfo,SimName,Attributes)->
	try
		#{ mac := Mac, serial := Serial, name := _Name , id := HardwareId, bands:= Bands} = Attributes,
		BaseFileName = filename:join( [ binary_to_list(CAInfo#ca_info.clients_dir_name),binary_to_list(Serial)]),
		ClientKeyPem = BaseFileName ++ "-key.pem",
		ClientKeyDec = BaseFileName ++  "-key_dec.pem",
		ClientCertCsr = BaseFileName ++  "-cert.csr",
		ClientCertPem = BaseFileName ++  "-cert.pem",

		CaBase = binary_to_list(CAInfo#ca_info.dir_name),
		CreateClientScriptFileName = filename:join([CaBase,"ssl-create-client.sh"]),
		_Result = os:cmd(CreateClientScriptFileName),
		%% io:format("RESULT: ~p~n",[Result]),

		_ = file:rename( filename:join([CaBase,"clientcert.csr"]),ClientCertCsr ),
		_ = file:rename( filename:join([CaBase,"clientcert.pem"]),ClientCertPem ),
		_ = file:rename( filename:join([CaBase,"clientkey.pem"]),ClientKeyPem ),
		_ = file:rename( filename:join([CaBase,"clientkey_dec.pem"]),ClientKeyDec ),

		{ok,ClientCertCsrData} = file:read_file(ClientCertCsr),
		{ok,ClientKeyDecData} = utils:pem_to_key(ClientKeyDec),
		{ok,ClientKeyPemData} = utils:pem_to_key(ClientKeyPem),
		{ok,ClientCertPemData} = utils:pem_to_cert(ClientCertPem),

		WiFiClients = lists:sort(lists:flatten(gen_wlan_clients(Bands))),
		LanClients = lists:sort(lists:flatten(gen_lan_clients([<<"eth0">>,<<"eth1">>]))),
		%% io:format(">>>~p~n",[WiFiClients]),

		[X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,_X6b] = string:to_lower(binary_to_list(Mac)),
		Client = #client_info{
			name = Serial,
			ca = CAInfo#ca_info.name,
			sim_name = SimName,
			cap = [ mqtt_client , ovsdb_client ],
			wan_mac0 = list_to_binary([X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,$0]),
			lan_mac0 = list_to_binary([X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,$8]),
			id = HardwareId,
			serial = Serial,
			bands = Bands,
			wifi_clients = WiFiClients,
			lan_clients = LanClients,
			key = ClientKeyPemData,
			cert = ClientCertPemData,
			decrypt = ClientKeyDecData,
			cacert = CAInfo#ca_info.cert,
			csr = ClientCertCsrData
		},
		%% io:format(">>>SERIAL: ~p~n",[Serial]),
		_R = add_record(Client),
		%% ("RESULT>>>=~p~n",[R]),
		ok
	catch
		_:_ ->
			{ error , cannot_create_client }
	end.

gen_client(OUI)->
	[A1,A2,A3,A4,A5,A6] = OUI,
	[X1,X2,X3,X4,X5,X6] = lists:flatten(string:pad(integer_to_list(rand:uniform(1 bsl 24),16),6,leading,$0)),
	list_to_binary(string:to_lower([A1,A2,$:,A3,A4,$:,A5,A6,$:,X1,X2,$:,X3,X4,$:,X5,X6])).

gen_lan_clients(Ports) ->
	gen_lan_clients(Ports,1,[]).

gen_lan_clients([],_,Acc)->
	Acc;
gen_lan_clients([Port|T],Index,Acc)->
	Count = rand:uniform(4)+2,
	gen_lan_clients(T,Index+Count,[generate_lan_tuples(Index,Count,Port,[])|Acc]).

generate_lan_tuples(_Index,0,_Port,Acc)->
	Acc;
generate_lan_tuples(Index,Count,Port,Acc)->
	OUI = oui_server:get_an_oui(),
	FakeMAC = gen_client(binary_to_list(OUI)),
	{ ok, FakeVendor } = oui_server:lookup_oui(OUI),
	generate_lan_tuples(Index+1,Count-1,Port,[{Index,Port,FakeMAC,FakeVendor}|Acc]).

gen_wlan_clients(Bands)->
	gen_wlan_clients(Bands,1,[]).
gen_wlan_clients([],_,Acc)->
	Acc;
gen_wlan_clients([Band|T],Index,Acc)->
	% FakeSSID = list_to_binary(animals:get_an_animal()),
	FakeSSID = <<"TipWlan-cloud-wifi">>,
	Count = rand:uniform(6)+2,
	gen_wlan_clients(T,Index+Count,[generate_wlan_tuples(Index,Count,Band,FakeSSID,[])|Acc]).

generate_wlan_tuples(_Index,0,_Band,_FakeSSID,Acc)->
	Acc;
generate_wlan_tuples(Index,Count,Band,FakeSSID,Acc)->
	OUI = oui_server:get_an_oui(),
	FakeMAC = gen_client(binary_to_list(OUI)),
  { ok, FakeVendor } = oui_server:lookup_oui(OUI),
	generate_wlan_tuples(Index+1,Count-1,Band,FakeSSID,[{Index,Band,FakeSSID,FakeMAC,FakeVendor}|Acc]).

generate_client_batch(CAInfo,SimName,Start,HowMany,Attributes,Notification,State)->
	#{ id := HardwareId } = Attributes,
	case hardware:get_by_id(HardwareId) of
		{ok,[HardwareInfo]} ->
			{ok,[OUI|_]} = oui_server:lookup_vendor(HardwareInfo#hardware_info.vendor),
			<<A,B,C,D,E,F>> = OUI,
			Prefix = [A,B,$:,C,D,$:,E,F,$:],
			%% io:format("BATCH: ca=~p prefix=~p start=~p howmany=~p attrs=~p notify=~p~n",[CAInfo,Prefix,Start,HowMany,Attributes,Notification]),
			generate_client_batch(CAInfo,SimName,Prefix,1,Start,HowMany,Attributes,Notification,State);
		Error ->
			Error
	end.

-spec generate_single_client(HardwareId::binary(),CAInfo::ca_info(),SimName::binary(),Index::integer(),Attributes::#{ atom() => term() }) -> ok.
generate_single_client(HardwareId,CAInfo,SimName,Index,Attributes)->
	case hardware:get_by_id(HardwareId) of
		{ok,[HardwareInfo]} ->
			{ok,[OUI|_]} = oui_server:lookup_vendor(HardwareInfo#hardware_info.vendor),
			<<A,B,C,D,E,F>> = OUI,
			Prefix = [A,B,$:,C,D,$:,E,F,$:],
			%% io:format("BATCH: ca=~p prefix=~p start=~p howmany=~p attrs=~p notify=~p~n",[CAInfo,Prefix,Start,HowMany,Attributes,Notification]),
			[X1,X2,X3,X4,X5] = lists:flatten(string:pad(integer_to_list(Index,16),5,leading,$0)),
			#{ serial := Serial, name := Name } = Attributes,
			[A,B,$:,C,D,$:,E,F,$:] = Prefix,
			Mac = Prefix ++ [X1,X2,$:,X3,X4,$:,X5,$0],
			RealSerial = binary_to_list(Serial) ++ [A,B,C,D,E,F] ++ [X1,X2,X3,X4,X5,$0],
			RealName = binary_to_list(Name) ++ "-" ++ [X1,X2,X3,X4,X5,$0],
			_ = create_client(CAInfo,SimName,Attributes#{ id => HardwareId, bands => HardwareInfo#hardware_info.bands, name => list_to_binary(RealName), mac => list_to_binary(Mac) , serial => list_to_binary(RealSerial) });
		{error,_Reason} = Error ->
			Error
	end.

generate_client_batch(_CaInfo,_SimName,_Prefix,_Current,_Start,0,_Attributes,{M,F,A}=_Notification,_State)->
	apply(M,F,A);
generate_client_batch(CaInfo,SimName,Prefix,Current,Start,Left,Attributes,Notification,State)->
	[X1,X2,X3,X4,X5] = lists:flatten(string:pad(integer_to_list(Current,16),5,leading,$0)),
	#{ serial := Serial, name := Name } = Attributes,
	[A,B,$:,C,D,$:,E,F,$:] = Prefix,
	Mac = Prefix ++ [X1,X2,$:,X3,X4,$:,X5,$0],
	RealSerial = binary_to_list(Serial) ++ [A,B,C,D,E,F] ++ [X1,X2,X3,X4,X5,$0],
	RealName = binary_to_list(Name) ++ "-" ++ [X1,X2,X3,X4,X5,$0],
	_ = create_client(CaInfo, SimName, Attributes#{ name => list_to_binary(RealName), mac => list_to_binary(Mac) , serial => list_to_binary(RealSerial) }),
	generate_client_batch(CaInfo,SimName,Prefix,Current+1,Start,Left-1,Attributes,Notification,State).

all_files_exist([])->
	true;
all_files_exist([H|T])->
	case filelib:is_file(H) of
		true ->
			all_files_exist(T);
		false ->
			false
	end.

valid_ca_name(Name)->
	valid_ca_name(Name,1).
valid_ca_name([],Pos) when Pos>1 ->
	true;
valid_ca_name([],Pos) when Pos==1 ->
	false;
valid_ca_name([H|T],Pos) when (((H >= $0) and (H =< $9)) or ((H >= $a) and (H =< $z)) or (H == $_)) ->
	valid_ca_name(T,Pos+1);
valid_ca_name(_,_Pos)->
	false.

valid_password("")->
	true;
valid_password(Password)->
	valid_password(Password,1).
valid_password([],Pos) when Pos >= 8 ->
	true;
valid_password([],_Pos) ->
	false;
valid_password([H|T],Pos) when (((H>=$0) and (H=<$9)) or ((H>=$a) and (H=<$z)) or ((H>=$A) and (H=<$Z)) or (H==$-) or (H==$_)) ->
	valid_password(T,Pos+1);
valid_password(_,_) ->
	false.

create_tables()->
	{atomic,ok} = mnesia:create_table(simulations,[{disc_copies,[node()]}, {record_name,simulation}, {attributes,record_info(fields,simulation)}]),
	{atomic,ok} = mnesia:create_table(cas,    [{disc_copies,[node()]}, {record_name,ca_info},     {attributes,record_info(fields,ca_info)}]),
	{atomic,ok} = mnesia:create_table(clients,[{disc_copies,[node()]}, {record_name,client_info}, {index,[wan_mac0,serial]},{attributes,record_info(fields,client_info)}]),
	{atomic,ok} = mnesia:create_table(servers,[{disc_copies,[node()]}, {record_name,server_info}, {attributes,record_info(fields,server_info)}]),
	ok.

ca_in_use(CAName) ->
	case mnesia:transaction( fun() ->
												mnesia:foldr( fun(R,A) ->
													case CAName == R#simulation.ca of
														true ->
															[ binary_to_list(R#simulation.name) | A ];
														false ->
															A
													end
												end,[],simulations)
	                    end) of
		{atomic,List} ->
			List;
		_ ->
			[]
	end.

add_record(R) when is_record(R,ca_info) ->
	case mnesia:transaction( fun() ->
												mnesia:dirty_write(cas,R)
											end ) of
		{atomic,_} -> ok;
		_ -> error
	end;
add_record(R) when is_record(R,server_info) ->
	case mnesia:transaction( fun() ->
												mnesia:dirty_write(servers,R)
	                    end) of
		{atomic,_} ->
			ok;
		_ -> error
	end;
add_record(R) when is_record(R,simulation) ->
	case mnesia:transaction( fun() ->
												mnesia:dirty_write(simulations,R#simulation{creation_date = list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second)))})
	                    end) of
		{atomic,_} ->
			ok;
		_ ->
			error
end;
add_record(R) when is_record(R,client_info) ->
	case mnesia:transaction( fun() ->
												mnesia:dirty_write(clients,R)
	                    end) of
		{atomic,_} ->
			ok;
		_ ->
			error
end.

del_record(R) when is_record(R,ca_info) ->
	mnesia:transaction(   fun() ->
													mnesia:dirty_delete(cas,R#ca_info.name)
	                      end);
del_record(R) when is_record(R,server_info) ->
	mnesia:transaction(   fun() ->
													mnesia:dirty_delete(servers,R#server_info.name)
	                      end);
del_record(R) when is_record(R,simulation) ->
	mnesia:transaction(   fun() ->
		mnesia:dirty_delete(simulations,R#simulation.name)
	                      end);
del_record(R) when is_record(R,client_info) ->
	mnesia:transaction( fun() ->
													mnesia:dirty_delete(clients,R#client_info.name)
	                    end).

get_record(R) when is_record(R,ca_info)->
	case mnesia:transaction( fun() ->
												mnesia:read(cas,R#ca_info.name)
	                    end) of
		{atomic,[Record]} ->
			{ok,Record};
		_ ->
			{error,unknown}
	end;
get_record(R) when is_record(R,simulation)->
	case mnesia:transaction( fun() ->
												mnesia:read(simulations,R#simulation.name)
	                    end) of
		{atomic,[Record]} ->
			{ok,Record};
		_ ->
			{error,unknown}
end;
get_record(R) when is_record(R,server_info)->
	case mnesia:transaction( fun() ->
												mnesia:read(servers,R#server_info.name)
	                    end) of
		{atomic,[Record]} ->
			{ok,Record};
		_ ->
			{error,unknown}
	end;
get_record(R) when is_record(R,client_info)->
	case mnesia:transaction( fun() ->
												mnesia:read(clients,R#client_info.name)
	                    end) of
		{atomic,[Record]} ->
			{ok,Record};
		_ ->
			{error,unknown}
	end.

list_sim_clients(SimName) ->
	mnesia:transaction( fun() ->
												mnesia:foldr( fun(R,A) ->
																					case R#client_info.sim_name == SimName of
																						true -> [(R#client_info.name)|A];
																						false-> A
																					end
																			end,[],clients)
				              end).

list_records_names(#ca_info{}) ->
	case mnesia:transaction( fun() ->
												mnesia:foldr( fun(R,A) ->
																					[ binary_to_list(R#ca_info.name) | A]
												              end,[],cas)
	                    end) of
		{atomic,CASList} ->
			{ok,CASList};
		_ ->
			{ok,[]}
	end;
list_records_names(#simulation{}) ->
	case mnesia:transaction( fun() ->
												mnesia:foldr( fun(R,A) ->
																					[ binary_to_list(R#simulation.name) | A]
												              end,[],simulations)
	                    end) of
		{atomic,SIMList} ->
			{ok,SIMList};
		_ ->
			{ok,[]}
	end;
list_records_names(#client_info{}) ->
	case mnesia:transaction( fun() ->
		mnesia:foldr( fun(R,A) ->
			[ binary_to_list(R#client_info.name) | A]
		              end,[],clients)
	                         end) of
		{atomic,ClientList} ->
			{ok,ClientList};
		_ ->
			{ok,[]}
	end;
list_records_names(#server_info{}) ->
	case mnesia:transaction( fun() ->
		mnesia:foldr( fun(R,A) ->
			[ binary_to_list(R#server_info.name) | A]
		              end,[],servers)
	                         end) of
		{atomic,ServerList} ->
			{ok,ServerList};
		_ ->
			{ok,[]}
	end.

delete_all_records(simulations) ->
	_ = mnesia:clear_table(simulations),
	ok;
delete_all_records(clients) ->
	_ = mnesia:clear_table(clients),
	ok;
delete_all_records(servers) ->
	_ = mnesia:clear_table(servers),
	ok;
delete_all_records(cas) ->
	_ = mnesia:clear_table(cas),
	ok.

exists( #ca_info{ } = CAInfo ) ->
	Return = mnesia:transaction(  fun() ->
																	mnesia:read(cas,CAInfo#ca_info.name)
	                              end),
	case Return of
		{aborted,{no_exists,cas}} -> false;
		{atomic,[]} -> false;
		{atomic,_}-> true
	end;
exists( #simulation{ } = SimInfo ) ->
	Return = mnesia:transaction(  fun() ->
																	mnesia:read(simulations,SimInfo#simulation.name)
	                              end),
	case Return of
		{aborted,{no_exists,simulations}} -> false;
		{atomic,[]} -> false;
		{atomic,_}-> true
	end;
exists( #client_info{ } = ClientInfo ) ->
	Return = mnesia:transaction(  fun() ->
																	mnesia:read(clients,ClientInfo#client_info.name)
	                              end),
	case Return of
		{aborted,{no_exists,clients}} -> false;
		{atomic,[]} -> false;
		{atomic,_}-> true
	end;
exists( #server_info{ } = ServerInfo ) ->
	Return = mnesia:transaction(  fun() ->
																	mnesia:read(servers,ServerInfo#server_info.name)
	                              end),
	case Return of
		{aborted,{no_exists,servers}} -> false;
		{atomic,[]} -> false;
		{atomic,_}-> true
	end.

list_records(#simulation{})->
	Return = mnesia:transaction( fun()->
		mnesia:foldr( fun(E,A)->
			[ E | A ]
		              end, [], simulations)
	                             end),
	case Return of
		{aborted,{no_exists,simulations}} -> [];
		{atomic,Result} ->Result
	end;
list_records(#ca_info{})->
	Return = mnesia:transaction( fun()->
		mnesia:foldr( fun(E,A)->
			[ E | A ]
		              end, [], cas)
	                             end),
	case Return of
		{aborted,{no_exists,cas}} -> [];
		{atomic,Result} ->Result
	end;
list_records(#client_info{})->
	Return = mnesia:transaction( fun()->
		mnesia:foldr( fun(E,A)->
			[ E | A ]
		              end, [], clients)
	                             end),
	case Return of
		{aborted,{no_exists,clients}} -> [];
		{atomic,Result} ->Result
	end;
list_records(#server_info{})->
	Return = mnesia:transaction( fun()->
		mnesia:foldr( fun(E,A)->
			[ E | A ]
		              end, [], servers)
	                             end),
	case Return of
		{aborted,{no_exists,servers}} -> [];
		{atomic,Result} ->Result
	end.


