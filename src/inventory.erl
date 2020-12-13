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

-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").
-include("../include/inventory.hrl").

%% API
-export([start_link/0,creation_info/0,
	make_ca/2,get_ca/1,get_cas/0,delete_ca/1,
	make_server/3,get_server/2,make_servers/3,
	make_client/2,make_clients/5,generate_client_batch/6,get_client/2,
	all_files_exist/1,valid_ca_name/1,valid_password/1,
	delete_server/2,import_ca/2,create_tables/0,
	list_clients/1,gen_lan_clients/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-define(CADB_TABLE,cadb_tab).
-define(CLIENTS_TABLE,clients_tab).
-define(SERVERS_TABLE,servers_tab).

-define(CADB_TABLE_FILENAME,"cadb.ets").
-define(CLIENTS_TABLE_FILENAME,"clients.dets").
-define(SERVERS_TABLE_FILENAME,"servers.dets").

-record(inventory_state, {
						status = undefined,
						table,            %% DETS table name
						inventory_db_dir, %% where to store anything about inventory
						inventory_file_name,
						cert_db_dir,       %% where to put all certs
						ca_db_filename,
						clients_db_filename,
						servers_db_filename,
						batch_generation_pid = undefined,
						clients_tab,
						servers_tab
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
			gen_server:call(?SERVER,{make_ca,list_to_binary(CAName),list_to_binary(Password),self()});
		false->
			{error,invalid_ca_name_or_password}
	end.

-spec import_ca(Name::string(),Attributes::attribute_list()) -> {ok,ca_info()} | generic_error().
import_ca(CAName,Attributes)->
	gen_server:call(?SERVER,{import_ca,list_to_binary(CAName),Attributes,self()}).

-spec get_ca( Name::string()|binary() )-> {ok,ca_info()} | generic_error().
get_ca(Name)->
	gen_server:call(?SERVER,{get_ca,safe_binary(Name),self()}).

-spec delete_ca( Name::string()|binary() ) -> ok | generic_error().
delete_ca(Name)->
	gen_server:call(?SERVER,{delete_ca,safe_binary(Name),self()}).

-spec get_cas() -> { ok , [ CAName::string() ]} | generic_error().
get_cas()->
	gen_server:call(?SERVER,{get_cas,self()}).

-spec safe_binary(string()|binary())->binary().
safe_binary(X) when is_list(X)->
	list_to_binary(X);
safe_binary(X) when is_binary(X)->
	X.

-spec make_server(CAName::string()|binary(),Name::string()|binary(),Type::service_role())-> { ok , SI::server_info() } | generic_error().
make_server(CAName,Name, mqtt_server )->
	gen_server:call(?SERVER,{make_server,safe_binary(CAName),safe_binary(Name),mqtt_server,self()});
make_server(CAName,Name, ovsdb_server )->
	gen_server:call(?SERVER,{make_server,safe_binary(CAName),safe_binary(Name),ovsdb_server,self()}).

-spec make_servers(CAName::string()|binary(),Servers::list(ServerName::string()),Type::service_role())-> { ok, list({ ServerName::string(),SI::server_info()})} | generic_error().
make_servers(CAName,ServerList,mqtt_server)->
	gen_server:call(?SERVER,{make_servers,safe_binary(CAName),utils:to_binary_list(ServerList,[]),mqtt_server,self()});
make_servers(CAName,ServerList,ovsdb_server)->
	gen_server:call(?SERVER,{make_servers,safe_binary(CAName),utils:to_binary_list(ServerList,[]),ovsdb_server,self()}).

-spec get_server(CAName::string()|binary(),Id::string()|binary())-> { ok, SI::server_info()} | generic_error().
get_server(CAName,Id)->
	gen_server:call(?SERVER,{get_server,safe_binary(CAName),safe_binary(Id),self()}).

-spec delete_server(CAName::string()|binary(),Id::string()|binary())-> { ok, SI::server_info()} | generic_error().
delete_server(CAName,Id)->
	gen_server:call(?SERVER,{delete_server,safe_binary(CAName),safe_binary(Id),self()}).

-spec make_client(CAName::string()|binary(),Attributes::#{ atom() => term() })-> {ok,Client::client_info()} | generic_error().
make_client(CAName,Attributes)->
	case validate_attributes(Attributes) of
		true ->
			gen_server:call(?SERVER,{make_client,safe_binary(CAName),Attributes});
		false ->
			{error,missing_attributes}
	end.

-spec make_clients(CAName::string()|binary(),Start::integer(),HowMany::integer(),Attributes::#{ atom() => term() }, Notification::notification_cb() ) -> {ok,HowManyDone::integer()} | generic_error().
make_clients(CAName,Start,HowMany,Attributes,Notification) ->
	case validate_attributes(Attributes) of
		true -> gen_server:call(?SERVER,{make_many_clients,safe_binary(CAName),Start,HowMany,Attributes,Notification});
		false -> { error, missing_attributes }
	end.

validate_attributes(Attrs) when is_map(Attrs)->
	maps:is_key(serial,Attrs) and maps:is_key(name,Attrs) and maps:is_key(mac,Attrs) and maps:is_key(id,Attrs).

-spec get_client(CAName::string()|binary(),Id::string()|binary())-> { ok , Client::client_info() } | generic_error().
get_client(CAName,Id)->
	gen_server:call(?SERVER,{get_client,safe_binary(CAName),safe_binary(Id)}).

-spec list_clients(CAName::string()|binary())-> { ok , [Client::binary()] } | generic_error().
list_clients(CAName)->
	gen_server:call(?SERVER,{list_clients,safe_binary(CAName)}).

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
	CaDbFileName=filename:join(CertsDbDir,?CADB_TABLE_FILENAME),
	ServersDbFileName=filename:join([InventoryDbDir,?SERVERS_TABLE_FILENAME]),
	ClientsDbFileName=filename:join([InventoryDbDir,?CLIENTS_TABLE_FILENAME]),

	{ok,ClientsTab} = dets:open_file(?CLIENTS_TABLE,[{file,ClientsDbFileName},{keypos,2}]),
	{ok,ServersTab} = dets:open_file(?SERVERS_TABLE,[{file,ServersDbFileName},{keypos,2}]),

	_ = case filelib:is_file(CaDbFileName) of
		true ->
			case ets:file2tab(CaDbFileName) of
				{ok,_Tab} -> ok;
				{error,_} ->
					_ = ets:new(?CADB_TABLE,[named_table,public,{keypos,2}]),
					_ = ets:tab2file(?CADB_TABLE,CaDbFileName)
			end;
		false->
			_ = ets:new(?CADB_TABLE,[named_table,public,{keypos,2}]),
			_ = ets:tab2file(?CADB_TABLE,CaDbFileName)
	end,
	{ok, #inventory_state{
		status = started,
		ca_db_filename = CaDbFileName,
		inventory_db_dir = InventoryDbDir,
		cert_db_dir = CertsDbDir,
		servers_db_filename = ServersDbFileName,
		clients_db_filename = ClientsDbFileName,
		servers_tab = ServersTab,
		clients_tab = ClientsTab
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
handle_call({make_ca,Ca,Password,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[]->
			{ok,NewState}=create_ca(Ca,Password,State,Pid),
			{reply, ok, NewState};
		[_CAInfo]->
			{reply,{error,ca_already_exists},State}
	end;

handle_call({import_ca,Ca,Attributes,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[]->
			{ok,NewState}=import_a_ca(Ca,Attributes,State,Pid),
			{reply, ok, NewState};
		[_CAInfo]->
			{reply,{error,ca_already_exists},State}
	end;

handle_call({delete_ca,Ca,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[]->
			{reply,{error,unknown_ca},State};
		[CAInfo]->
			{ok,NewState}=delete_ca(CAInfo,State,Pid),
			{reply, ok, NewState}
	end;

handle_call({get_ca,Ca,_Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[]->
			{reply,{error,unknown_ca},State};
		[CAInfo]->
			try
				case get_record(CAInfo) of
					{atomic,[Record]} ->
						{reply, {ok,Record}, State};
					_ ->
						{reply, {error,unknown_ca}, State}
				end
			catch
				_:_ ->
					{reply,{error,unknown_error},State}
			end
	end;

handle_call({get_cas,_Pid}, _From, State = #inventory_state{}) ->
	CAs = ets:foldr(fun(E,A)-> [ binary_to_list(E#ca_info.name) | A ] end,[],?CADB_TABLE),
	{ reply, { ok,CAs}, State};

handle_call({make_server,Ca,Id,Type,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_server(CAInfo,Id,Type,State,Pid),
			{reply, ok, NewState}
	end;

handle_call({make_servers,Ca,ServerList,Type,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_servers(CAInfo,ServerList,Type,State,Pid),
			{reply, ok, NewState}
	end;

handle_call({get_server,Ca,Id,_Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			Server = #server_info{ name = Id },
			case get_record(Server) of
				{atomic,[Record]} ->
					case Record#server_info.ca == CAInfo#ca_info.name of
						true ->
							{ reply, {ok,Record},State};
						false ->
							{ reply , {error, unknown_client},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end
	end;

handle_call({delete_server,Ca,Id,_ParentPid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			Server = #server_info{ name = Id },
			case get_record(Server) of
				{atomic,[Record]} ->
					case Record#server_info.ca == CAInfo#ca_info.name of
						true ->
							_ = del_record(Record),
							{ reply, ok,State};
						false ->
							{ reply , {error, unknown_server},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end
	end;

handle_call({make_client,Ca,Attributes}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_client(CAInfo,Attributes,State),
			{reply, ok, NewState}
	end;

handle_call({make_many_clients,Ca,Start,HowMany,Attributes,Notification}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			JobPid = spawn(?MODULE,generate_client_batch,[CAInfo,Start,HowMany,Attributes,Notification,State]),
			{ reply, ok , State#inventory_state{batch_generation_pid = JobPid} }
	end;

handle_call({get_client,Ca,Id}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			Client = #client_info{ name = Id },
			case get_record(Client) of
				{atomic,[Record]} ->
					case Record#client_info.ca == CAInfo#ca_info.name of
						true ->
							{ reply, {ok,Record},State};
						false ->
							{ reply , {error, unknown_client},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end
	end;

handle_call({list_clients,CAName}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,CAName) of
		[] ->
			{reply,{error,unknown_ca},State};
		[_CAInfo] ->
			case list_ca_clients(CAName) of
				{atomic,Records} ->
					{ reply, {ok,Records},State};
				Error ->
					{ reply, {error, Error}, State}
			end
	end;

handle_call({delete_client,Ca,Id}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			Client = #client_info{ name = Id },
			case get_record(Client) of
				{atomic,[Record]} ->
					case Record#client_info.ca == CAInfo#ca_info.name of
						true ->
							_ = del_record(Record),
							{ reply, ok,State};
						false ->
							{ reply , {error, unknown_client},State}
					end;
				Error ->
					{ reply, {error, Error}, State}
			end
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
terminate(Reason, State = #inventory_state{}) ->
	update_disk_db(State),
	ok = dets:close(State#inventory_state.clients_tab),
	ok = dets:close(State#inventory_state.servers_tab),
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
	ets:insert(?CADB_TABLE,NewCa),
	update_disk_db(State),

	{ ok, State#inventory_state{ status = created } }.

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
	ets:insert(?CADB_TABLE,NewCa),
	update_disk_db(State),

	{ ok, State#inventory_state{ status = created } }.

delete_ca(CAInfo,State,_Pid)->
	_ = del_record(CAInfo),
	_ = file:del_dir_r(CAInfo#ca_info.dir_name),
	ets:delete(?CADB_TABLE,CAInfo#ca_info.name),
	update_disk_db(State),
	{ ok , State }.

%% -subj "/C=US/ST=Utah/L=Lehi/O=Your Company, Inc./OU=IT/CN=yourdomain.com"

%% openssl req -batch -config mqtt-server.cnf -newkey rsa:2048 -sha256 -out mqttservercert.csr -outform PEM
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_server -out mqttservercert.pem -infiles mqttservercert.csr
%% openssl rsa -passin pass:apassword -in mqttserverkey.pem -out mqttserverkey_dec.pem
-spec create_server(CAInfo::ca_info(),Name::binary(),Type::service_role(),State::#inventory_state{},ParentPid::pid()) -> { ok , NewState::#inventory_state{} } | generic_error().
create_server(CAInfo,Name,Type,State,_Pid)->
	BaseDir = binary_to_list(CAInfo#ca_info.servers_dir_name),
	ServerKeyPem = filename:join([BaseDir,binary_to_list(Name) ++ "-key.pem"]),
	ServerKeyDec = filename:join([BaseDir,binary_to_list(Name) ++ "-key_dec.pem"]),
	ServerCertCsr = filename:join([BaseDir,binary_to_list(Name) ++ "-cert.csr"]),
	ServerCertPem = filename:join([BaseDir,binary_to_list(Name) ++ "-cert.pem"]),

	CaBase = binary_to_list(CAInfo#ca_info.dir_name),
	CreateServerScriptFileName = filename:join([CaBase,"ssl-create-server.sh"]),
	_Result = os:cmd(CreateServerScriptFileName),
	%% io:format("RESULT: ~p~n",[Result]),

	file:rename( filename:join([CaBase,"servercert.csr"]),ServerCertCsr ),
	file:rename( filename:join([CaBase,"servercert.pem"]),ServerCertPem ),
	file:rename( filename:join([CaBase,"serverkey.pem"]),ServerKeyPem ),
	file:rename( filename:join([CaBase,"serverkey_dec.pem"]),ServerKeyDec ),

	{ok,KeyPemData} = utils:pem_to_key(ServerKeyPem),
	{ok,ServerCertPemData} = utils:pem_to_cert(ServerCertPem),
	{ok,ServerKeyDecPemData} = utils:pem_to_key(ServerKeyDec),
	{ok,ServerCertCsrPemData} = file:read_file(ServerCertCsr),

	NewServerInfo = #server_info{
		name = Name,
		service = Type,
		ca = CAInfo#ca_info.name,
		key = KeyPemData,
		cert = ServerCertPemData,
		decrypt = ServerKeyDecPemData,
		csr = ServerCertCsrPemData,
		cacert = CAInfo#ca_info.cert
	},

	_ = add_record(NewServerInfo),

	{ ok, State }.

create_servers(_CAInfo,[],_,State,_Pid)->
	{ok,State};
create_servers(CAInfo,[H|T],Type,State,Pid)->
	_ = create_server(CAInfo,H,Type,State,Pid),
	create_servers(CAInfo,T,Type,State,Pid).

%% openssl req -batch -config openssl-client.cnf -newkey rsa:2048 -sha256 -out clientcert.csr -outform PEM -nodes
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_client -out clientcert.pem -infiles clientcert.csr
%% openssl rsa -passin pass:apassword -in clientkey.pem -out clientkey_dec.pem
-spec create_client( CAInfo :: ca_info(), Attributes::#{ atom() => term() }, State::#inventory_state{}) ->
			{ ok , NewState::#inventory_state{} } | { error , Reason :: term()}.
create_client(CAInfo,Attributes,State)->
	#{ mac := Mac, serial := Serial, name := _Name , id := HardwareId } = Attributes,
	BaseFileName = filename:join( [ binary_to_list(CAInfo#ca_info.clients_dir_name),binary_to_list(Serial)]),
	ClientKeyPem = BaseFileName ++ "-key.pem",
	ClientKeyDec = BaseFileName ++  "-key_dec.pem",
	ClientCertCsr = BaseFileName ++  "-cert.csr",
	ClientCertPem = BaseFileName ++  "-cert.pem",

	CaBase = binary_to_list(CAInfo#ca_info.dir_name),
	CreateClientScriptFileName = filename:join([CaBase,"ssl-create-client.sh"]),
	_Result = os:cmd(CreateClientScriptFileName),
	%% io:format("RESULT: ~p~n",[Result]),

	file:rename( filename:join([CaBase,"clientcert.csr"]),ClientCertCsr ),
	file:rename( filename:join([CaBase,"clientcert.pem"]),ClientCertPem ),
	file:rename( filename:join([CaBase,"clientkey.pem"]),ClientKeyPem ),
	file:rename( filename:join([CaBase,"clientkey_dec.pem"]),ClientKeyDec ),

	{ok,ClientCertCsrData} = file:read_file(ClientCertCsr),
	{ok,ClientKeyDecData} = utils:pem_to_key(ClientKeyDec),
	{ok,ClientKeyPemData} = utils:pem_to_key(ClientKeyPem),
	{ok,ClientCertPemData} = utils:pem_to_cert(ClientCertPem),

	Bands = gen_bands(),

	[X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,_X6b] = string:to_lower(binary_to_list(Mac)),
	Client = #client_info{
		name = Serial,
		ca = CAInfo#ca_info.name,
		cap = [ mqtt_client , ovsdb_client ],
		wan_mac0 = list_to_binary([X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,$0]),
		lan_mac0 = list_to_binary([X1a,X1b,$:,X2a,X2b,$:,X3a,X3b,$:,X4a,X4b,$:,X5a,X5b,$:,X6a,$8]),
		id = HardwareId,
		serial = Serial,
		bands = Bands,
		wifi_clients = gen_wan_clients(Bands),
		lan_clients = [{<<"lan0">>,gen_lan_clients()}],
		key = ClientKeyPemData,
		cert = ClientCertPemData,
		decrypt = ClientKeyDecData,
		cacert = CAInfo#ca_info.cert,
		csr = ClientCertCsrData
	},
	%% io:format(">>>SERIAL: ~p~n",[Serial]),
	_R = add_record(Client),
	%% io:format("RESULT>>>=~p~n",[R]),
	{ ok, State }.

gen_bands()->
	case rand:uniform(4) of
		1 -> ['BAND2G'];
		2 -> ['BAND2G','BAND5G'];
		3 -> ['BAND2G','BAND5G','BAND5GL','BAND5GU'];
		4 -> ['BAND2G','BAND5G','BAND5GL','BAND5GU']
	end.

gen_client(OUI)->
	[A1,A2,A3,A4,A5,A6] = OUI,
	[X1,X2,X3,X4,X5,X6] = lists:flatten(string:pad(integer_to_list(rand:uniform(1 bsl 24),16),6,leading,$0)),
	list_to_binary(string:to_lower([A1,A2,$:,A3,A4,$:,A5,A6,$:,X1,X2,$:,X3,X4,$:,X5,X6])).
gen_clients(0,Acc)->
	Acc;
gen_clients(Number,Acc)->
	OUI=binary_to_list(oui_server:get_an_oui()),
	gen_clients(Number-1,[gen_client(OUI)|Acc]).
gen_lan_clients() ->
	gen_clients(rand:uniform(8),[]).
gen_wan_clients(Bands)->
	gen_wlan_clients(Bands,[]).
gen_wlan_clients([],Acc)->
	Acc;
gen_wlan_clients([Band|T],Acc)->
	gen_wlan_clients(T,[{Band,list_to_binary(animals:get_an_animal()),gen_lan_clients()}|Acc]).

generate_client_batch(CAInfo,Start,HowMany,Attributes,Notification,State)->
	#{ id := HardwareId } = Attributes,
	case hardware:get_by_id(HardwareId) of
		{ok,[HardwareInfo]} ->
			{ok,[OUI|_]} = oui_server:lookup_vendor(HardwareInfo#hardware_info.vendor),
			<<A,B,C,D,E,F>> = OUI,
			Prefix = [A,B,$:,C,D,$:,E,F,$:],
			%% io:format("BATCH: ca=~p prefix=~p start=~p howmany=~p attrs=~p notify=~p~n",[CAInfo,Prefix,Start,HowMany,Attributes,Notification]),
			generate_client_batch(CAInfo,Prefix,1,Start,HowMany,Attributes,Notification,State);
		Error ->
			Error
	end.

generate_client_batch(_CaInfo,_Prefix,_Current,_Start,0,_Attributes,{M,F,A}=_Notification,_State)->
	apply(M,F,A);
generate_client_batch(CaInfo,Prefix,Current,Start,Left,Attributes,Notification,State)->
	[X1,X2,X3,X4,X5] = lists:flatten(string:pad(integer_to_list(Current,16),5,leading,$0)),
	#{ serial := Serial, name := Name } = Attributes,
	[A,B,$:,C,D,$:,E,F,$:] = Prefix,
	Mac = Prefix ++ [X1,X2,$:,X3,X4,$:,X5,$0],
	RealSerial = binary_to_list(Serial) ++ [A,B,C,D,E,F] ++ [X1,X2,X3,X4,X5,$0],
	RealName = binary_to_list(Name) ++ "-" ++ [X1,X2,X3,X4,X5,$0],
	_ = create_client(CaInfo, Attributes#{ name => list_to_binary(RealName), mac => list_to_binary(Mac) , serial => list_to_binary(RealSerial) },State),
	generate_client_batch(CaInfo,Prefix,Current+1,Start,Left-1,Attributes,Notification,State).

all_files_exist([])->
	true;
all_files_exist([H|T])->
	case filelib:is_file(H) of
		true ->
			all_files_exist(T);
		false ->
			false
	end.

update_disk_db(State)->
	_ = ets:tab2file(?CADB_TABLE,State#inventory_state.ca_db_filename),
	ok.

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
	{atomic,ok} = mnesia:create_table(cas,    [{disc_copies,[node()]}, {record_name,ca_info},     {attributes,record_info(fields,ca_info)}]),
	{atomic,ok} = mnesia:create_table(clients,[{disc_copies,[node()]}, {record_name,client_info}, {index,[wan_mac0,serial]},{attributes,record_info(fields,client_info)}]),
	{atomic,ok} = mnesia:create_table(servers,[{disc_copies,[node()]}, {record_name,server_info}, {attributes,record_info(fields,server_info)}]),
	ok.

add_record(R) when is_record(R,ca_info) ->
	mnesia:transaction( fun() ->
												mnesia:dirty_write(cas,R)
											end );
add_record(R) when is_record(R,server_info) ->
	mnesia:transaction( fun() ->
												mnesia:dirty_write(servers,R)
	                    end);
add_record(R) when is_record(R,client_info) ->
	mnesia:transaction( fun() ->
												mnesia:dirty_write(clients,R)
	                    end).

del_record(R) when is_record(R,ca_info) ->
	mnesia:transaction(   fun() ->
													mnesia:dirty_delete(cas,R#ca_info.name)
	                      end);
del_record(R) when is_record(R,server_info) ->
	mnesia:transaction(   fun() ->
													mnesia:dirty_delete(servers,R#server_info.name)
	                      end);
del_record(R) when is_record(R,client_info) ->
	mnesia:transaction( fun() ->
													mnesia:dirty_delete(clients,R#client_info.name)
	                    end).

get_record(R) when is_record(R,ca_info)->
	mnesia:transaction( fun() ->
												mnesia:read(cas,R#ca_info.name)
	                    end);
get_record(R) when is_record(R,server_info)->
	mnesia:transaction( fun() ->
												mnesia:read(servers,R#server_info.name)
	                    end);
get_record(R) when is_record(R,client_info)->
	mnesia:transaction( fun() ->
												mnesia:read(clients,R#client_info.name)
	                    end).

list_ca_clients(CAName) ->
	mnesia:transaction( fun() ->
												mnesia:foldr( fun(R,A) ->
																					case R#client_info.ca == CAName of
																						true -> [(R#client_info.name)|A];
																						false-> A
																					end
																			end,[],clients)
				              end).

