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

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").
-include("../include/inventory.hrl").

%% API
-export([start_link/0,creation_info/0,
	make_ca/2,get_ca/1,get_cas/0,delete_ca/1,
	make_server/2,get_server/2,make_servers/2,
	make_client/2,make_clients/3,generate_client_batch/6,get_client/2,
	all_files_exist/1,valid_ca_name/1,valid_password/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

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

-spec make_ca( Name::string(), Password::string() ) -> ok | { error,Reason::term() }.
make_ca(Name,Password)->
	case (valid_ca_name(Name) and valid_password(Password) )of
		true->
			gen_server:call(?MODULE,{make_ca,Name,Password,self()});
		false->
			{error,invalid_ca_name_or_password}
	end.

-spec get_ca( Name::string() )-> {ok,ca_info()} | {error,Reason::term()}.
get_ca(Name)->
	gen_server:call(?MODULE,{get_ca,Name,self()}).

-spec delete_ca( Name::string() ) -> ok | { error,Reason::term() }.
delete_ca(Name)->
	gen_server:call(?MODULE,{delete_ca,Name,self()}).

-spec get_cas() -> { ok , [ CAName::string() ]} | {error,Reason::term()}.
get_cas()->
	gen_server:call(?MODULE,{get_cas,self()}).

-spec make_server(Ca::string(),Id::string())-> { ok , SI::server_info() } | { error, Reason::term()}.
make_server(Ca,Id)->
	gen_server:call(?MODULE,{make_server,Ca,Id,self()}).

-spec make_servers(Ca::string(),Servers::list(ServerName::string()))-> { ok, list({ ServerName::string(),SI::server_info()})} | { error, Reason::term() }.
make_servers(Ca,ServerList)->
	gen_server:call(?MODULE,{make_servers,Ca,ServerList,self()}).

-spec get_server(Ca::string(),Id::string())-> { ok, SI::server_info()} | { error, Reason::term() }.
get_server(Ca,Id)->
	gen_server:call(?MODULE,{get_server,Ca,Id,self()}).

-spec make_client(Ca::string(),Id::string())-> {ok,Client::client_info()} | {error,Reason::term()}.
make_client(Ca,Id)->
	gen_server:call(?MODULE,{make_client,Ca,Id,self()}).

-spec make_clients(Ca::string(),OUIBase::string(),HowMany::integer()) -> {ok,HowManyDone::integer()} | {error,Reason::term()}.
make_clients(Ca,OuiBase,HowMany) when is_list(OuiBase), HowMany >= 1 ->
	gen_server:call(?MODULE,{make_many_clients,Ca,OuiBase,HowMany,self()}).

-spec get_client(Ca::string(),Id::string())-> { ok , Client::client_info() } | {error,Reason::term()}.
get_client(Ca,Id)->
	gen_server:call(?MODULE,{get_client,Ca,Id}).

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #inventory_state{}} | {ok, State :: #inventory_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	startdb(),
	process_flag(trap_exit, true),
	InventoryDbDir = application:get_env(?OWLS_APP,inventory_db_dir,""),
	ok = utils:make_dir(InventoryDbDir),
	CertsDbDir = application:get_env(?OWLS_APP,cert_db_dir,""),
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
				Attributes = [{key_pem,CAInfo#ca_info.key_data},
					{cert_pem,CAInfo#ca_info.cert_data},
					{config,CAInfo#ca_info.config_data}],
				{ reply, {ok,Attributes}, State }
			catch
				_:_ ->
					{reply,{error,unknown_error},State}
			end
	end;
handle_call({get_cas,_Pid}, _From, State = #inventory_state{}) ->
	CAs = ets:foldr(fun(E,A)-> [ E#ca_info.name | A ] end,[],?CADB_TABLE),
	{ reply, { ok,CAs}, State};

handle_call({get_client,Ca,Id}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[_CAInfo] ->
			case dets:lookup(State#inventory_state.clients_tab,{Ca,Id}) of
				[ClientInfo] ->
					{ reply, {ok,ClientInfo}, State };
				_ ->
					{reply,{error,unknown_client},State}
			end
	end;
handle_call({make_server,Ca,Id,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_server(CAInfo,Id,State,Pid),
			{reply, ok, NewState}
	end;
handle_call({make_servers,Ca,ServerList,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_servers(CAInfo,ServerList,State,Pid),
			{reply, ok, NewState}
	end;
handle_call({get_server,Ca,Id,_Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[_CAInfo] ->
			case dets:lookup(State#inventory_state.servers_tab,{Ca,Id}) of
				[] ->
					{reply, {error,server_not_find}, State };
				[ServerInfo]->
					{reply, {ok,ServerInfo},State}
			end
	end;
handle_call({make_client,Ca,Name,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			{ok , NewState}=create_client(CAInfo,Name,State,Pid),
			{reply, ok, NewState}
	end;
handle_call({make_many_clients,Ca,OuiBase,HowMany,Pid}, _From, State = #inventory_state{}) ->
	case ets:lookup(?CADB_TABLE,Ca) of
		[] ->
			{reply,{error,unknown_ca},State};
		[CAInfo] ->
			JobPid = spawn(?MODULE,generate_client_batch,[CAInfo,OuiBase,HowMany,Pid,self(),State]),
			{ reply, ok , State#inventory_state{batch_generation_pid = JobPid} }
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
create_ca(CaName,Password,State,Pid)->
	%% Make all the directories
	CaDir = filename:join([State#inventory_state.cert_db_dir,CaName]),
	ok = utils:make_dir(CaDir),
	CaClientsDir = filename:join([CaDir,"clients"]),
	CaServersDir = filename:join([CaDir,"servers"]),
	ok = utils:make_dir(CaClientsDir),
	ok = utils:make_dir(CaServersDir),

	{ ok , TemplateConf } = file:read_file( filename:join([code:priv_dir(?OWLS_APP),"templates","ca.cnf.template"] )),
	NewConf = string:replace(binary_to_list(TemplateConf),"$$DIR_ROOT$$",CaDir,all),
	CaConfigFileName = filename:join([CaDir,CaName++".cnf"]),
	ok = file:write_file(CaConfigFileName , list_to_binary(NewConf)),

	CaKeyFileName = filename:join([CaDir,CaName ++ "_key.pem"]),
	CaKeyCertFileName = filename:join([CaDir,CaName ++ "_cert.pem"]),
	CaConfigFileName = filename:join([CaDir,CaName++".cnf"]),

	Cmd0 = io_lib:format("openssl genrsa -passout pass:~s -aes256 -out ~s 4096",[Password,CaKeyFileName]),
	CommandResult0 = os:cmd(Cmd0),
	_ = file:change_mode(CaKeyFileName,8#0400),
	io:format("~p> CMD0: ~s, RESULT: ~s~n~n",[Pid,Cmd0,CommandResult0]),
	Cmd1 = io_lib:format("openssl req -config ~s -batch -new -x509 -days 3000 -sha256 -extensions v3_ca -passin pass:~s -key ~s -out ~s",
		[ CaConfigFileName,
			Password,
			CaKeyFileName,
			CaKeyCertFileName ]),
	CommandResult1 = os:cmd(Cmd1),
	_ = file:change_mode(CaKeyCertFileName,8#0444),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),

	ok = file:write_file( filename:join([CaDir, "index.txt"]),<<>>),
	ok = file:write_file( filename:join([CaDir, "serial.txt"]),<<$0,$1>>),

	ok = utils:make_dir(filename:join([CaDir,"certs"])),
	ok = utils:make_dir(filename:join([CaDir,"newcerts"])),
	ok = utils:make_dir(filename:join([CaDir,"crl"])),
	ok = utils:make_dir(filename:join([CaDir,"private"])),

	{ok,CertData}=file:read_file(CaKeyCertFileName),

	NewCa = #ca_info{ name = CaName,
		dir_name = CaDir,
		clients_dir_name = CaClientsDir,
		servers_dir_name = CaServersDir,
		cert_file_name = CaKeyCertFileName,
		key_file_name =  CaKeyFileName,
		config_file_name = CaConfigFileName,
		cert_data = CertData,
		password = Password
		},
	ets:insert(?CADB_TABLE,NewCa),
	update_disk_db(State),
	{ ok, State#inventory_state{ status = created } }.

delete_ca(CAInfo,State,_Pid)->
	_ = file:del_dir_r(CAInfo#ca_info.dir_name),
	ets:delete(?CADB_TABLE,CAInfo#ca_info.name),
	update_disk_db(State),
	{ ok , State }.

%% -subj "/C=US/ST=Utah/L=Lehi/O=Your Company, Inc./OU=IT/CN=yourdomain.com"

%% openssl req -batch -config mqtt-server.cnf -newkey rsa:2048 -sha256 -out mqttservercert.csr -outform PEM
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_server -out mqttservercert.pem -infiles mqttservercert.csr
%% openssl rsa -passin pass:apassword -in mqttserverkey.pem -out mqttserverkey_dec.pem
create_server(CAInfo,Name,State,Pid)->
	ServerKeyPem = filename:join([CAInfo#ca_info.servers_dir_name,"server-" ++ Name ++ "-key.pem"]),
	ServerKeyDec = filename:join([CAInfo#ca_info.servers_dir_name,"server-" ++ Name ++ "-key_dec.pem"]),
	ServerCertCsr = filename:join([CAInfo#ca_info.servers_dir_name,"server-" ++ Name ++ "-cert.csr"]),
	ServerCertPem = filename:join([CAInfo#ca_info.servers_dir_name,"server-" ++ Name ++ "-cert.pem"]),
	Subject = "\"/C=CA/ST=BC/L=Vancouver/O=Arilia Wireless Inc./OU=Servers/CN=" ++ Name ++ "\"",
	Cmd1 = io_lib:format("openssl req -config ~s -batch -passout pass:~s -newkey rsa:2048 -sha256 -keyout ~s -out ~s -outform PEM",
		[ CAInfo#ca_info.config_file_name,
			CAInfo#ca_info.password,
			ServerKeyPem, ServerCertCsr]),
	CommandResult1 = os:cmd(Cmd1),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),
	Cmd2 = io_lib:format("openssl ca -batch -passin pass:~s -config ~s -subj ~s -keyfile ~s -cert ~s -extensions server_cert -policy policy_loose -out ~s -infiles ~s",
		[ CAInfo#ca_info.password,
			CAInfo#ca_info.config_file_name,
			Subject,
			CAInfo#ca_info.key_file_name,
			CAInfo#ca_info.cert_file_name,
			ServerCertPem,
			ServerCertCsr]),
	CommandResult2 = os:cmd(Cmd2),
	io:format("~p> CMD2: ~s, RESULT: ~s~n~n",[Pid,Cmd2,CommandResult2]),
	Cmd3 = io_lib:format("openssl rsa -passin pass:~s -in ~s -out ~s",
		[	CAInfo#ca_info.password, ServerKeyPem, ServerKeyDec]),
	CommandResult3 = os:cmd(Cmd3),
	io:format("~p> CMD3: ~s, RESULT: ~s~n~n",[Pid,Cmd3,CommandResult3]),
	{ ok, State }.

create_servers(_CAInfo,[],State,_Pid)->
	{ok,State};
create_servers(CAInfo,[H|T],State,Pid)->
	_ = create_server(CAInfo,H,State,Pid),
	create_servers(CAInfo,T,State,Pid).

%% openssl req -batch -config openssl-client.cnf -newkey rsa:2048 -sha256 -out clientcert.csr -outform PEM -nodes
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_client -out clientcert.pem -infiles clientcert.csr
%% openssl rsa -passin pass:apassword -in clientkey.pem -out clientkey_dec.pem
create_client(CAInfo,Name,State,Pid)->
	ClientKeyPem = filename:join([CAInfo#ca_info.clients_dir_name,"client-" ++ Name ++ "-key.pem"]),
	ClientKeyDec = filename:join([CAInfo#ca_info.clients_dir_name,"client-" ++ Name ++ "-key_dec.pem"]),
	ClientCertCsr = filename:join([CAInfo#ca_info.clients_dir_name,"client-" ++ Name ++ "-cert.csr"]),
	ClientCertPem = filename:join([CAInfo#ca_info.clients_dir_name,"client-" ++ Name ++ "-cert.pem"]),
	Subject = "\"/C=CA/ST=BC/L=Vancouver/O=Arilia Wireless Inc./OU=Clients/CN=" ++ Name ++ "\"",
	Cmd1 = io_lib:format("openssl req -config ~s -batch -passout pass:~s -newkey rsa:2048 -sha256 -keyout ~s -out ~s -outform PEM -nodes",
		[ CAInfo#ca_info.config_file_name, CAInfo#ca_info.password, ClientKeyPem,ClientCertCsr]),
	CommandResult1 = os:cmd(Cmd1),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),
	Cmd2 = io_lib:format("openssl ca -batch -passin pass:~s -config ~s -subj ~s -keyfile ~s -cert ~s -extensions usr_cert -policy policy_loose -out ~s -infiles ~s",
		[ CAInfo#ca_info.password,
			CAInfo#ca_info.config_file_name,
			Subject,
			CAInfo#ca_info.key_file_name,
			CAInfo#ca_info.cert_file_name,
			ClientCertPem	,
			ClientCertCsr
			]),
	CommandResult2 = os:cmd(Cmd2),
	io:format("~p> CMD2: ~s, RESULT: ~s~n~n",[Pid,Cmd2,CommandResult2]),
	Cmd3 = io_lib:format("openssl rsa -passin pass:~s -in ~s -out ~s",
		[	CAInfo#ca_info.password, ClientKeyPem, ClientKeyDec]),
	CommandResult3 = os:cmd(Cmd3),
	io:format("~p> CMD3: ~s, RESULT: ~s~n~n",[Pid,Cmd3,CommandResult3]),
	{ ok, State }.

generate_client_batch(CaInfo,[X1,X2,X3,X4,X5,X6],HowMany,Pid,ServicePid,State)->
	Prefix = [ X1,X2,":",X3,X4,":",X5,X6] ++ ":",
	generate_client_batch(CaInfo,Prefix,1,HowMany,Pid,ServicePid,State).

generate_client_batch(_CaInfo,Prefix,Done,0,Pid,ServicePid,_State)->
	gen_server:cast(ServicePid,{batch_generation_done,self()}),
	Pid ! { make_clients , done , Prefix, Done };
generate_client_batch(CaInfo,Prefix,Index,Left,Pid,ServicePid,State)->
	[X1,X2,X3,X4,X5,X6] = lists:flatten(string:pad(integer_to_list(Index,16),6,leading,$0)),
	Name = Prefix ++ [X1,X2,$:,X3,X4,$:,X5,X6],
	_ = create_client(CaInfo,Name,State,self()),
	generate_client_batch(CaInfo,Prefix,Index+1,Left-1,Pid,ServicePid,State).

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

startdb()->
	_ = case filelib:is_file(filename:join([code:priv_dir(?OWLS_APP),"mnesia","schema.DAT"])) of
		true ->
			mnesia:start();
		false ->
			ok=mnesia:create_schema([node()]),
			_ = mnesia:start(),
			create_tables()
	end,
	ok.

create_tables()->
	{atomic,ok}=mnesia:create_table(cas,[{attributes,record_info(fields,ca_info)}]),
	{atomic,ok}=mnesia:create_table(clients,[{attributes,record_info(fields,client_info)}]),
	{atomic,ok}=mnesia:create_table(servers,[{attributes,record_info(fields,server_info)}]),
	ok.
