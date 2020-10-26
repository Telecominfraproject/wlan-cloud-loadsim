%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2020 8:51 a.m.
%%%-------------------------------------------------------------------
-module(inventory).
-author("stephb").

-behaviour(gen_server).
-include("../include/mqtt_definitions.hrl").
-include("../include/internal.hrl").

%% API
-export([start_link/0,creation_info/0,
	make_ca/0,get_ca/0,
	make_server/1,get_server/1,make_servers/1,
	make_client/1,make_clients/2,generate_client_batch/5,get_client/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(inventory_state, {
														status = undefined,
														table,            %% DETS table name
														inventory_db_dir, %% where to store anything about inventory
														inventory_file_name,
														cert_db_dir,       %% where to put all certs
														ca_config_file,     %% CA SSL config file name
														ca_name,
														cakey_file,
														cacert_file,
														server_config_file, %% server SLL config file
														client_config_file,  %% client SSL config file
														ssl_ca_config_file,
														ssl_server_config_file,
														ssl_client_config_file,
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

make_ca()->
	gen_server:call(?MODULE,{make_ca,self()}).

get_ca()->
	gen_server:call(?MODULE,{get_ca,self()}).

make_server(Id)->
	gen_server:call(?MODULE,{make_server,Id,self()}).

make_servers(ServerList)->
	gen_server:call(?MODULE,{make_servers,ServerList,self()}).

get_server(Id)->
	gen_server:call(?MODULE,{get_server,Id,self()}).

make_client(Id)->
	gen_server:call(?MODULE,{make_client,Id,self()}).

make_clients(OuiBase,HowMany) when is_list(OuiBase), HowMany >= 1 ->
	gen_server:call(?MODULE,{make_many_clients,OuiBase,HowMany,self()}).

get_client(Id)->
	gen_server:call(?MODULE,{get_client,Id}).

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
	process_flag(trap_exit, true),
	InventoryDbDir = application:get_env(?MQTT_APP,inventory_db_dir,""),
	CertsDbDir = application:get_env(?MQTT_APP,cert_db_dir,""),
	CaName = application:get_env(?MQTT_APP,ca_name,"sim_ca"),
	file:make_dir(InventoryDbDir),
	file:make_dir(CertsDbDir),
	file:make_dir(CertsDbDir ++ "/clients" ),
	file:make_dir(CertsDbDir ++ "/servers" ),

	{ ok , TemplateConf } = file:read_file( filename:join([code:priv_dir(?MQTT_APP),"templates","ca.cnf.template"] )),
	NewConf = string:replace(binary_to_list(TemplateConf),"$$DIR_ROOT$$",CertsDbDir,all),
	SslCaConfigFile = filename:join([CertsDbDir,CaName ++ ".cnf"]),
	file:write_file(SslCaConfigFile , list_to_binary(NewConf)),
	DbFileName = filename:join([InventoryDbDir,CaName ++ "_inventory.dets"]),
	{ok,Name} = dets:open_file(inventory,[{file,DbFileName},{auto_save,10000}]),

	CaKeyFileName = filename:join([CertsDbDir,CaName ++ "_key.pem"]),
	CaKeyCertFileName = filename:join([CertsDbDir,CaName ++ "_cert.pem"]),

	CaStatus =
		case all_files_exist([SslCaConfigFile,CaKeyCertFileName,CaKeyFileName]) of
			true -> created;
			false-> initialized
		end,

	{ok, #inventory_state{
		status = CaStatus,
		table = Name,
		inventory_db_dir = InventoryDbDir,
		cert_db_dir = CertsDbDir,
		inventory_file_name = DbFileName,
		cakey_file = CaKeyFileName ,
		cacert_file = CaKeyCertFileName ,
		ssl_ca_config_file = SslCaConfigFile }}.

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
handle_call({make_ca,_}, _From, State = #inventory_state{ status = created }) ->
	{reply, {error,ca_already_created}, State};
handle_call({make_ca,Pid}, _From, State = #inventory_state{}) ->
	{ok,NewState}=create_ca(State,Pid),
	{reply, ok, NewState};
handle_call({get_ca,_Pid}, _From, State = #inventory_state{}) ->
	try
		{ok,SslCaConfigBin} = file:read_file(State#inventory_state.ssl_ca_config_file),
		{ok,CaKeyBin} = file:read_file(State#inventory_state.cakey_file),
		{ok,CaCertBin} = file:read_file(State#inventory_state.cacert_file),
			Attributes = [{config,SslCaConfigBin},
				{cakey,CaKeyBin},
				{cacert,CaCertBin}],
		{reply,{ok,Attributes},State}
	catch
		_ ->
			{reply,{error,enoent},State};
		_:_ ->
			{reply,{error,enoent},State}
	end;

handle_call({get_client,Id}, _From, State = #inventory_state{}) ->
	ClientKeyPem = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Id ++ "-key.pem",
	ClientKeyDec = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Id ++ "-key_dec.pem",
	ClientCertCsr = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Id ++ "-cert.csr",
	ClientCertPem = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Id ++ "-cert.pem",
	try
			{ok,ClientKeyPemBin} = file:read_file(ClientKeyPem),
			{ok,ClientKeyDecBin} = file:read_file(ClientKeyDec),
			{ok,ClientCertCsrBin} = file:read_file(ClientCertCsr),
			{ok,ClientCertPemBin} = file:read_file(ClientCertPem),
			Attributes = [{client_key_pem,ClientKeyPemBin},
				{client_key_dec,ClientKeyDecBin},
				{client_cert_csr,ClientCertCsrBin},
				{client_cert_pem,ClientCertPemBin}],
			{ reply, {ok,Attributes}, State }
	catch
		_ ->
			{reply, { error, enoent }, State};
		_:_ ->
			{reply, { error, enoent }, State}
	end;
handle_call({make_server,_,_}, _From, State = #inventory_state{ status = initialized }) ->
	{ reply, {error,ca_not_created},State};
handle_call({make_server,Id,Pid}, _From, State = #inventory_state{}) ->
	{ok , NewState}=create_server(Id,State,Pid),
	{reply, ok, NewState};
handle_call({make_servers,ServerList,Pid}, _From, State = #inventory_state{}) ->
	{ok,NewState}=create_servers(ServerList,State,Pid),
	{reply, ok, NewState};
handle_call({get_server,Id,_Pid}, _From, State = #inventory_state{}) ->
	ServerKeyPem = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Id ++ "-key.pem",
	ServerKeyDec = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Id ++ "-key_dec.pem",
	ServerCertCsr = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Id ++ "-cert.csr",
	ServerCertPem = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Id ++ "-cert.pem",
	try
		{ok,ServerKeyPemBin} = file:read_file(ServerKeyPem),
		{ok,ServerKeyDecBin} = file:read_file(ServerKeyDec),
		{ok,ServerCertCsrBin} = file:read_file(ServerCertCsr),
		{ok,ServerCertPemBin} = file:read_file(ServerCertPem),
		Attributes=[{server_key_pem,ServerKeyPemBin},
			{server_key_dec_bin,ServerKeyDecBin},
			{server_cert_csr,ServerCertCsrBin},
			{server_cert_pem,ServerCertPemBin}],
		{reply,{ok,Attributes},State}
	catch
		_ ->
			{reply,{error,enoent},State};
		_:_ ->
			{reply,{error,enoent},State}
	end;
handle_call({make_client,_,_}, _From, State = #inventory_state{ status = initialized }) ->
	{ reply, {error,ca_not_created},State};
handle_call({make_client,Name,Pid}, _From, State = #inventory_state{}) ->
	{ok,NewState}=create_client(Name,State,Pid),
	{reply, ok, NewState};
handle_call({make_many_clients,_,_,_}, _From, State = #inventory_state{ status = initialized }) ->
	{ reply, {error,ca_not_created},State};
handle_call({make_many_clients,OuiBase,HowMany,Pid}, _From, State = #inventory_state{ batch_generation_pid = undefined  }) ->
	JobPid = spawn(?MODULE,generate_client_batch,[OuiBase,HowMany,Pid,self(),State]),
	{ reply, ok , State#inventory_state{batch_generation_pid = JobPid} };
handle_call({make_many_clients,_,_,_}, _From, State = #inventory_state{}) ->
	{ reply, { error , running, State#inventory_state.batch_generation_pid} , State};
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
handle_info({'EXIT',FromPid,Reason},State)->
	dets:close(State#inventory_state.table),
	io:format("EXIT message: ~p ~p~n",[FromPid,Reason]),
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
terminate(_Reason, _State = #inventory_state{}) ->
	ok.

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
create_ca(State,Pid)->
	Cmd0 = io_lib:format("openssl genrsa -passout pass:apassword -aes256 -out ~s 4096",[State#inventory_state.cakey_file]),
	CommandResult0 = os:cmd(Cmd0),
	file:change_mode(State#inventory_state.cakey_file,8#0400),
	io:format("~p> CMD0: ~s, RESULT: ~s~n~n",[Pid,Cmd0,CommandResult0]),
	Cmd1 = io_lib:format("openssl req -config ~s -batch -new -x509 -days 3000 -sha256 -extensions v3_ca -passin pass:apassword -key ~s -out ~s",
		[ State#inventory_state.ssl_ca_config_file,
			State#inventory_state.cakey_file,
			State#inventory_state.cacert_file ]),
	CommandResult1 = os:cmd(Cmd1),
	file:change_mode(State#inventory_state.cacert_file,8#0444),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),
	file:write_file( State#inventory_state.cert_db_dir ++ "/index.txt",<<>>),
	file:write_file( State#inventory_state.cert_db_dir ++ "/serial.txt",<<$0,$1>>),

	file:make_dir(State#inventory_state.cert_db_dir ++ "/certs"),
	file:make_dir(State#inventory_state.cert_db_dir ++ "/newcerts"),
	file:make_dir(State#inventory_state.cert_db_dir ++ "/crl"),
	file:make_dir(State#inventory_state.cert_db_dir ++ "/private"),
	{ ok, State#inventory_state{ status = created } }.

%% -subj "/C=US/ST=Utah/L=Lehi/O=Your Company, Inc./OU=IT/CN=yourdomain.com"

%% openssl req -batch -config mqtt-server.cnf -newkey rsa:2048 -sha256 -out mqttservercert.csr -outform PEM
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_server -out mqttservercert.pem -infiles mqttservercert.csr
%% openssl rsa -passin pass:apassword -in mqttserverkey.pem -out mqttserverkey_dec.pem
create_server(Name,State,Pid)->
	ServerKeyPem = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Name ++ "-key.pem",
	ServerKeyDec = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Name ++ "-key_dec.pem",
	ServerCertCsr = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Name ++ "-cert.csr",
	ServerCertPem = State#inventory_state.cert_db_dir ++ "/servers/server-" ++ Name ++ "-cert.pem",
	Subject = "\"/C=CA/ST=BC/L=Vancouver/O=Arilia Wireless Inc./OU=Servers/CN=" ++ Name ++ "\"",
	Cmd1 = io_lib:format("openssl req -config ~s -batch -passout pass:apassword -newkey rsa:2048 -sha256 -keyout ~s -out ~s -outform PEM",
		[ State#inventory_state.ssl_ca_config_file,
			ServerKeyPem, ServerCertCsr]),
	CommandResult1 = os:cmd(Cmd1),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),
	Cmd2 = io_lib:format("openssl ca -batch -passin pass:apassword -config ~s -subj ~s -keyfile ~s -cert ~s -extensions server_cert -policy policy_loose -out ~s -infiles ~s",
		[ State#inventory_state.ssl_ca_config_file,
			Subject,
			State#inventory_state.cakey_file,
			State#inventory_state.cacert_file,
			ServerCertPem,
			ServerCertCsr]),
	CommandResult2 = os:cmd(Cmd2),
	io:format("~p> CMD2: ~s, RESULT: ~s~n~n",[Pid,Cmd2,CommandResult2]),
	Cmd3 = io_lib:format("openssl rsa -passin pass:apassword -in ~s -out ~s",
		[	ServerKeyPem, ServerKeyDec]),
	CommandResult3 = os:cmd(Cmd3),
	io:format("~p> CMD3: ~s, RESULT: ~s~n~n",[Pid,Cmd3,CommandResult3]),
	{ ok, State }.

create_servers([],State,_Pid)->
	{ok,State};
create_servers([H|T],State,Pid)->
	create_server(H,State,Pid),
	create_servers(T,State,Pid).

%% openssl req -batch -config openssl-client.cnf -newkey rsa:2048 -sha256 -out clientcert.csr -outform PEM -nodes
%% openssl ca -batch -key apassword -config openssl-ca.cnf -policy signing_policy -extensions signing_req_client -out clientcert.pem -infiles clientcert.csr
%% openssl rsa -passin pass:apassword -in clientkey.pem -out clientkey_dec.pem
create_client(Name,State,Pid)->
	ClientKeyPem = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Name ++ "-key.pem",
	ClientKeyDec = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Name ++ "-key_dec.pem",
	ClientCertCsr = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Name ++ "-cert.csr",
	ClientCertPem = State#inventory_state.cert_db_dir ++ "/clients/client-" ++ Name ++ "-cert.pem",
	Subject = "\"/C=CA/ST=BC/L=Vancouver/O=Arilia Wireless Inc./OU=Clients/CN=" ++ Name ++ "\"",
	Cmd1 = io_lib:format("openssl req -config ~s -batch -passout pass:apassword -newkey rsa:2048 -sha256 -keyout ~s -out ~s -outform PEM -nodes",
		[ State#inventory_state.ssl_ca_config_file, ClientKeyPem,ClientCertCsr]),
	CommandResult1 = os:cmd(Cmd1),
	io:format("~p> CMD1: ~s, RESULT: ~s~n~n",[Pid,Cmd1,CommandResult1]),
	Cmd2 = io_lib:format("openssl ca -batch -passin pass:apassword -config ~s -subj ~s -keyfile ~s -cert ~s -extensions usr_cert -policy policy_loose -out ~s -infiles ~s",
		[ State#inventory_state.ssl_ca_config_file,
			Subject,
			State#inventory_state.cakey_file,
			State#inventory_state.cacert_file,
			ClientCertPem	,
			ClientCertCsr
			]),
	CommandResult2 = os:cmd(Cmd2),
	io:format("~p> CMD2: ~s, RESULT: ~s~n~n",[Pid,Cmd2,CommandResult2]),
	Cmd3 = io_lib:format("openssl rsa -passin pass:apassword -in ~s -out ~s",
		[	ClientKeyPem, ClientKeyDec]),
	CommandResult3 = os:cmd(Cmd3),
	io:format("~p> CMD3: ~s, RESULT: ~s~n~n",[Pid,Cmd3,CommandResult3]),
	{ ok, State }.

generate_client_batch([X1,X2,X3,X4,X5,X6],HowMany,Pid,ServicePid,State)->
	Prefix = [ X1,X2,":",X3,X4,":",X5,X6] ++ ":",
	generate_client_batch(Prefix,1,HowMany,Pid,ServicePid,State).

generate_client_batch(Prefix,Done,0,Pid,ServicePid,_State)->
	gen_server:cast(ServicePid,{batch_generation_done,self()}),
	Pid ! { make_clients , done , Prefix, Done };
generate_client_batch(Prefix,Index,Left,Pid,ServicePid,State)->
	[X1,X2,X3,X4,X5,X6] = lists:flatten(string:pad(integer_to_list(Index,16),6,leading,$0)),
	Name = Prefix ++ [X1,X2,$:,X3,X4,$:,X5,X6],
	create_client(Name,State,self()),
	generate_client_batch(Prefix,Index+1,Left-1,Pid,ServicePid,State).

all_files_exist([])->
	true;
all_files_exist([H|T])->
	case filelib:is_file(H) of
		true ->
			all_files_exist(T);
		false ->
			false
	end.

