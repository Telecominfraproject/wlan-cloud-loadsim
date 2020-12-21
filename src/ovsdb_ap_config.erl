%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 24. November 2020 @ 13:55:04
%%%-----------------------------------------------------------------------------
-module(ovsdb_ap_config).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_ap_tables.hrl").
-include("../include/inventory.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	ca_name   = <<>> :: string() | binary(),
	redirector= <<>> :: binary(),
	serial    = <<>> :: binary(),
	id        = <<>> :: binary(),
	store_ref :: ets:tid(),
	cacert    = <<>> :: binary(),		% pem file (in memory) of the server certificate chain
	cert      = <<>> :: binary(),
	key       = {none,<<>>} :: {atom(), binary()}% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.
-export_type([cfg/0]).


-export([new/4,configure/1]).
-export ([id/1,ca_certs/1,client_cert/1,client_key/1,tip_redirector/2,tip_manager/2,caname/1,serial/1]).


%%------------------------------------------------------------------------------
%% API


-spec new (CAName :: string() | binary(), Id :: binary(), Store :: ets:tid(), Redirector :: binary()) -> Config :: cfg().
new (CAName,Id,Store,Redirector) ->
	#cfg{ca_name=CAName, id=Id, store_ref = Store, redirector=Redirector}.

-spec configure (Config :: cfg()) -> NewConfig :: cfg().
configure (#cfg{ca_name=CAName, id=ID, redirector=R}=Config) ->
	{ok,Info} = inventory:get_client(CAName,ID),
	SSIDs = #{'BAND2G' => <<"TipWlan-cloud-wifi">>,
			  'BAND5G' => <<"TipWlan-cloud-wifi">>,
			  'BAND5GL' => <<"Maverick">>,
			  'BAND5GU' => <<"Maverick">>},
	WifiClients = lists:flatten(Info#client_info.wifi_clients),
	HW = case hardware:get_by_id(Info#client_info.id) of
		{ok,[HardwareInfo]} ->
			HardwareInfo;
		_ ->
			#hardware_info{}
	end,

	APC = [
		{serial,Info#client_info.serial},
		{type,Info#client_info.type},
		{wan_addr,make_ip_addr(ID)},
		{wan_mac,Info#client_info.wan_mac0},
		{lan_addr,<<"192.168.1.1">>},
		{lan_mac,Info#client_info.lan_mac0},
		{tip_redirector,R},
		{wifi_clients,WifiClients},
		{name,Info#client_info.name},
		{bands,Info#client_info.bands},
		{hardware,HW},
		{ssids,SSIDs}
	],
	APC2 = validate_config(APC),
	ovsdb_ap_simop:create_ap_node(APC2,Config#cfg.store_ref),
	ovsdb_ap_simop:create_radios(APC2,Config#cfg.store_ref),
	ovsdb_ap_simop:create_clients(APC2,Config#cfg.store_ref),
	Config#cfg{
		cacert = Info#client_info.cacert,
		serial = Info#client_info.serial,
		cert = Info#client_info.cert,
		key  = Info#client_info.key
	}.

-spec validate_config(APC :: [{atom(),term()}]) -> CorrAPC :: [{atom(),term()}].
validate_config (APC) ->
	File = filename:join([utils:priv_dir(),"templates","default_ap.cfg"]),
	{ok, [Defaults]} = file:consult(File),
	F = fun({K,V}) ->
		case V of 
			<<"">> ->
				{K,proplists:get_value(K,Defaults)};
			_ ->
				{K,V}
		end
	end,
	[F(X)||X<-APC].


-spec make_ip_addr(ID::binary()) -> IPAddr :: binary().
make_ip_addr(_ID) ->
	A = rand:uniform(30) + 60,
	B = rand:uniform(200) + 20,
	C = rand:uniform(50) + 100,
	D = rand:uniform(230) + 10,
	list_to_binary(io_lib:format("~B.~B.~B.~B",[A,B,C,D])).

	
%%------------------------------------------------------------------------------
%% accessor API - direct config settings

-spec id (Config :: cfg()) -> Id :: binary().
id(Cfg) ->
	Cfg#cfg.id.

-spec caname (Config :: cfg()) -> CAName :: binary().
caname(Cfg) ->
	Cfg#cfg.ca_name.

-spec serial (Config :: cfg()) -> Serial :: binary().
serial(Cfg) ->
	Cfg#cfg.serial.

-spec ca_certs (Config :: cfg()) -> binary().
ca_certs (Cfg) ->
	Cfg#cfg.cacert.

-spec client_cert (Config :: cfg()) -> binary().
client_cert (Cfg) ->
	Cfg#cfg.cert.

-spec client_key (Config :: cfg()) -> {atom(),binary()}.
client_key (Cfg) ->
	Cfg#cfg.key.


%%------------------------------------------------------------------------------
%% accessor API from Store tables

-spec tip_redirector (Part :: host | port, Config :: cfg()) -> string() | integer().
tip_redirector (Part,#cfg{store_ref=Store}) ->
	[#'AWLAN_Node'{redirector_addr=R}|_] = ets:lookup(Store,'AWLAN_Node'),
	get_host_or_port(Part,R).

-spec tip_manager (Part :: host | port, Config :: cfg()) -> string() | integer().
tip_manager (Part,#cfg{store_ref=Store}) ->
	[#'AWLAN_Node'{manager_addr=R}|_] = ets:lookup(Store,'AWLAN_Node'),
	get_host_or_port(Part,R).

-spec get_host_or_port (Part :: host | port, Addr :: binary()) -> string() | integer().
get_host_or_port (Part, Addr) when is_binary(Addr) ->
	Parts = string:split(Addr,":",all),
	case Part of
		host -> case Parts of
					[_,H,_] -> binary_to_list(H);
					[H,_]   -> binary_to_list(H);
						  _ -> ""
				end;
		port -> case Parts of
					[_,_,P] -> binary_to_integer(P);
					[_,P]   -> binary_to_integer(P);
						  _ -> 0
				end
	end.
	

