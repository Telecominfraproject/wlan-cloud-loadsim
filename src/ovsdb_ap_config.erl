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
-include("ovsdb_ap_tables.hrl").

%%------------------------------------------------------------------------------
%% types and specifications

-record (cfg, {
	id :: string(),
	store_ref :: ets:tid(),
	ca_certs = <<"">> :: binary(),		% pem file (in memory) of the server certificate chain
	client_cert = <<"">> :: binary()	% client certificate + private key in pem format
}).

-opaque cfg() :: #cfg{}.


-export_type([cfg/0]).


-export([new/2,configure/2]).
-export ([id/1,ca_certs/1,client_cert/1,tip_redirector/2,tip_manager/2]).


%%------------------------------------------------------------------------------
%% API


-spec new (Id :: string(), Store :: ets:tid()) -> Config :: cfg().

new (Id,Store) ->
	#cfg{id=Id, store_ref = Store}.


-spec configure (Manager :: tuple(), Config :: cfg()) -> NewConfig :: cfg().

configure (_Manager,Config) ->
	%% @TODO: remote procisioned configuration
	%% in the meantime read a sample config from a file
	File = filename:join([code:priv_dir(?OWLS_APP),"ovsdb","test_ap.cfg"]),
	{ok, [M]} = file:consult(File),
	APC = maps:get(Config#cfg.id,M),
	initialize_ap_store(Config#cfg.store_ref,APC),
	Config#cfg{
		ca_certs = proplists:get_value(ca_certs,APC),
		client_cert = proplists:get_value(client_cert,APC)
	}.



-spec initialize_ap_store (Store :: ets:tid(), APConfig :: proplists:proplist()) -> ok.

initialize_ap_store (Store, APC) ->
	ets:insert(Store,{'AWLAN_Node_seq',1}),
	Node = #'AWLAN_Node'{
		row_idx = 0,
		% data = #{
		% 	redirector_addr => list_to_binary([proplists:get_value(tip_host,APC),":",
		% 								       integer_to_list(proplists:get_value(tip_port,APC))]),
		% 	serial_number => proplists:get_value(serial,APC),
		% 	model => proplists:get_value(type,APC)
		% }
		redirector_addr = list_to_binary([proplists:get_value(tip_host,APC),":",
										 integer_to_list(proplists:get_value(tip_port,APC))]),
		serial_number = proplists:get_value(serial,APC),
		model = proplists:get_value(type,APC)
	},
	ets:insert(Store,Node),
	ok.



%%------------------------------------------------------------------------------
%% accessor API - direct config settings

-spec id (Config :: cfg()) -> Id :: string().
id (Cfg) -> Cfg#cfg.id.

-spec ca_certs (Config :: cfg()) -> binary().
ca_certs (Cfg) -> Cfg#cfg.ca_certs.

-spec client_cert (Config :: cfg()) -> binary().
client_cert (Cfg) -> Cfg#cfg.client_cert.


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

