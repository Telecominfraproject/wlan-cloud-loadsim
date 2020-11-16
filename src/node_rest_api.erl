%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 09. Nov 2020 11:19 a.m.
%%%-------------------------------------------------------------------
-module(node_rest_api).
-author("stephb").

-behaviour(gen_server).

-include("../include/common.hrl").

%% API
-export([start_link/0,creation_info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(node_rest_api_state, {env}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
creation_info() ->
	[	#{	id => ?MODULE ,
		start => { ?MODULE , start_link, [] },
		restart => permanent,
		shutdown => 100,
		type => worker,
		modules => [?MODULE]} ].

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #node_rest_api_state{}} | {ok, State :: #node_rest_api_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	Port = application:get_env( ?OWLS_APP, rest_api_port, 9091),
	Secure = application:get_env( ?OWLS_APP, rest_api_secure, true ),
	PrivDir = code:priv_dir(?OWLS_APP),
	Dispatch = cowboy_router:compile([
	{
	'_', [
				{ "/api/v1/:restype/[:resid]" ,   node_api_rest_handler, [] },
				{ "/", cowboy_static, {priv_file, ?OWLS_APP, "www/index.html"} },
				{ "/[...]", cowboy_static, {priv_dir, ?OWLS_APP, "www" } }
			]}
		]),
	{ok, CB } = case Secure of
		true ->
			?L_I("Starting in secure mode."),
				PrivDir = code:priv_dir(?OWLS_APP),
				cowboy:start_tls(
				rest_http_listener,
				[
					{ port, Port } ,
					{cacertfile, filename:join([PrivDir,"ssl","sim_cert.pem"])},
					{certfile, filename:join([PrivDir,"ssl","server-api-cert.pem"])},
					{keyfile, filename:join([PrivDir,"ssl","server-api-key_dec.pem"])}
				],
				#{env => #{dispatch => Dispatch}} );
		false ->
			?L_I("Starting in clear mode."),
				cowboy:start_clear(
				rest_http_listener,
				[
				{ port, Port }
				],
				#{env => #{dispatch => Dispatch}} )
			end,
	{ok, #node_rest_api_state{ env = CB }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #node_rest_api_state{}) ->
	{reply, Reply :: term(), NewState :: #node_rest_api_state{}} |
	{reply, Reply :: term(), NewState :: #node_rest_api_state{}, timeout() | hibernate} |
	{noreply, NewState :: #node_rest_api_state{}} |
	{noreply, NewState :: #node_rest_api_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #node_rest_api_state{}} |
	{stop, Reason :: term(), NewState :: #node_rest_api_state{}}).
handle_call(_Request, _From, State = #node_rest_api_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #node_rest_api_state{}) ->
	{noreply, NewState :: #node_rest_api_state{}} |
	{noreply, NewState :: #node_rest_api_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_rest_api_state{}}).
handle_cast(_Request, State = #node_rest_api_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #node_rest_api_state{}) ->
	{noreply, NewState :: #node_rest_api_state{}} |
	{noreply, NewState :: #node_rest_api_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #node_rest_api_state{}}).
handle_info(_Info, State = #node_rest_api_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #node_rest_api_state{}) -> term()).
terminate(_Reason, _State = #node_rest_api_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #node_rest_api_state{},
		Extra :: term()) ->
	{ok, NewState :: #node_rest_api_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #node_rest_api_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
