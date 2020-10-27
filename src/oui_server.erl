%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2020 3:45 p.m.
%%%-------------------------------------------------------------------
-module(oui_server).
-author("stephb").

-behaviour(gen_server).

-include("../include/mqtt_definitions.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(OUI_LOOKUP_TABLE,oui_lookup_table).
-define(MAKER_LOOKUP_TABLE,maker_lookup_table).
-define(OUI_LOOKUP_TABLE_FILENAME,"oui_lookup_table.ets").
-define(MAKER_LOOKUP_TABLE_FILENAME,"maker_lookup_table.ets").

%% API
-export([start_link/0,creation_info/0,refresh/0,refresh/2,get_all/0,lookup_oui/1,lookup_maker/1,get_ouis/0,get_makers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(oui_server_state, { service_pid, uri,transfer_process_pid, all_ouis, all_makers,
	oui_tab_filename,
	maker_tab_filename }).

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

refresh() ->
	gen_server:call(?MODULE,{refresh,self()}).

get_all()->
	gen_server:call(?MODULE,get_all).

get_ouis()->
	gen_server:call(?MODULE,get_ouis).

get_makers()->
	gen_server:call(?MODULE,get_makers).

lookup_oui(Id)->
	gen_server:call(?MODULE,{lookup_oui,Id}).

lookup_maker(Id)->
	gen_server:call(?MODULE,{lookup_maker,Id}).

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
	{ok, State :: #oui_server_state{}} | {ok, State :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	OuiUri = application:get_env(?MQTT_APP,oui_uri,?OUI_DEFAULT_DOWNLOAD_LINK),

	OuiTabFileName = filename:join([code:priv_dir(?MQTT_APP),"data",?OUI_LOOKUP_TABLE_FILENAME]),
	MakerTabFileName = filename:join([code:priv_dir(?MQTT_APP),"data",?MAKER_LOOKUP_TABLE_FILENAME]),

	{ AllOuis , AllMakers } = try
		                            ets:file2tab(OuiTabFileName),
		                            ets:file2tab(MakerTabFileName),
		                            lager:info("OUI tables restored from disk."),
																set_keys()
                            catch
															_:_ ->
																lager:info("No OUI tables on disk."),
																ets:new(?OUI_LOOKUP_TABLE,[named_table,public,ordered_set]),
																ets:new(?MAKER_LOOKUP_TABLE,[named_table,public,ordered_set]),
																{ [], [] }
														end,
	{ok, #oui_server_state{ uri = OuiUri ,
		oui_tab_filename = OuiTabFileName,
		maker_tab_filename = MakerTabFileName,
		transfer_process_pid = undefined,
		all_ouis =  AllOuis,
		all_makers = AllMakers,
		service_pid = self() }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #oui_server_state{}) ->
	{reply, Reply :: term(), NewState :: #oui_server_state{}} |
	{reply, Reply :: term(), NewState :: #oui_server_state{}, timeout() | hibernate} |
	{noreply, NewState :: #oui_server_state{}} |
	{noreply, NewState :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #oui_server_state{}} |
	{stop, Reason :: term(), NewState :: #oui_server_state{}}).
handle_call(get_makers, _From, State = #oui_server_state{}) ->
	{reply, {ok,State#oui_server_state.all_makers}, State};
handle_call(get_ouis, _From, State = #oui_server_state{}) ->
	{reply, {ok,State#oui_server_state.all_ouis}, State};
handle_call(get_all, _From, State = #oui_server_state{}) ->
	{reply, {ok,State#oui_server_state.all_ouis,State#oui_server_state.all_makers}, State};
handle_call({lookup_oui,OUI}, _From, State = #oui_server_state{}) ->
	Answer = try
	    [ {_ , Maker} ] = ets:lookup(?OUI_LOOKUP_TABLE,OUI),
	    { reply, { ok , Maker }, State}
	catch
	    _:_  ->
		    { reply , { error , "OUI not found" },State}
	end,
	Answer;
handle_call({lookup_maker,Maker}, _From, State = #oui_server_state{}) ->
	Answer = try
		[{Maker,OUIs}]=ets:lookup(?MAKER_LOOKUP_TABLE,Maker),
		{ reply, { ok , OUIs} , State}
	catch
		_:_ ->
			{ reply, { error, "Maker not found."},State}
   end,
	Answer;

handle_call({refresh,Pid}, _From, State = #oui_server_state{}) ->
	ProcessingPid = spawn(?MODULE,refresh,[State,Pid]),
	{reply, ok, State#oui_server_state{transfer_process_pid = ProcessingPid}};
handle_call(_Request, _From, State = #oui_server_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #oui_server_state{}) ->
	{noreply, NewState :: #oui_server_state{}} |
	{noreply, NewState :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #oui_server_state{}}).
handle_cast({replace,AllOuis,AllMakers,_Pid}, State = #oui_server_state{}) ->
	lager:info("New OUI DB in memory updated."),
	{noreply, State#oui_server_state{ all_ouis = AllOuis, all_makers = AllMakers }};
handle_cast(_Request, State = #oui_server_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #oui_server_state{}) ->
	{noreply, NewState :: #oui_server_state{}} |
	{noreply, NewState :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #oui_server_state{}}).
handle_info({'DOWN', Ref, process, Pid2, Reason},State)->
	io:format("DOWN message: ~p ~p ~p~n",[Ref,Pid2,Reason]),
	{noreply,State};
handle_info(_Info, State = #oui_server_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #oui_server_state{}) -> term()).
terminate(_Reason, _State = #oui_server_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #oui_server_state{},
		Extra :: term()) ->
	{ok, NewState :: #oui_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #oui_server_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_latest_oui(State) ->
	lager:info("Downloading latest OUI list."),
	FileName = latest_filename(),
	case httpc:request( State#oui_server_state.uri ) of
		{ ok , Result } ->
			Res = case Result of
				{ _Status , _Headers , Body } ->
					file:write_file( FileName, Body ),
					ok;
				{ _Status , Body } ->
					file:write_file( FileName, Body ),
					ok;
				_Id ->
					{ error , incomplete }
			end,
			lager:info("Downloaded latest OUI list."),
			Res;
		{ error , Reason } ->
			lager:info("Failed to download latest OUI list. Reason:~p",[Reason]),
			{ error , Reason };
		Result ->
			lager:info("Failed to download latest OUI list. Reason:~p",[Result]),
			{ error , Result }
	end.

find_blank( Io ) ->
	case file:read_line( Io ) of
		{ ok , Line } ->
			case string:len( Line ) of
				1 -> ok ;
				_ -> find_blank( Io )
			end;
		_ ->
			ok
	end.

skip_lines( _Io , 0 ) ->
	ok;

skip_lines( Io , Lines ) ->
	file:read_line( Io ) ,
	skip_lines( Io , Lines-1 ).

filter_oui([C1,C2,$-,C3,C4,$-,C5,C6|_]) -> [C1,C2,C3,C4,C5,C6];
filter_oui([C1,C2,$:,C3,C4,$:,C5,C6|_]) -> [C1,C2,C3,C4,C5,C6].

process_record( Io , Acc ) ->
	case file:read_line( Io ) of
		{ ok , Line } ->
			[ OUI , _Hex | Company ] = string:tokens( Line , "\n\t ") ,
			find_blank( Io ) ,
			process_record( Io , maps:put( filter_oui(OUI) , string:join( Company , " ") , Acc ) );
		_ ->
			Acc
	end.

process_oui_file() ->
	%% skip the top 4 lines
	{ ok , Io } = file:open( latest_filename() , read ) ,
	skip_lines( Io , 4 ) ,
	M = process_record( Io , maps:new() ),
	file:close( Io ),
	M.

create_oui_lookup_table(M)->
	ets:delete_all_objects(?OUI_LOOKUP_TABLE),
	ListOfOuis = maps:to_list(M),
	ets:insert_new(?OUI_LOOKUP_TABLE,ListOfOuis).

create_maker_lookup_table(M)->
	ets:delete_all_objects(?MAKER_LOOKUP_TABLE),
	CrossMap = maps:fold(fun(K,V,A) ->
													case maps:get(V,A,undefined) of
														undefined ->
															maps:put(V,[K],A);
														CV ->
															maps:put(V,lists:sort(CV ++ [K]),A)
													end
												end, maps:new(),M),
	ListOfMakers = maps:to_list(CrossMap),
	ets:insert_new(?MAKER_LOOKUP_TABLE,ListOfMakers).

set_keys()->
	{ ets:foldr(fun({Oui,_L},List) -> [Oui|List] end, [], ?OUI_LOOKUP_TABLE),
		ets:foldr(fun({Maker,_L},List) -> [Maker|List] end, [], ?MAKER_LOOKUP_TABLE)}.


refresh(State,_Pid) ->
	case get_latest_oui(State) of
		ok ->
			case process_oui_file() of
				M when is_map(M) ->
					lager:info("Replacing memory copies of OUI data."),
					create_oui_lookup_table(M),
					create_maker_lookup_table(M),
					ets:tab2file(?OUI_LOOKUP_TABLE,	State#oui_server_state.oui_tab_filename),
					ets:tab2file(?MAKER_LOOKUP_TABLE,	State#oui_server_state.maker_tab_filename),
					{ AllOuis , AllMakers } = set_keys(),
					gen_server:cast(State#oui_server_state.service_pid,{replace,AllOuis,AllMakers,self()});
				Error ->
					lager:info("Please refresh OUI lists later.",[Error])
			end;
		{ error, Error } ->
			lager:info("Please refresh OUI lists later.",[Error])
	end.

latest_filename() ->
	filename:join([code:priv_dir(?MQTT_APP),"data",?OUI_FILENAME]).
