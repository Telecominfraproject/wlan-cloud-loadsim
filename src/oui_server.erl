%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 25. Oct 2020 3:45 p.m.
%%%-------------------------------------------------------------------
-module(oui_server).
-author("stephb").

-compile({parse_transform, lager_transform}).

-dialyzer(no_match).
-behaviour(gen_server).

-include("../include/common.hrl").
-include("../include/mqtt_definitions.hrl").

-define(OUI_LOOKUP_TABLE,oui_lookup_table).
-define(MAKER_LOOKUP_TABLE,maker_lookup_table).
-define(OUI_LOOKUP_TABLE_FILENAME,"oui_lookup_table.ets").
-define(MAKER_LOOKUP_TABLE_FILENAME,"maker_lookup_table.ets").

%% API
-export([start_link/0,creation_info/0,refresh/0,refresh/2,get_all/0,lookup_oui/1,lookup_vendor/1,
         get_an_oui/0,get_ouis/0,get_vendors/0,lookup_oui_from_mac/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(oui_server_state, { service_pid, uri,transfer_process_pid, all_ouis, all_makers,
	oui_tab_filename,oui_length,maker_length,
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
	gen_server:call(?SERVER,{refresh,self()}).

get_an_oui()->
	gen_server:call(?SERVER,get_an_oui).

get_all()->
	gen_server:call(?SERVER,get_all).

get_ouis()->
	gen_server:call(?SERVER,get_ouis).

get_vendors()->
	gen_server:call(?SERVER,get_vendors).

-spec lookup_oui(OUI:: string() | binary()) -> {ok,Vendor::binary()} | generic_error().
lookup_oui(OUI)->
	gen_server:call(?SERVER,{lookup_oui,utils:safe_binary(OUI)}).

-spec lookup_oui_from_mac(OUI:: string() | binary()) -> {ok,Vendor::binary()} | generic_error().
lookup_oui_from_mac(OUI)->
	try
		<<X1,X2,$:,X3,X4,$:,X5,X6,_/binary>> = utils:safe_binary(OUI),
		ProperOUI = list_to_binary(string:to_upper([X1,X2,X3,X4,X5,X6])),
		gen_server:call(?SERVER,{lookup_oui,ProperOUI})
	catch
		_:_ ->
			{ error, invalid_mac_address_format }
	end.

-spec lookup_vendor(Vendor::string() | binary() ) -> {ok,[ OUI::binary() ] } | generic_error().
lookup_vendor(Vendor)->
	gen_server:call(?SERVER,{lookup_vendor,utils:safe_binary(Vendor)}).

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
	{ok, State :: #oui_server_state{}} | {ok, State :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	process_flag(trap_exit, true),
	OuiUri = utils:app_env(oui_uri,?OUI_DEFAULT_DOWNLOAD_LINK),

	ok = utils:make_dir(filename:join([utils:priv_dir(),"data"])),

	OuiTabFileName = filename:join([utils:priv_dir(),"data",?OUI_LOOKUP_TABLE_FILENAME]),
	MakerTabFileName = filename:join([utils:priv_dir(),"data",?MAKER_LOOKUP_TABLE_FILENAME]),

	{ AllOuis , AllMakers } = try
		                            _=ets:file2tab(OuiTabFileName),
		                            _=ets:file2tab(MakerTabFileName),
		                            ?L_I("OUI tables restored from disk."),
																set_keys()
                            catch
															_:_ ->
																_=ets:new(?OUI_LOOKUP_TABLE,[named_table,public,ordered_set]),
																_=ets:new(?MAKER_LOOKUP_TABLE,[named_table,public,ordered_set]),
																_=file:copy(filename:join([utils:priv_dir(),"templates",?OUI_FILENAME]),latest_filename()),
																_=timer:apply_after(5000, ?MODULE, refresh, []),
																?L_I("No OUI tables on disk. Automatic refresh in 10 seconds."),
																{ [], [] }
														end,
	{ok, #oui_server_state{ uri = OuiUri ,
		oui_tab_filename = OuiTabFileName,
		maker_tab_filename = MakerTabFileName,
		transfer_process_pid = undefined,
		all_ouis =  AllOuis,
		all_makers = AllMakers,
    oui_length = length(AllOuis),
    maker_length = length(AllMakers),
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
handle_call(get_vendors, _From, State = #oui_server_state{}) ->
	{reply, {ok,State#oui_server_state.all_makers}, State};
handle_call(get_ouis, _From, State = #oui_server_state{}) ->
	{reply, {ok,State#oui_server_state.all_ouis}, State};
handle_call(get_an_oui, _From, State = #oui_server_state{}) ->
	OUI = valid_oui(State#oui_server_state.all_ouis),
	{reply, OUI, State};
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
handle_call({lookup_vendor,Vendor}, _From, State = #oui_server_state{}) ->
	Answer = try
		[{Vendor,OUIs}]=ets:lookup(?MAKER_LOOKUP_TABLE,Vendor),
		{ reply, { ok , OUIs} , State}
	catch
		_:_ ->
			{ reply, { error, unknown_vendor },State}
   end,
	Answer;

handle_call({refresh,Pid}, _From, State = #oui_server_state{}) ->
	ProcessingPid = spawn(?MODULE,refresh,[State,Pid]),
	{reply, ok, State#oui_server_state{transfer_process_pid = ProcessingPid}};
handle_call(_Request, _From, State = #oui_server_state{}) ->
	{reply, ok, State}.

%% This will generate OUIs that never include the first nibble greater than 7
valid_oui(List)->
	valid_oui(List,lists:nth(rand:uniform(length(List)),List)).
valid_oui(List,<<X,_,_,_,_,_>>) when X>$7 ->
	valid_oui(List,lists:nth(rand:uniform(length(List)),List));
valid_oui(_List,OUI)->
	OUI.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #oui_server_state{}) ->
	{noreply, NewState :: #oui_server_state{}} |
	{noreply, NewState :: #oui_server_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #oui_server_state{}}).
handle_cast({replace,AllOuis,AllMakers,_Pid}, State = #oui_server_state{}) ->
	?L_I("New OUI DB in memory updated."),
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
	?L_I("Downloading latest OUI list."),
	FileName = latest_filename(),
	case filelib:is_file(FileName) of
		false ->
			case httpc:request( State#oui_server_state.uri ) of
				{ ok , Result } ->
					Res = case Result of
						{ _Status , _Headers , Body } ->
							file:write_file( FileName, Body );
						{ _Status , Body } ->
							file:write_file( FileName, Body );
						_Id ->
							{ error , incomplete }
					end,
					?L_I("Downloaded latest OUI list."),
					Res;
				{ error , Reason } ->
					?L_IA("Failed to download latest OUI list. Reason:~p",[Reason]),
					{ error , Reason };
				Result ->
					?L_IA("Failed to download latest OUI list. Reason:~p",[Result]),
					{ error , Result }
			end;
		true ->
			ok
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
	_=file:read_line( Io ) ,
	skip_lines( Io , Lines-1 ).

filter_oui([C1,C2,$-,C3,C4,$-,C5,C6|_]) -> [C1,C2,C3,C4,C5,C6];
filter_oui([C1,C2,$:,C3,C4,$:,C5,C6|_]) -> [C1,C2,C3,C4,C5,C6].

process_record( Io , Acc ) ->
	case file:read_line( Io ) of
		{ ok , Line } ->
			[ OUI , _Hex | Company ] = string:tokens( Line , "\n\t ") ,
			find_blank( Io ) ,
			process_record( Io , maps:put( list_to_binary(filter_oui(OUI)) , list_to_binary(string:join( Company , " ")) , Acc ) );
		_ ->
			Acc
	end.

process_oui_file() ->
	%% skip the top 4 lines
	case file:open( latest_filename() , [read] ) of
		{ ok , Io } ->
			skip_lines( Io , 4 ) ,
			M = process_record( Io , maps:new() ),
			_ = file:close( Io ),
			M;
		Error ->
			Error
	end.

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
					?L_I("Replacing memory copies of OUI data."),
					create_oui_lookup_table(M),
					create_maker_lookup_table(M),
					_ = ets:tab2file(?OUI_LOOKUP_TABLE,	State#oui_server_state.oui_tab_filename),
					_ = ets:tab2file(?MAKER_LOOKUP_TABLE,	State#oui_server_state.maker_tab_filename),
					{ AllOuis , AllMakers } = set_keys(),
					file:delete(latest_filename()),
					gen_server:cast(State#oui_server_state.service_pid,{replace,AllOuis,AllMakers,self()});
				Error ->
					?L_IA("Please refresh OUI lists later.",[Error])
			end;
		{ error, Error } ->
			?L_IA("Please refresh OUI lists later.",[Error])
	end.

-spec latest_filename() -> string().
latest_filename() ->
	filename:join([utils:priv_dir(),"data",?OUI_FILENAME]).
