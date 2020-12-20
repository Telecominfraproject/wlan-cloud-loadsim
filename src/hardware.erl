%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 23. Nov 2020 2:52 p.m.
%%%-------------------------------------------------------------------
-module(hardware).
-author("stephb").

-include("../include/common.hrl").
-include("../include/inventory.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,creation_info/0,get_by_id/1,get_by_model/1,get_by_vendor/1,get_definitions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, {global,?MODULE}).
-define(START_SERVER,{global,?MODULE}).

%% -define(SERVER, ?MODULE).
%% -define(START_SERVER,{local,?MODULE}).

-record(hardware_state, { hardware }).

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

-spec get_definitions() -> { ok , [ hardware_info() ]}.
get_definitions() ->
	gen_server:call(?SERVER,{get_hardware_definitions,self()}).

-spec get_by_id(Id::string()|binary()) -> { ok , [hardware_info()] } | generic_error().
get_by_id(Id)->
	gen_server:call(?SERVER,{get_hardware_by_id,utils:safe_binary(Id)}).

-spec get_by_model(Model::string()|binary()) -> { ok , [hardware_info()] } | generic_error().
get_by_model(Model)->
	gen_server:call(?SERVER,{get_hardware_by_model,utils:safe_binary(Model)}).

-spec get_by_vendor(Vendor::string()|binary()) -> { ok , [hardware_info()] } | generic_error().
get_by_vendor(Vendor)->
	gen_server:call(?SERVER,{get_hardware_by_vendor,utils:safe_binary(Vendor)}).

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
	{ok, State :: #hardware_state{}} | {ok, State :: #hardware_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	HardwareFileName = filename:join([utils:priv_dir(),"data","hardware.yaml"]),
	[HardwareRaw] = try
		             case filelib:is_file(HardwareFileName) of
			             true ->
				             yamerl_constr:file(HardwareFileName);
			             false->
				             TemplateFileName = filename:join([utils:priv_dir(),"templates","hardware.yaml"]),
				             yamerl_constr:file(TemplateFileName)
			           end
	             catch
								 _:_ -> [[]]
							 end,
	Devices = proplists:get_value("Devices",HardwareRaw),
	{ok, #hardware_state{ hardware = convert(Devices) }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #hardware_state{}) ->
	                 {reply, Reply :: term(), NewState :: #hardware_state{}} |
	                 {reply, Reply :: term(), NewState :: #hardware_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #hardware_state{}} |
	                 {noreply, NewState :: #hardware_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #hardware_state{}} |
	                 {stop, Reason :: term(), NewState :: #hardware_state{}}).
handle_call({get_hardware_definitions,_Pid}, _From, State = #hardware_state{}) ->
	{reply, {ok,State#hardware_state.hardware}, State};
handle_call({get_hardware_by_id,Id}, _From, State = #hardware_state{}) ->
	Res = filter_devices(State#hardware_state.hardware,id,Id),
	{reply, {ok,Res}, State};
handle_call({get_hardware_by_model,Model}, _From, State = #hardware_state{}) ->
	Res = filter_devices(State#hardware_state.hardware,model,Model),
	{reply, {ok,Res}, State};
handle_call({get_hardware_by_vendor,Vendor}, _From, State = #hardware_state{}) ->
	Res = filter_devices(State#hardware_state.hardware,vendor,Vendor),
	{reply, {ok,Res}, State};
handle_call(_Request, _From, State = #hardware_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #hardware_state{}) ->
	{noreply, NewState :: #hardware_state{}} |
	{noreply, NewState :: #hardware_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #hardware_state{}}).
handle_cast(_Request, State = #hardware_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #hardware_state{}) ->
	{noreply, NewState :: #hardware_state{}} |
	{noreply, NewState :: #hardware_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #hardware_state{}}).
handle_info(_Info, State = #hardware_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #hardware_state{}) -> term()).
terminate(_Reason, _State = #hardware_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #hardware_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #hardware_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #hardware_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
filter_devices( Hardware, id, Value )->
	[ X || X <- Hardware, X#hardware_info.id == Value ];
filter_devices( Hardware, model, Value )->
	[ X || X <- Hardware, X#hardware_info.model == Value ];
filter_devices( Hardware, vendor, Value )->
	[ X || X <- Hardware, X#hardware_info.vendor == Value ].

convert(Hardware)->
	convert(Hardware,[]).

convert([],Result)->
	lists:reverse(Result);
convert([H|T],Result)->
	Entry=convert_entry(H),
	convert(T,[Entry|Result]).

convert_entry(Entry)->
	convert_entry(Entry,#hardware_info{}).

convert_entry([],R)->
	R;
convert_entry([{"Id",Value}|Tail],R)->
	convert_entry( Tail, R#hardware_info{ id = list_to_binary(Value)} );
convert_entry([{"Description",Value}|Tail],R)->
	convert_entry(Tail, R#hardware_info{ description = list_to_binary(Value)});
convert_entry([{"Vendor",Value}|Tail],R)->
	convert_entry(Tail, R#hardware_info{ vendor = list_to_binary(Value)});
convert_entry([{"Model",Value}|Tail],R)->
	convert_entry(Tail,R#hardware_info{model=list_to_binary(Value)});
convert_entry([{"Firmware",Value}|Tail],R)->
	convert_entry(Tail,R#hardware_info{firmware=list_to_binary(Value)});
convert_entry([{"Capabilities",Value}|Tail],R)->
	convert_entry(Tail,R#hardware_info{capabilities = convert_list(Value,[])});
convert_entry([{"Bands",Value}|Tail],R)->
	convert_entry(Tail,R#hardware_info{bands = convert_list(Value,[])});
convert_entry([_|Tail],R)->
	convert_entry(Tail,R).

convert_list([],R)->
	R;
convert_list(["mqtt_client" | Tail], R)->
	convert_list(Tail,[mqtt_client|R]);
convert_list(["ovsdb_client" | Tail], R)->
	convert_list(Tail,[ovsdb_client|R]);
convert_list(["BAND2G" | Tail], R)->
	convert_list(Tail,['BAND2G'|R]);
convert_list(["BAND5GL" | Tail], R)->
	convert_list(Tail,['BAND5GL'|R]);
convert_list(["BAND5GU" | Tail], R)->
	convert_list(Tail,['BAND5GU'|R]);
convert_list([ _ | Tail], R)->
	convert_list(Tail,R).

%% hardware_to_json(HW) ->


