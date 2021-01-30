%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2021, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2021 10:18 a.m.
%%%-------------------------------------------------------------------
-module(web_token_manager).
-author("stephb").

-behaviour(gen_server).
-include("../include/common.hrl").

%% API
-export([start_link/0,creation_info/0,login/2,logout/1,valid/1,acl/1,user/1,validate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% cowboy callbacks
-export([ init/2,allowed_methods/2,is_authorized/2]).
-export([ content_types_provided/2, content_types_accepted/2,options/2 ]).
-export([ db_to_json/2 , json_to_db/2 ,resource_exists/2,delete_resource/2]).

-define(HTTP_GET,<<"GET">>).
-define(HTTP_POST,<<"POST">>).
-define(HTTP_PUT,<<"PUT">>).
-define(HTTP_DELETE,<<"DELETE">>).
-define(HTTP_OPTIONS,<<"OPTIONS">>).
-define(HTTP_HEAD,<<"HEAD">>).

-record(request_state,{
	token = <<>>  :: binary(),
	method = <<>> :: binary(),
	time_in = 0 :: non_neg_integer() }).

-define(SERVER, ?MODULE).

-record(web_acl,{
	delete = true :: boolean(),
	read = true :: boolean(),
	portalLogin = true :: boolean(),
	readWrite = true :: boolean(),
	readWriteCreate = true :: boolean()
}).

-record(web_token,{
	created = 0 :: non_neg_integer(),
	last_access = 0 :: non_neg_integer(),
	access_token = <<>> :: binary(),
	refresh_token = <<>> :: binary(),
	token_type = <<"Bearer">> :: binary(),
	expires_in = 3000 :: non_neg_integer(),
	idle_timeout = 3600 :: non_neg_integer(),
	aclTemplate = #web_acl{} :: #web_acl{}
}).

-record(web_token_manager_state, { tokens, users , validation_module, validation_function }).

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

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec login(UserName::binary()|string(),Password::binary()|string()) -> {ok,#{}} | generic_error().
login(UserName,Password)->
	gen_server:call(?SERVER,{login,utils:safe_binary(UserName),utils:safe_binary(Password)}).

-spec logout(Token::binary()|string()) -> ok.
logout(Token)->
	gen_server:call(?SERVER,{logout,utils:safe_binary(Token)}).

-spec valid(Token::binary()|string()) -> boolean().
valid(Token) ->
	gen_server:call(?SERVER,{valid,utils:safe_binary(Token)}).

-spec acl(Token::binary()|string()) -> {ok,#{}} | generic_error().
acl(Token) ->
	gen_server:call(?SERVER,{acl,utils:safe_binary(Token)}).

-spec user(Token::binary()|string()) -> {ok,#{}} | generic_error().
user(Token) ->
	gen_server:call(?SERVER,{user,utils:safe_binary(Token)}).

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
	{ok, State :: #web_token_manager_state{}} | {ok, State :: #web_token_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{VM,VF} = application:get_env(?MODULE,auth,{?MODULE,validate}),
	{M,F} = case erlang:function_exported(VM,VF,2) of
						true -> {VM,VF};
						false-> {?MODULE,validate}
					end,
	{ok, #web_token_manager_state{ tokens = #{}, users = #{}, validation_module = M, validation_function = F }}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #web_token_manager_state{}) ->
	                 {reply, Reply :: term(), NewState :: #web_token_manager_state{}} |
	                 {reply, Reply :: term(), NewState :: #web_token_manager_state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #web_token_manager_state{}} |
	                 {noreply, NewState :: #web_token_manager_state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #web_token_manager_state{}} |
	                 {stop, Reason :: term(), NewState :: #web_token_manager_state{}}).
handle_call({login,UserName,Password}, _From, State = #web_token_manager_state{}) ->
	case apply(State#web_token_manager_state.validation_module,
	           State#web_token_manager_state.validation_function,[UserName,Password]) of
		true ->
			NewToken = generate(),
			NewRecord = #web_token{
				created = os:system_time(),
				last_access = os:system_time(),
				access_token = NewToken,
				refresh_token = generate(),
				aclTemplate = #web_acl{}
			},
			WebTokenAttributes = to_map(NewRecord),
			{reply,{ok,WebTokenAttributes},State#web_token_manager_state{
				tokens = maps:put(NewToken,NewRecord,State#web_token_manager_state.tokens),
				users = maps:put(NewToken,UserName,State#web_token_manager_state.users)}};
		false ->
			{reply,{error,unknown_credentials},State}
	end;
handle_call({logout,Token}, _From, State = #web_token_manager_state{}) ->
	{reply, ok, State#web_token_manager_state{
		tokens = maps:remove(Token,State#web_token_manager_state.tokens),
		users = maps:remove(Token,State#web_token_manager_state.users)}};
handle_call({user,Token}, _From, State = #web_token_manager_state{}) ->
	case maps:get(Token,State#web_token_manager_state.users,undefined) of
		undefined ->
			{reply,{error,unknown_token},State};
		UserName ->
			{reply,{ok,UserName},State}
	end;
handle_call({valid,Token}, _From, State = #web_token_manager_state{}) ->
	case maps:get(Token,State#web_token_manager_state.tokens,undefined) of
		undefined ->
			{reply,false,State};
		Record ->
			NewRecord = Record#web_token{ last_access = os:system_time() },
			{reply,true,State#web_token_manager_state{tokens = maps:put(Token,NewRecord,State#web_token_manager_state.tokens)}}
	end;
handle_call({acl,Token}, _From, State = #web_token_manager_state{}) ->
	case maps:get(Token,State#web_token_manager_state.tokens,undefined) of
		undefined ->
			{ok,{error,unknown_token},State};
		Record ->
			{ok,{ok,to_map(Record#web_token.aclTemplate)},State}
	end;
handle_call(_Request, _From, State = #web_token_manager_state{}) ->
	{reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #web_token_manager_state{}) ->
	{noreply, NewState :: #web_token_manager_state{}} |
	{noreply, NewState :: #web_token_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #web_token_manager_state{}}).
handle_cast(_Request, State = #web_token_manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #web_token_manager_state{}) ->
	{noreply, NewState :: #web_token_manager_state{}} |
	{noreply, NewState :: #web_token_manager_state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #web_token_manager_state{}}).
handle_info(_Info, State = #web_token_manager_state{}) ->
	{noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #web_token_manager_state{}) -> term()).
terminate(_Reason, _State = #web_token_manager_state{}) ->
	ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #web_token_manager_state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #web_token_manager_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #web_token_manager_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Cowboy Handler
%%%===================================================================
init(Req, _State) ->
	Method = cowboy_req:method(Req),
	Token = cowboy_req:binding(token, Req, <<>>),
	{ cowboy_rest,
	  restutils:add_CORS(Req),
	  #request_state{
			method = Method,
			token = Token,
			time_in = os:system_time() }}.

allowed_methods(Req, State) ->
	{[?HTTP_OPTIONS,?HTTP_POST,?HTTP_DELETE], Req, State}.

content_types_provided(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, db_to_json}], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, json_to_db}], Req, State}.

is_authorized(Req,#request_state{ method = <<"OPTIONS">> }=State)->
	{true,Req,State};
is_authorized(Req, State) ->
	{true,Req,State}.

resource_exists(Req,#request_state{ method = ?HTTP_POST }=State) ->
	{true,Req,State};
resource_exists(Req,#request_state{ method = ?HTTP_DELETE, token = <<>>}=State)->
	{false,Req,State};
resource_exists(Req,#request_state{ method = ?HTTP_DELETE }=State)->
	{web_token_manager:valid(State#request_state.token),Req,State};
resource_exists(Req,State)->
	{false,Req,State }.

delete_resource(Req,State)->
	try
		web_token_manager:logout(State#request_state.token)
	catch
		_:_ ->
			ok
	end,
	{true,Req,State}.

options(Req0, State) ->
	Req1 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, DELETE, OPTIONS, POST">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"Pragma">>, <<"no-cache">>, Req1),
	Req3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Credentials">>, <<"true">>, Req2),
	Req4 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"*">>, Req3),
	{ok,Req4,State}.

json_to_db(Req, State) ->
	do( State#request_state.method , Req , State ).

db_to_json(Req, State) ->
	do( State#request_state.method , Req , State ).

%%%===================================================================
%%% CAs Management
%%%===================================================================
do( ?HTTP_POST,Req,#request_state{}=State)->
	try
		{ok,RawData,Req1} = cowboy_req:read_body(Req),
		ReqFields = jsx:decode(RawData,[return_maps]),    %% do not use jiffy here...
		#{ <<"userId">> := UserId, <<"password">> := Password } = ReqFields,
		case web_token_manager:login(UserId,Password) of
			{ok,TokenAttributes} ->
				JSON = jiffy:encode(TokenAttributes),
				Req2 = cowboy_req:set_resp_body(JSON,Req1),
				{true, Req2,State};
			{error,_Reason} ->
				create_error(102,"Some fields are invalid or missing.",Req1,State)
		end
	catch
		_:_ ->
			create_error(103,"Some fields are invalid or missing.",Req,State)
	end;

do( _Verb,Req,State)->
	create_error(102,"Some fields are invalid or missing.",Req,State).

create_error(Error,Reason,Req,#request_state{ method = ?HTTP_GET} = State ) ->
	{restutils:generate_error(Error,Reason), Req,State};
create_error(Error,Reason,Req,#request_state{ method = ?HTTP_POST} = State ) ->
	Req1 = cowboy_req:set_resp_body(restutils:generate_error(Error,Reason), Req),
	{false,Req1,State}.

to_map(R) when is_record(R,web_acl) ->
	#{ 'Delete' => R#web_acl.delete ,
     'Read' => R#web_acl.read,
     'PortalLogin' => R#web_acl.portalLogin,
     'ReadWrite' => R#web_acl.readWrite,
     'ReadWriteCreate' => R#web_acl.readWriteCreate };
to_map(R) when is_record(R,web_token) ->
	#{ access_token => R#web_token.access_token ,
		refresh_token => R#web_token.refresh_token,
		token_type => R#web_token.token_type,
		expires_in => R#web_token.expires_in,
		idle_timeout => R#web_token.idle_timeout,
		aclTemplate => to_map(R#web_token.aclTemplate) }.

-define(TOKEN_LENGTH, 256).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc Generates a random OAuth2 token.
-spec generate() -> binary().
generate() -> generate_fragment(?TOKEN_LENGTH).

%%%_* Private functions ================================================
-spec generate_fragment(integer()) -> binary().
generate_fragment(0) -> <<>>;
generate_fragment(N) ->
	Rand = base64:encode(crypto:strong_rand_bytes(N)),
	Frag = << <<C>> || <<C>> <= <<Rand:N/bytes>>, is_alphanum(C) >>,
	<<Frag/binary, (generate_fragment(N - byte_size(Frag)))/binary>>.

%% @doc Returns true for alphanumeric ASCII characters, false for all others.
-spec is_alphanum(char()) -> boolean().
is_alphanum(C) when C >= 16#30 andalso C =< 16#39 -> true;
is_alphanum(C) when C >= 16#41 andalso C =< 16#5A -> true;
is_alphanum(C) when C >= 16#61 andalso C =< 16#7A -> true;
is_alphanum(_)                                    -> false.

-spec validate(UserName::binary(),Password::binary()) -> boolean().
validate(UserName,Password) ->
	try
		{ok,[AuthTable]} = file:consult(filename:join([utils:priv_dir(),"templates","default_auth.txt"])),
		case lists:keysearch(binary_to_list(UserName),1,AuthTable) of
			false ->
				false;
			{ _Username, {_U,Pwd} } ->
				list_to_binary(Pwd) == Password
		end
	catch
		_:_ ->
			false
	end.
