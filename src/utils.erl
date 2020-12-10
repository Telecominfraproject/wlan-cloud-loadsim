%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2020 9:47 a.m.
%%%-------------------------------------------------------------------
-module(utils).
-author("stephb").

-include("../include/common.hrl").

%% API
-export([ make_dir/1,uuid/0,get_addr/0,get_addr2/0,app_name/0,app_name/1,priv_dir/0,app_env/2,to_string_list/2,to_binary_list/2,print_nodes_info/1,
					do/2,pem_to_cert/1,pem_to_key/1,safe_binary/1,uuid_b/0,pem_key_is_encrypted/1,remove_pem_key_password/3,
					noop/0,noop_mfa/0,split_into/2,select/3,adjust/2,
					get_avg/1, new_avg/0,compute_avg/2,search_replace/3]).

-type average() :: { CurrentValue::number(), HowManyValues::integer(), PastValues::[number()]}.
-export_type([average/0]).

-spec make_dir( DirName::string() ) -> ok | { error, atom() }.
make_dir(DirName)->
	case file:make_dir(DirName) of
		ok -> ok;
		{error,eexist} -> ok;
		Error -> Error
	end.

-spec uuid()->string().
uuid()->
	uuid:uuid_to_string(uuid:get_v4()).

-spec uuid_b()->binary().
uuid_b()->
	list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

-spec get_addr() -> IpAddress::string().
get_addr()->
	{ok,Ifs} = inet:getifaddrs(),
	case strip_ifs(Ifs) of
		none -> "0.0.0.0";
		[A|_] -> inet:ntoa(A)
	end.

-spec get_addr2() -> IpAddress::string().
get_addr2()->
	Node=atom_to_list(node()),
	[_,Host]=string:tokens(Node,"@"),
	case inet:gethostbyname(Host) of
		{ok,{hostent,_Host2,_,inet,4,[Address1|_]}} ->
			inet:ntoa(Address1);
		Error->
			Error
	end.

-spec app_name( AppName::atom() )->ok.
app_name(AppName)->
	persistent_term:put(running_app,AppName),
	persistent_term:put(priv_dir,code:priv_dir(AppName)).

-spec app_name()->AppName::atom().
app_name()->
	persistent_term:get(running_app).

-spec priv_dir()->DirName::string().
priv_dir()->
	persistent_term:get(priv_dir).

-spec app_env(Key::atom(),Default::term())->Value::term().
app_env(Key,Default)->
	application:get_env(app_name(),Key,Default).


print_nodes_info(Nodes)->
	io:format("---------------------------------------------------------------------------------------------~n"),
	io:format("|Node name                             | Total       | Allocated   | Biggest      |  Procs  |~n"),
	io:format("|--------------------------------------|-------------|-------------|-------------------------~n"),
	print_line(Nodes),
	io:format("---------------------------------------------------------------------------------------------~n").

print_line([])->
	ok;
print_line([H|T])->
	NodeInfo = node_info(H),
	#{ total := Total , allocated := Allocated , worst := Worst , processes := Processes } = NodeInfo,
	io:format("|~37s |~9.2f MB |~9.2f MB | ~9.2f MB | ~7b |~n",[atom_to_list(H),Total,Allocated,Worst,Processes]),
	print_line(T).

node_info(Node)->
	try
		{Total,Allocated,{ _Pid, Worst}}=rpc:call(Node,memsup,get_memory_data,[]),
		Processes = rpc:call(Node,cpu_sup,nprocs,[]),
		#{ total => Total/(1 bsl 20), allocated => Allocated/(1 bsl 20), worst => Worst/(1 bsl 20), processes => Processes }
	catch
		_:_ ->
			#{ total => 0, allocated => 0, worst => 0, processes => 0 }
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
strip_ifs(Ifs)->
	strip_ifs(Ifs,[]).

strip_ifs([],[])->
	none;
strip_ifs([],GoodAddresses)->
	lists:reverse(GoodAddresses);
strip_ifs([{_IfName,Ifprops}|Tail],Addrs)->
	Ifs=proplists:lookup_all(addr,Ifprops),
	case Ifs of
		[] ->
			strip_ifs(Tail,Addrs);
		Ifs ->
			case good_address(Ifs) of
				none ->
					strip_ifs(Tail,Addrs);
				Addr->
					strip_ifs(Tail,[Addr|Addrs])
			end
	end.

good_address([])->
	none;
good_address([{addr,{127,_,_,_}}|T]) ->
	good_address(T);
good_address([{addr,{A,B,C,D}}|_Tail]) when A=/=127 ->
	{A,B,C,D};
good_address([_|T])->
	good_address(T).

-spec to_string_list([term()],[term()])->[string()].
to_string_list([],R)->
	lists:reverse(R);
to_string_list([H|T],R) when is_list(H)->
	to_string_list(T,[H|R]);
to_string_list([H|T],R) when is_atom(H)->
	to_string_list(T,[atom_to_list(H)|R]);
to_string_list([H|T],R) when is_binary(H)->
	to_string_list(T,[binary_to_list(H)|R]).

-spec to_binary_list([term()],[term()])->[string()].
to_binary_list([],R)->
	lists:reverse(R);
to_binary_list([H|T],R) when is_list(H)->
	to_binary_list(T,[list_to_binary(H)|R]);
to_binary_list([H|T],R) when is_atom(H)->
	to_binary_list(T,[list_to_binary(atom_to_list(H))|R]);
to_binary_list([H|T],R) when is_binary(H)->
	to_binary_list(T,[H|R]).

-spec do(boolean(),{atom(),atom(),term()})->ok.
do(true,{M,F,A})->
	_=apply(M,F,A), ok;
do(false,_)->
	ok.

pem_to_cert(FileName) when is_binary(FileName)->
	pem_to_cert(binary_to_list(FileName));
pem_to_cert(FileName) when is_list(FileName)->
	try
		{ok,FileData}=file:read_file(FileName),
		[{'Certificate',Cert,_}] = public_key:pem_decode(FileData),
		{ ok , Cert }
	catch
		_:_ ->
			{error,error_no_pem_file}
	end.

pem_to_key(FileName) when is_binary(FileName)->
	pem_to_key(binary_to_list(FileName));
pem_to_key(FileName) when is_list(FileName)->
	try
		{ok,FileData}=file:read_file(FileName),
		[{T,K,_}]=public_key:pem_decode(FileData),
		{ ok , {T,K}}
	catch
		_:_ ->
			{error,error_no_pem_file}
	end.

pem_key_is_encrypted(FileName) when is_binary(FileName)->
	pem_to_key(binary_to_list(FileName));
pem_key_is_encrypted(FileName) when is_list(FileName)->
	try
		{ok,FileData}=file:read_file(FileName),
		[{_,_,not_encrypted}]=public_key:pem_decode(FileData),
			false
	catch
		_:_ ->
			true
	end.

-spec remove_pem_key_password(Password::string(),InFileName::string(),OutFileName::string()) -> boolean().
remove_pem_key_password(Password,InFileName,OutFilename)->
	try
		Cmd = io_lib:format("openssl rsa -passin pass:~s -in ~s -out ~s",[Password,InFileName,OutFilename]),
		_Result = os:cmd(Cmd),
		not pem_key_is_encrypted(OutFilename)
	catch
		_:_ ->
			false
	end.

-spec noop_mfa()->notification_cb().
noop_mfa()->
	{utils,noop,[]}.

-spec noop()->ok.
noop()->
	ok.

-spec safe_binary(binary()|string()|atom()|integer())->binary().
safe_binary(X) when is_binary(X)->
	X;
safe_binary(X) when is_list(X)->
	list_to_binary(X);
safe_binary(X) when is_atom(X)->
	atom_to_binary(X);
safe_binary(X) when is_integer(X)->
	integer_to_binary(X).

-spec split_into([term()],[term()])->[{term(),[term()]}].
split_into( A, B )->
	LA = length(A),
	LB = length(B),
	Batch = LB div LA,
	split_it(A,B,1,Batch,LB,[]).

split_it([H|T],B,Pos,Size,LB,Acc) when (Pos < LB) ->
	SubList = lists:sublist(B,Pos,Size),
	split_it(T,B,Pos+Size,Size,LB,[{H,SubList}|Acc]);
split_it(_,_,_,_,_,Acc)->
	lists:reverse(Acc).

select(true,A,_B) ->A;
select(false,_A,B)->B.

-spec adjust(Max::integer(),Divisor::integer())->integer().
adjust(_Max,0)->
	0;
adjust(Max,Divisor)->
	min(Max, Max - ( Max rem Divisor)).

-spec new_avg()-> average().
new_avg()->
	{0,10,[]}.

-spec compute_avg(number(),average())->average().
compute_avg(V,Avg)->
	{_,L,Values}=Avg,
	case length(Values) == L of
		true ->
			NV = [V|tl(Values)],
			NewAverage = lists:sum(NV) / L,
			{ NewAverage, L, NV };
		false ->
			NV = [V|Values],
			NewAverage = lists:sum(NV) / length(NV),
			{ NewAverage, L, NV }
	end.

-spec get_avg(average())->number().
get_avg({V,_,_})->
	V.

safe_list(I) when is_binary(I)->
	binary_to_list(I);
safe_list(I) when is_list(I)->
	I.

replace_data(Data,[])->
	Data;
replace_data(Data,[{From,To}|T])->
	NewData = string:replace(Data,From,To,all),
	replace_data(NewData,T).

-spec search_replace(Infile::binary()|string(),OutFile::binary()|string(),[{From::string(),To::string()}]) -> ok.
search_replace(InFile,OutFile,Elements)->
	{ ok , InData } = file:read_file(safe_list(InFile)),
	NewData = replace_data(binary_to_list(InData),Elements),
	file:write_file(safe_list(OutFile),list_to_binary(NewData)).
