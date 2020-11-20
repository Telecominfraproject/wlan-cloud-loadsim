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

%% API
-export([make_dir/1,uuid/0,get_addr/0,get_addr2/0]).

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
good_address([{addr,{A,B,C,D}}|_Tail]) when A=/=127 ->
	{A,B,C,D};
good_address([_|T])->
	good_address(T).

