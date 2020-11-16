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
-export([make_dir/1]).

-spec make_dir( DirName::string() ) -> ok | { error, atom() }.
make_dir(DirName)->
	case file:make_dir(DirName) of
		ok -> ok;
		{error,eexist} -> ok;
		Error -> Error
	end.
