%%%-------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 11:34 p.m.
%%%-------------------------------------------------------------------
-author("helge").

-compile({parse_transform, lager_transform}).

-include_lib("stdlib/include/ms_transform.hrl").

-define(OWLS_APP,owls).

-ifdef(debug).
    -define(DBGTRC(Msg), io:format("~s:~s (~p) => ~s~n",[?MODULE,?FUNCTION_NAME,?LINE,Msg])).
-else.
    -define(DBGTRC(Msg), true).
-endif.

-define(L_I1(X),_=lager:info(X)).
-define(L_I2(X,Y),_=lager:info(X,Y)).
