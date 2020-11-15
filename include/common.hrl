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

-define(OWLS_APP,owls).

-ifdef(debug).
    -define(DBGTRC(Msg), io:format("~s:~s (~p) => ~s~n",[?MODULE,?FUNCTION_NAME,?LINE,Msg])).
-else.
    -define(DBGTRC(Msg), true).
-endif.

