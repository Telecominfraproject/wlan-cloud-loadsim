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

%% Local logging shortcuts
-define(L_I(X),_=lager:info(X)).
-define(L_IA(X,Y),_=lager:info(X,Y)).
-define(L_E(X),_=lager:error(X)).
-define(L_EA(X,Y),_=lager:error(X,Y)).

%% Remove logging shortcuts
-define(RL_I(X),_=manager:log_info(X)).
-define(RL_IA(X,Y),_=manager:log_info(X,Y)).
-define(RL_E(X),_=manager:log_error(X)).
-define(RL_EA(X,Y),_=manager:log_error(X,Y)).
