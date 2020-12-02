%%%-------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 11:34 p.m.
%%%-------------------------------------------------------------------
-author("helge").

-ifndef(__OWLS_COMMON_HRL__).
-define(__OWLS_COMMON_HRL__,1).

-compile({parse_transform, lager_transform}).

-include_lib("stdlib/include/ms_transform.hrl").

-type attribute_list() :: #{ atom() => term() }.
-type generic_error() :: { error, Reason::term() }.
-type generic_result() :: ok | generic_error().
-type notification_cb() ::  { Module::atom(), Function::atom(),Args::[term()]}.

-export_type([attribute_list/0,generic_error/0,generic_result/0,notification_cb/0]).

-define(OWLS_APP,owls).

-ifdef(debug).
    -define(DBGTRC(Msg), io:format("~s:~s (~p) => ~s~n",[?MODULE,?FUNCTION_NAME,?LINE,Msg])).
	-define(DBGTRC(FMsg,Args), io:format("~s:~s (~B) => " FMsg "~n",[?MODULE,?FUNCTION_NAME,?LINE] ++ Args)).
	-define(DBGSTR(Msg), io_lib:format("~s:~s (~B) => ~s",[?MODULE,?FUNCTION_NAME,?LINE,Msg])).
	-define(DBGSTR(FMsg,Args), io_lib:format("~s:~s (~B) => " FMsg,[?MODULE,?FUNCTION_NAME,?LINE] ++ Args)).
-else.
    -define(DBGTRC(Msg), true).
	-define(DBGTRC(FMsg,Args), true).
	-define(DBGSTR(Msg), Msg).
	-define(DBGSTR(FMsg,Args), io_lib:format(FMsg,Args)).
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


-endif.