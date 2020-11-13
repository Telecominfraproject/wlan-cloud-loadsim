-author("helge").


-define(OWLS_APP,owls).


-ifdef(debug).
    -define(DBGTRC(Msg), io:format("~s:~s (~p) => ~s~n",[?MODULE,?FUNCTION_NAME,?LINE,Msg])).
-else.
    -define(DBGTRC(Msg), true).
-endif.

