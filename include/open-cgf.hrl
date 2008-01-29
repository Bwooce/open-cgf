%%%-------------------------------------------------------------------
%%% File    : open-cgf.hrl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%-------------------------------------------------------------------

-ifdef(debug).
    -define(PRINTDEBUG(Msg),
            io:format("~p :~p ~p~n", [Msg, ?FILE, ?LINE])).
    -define(PRINTDEBUG2(F, A),
            io:format(F ++ ":~p ~p~n", A ++ [?FILE, ?LINE])).
-else.
    -define(PRINTDEBUG(Msg), ok).
    -define(PRINTDEBUG2(F, A), ok).
-endif.

