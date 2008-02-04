%%%-------------------------------------------------------------------
%%% File    : open-cgf.hrl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%
%%% Copyright 2008 Bruce Fitzsimons
%%%
%%% This file is part of open-cgf.
%%%
%%% open-cgf is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License version 2 as
%%% published by the Free Software Foundation.
%%%
%%% open-cgf is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with open-cgf.  If not, see <http://www.gnu.org/licenses/>.
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

