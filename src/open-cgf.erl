%%%-------------------------------------------------------------------
%%% File    : open-cgf.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%-------------------------------------------------------------------
-module('open-cgf').

-export([start/0]).

start() ->
    application:start('open-cgf').
