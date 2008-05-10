%%%-------------------------------------------------------------------
%%% File    : open-cgf_config.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 18 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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

-module('open-cgf_config').

%% API
-export([get_item/2, get_item/3]).
-export([test/0]).

%%====================================================================
%% API
%%====================================================================
get_item({App,Tag}, Validation_type) ->
    %% try and get App and Tag
    case application:get_env(App, Tag) of
	{ok, Value} ->
	    %% validate using Validation_type (may be fun)
	    case validate_item(Validation_type, Value) of
		{ok, Validated_value} ->
		    error_logger:info_msg("CONFIG: ~p Tag: \'~p\' - Value ~p found and validated~n",[App,Tag,Value]), %% use original format, not internal
		    {ok, Validated_value};
		{error, Message} ->
		    Msg = lists:flatten(io_lib:format("CONFIG: ~p Tag: \'~p\' - failed validation for reason ~s~n",[App, Tag, Message])),
		    error_logger:error_msg(Msg),
		    {error, Msg}
	    end;
	undefined ->
	    Msg = lists:flatten(io_lib:format("CONFIG: ~p Tag: \'~p\' could not be found in the configuration or .app file~n",[App, Tag])),
	    error_logger:warning_msg(Msg),
	    {error, Msg}
    end.

get_item({App,Tag}, Validation_type, Default_value) ->
    case get_item({App,Tag}, Validation_type) of 
	{error, _Reason} ->
	    error_logger:warning_msg("CONFIG: ~p Tag: \'~p\' - Value defaulting to ~p~n",[App,Tag,Default_value]),
	    {ok, Default_value};
	{ok, Value} ->
	    {ok, Value}
    end.

test() ->
    {ok, tty} = get_item({kernel, error_logger}, atom),
    {ok, not_tty} = get_item({kernel, not_error_logger},atom,not_tty),
    {error, _Message} = get_item({kernel, 'not_to_be-found'}, atom),
    io:format("Tests passed successfully~n").


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
validate_item(none, Value) -> %% lazy sod
    {ok, Value};
validate_item({validation_fun, Fun}, Value) ->
    apply(Fun, [Value]);
validate_item(atom, Value) when is_atom(Value) ->
    {ok, Value};
validate_item(atom, Value) ->
    {error, lists:flatten(io_lib:format("atom value must be lowercase, without quotes or punctuation. Not ~p",[Value]))};
validate_item(ip_port, Value) ->
    parse_IP_port(Value);
validate_item(ip_port_or_none, Value) ->
    case parse_IP_port(Value) of
	{error, Invalid_IP_message} ->
	    case Value of
		none ->
		    {ok, none};
		_ ->
		    {error, Invalid_IP_message}
	    end;
	{ok, NewValue} ->
	    {ok, NewValue}
    end;
validate_item(ip, Value) ->
    parse_IP_address(Value);
validate_item(bool, Value) ->
    case Value of
	true ->
	    {ok, Value};
	false ->
	    {ok, Value};
	Other ->
	    {error, lists:flatten(io_lib:format("boolean value must be true or false, not ~p",[Other]))}
    end;
validate_item({integer, Min, Max}, Value) when is_integer(Value), Value >= Min, Value =< Max ->
    {ok, Value};
validate_item({integer, Min, Max}, Value) ->
    {error, lists:flatten(io_lib:format("integer value must be between ~B and ~B, not ~p",[Min, Max, Value]))}.

parse_IP_port({IP,Port}) when Port >=0, Port < 65536 ->
    case parse_IP_address(IP) of 
	{ok, ParsedIP} ->
	    {ok, {ParsedIP, Port}};
	{error, Err} ->
	    {error, Err}
    end;
parse_IP_port({_IP, _Port}) ->
    {error, "port not in range of 0-65535"};
parse_IP_port(_) ->
    {error, "not in {IP, Port} format. e.g. {{127,0,0,1},2048}"}.

parse_IP_address({IP1,IP2,IP3,IP4}) ->
    {ok, {IP1, IP2, IP3, IP4}};
parse_IP_address(Addr) ->
    case inet_parse:address(Addr) of
	{ok, Address} ->
	    {ok, Address};
	{error, _Reason} ->
	    {error, "invalid IP address format, use {127,0,0,1} or \"127.0.0.1\", or IPv6 variants"}
    end.

