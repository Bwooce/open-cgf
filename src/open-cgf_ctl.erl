%%%-------------------------------------------------------------------
%%% File    : open-cgf_ctl.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
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
-module('open-cgf_ctl').

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").
-include("open-cgf.hrl").

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, port}).
-define(CONTROL_FILE, "/tmp/.open-cgf_controlfile").

%%====================================================================
%% API
%%====================================================================
%% 
%% 
stop(_) ->
    ?PRINTDEBUG("open-cgf_ctl STOP"),
    {ok, Port} = get_port(?CONTROL_FILE),
    send(Port, "stop"),
    init:stop().
    
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, 'open-cgf_ctl'}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, Socket, Port} = listen(?CONTROL_FILE),
    {ok, #state{socket=Socket, port=Port}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({udp, _Socket, _IP, _InPortNo, "stop"}, State) ->
    error_logger:info_report("stop command received: open-cgf stopping"),
    init:stop(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

listen(Filename) ->
    %% listen on the first available port
    case gen_udp:open(0, [{ip, {127,0,0,1}}]) of
	{ok, Socket} ->
	    case inet:port(Socket) of
                {ok, Port} ->		    
		    file:write_file(Filename, io_lib:format("~w.", [Port])),
		    {ok, F} = file:read_file_info(Filename),
		    ok = file:write_file_info(Filename, F#file_info{mode = 8#00600}),
		    {ok, Socket, Port};
		Error ->
		    {error, Error}
	    end;
	Error ->
	    {error, Error}
    end.

get_port(Filename) ->
    case file:consult(Filename) of
        {ok, [Port]} ->
	    ?PRINTDEBUG2("Got port ~p",[Port]),
	    {ok, Port};
	{error, Error} ->
	    Error
    end.

send(Port, Message) ->
    ?PRINTDEBUG2("Sending Port ~p message \'~p\'",[Port, Message]),
    {ok, Socket} = gen_udp:open(0, [{ip, {127,0,0,1}}]),
    gen_udp:send(Socket, {127,0,0,1}, Port, Message),
    gen_udp:close(Socket).

