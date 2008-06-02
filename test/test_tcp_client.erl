%%%-------------------------------------------------------------------
%%% File    : test_tcp_client.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 13 May 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module(test_tcp_client).

-behaviour(gen_server).

-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start_link/3, send/2, register/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, owner_pid, buffer}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SourceIP, DestIP, DestPort) ->
    gen_server:start_link(?MODULE, [self(), SourceIP, DestIP, DestPort], []).

%% register as a parent who wants the received messages
register(TPid, Pid) ->
    gen_server:call(TPid, {register, Pid}).

%% send a message (once, dumb impl)
send(TPid, Msg) ->
    gen_server:call(TPid, {send, Msg}).

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
init([OwnerPid, SourceIP, DestIP, DestPort]) ->
    {ok, Socket} = gen_tcp:connect(DestIP, DestPort, [binary, {ip, SourceIP}], 60*1000),
    process_flag(trap_exit, true),
    {ok, #state{socket=Socket, owner_pid=OwnerPid, buffer= << >>}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Pid}, _From, State) ->
    {reply, ok, State#state{owner_pid = Pid}};

handle_call({send, Msg}, _From, State) ->
    Reply = gen_tcp:send(State#state.socket, Msg),
    {reply, Reply, State};

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
handle_info({tcp_closed, _InSocket}, State) ->
    State#state.owner_pid ! {tcp, self(), closed},
    {stop, closed, State};

handle_info({tcp_error, _InSocket, Reason}, State) ->
    State#state.owner_pid ! {tcp, self(), {error, Reason}},
    {stop, error, State};

handle_info({tcp, _InSocket, Packet}, State) ->
    OldBuf = State#state.buffer,
    Buffer = << OldBuf/binary, Packet/binary >>,
    Length = get_lengths(Buffer),
    ?PRINTDEBUG("Got to handle info with packet, and lengths"),
    decode(Buffer, Length, State);



handle_info({Pid, diediedie}, State) when Pid =:= State#state.owner_pid ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket=undefined} = State) ->
    gen_tcp:close(State#state.socket),
    ok;
terminate(_Reason, State) ->
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

get_lengths(Buffer) ->
    case catch gtpp_decode:decode_GTPP_header(Buffer) of
	{'EXIT', _} ->
	    no_header;
	{H, Rest} ->	
	    ?PRINTDEBUG2("Got ~p~n,~p",[H,Rest]),
	    {size(Buffer) - size(Rest), H#gtpp_header.msg_len}
    end.


%%% some parts of the gtp' understanding escape gtpp_decode here. sob. 
decode(<< >>, no_header, State) ->
    {noreply, State};
decode(<<0:3, 0:1, _:3, 0:1, Rest/binary>>, no_header, State) when size(Rest) < 19 ->
    ?PRINTDEBUG("decode but not enough of a packet yet for a header (20 byte header)"),
    {noreply, State#state{buffer = <<0:3, 0:1, 0:3, 0:1, Rest/binary >> }};
decode(Packet, no_header, State) when size(Packet) < 6 ->
    ?PRINTDEBUG("decode but not enough of a packet yet for a header (6 byte header)"),
    {noreply, State#state{buffer=Packet}};
decode(Packet, no_header, State) ->
    {stop, normal, State}; %% if we have lots of stuff in the buffer, but nothing decodeable, so we should kill the connection and cut our losses
decode(Packet, {HLen, Length}, State) when size(Packet) < (HLen+Length) -> %% if not enough data yet
    ?PRINTDEBUG2("decode but not enough of a packet yet - header says ~B bytes required",[Length]),
    {noreply, State#state{buffer=Packet}};

decode(Packet, {HLen, Length}, State) -> %% if we get here then we might have enough, or multiple packets. Hence we always recurse.
    ?PRINTDEBUG2("decode header len ~B, body len ~B, packet size ~B",[HLen, Length, size(Packet)]),
    Total_len = HLen+Length,
    <<Buffer:Total_len/binary, Remainder/binary>> = Packet,
    Remainder_len = get_lengths(Remainder),
    case gtpp_decode:decode_message(Buffer) of
	{ok, Message} ->
	    State#state.owner_pid ! {tcp, self(), Message};
	_ ->
	    ok
    end,
    decode(Remainder, Remainder_len, State).


