%%%-------------------------------------------------------------------
%%% File    : gtp_udp_client.erl
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
-module(gtpp_udp_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    {ok, Socket} = gen_udp:open(3386, [binary]),
    {ok, #state{socket=Socket}}.

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
handle_info({udp, InSocket, InIP, InPort, Packet}, State) ->
    ?PRINTDEBUG2("Got Message ~p from ~p:~p", [Packet, InIP, InPort]),
    %% decode the header, {InIP, Port, SeqNum} forms a unique tuple. I think. How ugly.
    case gtpp_decode:decode_message(Packet) of
	{ok, {Header, Message}} ->
	    case Header#gtpp_header.msg_type of
		data_record_transfer_request ->
		    {{Type, Content}, _} = Message,
		    case Type of 
			send_data_record_packet ->
			    %% log Content as DRPs [] - cdr_log_srv:log(Message)
			    %% TODO in the future cdr_file_srv will instigate response when file closed. More efficient to batch them up.
			    ok;
			send_potential_duplicate_record_packet ->
			    ok;
			cancel_packets ->
			    ok;
			release_packets ->
			    ok
		    end,
		    send_data_record_transfer_response(InSocket, {InIP, InPort}, Header),
		    {noreply, State};
		node_alive_request ->
		    {noreply, State};
		node_alive_response ->
		    {noreply, State};
		redirection_response ->
		    {noreply, State};
		echo_request ->
		    send_echo_response(InSocket, {InIP, InPort}, Header),
		    {noreply, State};
		echo_response ->
		    {noreply, State}
	    end;
	{error, Reason} ->
	    %% Send back error of some kind
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
send_data_record_transfer_response(InSocket, {InIP, Port}, Header) ->
    ok.

send_echo_response(InSocket, {InIP, Port}, Header) ->
    R = gtpp_encode:echo_response(2, 0, 0, << >>), %% TODO, counter and restart_counter properly, counter per IP:Port I think.
    ok = gen_udp:send(InSocket, InIP, Port, R).
