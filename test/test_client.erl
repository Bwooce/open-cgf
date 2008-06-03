%%%-------------------------------------------------------------------
%%% File    : test_client.erl
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
-module(test_client).

-behaviour(gen_server).

-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start_link/2, open/1, auto_respond/1, send/1, expect/1, expect_block/2, wait_for_all_expected/1, close/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% in the future we may change this to support multiple tests running simultaneously. Not now though.
-record(state, {proto, source, dest, tcp_pid, sent_queue=[], expected_queue=[], auto_respond_flag=false}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Proto, Source) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{Proto, Source}], []).

open(Dest) ->
    gen_server:call(?SERVER, {open, Dest}).

auto_respond(Flag) ->
    gen_server:call(?SERVER, {auto_respond, Flag}).

send(Msg) ->
    gen_server:call(?SERVER, {send, Msg}).

expect(Pattern) ->
    gen_server:call(?SERVER, {expect, Pattern}).

expect_block(Pattern, Timeout) ->
    gen_server:call(?SERVER, {expect_block, Pattern, Timeout}).

wait_for_all_expected(Timeout) ->
    gen_server:call(?SERVER, {wait_for_all_expected, Timeout}).

close() ->
    gen_server:call(?SERVER, {close}).


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
init([{Proto, {SourceIP, SourcePort}}]) ->
    process_flag(trap_exit, true),
    case Proto of
	tcp -> ok;
	udp -> 
	    {ok, _Pid} = test_udp_client:start_link(SourceIP, SourcePort)
    end,
    {ok, #state{proto=Proto, source={SourceIP,SourcePort}}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({open, {DestIP, DestPort}}, _From, State) ->
    Tcp_pid = case State#state.proto of
		  tcp ->
		      {SourceIP, Port} = State#state.source,
		      {ok, Pid} = test_tcp_client:start_link(SourceIP, DestIP, DestPort),
		      Pid;
		  udp ->
		      undefined
	      end,
    {reply, ok, State#state{dest={DestIP, DestPort}, tcp_pid=Tcp_pid}};

handle_call({auto_respond, Flag}, _From, State) ->
    {reply, ok, State#state{auto_respond_flag=Flag}};

handle_call({send, Msg}, _From, State) ->
    case State#state.proto of
	tcp ->
	    test_tcp_client:send(State#state.tcp_pid, Msg);
	udp ->
	    ok = test_udp_client:send(State#state.dest, Msg)
    end,
    {reply, ok, State#state{sent_queue=State#state.sent_queue ++ [Msg]}};

handle_call({expect, Pattern}, _From, State) ->
    {reply, ok, State#state{expected_queue=State#state.expected_queue ++ [Pattern]}};

handle_call({close}, _First, State) ->
    case State#state.proto of
	tcp ->
	    State#state.tcp_pid ! {self(), diediedie};
	udp ->
	    test_udp_client:close()
    end,
    {reply, ok, State#state{tcp_pid=undefined, dest=undefined, sent_queue=[], expected_queue=[]}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

    %% send errors if unmatched expectations 

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
handle_info({'EXIT', Pid, Reason}, State) ->
    ?PRINTDEBUG2("Pid ~p exited for reason ~p",[Pid,Reason]),
    {noreply, State};

handle_info({Proto, Pid, Message}, State) ->
    ?PRINTDEBUG2("Got Message ~p from ~p ~p", [Message, Proto, Pid]),
    case State#state.auto_respond_flag of
	true ->
	    {Header, Body} = Message,
	    case Header#gtpp_header.msg_type of 
		echo_request ->
		    ?PRINTDEBUG("***Got echo_request - autoresponding"),
		    Msg = gtpp_encode:echo_response(Header#gtpp_header.version, Header#gtpp_header.seqnum, 0, << >>),
		    test_client_send(Msg, State),
		    {noreply, State};
		node_alive_request ->
		    ?PRINTDEBUG("***Got node_alive_request - autoresponding"),
		    Msg = gtpp_encode:node_alive_response(Header#gtpp_header.version, Header#gtpp_header.seqnum, << >>),
		    test_client_send(Msg, State),
		    {noreply, State};
		redirection_request ->
		    ?PRINTDEBUG("***Got redirection_request - autoresponding"),
		    Msg = gtpp_encode:redirection_response(Header#gtpp_header.version, Header#gtpp_header.seqnum),
		    test_client_send(Msg, State),
		    {noreply, State};
		_ ->
		    process_expected(Message, State)
	    end;
	_ ->
	    process_expected(Message, State)
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
process_expected(Message, State) ->
    ?PRINTDEBUG2("process_expected - Message ~p, State ~p",[Message,State]),
    NewExpected = find_match(Message, State#state.expected_queue),
    OldLen = length(State#state.expected_queue),
    NewLen = length(NewExpected),
    case OldLen of
	NewLen -> %% e.g. didn't change
	    error_logger:info_msg("Unmatched response from CGF - ~p",[Message]),
	    {noreply, State};
	_ ->
	    {noreply, State#state{expected_queue=NewExpected}}
    end.

test_client_send(Message, State) ->
    case State#state.proto of
	tcp ->
	    test_tcp_client:send(State#state.tcp_pid, Message);
	udp ->
	    ok = test_udp_client:send(State#state.dest, Message)
    end.

	    
find_match(Message, ExpectedList) ->
    find_match(Message, ExpectedList, []).

find_match(_Message, [], Output) ->
    Output;
find_match(Message, [Pattern|Rest], Output) ->
    case ets:test_ms(Message, [{Pattern,[],['$_']}]) of
	{ok, _} ->  
	    ?PRINTDEBUG("***Matched***"),
	    Output ++ Rest; %% delete the pattern we just matched
	_ -> find_match(Message, Rest, Output ++ [Pattern])
    end.
		
