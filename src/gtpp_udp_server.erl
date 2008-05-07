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
-export([start_link/0, confirm/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, ip, port, version, known_sources, altCGF}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

confirm(Address, SeqNums, Cause) ->
    'open-cgf_logger':debug("Reponding to sequence numbers ~p with cause ~p",[SeqNums, Cause]),
    %% 15s chosen arbitrarily. 5s timed out on 10K instant messages w/full debug.
    gen_server:call(?SERVER, {confirm, Address, SeqNums, Cause}, 15*1000). 

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
    RSCounter = 'open-cgf_state':inc_restart_counter(), %% both TCP and UDP servers do this so if either restarts it is noticable
    {ok, {IP,Port}} = application:get_env('open-cgf', listen),
    {ok, Socket} = gen_udp:open(Port, [binary,{ip, IP},{recbuf, 300000}]),
    error_logger:info_msg("open-cgf listening on ~p:~p UDP - session counter ~p~n",[inet_parse:ntoa(IP), Port, RSCounter]),
    {ok, CDFs} = application:get_env('open-cgf', cdf_list),
    {ok, Version} = application:get_env('open-cgf', gtpp_version),
    {ok, AltCGF} = application:get_env('open-cgf', peer_cgf),
    lists:foreach(fun({DestIP, DestPort}) ->
			  error_logger:info_msg("open-cgf sending echo request and node_alive request to ~p:~p~n",[DestIP, DestPort]),
			  send_echo_request(Socket, Version, {DestIP,DestPort}),
			  send_node_alive_request(Socket, Version, {DestIP, DestPort}, IP)
		  end, CDFs),			  
    process_flag(trap_exit, true),
    {ok, #state{socket=Socket, ip=IP, port=Port, version=Version, known_sources=orddict:new(), altCGF=AltCGF}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({confirm, {IP, Port}, SeqNums, Cause}, _From, State) ->
    SeqNum = hd(SeqNums),
    send_data_record_transfer_response(State#state.socket, State#state.version, SeqNum, 
				       {IP, Port}, 
				       Cause, SeqNums),
    {reply, ok, State};

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
     %% {ok,{{gtpp_header,0,0,1,echo_response,2,1},{{count,7},<<>>}}}
	{ok, {Header, Message}} ->
	    case Header#gtpp_header.msg_type of
	        version_not_supported ->
                    {noreply, State#state{version=Header#gtpp_header.version}};
		data_record_transfer_request ->
		    {{Type, _Content}, _} = Message,
		    case Type of 
			send_data_record_packet ->
			    cdr_file_srv:log({udp, InIP, InPort}, Header#gtpp_header.seqnum, Message),
			    {noreply, State};
			send_potential_duplicate_record_packet ->
			    cdr_file_srv:log_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, Message),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			cancel_packets ->
			    {{cancel_packets, {sequence_numbers, SeqNums}}, _} = Message,
			    ?PRINTDEBUG2("Cancelling packets with seqnums ~p",[SeqNums]),
			    cdr_file_srv:remove_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			release_packets ->
			    {{release_packets, {sequence_numbers, SeqNums}}, _} = Message,
			    ?PRINTDEBUG2("Releasing packets with seqnums ~p",[SeqNums]),
			    cdr_file_srv:commit_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			Other ->
	                    error_logger:error_msg("Ignored data transfer request with invalid content ~p",[Other]),
			    {noreply, State}
		    end;
		node_alive_request ->
		    send_node_alive_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
					     {InIP, InPort}),
		    {noreply, State};
		node_alive_response ->
		    {noreply, State};
		redirection_response ->
		    {noreply, State};
		echo_request ->
		    send_echo_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
				       {InIP, InPort}),
		    {noreply, State};
		echo_response ->
		    {{count, NewCount}, _} = Message,
		    case orddict:find({udp, InIP, InPort}, State#state.known_sources) of
			{ok, NewCount} ->
			    {noreply, State}; %% endpoint is known and has not restarted
			{ok, _} ->
			    %% endpoint is known and has restarted. Sound the klaxxon.
			    error_logger:warning_msg("CDF ~p:~p has restarted, saving pending CDRs and resetting sequencing",[InIP,InPort]),
			    cdr_file_srv:reset({udp, InIP, InPort}),
			    {noreply, State#state{known_sources=orddict:store({udp, InIP, InPort}, NewCount, State#state.known_sources)}};
			error ->
			    {noreply, State#state{known_sources=orddict:store({udp, InIP, InPort}, NewCount, State#state.known_sources)}}
		    end
	    end;
	{error, _Reason} ->
	    %% Send back error of some kind TODO
	    {noreply, State}
    end;

handle_info(Info, State) ->
    error_logger:warning_msg("Got unhandled info ~p while in state ~p",[Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    error_logger:info_msg("Closing open-cgf udp server for reason: \"~p\". Sending redirection requests to all known CDFs~n", [Reason]),
    case Reason of
	normal ->     timer:sleep(1000); %% give cdr_file_srv a second to catch up, if required. TODO improve this to be synchronised
	_ -> ok %% no time to bugger around
    end,
    lists:foreach(fun({{udp, DestIP, DestPort}, _Count}) ->
			  error_logger:info_msg("Sending redirection to CDF ~s:~p", [inet_parse:ntoa(DestIP), DestPort]), 
			  send_redirection_request(State#state.socket, State#state.version, {DestIP, DestPort}, State#state.altCGF)
		  end, orddict:to_list(State#state.known_sources)),
    error_logger:info_msg("~nopen-cgf exiting~n~n"),
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
send_data_record_transfer_response(Socket, Version, SeqNum, {InIP, InPort}, Cause, SeqNums) ->
    R = gtpp_encode:data_record_transfer_response(Version, SeqNum, Cause, SeqNums, << >>), 
    ok = gen_udp:send(Socket, InIP, InPort, R).

send_echo_response(Socket, Version, SeqNum, {InIP, InPort}) ->
    RC = 'open-cgf_state':get_restart_counter(),
    R = gtpp_encode:echo_response(Version, SeqNum, RC, << >>), 
    ok = gen_udp:send(Socket, InIP, InPort, R).

send_echo_request(Socket, Version, {DestIP, DestPort}) ->
    SeqNum = 'open-cgf_state':get_next_seqnum({DestIP, DestPort}),
    R = gtpp_encode:echo_request(Version, SeqNum, << >>), 
    ok = gen_udp:send(Socket, DestIP, DestPort, R).

send_node_alive_request(Socket, Version, {DestIP, DestPort}, MyAddress) ->
    SeqNum = 'open-cgf_state':get_next_seqnum({DestIP, DestPort}),
    R = gtpp_encode:node_alive_request(Version, SeqNum, MyAddress, << >>), 
    ok = gen_udp:send(Socket, DestIP, DestPort, R).

send_node_alive_response(Socket, Version, SeqNum, {InIP, InPort}) ->
    R = gtpp_encode:node_alive_response(Version, SeqNum, << >>), 
    ok = gen_udp:send(Socket, InIP, InPort, R).
    
send_redirection_request(Socket, Version, {DestIP, DestPort}, AltCGF) ->
    SeqNum = 'open-cgf_state':get_next_seqnum({DestIP, DestPort}),
    R = case AltCGF of 
	    none ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, << >>);
	    _ ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, AltCGF, << >>)
	end,
    ok = gen_udp:send(Socket, DestIP, DestPort, R).

