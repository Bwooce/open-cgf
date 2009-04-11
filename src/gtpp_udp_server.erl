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
-export([start_link/0, send_udp/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, ip, port, version, known_sources, outstanding_requests, altCGF, t3_response, n3_requests}).

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
    RSCounter = 'open-cgf_state':get_restart_counter(), 
    {ok, {IP,Port}} = 'open-cgf_config':get_item({'open-cgf', listen}, ip_port), %% error and crash if no match
    {ok, Socket} = gen_udp:open(Port, [binary,{ip, IP},{recbuf, 300000}, {read_packets, 50}]),
    error_logger:info_msg("open-cgf listening on ~s:~B UDP - session counter ~p~n",[inet_parse:ntoa(IP), Port, RSCounter]),
    {ok, CDFs} = 'open-cgf_config':get_item({'open-cgf', cdf_list}, none, []), %% default to [] (none), validation is TODO
    {ok, Version} = 'open-cgf_config':get_item({'open-cgf', gtpp_version}, {integer, 0, 2}, 2), %% default to 2, make people move into the future
    {ok, AltCGF} = 'open-cgf_config':get_item({'open-cgf', peer_cgf}, none, none), %% default to none, validation is TODO
    {ok, T3Response} = 'open-cgf_config':get_item({'open-cgf', t3_response}, {integer, 1, 3600}, 1),
    {ok, N3Requests} = 'open-cgf_config':get_item({'open-cgf', n3_requests}, {integer, 1, 100}, 5),
    Outstanding = lists:foldl(fun({DestIP, DestPort}, Acc) ->
				      error_logger:info_msg("open-cgf sending echo request and node_alive request to ~s:~p~n",
							    [inet_parse:ntoa(DestIP), DestPort]),
				      {ok, ERPid, ERSeq} = send_echo_request(Socket, Version, {DestIP,DestPort}, T3Response, N3Requests),
				      {ok, NAPid, NASeq} = send_node_alive_request(Socket, Version, {DestIP, DestPort}, IP, T3Response, N3Requests),
				      Acc ++ [{{DestIP, DestPort, NASeq}, NAPid}, {{DestIP, DestPort, ERSeq},ERPid}]
			      end, [], CDFs),			  
    process_flag(trap_exit, true),
    {ok, #state{socket=Socket, ip=IP, port=Port, version=Version, known_sources=orddict:new(), altCGF=AltCGF, 
		outstanding_requests=Outstanding, t3_response=T3Response, n3_requests=N3Requests}}.

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
    ?PRINTDEBUG2("Got Message ~p from ~s:~p", [Packet, inet_parse:ntoa(InIP), InPort]),
    %% decode the header, {InIP, Port, SeqNum} forms a unique tuple. I think. How ugly.
    case gtpp_decode:decode_message(Packet) of
     %% {ok,{{gtpp_header,0,0,1,echo_response,2,1},[{count,7},<<>>}]}
	{ok, {Header, Message}} ->
	    case Header#gtpp_header.msg_type of
	        version_not_supported ->
                    {noreply, State#state{version=Header#gtpp_header.version}};
		data_record_transfer_request ->
		    {Type, _Content} = hd(Message),
		    case Type of 
			send_data_record_packet ->
			    cdr_file_srv:log({udp, InIP, InPort}, Header#gtpp_header.seqnum, Message),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
				    
			    {noreply, State};
			send_potential_duplicate_record_packet ->
			    Resp = case cdr_file_srv:check_duplicate({udp, InIP, InPort}, Header#gtpp_header.seqnum) of
				       false ->
					   cdr_file_srv:log_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, Message),
					   request_accepted;
				       true ->
					   error_logger:warning_msg("Duplicate sequence number ~p from ~s:~B",
								    [Header#gtpp_header.seqnum, inet_parse:ntoa(InIP), InPort]),   
					   request_related_to_duplicates_already_fulfilled
				   end,
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       Resp, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			cancel_packets ->
			    {cancel_packets, {sequence_numbers, SeqNums}} = hd(Message),
			    ?PRINTDEBUG2("Cancelling packets with seqnums [~s]",['open-cgf_logger':format_seqnums(SeqNums)]),
			    cdr_file_srv:remove_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			release_packets ->
			    {release_packets, {sequence_numbers, SeqNums}} = (Message),
			    ?PRINTDEBUG2("Releasing packets with seqnums [~s]",['open-cgf_logger':format_seqnums(SeqNums)]),
			    cdr_file_srv:commit_possible_dup({udp, InIP, InPort}, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       request_accepted, [Header#gtpp_header.seqnum]),
			    {noreply, State};
			Other ->
			    send_data_record_transfer_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
							       {InIP, InPort}, 
							       invalid_message_format, [Header#gtpp_header.seqnum]),
	                    error_logger:error_msg("Ignored data transfer request with invalid content ~p",[Other]),
			    {noreply, State}
		    end;
		node_alive_request ->
		    send_node_alive_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
					     {InIP, InPort}),
		    {noreply, State};
		node_alive_response ->
		    ?PRINTDEBUG("********************Got NodeAlive response"),
		    NewOutstanding = reliable_ack({InIP, InPort}, Header#gtpp_header.seqnum, State#state.outstanding_requests),
		    {noreply, State#state{outstanding_requests=NewOutstanding}};
		redirection_response ->
		    {noreply, State};
		echo_request ->
		    send_echo_response(InSocket, State#state.version, Header#gtpp_header.seqnum,
				       {InIP, InPort}),
		    {noreply, State};
		echo_response ->
		    ?PRINTDEBUG("*******************Got Echo response"),
		    NewOutstanding = reliable_ack({InIP, InPort}, Header#gtpp_header.seqnum, State#state.outstanding_requests),
		    {count, NewCount} = hd(Message),
		    case orddict:find({udp, InIP, InPort}, State#state.known_sources) of
			{ok, NewCount} ->
			    {noreply, State#state{outstanding_requests=NewOutstanding}}; %% endpoint is known and has not restarted
			{ok, _} ->
			    %% endpoint is known and has restarted. Sound the klaxxon.
			    error_logger:warning_msg("CDF ~s:~p has restarted, saving pending CDRs and resetting sequencing",
						     [inet_parse:ntoa(InIP),InPort]),
			    cdr_file_srv:reset({udp, InIP, InPort}),
			    {noreply, State#state{known_sources=orddict:store({udp, InIP, InPort}, NewCount, State#state.known_sources),
						  outstanding_requests=NewOutstanding}};
			error ->
			    {noreply, State#state{known_sources=orddict:store({udp, InIP, InPort}, NewCount, State#state.known_sources),
						  outstanding_requests=NewOutstanding}}
		    end
	    end;
	{error, _Reason} ->
	    %% Send back error of some kind TODO
	    {noreply, State}
    end;

handle_info({timeout, {IP,Port}, SeqNum}, State) ->
    error_logger:warning_msg("CDF ~s:~B failed to respond to CGF request ~B, possibly down",[inet_parse:ntoa(IP), Port, SeqNum]),
    %% look up and see how important it is. Possibly reset the connection to the CDF. TODO once I can work out a rational response
    {noreply, State};

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
    %% don't bother storing them in a queue, we're shutting down so we're allowed to be messy.
    lists:foreach(fun({{udp, DestIP, DestPort}, _Count}) ->
			  error_logger:info_msg("Sending redirection to CDF ~s:~p", [inet_parse:ntoa(DestIP), DestPort]), 
			  send_redirection_request(State#state.socket, State#state.version, {DestIP, DestPort}, State#state.altCGF, 
						   State#state.t3_response, State#state.n3_requests)
		  end, orddict:to_list(State#state.known_sources)),
    error_logger:info_msg("~nopen-cgf udp exiting~n~n"),
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

send_echo_request(Socket, Version, Dest, Timeout, MaxAttempts) ->
    SeqNum = 'open-cgf_state':get_next_seqnum(Dest),
    R = gtpp_encode:echo_request(Version, SeqNum, << >>), 
    Pid = send_reliably(Socket, Dest, SeqNum, R, Timeout, MaxAttempts),
    {ok, Pid, SeqNum}.

send_node_alive_request(Socket, Version, Dest, MyAddress, Timeout, MaxAttempts) ->
    SeqNum = 'open-cgf_state':get_next_seqnum(Dest),
    R = gtpp_encode:node_alive_request(Version, SeqNum, MyAddress, << >>), 
    Pid = send_reliably(Socket, Dest, SeqNum, R, Timeout, MaxAttempts),
    {ok, Pid, SeqNum}.

send_node_alive_response(Socket, Version, SeqNum, {InIP, InPort}) ->
    R = gtpp_encode:node_alive_response(Version, SeqNum, << >>), 
    ok = gen_udp:send(Socket, InIP, InPort, R).
    
send_redirection_request(Socket, Version, Dest, AltCGF, Timeout, MaxAttempts) ->
    SeqNum = 'open-cgf_state':get_next_seqnum(Dest),
    R = case AltCGF of 
	    none ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, << >>);
	    _ ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, AltCGF, << >>)
	end,
    Pid = send_reliably(Socket, Dest, SeqNum, R, Timeout, MaxAttempts),
    {ok, Pid, SeqNum}.


send_reliably(Socket, Dest, SeqNum, Msg, Timeout, MaxAttempts) ->
    spawn(?MODULE, send_udp, [self(), Socket, Dest, SeqNum, Msg, Timeout, MaxAttempts]).

send_udp(OwnerPid, _Socket, Dest, SeqNum, _Msg, _Timeout, 0) ->
    OwnerPid ! {timeout, Dest, SeqNum};
send_udp(OwnerPid, Socket, {IP,Port}, SeqNum, Msg, Timeout, MaxAttempts) ->
    ok = gen_udp:send(Socket, IP, Port, Msg),
    receive
	ack -> ok %% die a sweet death knowing that all is right with the world		    
    after Timeout*1000 ->
	    send_udp(OwnerPid, Socket, {IP,Port}, SeqNum, Msg, Timeout*2, MaxAttempts-1) %% back off on send
    end.

reliable_ack({IP, Port}, SeqNum, List) ->
   case lists:keytake({IP, Port, SeqNum}, 1, List) of
	{value, {_, Pid}, NewList} -> 
	    Pid ! ack,
	    NewList;	
	_ -> 
	    ?PRINTDEBUG2("ack recvd but no request found, ~p:~p ~p",[IP,Port,SeqNum]),
	    List
   end.
 
