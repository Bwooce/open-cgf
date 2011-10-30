%%%-------------------------------------------------------------------
%%% File    : gtp_tcp_connection.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description :
%%%
%%% Created : 22 May 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module(gtpp_tcp_connection).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, ip, port, buffer, version, last_request_ts, timeout, altCGF, dest_ip, dest_port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start({IP, Port}) -> %% connect to this when started
    gen_server:start_link(?MODULE, [{IP, Port}], []);

start(Socket) -> %% incoming connection already accepted
    gen_server:start_link(?MODULE, [Socket], []).


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
init([{IP, Port}]) ->
    ?PRINTDEBUG2("gtpp_tcp_connection attempting connection to ~s:~B",[ inet_parse:ntoa(IP), Port]),
    %% get setup info
    State = get_config(),
    %% connect
    case gen_tcp:connect(IP, Port, [{ip, State#state.ip}, binary, {packet, 0}], 10*1000) of
	{ok, Socket} ->
	    error_logger:info_msg("open-cgf connected from ~s:~B to CDF ~s:~B TCP", [inet_parse:ntoa(State#state.ip), State#state.port,
										     inet_parse:ntoa(IP), Port]),
	    SeqNum = 'open-cgf_state':get_next_seqnum({tcp, IP, Port}),
	    R = gtpp_encode:node_alive_request(State#state.version, SeqNum, State#state.ip, << >>),
	    ok = gen_tcp:send(Socket, R),
	    process_flag(trap_exit, true),
	    {ok, State#state{socket=Socket, dest_ip=IP, dest_port=Port, last_request_ts=greg_now(), buffer= << >>}};
	{error, Reason} ->
	    error_logger:warning_msg("open-cgf failed to connect from ~s:~B to CDF ~s:~B TCP - reason ~p", [inet_parse:ntoa(State#state.ip), State#state.port,
													    inet_parse:ntoa(IP), Port, Reason]),
	    {ok, {error, Reason}}
    end;

init([Socket]) ->
    %% get setup info
    State = get_config(),
    %% already accepted
    {ok, {IP, Port}} = inet:peername(Socket),
    error_logger:info_msg("open-cgf connected from ~s:~B to CDF ~s:~B TCP", [inet_parse:ntoa(State#state.ip), State#state.port,
									     inet_parse:ntoa(IP), Port]),
    SeqNum = 'open-cgf_state':get_next_seqnum({tcp, IP, Port}),
    R = gtpp_encode:node_alive_request(State#state.version, SeqNum, State#state.ip, << >>),
    ok = gen_tcp:send(Socket, R),
    process_flag(trap_exit, true),
    {ok, State#state{socket=Socket, dest_ip=IP, dest_port=Port, last_request_ts=greg_now(), buffer = << >>}}.

get_config() ->
    {ok, {IP,Port}} = 'open-cgf_config':get_item({'open-cgf', listen}, ip_port), %% error and crash if no match
    {ok, Version} = 'open-cgf_config':get_item({'open-cgf', gtpp_version}, {integer, 0, 2}, 2), %% default to 2, make people move into the future
    {ok, AltCGF} = 'open-cgf_config':get_item({'open-cgf', peer_cgf}, none, none), %% default to none, validation is TODO
    {ok, Timeout} = 'open-cgf_config':get_item({'open-cgf', tcp_timeout}, {integer, 1, 3600}, 15), %% heartbeat-like timeout
    #state{ip=IP, port=Port, version=Version, altCGF=AltCGF, timeout=Timeout}.


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
handle_info({controlled_shutdown}, State) ->
    {stop, shutdown, State};

handle_info({tcp_closed, _Socket}, State) ->
    error_logger:warning_msg("open-cgf connection closed from ~s:~B to CDF ~s:~B TCP", [inet_parse:ntoa(State#state.ip), State#state.port,
											inet_parse:ntoa(State#state.dest_ip), State#state.dest_port]),
    {stop, normal, State#state{socket=undefined}};

handle_info({tcp_error, _Socket, Reason}, State) ->
    error_logger:warning_msg("open-cgf connection error ~p from ~s:~B to CDF ~s:~B TCP - closing", [Reason,
												    inet_parse:ntoa(State#state.ip),
												    State#state.port,
												    inet_parse:ntoa(State#state.dest_ip),
												    State#state.dest_port]),
    {stop, normal, State#state{socket=undefined}};

handle_info({tcp, _Socket, Packet}, State) ->
    ?PRINTDEBUG("Got to handle info with packet"),
    OldBuf = State#state.buffer,
    Buffer = <<OldBuf/binary, Packet/binary>>,
    ?PRINTDEBUG("Got to handle info with packet, appended"),
    Length = get_lengths(Buffer),
    ?PRINTDEBUG("Got to handle info with packet, and lengths"),
    decode(Buffer, Length, State);

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
    cdr_file_srv:reset({tcp, State#state.dest_ip, State#state.dest_port}),
    case State#state.socket of
	undefined ->
	    ok;
	_ ->
	    SeqNum = 'open-cgf_state':get_next_seqnum({tcp, State#state.dest_ip, State#state.dest_port}),
	    send_redirection_request(State#state.socket, State#state.version, SeqNum,
				    State#state.altCGF),
	    catch gen_tcp:close(State#state.socket)
    end,
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

send_data_record_transfer_response(Socket, Version, SeqNum, Cause, SeqNums) ->
    R = gtpp_encode:data_record_transfer_response(Version, SeqNum, Cause, SeqNums, << >>),
    ok = gen_tcp:send(Socket, R).

send_echo_response(Socket, Version, SeqNum) ->
    RC = 'open-cgf_state':get_restart_counter(),
    R = gtpp_encode:echo_response(Version, SeqNum, RC, << >>),
    ok = gen_tcp:send(Socket, R).

send_echo_request(Socket, SeqNum, Version) ->
    R = gtpp_encode:echo_request(Version, SeqNum, << >>),
    ok = gen_tcp:send(Socket, R).

send_node_alive_response(Socket, Version, SeqNum) ->
    R = gtpp_encode:node_alive_response(Version, SeqNum, << >>),
    ok = gen_tcp:send(Socket, R).

send_redirection_request(Socket, Version, SeqNum, AltCGF) ->
    R = case AltCGF of
	    none ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, << >>);
	    _ ->
		gtpp_encode:redirection_request(Version, SeqNum, node_about_to_go_down, AltCGF, << >>)
	end,
    ok = gen_tcp:send(Socket, R).


greg_now() ->
    calendar:datetime_to_gregorian_seconds({date(), time()}).

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
    SourceKey = {tcp, State#state.dest_ip, State#state.dest_port},
    Total_len = HLen+Length,
    <<Buffer:Total_len/binary, Remainder/binary>> = Packet,
    ?PRINTDEBUG("Seperated packet..."),
    Remainder_len = get_lengths(Remainder),
    case gtpp_decode:decode_message(Buffer) of
     %% {ok,{{gtpp_header,0,0,1,echo_response,2,1},[{count,7},<<>>]}}
	{ok, {Header, Message}} ->
	    case Header#gtpp_header.msg_type of
	        version_not_supported ->
		    decode(Remainder, Remainder_len, State#state{version=Header#gtpp_header.version});
		data_record_transfer_request ->
		    {Type, _Content} = hd(Message),
		    case Type of
			send_data_record_packet ->
			    cdr_file_srv:log(SourceKey, Header#gtpp_header.seqnum, Message),
			    send_data_record_transfer_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum,
							       request_accepted, [Header#gtpp_header.seqnum]),
			    decode(Remainder, Remainder_len, State);
			send_potential_duplicate_record_packet ->
			    Resp = case cdr_file_srv:check_duplicate(SourceKey, Header#gtpp_header.seqnum) of
				       false ->
					   cdr_file_srv:log_possible_dup(SourceKey, Header#gtpp_header.seqnum, Message),
					   request_accepted;
				       true ->
					   error_logger:warning_msg("Duplicate sequence number ~p from ~s:~B",
								    [Header#gtpp_header.seqnum,
								     inet_parse:ntoa(State#state.dest_ip), State#state.dest_port]),
					   request_related_to_duplicates_already_fulfilled
				   end,
			    send_data_record_transfer_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum,
							       Resp, [Header#gtpp_header.seqnum]),
			    decode(Remainder, Remainder_len, State);
			cancel_packets ->
			    {cancel_packets, {sequence_numbers, SeqNums}} = hd(Message),
			    ?PRINTDEBUG2("Cancelling packets with seqnums [~s]",['open-cgf_logger':format_seqnums(SeqNums)]),
			    cdr_file_srv:remove_possible_dup(SourceKey, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum,
							       request_accepted, [Header#gtpp_header.seqnum]),
			    decode(Remainder, Remainder_len, State);
			release_packets ->
			    {release_packets, {sequence_numbers, SeqNums}} = hd(Message),
			    ?PRINTDEBUG2("Releasing packets with seqnums [~s]",['open-cgf_logger':format_seqnums(SeqNums)]),
			    cdr_file_srv:commit_possible_dup(SourceKey, Header#gtpp_header.seqnum, SeqNums),
			    send_data_record_transfer_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum,
							       request_accepted, [Header#gtpp_header.seqnum]),
			    decode(Remainder, Remainder_len, State);
			Other ->
			    send_data_record_transfer_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum,
							       invalid_message_format, [Header#gtpp_header.seqnum]),
	                    error_logger:error_msg("Ignored data transfer request with invalid content ~p",[Other]),
			    decode(Remainder, Remainder_len, State)
		    end;
		node_alive_request ->
		    send_node_alive_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum),
		    decode(Remainder, Remainder_len, State);
		node_alive_response ->
		    %% reset timestamp
		    decode(Remainder, Remainder_len, State#state{last_request_ts=0});
		redirection_response ->
		    decode(Remainder, Remainder_len, State);
		echo_request ->
		    send_echo_response(State#state.socket, State#state.version, Header#gtpp_header.seqnum),
		    decode(Remainder, Remainder_len, State);
		echo_response ->
		    decode(Remainder, Remainder_len, State#state{last_request_ts=0})
	    end;
	{error, _Reason} ->
	    %% Send back error of some kind TODO
	    decode(Remainder, Remainder_len, State)
    end.
