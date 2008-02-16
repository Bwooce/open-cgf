%%%-------------------------------------------------------------------
%%% File    : test_udp_client.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 30 Jan 2008 by Bruce Fitzsmons <bruce@fitzsimons.org>
%%%-------------------------------------------------------------------
-module(test_udp_client).

-export([start_sender/1, start/0]).


start() ->
    {ok, Socket} = gen_udp:open(3385, [binary]),
    Address = {{65,23,156,215}, 3386},
    Version = 0,
    ets:new(test, [set, named_table, private]),
    ets:insert(test, {seqnum, 65500}), %% let's test rollover while we are here
    %% send 50 packets
 %   send_cdrs(Socket, Address, Version, 50),
    %% send 50 possible_dups
    send_dup_cdrs(Socket, Address, Version, 50),
    %% cancel 25 possible_dups
    SkipSeqNum = next_seqnum(), %% get current, plus a test to ensure a missing seqnum doesn't hurt
    send_cancel_cdr(Socket, Address, Version, next_seqnum(), lists:seq(SkipSeqNum-51, SkipSeqNum-26)),
    %% release 20 possible_dups (5 remain)
    send_release_cdr(Socket, Address, Version, next_seqnum(), lists:seq(SkipSeqNum-26, SkipSeqNum-6)),
 %   send_cdrs(Socket, Address, Version+1, 50),
 %   send_cdrs(Socket, Address, Version+2, 50),
 %   send_dup_cdr(Socket, Address, 2, 32000), %% wildcard, should stay buffered for a while
 %   send_cdr(Socket, Address, 2, 32001), %% wildcard, should stay buffered for a while
    gen_udp:close(Socket),
    ets:delete(test).

start_sender(Count) ->
    {ok, Socket} = gen_udp:open(3385, [binary]),    
    gen_udp:send(Socket, {65,23,156,215}, 3386, gtpp_encode:echo_request(2,0,<< >> )),
    start2(Socket, Count).
    

start2(Socket, 0) ->
    gen_udp:close(Socket),
    ok;
start2(Socket, Count) ->
    Packet = <<16#0f, 16#f0, 16#01, 16#47, Count:16, 16#7e, 16#01, 16#fc, 16#01,
	       16#42, 16#01, 16#01, 16#15, 16#07, 16#01, 16#3c, 16#b5, 16#82, 16#01, 16#38, 16#80, 16#01, 16#13, 16#83, 16#08,
	       16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#f0, 16#a4, 16#06, 16#80, 16#04, 16#a6, 16#0b, 16#00, 16#0b,
	       16#85, 16#03, 16#2f, 16#42, 16#b8, 16#a7, 16#82, 16#00, 16#06, 16#80, 16#04, 16#0a, 16#0a, 16#5b, 16#15, 16#87,
	       16#07, 16#6d, 16#73, 16#31, 16#2e, 16#61, 16#70, 16#6e, 16#88, 16#02, 16#01, 16#21, 16#a9, 16#08, 16#a0, 16#06,
	       16#80, 16#04, 16#65, 16#32, 16#01, 16#52, 16#8b, 16#01, 16#01, 16#ac, 16#82, 16#00, 16#28, 16#30, 16#82, 16#00,
	       16#24, 16#82, 16#0c, 16#01, 16#09, 16#11, 16#01, 16#21, 16#01, 16#fa, 16#fa, 16#11, 16#05, 16#fa, 16#fa, 16#83,
	       16#02, 16#02, 16#1e, 16#84, 16#02, 16#0a, 16#0e, 16#85, 16#01, 16#02, 16#86, 16#09, 16#07, 16#08, 16#02, 16#11,   
	       16#10, 16#43, 16#2d, 16#04, 16#00, 16#8d, 16#09, 16#07, 16#08, 16#02, 16#11, 16#10, 16#42, 16#2d, 16#04, 16#00,
	       16#8e, 16#01, 16#01, 16#8f, 16#01, 16#00, 16#92, 16#0e, 16#74, 16#62, 16#31, 16#2d, 16#36, 16#35, 16#30, 16#30,   
	       16#62, 16#2d, 16#31, 16#30, 16#2d, 16#32, 16#b3, 16#30, 16#30, 16#16, 16#06, 16#0c, 16#2b, 16#06, 16#01, 16#04,   
	       16#01, 16#09, 16#0a, 16#30, 16#01, 16#02, 16#02, 16#62, 16#81, 16#01, 16#00, 16#a2, 16#03, 16#02, 16#01, 16#06,   
	       16#30, 16#16, 16#06, 16#0c, 16#2b, 16#06, 16#01, 16#04, 16#01, 16#09, 16#0a, 16#30, 16#01, 16#02, 16#02, 16#63,   
	       16#81, 16#01, 16#00, 16#a2, 16#03, 16#02, 16#01, 16#04, 16#94, 16#04, 16#00, 16#af, 16#c8, 16#1b, 16#95, 16#01,   
	       16#00, 16#96, 16#09, 16#91, 16#01, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#f0, 16#97, 16#02, 16#01, 16#00,   
	       16#98, 16#01, 16#04, 16#bf, 16#7f, 16#82, 16#00, 16#5b, 16#30, 16#82, 16#00, 16#57, 16#81, 16#01, 16#67, 16#82,   
	       16#03, 16#4d, 16#53, 16#31, 16#83, 16#02, 16#07, 16#d1, 16#84, 16#01, 16#01, 16#85, 16#09, 16#07, 16#08, 16#02,   
	       16#11, 16#10, 16#34, 16#2d, 16#04, 16#00, 16#86, 16#09, 16#07, 16#08, 16#02, 16#11, 16#10, 16#34, 16#2d, 16#04,   
	       16#00, 16#87, 16#01, 16#01, 16#88, 16#04, 16#00, 16#00, 16#00, 16#10, 16#89, 16#0c, 16#01, 16#09, 16#11, 16#01,   
	       16#21, 16#01, 16#fa, 16#fa, 16#11, 16#05, 16#fa, 16#fa, 16#aa, 16#06, 16#80, 16#04, 16#0a, 16#0a, 16#5b, 16#15,   
	       16#8c, 16#02, 16#02, 16#1e, 16#8d, 16#02, 16#0a, 16#0e, 16#8e, 16#09, 16#07, 16#08, 16#02, 16#11, 16#10, 16#34,   
	       16#2d, 16#04, 16#00>>,

    ok = gen_udp:send(Socket, {65,23,156,215}, 3386, Packet),
    start2(Socket, Count-1).


send_cdrs(_Socket, _Address, _Version, 0) ->
    ok;
send_cdrs(Socket, Address, Version, Count) ->
    send_cdr(Socket, Address, Version, next_seqnum()),
    send_cdrs(Socket, Address, Version, Count-1).

send_cdr(Socket, {DestIP, DestPort}, Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet(["TEST CDR-"++integer_to_list(SeqNum),"TEST CDR2...-"++integer_to_list(SeqNum)], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, data_record_packet, DP, << >>),
    gen_udp:send(Socket, DestIP, DestPort, DRTR).

send_dup_cdrs(_Socket, _Address, _Version, 0) ->
    ok;
send_dup_cdrs(Socket, Address, Version, Count) ->
    send_dup_cdr(Socket, Address, Version, next_seqnum()),
    send_dup_cdrs(Socket, Address, Version, Count-1).

send_dup_cdr(Socket, {DestIP, DestPort}, Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet(["TEST CDR-POTENTIAL_DUP-A-"++integer_to_list(SeqNum),
					    "TEST CDR-POTENTIAL_DUP-B...-"++integer_to_list(SeqNum)], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, send_potential_duplicate_data_record_packet, DP, << >>),
    gen_udp:send(Socket, DestIP, DestPort, DRTR).

send_cancel_cdr(Socket, {DestIP, DestPort}, Version, SeqNum, SeqNums) ->
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, cancel_packets, SeqNums, << >>),
    gen_udp:send(Socket, DestIP, DestPort, DRTR).

send_release_cdr(Socket, {DestIP, DestPort}, Version, SeqNum, SeqNums) ->
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, release_packets, SeqNums, << >>),
    gen_udp:send(Socket, DestIP, DestPort, DRTR).

next_seqnum() ->
    ets:update_counter(test, seqnum, {2,1,65535,0}).
