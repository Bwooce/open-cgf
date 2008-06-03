%%%-------------------------------------------------------------------
%%% File    : test_udp.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 28 May 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module(test_udp).

-include("open-cgf.hrl").
-include("gtp.hrl").

-export([simple_test/2]).

-define(TIMEOUT, 60*1000).
-define(MAXATTEMPTS, 3).

simple_test(Origin, Dest) ->
    common_setup(udp, Origin),
    test_client:auto_respond(true),
    test_client:open(Dest),
    test_client:expect({simple_header(next_seqnum(), echo_response),'_'}),
    ?PRINTDEBUG2("curr seqnum ~p",[cur_seqnum()]),
    test_client:send(gtpp_encode:echo_request(0, cur_seqnum(), << >>)),
    receive
	ok_ ->
	    ok
    after 10000 ->
	    ok
    end,
    test_client:expect({'_',[{cause, response, 128},{sequence_numbers,[next_seqnum()]},'_']}), %% requires some refinement...
    test_client:send(dummy_cdr(0,cur_seqnum())),
    test_client:expect({'_',[{cause, response, 128},{sequence_numbers,[next_seqnum()]},'_']}), %% requires some refinement...
    test_client:send(unknown_cdr_seqnum(cur_seqnum())),
    test_client:expect({'_',[{cause, response, 128},{sequence_numbers,'_'},'_']}), %% requires some refinement...
    test_client:send(ggsn_cdr_fixed()),
    common_shutdown(), %% even if the test fails
    test_client:wait_for_all_expected(10),
    ?PRINTDEBUG("Sent all, waiting for all messages"),
    timer:sleep(2000),
    test_client:close().

common_setup(Proto, Origin) ->
    ?PRINTDEBUG2("Opening port for ~p",[Origin]),
    test_client:start_link(Proto, Origin),
    ets:new(test_udp, [set, named_table, private]),
    ets:insert(test_udp, {seqnum, 65500}), %% let's test rollover while we are here
    ok.

common_shutdown() ->
    ets:delete(test_udp),
    ok.

next_seqnum() ->
    ets:update_counter(test_udp, seqnum, {2,1,65535,0}).

cur_seqnum() ->
    [{seqnum, SeqNum}] = ets:lookup(test_udp, seqnum),
    SeqNum.

simple_header(Seqnum, Type) ->
    #gtpp_header{version='_',
		 pt='_',
		 modern_header='_',
		 msg_type=Type,
		 msg_len='_',
		 seqnum=Seqnum
		}.

dummy_cdr(Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet(["TEST CDR-"++integer_to_list(SeqNum),"TEST CDR2...-"++integer_to_list(SeqNum)], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, data_record_packet, DP, << >>),
    DRTR.

unknown_cdr_seqnum(SeqNum) ->
    <<16#0f, 16#f0, 16#01, 16#47, SeqNum:16, 16#7e, 16#01, 16#fc, 16#01,
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
     16#2d, 16#04, 16#00>>.

ggsn_cdr_fixed() ->
    <<15,240,2,229,0,0,252,2,226,2,1,21,7,1,244,181,
     130,1,240,128,1,19,131,7,53,0,114,82,134,8,241,
     164,6,128,4,166,179,11,40,133,3,77,7,212,166,
     130,0,6,128,4,10,207,35,181,135,8,105,110,116,
     101,114,110,101,116,136,2,1,33,169,8,160,6,128,
     4,166,179,10,68,139,1,1,172,130,0,40,48,130,0,
     36,130,12,1,9,17,1,41,1,1,1,17,5,1,1,131,2,10,
     120,132,2,30,240,133,1,2,134,9,8,5,1,1,82,71,43,
     0,0,141,9,8,5,1,1,73,70,43,0,0,142,2,0,181,143,
     1,20,146,15,77,79,68,95,85,77,84,83,95,71,71,83,
     78,48,56,179,48,48,22,6,12,43,6,1,4,1,9,10,48,1,
     2,2,98,129,1,0,162,3,2,1,39,48,22,6,12,43,6,1,4,
     1,9,10,48,1,2,2,99,129,1,0,162,3,2,1,58,148,4,1,
     51,158,18,149,1,0,150,6,145,114,82,134,8,241,
     151,2,11,0,152,1,1,158,1,1,159,32,8,0,17,33,34,
     0,33,0,4,191,127,130,1,6,48,130,0,89,129,1,1,
     130,2,49,49,131,2,7,209,132,1,1,133,9,8,5,1,1,
     73,71,43,0,0,134,9,8,5,1,1,73,86,43,0,0,135,1,9,
     136,4,0,0,32,0,137,12,1,9,17,1,41,1,1,1,17,5,1,
     1,170,6,128,4,10,207,35,181,140,2,10,120,141,2,
     30,240,142,9,8,5,1,1,73,86,43,0,0,143,1,1,48,
     130,0,73,129,1,1,130,2,49,49,131,2,7,209,132,1,
     2,133,9,0,1,1,0,0,0,43,0,0,134,9,0,1,1,0,0,0,43,
     0,0,135,1,1,136,4,0,0,0,16,170,6,128,4,10,207,
     35,181,140,1,0,141,1,0,142,9,8,5,1,1,73,86,43,0,
     0,143,1,1,48,130,0,88,129,2,3,232,130,2,49,49,
     131,2,7,209,132,1,1,133,9,0,1,1,0,0,0,43,0,0,
     134,9,0,1,1,0,0,0,43,0,0,135,1,0,136,4,0,0,2,0,
     137,12,1,9,17,1,41,1,1,1,17,5,1,1,170,6,128,4,
     10,207,35,181,140,1,0,141,1,0,142,9,0,1,1,0,0,0,
     43,0,0,143,1,1,0,230,181,130,0,226,128,1,19,131,
     7,53,0,114,82,134,8,241,164,6,128,4,166,179,11,
     40,133,3,77,7,213,166,130,0,6,128,4,10,207,35,
     181,135,8,105,110,116,101,114,110,101,116,136,2,
     1,33,169,8,160,6,128,4,166,179,10,68,139,1,1,
     172,130,0,38,48,130,0,34,130,12,1,9,17,1,41,1,1,
     1,17,5,1,1,131,1,0,132,1,0,133,1,2,134,9,8,5,1,
     1,83,1,43,0,0,141,9,8,5,1,1,83,0,43,0,0,142,1,1,
     143,1,0,146,15,77,79,68,95,85,77,84,83,95,71,71,
     83,78,48,56,179,48,48,22,6,12,43,6,1,4,1,9,10,
     48,1,2,2,98,129,1,0,162,3,2,1,0,48,22,6,12,43,6,
     1,4,1,9,10,48,1,2,2,99,129,1,0,162,3,2,1,0,148,
     4,1,51,158,19,149,1,0,150,6,145,114,82,134,8,
     241,151,2,11,0,152,1,1,158,1,1,159,32,8,0,17,33,
     34,0,33,0,4>>.

%%%----- old stuff

     
start_mockup(Address) -> %% Address is {IP,Port}, IP is {aa,bb,cc,dd}
%%    {ok, Socket} = gen_udp:open(3385, [binary]),
    Socket = undefined,
%%    Address = {{65,23,156,215}, 3386},
    Version = 0,
    ets:new(test, [set, named_table, private]),
    ets:insert(test, {seqnum, 65500}), %% let's test rollover while we are here
    %% send 50 packets
    %send_cdrs(Socket, Address, Version, 50),
    %% send 50 possible_dups
    send_dup_cdrs(Socket, Address, Version, 50),
    %% cancel 25 possible_dups
    SkipSeqNum = next_seqnum(), %% get current, plus a test to ensure a missing seqnum doesn't hurt
    send_cancel_cdr(Socket, Address, Version, next_seqnum(), lists:seq(SkipSeqNum-51, SkipSeqNum-26)),
    %% release 20 possible_dups (5 remain)
    send_release_cdr(Socket, Address, Version, next_seqnum(), lists:seq(SkipSeqNum-26, SkipSeqNum-6)),
    %send_cdrs(Socket, Address, Version+1, 50),
    %send_cdrs(Socket, Address, Version+2, 50),
    send_dup_cdr(Socket, Address, 2, 32000), %% wildcard, should stay buffered for a while
    send_empty_cdr(Socket, Address, 2, 32001), %% wildcard, should stay buffered for a while
    gen_udp:close(Socket),
    ets:delete(test).

start_replay({IP,Port}, Count) ->
    {ok, Socket} = gen_udp:open(3385, [binary]),    
    gen_udp:send(Socket, IP, Port, gtpp_encode:echo_request(2,0,<< >> )),
    start2(IP, Port, Socket, Count).
    

start2(_IP, _Port, Socket, 0) ->
    gen_udp:close(Socket),
    ok;
start2(IP, Port, Socket, Count) ->
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

    ok = gen_udp:send(Socket, IP, Port, Packet),
    start2(IP, Port, Socket, Count-1).

replay_real({IP,Port}) ->
    {ok, Socket} = gen_udp:open(3385, [binary]),    
    gen_udp:send(Socket, IP, Port, gtpp_encode:echo_request(0,0,<< >> )), %% make sure the recipient is awake
    gen_udp:send(Socket, IP, Port, <<15,2,0,2,0,1,14,7>>), %% echo_response v0 with restart counter of 7
    gen_udp:send(Socket, IP, Port, <<15,240,2,229,0,0,252,2,226,2,1,21,7,1,244,181,
                              130,1,240,128,1,19,131,7,53,0,114,82,134,8,241,
                              164,6,128,4,166,179,11,40,133,3,77,7,212,166,
                              130,0,6,128,4,10,207,35,181,135,8,105,110,116,
                              101,114,110,101,116,136,2,1,33,169,8,160,6,128,
                              4,166,179,10,68,139,1,1,172,130,0,40,48,130,0,
                              36,130,12,1,9,17,1,41,1,1,1,17,5,1,1,131,2,10,
                              120,132,2,30,240,133,1,2,134,9,8,5,1,1,82,71,43,
                              0,0,141,9,8,5,1,1,73,70,43,0,0,142,2,0,181,143,
                              1,20,146,15,77,79,68,95,85,77,84,83,95,71,71,83,
                              78,48,56,179,48,48,22,6,12,43,6,1,4,1,9,10,48,1,
                              2,2,98,129,1,0,162,3,2,1,39,48,22,6,12,43,6,1,4,
                              1,9,10,48,1,2,2,99,129,1,0,162,3,2,1,58,148,4,1,
                              51,158,18,149,1,0,150,6,145,114,82,134,8,241,
                              151,2,11,0,152,1,1,158,1,1,159,32,8,0,17,33,34,
                              0,33,0,4,191,127,130,1,6,48,130,0,89,129,1,1,
                              130,2,49,49,131,2,7,209,132,1,1,133,9,8,5,1,1,
                              73,71,43,0,0,134,9,8,5,1,1,73,86,43,0,0,135,1,9,
                              136,4,0,0,32,0,137,12,1,9,17,1,41,1,1,1,17,5,1,
                              1,170,6,128,4,10,207,35,181,140,2,10,120,141,2,
                              30,240,142,9,8,5,1,1,73,86,43,0,0,143,1,1,48,
                              130,0,73,129,1,1,130,2,49,49,131,2,7,209,132,1,
                              2,133,9,0,1,1,0,0,0,43,0,0,134,9,0,1,1,0,0,0,43,
                              0,0,135,1,1,136,4,0,0,0,16,170,6,128,4,10,207,
                              35,181,140,1,0,141,1,0,142,9,8,5,1,1,73,86,43,0,
                              0,143,1,1,48,130,0,88,129,2,3,232,130,2,49,49,
                              131,2,7,209,132,1,1,133,9,0,1,1,0,0,0,43,0,0,
                              134,9,0,1,1,0,0,0,43,0,0,135,1,0,136,4,0,0,2,0,
                              137,12,1,9,17,1,41,1,1,1,17,5,1,1,170,6,128,4,
                              10,207,35,181,140,1,0,141,1,0,142,9,0,1,1,0,0,0,
                              43,0,0,143,1,1,0,230,181,130,0,226,128,1,19,131,
                              7,53,0,114,82,134,8,241,164,6,128,4,166,179,11,
                              40,133,3,77,7,213,166,130,0,6,128,4,10,207,35,
                              181,135,8,105,110,116,101,114,110,101,116,136,2,
                              1,33,169,8,160,6,128,4,166,179,10,68,139,1,1,
                              172,130,0,38,48,130,0,34,130,12,1,9,17,1,41,1,1,
                              1,17,5,1,1,131,1,0,132,1,0,133,1,2,134,9,8,5,1,
                              1,83,1,43,0,0,141,9,8,5,1,1,83,0,43,0,0,142,1,1,
                              143,1,0,146,15,77,79,68,95,85,77,84,83,95,71,71,
                              83,78,48,56,179,48,48,22,6,12,43,6,1,4,1,9,10,
                              48,1,2,2,98,129,1,0,162,3,2,1,0,48,22,6,12,43,6,
                              1,4,1,9,10,48,1,2,2,99,129,1,0,162,3,2,1,0,148,
                              4,1,51,158,19,149,1,0,150,6,145,114,82,134,8,
                              241,151,2,11,0,152,1,1,158,1,1,159,32,8,0,17,33,
                              34,0,33,0,4>>), %% Cisco GGSN sending pre99 CDRs x2
    gen_udp:close(Socket).
    
    

send_cdrs(_Socket, _Address, _Version, 0) ->
    ok;
send_cdrs(Socket, Address, Version, Count) ->
%    send_cdr(Socket, Address, Version, next_seqnum()),
    send_cdrs(Socket, Address, Version, Count-1).



send_empty_cdr(Socket, {DestIP, DestPort}, Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet([], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, data_record_packet, DP, << >>),
    send_reliably(Socket, {DestIP, DestPort}, SeqNum, DRTR, ?TIMEOUT, ?MAXATTEMPTS).

send_dup_cdrs(_Socket, _Address, _Version, 0) ->
    ok;
send_dup_cdrs(Socket, Address, Version, Count) ->
    send_dup_cdr(Socket, Address, Version, next_seqnum()),
    send_dup_cdrs(Socket, Address, Version, Count-1).

send_dup_cdr(Socket, {DestIP, DestPort}, Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet(["TEST CDR-POTENTIAL_DUP-A-"++integer_to_list(SeqNum),
					    "TEST CDR-POTENTIAL_DUP-B...-"++integer_to_list(SeqNum)], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, send_potential_duplicate_data_record_packet, DP, << >>),
    send_reliably(Socket, {DestIP, DestPort}, SeqNum, DRTR, ?TIMEOUT, ?MAXATTEMPTS).

send_cancel_cdr(Socket, {DestIP, DestPort}, Version, SeqNum, SeqNums) ->
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, cancel_packets, SeqNums, << >>),
    send_reliably(Socket, {DestIP, DestPort}, SeqNum, DRTR, ?TIMEOUT, ?MAXATTEMPTS).

send_release_cdr(Socket, {DestIP, DestPort}, Version, SeqNum, SeqNums) ->
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, release_packets, SeqNums, << >>),
    send_reliably(Socket, {DestIP, DestPort}, SeqNum, DRTR, ?TIMEOUT, ?MAXATTEMPTS).


send_reliably(Socket, Dest, SeqNum, Msg, Timeout, MaxAttempts) ->
    spawn_link(?MODULE, send_udp, [self(), Socket, Dest, SeqNum, Msg, Timeout, MaxAttempts]).

send_udp(OwnerPid, _Socket, Dest, SeqNum, _Msg, _Timeout, 0) ->
    OwnerPid ! {timeout, Dest, SeqNum};
send_udp(OwnerPid, Socket, {IP,Port}, SeqNum, Msg, Timeout, MaxAttempts) ->
    ok = gen_udp:send(Socket, IP, Port, Msg),
    receive
	ack -> ok %% die a sweet death knowing that all is right with the world		    
    after Timeout*1000 ->
	    send_udp(OwnerPid, Socket, {IP,Port}, SeqNum, Msg, Timeout*2, MaxAttempts-1) %% back off on send
    end.

reliable_ack(Src, SeqNum, List) ->
   case lists:member({Src, SeqNum}, List) of
	{value, Pid} -> 
	   Pid ! ack,
	   lists:delete({Src, SeqNum}, List);
	_ -> ok
   end.
