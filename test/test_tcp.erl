%%%-------------------------------------------------------------------
%%% File    : test_tcp.erl
%%% Author  : bruce@3gtelcotools.com
%%% Description : 
%%%
%%% Created : 18 May 2008 by root <root@one.w8trk.com>
%%%-------------------------------------------------------------------
-module(test_tcp).

-include("open-cgf.hrl").

-export([simple_test/2,test/0]).

-define(TIMEOUT, 60*1000).
-define(MAXATTEMPTS, 3).

simple_test(Origin, Dest) ->
    common_setup(tcp, Origin),
    test_client:auto_respond(true),
    test_client:open(Dest),
    test_client:expect('_'),
    test_client:send(gtpp_encode:echo_request(0, next_seqnum(), << >>)),
    test_client:expect({'_',[{cause, response, 128},{sequence_numbers,[cur_seqnum()]},'_']}), %% requires some refinement...
    test_client:send(dummy_cdr(0,next_seqnum())),
    common_shutdown(), %% even if the test fails
    test_client:wait_for_all_expected(10),
    ?PRINTDEBUG("Sent all, waiting for all messages"),
    timer:sleep(2000),
    test_client:close().

test() ->
    P1 = {'_',[{cause, response, 128},{sequence_numbers,'_'},'_']},
    Message = {{gtpp_header,2,0,1,data_record_transfer_response,7,65502},
             [{cause,response,128},{sequence_numbers,[65502]},<<>>]},
    ets:test_ms(Message, [{P1,[],['$_']}]).

common_setup(Proto, Origin) ->
    test_client:start_link(Proto, Origin),
    ets:new(test_tcp, [set, named_table, private]),
    ets:insert(test_tcp, {seqnum, 65500}), %% let's test rollover while we are here
    ok.

common_shutdown() ->
    ets:delete(test_tcp),
    ok.

next_seqnum() ->
    ets:update_counter(test_tcp, seqnum, {2,1,65535,0}).

cur_seqnum() ->
    [ets:lookup(test_tcp, seqnum)].

dummy_cdr(Version, SeqNum) ->
    DP = gtpp_encode:ie_data_record_packet(["TEST CDR-"++integer_to_list(SeqNum),"TEST CDR2...-"++integer_to_list(SeqNum)], {1,1,1}, << >>),
    DRTR = gtpp_encode:data_record_transfer_request(Version, SeqNum, data_record_packet, DP, << >>),
    DRTR.

%%%----- old stuff
