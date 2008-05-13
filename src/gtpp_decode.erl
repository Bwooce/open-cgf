%%%-------------------------------------------------------------------
%%% File    : gtpp_decode.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 28 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module(gtpp_decode).


%% API
-export([decode_message/1, test/0, test2/0]).

-include("open-cgf.hrl").
-include("gtp.hrl").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%% long header, only for GTP0 when the indicator is 0. Weirdass protocol.
decode_GTPP_header(<<0:3,
		    0:1,
		    _:3,
		    0:1,
		    MSGType:8,
		    MSGLen:16,
		    SeqNum:16,
		    _:14/binary,
		    Rest/binary>>) ->
    {#gtpp_header{version=0, pt=0, modern_header=0, 
		 msg_type=decode_msg_type(MSGType), msg_len=MSGLen, seqnum = SeqNum},
     Rest};

decode_GTPP_header(<<Version:3,
		    0:1,
		    _:3,
		    _SH:1,
		    MSGType:8,
		    MSGLen:16,
		    SeqNum:16,
		    Rest/binary>>) ->
    {#gtpp_header{version=Version, pt=0, modern_header=1, 
		 msg_type=decode_msg_type(MSGType), msg_len=MSGLen, seqnum = SeqNum},
     Rest}.



decode_message(Bin) ->
    {Header, Rest} = decode_GTPP_header(Bin),
    case Header#gtpp_header.msg_type of 
	unknown ->
	    % respond with something saying WTF
	    {error, unknown_msg_type};
	echo_request -> 
	    {ok, {Header, {none, Rest}}};
	echo_response ->
	    {Decode, Rest2} = decode_ies(Rest, Header#gtpp_header.msg_len),
	    {ok, {Header, {Decode, Rest2}}};
	version_not_supported ->
	    {ok, {Header, {none, Rest}}};
	node_alive_request ->
	    {Response, Rest2} = decode_ies(Rest,  Header#gtpp_header.msg_len), %% TODO, cope with alternate node address
	    {ok, {Header, {Response, Rest2}}};
	node_alive_response ->
	    {ok, {Header, {none, Rest}}};
	redirection_request ->
	    {Decode, Rest2} = decode_ies(Rest, Header#gtpp_header.msg_len),
	    {ok, {Header, {Decode, Rest2}}}; %% TODO, cope with addresses not just cause
	redirection_response ->
	    {Decode, Rest2} = decode_ies(Rest, Header#gtpp_header.msg_len),
	    {ok, {Header, {Decode, Rest2}}};
	data_record_transfer_request ->
	    Response = decode_ies(Rest, Header#gtpp_header.msg_len),
	    {ok, {Header, Response}};
	data_record_transfer_response ->
	    {Response, Rest2} = decode_ies(Rest, Header#gtpp_header.msg_len),
%%	    {Response, Rest3} = decode_ie(Rest2, Header#gtpp_header.msg_len-2),
	    {ok, {Header, {Response, Rest2}}};
	invalid_msg_type ->
	    %% as per unknown
	    {error, invalid_msg_type}
    end. 



%%====================================================================
%% Internal functions
%%====================================================================

decode_msg_type(0) -> unknown;
decode_msg_type(1) -> echo_request;
decode_msg_type(2) -> echo_response;
decode_msg_type(3) -> version_not_supported;
decode_msg_type(4) -> node_alive_request;
decode_msg_type(5) -> node_alive_response;
decode_msg_type(6) -> redirection_request;
decode_msg_type(7) -> redirection_response;
decode_msg_type(240) -> data_record_transfer_request;
decode_msg_type(241) -> data_record_transfer_response;
decode_msg_type(_) -> invalid_msg_type. %% not valid for GTP' at least.
    
%% TODO - decide if gtpp_decode should really return a tuple (implying order) 
%% or a tagged list considering we're decoding in any order.
decode_ies(Bin, Len) ->
    decode_ies2(Bin, Len, {}).
decode_ies2(<< >>, _, Acc) ->
    ?PRINTDEBUG2("Finished processing IEs, Acc is ~p",[Acc]),
    erlang:append_element(Acc, << >>);
decode_ies2(Rest, 0, Acc) ->
    ?PRINTDEBUG2("Finished processing IEs (len remain=0), Acc is ~p, Bin=~p",[Acc,Rest]),
    erlang:append_element(Acc, Rest);
decode_ies2(Bin, Len, Acc) ->
    {Decode, Rest} = decode_ie(Bin, Len),
    decode_ies2(Rest, size(Rest), erlang:append_element(Acc, Decode)).

%% cause decode - could decode further in future...
decode_ie(<<1:8, Value:8, Rest/binary>>, _Len) ->
    ?PRINTDEBUG2("Decoding cause ~p", [Value]),
    <<Response_Reject_ind:2, _:6>> = <<Value:8>>,
    ?PRINTDEBUG("Decoded indicators"),
    case Response_Reject_ind of 
	0 -> {{cause, request, Value}, Rest};
	1 -> {{cause, reject, Value}, Rest}; %% unknown, treat as reject as per 29.060 ss 7.7.1
	2 -> {{cause, response, Value}, Rest};
	3 -> {{cause, reject, Value}, Rest}
    end;

%% restart counter
decode_ie(<<14:8, Count:8, Rest/binary>>, _Len) ->
    {{count, Count}, Rest};

%% Data Record Packet
decode_ie(<<126:8, 1:8, Rest/binary>>, Len) ->
    {DRPs, Rest2} = decode_ie_data_record_packet(Rest, Len),
    {{send_data_record_packet, DRPs}, Rest2};
decode_ie(<<126:8, 2:8, Rest/binary>>, Len) ->
    {DRPs, Rest2} = decode_ie_data_record_packet(Rest, Len-2),
    {{send_potential_duplicate_record_packet, DRPs}, Rest2};
decode_ie(<<126:8, 3:8, Rest/binary>>, _Len) ->
    {Seq_nums, Rest2} = decode_ie(Rest,0),
    {{cancel_packets, Seq_nums}, Rest2};
decode_ie(<<126:8, 4:8, Rest/binary>>, _Len) ->
    {Seq_nums, Rest2} = decode_ie(Rest,0),
    {{release_packets, Seq_nums}, Rest2};

%% Charging Gateway Address (or Alternate) 
decode_ie(<<251:8, 4:16, Address:4, Rest/binary>>, _Len) ->
    {{ipv4, Address}, Rest};
decode_ie(<<251:8, 16:16, Address:16, Rest/binary>>, _Len) ->
    {{ipv6, Address}, Rest};

%% Recommended Gateway Address (or Alternate) 
decode_ie(<<254:8, 4:16, Address:4, Rest/binary>>, _Len) ->
    {{ipv4, Address}, Rest};
decode_ie(<<254:8, 16:16, Address:16, Rest/binary>>, _Len) ->
    {{ipv6, Address}, Rest};

decode_ie(<<249:8, Len:16, SeqNum:16, Rest/binary>>, _Len) ->
    decode_ie_sequence_numbers(Rest, Len, [SeqNum]);

decode_ie(<<250:8, Len:16, SeqNum:16, Rest/binary>>, _Len) ->
    decode_ie_sequence_numbers(Rest, Len, [SeqNum]);
decode_ie(<<253:8, Len:16, SeqNum:16, Rest/binary>>, _Len) ->
    decode_ie_sequence_numbers(Rest, Len, [SeqNum]);

decode_ie(<<252:8, Rest/binary>>, Len) -> %% pre  R99 GTP' CDR transfer
    {DRPs, Rest2} = decode_ie_data_record_packet(<<252:8, Rest/binary>>, Len),
    {{send_data_record_packet, DRPs}, Rest2};    

decode_ie(<<Other:8, Rest/binary>>, Len) when Other > 127 ->
    ?PRINTDEBUG2("Got private_extension (>127) type ~p of length ~p",[Other, Len]),
    Bytes = Len-1,
    <<Content:Bytes/binary, Rest2/binary>> = Rest,
    {{private_extension, Other, Bytes, Content}, Rest2};
decode_ie(<<Other:8, Length:16, Rest/binary>>, _Len) ->
    ?PRINTDEBUG2("Got private_extension type ~p of length ~p",[Other, Length]),
    <<Content:Length/binary, Rest2>> = Rest,
    {{private_extension, Other, Length, Content}, Rest2}.


decode_ie_data_record_packet(<<252:8, 
			      Len:16, 
			      Num_records:8,
			      $1:8, %% only ASN.1 BER (!) encoded as ascii
			      Rec_format:2/binary,
			      Rest/binary>>, _TotalLen) ->
    _Decoded_rec_format = decode_ie_data_record_format_version(Rec_format), %% Not going to do anything special with it right now.
    decode_cdrs(Len, Num_records, Rest);

decode_ie_data_record_packet(<<252:8, 
			      Len:16, 
			      Num_records:8,
			      1:8, %% only ASN.1 BER (!) encoded as decimal. ONE OF THESE IS A BUG...encode uses $1
			      Rec_format:2/binary,
			      Rest/binary>>, _TotalLen) ->
    _Decoded_rec_format = decode_ie_data_record_format_version(Rec_format), %% Not going to do anything special with it right now.
    decode_cdrs(Len, Num_records, Rest).

decode_ie_sequence_numbers(<< >>, _Len, List) ->
    {{sequence_numbers, List}, << >>};
decode_ie_sequence_numbers(Rest, 0, List) ->
    {{sequence_numbers, List}, Rest};
decode_ie_sequence_numbers(<<SeqNum:16, Rest/binary>>, Len, List) ->
    decode_ie_sequence_numbers(Rest, Len-2, List ++ [SeqNum]).
    

decode_ie_data_record_format_version(<<App_ID:4, Release_ID:4, Version:8>>) ->
    {App_ID, Release_ID, Version}.


decode_cdrs(Total_len, Num_records, Bin) ->
    ?PRINTDEBUG2("Decoding CDRs, total_len ~p, count ~p",[Total_len, Num_records]),
    decode_cdrs(Total_len, Num_records, Bin, []).

decode_cdrs(_, 0, LeftOver, Acc) ->
    ?PRINTDEBUG("Finished extraction of CDRs"),
    {Acc, LeftOver};
decode_cdrs(0, _, LeftOver, Acc) ->
    {Acc, LeftOver};
decode_cdrs(Total_len, Num_records, <<Rec_len:16, Rest/binary>>, Acc) ->
    ?PRINTDEBUG2("Looking for a CDR of length ~p",[Rec_len]),
%    Bits = Rec_len*8,
    <<CDR:Rec_len/binary, Rest2/binary>> = Rest,
    ?PRINTDEBUG("Extracted CDR"),
    decode_cdrs(Total_len-Rec_len, Num_records-1, Rest2, Acc ++ [CDR]).
    
	   
	    
test() -> 
  Msg = <<15,240,2,229,0,0,252,2,226,2,1,21,7,1,244,181,
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
                              34,0,33,0,4>>,
	D = decode_message(Msg),
	io:format("~p~n",[D]).

test2() ->
	Msg = <<15,2,0,2,0,1,14,7>>,
	D = decode_message(Msg),
        io:format("~p~n",[D]).

