%%%-------------------------------------------------------------------
%%% File    : gtpp_decode.erl
%%% Author  : root <root@one.w8trk.com>
%%% Description : 
%%%
%%% Created : 28 Jan 2008 by root <root@one.w8trk.com>
%%%-------------------------------------------------------------------
-module(gtpp_decode).


%% API
-export([decode_message/1]).

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
%% mountains of GTP header crap that we'll ignore in here TODO. I presume optional IEs as well. dammit.
		    MSGType:8,
		    MSGLen:16,
		    SeqNum:16,
		    Rest/binary>>) ->
    {#gtpp_header{version=0, pt=0, short_header_for_gtp0_ind=0, 
		 msg_type=decode_msg_type(MSGType), msg_len=MSGLen, seqnum = SeqNum},
     Rest};

decode_GTPP_header(<<Version:3,
		    0:1,
		    _:3,
		    SH:1,
		    MSGType:8,
		    MSGLen:16,
		    SeqNum:16,
		    Rest/binary>>) ->
    {#gtpp_header{version=Version, pt=0, short_header_for_gtp0_ind=SH, 
		 msg_type=decode_msg_type(MSGType), msg_len=MSGLen, seqnum = SeqNum},
     Rest}.



decode_message(Bin) ->
    {Header, Rest} = decode_GTPP_header(Bin),
    case Header#gtpp_header.msg_type of 
	unknown ->
	    % respond with something saying WTF
	    {error, unknown_msg_type};
	echo_request ->
	    {ok, {Header, not_yet_implemented}};
	echo_response ->
	    {ok, {Header, not_yet_implemented}};
	version_not_supported ->
	    {ok, {Header, not_yet_implemented}};
	node_alive_request ->
	    {ok, {Header, not_yet_implemented}};
	node_alive_response ->
	    {ok, {Header, not_yet_implemented}};
	redirection_request ->
	    {ok, {Header, not_yet_implemented}};
	redirection_response ->
	    {ok, {Header, not_yet_implemented}};
	data_record_transfer_request ->
	    MSG = decode_ie(Rest, Header#gtpp_header.msg_len),
	    {ok, {Header, MSG}};
	data_record_transfer_response ->
	    {ok, {Header, not_yet_implemented}};
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
    

%% cause decode - could decode further in future...
decode_ie(<<1:8, Value:8, Rest/binary>>, _Len) ->
    <<Response_Reject_ind:2, _:6>> = Value,
    case Response_Reject_ind of 
	0 -> {{cause, request, Value}, Rest};
	1 -> {{cause, reject, Value}, Rest}; %% unknown, treat as reject as per 29.060 ss 7.7.1
	2 -> {{cause, response, Value}, Rest};
	3 -> {{cause, reject, Value}, Rest}
    end;

%% Data Record Packet
decode_ie(<<126:8, 1:8, Rest/binary>>, Len) ->
    {DRPs, Rest2} = decode_ie_data_record_packet(Rest, Len),
    {{send_data_record_packet, DRPs}, Rest2};
decode_ie(<<126:8, 2:8, Rest/binary>>, Len) ->
    {DRPs, Rest2} = decode_ie_data_record_packet(Rest, Len-2),
    {{send_potential_duplicate_record_packet, DRPs}, Rest2};
decode_ie(<<126:8, 3:8, Rest/binary>>, _Len) ->
    {Seq_nums, Rest2} = decode_ie_sequence_numbers(Rest),
    {{cancel_packets, Seq_nums}, Rest2};
decode_ie(<<126:8, 4:8, Rest/binary>>, _Len) ->
    {Seq_nums, Rest2} = decode_ie_sequence_numbers(Rest),
    {{release_packets, Seq_nums}, Rest2};

%% Charging Gateway Address (or Alternate) 
decode_ie(<<251:8, 4:8, Address:4, Rest/binary>>, _Len) ->
    {{ipv4, Address}, Rest};
decode_ie(<<251:8, 16:8, Address:16, Rest/binary>>, _Len) ->
    {{ipv6, Address}, Rest};

%% Recommended Gateway Address (or Alternate) 
decode_ie(<<254:8, 4:8, Address:4, Rest/binary>>, _Len) ->
    {{ipv4, Address}, Rest};
decode_ie(<<254:8, 16:8, Address:16, Rest/binary>>, _Len) ->
    {{ipv6, Address}, Rest};

decode_ie(<<Other:8, Rest/binary>>, Len) when Other > 127 ->
    Bits = 8*(Len-1),
    <<Content:Bits, Rest2/binary>> = Rest,
    {{private_extension, Content}, Rest2};
decode_ie(<<_Other:8, Length:16, Rest/binary>>, _Len) ->
    Bits =8*Length,
    <<Content:Bits, Rest2>> = Rest,
    {{private_extension, Length, Content}, Rest2}.

decode_ie_data_record_packet(<<252:8, 
			      Len:16, 
			      Num_records:8,
			      1:8, %% only ASN.1 BER (!)
			      Rec_format:16,
			      Rest/binary>>, _TotalLen) ->
    _Decoded_rec_format = decode_ie_data_record_format_version(Rec_format), %% Not going to do anything special with it right now.
    decode_cdrs(Len, Num_records, Rest).

decode_ie_sequence_numbers(<<249:8, Len:16, SeqNum:16, Rest/binary>>) ->
    decode_ie_sequence_numbers(Rest, Len, [SeqNum]);
decode_ie_sequence_numbers(<<250:8, Len:16, SeqNum:16, Rest/binary>>) ->
    decode_ie_sequence_numbers(Rest, Len-2, [SeqNum]);
decode_ie_sequence_numbers(<<253:8, Len:16, SeqNum:16, Rest/binary>>) ->
    decode_ie_sequence_numbers(Rest, Len-2, [SeqNum]).

decode_ie_sequence_numbers(<< >>, _Len, List) ->
    {{sequence_numbers, List}, << >>};
decode_ie_sequence_numbers(Rest, 0, List) ->
    {{sequence_numbers, List}, Rest};
decode_ie_sequence_numbers(<<SeqNum:16, Rest/binary>>, Len, List) ->
    decode_ie_sequence_numbers(Rest, Len-2, List ++ [SeqNum]).
    

decode_ie_data_record_format_version(<<App_ID:4, Release_ID:4, Version:8>>) ->
    {App_ID, Release_ID, Version}.

decode_ie_data_record_transfer_response() ->
     ok.


decode_cdrs(Total_len, Num_records, Bin) ->
    decode_cdrs(Total_len, Num_records, Bin, []).

decode_cdrs(_, 0, _, Acc) ->
    Acc;
decode_cdrs(0, _, _, Acc) ->
    Acc;
decode_cdrs(Total_len, Num_records, <<Rec_len:16, Rest/binary>>, Acc) ->
    Bits = Rec_len*8,
    <<CDR:Bits, Rest2/binary>> = Rest,
    decode_cdrs(Total_len-Rec_len, Num_records=1, Rest2, Acc ++ [CDR]).
    
	   
	    
