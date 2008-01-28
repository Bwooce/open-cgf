
-record(gtp_header, {version,
		    pt,
		    e,
		    s,
		    pn,
		    msg_type,
		    msg_len,
		    tepid,
		    seqnum,
		    n_pdu,
		    extensions}).

decode_GTP_header(Bin) ->
    <<Version:3,
      PT:1,
      _:1,
      E:1,
      S:1,
      PN:1,
      Rest/binary>> = Bin,
    <<MSGType:8, MSGLen:16, TEPID:32, Rest2/binary>> = Rest,
    {SeqNum, Rest3} = case S of 
			  1 -> <<Num:16, Rem/binary>> = Rest2, 
			      {Num, Rem};
			  0 -> {undefined, Rest2}
		      end,
    {N_PDU, Rest4} = case PN of 
			 1 -> <<Num:8, Rem/binary>> = Rest3,
			      {Num, Rem};
			 0 -> {undefined, Rest3}
		     end,
    {Extensions, Rest5} = decode_extension_headers(Rest4),
    {#gtp_header{version=Version, pt=PT, e=E, s=S, pn=PN, msg_type=MSGType, msg_len=MSGLen, tepid=TEPID,
	       seqnum = SeqNum, n_pdu=N_PDU, extensions=Extensions},
     Rest5}.

encode_GTP_header(GTP_header) ->
    Version = GTP_header#gtp_header.version,
    PT = GTP_header#gtp_header.pt,
    E = GTP_header#gtp_header.e,
    S = GTP_header#gtp_header.s,
    PN = GTP_header#gtp_header.pn,
    MSGType = GTP_header#gtp_header.msg_type,
    MSGLen = GTP_header#gtp_header.msg_len,
    TEPID = GTP_header#gtp_header.tepid,
    SeqNum = GTP_header#gtp_header.seqnum,
    N_PDU = GTP_header#gtp_header.n_pdu,
    Extensions = GTP_header#gtp_header.extensions,
    FirstBin = <<Version:3,
		PT:1,
		_:1,
		E:1,
		S:1,
		PN:1,
		MSGType:8,
		MSGLen:16,
		TEPID:32>>,
    SecondBin = case S of
		    1 ->
			<<SeqNum:8>>;
		    0 -> << >>
		end,
    ThirdBin = case PN of
		   1 ->
		       <<N_PDU:8>>;
		   0 -> << >>
	       end,
    FourthBin = encode_extension_headers(Extensions),
    [FirstBin, SecondBin, ThirdBin, FourthBin].


decode_extension_headers(Bin) ->
    decode_extension_headers(Bin, []).

decode_extension_headers(<< >>, Acc) ->
    Acc;
decode_extension_headers(Bin, Acc) ->
    <<Type:8>> = Bin,
    {Ext, Bin2} = decode_extension_type(Type Bin),
    case Ext of
	eol -> {Acc, Bin2};
	_ -> decode_extension_headers(Bin2, Acc ++ [Ext])
    end.

decode_extension_type(2#11000000, Bin) ->
    <<1:8, PDCP1:8, PDCP2:8, Rest/binary>> = Bin,
    {{pdcp, PDCP1, PDCP2}, Rest};
decode_extension_type(2#0, Bin) ->
    {eol, Bin};
decode_extension_type(2#11000001,Bin) ->
    <<1:8, 255:8, 255:8, Rest/binary>> = Bin,
    {{suspend_request},Rest};
decode_extension_type(2#11000010,Bin) ->
    <<1:8, 255:8, 255:8, Rest/binary>> = Bin,
    {{suspend_response},Rest};
decode_extension_type(2#00000001,Bin) ->
    <<1:8, 255:8, 255:8, Rest/binary>> = Bin,
    {{mbms_support},Rest};
decode_extension_type(2#00000010,Bin) ->
    <<1:8, 255:8, 255:8, Rest/binary>> = Bin,
    {{ms_info_cr_ind},Rest};
decode_extension_type(_,Bin) ->
    <<Len:8, Rest/binary>> = Bin,
    ByteLen = (Len*4)-1, %% leave the next ext header ind alone
    <<_:ByteLen/binary, Rest2/binary>> = Rest, %% throw away extension
    {{unsupported},Rest}.

decode_GTP_body(GTPHeader, BinRest) ->
    decode_GTP_message_body(GTPHeader#gtp_header.msg_type, GTPHeader#gtp_header.msg_len, BinRest).

decode_GTP_message_body(0,Len,BinRest) ->
    <<_:Len/binary, Remainder/binary>> = BinRest,
    {none, Remainder};
decode_GTP_message_body(1,0,BinRest) -> %% no ext
    {{echo_request}, BinRest};
decode_GTP_message_body(1,Len,BinRest) -> 
    <<255:8, ExLen:16, Remainder/binary>> = BinRest,
    <<ExtId:16, ExtValue:ExLen/binary, Remainder2/binary>> = Remainder,
    {{echo_request, ExtId, ExtValue}, Remainder2};
decode_GTP_message_body(2,2,BinRest) ->
    <<14:8, Counter:8, Remainder/binary>> = BinRest,
    {{echo_response, Counter},Remainder};
decode_GTP_message_body(2,Len,BinRest) ->
    <<14:8, Counter:8, ExLen:16, Remainder/binary>> = BinRest,
    <<ExtId:16, ExtValue:ExLen/binary, Remainder2/binary>> = Remainder,
    {{echo_response, Counter, ExtId, ExtValue}, Remainder2};
decode_GTP_message_body(3,Len,BinRest) -> %% version not supported
    {{version_not_supported}, BinRest};
decode_GTP_message_body(4) -> %% node_alive_request
    <<node_address, opt_alt_node_address, opt_ext>>;
decode_GTP_message_body(5) -> %% node_alive_response
    <<opt_ext>>;
decode_GTP_message_body(6) -> %% redirection_request
    <<cause, opt_recommended_node_address, opt_recommended_node_alt_address, opt_ext>>;
decode_GTP_message_body(7) -> %% redirection_response
    <<cause_resp, opt_ext>>;
decode_GTP_message_body(240) -> %% data_transfer_request
    <<126:8. Packet_transfer_command:8>>,
    case Packet_transfer_command of

    1 = packet, 2= possible_dup_packet, 3 = cancel_seq_nums, 4= release_seq_nums
decode_GTP_message_body(241) -> %% data_transfer_response
    <<cause_response, requests_responded, opt_ext>>
 
			      
	
decode_node_address(). 

