%%%-------------------------------------------------------------------
%%% File    : gtpp_encode.erl
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
-module(gtpp_encode).

-export([echo_request/3, echo_response/4, version_not_supported/2]).
-export([node_alive_request/4, node_alive_request/5, node_alive_response/3]).
-export([redirection_request/4, redirection_request/5, redirection_request/6, redirection_response/2, redirection_response/4]).
-export([data_record_transfer_request/5, data_record_transfer_response/5]).

-include("open-cgf.hrl").
-include("gtp.hrl").

header(Header) ->
    case Header#gtpp_header.version of
	0 ->
	    case Header#gtpp_header.modern_header of
		0 ->
		    old_header(Header);
		_ ->
		    modern_header(Header)
	    end;
	1 ->
	    modern_header(Header);
	2 ->
	    modern_header(Header)
    end.

modern_header(Header) ->
    Version = Header#gtpp_header.version,
    MsgType = Header#gtpp_header.msg_type,
    MsgLen = Header#gtpp_header.msg_len,
    SeqNum = Header#gtpp_header.seqnum,
    <<Version:3, 
      0:1, %% GTP'
      7:3, %% defined as unused, all 1's
      0:1, %% modern header if GTPv0
      MsgType:8,
      MsgLen:16,
      SeqNum:16>>.

old_header(_Header) ->
    not_yet_implemented. %% TODO

echo_request(Version, SeqNum, Extensions) ->
    ?PRINTDEBUG("echo_request"),
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(echo_request), 
			  msg_len=size(Extensions), seqnum=SeqNum}),
    <<H/binary, Extensions/binary>>.    

echo_response(Version, SeqNum, Counter, Extensions) ->
    ?PRINTDEBUG("echo_response"),
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(echo_response), 
			    msg_len=size(Extensions)+2, %% 2 for the restart_counter
			    seqnum=SeqNum}),
    B = restart_counter(Counter),
    <<H/binary, B/binary, Extensions/binary>>.

version_not_supported(Version, SeqNum) ->
    ?PRINTDEBUG("version_not_supported/2"),
    header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(version_not_supported), msg_len=0, seqnum=SeqNum}).

node_alive_request(Version, SeqNum, NodeAddress, Extensions) ->
    ?PRINTDEBUG("node_alive_request/4"),
    NA = charging_gateway_address(NodeAddress),
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(node_alive_request), 
			    msg_len=size(NA)+size(Extensions), seqnum=SeqNum}),
    <<H/binary, NA/binary, Extensions/binary>>.

node_alive_request(Version, SeqNum, NodeAddress, AltNodeAddress, Extensions) ->
        ?PRINTDEBUG("node_alive_request/5"),
    NA = charging_gateway_address(NodeAddress),
    ANA = charging_gateway_address(AltNodeAddress),
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(node_alive_request), 
			    msg_len=size(NA)+size(ANA)+size(Extensions), seqnum=SeqNum}),
    <<H/binary, NA/binary, ANA/binary, Extensions/binary>>.

node_alive_response(Version, SeqNum, Extensions) ->
    ?PRINTDEBUG("node_alive_response/3"),
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(node_alive_response), 
			    msg_len=size(Extensions), seqnum=SeqNum}),
    <<H/binary, Extensions/binary>>.


redirection_request(Version, SeqNum, Cause, Extensions)->
    ?PRINTDEBUG("redirection_request/4"),
    C = cause(Cause),
    B = <<C:8, Extensions/binary>>,
    redirection_request2(Version, SeqNum, B).
redirection_request(Version, SeqNum, Cause, NodeAddress, Extensions) ->
    ?PRINTDEBUG("redirection_request/5"),
    C = cause(Cause),
    NA = recommended_node_address(NodeAddress),
    B = <<C:8, NA/binary, Extensions/binary>>,
    redirection_request2(Version, SeqNum, B).
redirection_request(Version, SeqNum, Cause, NodeAddress, AltNodeAddress, Extensions) ->
    ?PRINTDEBUG("redirection_request/6"),
    C = cause(Cause),
    NA = recommended_node_address(NodeAddress),
    ANA = recommended_node_address(AltNodeAddress),
    B = <<C:8, NA/binary, ANA/binary, Extensions/binary>>,
    redirection_request2(Version, SeqNum, B).

redirection_request2(Version, SeqNum, B) ->
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(redirection_request), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>.

redirection_response(Version, SeqNum) ->
    ?PRINTDEBUG("redirection_response/2"),
    redirection_response(Version, SeqNum, request_accepted, << >>).
redirection_response(Version, SeqNum, Cause, Extensions)->    
    ?PRINTDEBUG("redirection_response/4"),
    C = cause(Cause),
    B = <<C:8, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(redirection_response), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>.

data_record_transfer_request(Version, SeqNum, data_record_packet, DataRecordPackets, Extensions) ->
    ?PRINTDEBUG("data_record_transfer_request/5 - data_record_packet"),
    B = <<126:8, 1:8, DataRecordPackets/binary, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(data_record_transfer_request), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>;
data_record_transfer_request(Version, SeqNum, send_potential_duplicate_data_record_packet, DataRecordPackets, Extensions) ->
    ?PRINTDEBUG("data_record_transfer_request/5 - potential_dup_data_record_packet"),
    B = <<126:8, 2:8, DataRecordPackets/binary, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(data_record_transfer_request), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>;
data_record_transfer_request(Version, SeqNum, cancel_packets, SeqNums, Extensions) ->
    ?PRINTDEBUG("data_record_transfer_request/5 - cancel_packets"),
    C = cancelled_seqnums(SeqNums),
    B = <<126:8, 3:8, C/binary, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(data_record_transfer_request), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>;
data_record_transfer_request(Version, SeqNum, release_packets, SeqNums, Extensions) ->
    ?PRINTDEBUG("data_record_transfer_request/5 - release_packets"),
    R = released_seqnums(SeqNums),
    B = <<126:8, 4:8, R/binary, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(data_record_transfer_request), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>.

data_record_transfer_response(Version, SeqNum, Cause, RequestsResponded, Extensions) ->
    ?PRINTDEBUG("data_record_transfer_response/5"),
    C = cause(Cause),
    R = requests_responded(RequestsResponded),
    B = <<C:8, R/binary, Extensions/binary>>,
    H = header(#gtpp_header{version=Version, pt=0, modern_header=1, msg_type=msg_type(data_record_transfer_response), 
			    msg_len=size(B), seqnum=SeqNum}),
    <<H/binary, B/binary>>.

  


requests_responded(List) ->
    Bin = list_to_binary([<<X:16>> || X <- List]),
    L = size(Bin),
    <<253:8, L:16, Bin/binary>>.

cancelled_seqnums(List) ->
    Bin = list_to_binary([<<X:16>> || X <- List]),
    L = size(Bin),
    <<250:8, L:16, Bin/binary>>.

released_seqnums(List) ->    
    Bin = list_to_binary([<<X:16>> || X <- List]),
    L = size(Bin),
    <<249:8, L:16, Bin/binary>>.
    

charging_gateway_address({IP1, IP2, IP3, IP4}) ->
    <<251:8, 4:8, IP1:8, IP2:8, IP3:8, IP4:8>>;
charging_gateway_address({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    <<251:8, 16:8, IP1:16, IP2:16, IP3:16, IP4:16, 
                   IP5:16, IP6:16, IP7:16, IP8:16>>. 

recommended_node_address({IP1, IP2, IP3, IP4}) ->
    <<254:8, 4:8, IP1:8, IP2:8, IP3:8, IP4:8>>;
recommended_node_address({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    <<254:8, 16:8, IP1:16, IP2:16, IP3:16, IP4:16, 
                   IP5:16, IP6:16, IP7:16, IP8:16>>. 

msg_type(echo_request) -> 1;
msg_type(echo_response) -> 2;
msg_type(version_not_suported) -> 3;
msg_type(node_alive_request) -> 4;
msg_type(node_alive_response) -> 5;
msg_type(redirection_request) -> 6;
msg_type(redirection_response) -> 7;
msg_type(data_record_transfer_request) -> 240;
msg_type(data_record_transfer_response) -> 241.

restart_counter(Count) ->
  <<14:8, Count:8>>.


%% Request causes, specific to GTP'
cause(node_about_to_go_down) -> 63;
cause(another_node_about_to_go_down) -> 62;
cause(receive_buffers_becoming_full) -> 61;
cause(transmit_buffers_becoming_full) -> 60;
cause(request_system_failure) -> 59; %% duplicated by GTP system_failure...but this is a "request".

%% Response cause, specific to GTP'
cause(cdr_decoding_error) -> 177;

%% Reject cause, specific to GTP'
cause(request_not_fulfilled) -> 255;
cause(sequence_numbers_released_cancelled_incorrect) -> 254;
cause(request_already_fulfilled) -> 253;
cause(request_related_to_duplicates_already_fulfilled) -> 252;

%% Request causes, generic

%% Response causes, generic
cause(request_accepted) -> 128;

%% Reject causes, generic
cause(no_resources_available) -> 199;
cause(service_not_supported) -> 200;
cause(system_failure) -> 204;
cause(mandatory_ie_incorrect) -> 201;
cause(mandatory_ie_missing) -> 202;
cause(optional_ie_incorrect) -> 203;
cause(invalid_message_format) -> 193;
cause(version_not_supported) -> 198;

cause({cause, request, _Value}) ->
    %% generic request cause
    cause(request_system_failure);
cause({cause, response, _Value}) ->
    %% generic response cause
    cause(request_accepted);
cause({cause, reject, _Value}) ->
    %% generic reject cause
    cause(request_not_fulfilled).

