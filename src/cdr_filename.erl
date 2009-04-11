%%%-------------------------------------------------------------------
%%% File    : cdr_filename.erl
%%% Author  : root <root@one.w8trk.com>
%%% Description : 
%%%
%%% Created : 24 Jul 2008 by root <root@one.w8trk.com>
%%%-------------------------------------------------------------------
-module(cdr_filename).

%% API
-export([build_filename/6,build_pd_filename/7]).
-export([test/0]).


-record(input, {gsn_proto_ip_port, cgf_proto_ip_port, now, seqnum, hostname}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
build_pd_filename(Template, PD_suffix, Hostname, GSN_address, CGF_address, Now, Seqnum) ->
    build_filename(Template, Hostname, GSN_address, CGF_address, Now, Seqnum) ++ PD_suffix.

build_filename(Template, GSN_address, CGF_address, Now, Seqnum, HN) ->
    Toks = string:tokens(Template,"%"),
    substitute(Toks, #input{gsn_proto_ip_port=GSN_address,
			    cgf_proto_ip_port=CGF_address,
			    now=Now,
			    seqnum=Seqnum,
			    hostname=HN}).

test() ->
    "CDR-19700101T000000-myhost-10_151_33_1-6666-000022.asn1" = build_filename(
								  "CDR-%utc_datetime%-%hostname%-%gsn_ip_port%-%seqnum%.asn1",
								  {udp, {10,151,33,1}, 6666},
								  {udp, {10,151,33,2},3333}, 
								  {0,0,0},
								  22,
								  "myhost"),
    "CDR-19700101T000000-10_151_33_2-tcp-000000.asn1" = build_filename(
							  "CDR-%utc_datetime%-%cgf_ip_port%-%seqnum%.asn1",
							  {tcp, {10,151,33,1}, 6666},
							  {tcp, {10,151,33,2},3333}, 
							  {0,0,0},
							  0,
							  "myhost"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
substitute(Tokens, Inputs) ->
    lists:flatten(lists:reverse(substitute(Tokens, Inputs, []))).

substitute(["utc_datetime" | Rest], Inputs, Output) ->
    {{YY,MMM,DD},{HH,MM,SS}} = calendar:now_to_universal_time(Inputs#input.now),
    DT = io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B",[YY, MMM, DD, HH, MM, SS]),
    substitute(Rest, Inputs, [DT|Output]);
substitute(["local_datetime" | Rest], Inputs, Output) ->
   {{YY,MMM,DD},{HH,MM,SS}} = calendar:now_to_local_time(Inputs#input.now),
    DT = io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B",[YY, MMM, DD, HH, MM, SS]),
    substitute(Rest, Inputs, [DT|Output]);
substitute(["gsn_ip_port" | Rest], Inputs, Output) ->
    Addr = pretty_format_address(Inputs#input.gsn_proto_ip_port),
    substitute(Rest, Inputs, [Addr|Output]);
substitute(["cgf_ip_port" | Rest], Inputs, Output) ->
    Addr = pretty_format_address(Inputs#input.cgf_proto_ip_port),
    substitute(Rest, Inputs, [Addr|Output]);
substitute(["seqnum" | Rest], Inputs, Output) ->
    SN = io_lib:format("~6.10.0B", [Inputs#input.seqnum]),
    substitute(Rest, Inputs, [SN|Output]);
substitute(["proto" | Rest], Inputs, Output) ->
    {Proto, _, _} = Inputs#input.gsn_proto_ip_port,
    substitute(Rest, Inputs, [atom_to_list(Proto)|Output]);
substitute(["hostname" | Rest], Inputs, Output) ->
    substitute(Rest, Inputs, [Inputs#input.hostname|Output]);
substitute([], _, Output) ->
    Output;
substitute([Anything | Rest], Inputs, Output) ->
    substitute(Rest, Inputs, [Anything|Output]).

pretty_format_address({udp, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, Port]);
pretty_format_address({udp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port]);
%% TCP uses ephemeral ports, and we don't have any other identifier...
pretty_format_address({tcp, {IP1,IP2,IP3,IP4},_Port}) ->
    io_lib:format("~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4]);
pretty_format_address({tcp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},_Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8]).
