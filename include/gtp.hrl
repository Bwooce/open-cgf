%%%-------------------------------------------------------------------
%%% File    : gtp.hrl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 28 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%-------------------------------------------------------------------

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

-record(gtpp_header, {version,
		      pt,
		      short_header_for_gtp0_ind,
		      msg_type,
		      msg_len,
		      seqnum}).


