{application, 'open-cgf',
 [{description, "Open Charging Gateway Function"},
  {vsn, "1"},
  {modules, ['open-cgf','open-cgf_app','open-cgf_sup', 'open-cgf_state', gtpp_udp_server,gtpp_tcp_server,
		gtpp_tcp_connection, cdr_file_srv, gtpp_encode, gtpp_decode]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {'open-cgf_app',[]}},
  {env, [{cdr_dir, "/tmp/log/"},          %% directory for cdr files to be written  BOTH DIRECTORIES MUST BE ON SAME VOLUME
	 {cdr_temp_dir, "/tmp/log_tmp/"}, %% directory for files to be written in before being closed
	 {cdr_post_close_command, none},  %% command to execute once CDR file is closed - expects to be in path or start dir. Alternatively, 'none'
	 {cdr_file_age_limit_seconds, 60}, %% file will be closed (and records confirmed) after this expires
	 {cdr_file_record_limit, 100},     %% file will be closed (and records confirmed) after this expires
	 {cdr_possible_duplicate_limit_seconds, 600},
	 {tcp_server, false},                   %% start a TCP (for GTP' v0 only) server?
	 {cdf_list, [{{65,23,156,214}, 3386}]}, %% list of CDFs to notify when CGF is initialised
	 {listen, {{65,23,156,215},3386}},      %% which interface/port to listen on
	 {syslog, {{127,0,0,1},514}},		%% Syslog server address, or 'none' e.g. {syslog, none}
	 {peer_cgf, {127,0,0,1}},               %% peer to redirect traffic to when this cgf is disabled, or 'none' (without quotes)
	 {gtpp_version, 2}			%% GTP' version to use to start (0-2)
	]}
 ]}.
