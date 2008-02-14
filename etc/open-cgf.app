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
	 {cdr_file_age_limit_seconds, 60},
	 {cdr_file_record_limit, 100},
	 {cdr_possible_duplicate_limit_seconds, 600},
	 {tcp_server, false},                    %% start a TCP (for GTP' v0 only) server?
	 {cdf_list, [{{65,23,156,214}, 3386}]},      %% list of CDFs to notify when CGF is initialised
	 {listen, {{65,23,156,215},3386}},           %% which interface/port to listen on
	 {gtpp_version, 2}			%% GTP' version to use (0-2)
	]}
 ]}.
