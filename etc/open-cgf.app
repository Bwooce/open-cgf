{application, 'open-cgf',
 [{description, "Open Charging Gateway Function"},
  {vsn, "1"},
  {modules, ['open-cgf','open-cgf_app','open-cgf_sup',gtpp_udp_server,gtpp_tcp_server,
		gtpp_tcp_connection, cdr_file_srv]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {'open-cgf',[]}},
  {env, [{cdr_dir, "/usr/local/log/"},          %% directory for cdr files to be written
	 {cdr_temp_dir, "/usr/local/log_tmp/"}, %% directory for files to be written in before being closed
	 {tcp_server, false},                    %% start a TCP (for GTP' v0 only) server?
	 {cdf_list, [{{ipv4, {127,0,0,1}, 9000}}]}      %% list of CDFs to notify when CGF is initialised TODO
	]}
 ]}.
