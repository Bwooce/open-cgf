{application, 'open-cgf',
 [{description, "Open Charging Gateway Function"},
  {vsn, "1"},
  {modules, ['open-cgf','open-cgf_app','open-cgf_sup',gtpp_udp_server,gtpp_tcp_server,
		gtpp_tcp_connection, cdr_file_srv]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {'open-cgf',[]}},
  {env, [{cdr_dir, "/usr/local/log"}, %% directory for cdr files to be written
	 {tcp_server, true}           %% start a TCP (GTP' v0) server?
	]}
 ]}.
