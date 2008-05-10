{application, 'open-cgf',
 [{description, "Open Charging Gateway Function"},
  {vsn, "1"},
  {modules, ['open-cgf','open-cgf_app','open-cgf_sup', 'open-cgf_state', gtpp_udp_server,gtpp_tcp_server,
		gtpp_tcp_connection, cdr_file_srv, gtpp_encode, gtpp_decode]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {'open-cgf_app',[]}} 
 ]}.
