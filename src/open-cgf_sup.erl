%%%-------------------------------------------------------------------
%%% File    : open-cgf_sup.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
%%%-------------------------------------------------------------------
-module('open-cgf_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {ok, StartTCP} = application:get_env('open-cgf', tcp_server), %%% only start the TCP server if required
    CDRFileServer = {'CDRServer',{cdr_file_srv,start_link,[]},
		     permanent,2000,worker,[cdr_file_srv]},
    UDPServer = {'UDPServer',{gtpp_udp_server,start_link,[]},
		 permanent,2000,worker,[gtpp_udp_server]},
    TCPServer = case StartTCP of
		    true ->
			[{'TCPServer',{gtpp_tcp_server,start_link,[]},
			 permanent,2000,worker,[gtpp_tcp_server]}];
		    _ -> []
		end,
    {ok,{{one_for_all,0,1}, [CDRFileServer, UDPServer] ++ TCPServer}}.

%%====================================================================
%% Internal functions
%%====================================================================
