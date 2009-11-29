%%%-------------------------------------------------------------------
%%% File    : open-cgf_sup.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 27 Jan 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module('open-cgf_sup').

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
start_link(_) ->
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
    {ok, StartTCP} = 'open-cgf_config':get_item({'open-cgf',tcp_server}, bool, false), %%% only start the TCP server if required
    {ok, MaxRestarts} = 'open-cgf_config':get_item({'open-cgf',max_restarts}, {integer, 0, 1000}, 10),
    CTLServer = {'CTLServer',{'open-cgf_ctl',start_link,[]},
		     permanent,2000,worker,['open-cgf_ctl']},
    CDRFileServer = {'CDRServer',{cdr_file_srv,start_link,[]},
		     permanent,2000,worker,[cdr_file_srv]},
    CGFState = {'CGFState',{'open-cgf_state',start_link,[]},
		permanent,2000,worker,['open-cgf_state']},
    UDPServer = {'UDPServer',{gtpp_udp_server,start_link,[]},
		 permanent,2000,worker,[gtpp_udp_server]},
    TCPServer = case StartTCP of
		    true ->
			[{'TCPServer',{gtpp_tcp_server,start_link,[]},
			 permanent,2000,worker,[gtpp_tcp_server]}];
		    _ -> []
		end,
    CDRSupervisor = {'CDRSupervisor',{'open-cgf_cdr_sup',start_link,[]},
		     permanent,2000,supervisor,['open-cgf_cdr_sup']},
    % max n restarts in the last 30s
    {ok,{{one_for_one,MaxRestarts,30}, [CTLServer, CDRSupervisor, CDRFileServer, CGFState, UDPServer] ++ TCPServer}}.

%%====================================================================
%% Internal functions
%%====================================================================
