%%%-------------------------------------------------------------------
%%% File    : gtp_tcp_server.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
%%%
%%% Created : 22 May 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module(gtpp_tcp_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start_link/0, listen_loop_init/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {ip, port, listener, configured_cdfs, connections}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    RSCounter = 'open-cgf_state':get_restart_counter(),
    {ok, {IP,Port}} = 'open-cgf_config':get_item({'open-cgf', listen}, ip_port), %% error and crash if no match
    ListenPid = spawn_link(?MODULE, listen_loop_init, [self(), {IP, Port}]),
    error_logger:info_msg("open-cgf listening on ~s:~B TCP - session counter ~p~n",[inet_parse:ntoa(IP), Port, RSCounter]),
    {ok, CDFs} = 'open-cgf_config':get_item({'open-cgf', cdf_list}, none, []), %% default to [] (none), validation is TODO
    Connections = lists:foldl(fun({DestIP, DestPort}, Acc) ->
				      case gtpp_tcp_connection:start({DestIP, DestPort}) of
					  {ok, {error, _Reason}} ->  Acc;
					  {ok, Pid} -> Acc ++ [Pid]
				      end
			      end, [], CDFs),			  
    {ok, #state{ip=IP, port=Port, listener=ListenPid, configured_cdfs=CDFs, connections=Connections}}.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% remove from lists of open connections, I guess.
    {noreply, State};

handle_info({_LoopPid, new_connection, Pid}, State) ->
    {noreply, State#state{connections=State#state.connections ++ [Pid]}};

handle_info(Info, State) ->
    error_logger:warning_msg("Got unhandled info ~p while in state ~p",[Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    error_logger:info_msg("Closing open-cgf tcp server for reason: \"~p\". Sending redirection requests to all active CDFs~n", [Reason]),
    case Reason of
	normal ->     timer:sleep(1000); %% give cdr_file_srv a second to catch up, if required. TODO improve this to be synchronised
	_ -> ok %% no time to bugger around
    end,
    %% don't bother storing them in a queue, we're shutting down so we're allowed to be messy.
    lists:foreach(fun(Pid) ->
			  Pid ! {controlled_shutdown}
		  end, State#state.connections),
    error_logger:info_msg("~nopen-cgf tcp exiting~n~n"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
listen_loop_init(OwnerPid, {IP, Port}) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, true}, {ip, IP}, {reuseaddr, true}]),
    listen_loop(OwnerPid, LSock).
listen_loop(OwnerPid, LSock) ->
    case gen_tcp:accept(LSock) of
	{ok, Socket} ->
	    {ok, Pid} = gtpp_tcp_connection:start(Socket),
	    ok = gen_tcp:controlling_process(Socket, Pid),
	    OwnerPid ! {self(), new_connection, Pid},
	    listen_loop(OwnerPid, LSock);
	{error, closed} ->
	    ok;
	{error, Other} ->
	    error_logger:info_msg("gtpp_tcp listen socket accept error ~p, continuing to listen",[Other]),
	    listen_loop(OwnerPid, LSock)
    end.

