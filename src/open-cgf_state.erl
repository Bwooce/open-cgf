%%%-------------------------------------------------------------------
%%% File    : open-cgf_state.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : 
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
-module('open-cgf_state').

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-include("open-cgf.hrl").
-include("gtp.hrl").

%% API
-export([start_link/0, get_next_seqnum/1, get_restart_counter/0, inc_restart_counter/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {sequence_numbers, restart_counter }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


get_next_seqnum(Key) ->
    gen_server:call(?SERVER, {seqnum, Key}).

get_restart_counter() ->
    gen_server:call(?SERVER, {restart_counter}).

inc_restart_counter() ->
        gen_server:call(?SERVER, {inc_restart_counter}).

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
    %% read state from dets
    {ok, _} = dets:open_file(cgf_state_dets, [{ram_file, true}]), %% we're storing nothing that isn't sync'd (for the moment)
    {_, RC} = case dets:lookup(cgf_state_dets, restart_counter) of
		  [C] -> C;
		  [] -> 
		      ok = dets:insert(cgf_state_dets, {restart_counter, 0}),
		      {nothing, 0}
	      end,
    {ok, #state{restart_counter=RC, sequence_numbers=orddict:new()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({seqnum, Key}, _, State) ->
    NewDict = orddict:update_counter(Key, 1, State#state.sequence_numbers),
    SeqNum = orddict:fetch(Key, NewDict),
    ?PRINTDEBUG2("Next seqnum for key ~p is ~p",[Key, SeqNum]),
    {reply, SeqNum rem 65536, State#state{sequence_numbers=NewDict}};

handle_call({restart_counter}, _, State) ->
    ?PRINTDEBUG2("Restart counter is ~p",[State#state.restart_counter]),
    {reply, State#state.restart_counter, State};

handle_call({inc_restart_counter}, _, State) ->
    RC = dets:update_counter(cgf_state_dets, restart_counter, 1) rem 256,
    dets:sync(cgf_state_dets),
    {reply, RC, State#state{restart_counter=RC}};

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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    dets:sync(cgf_state_dets),
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
