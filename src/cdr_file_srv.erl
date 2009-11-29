%%%-------------------------------------------------------------------
%%% File    : cdr_file_srv.erl
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
-module(cdr_file_srv).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("open-cgf.hrl").

%% API
-export([start_link/0, check_duplicate/3, log/4, log_possible_dup/4, commit_possible_dup/4, remove_possible_dup/4, reset/2, flush_pending_duplicates/3]).
-export([print_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {cdr_loggers = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

check_duplicate(SourceKey, OwnerAddress, Seq_num) ->
    gen_server:call(?SERVER, {check_duplicate, SourceKey, OwnerAddress, Seq_num}).

log(Source, OwnerAddress, Seq_num, Data) -> 
    gen_server:call(?SERVER, {log, Source, OwnerAddress, Seq_num, Data}).

log_possible_dup(SourceKey, OwnerAddress, Seq_num, Data) ->
    gen_server:cast(?SERVER, {log_possible_dup, SourceKey, OwnerAddress, Seq_num, Data}).

commit_possible_dup(SourceKey, OwnerAddress, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {commit_possible_dup, SourceKey, OwnerAddress, Seq_num, Seq_nums}).

remove_possible_dup(SourceKey, OwnerAddress, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {remove_possible_dup, SourceKey, OwnerAddress, Seq_num, Seq_nums}).

flush_pending_duplicates(SourceKey, OwnerAddress, Seq_nums) ->
    gen_server:cast(?SERVER, {flush_pending_duplicates, SourceKey, OwnerAddress, Seq_nums}).

reset(SourceKey, OwnerAddress) ->
    gen_server:cast(?SERVER, {reset, SourceKey, OwnerAddress}).

print_state() ->    
    gen_server:cast(?SERVER, {print_state}).

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
    {ok, #state{cdr_loggers = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_duplicate, Source, OwnerAddress, Seq_num}, _From, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    Result = gen_server:call(PID, {check_duplicate, Seq_num}),
    {reply, Result, State#state{cdr_loggers=NewCDRLoggers}};

handle_call({log, Source, OwnerAddress, Seq_num, Data}, _From, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:call(PID, {log, Seq_num, Data}),
    {reply, ok, State#state{cdr_loggers=NewCDRLoggers}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({log, Source, OwnerAddress, Seq_num, Data}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {log, Seq_num, Data}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({log_possible_dup, Source, OwnerAddress, Seq_num, Data}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {log_possible_dup, Seq_num, Data}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({remove_possible_dup, Source, OwnerAddress, _Seq_num, Seq_nums}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {remove_possible_dup, Seq_nums}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({commit_possible_dup, Source, OwnerAddress, _Seq_num, Seq_nums}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {commit_possible_dup, Seq_nums}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({flush_pending_duplicates, Source, OwnerAddress, Seq_nums}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {flush_pending_duplicates, Seq_nums}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({reset, Source, OwnerAddress}, State) ->
    {PID, NewCDRLoggers} = find_logger(Source, OwnerAddress, State#state.cdr_loggers),
    ok = gen_server:cast(PID, {reset}),
    {noreply, State#state{cdr_loggers=NewCDRLoggers}};

handle_cast({print_state}, State) ->
    error_logger:info_msg("Requested state printout:~n~p~n",[State]),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:warning_msg("Got unhandled cast in cdr_file_srv. Msg was ~p",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({newCDRPID, Source, NewPID}, State) ->
    {noreply, State#state{cdr_loggers = update_logger(NewPID, Source, State#state.cdr_loggers)}};

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
terminate(Reason, _State) ->
    error_logger:info_msg("cdr_file_srv is shutting down for reason: ~p",[Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions

find_logger(Source, OwnerAddress, CDRLoggerList) ->    
    case proplists:get_value(Source, CDRLoggerList) of
	undefined ->
	    {ok, NewPID} = 'open-cgf_cdr_sup':add(self(), self(), Source, OwnerAddress),
	    {NewPID, [CDRLoggerList | {Source, NewPID}]};
	PID ->            
	    {PID, CDRLoggerList}
    end.


update_logger(PID, Source, CDRLoggerList) ->
    lists:keyreplace(Source, 1, CDRLoggerList, {Source, PID}).
				      
