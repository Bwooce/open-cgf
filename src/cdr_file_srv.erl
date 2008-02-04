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

%% API
-export([start_link/0, log/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {open_files,
	        cdr_dir, 
		cdr_temp_dir,
	        file_record_limit,
		file_age_limit_seconds}).

-record(file, {source, %% {IP,Port} of sending CDF
	       possible_duplicate_list, %% [{seq_num, [CDRs]}] of acknowledged but unwritten CDRs. Written eventually even if not confirmed. 
	       written_seq_nums, %% [] seq_nums in current file, non-duplicates
	       old_seq_nums_ringbuffer, %% maintain a list of n thousand seq_nums, in case CDF loses an ACK somehow. Sigh.
	       file_handle, %% currently open file handle, or undefined if none
	       file_name, 
	       file_records, %% number of CDR records in file
	       file_open_timestamp %% time file opened in greg_seconds
	      }). 

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


log(Source,Data) -> 
    gen_server:cast(?SERVER, {log, Source, Data}).

log_possible_dup(Source, Seq_num, Data) ->
    gen_server:cast(?SERVER, {log_possible_dup, Source, Seq_num, Data}).

commit_possible_dup(Source, Seq_num) ->
    gen_server:cast(?SERVER, {commit_possible_dup, Source, Seq_num}).

remove_possible_dup(Source, Seq_num) ->
    gen_server:cast(?SERVER, {remove_possible_dup, Source, Seq_num}).



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
    {ok, CDR_temp_dir} = application:get_env('open-cgf', cdr_temp_dir),
    {ok, CDR_dir} = application:get_env('open-cgf', cdr_dir),
    {ok, CDR_file_record_limit} = application:get_env('open-cgf', cdr_file_record_limit),
    {ok, CDR_file_age_limit} = application:get_env('open-cgf', cdr_file_age_limit_seconds),
    %% set the callback timer to close files after time limit is up
    {ok, #state{open_files=[], cdr_dir=CDR_dir, cdr_temp_dir=CDR_temp_dir,
	        file_record_limit=CDR_file_record_limit, file_age_limit_seconds=CDR_file_age_limit}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({log, Source, Data}, _From, State) ->
    {ok, NewState} = log_cdr(Source, Data, State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({log, Source, Data}, State) ->
    {ok, NewState} = log_cdr(Source, Data,State),
    {noreply, NewState};

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
log_cdr(Source, Data, State) ->
    %% check if file open, if not then open new
    %% log data to file
    {ok, State}.
