%%%-------------------------------------------------------------------
%%% File    : open-cgf_cdr_srv.erl
%%% Author  : Bruce Fitzsimons <bruce@fitzsimons.org>
%%% Description : CDR server for the UDP server or one of the TCP 
%%%               servers spawned from the listen socket.
%%%
%%% Created : 26 July 2008 by Bruce Fitzsimons <bruce@fitzsimons.org>
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
-module('open-cgf_cdr_srv').

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("../include/open-cgf.hrl").

-record(state, {address, %% reference only, nice for errors
		cdr_file_handle, %% one file per destination, undefined if closed
		cdr_timer, %%% TID or undefined
		cdr_file_name, %% file name in the temporary directory
		cdr_final_file_name, %% filename in the final directory
		cdr_file_ts, %% timestamp of file open (so we know when to close it, if fairly idle)
		cdr_file_count, %% record count of cdr file (so we know when to close it, if under load)
		possible_duplicate_list, %% [{seq_num, ts, [CDRs]}] of acknowledged but unwritten CDRs. Written eventually even if not confirmed. 
		cdr_writer_pid, %% PID of the cdr writing task, so we know if it died/we can't write more

		% saved configuration
	        cdr_dir, 
		cdr_temp_dir,
	        file_record_limit,
		file_age_limit_seconds,
		possible_duplicate_limit_seconds,
		post_close_command,
		template,
		pd_suffix,
		hostname
	       }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []). %% no name, the main server knows us

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
init([GSN_address]) ->
    {ok, CDR_temp_dir} = 'open-cgf_config':get_item({'open-cgf', cdr_temp_dir}, none), %% crash if not defined. validation TODO
    {ok, CDR_dir} = 'open-cgf_config':get_item({'open-cgf', cdr_dir}, none), %% crash if not defined. validation TODO
    {ok, CDR_file_record_limit} = 'open-cgf_config':get_item({'open-cgf', cdr_file_record_limit}, {integer, 1, 10000}, 100),
    {ok, CDR_file_age_limit} = 'open-cgf_config':get_item({'open-cgf', cdr_file_age_limit_seconds}, {integer, 1, 3600}, 60), 
    {ok, CDR_duplicate_limit} = 'open-cgf_config':get_item({'open-cgf', cdr_possible_duplicate_limit_seconds}, {integer, 1, 3600}, 60), 
    {ok, CDR_close_command} = 'open-cgf_config':get_item({'open-cgf', cdr_post_close_command}, none, none), %% validation TODO
    {ok, CDR_template} = 'open-cgf_config':get_item({'open-cgf', cdr_filename_template}, {string, 1, 100},
						    "CDR-%hostname%-%gsn_ip_port%-%utc_datetime%.asn1"), 
    {ok, CDR_PD_suffix} = 'open-cgf_config':get_item({'open-cgf', cdr_dup_filename_suffix}, {string, 1, 100}, 
						     ".potential_duplicate"), 
    {ok,HN} = inet:gethostname(),
    %% set the callback timer to close files after time limit is up
    I2 = trunc((CDR_duplicate_limit*1000)/2),
    {ok, _} = timer:send_interval(I2, {close_duplicates_tick}),
    process_flag(trap_exit, true),
    {ok, #state{address=GSN_address,
                cdr_file_handle=undefined,
		cdr_file_name=[],
		cdr_final_file_name=[],
		cdr_file_ts=0,
		cdr_file_count=0,
		possible_duplicate_list=[],
		cdr_writer_pid=none,
		
		cdr_dir=CDR_dir, cdr_temp_dir=CDR_temp_dir,
	        file_record_limit=CDR_file_record_limit, file_age_limit_seconds=CDR_file_age_limit,
	        possible_duplicate_limit_seconds = CDR_duplicate_limit,
	        post_close_command=CDR_close_command,
		template=CDR_template,
		pd_suffix=CDR_PD_suffix,
		hostname=HN}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({log, Seq_num, Data}, _From, State) ->
    {ok, NewState} = buffer_cdr(Seq_num, Data, State),
    {reply, ok, NewState};

handle_call({log_possible_dup, Seq_num, Data}, _From, State) ->
    {ok, NewState} = buffer_duplicate_cdr(Seq_num, Data, State),
    {noreply, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({remove_possible_dup, _Seq_num, Seq_nums}, State) ->
    NewS = lists:foldl(fun(SSeq_num, OldS) ->
			       State#state{possible_duplicate_list=
					   lists:keydelete(SSeq_num, 1, State#state.possible_duplicate_list)} %% remove the CDR, if possible
		      end, S, Seq_nums),		       
    {noreply, NewS};

handle_cast({commit_possible_dup, Seq_num, Seq_nums},State) ->
    NewS = lists:foldl(fun(SSeq_num, State) ->
			       case lists:keysearch(SSeq_num, 1, State#state.possible_duplicate_list) of
				   false ->
				       State; %% wasn't in the list, can't do much
				   {value, {Seq_num, _TS, Data}} ->
					    NewState = buffer_cdr(Seq_num, Data, State),
					    NewState#state{possible_duplicate_list=
							   lists:keydelete(SSeq_num, 1, NewState#state.possible_duplicate_list)} %% remove the CDR
				    end
			    end, S, Seq_nums),
    {noreply, NewS};

handle_cast({erase_pending_duplicates, SeqNums}, State) -> %% used once the writer process completes
    %% no need to ack as these are already ack'd
    NewS = State#state{cdr_writer_pid=none,
		       possible_duplicate_list=delete_seqnums(State#state.possible_duplicate_list, SeqNums)},
    {noreply, NewS};

handle_cast(reset, State) ->
    %% write all the CDRs, close the files
    Pid = spawn_link(?MODULE, log_duplicate_cdr, [State]), 
    close_cdr_file(State),
    NewS = State#state{cdr_file_handle=undefined,
		       cdr_timer=undefined,
		       cdr_file_name=[],
		       cdr_final_file_name=[],
		       cdr_file_ts=0,
		       cdr_file_count=0,
		       possible_duplicate_list=[],
		       cdr_writer_pid=Pid},
    {noreply, NewS};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({close_cdr, SourceKey}, State) ->
    NewState = close_source(State),
    {noreply, NewState};

handle_info({close_duplicates_tick}, State) ->
    spawn_link(?MODULE, flush_duplicates_timer, [State]),
    {noreply, State};

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

%% fixup so we don't stall in a state with a dead cdr_writer
handle_info({'EXIT', Pid, Cause}, State) ->
    NewS = case State#state.cdr_writer_pid of
	       Pid ->
		   error_logger:error_msg("CDR Writer for ~p exited with ~p",[State#state.address, Cause]),
		   State#state{cdr_writer_pid=none};				      
	       _ -> State
	   end,
    {noreply, News};

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
    error_logger:info_msg("cdr_file_srv ~p is shutting down for reason: ~p",[State#state.address, Reason]),
    log_duplicate_cdr(State),
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
write_cdr(_SourceKey, _Seq_num, [], State) ->
    ?PRINTDEBUG("Writing CDR with no CDR"),
    {ok, State};

write_cdr(SourceKey, Seq_num, Data, State) ->
    ?PRINTDEBUG2("Writing CDR for ~p, seqnum ~B",[SourceKey, Seq_num]),
    NewState = write_cdr(Seq_num, Data, State),
    {ok, NewState}.

write_cdr(_SeqNum, Data, State) ->
    NewS = case State#state.cdr_file_handle of
	       undefined ->
		   Filename = cdr_filename:build_filename(State#state.cdr_template, 
							  State#state.address, 
							  State#state.cgf_address, 
							  now(), 
							  Seqnum, vvvvvvvvvvvvvvvvvvv 
							  State#state.hostname) 
		   Temp_filename = filename:join([State#state.cdr_temp_dir, Filename]),
		   Final_filename = filename:join([State#state.cdr_dir, Filename]),
		   ok = filelib:ensure_dir(Temp_filename),
		   ok = filelib:ensure_dir(State#state.cdr_dir),
		   ?PRINTDEBUG2("Opening CDR file ~s",[Temp_filename]),
		   {ok, F} = file:open(Temp_filename, [raw, write, delayed_write]),
		   %% set timer for closure
		   {ok, TID} = timer:send_after(State#state.file_age_limit_seconds*1000, {close_cdr, State#state.address}),
		   N = State#state{cdr_file_handle=F,
				   cdr_timer=TID,
				   cdr_file_name=Temp_filename,
				   cdr_final_file_name=Final_filename,
				   cdr_file_ts=greg_now(),
				   cdr_file_count=1},
		   N;
	       _H -> 
		   State#state{cdr_file_count=State#state.cdr_file_count+1}
	   end,
    ok = file:write(NewS#state.cdr_file_handle, Data),
    check_file_count(State). %% check_file_count always updates the state




buffer_duplicate_cdr(Seq_num, Data, State) ->
    ?PRINTDEBUG2("Buffering possible duplicate CDR for ~p, seqnum ~B",[State#state.address, Seq_num]),
    TS = greg_now(),
    NewState = check_duplicate_buffer(TS, State),
    NewState2 = NewState#state{possible_duplicate_list = NewState#state.possible_duplicate_list ++ [{Seq_num, TS, Data}]},
    {ok, NewState2}.

check_duplicate_buffer(TS, State) when State#state.cdr_writer_pid == none, not State#state.possible_duplicate_list == [] ->
    ?PRINTDEBUG("check_duplicate_buffer state"),
    TScomp = oldest_TS(State#state.possible_duplicate_list)+State#state.file_age_limit_seconds,
    if (TScomp < TS) or length(State#state.possible_duplicate_list) > State#state.possible_duplicate_limit_seconds ->
	    Pid = spawn_link(?MODULE, log_duplicate_cdr, [State]),  %% will write
	    NewS = State#state{cdr_writer_pid = Pid},
	    NewS;
       true ->
	    State
    end;
check_duplicate_buffer(_,State) when not State#state.cdr_writer_pid == none ->
    ?PRINTDEBUG("check_duplicate_buffer state - busy writing buffer already"),
    case is_process_alive(State#state.cdr_writer_pid) of  %% belts and braces check. Could cause infinite cycling...
	false ->
	    error_logger:error_msg("CDR writer died unexpectedly. resetting"),
	    NewS = State#state{cdr_writer_pid = none},
	    NewS;
	_ -> State
    end;
check_duplicate_buffer(_,State) ->
   ?PRINTDEBUG("check_duplicate_buffer state - no records to write"),
    State.


check_file_count(State) ->
    if(State#state.cdr_file_count >= State#state.file_record_limit) ->
	    ?PRINTDEBUG2("Closing cdr file for ~p",[pretty_format_address(State#state.address_]),
	    ok = close_cdr_file(State),
	    execute_post_close_command(State#state.post_close_command, State#state.cdr_final_file_name),
	    NewS = State#state{cdr_file_handle=undefined,
			       cdr_timer=undefined,
			       cdr_file_name=[],
			       cdr_final_file_name=[],
			       cdr_file_ts=0,
			       cdr_file_count=0},
	    NewS;
      true ->
	    State
    end.

close_cdr_file(State) when State#state.cdr_file_handle =:= undefined ->
    ok;
close_cdr_file(State) ->
    timer:cancel(State#state.cdr_timer),
    ok = file:close(State#state.cdr_file_handle),
    ok = file:rename(State#state.cdr_file_name, S#state.cdr_final_file_name).

execute_post_close_command(none, Filename) ->
    'open-cgf_logger':debug("no cdr_post_close_command for file ~s; skipping",[Filename]),
    ok;
execute_post_close_command(Command, Filename) ->
    Cmd = Command ++ " " ++ "\"" ++ Filename ++ "\"",
    try os:cmd(Cmd) of
	Result ->
	    'open-cgf_logger':debug("cdr_post_close_command ~s completed with result ~s",[Cmd, Result]),
	    ok
    catch
	exit: X ->
	    error_logger:error_msg("cdr_post_close command failed: command ~s, error ~p",[Cmd,X])
    end.


oldest_TS([]) ->
    greg_now();
oldest_TS([{_, TS, _}|_]) ->
    TS.

greg_now() ->
    calendar:datetime_to_gregorian_seconds({date(), time()}).

pretty_format_address({udp, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, Port]);
pretty_format_address({udp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port]);
%% TCP uses ephemeral ports, and we don't have any other identifier...
pretty_format_address({tcp, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4]);
pretty_format_address({tcp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8]).

    

