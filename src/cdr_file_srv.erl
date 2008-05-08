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
-define(MAXRINGBUF, 1000). %% can't be more than size(64K - max_outstanding) otherwise wraparound will cause errors

-include("open-cgf.hrl").

%% API
-export([start_link/0, log/3, log_possible_dup/3, commit_possible_dup/3, remove_possible_dup/3, reset/1]).
-export([print_state/0]).

%% internal API
-export([log_cdr/2,log_duplicate_cdr/2,flush_pending/2,flush_pending_duplicates/2]).
-export([flush_pending_timer/1, flush_duplicates_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {known_sources,
	        cdr_dir, 
		cdr_temp_dir,
	        file_record_limit,
		file_age_limit_seconds,
		possible_duplicate_limit_seconds,
		post_close_command
	       }).

-record(source, {address, %% {IP,Port} of sending CDF
		 possible_duplicate_list, %% [{seq_num, ts, [CDRs]}] of acknowledged but unwritten CDRs. Written eventually even if not confirmed. 
		 pending_write_list, %% [{seq_num, ts, [CDRs]}]  - non-duplicate CDRs that we will write shortly
		 old_seq_nums_ringbuffer, %% maintain a list of n thousand seq_nums, in case CDF loses an ACK somehow. 
		 pending_records, %% number of CDR records pending (non-dup)
		 cdr_writer_pid %% PID of the cdr writing task, so we know if it died/we can't write more
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


log(Source, Seq_num, Data) -> 
    gen_server:cast(?SERVER, {log, Source, Seq_num, Data}).

log_possible_dup(SourceKey, Seq_num, Data) ->
    gen_server:cast(?SERVER, {log_possible_dup, SourceKey, Seq_num, Data}).

commit_possible_dup(SourceKey, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {commit_possible_dup, SourceKey, Seq_num, Seq_nums}).

remove_possible_dup(SourceKey, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {remove_possible_dup, SourceKey, Seq_num, Seq_nums}).

flush_pending(SourceKey, Seq_nums) ->
    gen_server:cast(?SERVER, {flush_pending, SourceKey, Seq_nums}).

flush_pending_duplicates(SourceKey, Seq_nums) ->
    gen_server:cast(?SERVER, {flush_pending_duplicates, SourceKey, Seq_nums}).

reset(SourceKey) ->
    gen_server:cast(?SERVER, {reset, SourceKey}).

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
    {ok, CDR_temp_dir} = application:get_env('open-cgf', cdr_temp_dir),
    {ok, CDR_dir} = application:get_env('open-cgf', cdr_dir),
    {ok, CDR_file_record_limit} = application:get_env('open-cgf', cdr_file_record_limit),
    {ok, CDR_file_age_limit} = application:get_env('open-cgf', cdr_file_age_limit_seconds),
    {ok, CDR_duplicate_limit} = application:get_env('open-cgf', cdr_possible_duplicate_limit_seconds), 
    CDR_close_command = case application:get_env('open-cgf', cdr_post_close_command) of
			    {ok, none} -> none;
			    undefined -> none;
			    {ok, CMD} -> CMD
			end,
    %% set the callback timer to close files after time limit is up
    I1 = trunc((CDR_file_age_limit*1000)/2),
    {ok, _} = timer:send_interval(I1, {flush_pending_tick}),
    I2 = trunc((CDR_duplicate_limit*1000)/2),
    {ok, _} = timer:send_interval(I2, {flush_duplicates_tick}),
    process_flag(trap_exit, true),
    {ok, #state{known_sources=[], cdr_dir=CDR_dir, cdr_temp_dir=CDR_temp_dir,
	        file_record_limit=CDR_file_record_limit, file_age_limit_seconds=CDR_file_age_limit,
	        possible_duplicate_limit_seconds = CDR_duplicate_limit,
	        post_close_command=CDR_close_command}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({log, Source, Seq_num, Data}, _From, State) ->
    {ok, NewState} = buffer_cdr(Source, Seq_num, Data, State),
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
handle_cast({log, Source, Seq_num, Data}, State) ->
    {ok, NewState} = buffer_cdr(Source, Seq_num, Data, State),
    {noreply, NewState};

handle_cast({log_possible_dup, SourceKey, Seq_num, Data}, State) ->
    {ok, NewState} = buffer_duplicate_cdr(SourceKey, Seq_num, Data, State),
    {noreply, NewState};

handle_cast({remove_possible_dup, SourceKey, Seq_num, Seq_nums}, State) ->
    S = get_source(SourceKey, State),
    NewS = lists:foldl(fun(SSeq_num, OldS) ->
			       OldS#source{possible_duplicate_list=
					   lists:keydelete(SSeq_num, 1, OldS#source.possible_duplicate_list)} %% remove the CDR, if possible
		      end, S, Seq_nums),		       
    RB=update_ringbuffer(NewS#source.old_seq_nums_ringbuffer, [Seq_num], ?MAXRINGBUF),
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, 
						 NewS#source{old_seq_nums_ringbuffer=RB})}};

handle_cast({commit_possible_dup, SourceKey, Seq_num, Seq_nums},State) ->
    S = get_source(SourceKey, State),
    NewS = lists:foldl(fun(SSeq_num, OldS) ->
			   case lists:keysearch(SSeq_num, 1, OldS#source.possible_duplicate_list) of
			       false ->
				   OldS; %% wasn't in the list, can't do much
			       {value, CDR} ->
				   OldS#source{possible_duplicate_list=
					       lists:keydelete(SSeq_num, 1, OldS#source.possible_duplicate_list), %% remove the CDR
					    pending_records=OldS#source.pending_records+1,
					    pending_write_list=OldS#source.pending_write_list++[CDR]} %% add it to the new list
			   end
		      end, S, Seq_nums),
        RB=update_ringbuffer(NewS#source.old_seq_nums_ringbuffer, [Seq_num], ?MAXRINGBUF),
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, 
						 NewS#source{old_seq_nums_ringbuffer=RB})}};

handle_cast({flush_pending, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending, SourceKey, SeqNums}, State) ->
    S = get_source(SourceKey, State),
    %%send ack via gtpp_udp_server
    notify_cdf(SourceKey, SeqNums, request_accepted),
    %% remove seq
    RB=update_ringbuffer(S#source.old_seq_nums_ringbuffer, SeqNums, ?MAXRINGBUF),
    NewS = S#source{old_seq_nums_ringbuffer=RB, 
		    cdr_writer_pid=none,
		    pending_records=S#source.pending_records-length(SeqNums),
		    pending_write_list=delete_seqnums(S#source.pending_write_list, SeqNums)},
    ?PRINTDEBUG2("Finished flushing, NewS is ~p",[NewS]),
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};

handle_cast({flush_pending_from_timer, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending_from_timer, SourceKey, SeqNums}, State) ->
    S = get_source(SourceKey, State),
    case S#source.cdr_writer_pid of
	none ->  
	    %%send ack via gtpp_udp_server
	    notify_cdf(SourceKey, SeqNums, request_accepted),
	    %% remove seq
	    RB=update_ringbuffer(S#source.old_seq_nums_ringbuffer, SeqNums, ?MAXRINGBUF),
	    NewS = S#source{old_seq_nums_ringbuffer=RB, 
			    pending_records=S#source.pending_records-length(SeqNums),
			    pending_write_list=delete_seqnums(S#source.pending_write_list, SeqNums)},
	    ?PRINTDEBUG2("Finished timer flushing, NewS is ~p",[NewS]),
	    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};
	_ ->
	    {noreply, State}
    end;

handle_cast({flush_pending_duplicates, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending_duplicates, SourceKey, SeqNums}, State) ->
    %% no need to ack as these are already ack'd
    S = get_source(SourceKey, State),
    RB=update_ringbuffer(S#source.old_seq_nums_ringbuffer, SeqNums, ?MAXRINGBUF),
    NewS = S#source{old_seq_nums_ringbuffer=RB, 
		    cdr_writer_pid=none,
		    possible_duplicate_list=delete_seqnums(S#source.pending_write_list, SeqNums)},
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};

handle_cast({flush_pending_duplicates_from_timer, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending_duplicates_from_timer, SourceKey, SeqNums}, State) ->
    %% no need to ack as these are already ack'd
    S = get_source(SourceKey, State),
    case S#source.cdr_writer_pid of
	none ->
	    RB=update_ringbuffer(S#source.old_seq_nums_ringbuffer, SeqNums, ?MAXRINGBUF),
	    NewS = S#source{old_seq_nums_ringbuffer=RB, 
			    possible_duplicate_list=delete_seqnums(S#source.pending_write_list, SeqNums)},
	    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};
	_ ->
	    {noreply, State} %% we can't stomp on an existing writer sessions
    end;

handle_cast({reset, SourceKey}, State) ->
    %% write all the CDRs
    S = get_source(SourceKey, State),
    Pid = spawn_link(?MODULE, log_cdr, [S, State]),  
    Pid = spawn_link(?MODULE, log_duplicate_cdr, [S, State]), 
    NewS = S#source{old_seq_nums_ringbuffer=[],
		    pending_write_list=[],
		    possible_duplicate_list=[],
		    pending_records=0,
		    cdr_writer_pid=none},
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS)}};    

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
handle_info({flush_pending_tick}, State) ->
    spawn_link(?MODULE, flush_pending_timer, [State]),
    {noreply, State};

handle_info({flush_duplicates_tick}, State) ->
    spawn_link(?MODULE, flush_duplicates_timer, [State]),
    {noreply, State};

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

%% fixup so we don't stall in a state with a dead cdr_writer
handle_info({'EXIT', Pid, Cause}, State) ->
    NewKS = lists:map(fun(S) ->
			      case S#source.cdr_writer_pid of
				  Pid ->
				      error_logger:error_msg("CDR Writer for ~p exited with ~p",[S#source.address, Cause]),
				      S#source{cdr_writer_pid=none};				      
				  _ -> S
			      end
		      end, State#state.known_sources),
    {noreply, State#state{known_sources=NewKS}};

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
terminate(_Reason, State) ->
    ?PRINTDEBUG("cdr_file_srv terminating, writing pending"),
    lists:foreach(fun(S) ->
			  log_cdr(S, State),
			  log_duplicate_cdr(S, State)
		  end, State#state.known_sources),
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
buffer_cdr(SourceKey, Seq_num, {{_,[]},_}, State) ->
    ?PRINTDEBUG("Buffering CDR with no CDR - responding immediately"),
    S = get_source(SourceKey, State),
    NewS = case check_duplicate(S, Seq_num) of 
	       true -> 
		   notify_cdf(SourceKey, [Seq_num], request_already_fulfilled),
		   S;
	       false ->
		   notify_cdf(SourceKey, [Seq_num], request_accepted),
		   S#source{old_seq_nums_ringbuffer=
			    update_ringbuffer(S#source.old_seq_nums_ringbuffer, [Seq_num], ?MAXRINGBUF)}
	   end,
    KS = keystore(SourceKey, 2, State#state.known_sources, NewS),
    {ok, State#state{known_sources=KS}};

buffer_cdr(SourceKey, Seq_num, Data, State) ->
    ?PRINTDEBUG2("Buffering CDR for ~p, seqnum ~p",[SourceKey, Seq_num]),
    NewState = check_buffer(SourceKey, State),
    TS = greg_now(),
    S = get_source(SourceKey, NewState),
    NewS = case check_duplicate(S, Seq_num) of 
	       true ->
		   error_logger:warning_msg("Duplicate sequence number ~p from ~p",[Seq_num, SourceKey]),
		   notify_cdf(SourceKey, [Seq_num], request_already_fulfilled),
		   S; %% update already in queue
	       false ->
		   S#source{pending_write_list =  S#source.pending_write_list ++ [{Seq_num, TS, Data}], 
			    pending_records = S#source.pending_records+1}
	   end,
    KS = keystore(SourceKey, 2, NewState#state.known_sources, NewS),
    {ok, NewState#state{known_sources=KS}}.

buffer_duplicate_cdr(SourceKey, Seq_num, Data, State) ->
    ?PRINTDEBUG2("Buffering possible duplicate CDR for ~p, seqnum ~p",[SourceKey, Seq_num]),
    NewState = check_buffer(SourceKey, State),
    TS = greg_now(),
    S = get_source(SourceKey, NewState),
    NewS = case check_duplicate(S, Seq_num) of 
	       true ->
		   error_logger:warning_msg("Duplicate sequence number ~p from ~p",[Seq_num, SourceKey]),
		   notify_cdf(SourceKey, [Seq_num], request_related_to_duplicates_already_fulfilled),
		   S;
	       false ->
		   S#source{possible_duplicate_list = S#source.possible_duplicate_list ++ [{Seq_num, TS, Data}]}
	   end,
    KS = keystore(SourceKey, 2, NewState#state.known_sources, NewS),
    {ok, NewState#state{known_sources=KS}}.

%% check to see if the buffers need to be flushed to disk.
check_buffer(SourceKey, State) ->
    TS = greg_now(),
    S = get_source(SourceKey, State),
    State2 = check_duplicate_buffer(TS, S, State),
    check_pending_buffer(TS, S, State2).


check_pending_buffer(TS, S, State) when S#source.cdr_writer_pid == none, S#source.pending_records > 0  ->    
    ?PRINTDEBUG2("check_pending_buffer, Writer PID = ~p", [ S#source.cdr_writer_pid]),
    TScomp = oldest_TS(S#source.pending_write_list)+State#state.file_age_limit_seconds,
    if (TScomp < TS) or (S#source.pending_records >= State#state.file_record_limit) ->
	    ?PRINTDEBUG("Spawning pending_buffer cdr writer"),
	    Pid = spawn_link(?MODULE, log_cdr, [S, State]),  %% will write
	    NewS = S#source{cdr_writer_pid = Pid},
	    NewKS = keystore(S#source.address, 2, State#state.known_sources, NewS),
	    State#state{known_sources=NewKS};
       true ->
	    ?PRINTDEBUG("No pending buffers to write"),
	    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, S)}
    end;
check_pending_buffer(_,S,State) when not S#source.cdr_writer_pid == none ->
    ?PRINTDEBUG("check_pending_buffer - busy writing buffer already or no records to write"),
    case is_process_alive(S#source.cdr_writer_pid) of  %% belts and braces check. Could cause infinite cycling...
	false ->
	    error_logger:error_msg("CDR writer died unexpectedly. resetting"),
	    NewS = S#source{cdr_writer_pid = none},
	    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, NewS)};
	_ -> State
    end;
check_pending_buffer(_,_,State) ->
    ?PRINTDEBUG("check_pending_buffer - no records to write"),
    State.

check_duplicate_buffer(TS, S, State) when S#source.cdr_writer_pid == none ->
    ?PRINTDEBUG("check_duplicate_buffer state"),
    TScomp = oldest_TS(S#source.possible_duplicate_list)+State#state.file_age_limit_seconds,
    if (TScomp < TS) or length(S#source.possible_duplicate_list) > State#state.possible_duplicate_limit_seconds ->
	    Pid = spawn_link(?MODULE, log_duplicate_cdr, [S, State]),  %% will write
	    NewS = S#source{cdr_writer_pid = Pid},
	    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, NewS)};
       true ->
	    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, S)}
    end;
check_duplicate_buffer(_,S,State) when not S#source.cdr_writer_pid == none ->
    ?PRINTDEBUG("check_duplicate_buffer state - busy writing buffer already"),
    case is_process_alive(S#source.cdr_writer_pid) of  %% belts and braces check. Could cause infinite cycling...
	false ->
	    error_logger:error_msg("CDR writer died unexpectedly. resetting"),
	    NewS = S#source{cdr_writer_pid = none},
	    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, NewS)};
	_ -> State
    end;
check_duplicate_buffer(_,_,State) ->
   ?PRINTDEBUG("check_duplicate_buffer state - no records to write"),
    State.

get_source(SourceKey, State) ->
    case lists:keysearch(SourceKey, 2, State#state.known_sources) of
	{value, S1} -> S1;
	_ ->
	    #source{address = SourceKey,
		    possible_duplicate_list=[],
		    pending_write_list=[],
		    old_seq_nums_ringbuffer=[],
		    pending_records=0,
		    cdr_writer_pid = none
		   }
    end.

log_cdr(S, State) ->
    ?PRINTDEBUG2("Logging CDRs PID ~p",[self()]),
    Now = now(),
    Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ""),
    Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ""), 
    SeqNums = write_cdrs(S#source.pending_write_list, Temp_filename, Final_filename),
    cdr_file_srv:flush_pending(S#source.address,SeqNums),
    case SeqNums of
	[] -> ok;
	_ -> execute_post_close_command(State#state.post_close_command, Final_filename)
    end.



log_duplicate_cdr(S, State) ->
    ?PRINTDEBUG2("Logging duplicate CDRs PID ~p",[self()]),
    Now = now(),
    Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ".possible_duplicate"),
    Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ".possible_duplicate"), 
    SeqNums = write_cdrs(S#source.possible_duplicate_list, Temp_filename, Final_filename),
    cdr_file_srv:flush_pending_duplicates(S#source.address,SeqNums),
    case SeqNums of
	[] -> ok;
	_ ->  execute_post_close_command(State#state.post_close_command, Final_filename)
    end.

build_filename(Address, Now, Dir, Suffix) ->
    {{YY,MMM,DD},{HH,MM,SS}} = calendar:now_to_universal_time(Now),
    DT = io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B",[YY, MMM, DD, HH, MM, SS]),
    {ok,HN} = inet:gethostname(),
    filename:join([Dir, "CDR-" ++ HN ++ "-" ++ pretty_format_address(Address) ++ "-" ++ DT ++ Suffix]).

write_cdrs([], _, _) ->
    ?PRINTDEBUG("No CDRs to write"),
    [];
write_cdrs(List, Temp_filename, Final_filename) ->
    ok = filelib:ensure_dir(Temp_filename),
    ok = filelib:ensure_dir(Final_filename),
    %% log data to file
    {ok, F} = file:open(Temp_filename, [raw, write, delayed_write]),
    SeqNums = write_cdr(F, List),
    ok = file:close(F),
    ok = file:rename(Temp_filename, Final_filename),
    SeqNums.

write_cdr(F, List) ->
    write_cdr(F, List, []).
write_cdr(_, [], SeqNums) ->
    ?PRINTDEBUG2("Written SeqNums ~p",[SeqNums]),
    SeqNums;
write_cdr(F, [{SeqNum, _, {{_,Data},_}}|Rest], SeqNums) ->
    ok = file:write(F, Data),
    write_cdr(F, Rest, SeqNums ++ [SeqNum]).

oldest_TS([]) ->
    greg_now();
oldest_TS([{_, TS, _}|_]) ->
    TS.

greg_now() ->
    calendar:datetime_to_gregorian_seconds({date(), time()}).

update_ringbuffer(OldSeqNums, SeqNums, MaxSize) when length(OldSeqNums) =< length(SeqNums)+MaxSize ->
    lists:append(SeqNums, OldSeqNums);
update_ringbuffer(OldSeqNums, SeqNums, MaxSize) ->
    NewList = lists:sublist(OldSeqNums, (MaxSize-length(SeqNums))-1),
    update_ringbuffer(NewList, SeqNums, MaxSize).
							

delete_seqnums(OldSeqNums, []) ->
%%    ?PRINTDEBUG2("delete_seqnums, leaving with ~p",[OldSeqNums]),
    OldSeqNums;
delete_seqnums(OldSeqNums, [SeqNum|Rest]) ->
    delete_seqnums(lists:keydelete(SeqNum, 1, OldSeqNums), Rest).


%% check for duplicates, most likely/shortest first
check_duplicate(S, Seq_num) ->
    case lists:keymember(Seq_num, 1, S#source.pending_write_list) of
	true -> true;
	false ->
	    case lists:keymember(Seq_num, 1, S#source.possible_duplicate_list) of
		true -> true;
		false ->
		    lists:member(Seq_num, S#source.old_seq_nums_ringbuffer)
	    end
    end.

notify_cdf({udp, Address, Port}, Seq_nums, Cause) ->
    gtpp_udp_server:confirm({Address, Port}, Seq_nums, Cause);
notify_cdf({tcp, Address, Port}, Seq_nums, Cause) ->
    gtpp_tcp_server:confirm({Address, Port}, Seq_nums, Cause).

%% flushes all records when the first in the queue expires. 
flush_pending_timer(State) ->
    lists:foreach(fun(S) ->
			  TSnow = greg_now(),
			  case S#source.pending_write_list of
			      [] ->
				  ok;
			      [{_,TS,_}|_] -> %% check the first record, if expired then flush all
				  if(TS+State#state.file_age_limit_seconds > TSnow) ->
					  SeqNums = lists:foldl(fun({SeqNum, _, _}, Acc) ->
									Acc ++ [SeqNum]
								end, [], S#source.pending_write_list),
					  Now = now(),
					  Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ""),
					  Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ""), 
					  SeqNums = write_cdrs(S#source.pending_write_list, Temp_filename, Final_filename),
					  gen_server:cast(?SERVER, {flush_pending_from_timer,
								    S#source.address, SeqNums}),
					  case SeqNums of
					      [] -> ok;
					      _ -> execute_post_close_command(State#state.post_close_command, Final_filename)
					  end;
				  true ->
					  ok
				  end
			  end
		  end, State#state.known_sources).

%% flushes only those records that have timed out, allowing the CDF is decide to remove some of the non-expired ones at it's pleasure
flush_duplicates_timer(State) ->
    lists:foreach(fun(S) ->
			  TSnow = greg_now(),
			  List = lists:filter(fun({_, TS, _}) ->
						      (TS+State#state.possible_duplicate_limit_seconds > TSnow)
					      end, S#source.possible_duplicate_list),
			  case List of 
			      [] -> ok;
			      _ ->
				  Now = now(),
				  Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ".possible_duplicate"),
				  Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ".possible_duplicate"), 
				  SeqNums = write_cdrs(List, Temp_filename, Final_filename),
				  gen_server:cast(?SERVER, {flush_pending_duplicates_from_timer, S#source.address, SeqNums}),
				  case SeqNums of
				      [] -> ok;
				      _ -> execute_post_close_command(State#state.post_close_command, Final_filename)
				  end
			  end
		  end, State#state.known_sources).
    

pretty_format_address({_, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, Port]);
pretty_format_address({_, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port]).


%% inefficient replacement for keystore from R12B. We will remove this in a few releases and return to lists:keystore.
keystore(K, N, L, New) ->
    NewList = lists:keydelete(K,N,L),
    [New|NewList].

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
		       
