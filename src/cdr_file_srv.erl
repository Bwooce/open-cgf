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
-export([start_link/0, check_duplicate/2, log/3, log_possible_dup/3, commit_possible_dup/3, remove_possible_dup/3, reset/1]).
-export([print_state/0]).

%% internal API
-export([log_duplicate_cdr/2, flush_pending_duplicates/2]).
-export([flush_duplicates_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {known_sources,
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

-record(source, {address, %% {IP,Port} of sending CDF
		 cdr_file_handle, %% one file per destination, undefined if closed
		 cdr_timer, %%% TID or undefined
		 cdr_file_name, %% file name in the temporary directory
		 cdr_final_file_name, %% filename in the final directory
		 cdr_file_ts, %% timestamp of file open (so we know when to close it, if fairly idle)
		 cdr_file_count, %% record count of cdr file (so we know when to clost it, if under load)
		 possible_duplicate_list, %% [{seq_num, ts, [CDRs]}] of acknowledged but unwritten CDRs. Written eventually even if not confirmed. 
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

check_duplicate(SourceKey, Seq_num) ->
    gen_server:call(?SERVER, {check_duplicate, SourceKey,Seq_num}).

log(Source, Seq_num, Data) -> 
    gen_server:call(?SERVER, {log, Source, Seq_num, Data}).

log_possible_dup(SourceKey, Seq_num, Data) ->
    gen_server:cast(?SERVER, {log_possible_dup, SourceKey, Seq_num, Data}).

commit_possible_dup(SourceKey, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {commit_possible_dup, SourceKey, Seq_num, Seq_nums}).

remove_possible_dup(SourceKey, Seq_num, Seq_nums) ->
    gen_server:cast(?SERVER, {remove_possible_dup, SourceKey, Seq_num, Seq_nums}).

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
    {ok, #state{known_sources=[], cdr_dir=CDR_dir, cdr_temp_dir=CDR_temp_dir,
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
handle_call({check_duplicate, SourceKey, Seq_num}, _From, State) ->
    S = get_source(SourceKey, State),
    Result = lists:keymember(Seq_num, 1, S#source.possible_duplicate_list),
    {reply, Result, State};

handle_call({log, Source, Seq_num, Data}, _From, State) ->
    {ok, NewState} = write_cdr(Source, Seq_num, Data, State),
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
    {ok, NewState} = write_cdr(Source, Seq_num, Data, State),
    {noreply, NewState};

handle_cast({log_possible_dup, SourceKey, Seq_num, Data}, State) ->
    {ok, NewState} = buffer_duplicate_cdr(SourceKey, Seq_num, Data, State),
    {noreply, NewState};

handle_cast({remove_possible_dup, SourceKey, _Seq_num, Seq_nums}, State) ->
    S = get_source(SourceKey, State),
    NewS = lists:foldl(fun(SSeq_num, OldS) ->
			       OldS#source{possible_duplicate_list=
					   lists:keydelete(SSeq_num, 1, OldS#source.possible_duplicate_list)} %% remove the CDR, if possible
		      end, S, Seq_nums),		       
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS)}};

handle_cast({commit_possible_dup, SourceKey, Seq_num, Seq_nums},State) ->
    S = get_source(SourceKey, State),
    NewState2 = lists:foldl(fun(SSeq_num, OldS) ->
				    case lists:keysearch(SSeq_num, 1, OldS#source.possible_duplicate_list) of
					false ->
					    OldS; %% wasn't in the list, can't do much
					{value, {Seq_num, _TS, Data}} ->
					    NewState = write_cdr(SourceKey, Seq_num, Data, State),
					    NewS = OldS#source{possible_duplicate_list=
							       lists:keydelete(SSeq_num, 1, OldS#source.possible_duplicate_list)}, %% remove the CDR
					    NewState#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS)}
				    end
			    end, S, Seq_nums),
    {noreply, NewState2};

handle_cast({flush_pending_duplicates, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending_duplicates, SourceKey, SeqNums}, State) ->
    %% no need to ack as these are already ack'd
    S = get_source(SourceKey, State),
    NewS = S#source{cdr_writer_pid=none,
		    possible_duplicate_list=delete_seqnums(S#source.possible_duplicate_list, SeqNums)},
    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};

handle_cast({flush_pending_duplicates_from_timer, _, []}, State) ->
    {noreply, State};
handle_cast({flush_pending_duplicates_from_timer, SourceKey, SeqNums}, State) ->
    %% no need to ack as these are already ack'd
    S = get_source(SourceKey, State),
    case S#source.cdr_writer_pid of
	none ->
	    NewS = S#source{possible_duplicate_list=delete_seqnums(S#source.possible_duplicate_list, SeqNums)},
	    {noreply, State#state{known_sources=keystore(SourceKey, 2, State#state.known_sources, NewS) }};
	_ ->
	    {noreply, State} %% we can't stomp on an existing writer sessions
    end;

handle_cast({reset, SourceKey}, State) ->
    %% write all the CDRs, close the files
    S = get_source(SourceKey, State),
    Pid = spawn_link(?MODULE, log_duplicate_cdr, [S, State]), 
    close_cdr_file(S),
    NewS = S#source{cdr_file_handle=undefined,
		    cdr_timer=undefined,
		    cdr_file_name=[],
		    cdr_final_file_name=[],
	            cdr_file_ts=0,
		    cdr_file_count=0,
	            possible_duplicate_list=[],
		    cdr_writer_pid=Pid},
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
handle_info({close_cdr, SourceKey}, State) ->
    S = get_source(SourceKey, State),
    NewState = close_source(S, State),
    {noreply, NewState};

handle_info({close_duplicates_tick}, State) ->
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
terminate(Reason, State) ->
    error_logger:info_msg("cdr_file_srv is shutting down for reason: ~p",[Reason]),
    lists:foreach(fun(S) ->
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
write_cdr(_SourceKey, _Seq_num, [{_,[]}|_], State) ->
    ?PRINTDEBUG("Write CDR with no CDR"),
    {ok, State};

write_cdr(SourceKey, Seq_num, [{_,Data}|_], State) ->
    ?PRINTDEBUG2("Writing CDR for ~p, seqnum ~B",[SourceKey, Seq_num]),
    S = get_source(SourceKey, State),
    NewState = write_cdr_source(S, Seq_num, Data, State),
    {ok, NewState}.

buffer_duplicate_cdr(SourceKey, Seq_num, Data, State) ->
    ?PRINTDEBUG2("Buffering possible duplicate CDR for ~p, seqnum ~B",[SourceKey, Seq_num]),
    NewState = check_buffer(SourceKey, State),
    TS = greg_now(),
    S = get_source(SourceKey, NewState),
    NewS = S#source{possible_duplicate_list = S#source.possible_duplicate_list ++ [{Seq_num, TS, Data}]},
    KS = keystore(SourceKey, 2, NewState#state.known_sources, NewS),
    {ok, NewState#state{known_sources=KS}}.

%% check to see if the duplicate_buffer needs anything to be flushed to disk.
check_buffer(SourceKey, State) ->
    TS = greg_now(),
    S = get_source(SourceKey, State),
    State2 = check_duplicate_buffer(TS, S, State),
    State2.

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
		    cdr_file_handle=undefined,
		    cdr_file_name=[],
		    cdr_final_file_name=[],
	            cdr_file_ts=0,
		    cdr_file_count=0,
	            possible_duplicate_list=[],
		    cdr_writer_pid=none}
    end.

write_cdr_source(S, _SeqNum, Data, State) ->
    NewS = case S#source.cdr_file_handle of
	       undefined ->
		   Now = now(),
		   Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ".cdr"),
		   Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ".cdr"), 
		   ok = filelib:ensure_dir(Temp_filename),
		   ok = filelib:ensure_dir(State#state.cdr_dir),
		   ?PRINTDEBUG2("Opening CDR file ~s",[Temp_filename]),
		   {ok, F} = file:open(Temp_filename, [raw, write, delayed_write]),
		   %% set timer for closure
		   {ok, TID} = timer:send_after(State#state.file_age_limit_seconds*1000, {close_cdr, S#source.address}),
		   N = S#source{cdr_file_handle=F,
				cdr_timer=TID,
				cdr_file_name=Temp_filename,
				cdr_final_file_name=Final_filename,
				cdr_file_ts=greg_now(),
				cdr_file_count=0},
		   N;
	       _H -> 
		   S#source{cdr_file_count=S#source.cdr_file_count+1}
	   end,
%%    ?PRINTDEBUG2("Going to write to Handle ~p, Data ~p",[NewS#source.cdr_file_handle, Data]),
    ok = file:write(NewS#source.cdr_file_handle, Data),
    check_file_count(NewS,State). %% check_file_count always updates the state


log_duplicate_cdr(S, State) ->
    ?PRINTDEBUG2("Logging duplicate CDRs PID ~p",[self()]),
    Now = now(),
    Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ".possible_duplicate.cdr"),
    Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ".possible_duplicate.cdr"), 
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
    ?PRINTDEBUG2("Written SeqNums ~s",['open-cgf_logger':format_seqnums(SeqNums)]),
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


delete_seqnums(OldSeqNums, []) ->
%%    ?PRINTDEBUG2("delete_seqnums, leaving with ~p",[OldSeqNums]),
    OldSeqNums;
delete_seqnums(OldSeqNums, [SeqNum|Rest]) ->
    delete_seqnums(lists:keydelete(SeqNum, 1, OldSeqNums), Rest).


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
				  Temp_filename = build_filename(S#source.address, Now, State#state.cdr_temp_dir, ".possible_duplicate.cdr"),
				  Final_filename = build_filename(S#source.address, Now, State#state.cdr_dir, ".possible_duplicate.cdr"), 
				  SeqNums = write_cdrs(List, Temp_filename, Final_filename),
				  gen_server:cast(?SERVER, {flush_pending_duplicates_from_timer, S#source.address, SeqNums}),
				  case SeqNums of
				      [] -> ok;
				      _ -> execute_post_close_command(State#state.post_close_command, Final_filename)
				  end
			  end
		  end, State#state.known_sources).
    

pretty_format_address({udp, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, Port]);
pretty_format_address({udp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-~B",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, Port]);
%% TCP uses ephemeral ports, and we don't have any other identifier...
pretty_format_address({tcp, {IP1,IP2,IP3,IP4},Port}) ->
    io_lib:format("~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4]);
pretty_format_address({tcp, {IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8},Port}) ->
    io_lib:format("~B_~B_~B_~B_~B_~B_~B_~B-tcp",[IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8]).

    


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

close_source(S, State) ->       
    ?PRINTDEBUG2("Closing cdr file for ~p",[S#source.address]),
    ok = close_cdr_file(S),
    execute_post_close_command(State#state.post_close_command, S#source.cdr_final_file_name),
    NewS = S#source{cdr_file_handle=undefined,
		    cdr_timer=undefined,
		    cdr_file_name=[],
		    cdr_final_file_name=[],
		    cdr_file_ts=0,
		    cdr_file_count=0},
    State#state{known_sources=keystore(S#source.address, 2, State#state.known_sources, NewS)}.

close_cdr_file(S) when S#source.cdr_file_handle =:= undefined ->
    ok;
close_cdr_file(S) ->
    timer:cancel(S#source.cdr_timer),
    ok = file:close(S#source.cdr_file_handle),
    ok = file:rename(S#source.cdr_file_name, S#source.cdr_final_file_name).


check_file_count(Source, State) ->
    if(Source#source.cdr_file_count >= State#state.file_record_limit) ->
	    close_source(Source, State);
      true ->
	    State#state{known_sources=keystore(Source#source.address, 2, State#state.known_sources, Source)}
    end.
