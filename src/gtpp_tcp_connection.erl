%%%-------------------------------------------------------------------
%%% File    : gtpp_tcp_connection.erl
%%% Author  : root <root@one.w8trk.com>
%%% Description : 
%%%
%%% Created : 26 Jan 2008 by root <root@one.w8trk.com>
%%%-------------------------------------------------------------------
-module(gtpp_tcp_connection).

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 60).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-record(state, {socket}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([LS]) ->
    process_flag(trap_exit, true),
    {ok, waiting_for_data, #state{socket=LS}}.


%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket=S} = State) ->
    ok = gen_tcp:send(S, Data),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};

'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State};

'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

waiting_for_data(_Event, State) ->
    {waiting_for_data, waiting_for_data, State}.


%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
