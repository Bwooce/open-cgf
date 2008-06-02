%%%-------------------------------------------------------------------
%%% File    : test_udp_client.erl
%%% Author  : root <root@one.w8trk.com>
%%% Description : Minimal UDP client.
%%%
%%% Created : 13 May 2008 by root <root@one.w8trk.com>
%%%-------------------------------------------------------------------
-module(test_udp_client).

-behaviour(gen_server).

-include("open-cgf.hrl").

%% API
-export([start_link/2, send/2, register/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, owner_pid}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(IP, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), IP, Port], []).

%% register as a parent who wants the received messages
register(Pid) ->
    gen_server:call(?SERVER, {register, Pid}).

%% send a message (once, dumb impl)
send(Dest, Msg) ->
    gen_server:call(?SERVER, {send, Dest, Msg}).

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
init([OwnerPid, IP, Port]) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {ip, IP}]),
    process_flag(trap_exit, true),
    {ok, #state{socket=Socket, owner_pid=OwnerPid}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, Pid}, _From, State) ->
    {reply, ok, State#state{owner_pid = Pid}};

handle_call({send, {IP,Port}, Msg}, _From, State) ->
    Reply = gen_udp:send(State#state.socket, IP, Port, Msg),
    {reply, Reply, State};

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
handle_info({udp, InSocket, InIP, InPort, Packet}, State) ->
    ?PRINTDEBUG2("Got Message ~p from ~s:~p", [Packet, inet_parse:ntoa(InIP), InPort]),
    case gtpp_decode:decode_message(Packet) of
     %% {ok,{{gtpp_header,0,0,1,echo_response,2,1},[{count,7},<<>>}]}
	{ok, Message} ->
	    State#state.owner_pid ! {udp, {InSocket, InPort}, Message};
	_ ->
	    ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
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


