%% NON GPL - EPL Licensed. Slightly modified from eddie source.

-module('open-cgf_logger').
-author('tobbe@eddieware.com.defunct').

-behaviour(gen_event).

-include("open-cgf.hrl").

%% External exports
-export([add_handler/0,add_handler/1,add_handler/2,error/1,error/2,
         warning/1, warning/2, info/1,info/2, debug/1, debug/2, log/3,log/5]).
-export([emergency/0,alert/0,critical/0,error/0,warning/0,
         notice/0,info/0,debug/0]).
-export([kern/0,user/0,mail/0,daemon/0,auth/0,syslog/0,lpr/0,
         news/0,uucp/0,cron/0,authpriv/0,ftp/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-define(WHO, 'open-cgf').       % Under what name syslog should log the info.

-record(state,
        {sock,             % UDP socket
         host,             % Host running syslogd
         port=514          % The syslog UDP port number
        }).

%% --------------------
%% Exported interface.

%% Add the report handler. As default, the syslog
%% facility will be used on the local host.

add_handler()          -> add_handler(syslog).
add_handler(Type)      -> error_logger:add_report_handler(?MODULE,[Type]).
add_handler(Type,IPPort) -> error_logger:add_report_handler(?MODULE,[Type,IPPort]).

%% A couple of examples:
%%  error("no response from front-end~n").
%%  info("~w: dns server ~w started~n", [time(), DnsServer]).
%%  log(emergency(), "the network card is on fire~n", []).
%%  log(eddie_ftp, ftp(), notice(), "replacing std-ftp~n", []).

error(Msg)      -> log(error(),Msg,[]).
error(Msg,Args) -> log(error(),Msg,Args).

warning(Msg)    -> log(warning(), Msg, []).
warning(Msg, Args) -> log(warning(), Msg, Args).    

info(Msg)      -> log(info(),Msg,[]).
info(Msg,Args) -> log(info(),Msg,Args).

debug(Msg)       -> 
    ?PRINTDEBUG(Msg),
    log(debug(), Msg, []).
debug(Msg, Args) -> 
    ?PRINTDEBUG2(Msg, Args),
    log(debug(), Msg, Args).
    

log(Level,Msg,Args) when integer(Level),list(Msg),list(Args) ->
    error_logger:error_report(syslog, {{?WHO,Level},{Msg,Args}}).

log(Who,Facility,Level,Msg,Args) when atom(Who),integer(Facility),
                                      integer(Level),list(Msg),list(Args) ->
    error_logger:error_report(syslog, {{Who,Facility,Level},{Msg,Args}}).

%% Convenient routines for specifying levels.

emergency() -> 0. % system is unusable
alert()     -> 1. % action must be taken immediately
critical()  -> 2. % critical conditions
error()     -> 3. % error conditions
warning()   -> 4. % warning conditions
notice()    -> 5. % normal but significant condition
info()      -> 6. % informational
debug()     -> 7. % debug-level messages

level(0) -> "EMERGENCY";
level(1) -> "ALERT";
level(2) -> "CRITICAL";
level(3) -> "ERROR";
level(4) -> "WARNING";
level(5) -> "NOTICE";
level(6) -> "INFO";
level(7) -> "DEBUG".

%% Convenient routines for specifying facility codes

kern()     -> (0 bsl 3) . % kernel messages
user()     -> (1 bsl 3) . % random user-level messages
mail()     -> (2 bsl 3) . % mail system
daemon()   -> (3 bsl 3) . % system daemons
auth()     -> (4 bsl 3) . % security/authorization messages
syslog()   -> (5 bsl 3) . % messages generated internally by syslogd
lpr()      -> (6 bsl 3) . % line printer subsystem
news()     -> (7 bsl 3) . % network news subsystem
uucp()     -> (8 bsl 3) . % UUCP subsystem
cron()     -> (9 bsl 3) . % clock daemon
authpriv() -> (10 bsl 3). % security/authorization messages (private)
ftp()      -> (11 bsl 3). % ftp daemon


%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([syslog]) ->
    S = init_syslog(local_host()),
    {ok,S};
init([syslog,IPPort]) ->
    S = init_syslog(IPPort),
    {ok,S};
init(Other) ->
    io:fwrite("~w: <ERROR> No support for ~w , no handler is added !~n",
              [?MODULE,Other]),
    {error,not_supported}.

init_syslog({IP,Port}) ->
    {ok,Sock} = gen_udp:open(0),
    #state{sock=Sock,
           host=IP,
	   port=Port}.


%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({error_report,_GLPid,{_Pid,syslog,{{Who,Level},{Msg,Args}}}},S) ->
    try lists:flatten(level(Level) ++ ":" ++ io_lib:format(Msg,Args)) of
	Emsg ->     do_send(S#state.sock,S#state.host,S#state.port,{Who,Level,Emsg})

    catch
	exit: _Why ->
	    ok
    end,
    {ok,S};
handle_event({error_report,_GLPid,{_Pid, syslog,{{Who,Facility,Level},{Msg,Args}}}},S) ->
    try lists:flatten(level(Level) ++ ": " ++ io_lib:format(Msg,Args)) of
	Emsg ->         do_send(S#state.sock,S#state.host,S#state.port,{Facility,Who,Level,Emsg})
    catch
	exit: _Why ->
	    ok
    end,
    {ok,S};

handle_event({error_msg,_GLPid,{_Pid, Format, Data}},State) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,error()},{Format, Data}}}},State),
    {ok,State};
handle_event({warning_msg,_GLPid,{_Pid, Format, Data}},State) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,warning()},{Format, Data}}}},State),
    {ok,State};
handle_event({info_msg,_GLPid,{_Pid, Format, Data}},State) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,info()},{Format, Data}}}},State),
    {ok,State};

%handle_event({_Type,_GLPid,{_Pid, progress, _Report}},State) -> %% we don't log progress reports
%    {ok, State};
handle_event({error_report,_GLPid,{_Pid, Type, Report}},State) when atom(Type) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,error()},{"~p: ~p", [Type, Report]}}}},State),
    {ok,State};
handle_event({warning_report,_GLPid,{_Pid, Type, Report}},State) when atom(Type) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,warning()},{"~p: ~p", [Type, Report]}}}},State),
    {ok,State};
handle_event({info_report,_GLPid,{_Pid, Type, Report}},State) when atom(Type) ->
    handle_event({error_report,none,{none, syslog, {{?WHO,info()},{"~p: ~p", [Type, Report]}}}},State),
    {ok,State};
%% ignore other events, such as progress events.
handle_event(Msg, State) ->
    io:format("open-cgf_logger - going to log something like ~p",[Msg]),    
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_udp:close(State#state.sock),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% priorities/facilities are encoded into a single 32-bit
%% quantity, where the bottom 3 bits are the priority (0-7)
%% and the top 28 bits are the facility (0-big number).    

do_send(Sock, Host,Port,{Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg,
    gen_udp:send(Sock,Host,Port,Packet);
do_send(Sock, Host,Port,{Facil,Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Facil bor Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg,
    gen_udp:send(Sock,Host,Port,Packet).

local_host() ->
    {ok,Hname} = inet:gethostname(),
    Hname.

i2l(Int) -> integer_to_list(Int).

a2l(Atom) -> atom_to_list(Atom).
