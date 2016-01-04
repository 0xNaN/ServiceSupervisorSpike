-module(bash).

-behaviour(gen_server).

-export([spawn/1]).
-export([start/1]).
-export([stop/1]).

-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-define(SIGTERM, 15).
-define(SERVER, ?MODULE).

-record(params, {name, command, ospid, pid}).

spawn(Params) ->
    gen_server:start(?MODULE, Params, []).

start(BashProvider) ->
    gen_server:call(BashProvider, start).

stop(BashProvider) ->
    gen_server:call(BashProvider, stop).

init({Name, #{command := Command}}) ->
    io:format("[~p] Provider Ready ~n", [?MODULE]),
    process_flag(trap_exit, true),
    {ok, #params{name = Name, command = Command}}.

handle_call(start, _From, #params{command = Command} = Params) ->
    {ok, Pid, OSPid} = exec:run_link("bash -c \"" ++ Command ++ "\"", []),
    io:format("[~p] Started proccess, PID: ~p ~n", [?MODULE, OSPid]),
    {reply, ok, Params#params{ospid = OSPid, pid = Pid}};

handle_call(stop, _From, #params{ospid = OSPid, pid = Pid} = Params) ->
    io:format("[~p] Stopping proccess: ~p ~n", [?MODULE, OSPid]),
    exec:kill(Pid, ?SIGTERM),
    {reply, ok, Params};

handle_call(post_exec, _From, #params{ospid = OSPid} = Params) ->
    io:format("[~p] Post-Exec for ~p~n", [?MODULE, OSPid]),
    {reply, ok, Params}.

handle_cast(_M, Params)  ->
    {noreply, Params}.

handle_info({'EXIT', RecvPid, {exit_status, ExitStatus}}, #params{name = Name, pid = Pid} = Params) when RecvPid =:= Pid ->
    service:notify_exit(Name, ExitStatus),
    {noreply, Params};

handle_info(_Msg, Params) ->
    {noreply, Params}.

terminate(_Reason, _State) ->
    io:format("[~p] Process Died: ~p~n", [?MODULE, _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
