-module(service).

-behaviour(gen_fsm).

-export([spawn/1]).
-export([start/1]).
-export([exit/1]).

-export([get_state/1]).
-export([notify_exit/2]).
-export([add_children/2]).

-export([init/1,
         started/2,
         spawned/2,
         exited/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {name, provider, dependencies, children}).

spawn(Args) ->
    [{name, N} | _] = Args,
    gen_fsm:start({local, N},?MODULE, Args, []).

start(Service) ->
    gen_fsm:send_event(Service, start).

exit(Service) ->
    gen_fsm:send_event(Service, exit).

notify_exit(Service, ExitStatus) ->
   gen_fsm:send_event(Service, {exited, ExitStatus}).

add_children(Service, Children) ->
   gen_fsm:send_event(Service, {add_children, Children}).

get_state(Service) ->
    gen_fsm:sync_send_all_state_event(Service, get_state).

init([]) ->
        {ok, spawned, #state{}};

init([{name, Name}, {provider, ProviderName}, {params, Params}, {dependencies, Dependencies}]) ->
        io:format("[~p] spawned ~p with provider ~p and dependencies ~p~n", [?MODULE, Name, ProviderName, Dependencies]),

        {ok, Provider} = bash:spawn({Name, Params}),
        S = #state{name = Name,
                   provider = Provider,
                   dependencies = Dependencies,
                   children = []},

        {ok, spawned, S}.

spawned({add_children, Children}, S) ->
        {next_state, spawned, S#state{children = Children}};
spawned(start, #state{name = Name, dependencies = Dependencies} = S) ->
    NextState = case mysupervisor:are_started(Dependencies) of
        true ->
            start_service(S),
            started;
        false ->
            io:format("[~p] ~p :: dependencies not resolved ~n", [?MODULE, Name]),
            spawned
    end,
    {next_state, NextState, S}.

started(start, #state{name = Name} = State) ->
    io:format("[~p] ~p :: Already started ~n", [?MODULE, Name]),
    {next_state, started, State};
started(exit, #state{name = Name, provider = Provider} = State) ->
    io:format("[~p] ~p :: Exit ~n", [?MODULE, Name]),
    bash:stop(Provider),
    {next_state, started, State};
started({exited, ExitStatus}, #state{name = Name} = S) ->
    io:format("[~p] ~p :: Exited with ExitStatus ~p ~n", [?MODULE, Name, ExitStatus]),
    mysupervisor:exited(Name),
    {next_state, exited, S}.

exited(exit, #state{name = Name} = S) ->
    io:format("[~p] ~p :: Already Exited~n", [?MODULE, Name]),
    {next_state, exited, S}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, StateName, StateName, State}.

handle_info(_Info, StateName, State) ->
        io:format("[~p] handle_info: ~p", [?MODULE, _Info]),
        {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
        ok.

code_change(_OldVsn, StateName, State, _Extra) ->
        {ok, StateName, State}.

start_children(#state{children = Children}) ->
    lists:foreach(fun(Child) -> service:start(Child) end, Children),
    ok.

start_service(#state{name = Name, provider = Provider} = S) ->
    io:format("[~p] ~p :: started ~n", [?MODULE, Name]),

    bash:start(Provider),
    start_children(S).
