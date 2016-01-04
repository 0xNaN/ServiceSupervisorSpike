-module(mysupervisor).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([example/0]).
-export([execute/1]).
-export([exited/1]).
-export([are_started/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {services, children_edges}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

example() ->
    Services = [
                #{name => foo,
                provider => "bash",
                restart => "always",
                requires => [bar],
                params => #{
                  command => "while [ 1 ]; do echo foo; sleep 1; done"
                 }
               },
                #{name => baz,
                provider => "bash",
                restart => "always",
                requires => [],
                params => #{
                  command => "while [ 1 ]; do echo baz; sleep 1; done"
                 }
               },

              #{name => bar,
                provider => "bash",
                restart => "always",
                requires => [],
                params => #{
                  command => "while [ 1 ]; do echo bar; sleep 1; done"
                 }
               }],
    execute(Services).

execute([]) ->
    gen_server:call(?MODULE, link_children),
    gen_server:call(?MODULE, start);
execute([Service | Services]) ->
    gen_server:call(?MODULE, {spawn_service, Service}),
    execute(Services).

are_started(Services) ->
    lists:all(fun is_started/1, Services).

exited(Service) ->
    gen_server:cast(?MODULE, {exited, Service}).

init([]) ->
    {ok, #state{services = [], children_edges = #{}}}.

handle_call({spawn_service, Service}, _, #state{services = Services, children_edges = ChildrenEdges} = S) ->
    #{name := Name} = Service,
    #{provider := Provider} = Service,
    #{params := Params} = Service,
    #{requires := Requires} = Service,

    {ok, NewService} = service:spawn([{name, Name},
                                      {provider, Provider},
                                      {params, Params},
                                      {dependencies, Requires}]),

    NewServices = [NewService | Services],
    NewChildrenEdges = update_children_edges(NewService, Requires, ChildrenEdges),
    {reply, ok, S#state{services = NewServices, children_edges = NewChildrenEdges}};

handle_call(link_children, _From, #state{children_edges = ChildrenEdges} = S) ->
    lists:foreach(fun(Parent) -> service:add_children(Parent, maps:get(Parent, ChildrenEdges)) end, maps:keys(ChildrenEdges)),
    {reply, ok, S};

handle_call(start, _From, #state{services = Services} = S) ->
    lists:foreach(fun(Service) -> service:start(Service) end, Services),
    {reply, ok, S};

handle_call({is_started, Service}, _From, S) ->
    Reply = started =:= service:get_state(Service),
    {reply, {ok, Reply}, S};

handle_call(_Request, _From, S) ->
    Reply = ok,
    {reply, Reply, S}.

handle_cast({exited, Service}, #state{services = Services, children_edges = ChildrenEdges} = S) ->
    io:format("[~p] ~p exited~n", [?MODULE, Service]),

    exit_children(Service, Services, ChildrenEdges),
    NewServices = lists:delete(Service, Services),
    NewChildrenEdges = maps:remove(Service, ChildrenEdges),
    %% XXX: if it has a restart policy should be here
    {noreply, S#state{services = NewServices, children_edges = NewChildrenEdges}};

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

update_children_edges(Service, Requires, ChildrenEdges) ->
    UpdateChildrenHandler = fun(Parent, NewChildrenEdges) -> add_child_edge(Service, Parent, NewChildrenEdges) end,
    lists:foldl(UpdateChildrenHandler, ChildrenEdges, Requires).

add_child_edge(Service, Parent, ChildrenEdges) ->
    Children = case maps:is_key(Parent, ChildrenEdges) of
                   true -> maps:get(Parent, ChildrenEdges);
                   false -> []
               end,
    ChildrenEdges#{Parent => [Service | Children]}.

exit_children(Service, LocalServices, ChildrenEdges) ->
    ServiceChildren = case maps:is_key(Service, ChildrenEdges) of true -> maps:get(Service, ChildrenEdges); false -> [] end,
    LocalChildren = lists:takewhile(fun(Child) -> lists:member(Child, LocalServices) end, ServiceChildren),
    %% XXX: here we should exit all remote children
    lists:foreach(fun(Child) -> service:exit(Child) end, LocalChildren).

is_started(Service) ->
    case gen_server:call(?MODULE, {is_started, Service}) of
        {ok, true} -> true;
        {ok, false} -> false; % XXX: check globally if a servies is started
        _ -> false
    end.
