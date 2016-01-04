%%%-------------------------------------------------------------------
%% @doc supervisor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('supervisor_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    % allowed 1 restart each 5 second
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
                  #{id => mysupervisor,
                    start => {mysupervisor, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [mysupervisor]}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
