Sercvice Supervisor
=====
The aim of this spike is to try to understand how should look an application that allows to deploy an infrastructure defined through a set of services that is capable to resolve dependencies (locally and globally) and monitor the process that owns
and to responds to events such as process exit. 

`mysupervisor:example()` tries to run three services: `foo`, `bar` and `baz`, where `foo` can starts only if `bar` is already
started.

    # rebar3 as test shell
    1> mysupervisor:example()
    [service] spawned foo with provider "bash" and dependencies [bar]
    [bash] Provider Ready 
    [service] spawned baz with provider "bash" and dependencies []
    [bash] Provider Ready 
    [service] spawned bar with provider "bash" and dependencies []
    [bash] Provider Ready 
    [service] bar :: started 
    [service] baz :: started 
    [bash] Started proccess, PID: 8616 
    [service] foo :: started 
    [bash] Started proccess, PID: 8617 
    [bash] Started proccess, PID: 8618 
    [service] foo :: Already started 
    
if you try to kill one of the started processes, the app should responds to the event. 
For example if `bar` gets killed also its child `foo` gets killed.

    [service] bar :: Exited with ExitStatus 15 
    [mysupervisor] bar exited
    [service] foo :: Exit 
    [bash] Stopping proccess: 8618 
    [service] foo :: Exited with ExitStatus 15 
    [mysupervisor] foo exited
