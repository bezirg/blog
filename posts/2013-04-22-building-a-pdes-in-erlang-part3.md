---
title: Building a PDES in Erlang(Part 3)
author: Nikolaos Bezirgiannis
tags: simulation, PDES, erlang
---

In the [previous section](/posts/2013-03-06-building-a-pdes-in-erlang-part2.html), we saw
how the simulation application can be structured, together with an example, 
coded in our experimental Erlang PDES framework.
This time, we'll take a look where the heart of this simulation framework beats --- called
also the simulation executive or simulation engine --- and particularly how its internals are defined.


## OTP Behaviour guidelines

As we discussed earlier, OTP behaviours are similar to interfaces for the Erlang language; the user 
has to fill in code for a collection of callbacks.
Instances of these behaviours provide implementation to these callbacks, gathered in a single Erlang module.

Besides the standard behaviours that Erlang/OTP comes bundled with, the user can write 
custom behaviours and distribute them together with usual Erlang code.

There are currently two ways (the old and the new method) to define OTP behaviours, based on the 
particular Erlang/OTP version you are using. There is no backwards compatibility, so users
of newer Erlang versions (`OTP>=R15B`) should stick with the new method.


### The old method in `OTP<R15B`

The old method was rather crude and did not state explicitly that the module written is actually an OTP behaviour;
rather it relied on a 'magic' function, the `behaviour_info`, that should be exported by the behaviour module. This should be better illustrated with an example
taken from the actual old implementation of the `gen_server` behaviour:

~~~ {.erlang}
-module(gen_server).

%% Usual API export
-export([start/3, start/4,
	 start_link/3, start_link/4,
	 call/2, call/3,
	 cast/2, reply/2,
	 abcast/2, abcast/3,
	 multi_call/2, multi_call/3, multi_call/4,
	 enter_loop/3, enter_loop/4, enter_loop/5]).


%% The involving 'magic' function 
-export([behaviour_info/1]).

%% and its definition

behaviour_info(callbacks) ->
    [{init,1},{handle_call,3},{handle_cast,2},{handle_info,2},
     {terminate,2},{code_change,3}];
behaviour_info(_Other) ->
    undefined.
~~~

As it is obvious, the `behaviour_info` function simply returns a list of callback functions, together with their arity,
that the user of the behaviour has to implement in his/her behaviour instance.

### The new method in `OTP>=R15B`

The new way to write custom behaviours relies on a brand-new compiler directive, intuitively called  `-callback`. The user has to insert a single callback directive for every callback belonging to this behaviour.
The example show how the method is applied for the `sim_proc` behaviour:

~~~ {.erlang}

%% Usual API
-export([start/3, start/4,
         start_link/3, start_link/4,
         format_status/2,
         schedule/2, schedule/3, 
         clock/0,
         print/1, print/2, 
         println/1, println/2,
         link_from/1, link_to/2
        ]).

%%  Types and Callbacks
%% 
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout()} |
    {stop, Reason :: term()} | ignore.
-callback handle_event(Event :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
~~~


The `-callback` directive resembles that of `-spec` directive, because
it also requires each function to be accompanied by an appropriate
type signature. Of course, there is no type checking involved,
but in this way, the documentation generated from these custom behaviours becomes
much more concrete, since we know what these callbacks accept
as arguments and what are their return values. You can check
for example the [documentation](http://www.erlang.org/doc/man/gen_server.html) (erldoc) output for the standard `gen_server` behaviour.

## Other exports

Besides specifying the callbacks, the custom behaviour must export
any API functions that can be called by the behaviour instance and
a bunch of system functions that generally apply to behaviours. The approach
is the same when using both methods for specifying behaviours.
For example, the exported functions taken from the `sim_proc` behaviour are:

~~~ {.erlang}

%% API exports
-export([start/3, start/4,
         start_link/3, start_link/4,
         format_status/2,
         schedule/2, schedule/3, 
         clock/0,
         print/1, print/2, 
         println/1, println/2,
         link_from/1, link_to/2
        ]).


%% System exports
-export([
         init_it/6,
         system_continue/3,
         system_terminate/4,
         system_code_change/4
         ]).
~~~


The generic exported functions that are applied to almost any behaviour are:

Exported function               Role
----------------------------    ----
`start`                         internal behaviour callback
`start_link`                    like start but also linking
`format_status`                 calls Mod:format_status, prints the status of process
`init_it`                       System export that calls `Mod:init` and then enters the loop of the process `Behaviour:loop`
`system_continue`               Callback function for system messages
`system_terminate`              Callback function for system messages
`system_code_change`            Callback function for system messages

## `sim_proc` behaviour

The sim_proc behaviour is actually the simulation executive of the system. It exports
fundamental API functions that the simulation application of the user can call:

Exported function               Role
----------------------------    ----
`schedule(Event, Time)`         schedules a new local Event with timestamp Time
`schedule(LP, Event, Time)`     schedules a new remote event to the logical process LP
`clock()`                       Returns the current clock of the calling Logical Process
`print()`                       Wrapper to print
`println()`                     Utility function, wrapper to print
`link_from(LP)`                 Creates an incoming link from Logical Process LP
`link_to(LP, Lookahead)`        Creates an outgoing link to Logical Process with the value of the lookahead passed to the function


The logical process instance  is responsible for handling the local event list 
and any remote event queues. The process loop running follows these execution steps:

1) looks for the smallest timestamp from the current events
2) process the event by calling proper callbacks from the behaviour instance module
3) advances the current clock to the processed event
4) sends null messages to its 'neighbours' (outgoing-linked Logical Processes)
5) go back to 1, or finish if there are no events to process or the simulation-end-time has been reached.

## `sim_cont` behaviour

The sim_cont behaviour is an extra controller behaviour that is simply defined for convenience. It has a declarative style
and simply exports a single callback `init`. The user has to fill in the function `init`, so as to inform the simulation executive
which are the modules that the models reside in and 
which and how many are the participating Logical Processes, spawning an Erlang behaviour process for each.

The behaviour instance of `sim_cont` should be bundled together with the simulation application, because it is part
of the model definition. An example behaviour instance, called `sim_controller` can be specified as:

~~~ {.erlang}
-module(sim_controller).
-behaviour(sim_cont).
-export([start_link/0, init/1]).

start_link() ->
    sim_cont:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, [
          %% { LocalOrRemote, Name, Module}
          {local, lax, lax},
          {local, ord, ord},
          {local, abd, abd}
         ]
    }.
~~~
     
The above controller says that there are 3 Logical Process sitting in the local network, named lax, ord and abd.
Their model definitions are in the modules lax.erl , ord.erl and abd.erl respectively. By calling
`sim_controller:start_link()`, a single behaviour process will be spawned for each declared Logical Process.
