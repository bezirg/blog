---
title: Building a PDES in Erlang(Part 2)
author: Nikolaos Bezirgiannis
tags: simulation, PDES, erlang
---

## The application

This is the follow-up to [the introduction of Building a PDES in Erlang](/posts/2013-01-26-building-a-pdes-in-erlang-part1.html) series.
Here, I am going to show you how a simulation application (aka simulation model) is written and structured using this proposed experimental Erlang framework.
Worth noting is the difference between the simulation application and the rest part of the framework, the simulation engine. We could say that the simulation engine
is the heart of the simulation, where events are scheduled and executed in order. The simulation application is merely user code, linked
to events, which is passed to the simulation engine for execution. This code, as you can guess, must be specified in the Erlang language.

The model is comprised of multiple Erlang modules. This is in contrast to what is the common case in a sequential simulation,
where the code of the model is accumulated in a single file location. The reason for choosing a multi-module approach is
that the application stays as close as possible to the Erlang style guideline. In our framework, 
each Erlang application module corresponds to an individual Logical Process (LP). When each LP is instantiated (spawned in Erlang terms),
it will be assigned its own simulation engine and event code to be executed. That means that every LP will have a distinct time clock
and event queue.


## OTP

In our framework we try as much as hard to follow the OTP principles. OTP stands for *Open Telecom Platform* and happens to be a collection
of standards and best practices, created by the Erlang people for the Erlang community. In this sense,
the simulation application is bundled as a proper OTP application, and accordingly its modules are written as OTP behaviours.
If you come from an object-oriented background, you can think of the OTP behaviours as an abstract interface or a partially implemented
class, where you "fill the left out parts" with your implementation. 
When the OTP behaviour is instantiated, it will sit as an Erlang process and wait for certain events to happen or messages to be sent to it (*gen_server*, *gen_fsm*, *gen_event*, *supervisor* behaviours).
In this way, users can create rich interactive and reactive application processes.
Besides the behaviours that the OTP includes in its library (already mentioned above),
a user can create its own custom behaviours; and that is what we do in our simulation framework. Erlang modules
that contain the directive

~~~ {.erlang }
-module(a_module).
-behaviour(sim_proc).
~~~

are recognized as our custom-OTP simulation behaviour modules. When they are instantiated, each will spawn a single simulation process (LP), hence the name `sim_proc`.
We could say that we in fact exploit (or alternatively mistreat?), the OTP behaviour mechanisms, since our custom behaviour is not a reactive nor an interactive Erlang process.
The trick is to simply use the patterns defined by OTP, as specifications of a simulation API.

## An example of a simulation application module

For our example, we our modelling a network of airports, following the same example of Fujimoto
in his book [^1]. Each LP will be an individual airport, each with a single
runway, where aircraft land to and depart from.

[^1]: Fujimoto, R. M. “Parallel Simulation: Parallel and Distributed Simulation Systems.” In Proceedings of the 33nd Conference on Winter Simulation, 147–157, 2001. http://dl.acm.org.proxy.library.uu.nl/citation.cfm?id=564124.564145.

We first start the module file as usual, by defining the following directives:

~~~ {.erlang}
-module(abd).
-behaviour(sim_proc).
-compile(export_all).
~~~

The above mean that we specify a Logical Process (that is, airport in our case), named abd, and we want its whole API to be exported (`-compile(export_all` directive).

Next, we declare our constants for our program simulation. We have *R*, which
determines the time the runway is in use for an airplane to land. The constant *G*
specifies the time the aircraft after landing, stays in the ground and
travels to the gate for its next departure.

~~~ {.erlang}
%% constants
-define(R, 10). % time runway in use to land aircraft
-define(G, 5). % time required at gate
~~~

After that, we define a record that holds the state of the LP. The
LP state is also influenced by the performance measures we set up in our simulation study.
In this case, we have to declare 3 distinct state variables, the 1st to count
the planes that are in the air, the 2nd for the number of planes that have landed
and finally a boolean value to state if the runway is currently free:

~~~ {.erlang}
%% state variables
-record(state, {in_the_air,
                on_the_ground,
                runway_free}).
~~~

What follows, is the `init` function that will initialize our LP state,
schedule initial events to the simulation engine and ultimately
declare what are the incoming and outgoing connections (links) to other airports.
Consider that, for the outgoing links, we have to provide also a non-zero lookahead value.

~~~ {.erlang}
init(_Args) ->
    %% initialize state_variables
    State = #state{in_the_air = 0, 
                   on_the_ground = 0, 
                   runway_free = true},

    %% schedule initial event
    sim_proc:schedule(arrival, 30),
    sim_proc:schedule(arrival, 10),

    %% incoming links
    sim_proc:link_from(ord),

    %% outgoing links with lookahead
    sim_proc:link_to(lax, 3),

    {ok, State, 40}.
~~~

The code is self-explanatory. We initially have 0 planes in the air,
0 on the ground and the runway is not currently in use. Next,
we schedule two arrival events on time 30 and 10 respectively.
We have an incoming link with the ord airport; that means that we are expecting
incoming flights that start from ord and reach our destination. There is
also an outgoing link to the lax airport, with the lookahead value being 3 units of time.
This means that departures scheduled from the abd airport, to arrive to the lax airport
will take at least 3 units of time to reach the lax airport. So, in this case,
the lookahead can be determined by the distance between two airports.

The final line in the `init` function, `{ok, State, 40}`, simply returns
the state and specifies an endpoint for the simulation of the abd airport;
so at time 40 we are stopping the simulation of the abd LP and we are not
taking into account any events after the end time.

We continue to define the event handling functions. We make heavy use
of the pattern-matching capabilities of the Erlang language, to pattern-match
on the distinctive events of the LP. First, we handle the arrival
events to our abd airport:

~~~ {.erlang}
handle_event(arrival, State) ->
    sim_proc:println("Arrived"),
    In_the_air_ = State#state.in_the_air + 1,
    
    Runway_free_ = case State#state.runway_free of
                       true -> sim_proc:schedule(landed, ?R),
                               false;
                       false -> false
                   end,
    {ok, State#state{in_the_air = In_the_air_, runway_free = Runway_free_}};
~~~

We print to output and increment the `In_the_air` variable by 1. We
then check if the runway is free. If this is the case, then 
we schedule a future land event after `R` units of time. We then continue,
and return the new state.

The code for the landed event is similar:

~~~ {.erlang}
handle_event(landed, State) ->
    sim_proc:println("Landed"),
    In_the_air_ = State#state.in_the_air - 1,
    On_the_ground_ = State#state.on_the_ground + 1,
    sim_proc:schedule(departure, ?G),
    Runway_free_ = case In_the_air_ > 0 of
                       true -> sim_proc:schedule(landed, ?R),
                               false;
                       false -> true
                   end,
    {ok, State#state{in_the_air = In_the_air_, on_the_ground = On_the_ground_, runway_free = Runway_free_}};
~~~

We write to output and change the state variables accordingly. We don't forget
to schedule a departure events after `G` units of time. If there are still
aircraft circling over the airport waiting to land, we pick the next in line
and schedule it for landing. We then return the updated state.

Then we handle the departure events:

~~~ {.erlang}
handle_event(departure, State) ->
    sim_proc:println("Departed"),
    On_the_ground_ = State#state.on_the_ground - 1,
    sim_proc:schedule(lax, arrival, 5),

    {ok, State#state{on_the_ground = On_the_ground_}};
~~~

We decrement the state variable `On_the_ground` by 1 and schedule
an arrival at the remote lax airport in 5 units of time --- which is fine, since it is being larger than the 3 lookahead value
specified in `init`.


We then consider stop events, possibly generated by the local or a remote LP. We could ignore a stop event,
or do what is advisable and stop the simulation of the LP by returning `{stop, State}`, instead of an `{ok, State}`.

~~~ {.erlang}
handle_event(stop, State) ->
    {stop, State}.
~~~

When the LP stops, either running out of events (normal termination) or running out of simulation time
(timeout termination), the terminate callback function is called to handle last code
before the exit of process (similar to a destructor in the OO-world).
Here we simply announce the termination in the standard output.

~~~ {.erlang}
terminate(normal, _State) ->
    sim_proc:println("Finished simulation");
    
terminate(timeout, _State) ->
    sim_proc:println("Timeout reached").
~~~

Following closely the OTP guidelines, 
We can still consider messages sent to the Logical Process that are not
events. We can handle these `out-of-order` messages with the `handle_info` callback function, as:

~~~ {.erlang}
handle_info(_Info, State) ->
    {noreply,State}.
~~~

In this example, we simply ignore these kind of messages, but in a different situation
we might consider handling them, thus creating a reactive/interactive Erlang process.

What is left is to show the *hot-code loading* capabilities of the Erlang VM. In practice,
hot-code loading means that we could change the simulation code dynamically 
during the run-time of the simulation framework. In our case, we simply
don't react in possible code changes.
The benefits of hot-code loading for simulation purposes are not clear
and, so, are left to be explored in a later post. 

~~~ {.erlang}
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.
~~~

Ofcourse, we have to write appropriate `sim_proc` behaviour processes also
for the rest two airports that we model. But we can skip this for now, since it follows
the same principles. In the [next section](/posts/2013-04-22-building-a-pdes-in-erlang-part3.html), we are going to take a look at the internals of the simulation engine, which itself is also written in Erlang.
