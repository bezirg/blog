---
title: Building a PDES in Erlang(Part 1)
author: Nikolaos Bezirgiannis
tags: simulation, PDES, erlang
---

## Introduction

I'll go ahead and implement a parallel and distributed discrete-event
simulation (PDES) in the Erlang language. But first, let me
introduce you and give you some notes on the theory of simulation.

*Simulation* is the process of 'imitating' the behaviour of a particular system. To
do that, we first construct an abstract *model* of the system.
Why we do that, you say? Well, in certain circumstances measuring the performance of
an actual system is extremely difficult to do; reasons can be the costs for building such a system or the time
required to execute it. These systems can be too complex to compute an analytical solution for.
Instead, what we do, is simulate the system by estimating the performance of its constructed model.

A *model* of a system contains the algorithms and/or equations that best describe the system's behaviour.
We don't need to construct an elaborate specification of the system, because we risk overcomplicating
the problem and thus making it intractable. We only require a 'simplified' version of the system. On
the other hand, we must be careful not to oversimplify it, 
as we might lose in the detail of the model's outcome.

There are many types of simulations: pen&paper vs computer simulation, stochastic vs deterministic,
steady-state vs dynamic, continuous vs discrete, local vs distributed. Not to stress you with
extra terms, here, I will only consider computer distributed discrete-event simulations.
As you probably guessed it, we are going to run these simulations using computers. That is,
we will write a model using a computer language and execute it on a machine. The
simulations are going to be distributed; that means, we will comprise a bunch of
multiple processing units (being distributed over a network or using SMP technology). Each processing unit,
called the processor or *logical process*, will itself be an instance of a *simulation engine*.
The simulation engine's role is to receive events in discrete time, process them in timestamp order,
and schedule new events in the future to itself or other logical processes (for example by sending an event message
to a remote simulation engine). The simulation engine stops when there are no more events to process,
or a certain condition is met.

Why did I choose Erlang to implement such a simulation? Well, firstly Erlang is a functional language
and I, in general, like functional languages :). Secondly, the difficult part of implementing a simulation engine
is not how to process the incoming events (they are just linked to arbitrary code which gets executed by the engine),
but more how to easily communicate between logical processes (sending and receiving events). This can
be easily done in Erlang, since *message-passing* is a first-class citizen of the language. Events
are simply modelled as Erlang messages, and logical processes, likewise, are implemented as Erlang processes.
Another reason is that, in Erlang, running on many processors (SMP) or on multiple distributed machines is
transparent, that is we do not have to write extra code to handle these distinct cases.

When talking about PDES we have to be clear about what synchronization approach we make use of. It is the
case that, in parallel and distributed simulations certain conflicts will arise that must be resolved.
I'm not getting into much detail on this, I have to better direct you to the excellent book on PDES [^1].
There are three different synchronization approaches: the conservative approach, that avoids conflicts at all costs,
the optimistic that allows conflicts to happen but later has to go back and correct them, and the mixed approach
that employs the conservative on some and optimistic on other logical processes. In my implementation, I will
use a conservative non-zero-lookahead mechanism, influenced by the Chandy/Misra/Bryant null message protocol algorithm [^2].

Conservative mechanisms are easier to implement by the simulation developer, but require extra (lookahead) information
from the end-user; optimistic mechanisms on the other hand don't require such information by the end-user,
but are much more difficult to implement. We can say that, in most cases, an optimistic approach is faster in execution time
than a conservative simulation. Here, however, for the sake of simplicity, I demonstrate a conservative mechanism written in Erlang.

[^1]: Fujimoto, R. M. “Parallel Simulation: Parallel and Distributed Simulation Systems.” In Proceedings of the 33nd Conference on Winter Simulation, 147–157, 2001. http://dl.acm.org.proxy.library.uu.nl/citation.cfm?id=564124.564145.
[^2]: Misra, J. “Distributed Discrete-event Simulation.” ACM Computing Surveys (CSUR) 18, no. 1 (1986): 39–65.


## Related Work

**ErlangTW** is another similar simulation middleware written in Erlang, although it instead follows an optimistic approach. You
can find more information on their recently published [paper](http://arxiv.org/abs/1206.2775). Their
implementation is hosted on [GitHub](https://github.com/lucatoscano/ErlangTW), and I have to say that they provide a clean and easy-to-grasp codebase.

**μsik**[^3] is a classic simulation microkernel written in C++. The advantage of μsik is that it can dynamically alter
the deployed synchronization mechanism of the simulation to conservative, optimistic or mixed. It looks like
that the microkernel and its kernel processes emulate how an Erlang VM actually works and communicates with other machines.

[^3]: Perumalla, K.S. “Mu;sik - a Micro-kernel for Parallel/distributed Simulation Systems.” In Workshop on Principles of Advanced and Distributed Simulation, 2005. PADS 2005, 59 – 68, 2005. doi:10.1109/PADS.2005.1.



## How it is going to look like

In action, the simulation program will be comprised of two entities: the simulation application and the simulation engine.
The simulation application has the model specification in it, not mathematically defined, but rather through a computer language.  
The simulation engine is also written in a programming language (since it has to be executed); it takes the
the simulation application as input and runs it accordingly. The two entities can, but don't have to, be hosted
on the same programming language. In this case, I'm choosing Erlang for both the simulation application and the simulation engine.

What follows is an example definition of a simulation application. It is an Erlang module, that follows the OTP principles and
thus is defined as a `sim_proc` behaviour (more about this later, in a different post). What the application is responsible for
is the definition of state (the state record), simulation initialization (the `init` function), a series of
callbacks (`handle_event` function) and simulation termination cleanup (the `terminate` function). 
The event callbacks are simply associating a possible incoming event to specific code that should be executed. In this
example, the simulation application is modelling an airport, which schedules arrival, landed and departure events.
I am not going into detail here; more on this in the followup, where I'm  going to talk about the [structure
and the building blocks of a simulation application](/posts/2013-03-06-building-a-pdes-in-erlang-part2.html).


~~~ {.erlang}

-module(abd).
-behaviour(sim_proc).
-compile(export_all).


%% constants
-define(R, 10).
-define(G, 5).

%% state variables
-record(state, {in_the_air,
                on_the_ground,
                runway_free}).

init(_Args) ->
    %% initialize state_variables
    State = #state{in_the_air = 0, 
                   on_the_ground = 0, 
                   runway_free = true},

    %% schedule initial events
    sim_proc:schedule(arrival, 30),
    sim_proc:schedule(arrival, 10),

    %% create links
    sim_proc:link_from(ord),

    %% set correct lookahead
    sim_proc:link_to(lax, 3),

    {ok, State, 70}.


handle_event(arrival, State) ->
    sim_proc:println("Arrived"),
    In_the_air_ = State#state.in_the_air + 1,
    
    Runway_free_ = case State#state.runway_free of
                       true -> sim_proc:schedule(landed, ?R),
                               false;
                       false -> false
                   end,
    {ok, State#state{in_the_air = In_the_air_, runway_free = Runway_free_}};

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

handle_event(departure, State) ->
    sim_proc:println("Departed"),
    On_the_ground_ = State#state.on_the_ground - 1,
    sim_proc:schedule(lax, arrival, 5),

    {ok, State#state{on_the_ground = On_the_ground_}};

terminate(normal, _State) ->
    sim_proc:println("Finished simulation");
    
terminate(timeout, _State) ->
    sim_proc:println("Timeout reached").

~~~
