---
title: Enhancement to the Strathclyde Haskell Enhancement (SHE)
author: Nikolaos Bezirgiannis
tags: haskell, parsing 
---

Right now that I am building my Parallel Discrete-Event Simulation framework
in the Haskell language (more about this in a forthcoming post), 
I stumbled upon a case where I was writing a lot of boilerplate code.
Consider a `State` record that holds the state variables of each
logical process (LP) of the simulation environment. 

~~~ {.haskell}
data State = State {
    _runway_isfree :: Bool
  , _on_the_ground :: Int
  , _in_the_air :: Int
  , _total_flies :: Int
  , _seed :: Int
  } deriving (Eq, Show)
~~~

This State is updated whenever a new event is handled by each LP.

Imagine I would like to collect statistics for the state
variables; observe every change of a state variable
on each event, and build up a value from the observations,
for example, sum the up. 

That's where the `Monoid` class comes to play, to provide us with a unit element
and an associative binary operation. I came up with
three such operations that return me a Monoid, summation,
accumulation and just. The just operation, would just
ignore intermediate changes to state variables and just :)
return me the last observation of the variable. 


~~~ {.haskell}
summ :: (Num a, Show a) => (state -> a) -> ... -> (Sum a)

accu :: (Real a) => (state -> a)  -> ... -> (Sum Double)

just :: (Show a) => (state -> a) -> ... -> (Last a)
~~~


With these at hand, I could write a statistics specification as:

~~~ {.haskell}
stats = just _total_flies
~~~

What if I want more statistics collected? We could
easily bundle them up in a tuple, using the Applicative
style.


~~~ {.haskell}
{-# LANGUAGE TupleSections #-}

stats = (,,,) <$> just _total_flies
              <*> summ _in_the_air
              <*> summ _in_the_ground
              <*> accu _in_the_ground
~~~

As you can guess, if we have many variables to bundle up,
it gets kind of cumbersome to write the correct number of commas
in front followed by the dollars and asterisks.

Thankfully, I remembered about my professor Doaitse Swierstra
"preaching" about the idiomatic brackets notation and the great
functional pearl paper [^1].

[^1]: McBride, Conor, and Ross Paterson. “Functional Pearl: Applicative Programming with Effects.” Journal of Functional Programming 18, no. 1 (2008): 1–13.

There is an actual implementation of this idea in the [Strathclyde Haskell Enchancement](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/idiom.html)(SHE).

As the link tells us, SHE preprocesses code from:

~~~ {.haskell}
(|f a1 .. an|) 
~~~

to:

~~~ {.haskell}
pure f <*> a1 <*> .. <*> an 
~~~

, which turns out to be quite convenient.

SHE supports also binary lifting to applicative, so users can write

~~~ {.haskell}
(| a1 + a2 |)
~~~

and be translated to this code:

~~~ {.haskell}
pure (+) <*> a1 <*> a2
~~~

but that's up to it. I came up with an enhancement
to this notation, so we can easily deal with tuples and
the applicative style. It goes like this:

Somebody writes :

~~~ {.haskell}
(| a1, a2, a3 |)
~~~

and is translated to:

~~~ {.haskell}
(| pure (,,) <*> a1 <*> a2 <*> a3|)
~~~

I've modified SHE's source code to support this extension
and fixed up a bug to make SHE work with the latest GHC 7.6.
You can fetch my version of SHE by

~~~
git clone git://bezirg.net/she.git
~~~

Now I can write my statistics specification as:

~~~ {.haskell}
stats = (| just _total_flies,
           summ _in_the_air,
           summ _in_the_ground,
           accu _in_the_ground
         |)
~~~

   
and be done with it! I also thought of another extension
where the user would write:

~~~ {.haskell}
[| a1, a2 , a3 |]
~~~

and gets translated to 

~~~ {.haskell}
pure (\ x y z -> x:y:z:[]) <*> a1 <*> a2 <*> a3
~~~

but I haven't got to implement this yet.
I am looking forward to any suggestions!
