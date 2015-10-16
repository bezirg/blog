---
title: Benchmarking Haskell parsers
author: Nikolaos Bezirgiannis
tags: haskell, parsing
---

Last year, for the project assignment of the Advanced Functional Programming course,
here at Utrecht, I decided to benchmark the most widely used parsing combinator
libraries that exist in Haskell. I came up with rudimentary examples to compare the time and space of the following Haskell
libraries: [parsec2](http://hackage.haskell.org/package/parsec2), [parsec3](http://hackage.haskell.org/package/parsec3), [uulib](http://hackage.haskell.org/package/uulib), [uu-parsinglib](http://hackage.haskell.org/package/uu-parsinglib), [attoparsec](http://hackage.haskell.org/package/attoparsec) and [polyparse](http://hackage.haskell.org/package/polyparse). The comparison makes use of the amazing [Criterion](http://hackage.haskell.org/package/criterion) benchmarking library.

I have put the talk I gave recently online [here](http://bezirg.net/afp_presentation.pdf), so people 
that are indecisive on which parsing combinator library they should use, can make a better judgement!

