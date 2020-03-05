---
title: FP: A lingua franca for software
---

A lot has (quite rightly!) been written on the benefits of functional programming.
Most of this focusses on the following pillars of FP:
1. Referential transparency
2. Strong type systems improving safety and documenting behaviour
3. Type classes offering a sane and flexible approach to polymorphism

However, I think there is another huge advantage to FP which gets
comparatively little airtime - in FP we have a lingua franca for
modelling and discussing software. That is the language of maths
(specifically category theory)!

### Confessions of a Java developer

I was a Java developer for several years and during that period
I would estimate I spent approximately 57.3% of time on Stack
Overflow asking questions of the form "how do I achieve X using
library Y?" Since starting to write Haskell/FP Scala I've noticed that percentage
has fallen to approximately 0% and I don't think this is a coincidence.

Java lacks sufficiently powerful abstractions such as `Functor` or
`Monad` and consequently every Java library is forced to re-invent the
wheel and design it's own API. Just look at how many Java `Future`
implementations there are with entirely incompatible APIs. Scala
has a similar proliferation of `Future`-like implementations but
they all expose a nigh-on identical API.

### A language which spans libraries and languages

Category thoretical abstractions are often criticised for raising the bar for
entry to FP languages (and some of the naming is admittedly obscure at best) but
this is a one-off cost to be able to understand the vast majority of libraries
in existence.

I frequently find that I don't even need to read the documentation of a new
Haskell library (should that exist in the first place!) - the combination of the
types and the re-use of familiar abstractions is sufficient for me to start
using it.

And this benefit is not even restricted to a single language. You can get a
Haskell developer, a Purescript developer, an Idris developer and a Scala
developer in the same room and they will be able to discuss software at the
implementation level in a common language in which they are all fluent.

### Scala controversy

Scala's ZIO library [recently deprecated](https://github.com/zio/zio/issues/2649)
the `traverse` method in favour of the synonym `foreach`. There were many
admirable intentions behind this, centred around making the library more
accessible for newcomers (`traverse` isn't the _most_ descriptive name and
doesn't have the same canonical mathematical legacy as something like `Functor`).
However, most of the arguments against seemed to focus on the irritation
to experienced developers who will know to look for `traverse`, rather
than thinking what we will lose by abandoning a well-established naming
convention that facilitates discussion between programmers from multiple
different languages.

Whilst this is a small, isolated incident that I'm sure people will learn to
deal with, to me it sets a dangerous precendent. A common language for modelling
software is an amazing thing and one that functional programmers should be loath
to throw away. IMHO it ranks almost as highly as referential transparency in
terms of improvement in the quality of life of a programmer - the bar to entry
for new libraries and languages is so vastly lower than in any other ecosystem.
