---
title: Threading best practices in Cats Effect
---

I frequently get asked what the best way to manage threadpools in Cats Effect is
so this is my attempt to write a definitive answer that I can point to. My intention
is to cover both Cats Effect 2 and Cats Effect 3, although at the time of writing
the latter is at milestone 5 so some details are subject to change. I'll endeavour to
update this should that happen.

## High-level goals

The high-level goals of threading are covered in detail by [Daniel's gist](https://gist.github.com/djspiewak/46b543800958cf61af6efa8e072bfd5c) so I'll
just give the executive summary. We are aiming for:
- A single thread pool of roughly the number of available processors for compute-based operations
- An unbounded, cached threadpool for blocking operations
- 1 or 2 high-priority threads for handling asynchronous I/O events, the handling of which should
  immediately be shifted to the compute pool
  
The goal of this is to minimize the number of expensive thread context shifts and to maximize the
amount of time that our compute pool is doing useful work.

It is also worth noting that `scala.concurrent.ExecutionContext.global` is a poor choice for your
compute pool as its design assumes that there will be blocking operations performed on it and
hence it allocates more threads. In addition, there is no way to stop libraries on your
classpath from scheduling arbitrary code on it so it is a very unreliable basis for your
compute pool.

## The IO runloop

A simplified `IO` might look something like this:
```scala
sealed abstract class IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  
  def unsafeRun(): A = this match {
    case Pure(a) => a
    case Suspend(thunk) => thunk()
    case FlatMap(io, f) => f(io.unsafeRun()).unsafeRun()
  }
}

case class Pure[A](a: A) extends IO[A]
case class Suspend[A](thunk: () => A) extends IO[A]
case class FlatMap[A, B](io: IO[B], f: B => IO[A]) extends IO[A]
```

Of course this has no error handling, isn't stacksafe, doesn't support asynchronous effects, etc
but it's close enough for illustrative purposes. The key thing to note is that `unsafeRun`
is a tightly CPU-bound loop evaluating different layers of `IO`. The situation is just the same
when we evaluate the real `IO` via one of the `unsafeRunX` methods or as part of an `IOApp`. Note
as well that there will be one of these runloops for every `IO` fiber that we want to run.
This means that we obtain maximum performance when all of our CPU threads are free to evaluate
this runloop for one of our `IO` fibers. 

### Thread blocking

A direct consequence of the above is that running blocking code on our compute
pool is _very_ bad. If we're running on a node with 2 CPUs and we evaluate a
blocking call like `IO(Source.fromFile(path).getLines())` then for the duration
of that operation our capacity to evaluate `IO` fibers is _halved_. Run two such
operations at the same time and your application effectively stops until one of
those blocking calls completes.

The solution to this is to shift the execution of the blocking operation to our unbounded, cached
threadpool and then shift computation back to the compute pool once the blocking call has completed.
We'll see code samples for this later as it is quite different between CE2 and CE3.

### Semantic blocking

Of course, we do also need the ability to tell fibers to wait for conditions to
be fulfilled. If we can't call thread blocking operations (eg Java/Scala builtin
locks, semaphores, etc) then what can we do?  At this point we need to insist on a
distinction between thread blocking operations (synchronous I/O, Java locks,
semaphores, etc as above) and _semantic_ blocking, where a fiber yields control
of the thread it was running on and indicates that it should wait for a
condition to be satisfied.

Cats effect provides various APIs which have these semantics, such as `IO.sleep(duration)`.
Indeed this is why you must never call `IO(Thread.sleep(duration))` instead, as this is a
thread blocking operation, where as `IO.sleep` is only semantically blocking.

The building block for arbitrary semantic blocking is `Deferred`, which is a purely functional
promise that can only be completed once
```scala
trait Deferred[F[_], A] {
  def get: F[A]
  
  def complete(a: A): F[Unit]
}
```
`Deferred#get` is semantically blocking until `Deferred#complete` is called and cats effect
provides many more semantically blocking abstractions like semaphores that are built on top of
this.

## Summary (Part 1)

So we've seen that best performance is achieved when we dedicate use of the compute pool
to evaluating `IO` fiber runloops and ensure that we shift _all_ blocking operations
to a separate blocking threadpool. We've also seen that many things do not need to
block a thread at all - cats effect provides semantic blocking abstractions for waiting
for arbtirary conditions to be satisifed. Now it's time to see the details of how we achive
this in cats effect 2 and 3.

explain fibers?
## Semantic blocking

## Local reasoning

## CE2

context shift, blocker

## CE3

blocking, interruptible, evalOn
