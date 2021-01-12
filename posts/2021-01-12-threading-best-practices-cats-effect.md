---
title: Threading best practices in Cats Effect
---

I frequently get asked what the best way to manage threadpools in Cats Effect is
so this is my attempt to write a definitive explanation that I can point to. My intention
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
when we evaluate the real `IO` via one of the `unsafeRunX` methods or as part of an `IOApp`. 

### Fibers

Of course we tend to have many logical threads of execution in our applications.
Cats effect trivially supports this via lightweight `Fiber`s, each of which is an instance
of the `IO` runloop. These can be created via `IO#start`, as well as various
combinators like `IO#race`. It is important to note that this an implementation of
[cooperative
multi-tasking](https://en.wikipedia.org/wiki/Cooperative_multitasking) (as
opposed to pre-emptive). In practice this means that it is actually possible for
a fiber to take control of a CPU core and never give it back if it executes a
tight CPU-bound loop like
```scala
def factorial(n: BigInt): IO[Int] = n match {
  case 0 => IO.pure(1)
  case n => factorial(n-1).flatMap {
    m => m * n
  }
}

factorial(10000)
```

If you have such a loop then you can insert a fairness boundary via `IO.shift` (CE2 but has other
potential side-effects) or `IO.cede` (CE3).

Note that the runloop-per-fiber model means that we obtain maximum performance
when all of our CPU threads are free to evaluate this runloop for one of our
`IO` fibers. 

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
for arbtirary conditions to be satisifed. Now it's time to see the details of how we achieve
this in cats effect 2 and 3.

## Cats Effect 2

CE2 provides a fixed execution context sized to the number of available cores for us
to use for compute-bound work. Several abstractions are provided to facilitate
shifting work to other pools.

### Context shift

`ContextShift` is a pure representation of an execution context and looks a bit like this:
```scala
trait ContextShift[F[_]] {

  def shift: F[Unit]

  def evalOn[A](ec: ExecutionContext)(fa: F[A]): F[A] //Good

}
```

Assume that an instance of this will be backed by some thread pool. `IOApp` provides an instance
which is backed by the compute pool it provides.

`evalOn` allows us to shift an operation onto another pool and have the continuation be
automatically shifted back eg
```scala
CS.shift(blockingPool)(
    IO(println("I run on the blocking pool"))
  ) >> IO(println("I run on the pool that backs CS"))`
```

`shift` is a uni-directional shift of thread pool so that the continuation runs on the pool that
backs the `ContextShift`
```scala
IO(println("I run on some pool")) >> CS.shift >> IO(println("I run on the pool that backs CS"))
```

### Blocker

`Blocker` was introduced to provide an abstraction for our unbounded pool for blocking operations.
It relies upon `ContextShift` for its actual behaviour and is simply a marker for a threadpool
that is suitable for blocking operations.

```scala
trait Blocker {
  def blockOn[F[_], A](fa: F[A])(implicit cs: ContextShift[F]): F[A]
}
```

`blockOn` behaves exactly like `ContextShift#blockOn` - the provided `fa` will be run on the
blocker's pool and then the continuation will run on the pool that backs `cs`

### Local reasoning

Unfortunately there are some problems with these abstractions - we lose the ability to reason
locally about what thread pool effects are running on.

```scala
def prog(inner: IO[Unit]): IO[Unit] =
  for {
    _ <- IO(println("Running on the default pool"))
    _ <- inner
    _ <- IO(println("Uh oh! Where is this running?"))
  } yield ()
```

The problem is that `inner` could be something like `randomCS.shift` in which case the
second print will be run on whatever thread pool backs `randomCS`

In fact, `shift` is _never_ safe for this reason and `evalOn` is only safe if it
returns execution to the previous thread pool, rather than an arbitrary jump to
the threadpool that backs whatever implicit `ContextShift` was in scope.

What we need is the ability to locally change the threadpool with
the guarantee that the continuation will be shifted to the previous
pool aferwards. If you are familiar with `MonadReader`
```scala
trait MonadReader[F[_], R] {
  def ask: F[R_] //get the current execution context
  
  def local[A](alter: R => R)(inner: F[A]): F[A] //run an inner effect with a different execution 
                                                 //context and then restore the previous
                                                 //execution context
}
```
then you might see that this has exactly the semantics we need, where
`local` is like `evalOn` in allowing us to locally change the
execution context, but it will be restored to the previous value afterwards.

### Auto-yielding

Auto-yielding is not supported in CE2 as yielding requires re-enqueuing the fiber
on a global queue and waiting for it to be re-scheduled. This is too expensive
to be inserted automatically. If you have a tight CPU-bound loop then you should
insert `IO.shift` where appropriate whilst ensuring that the implicit `ContextShift`
is the one that represents the compute pool.

### Obtaining a handle to the compute pool

Another unfortunate wart is that it is very difficult to obtain a handle to `IOApp's`
compute pool. This can be worked around with `IOApp.WithContext` but it is somewhat
clunky, especially if you want to instantiate the same threadpool as `IOApp` would
otherwise instantiate.

```scala
object Main extends IOApp.WithContext {

  override protected def executionContextResource: Resource[SyncIO, ExecutionContext] =
    instantiateSomeCustomThreadpoolHere()

  override def run(args: List[String]): IO[ExitCode] = {
    val computeEC = executionContext
    program(computeEC)
  }
}
```

## CE3

The good news is that Cats effect 3 fixes these things and makes other things nicer as well! :)
Notably, `ContextShift` and `Blocker` are no more.

### Spawn

CE3 introduces a re-designed typeclass `Async`

```scala
trait Async[F[_]] {
  def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

  def executionContext: F[ExecutionContext]
}
```
which has exactly the `MonadReader` semantics we discussed above. Note that the
execution shifts back to the threadpool defined by Async#`executionContext`, rather
than a non-local jump to the threadpool belonging to some implicit `ContextShift`.

Also note that `Async[IO].executionContext` in `IOApp` will give us a handle to the
compute pool without the `WithContext` machinery.

### Blocking

CE3 has a builtin `blocking` which will shift execution to an internal
blocking threadpool and shift it back afterwards using `Async`.

This means that we can simply write
```scala
IO.println("current pool") >> IO.blocking(println("blocking pool")) >> IO.println("current pool")
```

There is also an analogous operation `interruptible` which shifts to the blocking pool
but will attempt to cancel the operation using `Thread#interrupt()` in the event that
the fiber is canceled.

### Work-stealing pool

CE3 also has a very exciting custom work-stealing threadpool implementation. This has
numerous benefits over the `FixedThreadpool` used in CE2:
- It maintains a work queue per core rather than a single global one so contention
  is dramatically reduced, especially with lots of cores
- This means that we can implement thread affinity, where a fiber that yields is most
  likely to be re-scheduled on the same thread. This makes yielding much cheaper
  as if the fiber is immediately re-scheduled we don't even have to flush CPU caches
- Consequently we can support auto-yielding where a fiber will insert an `IO.cede`
  every fixed number of iterations of the runloop
