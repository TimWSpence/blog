---
title: Polymorphic Effects in Scala
---

A common pattern in Typelevel scala is to write programs in terms of a
polymorphic effect type `F`:

```scala
def program[F[_] : Monad] =
  for {
    x <- Monad[F].pure(1)
    y <- Monad[F].pure(1)
  } yield x + y
```

Where the context bound on `F` lies somewhere in the `Functor` hierarchy,  from
`Functor` all the way down to `ConcurrentEffect` (in Cats Effect 2).

Whilst ubiquitous, the reasons why you might want to do this are often
misunderstood. The often cited reason is that it allows you to swap out your
effect system (Cats Effect -> Monix, Monix -> Cats Effect, etc) without
modifying your application. While this is undoubtedly true (assuming you've
stuck rigidly to the typeclass-based abstractions), I can honestly say that I've
never done it and I'd be very surprised if many people ever did.

So if that's not a justification, should we just go back to writing monomorphic
code in a concrete `IO` monad?

## Reasoning about Effects

Whilst working with a concrete effect type is undoubtedly simpler and more
beginner-friendly, the loss of information from the functor context bound means
that the function signature conveys strictly less information about the
behaviour of the program. Consider the following:

```scala
def subprogram[F[_] : Monad : HttpClient]: F[Unit] = ???
```

Here we can safely conclude _from the type signature_ that this program does not
eg modify the database.  There is simply no way for the programmer to introduce
such an effect (aside from simply by-passing the effect system entirely but
unfortunately there's not much we can do about that). Compare that with:

```scala
def subprogram: IO[Unit] = ???
```

What effects does this program perform? Unforunately, the only conclusion we can
draw from the type signature is: literally _anything_ in the world! The only way
we can know if it modifies the database is to go and read the source of the
entire transitive call-graph of `subprogram`!

## The unfortunate history of Cats Effect

Possibly part of the reason the benefits of this approach have been
misunderstood is that the design of the Cats Effect 2 typeclass hierarchy
severely limits this kind of reasoning.  The reason for this is that `Sync` and
`Async` are at the top of the CE2 hierarchy. These provide, respectively,
`delay` and `async` which are our FFI for suspending arbitrary effects in `F`.

Any time a `Sync[F]` instance is in scope we lose all ability to reason about
effects (exactly as if we were coding in plain `IO`). Consider our reasoning
about database modification from before. The only way to tell if the database is
modified is again to traverse the entire transitive call-graph of `subprogram`
looking for code such as `Sync[F].delay(someUnsafeJdbcCode())`

As `Sync` is at the top of the CE2 typeclass hierarchy that means that any time
we bring a CE2 typeclass into scope then we must necessarily introduce `delay`
into scope. Hence we have no more ability to reason about effects than if we
coded directly in `IO`, even if we just wanted `Concurrent` so that we can
spawn some fibers.

## Cats Effect 3

Fortunately this will soon be rectified with the release of cats effect 3! :)
This pushes `Sync` and `Async` to the bottom of the typeclass hierarchy so that
you can introduce other CE3 typeclasses into scope without losing the ability to
reason about effects. For example `Spawn` is a typeclass that allows you to
start/cancel/wait for fibers.

```scala
def subprogram[F[_] : HttpClient : Spawn]: F[Unit] = ???
```

We can deduce that this subprogram may make http calls and may manipulate fibers
and _nothing_ else.

## Writing your own effect typeclasses

At this point it's worth noting that the new cats effect typeclasses aren't
particularly special (other than having sets of laws that ensure that they
compose sensibly with other CE combinators).  It's entirely possible to build
your own. For example, if we decided we couldn't wait till the release of CE3 to
have access to a `Spawn` typeclass that doesn't break effectful reasoning by
introducing `delay` into scope, we could write our own! 

```scala
//Sketchy outline!
trait Spawn[F[_]] {
  def start[A](fa: F[A]): F[Fiber[F, A]]
}

object Spawn {
  implicit def spawnForConcurrent[F[_]](implicit F: Concurrent[F]): Spawn[F] =
    new Spawn[F] {
      def start[A](fa: F[A]): F[Fiber[F, A]] = F.start(fa)
  }
}
```

Similarly we could write our own `Files[F]` typeclass if our program needs to
perform file I/O, `Store[F, Foo]` if our program needs to persist `Foo`s to
a database, etc

## Thinking more about constraining effects

Hopefully that's at least convinced you that there are concrete benefits to
writing code with a polymorphic effect type. If you want to think more about
this in a slightly more abstract setting. I _highly_ recommend Runar's classic
talk [Constraints liberate, liberties
contrain](https://www.youtube.com/watch?v=GqmsQeSzMdw)

## Postscript

As an entirely subjective point of style for polymorphic effects, I find the
following to work well:

```scala
def subprogram[F[_]](implicit F: Constraint[F]) = ???
```

where `Constraint` is the (least privileged) member of the `Functor` hierarchy
that we require.  This means that in the body of `subprogram` we can write
`F.map` instead of `Functor[F].map`, which reduces syntactic noise, and means we
only have to change one word in the type signature  if we discover at a later
point that we need a more powerful constraint like `Applicative` - the body of
the function will still just say `F.xxx`.

