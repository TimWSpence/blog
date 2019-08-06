---
title: Having your cake and eating it
---

At Permutive, we’re committed to functional programming. That typically means
also committing to immutable data structures (a very good thing!), but there are
times when an algorithm would run much faster or be more space efficient if it
could update state in-place.

Consider the very useful but much-maligned nub, which removes duplicates from a
list:

```haskell
nub :: (Eq a) => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq [] =  []
nubBy eq (x:xs) = x : nubBy eq (filter (\ y -> not (eq x y)) xs)
```

This has O(n^2) time complexity due to an inner traversal of the list. But if
we’re willing to introduce an Ord a restriction, we can reduce this to
O(nlog(n)) by building up a binary tree, giving us the
[nubOrd](http://hackage.haskell.org/package/extra-1.6.14/docs/Data-List-Extra.html)
function in Data.List.Extra. Further, we could replace the Ord a restriction
with Hashable a and implement nub in O(n) average time if we could construct a
mutable hashmap.

One way to achieve this mutable state would be to store our states inside an
IORef, as this provides mutable memory protected by sequenced IO actions. It
*would* run in constant space but *isn’t* completely satisfactory, since our
types become polluted with IO. Clearly this shouldn’t be necessary: our nub
function does not want to modify any non-local state.

This reveals something about the nature of the problem. The reason we’re
required to introduce IO is because the compiler has no way to guarantee that we
won't share our mutable array with another thread, and hence to ensure
referential transparency we must work within the IO monad.

So can we construct a type which permits in-place mutation but which cannot be
shared between threads?

Yes! :)

(Many thanks to [John Launchbury and Simon Peyton Jones — Lazy Functional State Threads](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf))

## The ST Monad

Haskell defines types ST s a and STRef s a that support—amongst others—the following operations:

```haskell
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()
```

Which means that we can write (pseudo-imperative) code, like:

```haskell
increment :: STRef s Int -> ST s ()
increment ref = do
  current <- readSTRef ref
  writeSTRef ref (current + 1)
prog :: ST s Int
prog = do
  ref <- newSTRef 0
  increment ref
  increment ref
  readSTRef ref
```

Thus far, this looks quite like IORef. The difference however is that we can
safely extract a value from ST s a using:

```haskell
{-# LANGUAGE RankNTypes #-}
runST :: (forall s. ST s a) -> a
```

For example:

```haskell
runST prog :: Int
(runST prog) == 2 -- not haskell but you get the picture!
```

This looks a lot like unsafePerformIO—in fact they are closely related
internally but you need a *very* good reason to use unsafePerformIO. An STRef is
backed by a piece of mutable memory. How does runST not violate referential
transparency like unsafePerformIO does?

Let’s try to ‘leak’ the ref from an ST value to see what happens.

```haskell
runST $ newSTRef 0 -- attempt to leak the ref from a runST call
                   -- does not compile!!
```

The compiler will fail with the following error message:

    Couldn't match type ‘a’ with ‘STRef s Integer’ because type variable ‘s’ would escape its scope

Why is this? Consider the types:

```haskell
newSTRef 0 :: ST s (STRef s Int)
runST :: (forall s. ST s a) -> a
```

Suppose we could apply runST to newSTRef 0. We might try:

```haskell
runST $ newSTRef 0 :: STRef s Int
```

But this contradicts the type of runST as the return type is dependent on s,
whereas the return type of runST is independent of s

Similarly we can show that the compiler will not allow us to pass in an STRef to
be evaluated by runST:

```haskell
let v = runST (newVar True)
  in runST (readVar v)
```

If this were legal, v would have type ST s (STRef s Bool) and hence readVar v ::
ST s Bool. Again, this does not match the type (forall s. ST s a) required by
runST: s is free in the latter but not in the former.

**TL;DR **The phantom type s means the compiler will prohibit us from passing an
STRef as a parameter to runST or returning an STRef via runST. Hence we can
guarantee that the mutable state is never shared and so runST is safe, unlike
unsafePerformIO! (For a more rigorous proof see [the original
paper](https://www.microsoft.com/en-us/research/wp-content/uploads/1994/06/lazy-functional-state-threads.pdf))

## Back to nub

What has the ST monad bought us? We now have a way to perform in-place mutations
while maintaining referential transparency! Here is nub running in O(n)
on-average using a simplistic mutable hashmap as promised—specifically, using
STArray which is built on top of ST and works much the same way.

```haskell
nub :: (Hashable a, Eq a) => [a] -> [a]
nub l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = (hash j) `mod` 255
    current <- readArray arr index
    let new = if j `elem` current then current else j : current
    writeArray arr index new
  join <$> getElems arr
    where
      mr :: ST s (STArray s Int [a])
      mr = newListArray (0, 255) (replicate 256 [])
```

Note that we have not had to sacrifice referential transparency to obtain this —
the signature of the function is unchanged. It is also an almost direct
translation from an equivalent C implementation, but guarantees that our mutable
state cannot escape the scope of the nub function—something that no imperative
language can do.

## TL;DR

We can use the ST monad to implement functions that use mutable state for
efficiency while maintaining referential transparency, and have the compiler
guarantee that we won't accidentally share that mutable state. :)
