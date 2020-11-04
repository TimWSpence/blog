---
title: Functional streams for Haskell Part 2
---

In [part 1](./2020-04-30-functional-streams-for-haskell.md) we developed a simple
streaming library based on the first stream representation that popped into my head,
which happened to be:

```haskell
data Stream (m :: * -> *) a where
  Yield :: m a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Done :: m () -> Stream m a
```

Initially this seemed promising and indeed we got quite far with this representation.
However, the wheels start to come off when we attempt to implement `filter`. The problem
is that we're conflating to separate concepts: emitting elements of the stream and
performing effects.
Consider how we would define `filter` for `Yield ma cont`. There is no simply no way
to apply the filter function before making a decision about whether to sequence the
effect to emit `ma`.

This leads us to a different representation which separates the emitting of elements
from the performing of effects:

```haskell
data Stream (m :: * -> *) a where
  Yield :: a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Eff :: m (Stream m a) -> Stream m a
  Done :: m () -> Stream m a
```

And indeed this works! We can now define filter:

```haskell
filterS :: Functor m => (a -> Bool) -> Stream m a -> Stream m a
filterS f (Yield a next) = if f a then Yield a (filterS f next) else filterS f next
filterS f (Await ma cont) = Await ma (filterS f . cont)
filterS f (Eff ms) = Eff . fmap (filterS f) $ ms
filterS _ (Done close) = Done close
```

For completeness, here is the rest of the code updated to this new model, along with an example demonstrating `filterS`:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Streaming
  (
  )
where

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Functor.Identity
import Data.Monoid
import System.IO

-- Represents a stream of elements of type a with an effect type f
data Stream (m :: * -> *) a where
  Yield :: a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Eff :: m (Stream m a) -> Stream m a
  Done :: m () -> Stream m a

instance Monad m => Functor (Stream m) where
  fmap = liftM

instance Monad m => Applicative (Stream m) where
  pure = return

  (<*>) = ap

instance Monad m => Monad (Stream m) where
  return a = Yield a (Done (return ()))

  (Yield a next) >>= f = (f a) <> (next >>= f)
  (Await ma cont) >>= f = Await ma (cont >=> f)
  (Eff ms) >>= f = Eff . fmap (>>= f) $ ms
  (Done close) >>= f = Done close

instance Monad m => Semigroup (Stream m a) where
  (Yield a next) <> s = Yield a (next <> s)
  (Await mx cont) <> s = Await mx ((<> s) . cont)
  (Eff ms) <> s = Eff . fmap (<> s) $ ms
  (Done close) <> s = Eff $ close $> s

instance Monad m => Monoid (Stream m a) where
  mempty = Done (return ())
  mappend = (<>)

fromList :: Monad m => [a] -> Stream m a
fromList [] = Done (return ())
fromList (h : t) = Yield h (fromList t)

fromStdIn :: Stream IO String
fromStdIn = Await getLine return

fromFile :: FilePath -> Stream IO String
fromFile file = Await handle cont
  where
    handle = (openFile file ReadMode) >>= handleStatus
    handleStatus h = fmap (h,) (hIsEOF h)
    cont (h, isClosed) =
      if (isClosed)
        then Done (hClose h)
        else Await (hGetLine h) (\s -> Yield s (Await (handleStatus h) cont))

-- This is very inefficient as it doesn't close streams early
takeS :: Int -> Stream m a -> Stream m a
takeS 0 s = case s of
  (Yield _ next) -> takeS 0 next
  s@(Await mx cont) -> Await mx (takeS 0 . cont)
  (Done close) -> Done close
takeS n s = case s of
  (Yield ma next) -> Yield ma (takeS (n -1) next)
  (Await mx cont) -> Await mx (takeS n . cont)
  (Done close) -> Done close

toFile :: FilePath -> Stream IO String -> IO ()
toFile file stream = withFile file WriteMode $ \h -> drain . mapF (hPutStrLn h) $ stream

toList :: Monad m => Stream m a -> m [a]
toList (Yield a next) = do
  t <- toList next
  return (a : t)
toList (Await ma cont) = do
  a <- ma
  toList (cont a)
toList (Eff ms) = do
  s <- ms
  toList s
toList (Done close) = close >> return []

fold :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> m b
fold f b (Yield a next) = do
  let b' = f b a
  fold f (b') next
fold f b (Await ma cont) = do
  a <- ma
  fold f b (cont a)
fold f b (Eff ms) = do
  s <- ms
  fold f b s
fold _ b (Done close) = close >> return b

foldMonoid :: (Monad m, Monoid a) => Stream m a -> m a
foldMonoid = fold (<>) mempty

-- Run a stream purely for its effects
drain :: Monad m => Stream m a -> m ()
drain (Yield a next) = drain next
drain (Await ma cont) = ma >>= (drain . cont)
drain (Eff ms) = ms >>= drain
drain (Done close) = close

-- Map an effectful computation across a stream
mapF :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapF f (Yield a next) = Await (f a) (const (mapF f next))
mapF f (Await ma cont) = Await ma (mapF f . cont)
mapF f (Eff ms) = Eff . fmap (mapF f) $ ms
mapF _ (Done close) = Done close

filterS :: Functor m => (a -> Bool) -> Stream m a -> Stream m a
filterS f (Yield a next) = if f a then Yield a (filterS f next) else filterS f next
filterS f (Await ma cont) = Await ma (filterS f . cont)
filterS f (Eff ms) = Eff . fmap (filterS f) $ ms
filterS _ (Done close) = Done close

-- Print each line in /tmp/data in turn
example1 :: IO ()
example1 = drain . mapF putStrLn $ fromFile "/tmp/data"

-- Duplicate every element in the input list -> [1,1,2,2,3,3]
example2 :: [Int]
example2 = runIdentity . toList . (>>= \x -> return x <> return x) $ fromList [1, 2, 3]

-- Concatenate files to list
example3 :: IO [String]
example3 = toList $ (fromFile "/tmp/data") <> (fromFile "/tmp/data2")

-- Sum list
example4 :: Int
example4 = getSum . runIdentity . foldMonoid . fmap Sum $ fromList [1, 2, 3, 4, 5]

-- Copy file
example5 :: IO ()
example5 = toFile "/tmp/copy" $ fromFile "/tmp/data"

-- Take from list -> [1,2,3]
example6 :: [Int]
example6 = runIdentity . toList . takeS 3 $ fromList [1, 2, 3, 4, 5]

-- Filter list -> prints 2 4
example7 :: IO ()
example7 = drain . mapF print . filterS even . fromList $ [1,2,3,4] :: IO ()
```
