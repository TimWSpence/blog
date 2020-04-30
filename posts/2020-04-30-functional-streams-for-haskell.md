---
title: Functional streams for Haskell
---

Popular Haskell streaming libraries such as [Conduit](https://hackage.haskell.org/package/conduit)
and [Pipes](https://hackage.haskell.org/package/pipes) expose a top-level type that looks something
like `Stream m a r` where `m` is an effect type, `a` is the type of the elements in the stream and
`r` is the result type of the stream. In other words, they are functorial in the result
of the stream. Scala's [fs2](https://fs2.io/) library takes a different approach whereby it is
functorial in the element type of the stream instead - `Stream m a` and the result type is determined
by the way you run the stream, rather than the stream itself
(eg `toFile :: FilePath -> Stream m String -> IO ()`).

The following is an experiment in starting with that idea, picking the first stream representation
that came into my head and typing for a couple of hours. I think the result is surprisingly
expressive (if _very_ inefficient and entirely exception unsafe)! See the examples at the end
for what usage looks like.

`example2` in particular illustrates the part of the API which is a bit more painful (AFAIK) in
Pipes or Conduit - dynamically generating and concatenating substreams. In my experience, I've had
to resort to lower level APIs to achieve that in these libraries (although it is entirely possibly
that I missed a combinator somewhere - I have limited experience of both). Regardless, I was surprised
how much functionality could be implemented in such a short time and amount of code by representing
streams as data structures with interpreters.

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Streaming
  (
  )
where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad
import Data.Monoid
import System.IO

-- Represents a stream of elements of type a with an effect type f
data Stream (m :: * -> *) a where
  Yield :: m a -> Stream m a -> Stream m a
  Await :: m x -> (x -> Stream m a) -> Stream m a
  Concat :: Stream m a -> Stream m a -> Stream m a
  Done :: m () -> Stream m a

instance Monad m => Functor (Stream m) where
  fmap = liftM

instance Monad m => Applicative (Stream m) where
  pure = return

  (<*>) = ap

instance Monad m => Monad (Stream m) where
  return a = Yield (return a) (Done (return ()))

  (Yield ma next) >>= f = Concat (Await ma f) (next >>= f)
  (Await ma cont) >>= f = Await ma (cont >=> f)
  (Concat s1 s2) >>= f = Concat (s1 >>= f) (s2 >>= f)
  (Done close) >>= f = Done close

instance Semigroup (Stream m a) where
  (<>) = Concat

instance Monad m => Monoid (Stream m a) where
  mempty = Done (return ())
  mappend = (<>)

fromList :: Monad m => [a] -> Stream m a
fromList [] = Done (return ())
fromList (h : t) = Yield (return h) (fromList t)

fromStdIn :: Stream IO String
fromStdIn = Await getLine return

fromFile :: FilePath -> Stream IO String
fromFile file = Await handle cont
  where
    handle = (openFile file ReadMode) >>= handleStatus
    handleStatus h = fmap (h,) (hIsEOF h)
    cont (h, isClosed) = if (isClosed) then Done (hClose h) else Yield (hGetLine h) (Await (handleStatus h) cont)

toFile :: FilePath -> Stream IO String -> IO ()
toFile file stream = withFile file WriteMode $ \h -> drain . mapF (hPutStrLn h) $ stream

toList :: Monad m => Stream m a -> m [a]
toList (Yield ma next) = do
  h <- ma
  t <- toList next
  return (h : t)
toList (Await ma cont) = do
  a <- ma
  toList (cont a)
toList (Concat s1 s2) = liftA2 (<>) (toList s1) (toList s2)
toList (Done close) = close >> return []

fold :: (Monad m) => (b -> a -> b) -> b -> Stream m a -> m b
fold f b (Yield ma next) = do
  b' <- fmap (f b) ma
  fold f (b') next
fold f b (Await ma cont) = do
  a <- ma
  fold f b (cont a)
fold f b (Concat s1 s2) = do
  b' <- fold f b s1
  fold f b' s2
fold _ b (Done close) = close >> return b

foldMonoid :: (Monad m, Monoid a) => Stream m a -> m a
foldMonoid = fold (<>) mempty

-- Run a stream purely for its effects
drain :: Monad m => Stream m a -> m ()
drain (Yield ma next) = ma >> drain next
drain (Await ma cont) = ma >>= (drain . cont)
drain (Concat s1 s2) = drain s1 >> drain s2
drain (Done close) = close

-- Map an effectful computation across a stream
mapF :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapF f (Yield ma next) = Yield (ma >>= f) (mapF f next)
mapF f (Await ma cont) = Await ma (mapF f . cont)
mapF f (Concat s1 s2) = Concat (mapF f s1) (mapF f s2)
mapF f (Done close) = Done close

-- Print each line in /tmp/data in turn
example1 :: IO ()
example1 = drain . mapF putStrLn $ fromFile "/tmp/data"

-- Duplicate every element in the input list -> [1,1,2,2,3,3]
example2 :: [Int]
example2 = runIdentity . toList . (>>= \x -> return x <> return x) $ fromList [1,2,3]

-- Concatenate files to list
example3 :: IO [String]
example3 = toList $ (fromFile "/tmp/data") <> (fromFile "/tmp/data2")

-- Sum list
example4 :: Int
example4 = getSum . runIdentity . foldMonoid . fmap Sum $ fromList [1,2,3,4,5]

-- Copy file
example5 :: IO ()
example5 = toFile "/tmp/copy" $ fromFile "/tmp/data"
```
