---
title: Towards a better API for I/O
---

The core premise of Iteratees (and their various derivatives such as
[fs2](https://fs2.io/) in scala and
[pipes](http://hrckage.haskell.org/package/pipes) and
[conduit](http://hackage.haskell.org/package/conduit) in haskell)
is that there are 3 properties you should seek in an I/O API:

* incrementality (supports streaming/incrementally reading from file descriptors, etc)
* resource safety (resources should be released in a prompt and deterministic manner)
* compositionality
 
 Hopefully the first two properties are fairly self-explanatory. Compositionality requires
 some elaboration however.
 
 Consider for example a function to take a file and return the 
 number of 'x's in the first 5 lines that are longer than 100 characters (don't ask why you
 would want to do this):

 ```haskell
import System.IO
import Control.Exception

processFile :: String -> IO Int
processFile path = bracket (openFile path ReadMode) (hClose) $ \handle ->
  loop handle 0 0
  where
    loop :: Handle -> Int -> Int -> IO Int
    loop h count lineCount = do
      eof <- hIsEOF h
      if eof || (lineCount == 5) then return count else do
        next <- hGetLine h
        if length next < 100 then  loop h count lineCount else loop h (count + (length . filter (== 'x') $  next)) (lineCount + 1)
 ```
 
Now suppose we wanted to instead return the number of 'x's in the first 3 lines
that contain a 'y'. There is no way to re-use the previous code because
our own application logic is tightly entangled with the I/O handling - handle
based I/O does not compose!

Oleg Kiselyov’s [original paper on
iteratees](http://okmij.org/ftp/Haskell/Iteratee/describe.pdf) presents a
particularly concise and instructive implementation of such an I/O API.

In this post we’ll skip over some of the more complex implementation details and
focus more on the core design of iteratees and the reasoning behind the types
Oleg defines.
 
###  A digression on folds

The Haskell prelude defines a fold (let's ignore `Foldable` for the moment):
```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
```

which processes a list from left to right, using a supplied function and initial
value, by passing the output of one invocation of the function as input to the
next.

If we take off our glasses and squint hard enough, this looks a bit like what we
want - the source of the data (the list) is de-coupled from the computation (the
supplied folding function) we want to perform on it.

In fact, we can define a fold on a file as well (simplistic implementation for text files):
```haskell
foldFile :: (a -> Char -> a) -> a -> String -> IO a
foldFile f init path = bracket (openFile path ReadMode) (hClose) $ \handle ->
  loop handle init
  where
    loop h acc = do
      eof <- hIsEOF h
      if eof then return acc else do
        next <- hGetChar h
        loop h (f acc next)
```

Now we're really getting somewhere! Now we can (to a certain extent) compose
functions which we want to apply to a source of data and maintain resource
safety through the definiteion of fold.

We can view the folding function as a
state machine for stream processing and `fold` as a resource manager which
drives the processor. However, there are some limitations to this API:

 * the folding function cannot indicate to the fold that it is finished processing
   the data eg in the "take the first 3 lines of a file" example, the computation
   can finish and the file descriptor be closed after the first 3 lines are read
 * a corollary of the above is that we cannot concatenate stream processors as each
   of them must consume the whole stream
 * there is no encapsulation of the state of the stream processor
 
### Back to Iteratees
 
We are now (finally!) in a position to define the basics of iteratees:

 ```haskell
data Stream a = EOF | Chunks [a]

data Iteratee i m o = Done o
                    | Cont (Maybe Exception) (Stream i -> m (Iteratee i m o, Stream i))

instance Monad m => Monad (Iteratee i m) where
  return = Done

  it >>= f = case it of
    Done v -> f v
    Cont e k -> Cont e (\str -> k str >>= \(it', str') -> return (it' >>= f, str'))

instance MonadTrans (Iteratee i) where
  lift ma = Cont Nothing (\str -> ma >>= \a -> return (Done a, Chunks []))

instance MonadIO m => MonadIO (Iteratee i m) where
  liftIO = lift . liftIO
```
 
Erm, what?

Don’t panic! Firstly we’ve defined a stream type (`Stream a`) which can either be
finished or contain a list of data of type a. We've also defined Iteratee i m o
(like our folding function above), which represents a stream processor which
takes a stream of type i and produces a value of type o, whilst performing
effects of type `m` (`m` is probably `IO` if you are writing to a file or socket or
similar). The iteratee can also return an exception to indicate an error (or as
control flow) to its driver e.g. to request to rewind the stream, if that’s
possible. You can think if it as a state machine - it has no ability to obtain
data from anywhere, it is merely a representation of a computation.

The `Monad{,Trans,IO}` instances are included for interest and because they turn
out to be very useful. For example, here’s a stream processor which counts the
number of elements in a stream:

```haskell
count :: Monad m => Iteratee i m Int
count = Cont Nothing (k 0) where
  k n s@(EOF _) = return (Done n, s)
  k n (Chunk els) = return (Cont Nothing (k (n + length els)), Chunk [])
```

Don't worry too much about the details - writing the internals of an iteratee
library can get a bit messy but the high-level API you can create is well worth
it, I promise!

Now that we have a way to define stream processors, we need stream producers - a
way to drive input into the stream processors to advance their state machine
(like `fold` in our original example):

```haskell
type Enumerator i m o = Iteratee i m o -> m (Iteratee i m o)

-- concatenate inputs using Kleisli composition
(>>>) :: Monad m => Enumerator i m o -> Enumerator i m o -> Enumerator i m o
e1 >>> e2 = e1 >=> e2
```

Why is this the type of a stream producer? An enumerator must take in a stream
processor (`Iteratee i m o`) describing the computation to perform. The value
returned from the enumerator should be the final state of the computation
(iteratee), wrapped in some context m (probably IO) as the enumerator will have
to perform some action to obtain the input. In other words `Iteratee i m o -> m
(Iteratee i m o)`!

The enumerator will probably consist of some sort of loop, performing I/O to
obtain data and folding that with the current state of the iteratee. For
example, we can define an enumerator for text files:


```haskell
enumFileChars :: String -> Enumerator Char IO o
enumFileChars path iter = bracket (openFile path ReadMode) (hClose) $ \handle ->
  loop handle [] iter
  where
    loop h _ _ it@(Done _) = return it -- stop if interatee is finished
    loop h _ _ it@(Cont (Just e) _) = return it -- stop if iteratee signals error
    loop h s (Cont _ k) = do
      eof <- hIsEOF h
      if eof then return it else do
        chunk <- Chunk . (s++) <$> hGetLine h
        (it', str) <- k chunk
        loop h str it'
```

We’ve achieved quite a lot already but there’s one more type we need for general
stream processing. Iteratee can only represent terminal operations which produce
a value. We also need stream transducers to create derived streams so that we
can represent concepts such as filtering or mapping streams or codecs:

```haskell
type Enumeratee outer inner m a = Iteratee inner m a -> Iteratee outer m (Iteratee inner m a)
```

Again, why is this what we want? A stream transducer should take an Iteratee
inner m a describing the downstream computation that is to be performed. It
should use this to create a new Iteratee that consumes a stream of outer ,
transforms it to a stream of inner, passes that to the inner Iteratee and
returns the final state of the inner Iteratee as its result. In other words
`Iteratee inner m a -> Iteratee outer m (Iteratee inner m a)`!

Note that this means thatEnumeratee is effectively an Iteratee and an Enumerator
at the same time. For example, here is filter for streams:


```haskell
filter :: Monad m => (i -> Bool) -> Enumeratee i i m a
filter pred i@(Cont Nothing k) = Cont Nothing (f i) where
  f iter@(Cont Nothing k) s@(Chunk l) = k (Chunk $ filter pred l) >>= \(i, stream) -> return (Cont Nothing (f i), stream)
  f iter s = return (Done iter, s)
filter pred i = return i
```

Again, don't worry too much about the details - you won't have to program at
this level to use iteratees!

Now we just need to define a couple of useful functions and we're ready to try out our iteratees API!

```haskell
-- often we just want the result of the inner iteratee when we have an enumeratee
($=) :: Monad m => Enumeratee o i m a -> Iteratee i m a -> Iteratee o m a
enum $= iter = (enum iter) >>= lift . run
infixr 4 $=

-- run the iteratee! We flush the state by passing in an EOF
-- it is considered an error if the iterator does not transition
-- to state Done
run :: Monad m => Iteratee i m o -> m o
run (Done o) = return o
run (Cont Nothing k) = fst <$> k (EOF Nothing) >>= check where
  check (Done o) = return o
  check (Cont _ _) = error "Diverging iteratee"
run _ = error "Diverging iteratee"
```

Now our original example of counting the ‘x’s in a file under various conditions
would look something like this (definitions for most of the functions omitted -
the high level API is what concerns us here):

```haskell
enumLines :: Enumeratee [String] m o
take :: Int -> Enumeratee a a m o
filter :: (a -> Bool) -> Enumeratee a a m o
map :: (a -> b) -> Enumeratee a b m o
sum :: Num a => Iteratee a m a

run $ enumLines "/tmp/data" $ filter (\l -> length l > 100)
                            $= take 5 
                            $= map (length . filter (== 'x'))
                            $= sum

run $ enumLines "/tmp/data" $ filter (elem 'y')
                            $= take 3
                            $= map (length . filter (== 'x'))
                            $= sum
```

As you can see, we’ve split our computation into reusable, composable components :)

Now that you’ve finished you should (hopefully!) be better equipped to go and read Oleg’s Kiselyov’s original paper and John Lato’s equally excellent article in The Monad Reader 16 to see a more detailed explanation and implementation. Moreover, you’re hopefully more motivated to use one of the libraries that implement these concepts next time you have to perform I/O in your programs!

As you can see, we have split our computation into reusable, composable components :)

Now that you've finished this, you should (hopefully!) be better equipped to go
and read Oleg's Kiselyov's [original
paper](http://okmij.org/ftp/Haskell/Iteratee/describe.pdf) and John Lato's
equally excellent article in [The Monad Reader
16](https://themonadreader.files.wordpress.com/2010/05/issue16.pdf)

### Caveat Emptor

The purpose of this article was to explain the high-level design of iteratees
and their advantages over traditional handle-based I/O. The code is little
tested and absolutely not production quality. Please use a well-established
library like the ones listed above to write real code.
