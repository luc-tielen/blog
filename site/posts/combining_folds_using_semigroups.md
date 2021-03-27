---
title: Combining folds using semigroups
author: Luc Tielen
date: Mar 27, 2021
tags:
  - haskell
---

In today's post, I will show how to combine many recursion-scheme folds
using semigroups. For seasoned Haskellers using the library, this trick might be
well known or obvious, but I decided to write it down anyway for the rest of us
mere mortals :smiley:. I will assume a little familiarity with
recursion-schemes though. If this isn't the case, be sure to check out the
(recently revised) [recursion-schemes README](https://github.com/recursion-schemes/recursion-schemes/)
for an introduction to the library.

## The naive or straightforward approach

For our running example, let's take the idea of "static analysis" in a compiler.
Here, the compiler executes many checks to make sure a program is semantically
valid. Implemented in a straightforward way, each check is an (effectful)
function that does a full traversal of the program AST to collect the analysis
results. If we simplify this idea, we end up with the following:

```haskell
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

-- Our AST data type:
data Expr
  = Constant Int
  | Add Expr Expr

makeBaseFunctor ''Expr

-- 1 + 2 + 3
expr :: Expr
expr = Constant 1 `Add` Constant 2 `Add` Constant 3

-- An analysis pass is an effectful function (here IO is used
-- to print to the terminal):

-- Two effectful functions that we want to combine:
function1, function2 :: ExprF (IO ()) -> IO ()
function1 = function "function1:"
function2 = function "function2:"

-- An effectful function, prints out a prefix + a textual description
-- of each node it encounters.
function :: String -> ExprF (IO ()) -> IO ()
function prefix = \case
  ConstantF x ->
    putStrLn $ unwords [prefix, "Constant", show x]
  AddF action1 action2 -> do
    action1
    putStrLn $ unwords [prefix, "Add"]
    action2

main :: IO ()
main = do
  putStrLn "Running function1:"
  results1 <- cata function1 expr
  putStrLn "Running function2:"
  results2 <- cata function2 expr
  -- ... do something with results
```

Running the snippet above gives us the following output:

```bash
$ stack run  # Assuming we are in a stack project
Running function1:
function1: Constant 1
function1: Add
function1: Constant 2
function1: Add
function1: Constant 3
Running function2:
function2: Constant 1
function2: Add
function2: Constant 2
function2: Add
function2: Constant 3
```

You can see that each traversal happens one after the other. Ideally though, we
would like to collect all analysis results using a single traversal/fold. Let's
try and figure out how we can achieve that.

## Attempt 1: Using function composition

Others have already thought of ways to compose folds using recursion schemes.
Tim Williams has a great [presentation](https://github.com/willtim/recursion-schemes/raw/master/slides-final.pdf)
about recursion schemes where he shows techniques for composing them. First of,
there's sequential composition:

```haskell
-- Note: not valid Haskell, '=' is mathematical equality
cata f . cata g = cata (f `comp` g) where
  -- 'comp' first applies the function 'y', unwraps a single layer of
  -- the Fix datastructure, and then finally applies the function 'x'
  comp :: (f (Fix f) -> a)
       -> (g (Fix f) -> Fix f)
       -> g (Fix f)
       -> a
  comp x y = x . unfix . y
```

However, this approach is not applicable for our static analysis running example
due to the types not lining up. The result of the first function (`g` / `y`) needs
to return a datastructure wrapped in `Fix` (but we return a `IO ()` value).

The second approach Tim mentions (using the `(&&&)`-operator from
`Control.Arrow`) *is* applicable:

```haskell
cata f &&& cata g = cata alg where
  -- f :: f a -> a
  -- g :: f b -> b
  alg :: f (a, b) -> (a, b)
  alg = f . fmap fst &&& g . fmap snd
```

For "simple" values, this works great. However, this approach has the same
problem as before when you return a function or monadic result instead. To
illustrate this, let's run the snippet below:

```haskell
-- Note that alg returns a tuple of two IO actions now, instead of one!
alg :: ExprF (IO (), IO ()) -> (IO (), IO ())
alg = (function1 . fmap fst) &&& (function2 . fmap snd)

main :: IO ()
main = do
  let (action1, action2) = cata alg expr
  action1
  putStrLn "action1 finished, now action2:"
  action2
```

This gives us the following output:

```bash
$ stack run
function1: Constant 1
function1: Add
function1: Constant 2
function1: Add
function1: Constant 3
action1 finished, now action2:
function2: Constant 1
function2: Add
function2: Constant 2
function2: Add
function2: Constant 3
```

This is the same behavior as we had before. We perform only one fold now, but
then we *still* have to run the actions separately. That's not what we want!
Back to the drawing board!

## Attempt 2: Using semigroups

We tried some forms of composition so far that didn't work out, but luckily
Haskell has many more ways of composing expressions together. One that is
often used is `(<>)` / `mappend` from the `Semigroup` typeclass. Here's what
that looks like:

```haskell
main :: IO ()
main = cata alg expr where
  -- alg / function1 / function2 are a Semigroup because:
  -- A function is a Semigroup if it's result is a Semigroup;
  -- 'IO a' is a Semigroup if 'a' is a Semigroup;
  -- () is a Semigroup.
  alg :: ExprF (IO ()) -> IO ()
  alg = function1 <> function2
```

Effectively, this calls the 2 functions in a row, and then combines the results.
Running this code shows all effectful actions are interleaved as we'd expect:

```bash
$ stack run
function1: Constant 1
function2: Constant 1
function1: Add
function2: Add
function1: Constant 2
function2: Constant 2
function1: Add
function2: Add
function1: Constant 3
function2: Constant 3
```

Great! This is exactly what we want. We do only one fold over the datastructure
and all actions are interleaved!

## Other monads than IO

While the approach using semigroups is short and sweet, it is not always
possible to use it directly. This is because the monad that is being used needs
to have a Semigroup instance (and IO just so happens to have an implementation
for it). With a little more code, we can work around this limitation:

```haskell
compose :: (Monad m, Monoid b) => (a -> m b) -> (a -> m b) -> a -> m b
compose f g a = do
  result1 <- f a
  result2 <- g a
  pure $ result1 <> result2

-- Or using applicative notation:
compose' :: (Applicative f, Monoid b) => (a -> f b) -> (a -> f b) -> a -> f b
compose' f g a = (<>) <$> f a <*> g a

-- If you only care about side effects, you could use (*>)
-- instead of (<>) as well.

main :: IO ()
main = cata alg expr where
  alg = compose function1 function2
```

With the "compose" function, you can now also use the same approach for monads
that don't implement Semigroup (such as Reader/State/...). Nice!

## Wrapping up

In this article I showed 3 approaches to combining many smaller folds into one
fused fold. Though not all approaches may be applicable for a specific
situation, there are always multiple options to choose from when combining
functions in this way.

I need to point out that while I used the recursion-schemes library, the same
approach is possible if you use "normal" recursive functions (but maybe with a
little more work).

If you are interested in more content like this, follow me on
[Twitter](https://twitter.com/luctielen). Feel free to contact me if you have
any questions or comments about this topic.

