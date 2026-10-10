---
title: Supercharge your handles using phantom types
author: Luc Tielen
postDate: Mar 13, 2021
tags:
  - haskell
---

In this article, I show how to use phantom types  in combination with the
"Handle pattern" to create more type-safe and user-friendly APIs in Haskell. I
do this based on my experience adding this idea to the
[souffle-haskell](https://github.com/luc-tielen/souffle-haskell) library.

## A quick recap

Jasper van der Jeugt has
[blogged](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html) about the
handle pattern in the past. In summary, he recommends using handles when
interfacing external services (such as a database or file) with Haskell. A
handle type should be created representing access to that service; as well as
functions for communicating with the external service using that handle.

Here's what the handle pattern could look like for an external key-value store:

```haskell
-- Internals of the handle data type are opaque for external users
data DBHandle

get :: DBHandle -> Key -> IO (Maybe Value)
put :: DBHandle -> Key -> Value -> IO ()
```

## Applying the handle pattern to souffle-haskell

In the souffle-haskell library, the handle pattern also turned out to be a great
fit. The library exposes functionality for communicating with the Souffle Datalog
language, which can be viewed as an external resource. Here's what the top level
API looks like (simplified):

```haskell
-- Handle type for communicating with the external Datalog program.
data Handle = ...

-- Monad for performing Souffle-related actions in.
newtype SouffleM = ...

-- Many different functions for communicating with Souffle.
-- Note how all of these functions take a handle as an argument.
addFact :: Fact a => Handle -> a -> SouffleM ()
getFacts :: Fact a => Handle -> SouffleM [a]
run :: Handle -> SouffleM ()
-- Many other similar functions..

-- A function for running actions in the SouffleM monad.
-- Given a function that takes a handle as input and returns an action
-- in the SouffleM monad, this function will return the underlying IO action
-- that will perform all these Souffle-related actions.
runSouffle :: (Handle -> SouffleM a) -> IO a
```

The handle pattern helped to structure the API and to take care of resource
management. However, you can still mis-use this API. For example, since facts
have to be pre-defined for a Souffle program, it doesn't make sense to add a
fact to a Souffle program that doesnt' understand it. Can we do better and
prevent these kinds of logical mistakes?

## Enter the phantom types

If you take a look at the previous Haskell snippet, you will see that every
function takes the handle datatype as an argument. This makes it a great
candidate for doing compile-time checks based on type level information. To see
how we could do this for the snippet above, let's first introduce a phantom type
variable to our handle:

```haskell
{-# LANGUAGE RoleAnnotations #-}

-- "prog" is a type variable referring to a corresponding Datalog program
data Handle prog = ...
-- The next line is needed to prevent users from coercing between
-- handles with different phantom types:
type role Handle nominal

newtype SouffleM = ...

addFact :: Fact a => Handle prog -> a -> SouffleM ()
getFacts :: Fact a => Handle prog -> SouffleM [a]
run :: Handle prog -> SouffleM ()

-- Here we add an extra argument, so that Haskell can infer what type the
-- phantom type of the handle should be.
-- Note: another approach is to use "TypeApplications" (doesn't require an
-- extra argument) to tell Haskell what the type of "prog" should be.
runSouffle :: prog -> (Handle prog -> SouffleM a) -> IO a
```

This phantom type variable by itself doesn't do that much yet except for
catching some type errors, but it does give us a starting point to start adding
information on the type-level. One way to do this is by adding a constraint
(based on the phantom type) to limit the possible scenarios a function can be
used in. Conceptually this looks as follows:

```haskell
myConstrainedFunc :: MyConstraint prog => Handle prog -> SouffleM ()
```

## Taking it a step further with TypeFamilies

For simple usecases the above would suffice, but we need some more expressivity
if we want to check that a fact isn't part of a Souffle program. To achieve
this, we can make use of a combination of the TypeFamilies extension and
"TypeError" in GHC to create sophisticated constraints at the type-level:

```haskell
-- Add these extensions to top of the file.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- With the approach introduced here, some type families will only
-- return a constraint in the error cases (containing a TypeError).
-- GHC will thus think the constraints are redundant.
-- This turns off that warning.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

import Data.Kind (Type)
import GHC.TypeLits

-- Elem is a type family that checks if a type is contained in a
-- list of types. It returns a type-level boolean.
type Elem :: Type -> [Type] -> Bool
type family Elem a as where
  Elem _ '[]       = 'False
  Elem x (x ': _)  = 'True
  Elem x (_ ': xs) = Elem x xs

-- Example usage of Elem: x won't compile when you try to use it
-- However this gives us a rather cryptic error message:
--  • Couldn't match type ‘'False’ with ‘'True’
--      arising from a use of ‘x’
x :: Elem Int '[Char, Bool] ~ 'True => Int
x = 1234

-- To improve this, let's add a type-level assertion.
-- If the assertion is satisfied, everything is ok.
-- If the assertion fails, a custom type error is returned to the user.
type Assert :: Bool -> ErrorMessage -> Constraint
type family Assert condition message where
  Assert 'True _ = ()
  Assert 'False msg = TypeError msg

-- Example using Assert: y won't compile if a is not of type Bool,
-- giving us a custom type error instead.
constrainedId :: Assert (Elem a '[Bool]) ('ShowType "This won't compile!")
              => a -> a
constrainedId = id
```

It is now possible to write a check to see if a fact is contained in a Souffle
program:

```haskell
-- Type family for declaring which fact types belong to a Souffle program
type ProgramFacts :: Type -> [Type]
type family ProgramFacts a

-- The actual check to see if a program contains a certain fact type
type ContainsFact :: Type -> Type -> Constraint
type family ContainsFact prog fact where
  ContainsFact prog fact =
    Assert (Elem fact (ProgramFacts prog))
           ('ShowType "Unknown fact for Souffle program")
```

We can now add this type-level assertion to our initial Haskell snippet:

```haskell
addFact :: (Fact a, ContainsFact prog a) => Handle prog -> a -> SouffleM ()
getFacts :: (Fact a, ContainsFact prog a) => Handle prog -> SouffleM [a]

-- Rest unmodified..


-- In our application code we can specify that Fact1 and Fact2 belong to
-- the Souffle program managed by 'MyProgram':

data MyProgram = MyProgram
data Fact1 = Fact1 String
data Fact2 = Fact2 String

type instance ProgramFacts MyProgram = '[Fact1, Fact2]
```

If we now try to add a fact to a Souffle program that doesn't understand it, it
won't compile anymore. Great!


## Conclusion

Phantom types are a lightweight and powerful technique for adding more
type-safety to your code. Combined with the handle pattern and type families, it
allows you to write expressive type-level assertions that are verified by the
compiler.

Like mentioned before, the code in the post is based on my souffle-haskell
library. I simplified the types in this post to keep the mental overhead as
minimal as possible. Here's the [original
commit](https://github.com/luc-tielen/souffle-haskell/commit/4839959a9b2f45198747355466f066d768c6059a)
where I introduced the idea in the code.

If you are interested in more content like this, follow me on
[Twitter](https://twitter.com/luctielen). Feel free to contact me if you have
any questions or comments about this topic.

