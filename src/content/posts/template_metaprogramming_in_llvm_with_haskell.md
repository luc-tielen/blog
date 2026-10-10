---
title: Template metaprogramming in LLVM with Haskell
author: Luc Tielen
postDate: Oct 27, 2022
tags:
  - haskell
  - llvm
  - c++
image: template_metaprogramming_llvm.png
---

In today's article, I show how a single monad transformer can be used to gain
template metaprogramming capabilities à la C++ when generating LLVM code in
Haskell. I will be assuming that you are using either the
[llvm-codegen](https://github.com/luc-tielen/llvm-codegen.git) or the
[llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) library
(both have an almost identical API) and that you are a little familiar
with monad transformers.

## What is a template anyway?

Before we dive straight into the Haskell code, let's first take a look at (some
pseudo-code of) C++ templates to understand what they are and why they are so powerful.

```cpp
template <typename T>
class set {
  void add(T value) {
    // code to add a value to the set
  }

  bool contains(T value) {
    // code to check if the set contains a value
  }

  // many other fields and methods ...
}
```

The snippet above is a simplified example of a C++ template that represents a set
of values. The actual type of the values inside the set is not specified here
(it can be any type). Because of this, a template by itself can't be used directly
in code, you first need to _instantiate_ the parameters of the template with certain
types or values. This will cause the compiler to "stamp" out code according to what
is defined in the template, but with all the template parameters replaced with
concrete types or values.

Templates form a great way to write generic code in C++ and at the same time
they are a great way for writing performant code since all information is
specialized and inlined for a specific usecase.

However, templates are not available if you are working directly with a low
level assembly language such as LLVM. But can we somehow gain the potential to
do this by using a combination of LLVM and Haskell? Let's find out!

## A conceptual look at templates

If we take a step back and look at the functionality of templates, they do the
following things:

1. Templates can generate code (types and functions);
2. They do this based on certain template parameters;
3. The names of the generated functions need to be unique to avoid collisions
   (C++ does this automatically via _"name mangling"_);
4. Templates can be created in other locations (files) as where they are
   instantiated.

In summary: given some suffix and template params `p`, an instantiated template
will output generated LLVM IR code. This definition leads us to a first version
of what a template is:

```haskell
type Suffix = Text

template :: Monad m => Suffix -> p -> ModuleBuilderT m a
```

Some notes about this encoding:

1. `ModuleBuilderT` is a monad transformer used in the `llvm-codegen` and
   `llvm-hs-pure` libraries for generating LLVM types and functions.
2. The type variable `a` represents the actual output; this can be one
   or more functions or types.
3. Unlike C++ templates, we need to provide a suffix ourselves. I actually
   consider this a benefit, since it gives more control over the generated code.
   (Though you do need to watch out for naming collisions.)
4. C++ only allows types and a few other primitive types as template parameters,
   but in this variant every possible Haskell value is allowed!

## Iterating on our first attempt

While this encoding would work, it would certainly get tedious very fast
to be manually passing functions around. Luckily, Haskell already has a solution
for this: the `Reader` monad! Let's rewrite our previous example as a monad, and
while we're at it, turn it into a MTL-compatible monad transformer:

```haskell
type Suffix = Text

-- The type variable 'm' allows using this in a pure context with Identity as
-- the base monad, or in a effectful stack on top of IO.
newtype TemplateT p m a
  = TemplateT
  { unTemplateT :: ReaderT (Suffix, p) (ModuleBuilderT m) a
  } deriving ( Functor, Applicative, Monad, MonadFix
             , MonadIO, MonadError e, MonadState s, MonadModuleBuilder
             )
  via ReaderT (Suffix, p) (ModuleBuilderT m)

type Template p = TemplateT p Identity

-- MFunctor is from the `mmorph` package.
instance MFunctor (TemplateT p) where
  hoist f =
    TemplateT . hoist (hoist f) . unTemplateT

instance MonadReader r m => MonadReader r (TemplateT p m) where
  ask =
    lift ask

  local f (TemplateT m) =
    TemplateT $ hoist (local f) m

instance MonadTrans (TemplateT p) where
  lift m =
    TemplateT $ ReaderT $ const $ lift m
```

With the power of `DerivingVia`, we managed to quickly derive many of the
standard MTL instances. We even added support for `MonadModuleBuilder`
defined in the Haskell LLVM libraries I mentioned earlier. For `MonadReader`
we need to put in a little more effort, but with hole driven development and
the `hoist` function we can find the above implementation.

We can also add typeclasses to easily access the suffix and template params in a
monad transformer stack. I left out the usual "MTL boilerplate" in this
article, but you can find the complete code [here](https://github.com/luc-tielen/eclair-lang/blob/main/lib/Eclair/LLVM/Template.hs).

```haskell
class HasSuffix m where
  getSuffix :: m Suffix

instance Monad m => HasSuffix (TemplateT p m) where
  getSuffix =
    TemplateT $ asks (("_" <>) . fst)

class MonadTemplate p m | m -> p where
  getParams :: m p

instance Monad m => MonadTemplate p (TemplateT p m) where
  getParams =
    TemplateT $ asks snd
```

Now that we have defined this new monad transformer and typeclasses,
we can also create helper functions for using these templates in the rest of our
code. Let's start with `cmapParams`, a function to transform the type of the
template parameters type. This is a nice little utility to make a template of
one type "fit into" another template (remember, we're still working with monads
in Haskell).

```haskell
-- It would be nice to create a Contravariant instance, but the
-- kind of TemplateT doesn't match..
cmapParams :: (p2 -> p1) -> TemplateT p1 m a -> TemplateT p2 m a
cmapParams f (TemplateT m) =
  TemplateT $ flip withReaderT m $ second f
```

Next up is instantiation of a template to generate actual LLVM instructions.
With our encoding this ends up being trivial, all we need to do is "evaluate"
the `TemplateT` part of the monad transformer stack, giving us back the
underlying `ModuleBuilderT` action:

```haskell
instantiate :: Suffix -> p -> TemplateT p m a -> ModuleBuilderT m a
instantiate suffix p (TemplateT m) =
  runReaderT m (suffix, p)
```

A useful variation on this is "partial instantiation":

```haskell
-- This instantiates a template and wraps it up again as part of another template.
partialInstantiate :: Monad m => p1 -> TemplateT p1 m a -> TemplateT p2 m a
partialInstantiate p t = do
  suffix <- getSuffix
  embedIntoOtherTemplate $ instantiate suffix p t
  where
    -- This embeds a plain ModuleBuilderT action into a template.
    -- This action has no access to the actual template params
    -- from this point onwards.
    embedIntoOtherTemplate :: ModuleBuilderT m a -> TemplateT p m a
    embedIntoOtherTemplate m = TemplateT $ ReaderT $ const m
```

This is convenient if you have a template that generates types and/or functions,
but you want to defer further specialization up to a later point (e.g. a
templated class that contains templated member functions). As a quick side note,
this function is the reason I had to go down this whole rabbit-hole.
I needed this functionality
[somewhere in the Eclair runtime](https://github.com/luc-tielen/eclair-lang/blob/290b57f5e67ec134c5006841b83a5a4bd8349a49/lib/Eclair/LLVM/BTree.hs#L958).

The final two helper functions we need are enhanced versions of `function` and
`typedef`. These functions already exist in the `llvm-codegen` / `llvm-hs-pure`
libraries and allow you to define new functions and types respectively, but they
don't automatically take the template suffix into account yet.

We can work around this by hiding the original functions and exporting similarly
named functions with (almost) the same type signature. This let's us
automatically add a suffix via the `HasSuffix` type class:

```haskell
import LLVM.Codegen hiding (function, typedef)
import qualified LLVM.Codegen as CG


function :: (MonadModuleBuilder m, HasSuffix m)
         => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function (Name name) args retTy body = do
  suffix <- getSuffix
  let nameWithSuffix = Name $ name <> suffix
  CG.function nameWithSuffix args retTy body

typedef :: (MonadModuleBuilder m, HasSuffix m)
        => Name -> Flag Packed -> [Type] -> m Type
typedef (Name name) packedFlag tys = do
  suffix <- getSuffix
  let nameWithSuffix = Name $ name <> suffix
  CG.typedef nameWithSuffix packedFlag tys
```

And with that we have all we need to generate LLVM IR using templates!

## Conclusion

In this post, I showed how you can use a single monad transformer to gain
template metaprogramming capabilities when generating LLVM IR. The full code for
for the LLVM templates can be found
[here](https://github.com/luc-tielen/eclair-lang/blob/main/lib/Eclair/LLVM/Template.hs).
To see it being used in action, you can look
[here](https://github.com/luc-tielen/eclair-lang/blob/290b57f5e67ec134c5006841b83a5a4bd8349a49/lib/Eclair/EIR/Lower.hs#L242)
and [here](https://github.com/luc-tielen/eclair-lang/blob/290b57f5e67ec134c5006841b83a5a4bd8349a49/lib/Eclair/LLVM/Vector.hs)
for a couple more examples.

You could further extend this idea by introducing a caching mechanism so that
instantiations with the same template parameters return the same code, but I
left that out of scope for this post.

If you are interested in more content like this, follow me on
[Twitter](https://twitter.com/luctielen). Feel free to contact me if you have
any questions or comments about this topic.
