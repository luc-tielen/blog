---
title: How to lower an IR?
author: Luc Tielen
postDate: Apr 30, 2022
tags:
  - compilers
  - haskell
image: how_to_lower_an_ir.png
---

In today's article, I show the approach I use when writing a compiler to lower
one intermediary representation into another. I will be using Haskell to explain
how to do it, but this technique can be used in other languages as well (with
some minor modifications). Since this is a very general and broad topic, it will
be hard at times to go into specifics but I will try to mention the key things
that are applicable every time.

## So what is "lowering" anyway?

Before we dive into the code, let's first take a look at what a typical compiler
looks like. In essence, a compiler reads in one or more files, parses them and
transforms the contents into executable code. This transformation often ends up
being quite involved and is performed in multiple steps to manage the overall
complexity. A single one of these steps is often called a (lowering) pass.

Between each step of the compilation process, a different IR or intermediary
representation is used to gradually remove high level features and describe them
in terms of lower level features. Do this enough times, and eventually you will
end up with an IR simple enough to be compiled down to the assembly level.

## Writing a lowering pass

With the introduction out of the way, we can now start writing an actual
compiler lowering pass. Let's first create 2 IRs:

```haskell
-- variables, control flow (if), ..., * and +
data IR1

-- IR2: stack based language / BF
data IR2

TODO explanation about the IRs, what is different about them etc
mention recursive trees, AST
example:
  - simple calculator language, lowering * to +? or multiple chained + to ASM?
  - try to keep it simple and straight forward
```

With the IRs out of the way, we can now implement our pass. In Haskell, the
simplest type signature for a pass would look like this:

```haskell
loweringPass :: IR1 -> IR2
```

This should be easy to implement, right? A function that takes one argument (an
intermediary format) and transform it into another format. However, most of the
time, there are factors making this much more complicated:

1. The IR datastructures tend to be sum types (tagged/discriminated unions) with
   many different variants, that each need to be processed differently.
2. To perform the transformation, you often end up needing a lot of additional
   information, not immediately available at that point in the IR tree.
3. Some of these transformations might require a lot of logic (which might in
   turn require some internal state to be managed).
4. Higher level features in one IR might need to be mapped to a combination of
   multiple constructs in the lower level IR.

All these things combined quickly turn that simple function into a complex
beast. To deal with this complexity, we need a structured approach. I usually
like to split my code into two modules (next to my module defining the IR):

```bash
$ tree IR1
IR1
├── IR.hs
├── Codegen.hs
└── Lower.hs
```

`Codegen.hs` forms a module that contains helper types / functions / combinators
for doing the actual code generation. This module usually also defines a
`CodegenM` monad that keeps track of state during the compiler pass. Since all
helper functionality is defined in `Codegen.hs`, `Lower.hs` contains only the
high-level logic to transform one IR into the next.

This might seem a little abstract right now, so let's see what it would look
like for the 2 IRs defined earlier!

```haskell
-- How to actually define an IR is out of scope for this post.
TODO
actual implementation
add comments in between
check if text below still matches
TODO rewrite (+ add more text, or as comments in the code):
```

Lower.hs now contains a monadic function `loweringPass :: IR1 -> CodegenM IR2`
that does a pattern match and recursively handles all cases of the IR on a high
level, while all details are handled by the `CodegenM` monad and combinators.
This is close to what we initially wanted to write!

Besides the approach I described so far, here are some final tips that also make
writing lowering passes much easier.

1. Don't try to cram everything into one pass. Splitting up a transformation in
   two (or more) parts can be a great way to reduce complexity, increase
   maintainability and improve testability.
2. The `recursion-schemes` library in Haskell can help with structuring many
   moving parts of a lowering pass (though it comes with a steep learning
   curve). My previous blogpost showed how to
   [create recursion-schemes using comonads](../create_recursion_schemes_using_comonads/),
   which I ended up using in some of my compiler's passes.

## Conclusion

In this post, I explained how I structure my compiler to transform one
intermediary format to another format. By using a two-module approach, the
internal details stay separate from the high logic, giving a clear overview of
what the transformation is doing. Even though transforming an IR is always
different, this general technique should always be applicable.

If you have any questions or thoughts about this article, let me know on
[Twitter](https://twitter.com/luctielen).
