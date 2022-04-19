---
title: How to lower an IR?
author: Luc Tielen
postDate: Apr 30, 2022
tags:
  - compilers
  - haskell
---

In today's article, I show the approach I use when writing a compiler to lower
one intermediary representation into another. I will be using Haskell to explain
how to do it, but this technique can be used in other languages as well (with
some minor modifications).


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

If we try to represent this visually, it looks like the figure shown below:

TODO schema of typical compiler structure


## Writing a lowering pass

With the introduction out of the way, we can now start writing an actual
compiler lowering pass. Let's first create 2 IRs:

```haskell
-- variables, control flow (if), ..., * and +
data IR1

-- IR2: stack based language / BF
data IR2
```

TODO explanation about the IRs, what is different about them etc
mention recursive trees, AST
example:
  - simple calculator language, lowering * to +? or multiple chained + to ASM?
  - try to keep it simple and straight forward

With the IRs out of the way, we can now implement our pass. In Haskell, the
simplest type signature for a pass would look like this:

```haskell
loweringPass :: IR1 -> IR2
```

This should be easy to implement, right? A function that takes one argument (an
intermediary format) and transform it into another format. However, most of the
time, there are factors making this much more complicated:

1. The IR datastructures tend to be sum types (tagged unions) with many
   different variants, that each need to be handled.
2. To do the transformation, you often end up needing complex information, not
   immediately available at that point in the IR tree.
3. Higher level features in one IR might need to be mapped to multiple
   constructs in the lower level IR.

To deal with this, we need a structured approach.


TODO

strategy (explain using the example below):
  - 2 modules: 1 helper, 1 for actual lowering
  - helper module
    - CodegenM monad
    - combinators that do the work
  - lowering:
    - use combinators in the CodegenM monad to do the actual lowering
    - lowering = pattern match, recursively call lower correctly

TODO actual implementation




TODO

some final tips:
- don't try to cram everything into 1 pass

conclusion:
keep in mind that it can be very different for every compiler, but general
approach should always work
hopefully this post demystified a little bit how compilers work internally
lowering an IR is not easy, but extremely powerful technique
though this structured approach in multiple steps, the complexity is managed
if you liked this post or have thoughts/questions about it, let me know on
eclair vermelden, if you want to see it in action (though be warned, the actual
passes are quite .. intense)
twitter CTA

out of scope:
  - creating an IR, if interested -> let me know
  - next post: how to use generalized recursion schemes to do this in an even more
    structured (and performant!) way

figuur? IR1 -> IR2? compiler structuur met "lowering" aangeduid? (vertical picture?)
