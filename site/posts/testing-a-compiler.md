---
title: Techniques for testing a compiler
author: Luc Tielen
postDate: Nov 3, 2022
tags:
  - compilers
  - testing
---

In today's article, I discuss a set of techniques to improve the test coverage
and overall quality of a compiler. Compilers are some of the most complicated
programs that exist in the world, so special care should be taken to properly
test their behavior, to make sure as little as possible miscompilations occur.

In this post I will focus on unit/integration-style testing; formal verification
and performance testing are out of scope (though these can be a useful tool to
detect bugs and regressions).

Since these techniques are applicable for any compiler written in any language,
I will use pseudo-code where possible.

## 1. Standard best practices for testing apply

First things first: just like for any other piece of software, the basics of
unit testing apply. A compiler can be a complicated piece of software, so you
should create an extensive test suite to cover as many cases as possible to
catch regressions in the future. On top of that, if you come across an issue
during development, you should add atleast one test to ensure the problem
doesn't reappear in the future.

Since you will be writing _a lot of_ tests, they should be easy to write! You
can do this by investing in helper functions for your test suite that perform a
lot of the "boilerplate"/setup work. Without these helper functions, writing
tests will be a hassle and as a result not many tests will be written, leading
to regressions over time.

## 2. Use different kinds of testing approaches

TODO

what kind of tests:
small, focused tests on one usecase
integration style tests: typecheck/run the examples, stored as actual files in the repo
golden tests (what)
property tests (what, more on this later)

## 3. Start by writing an interpreter for your language

With the standard testing practices out of the way, one of the very next things
you should focus on is to work towards building an interpreter for your
language. Even if your end-goal is writing a compiler, an interpreter is a good
first stepping stone to get things up and running.

Implementing an interpreter gives you a couple of benefits:

1. You can start testing the frontend (parser, typechecker, ...), without
   needing a code generation backend;
2. Your interpreter can serve as a reference for the final compiler (same
   behavior when running a program).

An interpreter is not just great from a testing point of view, but also allows
you to quickly iterate and experiment with ideas in your language.

## 4. Add extra intermediate representations where needed

TODO
figuur
linken naar vorige post?
a compiler consists of many intermediate representations (also known as an "IR")
add an IR layer for better coverage, introspectability
dont do everything all at once, maybe a bit slower but reduces complexity

Surprised?

features in terms of other features => small core => easier to get full coverage on
maybe slight perf hit, but initially the correctness is more important. later it
can be refactored/optimized anyway (after profiling)

As an example: in the Datalog compiler I'm currently writing, I tried going to
the LLVM level way too fast. The code became extremely complicated, and was very
hard to test and inspect (LLVM generated code often just segfaults). By adding
an extra IR in between, I could first handle all the complexity not related to
LLVM, and then do a relatively simple transformation from this new IR to LLVM.

## 5. Make use of property based testing

While you are writing your compiler, you will be writing _a lot of_ tests.
However, it can be hard to think of all possible interactions between one
feature and the rest of the compiler. Chances are high you will not think
of some important edge cases (integer overflow or unicode shenanigans anyone?
:smirk:). This is where fuzz-testing or property based testing comes in and
makes sure your compiler is also tested against these types of issues.

Fuzz testing is a form of testing where pseudo-randomized inputs (programs) are
given to the compiler, in an attempt to make it crash or generate invalid code.
Property based testing is similar to fuzz testing, but on top of that, it also
allows you to make assertions about the behavior of your code.

You can combine these forms of testing together with some of the previously
mentioned steps. For example, you could write an interpreter for each IR in the
compiler and start writing property tests. Here are some potential properties
you could write:

1. Interpreting a program at one IR layer should give the same result as
   interpreting the generated IR for the same program at the next layer.
2. Interpreting a program should return the same result as first compiling and then
   running the program.

Or, in pseudo-code:

```
property1() {
  assert interpret(ir) == interpret(ir_next)
}

property2() {
  assert interpret(program) == run(compile(program))
}
```

With these properties defined, the testing framework will then generate
randomized inputs and run thousands of tests to stress-test your compiler,
giving you more confidence in the quality and correctness of your code.

## 6. Test with as many configurations as possible

Another way to stress-test your compiler using property based testing is via the
configuration options that your compiler provides. Do you have different
optimization levels? Different architectures to compile to? Can you turn some
features on or off?

You can verify this is true by compiling the same program with two different sets
of configuration options, and then running the results. In pseudo-code one of
these properties looks as follows:

`run(compile(file, config)) == run(compile(file, config2))`

You could arrange these configurations in a matrix and test all combinations,
but keep in mind that the total number of possible configurations quickly
increases. Selecting a key set of configurations to test is important here to
limit this combinatory explosion.

## 7. Structure your compiler around queries

A compiler is usually structured as a large compiler pipeline of multiple steps,
with a lot of internal state. As a consequence, this means you have to
re-implement part of that pipeline in the setup code of your tests each time,
which can get unwieldy over time as both the pipeline and test-suite grow in size.

TODO explain what it is

Olle Fredriksson has a great talk on query-based compilers and how his [rock
library](https://hackage.haskell.org/package/rock) in Haskell can help structure
your compiler in this way:

<iframe
  width="1280"
  height="720"
  src="https://www.youtube.com/embed/3D-ngGIP4fQ"
  title="Query-based compiler architecture"
  frameborder="0"
  allow="autoplay; clipboard-write; encrypted-media; gyroscope; accelerometer; picture-in-picture"
  allowfullscreen>
</iframe>

This suggestion is maybe one of the more invasive techniques on the list that
you can apply, but it also has significant advantages. By structuring your
compiler around queries, testing becomes trivial: if you give the compiler a
query it should execute, it will always return a result for that query. This
result can then be used directly in the assertions of your unit tests. In
pseudo-code, this looks as follows:

```
test() {
  # "query" could mean: compile / typecheck / ... a file
  result = runQuery query
  assert result == ...
}
```

Besides the benefits for testing, it can clean up or simplify the main compiler
pipeline by decomposing it into many small composable steps that can depend on
each other. On top of that, by structuring your compiler in this way, it also
makes it straight-forward to expose information about your programs to the
outside world. This makes developer tooling such as language servers, linters,
... trivial to implement!

## Conclusion

TODO no shortcuts, can be a lot of work, but an extensive test suite is the way to go
by making use of techniques mentioned here, the work can be reduced
significantly, while still having enough confidence

here's the techniques I use to test my compiler, did I miss anything?

If you have any questions or thoughts about this topic, let me know on
[Twitter](https://twitter.com/luctielen).

TODO
fix all open TODOs
read everything again
image
numbering of headers = ok?
nalezen (ook door anderen)
