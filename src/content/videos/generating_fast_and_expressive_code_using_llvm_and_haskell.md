---
title: Generating fast and expressive code using LLVM and Haskell
author: Luc Tielen
videoDate: Jan 20, 2022
youtubeUrl: _Qb0mL72l2o
---

In this talk, I show how to create a language by making use of LLVM and Haskell.
At the start, an introduction is given on LLVM and the corresponding Haskell
bindings. Afterwards, I explain how techniques such as combinators and partial
evaluation can be used to create performant functions optimized by LLVM. At the
end, everything is brought together and the result is a small language
that compiles all the way down to assembly.

The code for the BF compiler can be found in
[this repository](https://github.com/luc-tielen/bf).

Since I gave this presentation, I created a
[Haskell package](https://github.com/luc-tielen/llvm-hs-combinators)
that contains the functions I used in this talk.
