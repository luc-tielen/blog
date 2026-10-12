---
title: Porting C to LLVM using Haskell
author: Luc Tielen
videoDate: Oct 6, 2021
youtubeUrl: mwn646Z13Nc
---

In this video, I port over several C functions of an optimized BTree
implementation to LLVM. This is done using the llvm-hs-pure library in Haskell,
which allows writing Haskell code that eventually generates the corresponding
LLVM code. This offers many benefits over writing LLVM IR directly, mainly
because we can use Haskell's expressiveness to meta-program the generated LLVM
code.

The final result can be found in my
[eclair-lang repository](https://github.com/luc-tielen/eclair-lang/blob/main/lib/Eclair/Runtime/BTree.hs).
