---
title: Optimizing souffle-haskell
author: Luc Tielen
videoDate: May 10, 2021
youtubeUrl: JpbmG9zGYlA
---

In this video, I optimized the C++ bindings of my
[souffle-haskell](https://github.com/luc-tielen/souffle-haskell/) library.

Before this change, I allocated one big contiguous bytearray each time data was
sent from Haskell to C++. After the change, a previously allocated bytearray is
reused. Even with GHC Haskell's pinned memory bytearrays (not as expensive to
allocate as normal `malloc`/`new` in C or C++), this ended up saving 5-10% time
in terms of performance.

The code can be found in this
[pull request](https://github.com/luc-tielen/souffle-haskell/pull/43).
This also includes the optimizations I did when serializing from C++ to Haskell,
where I applied the same trick to get a 30-40% performance improvement.

