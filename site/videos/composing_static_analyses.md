---
title: Composing Souffle analyses in Haskell
author: Luc Tielen
videoDate: Mar 18, 2021
youtubeUrl: videoseries?list=PL0aeNUfC2dmk98_yVgL743r85GsCQDW1x
---

In this video series, I build 2 separate static analyses (unbound variable +
name shadowing analysis) for a small language. In part 1 I focus mostly on
creating the separate algorithms. In part 2 I [combine both folds using
semigroups](/posts/combining_folds_using_semigroups) so all analyses happen in a
single tree traversal.

The code can be found in [this
repository](https://github.com/luc-tielen/playground/blob/combining_analyses/src/Main.hs).

Note: Sorry about the sound in first half of part 2, I accidentally disabled my
mic before streaming. It should still be possible to follow along though!
