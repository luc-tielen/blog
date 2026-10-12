---
title: "Announcement: new Eclair website!"
author: Luc Tielen
postDate: Jul 2, 2023
tags:
  - compilers
  - meta
---

I haven't been able to post as much this year due to work and other _things in
real life_ :tm:, but today I've got a big announcement: my programming language
Eclair has a [new website](https://eclair-lang.org), as well as a new
[Eclair Discord server](https://discord.gg/mC2arUrxKg)!

Right now it's in a MVP state, but more information will be added there over
time, and I think it's now ready for others to start using it.

## Next steps

Looking back on my previous [article on the state of Eclair in 2023](/posts/state_of_eclair_in_2023),
I want to spend the majority of the remaining time this year in Eclair on
finishing the "bootstrapping" process of the compiler and improving performance.

Bootstrapping will make it so the Eclair compiler becomes a single binary to
install (by linking directly with generated Eclair code, without any runtime
dependency on Soufflé). After this is done, I'll update the install instructions
on the website with the new / simpler approach.

On the performance side, I'm going to add another page to the Eclair website
where I'm planning to show charts of a set of performance benchmarks after every
merged pull request to the `main` branch. After that is done, it will be mostly
a matter of adding several compiler optimizations and improvements to the
runtime. (I actually have a list of about 40 ideas lying around that I can't
wait to start on :smile:.)

Have any comments or questions? Let me know on
[Twitter](https://twitter.com/luctielen) or on the new Discord server!
