---
title: Lessons learned compiling to WebAssembly
author: Luc Tielen
postDate: Dec 4, 2022
tags:
  - wasm
  - llvm
image: eclair_to_webassembly.png
---

In today's post I list some of the lessons I learned when compiling to
WebAssembly using LLVM for [Eclair Datalog](https://github.com/luc-tielen/eclair-lang).
It is true that LLVM has direct support for WebAssembly, but as you will see,
for some things some extra effort is needed to get the language fully working
on top of WASM.

## There is more than one WebAssembly target

LLVM can compile to multiple different WebAssembly targets. There's
`wasm32-unknown-unknown` (also referenced as just `wasm32` sometimes),
`wasm32-wasi`, `wasm64-unknown-unknown`, ...

For Eclair, I ended up choosing the minimal `wasm32`. It's a compilation target
that is very bare-bones which makes it portable and lightweight to run (tiny
executables!), but it does have some implications as we will see in the next
section.

Compiling to WASM is straight-forward using LLVM. Assuming your language can
already compile to LLVM, you can use the following commands to compile to
WebAssembly:

```bash
# This assumes the language already generated program.ll
# Note: llc is also possible instead of clang
$ clang -Oz --target=wasm32 -nostdlib -c -o program.o program.ll
$ wasm-ld --no-entry --import-memory -o program program.o
```

## System-calls require extra work

LLVM gives you low-level instructions to do operations such as addition,
multiplication, ... But most languages provide more high-level functionality
than just these basic instructions. Think of allocating memory, network access
via sockets, ... In traditional native environments, this is made possible via
so-called system-calls (or `syscalls`) that interact with the OS-kernel.
WebAssembly has no direct access to the kernel, so if you need the behavior of
a syscall, you might have to implement it yourself and expose it to the WASM
runtime!

If your language relies on many different syscalls, it might be best to target
the `wasm32-wasi` environment. This compilation target includes a
WASM-counterpart for many of the traditional syscalls, but it comes at the cost
of larger executables.

Luckily though, Eclair relies on only a handful of syscalls because of its
pure and declarative nature. Here's the full list:

1. `malloc`
2. `free`
3. `memcpy`
4. `memset`
5. `memcmp`

Since I chose the `wasm32` target, I had to provide implementations for all of
these. I will briefly go over how I did this in the next sections.

### malloc and free

The `malloc` and `free` syscalls are used for allocating memory in an
application. Writing good, performant allocators is not an easy task, so I
decided to depend on the open source [walloc](https://github.com/wingo/walloc)
library for implementations of `malloc` and `free`. Other allocators
are possible too, as long as they support the WASM target you are trying to
support!

The compile commands earlier change a little when you add an allocator in the mix.
If you want to use `walloc`, this becomes:

```bash
# This assumes the language already generated program.ll
$ clang -Oz --target=wasm32 -nostdlib -c -o program.o program.ll
$ clang -DNDEBUG -Oz --target=wasm32 -nostdlib -c -o walloc.o walloc.c
$ wasm-ld --no-entry --import-memory -o program program.o walloc.o
```

### memcpy and memset

`memcpy` and `memset` were the easiest syscalls. If you pass `-mbulk-memory` to
`llc` or `clang`, you get optimized implementations for these two functions for
free. Nice!

### memcmp

`memcmp` is the last of the five syscalls, used to compare two chunks of memory
against each other (e.g. when comparing strings). Strangely enough,
`-mbulk-memory` doesn't give you an implementation for `memcmp`, so this syscall
I really had to write myself. The LLVM snippet below is a possible
implementation that uses some loop unrolling (so bytes are compared in groups of
eight at a time, not one-by-one):

```llvm
define external ccc i32 @memcmp_wasm32(i8* %array1, i8* %array2, i64 %byte_count) {
start:
  ;; Calculate how many i64 fit in the array.
  %0 = udiv i64 %byte_count, 8
  %1 = and i64 %byte_count, 7
  %2 = bitcast i8* %array1 to i64*
  %3 = bitcast i8* %array2 to i64*
  br label %for_begin_0
for_begin_0:
  %4 = phi i64 [0, %start], [%11, %end_if_0]
  %5 = icmp ult i64 %4, %0
  br i1 %5, label %for_body_0, label %for_end_0
for_body_0:
  ;; Compare i64 values, return 1 if they are not equal
  %6 = getelementptr i64, i64* %2, i64 %4
  %7 = getelementptr i64, i64* %3, i64 %4
  %8 = load i64, i64* %6
  %9 = load i64, i64* %7
  %10 = icmp ne i64 %8, %9
  br i1 %10, label %if_0, label %end_if_0
if_0:
  ret i32 1
end_if_0:
  %11 = add i64 1, %4
  br label %for_begin_0
for_end_0:
  %12 = mul i64 %0, 8
  br label %for_begin_1
for_begin_1:
  %13 = phi i64 [0, %for_end_0], [%21, %end_if_1]
  %14 = icmp ult i64 %13, %1
  br i1 %14, label %for_body_1, label %for_end_1
for_body_1:
  ;; The last few elements of the array needs to be checked byte-per-byte
  %15 = add i64 %13, %12
  %16 = getelementptr i8, i8* %array1, i64 %15
  %17 = getelementptr i8, i8* %array2, i64 %15
  %18 = load i8, i8* %16
  %19 = load i8, i8* %17
  %20 = icmp ne i8 %18, %19
  br i1 %20, label %if_1, label %end_if_1
if_1:
  ret i32 1
end_if_1:
  %21 = add i64 1, %13
  br label %for_begin_1
for_end_1:
  ret i32 0
}
```

One final thing to note here is that this `memcmp` function is slightly different
than the usual implementation, since it returns `0` if the arrays are equal,
and `1` otherwise. That's the freedom you get if you need to build your own
syscall. :wink:

## Allocate enough memory before running the code

WASM is different compared to most language runtimes in that you need to specify
how much memory you actually allow to be used. Here's how this is usually done:

```javascript
// This tells WASM to start with 10 pages = 640kB of memory, allow up to 6400kB
const wasmCode = ...
const memory = new WebAssembly.Memory({ initial: 10, maximum: 100 });
const { instance: wasmInstance } = await WebAssembly.instantiateStreaming(
  wasmCode,
  { env: { memory } }
);
```

This has a big impact on your language's runtime code! If we take a look at a
traditional snippet of C code using `malloc`:

```c
void func() {
  uint8_t* memory = (uint8_t*) malloc(10000);
  // code that uses the allocated memory ...
}
```

If `malloc` fails to allocate enough memory, it will return a `NULL` pointer.
So be sure to always allocate enough memory to the WASM runtime, and check that
the memory allocation succeeded.

## Invalid programs are harder to detect

While we're on the topic of failing programs, bugs can be harder to detect in
WASM because there's not a clear segmentation fault or crash like in C. The code
will try to continue running, which might leave the entire runtime in a corrupt
state.

On top of that, the previously mentioned `NULL`-pointer is actually a valid
address in WASM, which might lead to data being written to the wrong places.

As you can imagine, these kinds of errors can be really hard to track down..
:sweat_smile:

## Thoroughly test your language before porting

Before you try to port your language's runtime to WASM, it should already be
well-tested for another platform. This will make it much easier to port over
and add new code, since you can rely on the behavior of the existing code.
Potential issues should only pop up in the WASM-specific portions of the runtime.
And ofcourse add tests for the new WASM runtime too!

## Make sure you are compiling for the correct target

Ok, this one should really be obvious, but while I was adding WASM support to my
language I had a really subtle bug in my language's runtime. I didn't use libLLVMs
`LLVMABISizeOfType` function correctly in one place (I accidentally used the host
CPU architecture datalayout instead of the WASM datalayout).

Because of this, make sure you always use the correct datalayout, and be
consistent. Also, according to this
[document about improving LLVM performance](https://llvm.org/docs/Frontend/PerformanceTips.html#the-basics),
you should be setting the target datalayout for performance reasons anyway!

## Create language-specific bindings for language interopability

All communication with WebAssembly needs to be done via the earlier mentioned
`WebAssembly.Memory`. This is a contiguous block of memory (a byte-array), that
can be used to send data back and forth (e.g. numbers, strings, ...).

You have full liberty to implement this data serialization any way you want, but
you probably don't want to expose these details to end-users of your language.
For this reason, you should design a Javascript/Typescript library that does
all the (de-)serialization.

For Eclair I created the
[eclair-wasm-bindings package](https://www.npmjs.com/package/eclair-wasm-bindings)
that does exactly this.

## Conclusion

In this article I highlighted some things to watch out for when adding WASM
support to an existing language. LLVM does most of the work, but there are some
final minor things that need to handled to ensure a language can run smoothly on
WebAssembly. Overall though, it was a really smooth experience.

If you have any questions or thoughts about this article, let me know on
[Twitter](https://twitter.com/luctielen).
