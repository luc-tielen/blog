---
title: Calling Zig from Haskell
author: Luc Tielen
postDate: Mar 12, 2022
tags:
  - zig
  - haskell
---

In today's article, I will show how you can interface Zig code with Haskell. Zig
is a low level language that aims to be a modern replacement for C (minus all
the footguns). Zig has great C interop and because of this, calling into Zig
from Haskell is almost as easy as calling into C code.

## Project setup

Since our example will contain both Haskell and Zig code, we will need to setup
our project for both languages. From here on, I will assume you have both
`cabal` (a Haskell project/ build tool) and `zig` (the Zig compiler) installed.
First, let's run some commands to initialize a Haskell project using `cabal`:

```bash
$ mkdir language-interop
$ cd language-interop
$ cabal init --minimal --exe
```

This will generate the following directories and files:

```bash
$ tree
.
├── app
│   └── Main.hs
├── CHANGELOG.md
└── language-interop.cabal
```

Now that we have the initial Haskell project skeleton, let's setup our Zig
library called "example". We will setup our Zig project in a "cbits" folder
inside the Haskell project (a convention for storing C, C++ (and Zig?) files in
a Haskell project).

Here are the commands to setup the Zig library:

```bash
$ mkdir -p cbits/example
$ cd cbits/example
$ zig init-lib
```

We're not done yet though. We need to modify the `build.zig` file to take care
of a few things:

1. We need to link with the C library (`libc`) since we will be using the C
   allocator from Zig later on,
2. In debug mode: bundle the "compiler runtime" to avoid linker errors
   related to safety checks generated by Zig (e.g. stack protection),
3. In release mode: disable the safety checks in Zig.

This is what the modified `build.zig` should look like:

```zig
const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("example", "src/main.zig");
    lib.setBuildMode(mode);
    lib.linkLibC();  // Needed for linking with libc
    switch (mode) {
        // This includes the compiler runtime (Zig runtime checks):
        .Debug, .ReleaseSafe => lib.bundle_compiler_rt = true,
        // This turns off the checks completely for release mode:
        .ReleaseFast, .ReleaseSmall => lib.disable_stack_probing = true,
    }
    lib.install();

    const main_tests = b.addTest("src/main.zig");
    // Depending on how you set up your Zig code,
    // you may need to link with libc in your tests as well:
    main_tests.linkLibC();
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
}
```

The initial configuration for Zig is finished. Confirm everything works
correctly by running `zig build` in the `cbits/example/` directory.
If it doesn't show any errors, then the Zig setup is finished and we can move on
to the Haskell setup! For this, we need to update the cabal file to take the Zig
code into account by adding the `extra-lib-dirs` and `extra-libraries` info:

```cabal
-- other cabal config ...

executable zig-interop
  -- executable config ...

  -- The folder where cabal will look for .a files:
  extra-lib-dirs: cbits/example/zig-out/lib/
  -- The name of the libraries we want to link with.
  -- "example" means we want to link with "libexample.a".
  extra-libraries: example
```

And we're all set! If you followed along with all the steps, your entire project
structure should look as follows:

```bash
$ tree
.
├── app
│   └── Main.hs
├── cbits
│   └── example
│       ├── build.zig
│       └── src
│           └── main.zig
├── CHANGELOG.md
└── zig-interop.cabal
```

## Interfacing Haskell with Zig

Our project is all ready, so we can start calling into Zig code! Let's
start with a simple top level function `add` in `main.zig` that adds 2 numbers
together:

```zig
// Note: Export is needed to make the function available outside of Zig
// The C calling convention is used by default for exported functions,
// but it's better to be explicit about it (specified by "callconv").
export fn add(a: i32, b: i32) callconv(.C) i32 {
  return a + b;
}
```

Before we change the Haskell code, we need to re-compile the code using `zig
build` again. Quick tip: use a Makefile or a script to automate all these small
steps. Once the Zig library has been built, we need to introduce a foreign
import in `Main.hs`:

```haskell
foreign import ccall unsafe "add" foreignAdd
  :: CInt -> CInt -> CInt
```

This import is needed to be able to access the "add" function from Zig in
Haskell as "foreignAdd". For a detailed explanation of all the keywords in the
import, check out my previous blogpost on
[Calling C++ from Haskell](../calling_cpp_from_haskell). After importing the
Zig function, we can call it like any other Haskell code:

```haskell
main :: IO ()
main = print $ foreignAdd 3 4  -- prints 7 to the console
```

This snippet prints out `7` to the console. Great! Now for something a little
more complicated: managing a Zig struct from Haskell and calling it's member
functions. In order to do that, we first need an example struct, so let's define
one:

```zig
const Allocator = std.mem.Allocator;

const Example = struct {
    field: i32,

    // Note: normally the convention to initialize a struct is:
    // fn init() Example {
    //     return Example{ .field = 42 };
    // }
    //
    // This change is done so we can use any allocator (C allocator
    // in actual code, and the testing allocator in test code) to
    // allocate memory for our struct.
    fn create(allocator: Allocator) *Example {
        // If we fail to allocate, there is no good way to recover
        // in this case, so we error with a panic.
        const obj = allocator.create(Example)
          catch std.debug.panic("Failed to allocate Example struct", .{});
        obj.field = 42;
        return obj;
    }

    fn deinit(self: *Example) void {
        // No de-initialization needed for this simple struct
        _ = self;
    }

    fn do_stuff(self: *Example, arg: i32) bool {
        return self.field == arg;
    }
};
```

The final thing we need to do on the Zig side is to write some free
functions that wrap the functionality of the struct. The exported functions form
the API that the Haskell code will call into:

```zig
export fn example_create() callconv(.C) *Example {
    return Example.create(std.heap.c_allocator);
}

export fn example_destroy(ptr: ?*Example) callconv(.C) void {
    std.debug.assert(ptr != null);

    const obj = ptr.?;
    obj.deinit();
    std.heap.c_allocator.destroy(obj);
}

export fn example_do_stuff(ptr: ?*Example, arg: i32) callconv(.C) bool {
    std.debug.assert(ptr != null);

    return ptr.?.do_stuff(arg);
}
```

In the snippet above we have one function for each of the struct functions. Note
that `example_destroy` and `example_do_stuff` take an optional pointer to the
struct, since the Haskell code is in control of calling these functions and
could pass in _any_ pointer (that could be `null`).

Since the allocator is passed to the `Example.create` function (a common pattern
in Zig), this gives us flexibility regarding memory allocations. Here, the
exported functions make use of the `std.heap.c_allocator` which is a fast
allocator that uses the `malloc` and `free` functions from `libc` under the
hood. At the same time, this setup allows writing Zig unit tests that use the
testing allocator to check for potential memory leaks.

Now it's time to bind to our Zig struct from Haskell:

```haskell
foreign import ccall "example_create" createExample
  :: IO (Ptr Example)
foreign import ccall "&example_destroy" destroyExample
  :: FunPtr (Ptr Example -> IO ())
foreign import ccall "example_do_stuff" doStuffExample
  :: Ptr Example -> CInt -> IO CBool

data Example

mkExample :: IO (ForeignPtr Example)
mkExample = mask_ $ do
  -- NOTE: mask_ is needed to avoid leaking memory between
  -- allocating the struct and wrapping the `Ptr` in a `ForeignPtr`.
  ptr <- createExample
  newForeignPtr destroyExample ptr

main :: IO ()
main = do
  ptr <- mkExample
  withForeignPtr ptr $ \p -> do
    result <- doStuffExample p 10
    print result
```

On the Haskell side, we need to create an empty datatype `Example`, that is used
as a type-level tag for keeping track of the type a pointer is pointing to. This
ensures at compile time that pointers of different types do not get mixed up
(that could potentially lead to really weird and hard to find bugs).

In Haskell, we don't have `defer` like in Zig to automatically cleanup memory
that is no longer needed, but instead it provides a `ForeignPtr` type that
automatically frees a pointer when it is no longer referenced. For each of the
imported function calls to Zig we need to use `withForeignPtr` to get access to
the underlying pointer (the `Ptr` type). Finally, once we have access to the
`Ptr`, we can call the Zig code like any other function in Haskell.

## Conclusion

In this post I showed how it is possible to interface Zig with Haskell.
Integrating the two languages with each other is not that much harder compared
to calling C from Haskell thanks to the great language interop support provided
by Zig.

One thing to mention is that this approach isn't specific to Haskell,
you would only need to swap out the Haskell code with your language of choice to
bind to the underlying Zig code.

If you have any questions or thoughts about this article, let me know on
[Twitter](https://twitter.com/luctielen). If you want to play around with the
code from this post, it can be found
[here](https://github.com/luc-tielen/playground/tree/calling-zig-from-haskell).
