---
title: Calling C++ from Haskell
author: Luc Tielen
date: Jan 12, 2021
tags:
  - haskell
  - c++
image: calling-cpp-from-haskell.jpg
---

In today's blogpost, I will show an approach for calling C++ code from Haskell.
This can be useful in case you need the extra performance from C++ or if there
is no library in the Haskell ecosystem that provides the functionality you need
(but there is a C++ library for it).

The code snippets in this article are kept "simple" (though I will assume some
C++ knowledge, since you will need to write some extra C++ code when binding to
Haskell). The examples are derived from my [souffle-haskell
library](https://github.com/luc-tielen/souffle-haskell.git), where I had to
write Haskell bindings to interact with the Soufflé datalog language.


## Binding to C++

If a library is meant to be used from many other languages, most of the time
it will provide a C API for doing so. C has a relatively small feature-set and a
well-defined, stable ABI (binary interface). On top of that, most languages
have good support for interacting with C via a foreign function interface (FFI),
making it straight-forward to interface with C.

However, for some libraries this is not the case. Soufflé for example provides
only a [C++ interface](https://souffle-lang.github.io/interface). C++ is a lot
bigger language compared to C, has no stable ABI between different versions /
compilers and contains many complex features such as overloadable functions and
operators, templates, destructors, ... This makes binding to it a lot more
complicated than C.

The trick is to write an extra layer between the existing Haskell and C++
code, that exposes functionality using the C ABI. Fortunately, C++ offers an
easy way to be compatible with the C ABI with the _extern_ keyword. The snippet
below defines a function "answer_to_everything" that can be called from C (and
thus also from other languages!):

```cpp
extern "C" {
  int answer_to_everything(bool arg);

  int answer_to_everything(bool arg) {
    return 42;
  }
}
```

When using extern, you can still use C++-specific functionality inside the body
of a function, but the signature of the function needs to be compatible with C.

If you wanted to, you could now start using this function in Haskell after
importing it with a so-called _foreign import_:

```haskell
foreign import ccall unsafe "answer_to_everything" answerToEverything
  :: CBool -> CInt
```

A foreign import is Haskell's way of importing C-compatible functions. Note
that unlike Haskell modules, you will need to import foreign functions on a
per-function basis. The syntax is as follows:

- _foreign import ccall_: We're importing a foreign function!
- _unsafe_: This tells the Haskell compiler that the foreign function will not
  call back into Haskell. This results in less overhead when crossing language
  boundaries but you should only use the keyword when this is really the case!
- _"answer_to_everything"_: The name of the function (as defined in C++)
- _answerToEverything_: The name of the function (in Haskell)
- Type signature: You need to manually add a type signature here, using the
  types defined in
  [Foreign.C.Types](https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-C-Types.html).
  Depending on if the function is pure or not, you also should wrap the result
  in IO. The Haskell compiler blindly trusts you as a programmer to provide the
  correct signature. If you make a mistake here, _bad things will happen_:tm:,
  so be sure to get it right.


### Memory management and objects

Now that we know how to cross the language border and use a simple C++
function from Haskell, we can focus on other more complicated concepts.

First of all, there's memory management. Haskell uses a a garbage collector to
clean up unused memory automatically. This is in contrast with C++, which is
much "closer to the metal". C++ gives you very fine-grained control of the
memory layout of an object and when exactly to allocate or free the memory of an
object. If we want to bind C++ to Haskell, **we have to make sure both languages
work well together**. One way to achieve this is by letting Haskell be in charge
and call into the C++ code. Objects used via the FFI need to be allocated on the
C++ heap, so they don't automatically get cleaned up once the object goes out of
scope (like stack-allocated objects would). After objects are no longer in use,
they need to be cleaned up again (also from the Haskell side).

The next issue are _the objects themselves_. The C ABI has no support for
objects or calling methods on those objects. This isn't a problem though, since
objects can be represented as an (opaque) C-like struct, together with functions
that perform actions with that struct. Each of the functions has the same
signature as the corresponding method of the object, except the function takes
an additional argument for the pointer to the object. (In C++ this is handled
implicitly for you with the _this_ pointer.) Constructors and destructors also
need to have their own separate functions in the FFI.

Taking all this into account, our FFI layer for managing a C++ object
could look like this:

```cpp
// Suppose we want to call into this object from Haskell:

class Example {
public:
  // Note: Constructor and destructor are auto-generated here!
  // We will have to add functions for calling these as well.
  bool do_stuff(int arg);
};

// We can define the following FFI layer:

// in ffi.h
extern "C" {
  typedef struct ffi_example;

  ffi_example* example_create();
  void example_destroy(ffi_example* object);
  bool example_do_stuff(ffi_example* object, int arg);
}

// in ffi.cpp
#include "ffi.h"

extern "C" {

ffi_example* example_create() {
  auto object = new Example();
  return reinterpret_cast<ffi_example*>(object);
}

void example_destroy(ffi_example* object) {
  auto example = reinterpret_cast<Example*>(object);
  delete example;
}

bool example_do_stuff(ffi_example* object, int arg) {
  auto example = reinterpret_cast<Example*>(object);
  return example->do_stuff(arg);
}

}
```

Now that we have the C++ part, we still need to write the Haskell part. To do
this, we use an API from the Haskell _base_ library for interacting with foreign
functions. It provides data types that have a 1-to-1 correspondence with their C
equivalents (for example _CInt_, _CBool_, ...). The
[Ptr](https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-Ptr.html#t:Ptr)
data type is used for managing pointers from Haskell. The _Ptr_ type uses a
phantom type variable as a type-level tag to detect at compile time if you use a
pointer of an unexpected type.

You may have noticed in the last C++ snippet that there was a manual call to
_delete_. This is frowned upon in C++ nowadays since it can be very easy to make
a mistake and forget to call _delete_ in certain code paths (for example if an
exception occurred). C++ has a solution for this in the form of so-called _smart
pointers_ but those are unusable in combination with the FFI. Haskell has a very
similar mechanism though, called [foreign
pointers](https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-ForeignPtr.html#t:ForeignPtr).
A foreign pointer is a pointer managed by the Haskell runtime / garbage
collector, coupled with a finalizer function for freeing up the memory used by
the pointer. When the foreign pointer goes out of scope and there are no more
references to it in the Haskell runtime, it will call the finalizer function.

Continuing with the _Example_ class from earlier, we can now make sure the
destructor is always called when the object goes out of scope:

```haskell
-- An empty data type, used as a type tag for the Ptr type
data Example

foreign import ccall unsafe "example_create" exampleCreate
  :: IO (Ptr Example)
-- Slightly different syntax, this gives us a function pointer to the
-- "example_destroy" function (needed below):
foreign import ccall unsafe "&example_destroy" exampleDestroy
  :: FunPtr (Ptr Example -> IO ())
foreign import ccall unsafe "example_do_stuff" exampleDoStuff
  :: Ptr Example -> CInt -> IO CBool

-- This initializes a Example object on the heap, that will be
-- cleaned up with "exampleDestroy" when it is no longer in use.
mkExample :: IO (ForeignPtr Example)
mkExample = do
  ptr <- exampleCreate
  newForeignPtr exampleDestroy ptr

main :: IO ()
main = do
  obj <- mkExample
  -- Now we can use the pointer by unwrapping it with "withForeignPtr":
  withForeignPtr obj $ \ptr -> do
    result <- exampleDoStuff ptr 1234
    print result  -- Process the result.
```

### Overloads

C++ has support for _overloadable functions_ (functions with the same name and
scope, but with different type signatures and function bodies). However, this is
again not supported in C and as such is not possible to do in the FFI.
If we want to be able to call these overloads, we have to wrap each variant with
a helper function that is exposed in the FFI. If we add an overload to our Example
class defined earlier, we can expose overloaded functions as follows:

```cpp
class Example {
public:
  bool do_stuff(int arg);
  bool do_stuff(float arg);  // <- This overload is new.
};


// in ffi.h
extern "C" {
  // Rest of code is same as before.

  // The FFI functions all need a unique name.
  bool example_do_stuff_int(ffi_example* object, int arg);
  bool example_do_stuff_float(ffi_example* object, float arg);
  // ... add other variants as needed.
}

// in ffi.cpp
#include "ffi.h"

extern "C" {
// Rest of code is same as before.

bool example_do_stuff_int(ffi_example* object, int arg) {
  auto example = reinterpret_cast<Example*>(object);
  return example->do_stuff(arg);  // calls the overload with int argument
}

bool example_do_stuff_float(ffi_example* object, float arg) {
  auto example = reinterpret_cast<Example*>(object);
  return example->do_stuff(arg);  // calls the overload with float argument
}

}
```

Finally, each variant has to be imported on the Haskell side:

```haskell
-- Rest of code is same as before.

foreign import ccall unsafe "example_do_stuff_int" exampleDoStuffInt
  :: Ptr Example -> CInt -> IO CBool
foreign import ccall unsafe "example_do_stuff_float" exampleDoStuffFloat
  :: Ptr Example -> CFloat -> IO CBool
```

### Loops

So far we focused on a single object or data type, but what about
multiple objects? In C++ for- or while-loops are used to iterate over
collections of data. Loops, like the other concepts mentioned before, will
require some massaging into a different format until we have something that can
be used from the FFI.

Let's update our previous example to have a collection of some sort:

```cpp
class Example {
private:
  // Example now contains a collection of data
  std::vector<int> values;

public:
  // To keep things simple, we initialize the vector to
  // a collection of 3 hardcoded values.
  Example() : values({1, 2, 3}) {}

  auto& get_values() const {
      return values;
  }
};


// We can now loop over the data:

Example example;
auto& values = example.get_values();

for (auto& value : values) {
  process_value(value);
}
```

The example makes use of a range-based for-loop, which was introduced in C++11.
Range-based loops are a form of syntactic sugar and can be rewritten using
C++ iterators as follows:

```cpp
// Note: "it" is short for iterator
for (auto it = values.begin(); it != values.end(); ++it) {
  auto& value = *it;
  process_value(value);
}
```

While we're at it, a standard for-loop is also sugar for a while-loop.
Let's do another transform:

```cpp
auto it = values.begin();
while (it != values.end()) {
  auto& value = *it;
  process_value(value);
  ++it;
}
```

This is starting to come close to something that we can use from the FFI, we
only need to write some functions for calling each of the steps of the loop
separately:

```cpp
auto it = ffi_iterator_create(values);
while (ffi_iterator_has_next(it)) {
  auto& value = ffi_iterator_next(it);
  std::cout << value << std::endl;
}
ffi_iterator_destroy(it);
```

Writing each of the helper functions is quite a bit of work, but is based on
ideas mentioned earlier in this blogpost:

```cpp
// in ffi.h

struct ffi_iterator;  // New struct for iterating over the collection.

ffi_iterator* ffi_iterator_create(ffi_example* obj);
void ffi_iterator_destroy(ffi_iterator* it);
bool ffi_iterator_has_next(ffi_iterator* it);
int ffi_iterator_next(ffi_iterator* it);


// in ffi.cpp

struct ffi_iterator {
  using iterator_t = std::vector<int>::iterator;

  // We need to keep track of what the iterator is currently pointing to,
  // as well as the end of the collection.
  iterator_t iterator;
  const iterator_t end;

  ffi_iterator(const iterator_t& begin_, const iterator_t& end_)
    : iterator(begin_)
    , end(end_) {}
};

ffi_iterator* ffi_iterator_create(ffi_example* obj) {
  // We get the collection out of the object,
  // and get the iterators pointing to beginning and end.
  auto example = reinterpret_cast<Example*>(obj);
  auto& values = example.get_values();
  return new ffi_iterator(values.begin(), values.end());
}

void ffi_iterator_destroy(ffi_iterator* it) {
  delete it;
}

bool ffi_iterator_has_next(ffi_iterator* it) {
  // There is a next value if the iterator is not pointing to the end.
  return it->iterator != it->end;
}

int ffi_iterator_next(ffi_iterator* it) {
  // Get the current element, then update iterator to next element.
  auto& value = *it->iterator;
  ++it->iterator;
  return value;
}
```

Once we have these functions, we can build our own loop in Haskell and process the values there:

```haskell
data ExampleIterator

foreign import ccall unsafe "ffi_iterator_create" ffiIteratorCreate
  :: Ptr Example -> IO (Ptr ExampleIterator)
foreign import ccall unsafe "&ffi_iterator_destroy" ffiIteratorDestroy
  :: FunPtr (Ptr ExampleIterator -> IO ())
foreign import ccall unsafe "ffi_iterator_has_next" ffiIteratorHasNext
  :: Ptr ExampleIterator -> IO CBool
foreign import ccall unsafe "ffi_iterator_next" ffiIteratorNext
  :: Ptr ExampleIterator -> IO CInt


mkExampleIterator :: Ptr Example -> IO (ForeignPtr ExampleIterator)
mkExampleIterator ptr =
  newForeignPtr ffiIteratorDestroy =<< ffiIteratorCreate ptr

-- Helper function for looping over the data and collecting the results on
-- the Haskell side.
collectValues :: Ptr ExampleIterator -> IO [CInt]
collectValues = go [] where
  go acc iterator = do
    CBool hasNext <- ffiIteratorHasNext iterator
    if hasNext == 1
      then do
        value <- ffiIteratorNext iterator
        go (value : acc) iterator
      else pure acc

main :: IO ()
main = do
  obj <- mkExample
  withForeignPtr obj $ \objPtr -> do
    iterator <- mkExampleIterator objPtr
    withForeignPtr iterator $ \iteratorPtr -> do
      values <- collectValues iteratorPtr
      -- Now you can process the values like you would
      -- for any other Haskell value.
      print $ map (+1) values
```


### Compiler flags

That's it for the code part, but we're not finished quite yet! Because we are
using 2 languages, we need to provide compilation flags for both Haskell and C++.
This can be done in your project's package.yaml / cabal file / stack.yaml.

For example, in the souffle-haskell library, the package.yaml contains
additional configuration instructing the C++ compiler to look for files
in the _cbits_ directory and use the given C++ compiler flags:

```yaml
cxx-sources: cbits/*.cpp
include-dirs:
  - cbits
  - cbits/souffle
cxx-options:
  - -std=c++17
  # Provide other C++ compilation flags here as necessary.
```

Note that you should not use _cpp-options_ in the yaml file. This refers to the
Haskell C-preprocessor config and does not refer to C++-options like I initially
assumed.

Depending on the C++ code you are binding to, you may also need to add
specific compiler flags for certain C++ compilers or operating systems.
In package.yaml this can be done as follows:

```yaml
when:
  - condition: os(darwin)
    extra-libraries: c++
```

## Conclusion

This post turned out _much_ larger than I initially expected! I guess this
shows that binding to C++ is more work than C, but can definitely be done.
The code in some snippets is simplified to avoid making this blog post even
longer. If you are interested in what a "real" example of this approach looks
like, check out my [souffle-haskell library](https://github.com/luc-tielen/souffle-haskell).
You can find the most relevant files for binding to C++
[here](https://github.com/luc-tielen/souffle-haskell/blob/master/cbits/souffle.cpp),
[here](https://github.com/luc-tielen/souffle-haskell/blob/master/lib/Language/Souffle/Internal/Bindings.hs)
and [here](https://github.com/luc-tielen/souffle-haskell/blob/master/lib/Language/Souffle/Internal.hs).

One final thing to mention is that this approach isn't specific to Haskell, you
would only need to swap out the Haskell code with your language of choice to
bind to the underlying C++ code.

If you are interested in more content like this, follow me on
[Twitter](https://twitter.com/luctielen) for blogpost updates or subscribe to
the [RSS feed](https://luctielen.com/atom.xml).

