# Cuik (pronounced 'Quick')

<img src="logo/cuik.png" width="256px"/>

**warning: unfinished and buggy... also it doesn't compile on Linux or OSX currently, i'll get around to it**

The plan is a modern C11 compiler which can mostly work with Clang, GCC, and MSVC while also introducing some new ideas.

## Why write a C compiler?
* To improve the compile times on debug builds without sacrificing features like I would with TCC.
* To test out my upcoming (and currently private) compiler backend.
* To add some extensions to improve the workflow of C programmers.
* Because I can.

## How to install?
Mac & Linux aren't ready yet but i'll be moving forward to those platforms in the upcoming months

You'll need MSVC installed on windows (i use link.exe for now... sorry, i'll write a linker soon enough)

Just take a C compiler and compile the `compile.c` file and run it

### Options (define these using -D or /D depending on the compiler):
* `RELEASE_BUILD` for optimized builds.
* `ONLY_LIBRARY` will not compile the active driver, just the libCuik library
* `NO_TB` when compiling libCuik and don't wanna use TB.

### Compile script CLI
* passing a filepath into the compile executable will redirect libCuik's output location
* -test will run the tests
* -self1 will do type checking for all of Cuik
* -self2 will do IR generation for all of Cuik
* -self3 will do object files for all of Cuik
* -self4 just compiles Cuik with Cuik

## How am I doing?
mostly aight, thanks for asking.


## What are some cool new things?
One of my favorite new features are the live compiler (essentially an offline Godbolt but within a terminal) and the out of order declarations.

## What architectures will you support?
I'll be starting with x64 but I plan on having Aarch64 support soon enough, it's mostly a matter of user-demand what other platforms I add though I probably won't be supporting anything with a segmented address space because I don't want to implement it.


## What OSes will you support?
Ideally the essentials like Windows, Linux and MacOS but currently MacOS isn't setup and Linux has basic support (no live compiler)


## Will it have optimizations?
It will eventually have a smart but non-aggressive optimizer but that's still a work in progress


## What C extensions will you have?
I'll be supporting all the normal extensions such as:
- [x] pragma once
- [ ] builtin bitmath (popcount, ffs, clz, ctz, etc)
- [ ] __builtin_trap, __builtin_expect
- [ ] x86 SIMD intrinsics
- [x] typeof
- [ ] case ranges
- [ ] computed goto

And some possibly novel extensions such as:
- [x] Out of order functions
- [ ] [Tagged unions](https://gist.github.com/RealNeGate/94a3074dd4e6d29ee3170f4a70c3dad2)


## What's left?
It can currently compile programs using the subset of C it currently supports but it's still missing some essential details before it compiles any basic C program such as:
* More tests
* Bug fixes
* Self hosting
* Thread local
* Atomics (halfway there)
* MSVC extensions
* GNU extensions
* Proper Mac/Linux support

## File structure
In case you care to look around here's what the folders mean:
```
# libCuik itself
lib/
include/

# and it's dependencies (TB binaries go here)
deps/

# these are the programs that actually call into libCuik
drivers/

# not ciabatta but the freestanding includes like stddef.h along with some helpers
crt/

# random crap
logo/
tests/
```

## PS
If you're wondering about the tinybackend static library, you can recompile and place the binaries there, here's the project (i'll fix up the build process soon :P)
https://github.com/RealNeGate/tilde-backend
