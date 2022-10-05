# Cuik (pronounced 'Quick')

<img src="logo/cuik.svg" width="256px"/>

**warning: unfinished and buggy... also it doesn't compile on Linux or OSX currently, i'll get around to it**

The plan is a modern C11 compiler which can mostly work with Clang, GCC, and MSVC while also introducing some new ideas.

## Why write a C compiler?
* To improve the compile times on debug builds without sacrificing features like I would with TCC.
* To test out my upcoming (and currently private) compiler backend.
* To add some extensions to improve the workflow of C programmers.
* Because I can.

## How to install?
Mac & Linux aren't ready yet but i'll be moving forward to those platforms in the upcoming months

You will need Git, Python, a C compiler (not msvc but gcc and clang should work fine) and on windows you'll need MSVC (i use link.exe for now... sorry, i'll remove the linker dependency soon)

```
git clone https://github.com/RealNeGate/Cuik
cd Cuik/main
python build.py
```

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
# this is where the main driver goes (it does "mostly" normal CC command line interactions)
main/

# Actual library (can be used separate of the main driver)
libCuik/

# not [ciabatta](https://github.com/flysand7/ciabatta) but the freestanding includes like stddef.h along with some helpers
crt/

# random crap
logo/
tests/
```
