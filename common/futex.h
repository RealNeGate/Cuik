#pragma once

#include <stdint.h>

#ifdef __APPLE__
typedef _Atomic int32_t Futex;
typedef int32_t FutexV;
#else
typedef _Atomic uint64_t Futex;
typedef uint64_t FutexV;
#endif

void futex_wait(Futex* f, FutexV val); // leaves if *f != val
void futex_wait_eq(Futex* f, FutexV val); // leaves if *f == val
void futex_signal(Futex* f);
void futex_broadcast(Futex* f);
