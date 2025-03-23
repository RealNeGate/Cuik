#pragma once

#include <stdint.h>

#ifdef __APPLE__
typedef _Atomic int32_t Futex;

void futex_wait(Futex* f, int32_t val); // leaves if *f != val
void futex_wait_eq(Futex* f, int32_t val); // leaves if *f == val
#else
typedef _Atomic int64_t Futex;

void futex_wait(Futex* f, int64_t val); // leaves if *f != val
void futex_wait_eq(Futex* f, int64_t val); // leaves if *f == val
#endif

void futex_signal(Futex* f);
void futex_broadcast(Futex* f);
