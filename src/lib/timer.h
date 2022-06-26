#pragma once

#include "ext/threads.h"
#include <common.h>

// regardless of if you want to profile code or not, you'll need to initialize
// the timer be able to use timer_now()
void timer_init(void);

// Opens up a file and emits the profiler output to it, it's a JSON format compatible
// with chrome://tracing and speedscope
void timer_open(const char* path);

// Close the profiler file
void timer_close(void);

// Reports a region of time in the profiler file
void timer_end(uint64_t start, const char* fmt, ...);

// Time in "ticks"
//   seconds = ticks * timer_freq
uint64_t timer_now();

// Usage:
// timed_block("Beans %d", 5) {
//   ...
// }
#define timed_block(...) for (uint64_t __t1 = timer_now(), __i = 0; __i < 1; __i++, timer_end(__t1, __VA_ARGS__))
