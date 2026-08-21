////////////////////////////////////////////
// Profiler
////////////////////////////////////////////
// The callbacks are global and a user may even hook in using the cuikperf_start
// and cuikperf_stop, or by using CUIK_TIMED_BLOCK
#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// DONT USE THIS :(((
void cuik_init_timer_system(void);

void cuikperf_start(const char* path);
void cuikperf_stop(void);
bool cuikperf_is_active(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
uint64_t cuik_time_in_nanos(void);

void cuikperf_thread_start(void);
void cuikperf_thread_stop(void);

// Reports a region of time to the profiler callback
void cuikperf_region_start(const char* fmt, const char* extra);
void cuikperf_region_start2(const char* fmt, size_t extra_len, const char* extra);
void cuikperf_region_end(void);

// Special
void cuikperf_region_start3(const char* label, int tid, uint64_t t);
void cuikperf_region_end3(int tid, uint64_t t);

uint64_t cuik_special_time(uint64_t x);

// Usage:
// CUIK_TIMED_BLOCK("Beans %d", 5) {
//   ...
// }
#define CUIK_TIMED_BLOCK(label) for (uint64_t __i = (cuikperf_region_start(label, NULL), 0); __i < 1; __i++, cuikperf_region_end())
#define CUIK_TIMED_BLOCK_ARGS(label, extra) for (uint64_t __i = (cuikperf_region_start(label, extra), 0); __i < 1; __i++, cuikperf_region_end())
