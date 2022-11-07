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

typedef struct Cuik_IProfiler {
    void (*start)(void* user_data);
    void (*stop)(void* user_data);

    void (*begin_plot)(void* user_data, uint64_t nanos, const char* label);
    void (*end_plot)(void* user_data, uint64_t nanos);
} Cuik_IProfiler;

// lock_on_plot is true if the profiler->plot function shall not be called on multiple threads at
// the same time, make this false whenever plot has internal synchronization or is single threaded.
//
// returns the allocated userdata, call cuikperf_start to start making plots
void* cuikperf_init(size_t ud_size, const Cuik_IProfiler* profiler, bool lock_on_plot);
void cuikperf_start(void);
void cuikperf_stop(void);
bool cuikperf_is_active(void);

// the absolute values here don't have to mean anything, it's just about being able
// to measure between two points.
uint64_t cuik_time_in_nanos(void);

// Reports a region of time to the profiler callback
void cuikperf_region_start(uint64_t now, const char* fmt, ...);
void cuikperf_region_end(void);

// Usage:
// CUIK_TIMED_BLOCK("Beans %d", 5) {
//   ...
// }
#define CUIK_TIMED_BLOCK(...) for (uint64_t __i = (cuikperf_region_start(cuik_time_in_nanos(), __VA_ARGS__), 0); __i < 1; __i++, cuikperf_region_end())
