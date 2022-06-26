#pragma once

#include "ext/threads.h"
#include <common.h>

// Opens up a file and emits the profiler output to it, it's a JSON format compatible
// with chrome://tracing and speedscope
void timer_open(const char* path);

// Close the profiler file
void timer_close(void);

// Reports a region of time in the profiler file
void timer_end(uint64_t start, const char* fmt, ...);