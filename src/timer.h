#pragma once

#if _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

#include <stdio.h>
#include "ext/threads.h"

extern FILE* timer__output;
extern int timer__entry_count;
extern double timer__freq;
extern mtx_t timer__mutex;

// done regardless of the profiler running just to be able to query time in general
inline static void timer__init() {
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    timer__freq = 1.0 / (double)freq.QuadPart;
}

inline static void timer__open(const char* path) {
	assert(timer__output == NULL);
	
	mtx_init(&timer__mutex, mtx_plain);
	timer__output = fopen(path, "wb");
	fprintf(timer__output, "{\"otherData\": {},\"traceEvents\":[");
}

inline static void timer__close() {
	if (timer__output == NULL) return;
	
	fprintf(timer__output, "]}");
	fclose(timer__output);
}

inline static uint64_t timer__now() {
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    return t.QuadPart;
}

// Magic amirite
inline static void timer__end(const char* name, uint64_t start) {
	if (timer__output == NULL) return;
	
	uint64_t end = timer__now();
	double elapsed_in_seconds = (end - start) * timer__freq;
	if (elapsed_in_seconds < 0.000001f) return;
	
	mtx_lock(&timer__mutex);
	double start_in_seconds = start * timer__freq;
	
	int i = timer__entry_count++;
	
#if 1
	uintptr_t tid = (uintptr_t)thrd_current();
	tid ^= (tid >> 32);
	tid &= 0xFFFFFFFF;
	
	fprintf(timer__output,
			"%s"
			"{\n"
			"\"cat\":\"function\",\n"
			"\"dur\":%lld,\n"
			"\"name\":\"%s\",\n"
			"\"ph\":\"X\",\n"
			"\"pid\":0,\n"
			"\"tid\": %d,\n"
			"\"ts\": %lld\n"
			"}\n",
			
			i == 0 ? "\n" : ",\n",
			(long long)(elapsed_in_seconds * 1000000.0),
			name,
			(uint32_t)tid,
			(long long)(start_in_seconds * 1000000.0));
#else
	printf("%s took %.03f seconds\n", name, elapsed_in_seconds);
#endif
	
	mtx_unlock(&timer__mutex);
}

// Usage:
// timed_block("Beans") {
//   ...
// }
#define timed_block(name) for (uint64_t __t1 = timer__now(), __i = 0; __i < 1; __i++, timer__end(name, __t1))
