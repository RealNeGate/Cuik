#include "timer.h"
#include <stdarg.h>
#include <stdatomic.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <time.h>
#include <unistd.h>
#endif

FILE* timer_output;
mtx_t timer_mutex;

atomic_int timer_entry_count;

// just to organize the JSON timing stuff a bit better
static thread_local bool is_main_thread;

// done regardless of the profiler running just to be able to query time in general
#if _WIN32
double timer_freq;

void timer_init() {
	is_main_thread = true;
	
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    timer_freq = 1.0 / (double)freq.QuadPart;
}

void timer_open(const char* path) {
	assert(timer_output == NULL);
	
	mtx_init(&timer_mutex, mtx_plain);
	timer_output = fopen(path, "wb");
	fprintf(timer_output, "{\"otherData\": {},\"traceEvents\":[");
}

void timer_close() {
	if (timer_output == NULL) return;
	
	fprintf(timer_output, "]}");
	fclose(timer_output);
}

uint64_t timer_now() {
    LARGE_INTEGER t;
    QueryPerformanceCounter(&t);
    return t.QuadPart;
}
#else
void timer_init() {}

void timer_open(const char* path) {
	assert(timer_output == NULL);
	
	mtx_init(&timer_mutex, mtx_plain);
	timer_output = fopen(path, "wb");
	fprintf(timer_output, "{\"otherData\": {},\"traceEvents\":[");
}

void timer_close() {
	if (timer_output == NULL) return;
	
	fprintf(timer_output, "]}");
	fclose(timer_output);
}

uint64_t timer_now() {
    struct timespec tms;
    clock_gettime(CLOCK_MONOTONIC, &tms);
	
	uint64_t micro = tms.tv_sec * 1000000;
    micro += tms.tv_nsec / 1000;
    return micro;
}
#endif

void timer_end(uint64_t start, const char* fmt, ...) {
	if (timer_output == NULL) return;
	
#if _WIN32
	uint64_t end = timer_now();
	int64_t elapsed_in_microseconds = (int64_t) (((end - start) * timer_freq) * 1000000.0);
	int64_t start_in_microseconds = (int64_t) ((start * timer_freq) * 1000000.0);
#else
	int64_t elapsed_in_microseconds = timer_now() - start;
	int64_t start_in_microseconds = start;
#endif
	
	if (elapsed_in_microseconds > 1) {
		mtx_lock(&timer_mutex);
		int i = timer_entry_count++;
		
#if _WIN32
		uint32_t tid = GetCurrentThreadId();
#else
		uint32_t tid = pthread_self();
#endif
		
		char name[256];
		
		va_list ap;
		va_start(ap, fmt);
		int formatted_len = vsnprintf(name, sizeof(name), fmt, ap);
		name[sizeof(name) - 1] = '\0';
		va_end(ap);
		
		if (formatted_len < 0 || formatted_len >= sizeof(name)) {
			fprintf(stderr, "error: buffer overflow on sprintf_s!\n");
			abort();
		}
		
		fprintf(timer_output,
				"%c{\n"
				"\"cat\":\"function\",\n"
				"\"dur\":%lld,\n"
				"\"name\":\"%s\",\n"
				"\"ph\":\"X\",\n"
				"\"pid\":0,\n"
				"\"tid\": %u,\n"
				"\"ts\": %lld\n"
				"}",
				
				i ? ',' : ' ',
				(long long)elapsed_in_microseconds,
				name,
				is_main_thread ? 1 : tid,
				(long long)start_in_microseconds);
		
		mtx_unlock(&timer_mutex);
	}
}
