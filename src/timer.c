#include "timer.h"
#include <stdatomic.h>

#if _WIN32
#include <windows.h>
#else
#include <time.h>
#endif

FILE* timer_output;
double timer_freq;
mtx_t timer_mutex;

atomic_int timer_entry_count;

// done regardless of the profiler running just to be able to query time in general
void timer_init() {
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

void timer_end(uint64_t start, const char* fmt, ...) {
	if (timer_output == NULL) return;
	
	uint64_t end = timer_now();
	int64_t elapsed_in_seconds = (int64_t) (((end - start) * timer_freq) * 1000000.0);
	int64_t start_in_seconds = (int64_t) ((start * timer_freq) * 1000000.0);
	
	if (elapsed_in_seconds > 1) {
		mtx_lock(&timer_mutex);
		int i = timer_entry_count++;
		
#if _WIN32
		uint32_t tid = GetCurrentThreadId();
#else
		uint32_t tid = 1;
#endif
		
		char name[256];
		
		va_list ap;
		va_start(ap, fmt);
		vsnprintf(name, sizeof(name), fmt, ap);
		name[sizeof(name) - 1] = '\0';
		va_end(ap);
		
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
				elapsed_in_seconds,
				name,
				tid,
				start_in_seconds);
		
		mtx_unlock(&timer_mutex);
	}
}
