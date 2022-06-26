#include <common.h>
#include <cuik.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <ext/threads.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <time.h>
#include <unistd.h>
#endif

static FILE* timer_output;
static mtx_t timer_mutex;
static atomic_int timer_entry_count;
static uint64_t global_profiler_start;

// just to organize the JSON timing stuff a bit better
static thread_local bool is_main_thread;

CUIK_API void cuik_start_global_profiler(const char* filepath) {
    assert(timer_output == NULL);

    mtx_init(&timer_mutex, mtx_plain);
    timer_output = fopen(filepath, "wb");
    fprintf(timer_output, "{\"otherData\": {},\"traceEvents\":[");
    global_profiler_start = cuik_time_in_nanos();
}

CUIK_API void cuik_stop_global_profiler(void) {
    cuik_profile_region(global_profiler_start, "Cuik");
    if (timer_output != NULL) {
        fprintf(timer_output, "]}");
        fclose(timer_output);
    }
}

CUIK_API uint64_t cuik_time_in_nanos(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
}

CUIK_API void cuik_profile_region(uint64_t start, const char* fmt, ...) {
    if (timer_output == NULL) return;

    int64_t elapsed_in_microseconds = (cuik_time_in_nanos() - start) / 1000;
    int64_t start_in_microseconds = start / 1000;

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
        vsnprintf(name, sizeof(name), fmt, ap);
        name[sizeof(name) - 1] = '\0';
        va_end(ap);

        fprintf(timer_output,
            "%c{"
            "\"cat\":\"function\", "
            "\"dur\":%lld, "
            "\"name\":\"%s\", "
            "\"ph\":\"X\", "
            "\"pid\":0, "
            "\"tid\": %u, "
            "\"ts\": %lld }\n",

            i ? ',' : ' ',
            (long long)elapsed_in_microseconds,
            name,
            is_main_thread ? 1 : tid,
            (long long)start_in_microseconds);

        mtx_unlock(&timer_mutex);
    }
}
