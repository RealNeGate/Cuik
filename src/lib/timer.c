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

static FILE* timer_output;
static mtx_t timer_mutex;
static atomic_int timer_entry_count;

// just to organize the JSON timing stuff a bit better
static thread_local bool is_main_thread;

// done regardless of the profiler running just to be able to query time in general
void timer_init(void) {
    is_main_thread = true;
}

void timer_open(const char* path) {
    assert(timer_output == NULL);

    mtx_init(&timer_mutex, mtx_plain);
    timer_output = fopen(path, "wb");
    fprintf(timer_output, "{\"otherData\": {},\"traceEvents\":[");
}

void timer_close(void) {
    if (timer_output != NULL) {
        fprintf(timer_output, "]}");
        fclose(timer_output);
    }
}

uint64_t timer_now() {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
}

void timer_end(uint64_t start, const char* fmt, ...) {
    if (timer_output == NULL) return;

    int64_t elapsed_in_microseconds = (timer_now() - start) / 1000;
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
        int formatted_len = vsnprintf(name, sizeof(name), fmt, ap);
        name[sizeof(name) - 1] = '\0';
        va_end(ap);

        if (formatted_len < 0 || formatted_len >= sizeof(name)) {
            fprintf(stderr, "error: buffer overflow on sprintf_s!\n");
            abort();
        }

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
