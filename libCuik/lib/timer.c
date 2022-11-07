#include <common.h>
#include <threads.h>
#include <cuik.h>
#include <stdarg.h>
#include <stdatomic.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static LARGE_INTEGER timer_frequency;
static LARGE_INTEGER timer_start;
#else
#include <time.h>
#include <unistd.h>
#endif

static mtx_t timer_mutex;

static bool should_lock_profiler;
static uint64_t global_profiler_start;

static const Cuik_IProfiler* profiler;
static void* profiler_userdata;

void init_timer_system(void) {
    #ifdef _WIN32
    QueryPerformanceFrequency(&timer_frequency);
    QueryPerformanceCounter(&timer_start);
    #endif
}

void* cuikperf_init(size_t ud_size, const Cuik_IProfiler* p, bool lock_on_plot) {
    assert(profiler == NULL);

    profiler = p;
    profiler_userdata = calloc(ud_size, 1);
    should_lock_profiler = lock_on_plot;

    if (lock_on_plot) {
        mtx_init(&timer_mutex, mtx_plain);
    }
    return profiler_userdata;
}

void cuikperf_start(void) {
    profiler->start(profiler_userdata);

    global_profiler_start = cuik_time_in_nanos();
    profiler->begin_plot(profiler_userdata, global_profiler_start, "libCuik");
}

void cuikperf_stop(void) {
    assert(profiler != NULL);

    profiler->end_plot(profiler_userdata, cuik_time_in_nanos());
    profiler->stop(profiler_userdata);

    if (should_lock_profiler) {
        mtx_destroy(&timer_mutex);
    }
    profiler = NULL;
}

bool cuikperf_is_active(void) {
    return (profiler != NULL);
}

uint64_t cuik_time_in_nanos(void) {
    #ifdef _WIN32
    LARGE_INTEGER l;
    QueryPerformanceCounter(&l);
    l.QuadPart -= timer_start.QuadPart;

    return (l.QuadPart * 1000000000LL) / timer_frequency.QuadPart;
    #else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
    #endif
}

void cuikperf_region_start(uint64_t nanos, const char* fmt, ...) {
    if (profiler == NULL) return;

    // lock if necessary
    if (should_lock_profiler) mtx_lock(&timer_mutex);

    char label[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(label, sizeof(label), fmt, ap);
    label[sizeof(label) - 1] = '\0';
    va_end(ap);

    profiler->begin_plot(profiler_userdata, nanos, label);
    if (should_lock_profiler) mtx_unlock(&timer_mutex);
}

void cuikperf_region_end(void) {
    if (profiler == NULL) return;
    uint64_t nanos = cuik_time_in_nanos();

    if (should_lock_profiler) mtx_lock(&timer_mutex);
    profiler->end_plot(profiler_userdata, nanos);
    if (should_lock_profiler) mtx_unlock(&timer_mutex);
}
