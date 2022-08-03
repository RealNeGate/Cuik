#include <common.h>
#include <threads.h>
#include <cuik.h>
#include <stdarg.h>
#include <stdatomic.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <time.h>
#include <unistd.h>
#endif

static mtx_t timer_mutex;

static uint64_t global_profiler_start;

static bool should_lock_profiler;
static const Cuik_IProfiler* profiler;

CUIK_API void cuik_start_global_profiler(const Cuik_IProfiler* p, bool lock_on_plot) {
    assert(p != NULL);
    assert(profiler == NULL);

    profiler = p;
    should_lock_profiler = lock_on_plot;

    if (lock_on_plot) {
        mtx_init(&timer_mutex, mtx_plain);
    }

    CUIK_CALL(profiler, start);
    global_profiler_start = cuik_time_in_nanos();
}

CUIK_API void cuik_stop_global_profiler(void) {
    assert(profiler != NULL);

    CUIK_CALL(profiler, plot, 0, cuik_time_in_nanos() - global_profiler_start, "libCuik");
    CUIK_CALL(profiler, stop);

    if (should_lock_profiler) {
        mtx_destroy(&timer_mutex);
    }
    profiler = NULL;
}

CUIK_API bool cuik_is_profiling(void) {
    return (profiler != NULL);
}

CUIK_API uint64_t cuik_time_in_nanos(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
}

CUIK_API void cuik_profile_region(uint64_t start, const char* fmt, ...) {
    if (profiler == NULL) return;
    uint64_t end = cuik_time_in_nanos();

    // lock if necessary
    if (should_lock_profiler) mtx_lock(&timer_mutex);

    char label[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(label, sizeof(label), fmt, ap);
    label[sizeof(label) - 1] = '\0';
    va_end(ap);

    CUIK_CALL(profiler, plot, start - global_profiler_start, end - global_profiler_start, label);

    if (should_lock_profiler) mtx_unlock(&timer_mutex);
}
