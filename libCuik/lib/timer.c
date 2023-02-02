#include <common.h>
#include <threads.h>
#include <cuik.h>
#include <stdarg.h>
#include <stdatomic.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static double rdtsc_freq;
static uint64_t timer_start;
#else
#include <time.h>
#include <unistd.h>
#endif

static mtx_t timer_mutex;

static bool should_lock_profiler;

static const Cuik_IProfiler* profiler;
static void* profiler_userdata;

#ifdef _WIN32
double get_rdtsc_multiplier(void) {
    // Cache the answer so that multiple calls never take the slow path more than once
    static double multiplier = 0;
    if (multiplier) {
        return multiplier;
    }

    uint64_t tsc_freq = 0;

    // Fast path: Load kernel-mapped memory page
    HMODULE ntdll = LoadLibraryA("ntdll.dll");
    if (ntdll) {

        int (*NtQuerySystemInformation)(int, void *, unsigned int, unsigned int *) =
        (int (*)(int, void *, unsigned int, unsigned int *))GetProcAddress(ntdll, "NtQuerySystemInformation");
        if (NtQuerySystemInformation) {

            volatile uint64_t *hypervisor_shared_page = NULL;
            unsigned int size = 0;

            // SystemHypervisorSharedPageInformation == 0xc5
            int result = (NtQuerySystemInformation)(0xc5, (void *)&hypervisor_shared_page, sizeof(hypervisor_shared_page), &size);

            // success
            if (size == sizeof(hypervisor_shared_page) && result >= 0) {
                // docs say ReferenceTime = ((VirtualTsc * TscScale) >> 64)
                //      set ReferenceTime = 10000000 = 1 second @ 10MHz, solve for VirtualTsc
                //       =>    VirtualTsc = 10000000 / (TscScale >> 64)
                tsc_freq = (10000000ull << 32) / (hypervisor_shared_page[1] >> 32);
                // If your build configuration supports 128 bit arithmetic, do this:
                // tsc_freq = ((unsigned __int128)10000000ull << (unsigned __int128)64ull) / hypervisor_shared_page[1];
            }
        }
        FreeLibrary(ntdll);
    }

    // Slow path
    if (!tsc_freq) {

        // Get time before sleep
        uint64_t qpc_begin = 0; QueryPerformanceCounter((LARGE_INTEGER *)&qpc_begin);
        uint64_t tsc_begin = __rdtsc();

        Sleep(2);

        // Get time after sleep
        uint64_t qpc_end = qpc_begin + 1; QueryPerformanceCounter((LARGE_INTEGER *)&qpc_end);
        uint64_t tsc_end = __rdtsc();

        // Do the math to extrapolate the RDTSC ticks elapsed in 1 second
        uint64_t qpc_freq = 0; QueryPerformanceFrequency((LARGE_INTEGER *)&qpc_freq);
        tsc_freq = (tsc_end - tsc_begin) * qpc_freq / (qpc_end - qpc_begin);
    }

    // Failure case
    if (!tsc_freq) {
        tsc_freq = 1000000000;
    }

    multiplier = 1000000.0 / (double)tsc_freq;
    return multiplier;
}
#endif

void init_timer_system(void) {
    #ifdef _WIN32
    rdtsc_freq = get_rdtsc_multiplier() / 1000000.0;
    timer_start = __rdtsc();
    #endif
}

void cuikperf_start(void* ud, const Cuik_IProfiler* p, bool lock_on_plot) {
    assert(profiler == NULL);

    profiler = p;
    profiler_userdata = ud;
    should_lock_profiler = lock_on_plot;

    if (lock_on_plot) {
        mtx_init(&timer_mutex, mtx_plain);
    }

    profiler->start(profiler_userdata);
    profiler->begin_plot(profiler_userdata, cuik_time_in_nanos(), "libCuik", "");
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
    return (__rdtsc() - timer_start) * (rdtsc_freq * 1000000000.0);
    #else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
    #endif
}

void cuikperf_region_start(uint64_t nanos, const char* fmt, const char* extra) {
    if (profiler == NULL) return;

    // lock if necessary
    if (should_lock_profiler) mtx_lock(&timer_mutex);

    profiler->begin_plot(profiler_userdata, nanos, fmt, extra ? extra : "");
    if (should_lock_profiler) mtx_unlock(&timer_mutex);
}

void cuikperf_region_end(void) {
    if (profiler == NULL) return;
    uint64_t nanos = cuik_time_in_nanos();

    if (should_lock_profiler) mtx_lock(&timer_mutex);
    profiler->end_plot(profiler_userdata, nanos);
    if (should_lock_profiler) mtx_unlock(&timer_mutex);
}
