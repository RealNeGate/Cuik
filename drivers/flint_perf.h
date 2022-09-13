#include <stdio.h>
#include <threads.h>

#define FLINT_BUFFER_PROFILING
#define FLINT_BUFFER_PROFILING_GET_TIME() cuik_time_in_nanos()
#define FLINT_IMPLEMENTATION
#include "flint.h"

static FlintProfile ctx;
static thread_local FlintBuffer muh_buffer;

void flintperf__start_thread(void) {
    size_t size = 4 * 1024 * 1024;
    muh_buffer = (FlintBuffer){ malloc(size), size };
    FlintBufferInit(&ctx, &muh_buffer);
}

void flintperf__stop_thread(void) {
    FlintBufferQuit(&ctx, &muh_buffer);
}

#define USE_MEMWATCH 0

#if USE_MEMWATCH
static _Atomic bool memwatcher_running = true;
static thrd_t memwatch_thread;

static int memwatcher(void* arg) {
    uint64_t start = cuik_time_in_nanos();
    FILE* f = fopen("memwatch.txt", "wb");

    while (memwatcher_running) {
        MEMORYSTATUSEX state = { .dwLength = sizeof(MEMORYSTATUSEX) };
        GlobalMemoryStatusEx(&state);

        fprintf(f, "%10.3f,%ld,\n", (cuik_time_in_nanos() - start) / 1000000.0, state.dwMemoryLoad);

        // wait like a 10 milliseconds
        thrd_sleep(&(xtime){ .nsec = 10000000 });
    }

    fclose(f);
    return 0;
}
#endif

static void flintperf__start(void* user_data) {
    ctx = FlintInit((char*) user_data, 1.0 / 1000.0);
    flintperf__start_thread();

    #if USE_MEMWATCH
    if (thrd_create(&memwatch_thread, memwatcher, NULL) != thrd_success) {
        fprintf(stderr, "error: could not create worker threads!\n");
        abort();
    }
    #endif
}

static void flintperf__stop(void* user_data) {
    #if USE_MEMWATCH
    memwatcher_running = false;
    int res;
    thrd_join(memwatch_thread, &res);
    #endif

    flintperf__stop_thread();
    FlintQuit(&ctx);
}

static void flintperf__plot(void* user_data, uint64_t start_ns, uint64_t end_ns, const char* label) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = pthread_self();
    #endif

    FlintTraceCompleteTid(&ctx, &muh_buffer, start_ns, end_ns - start_ns, label, tid);
}

static Cuik_IProfiler flintperf_profiler = {
    .start = flintperf__start,
    .stop  = flintperf__stop,
    .plot  = flintperf__plot
};
