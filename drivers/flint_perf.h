#include <stdio.h>
#include <threads.h>
#include "flint.h"

static FlintContext ctx;

static void flintperf__start(void* user_data) {
    ctx = FlintInit((char*) user_data, 1.0 / 1000.0);
}

static void flintperf__stop(void* user_data) {
    FlintQuit(&ctx);
}

static void flintperf__plot(void* user_data, uint64_t start_ns, uint64_t end_ns, const char* label) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = pthread_self();
    #endif

    FlintTraceBeginTid(&ctx, start_ns, label, tid);
    FlintTraceEndTid(&ctx, end_ns, tid);
}

static Cuik_IProfiler flintperf_profiler = {
    .start = flintperf__start,
    .stop  = flintperf__stop,
    .plot  = flintperf__plot
};
