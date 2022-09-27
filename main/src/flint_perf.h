#include <stdio.h>

#define SPALL_BUFFER_PROFILING
#define SPALL_BUFFER_PROFILING_GET_TIME() cuik_time_in_nanos()
#define SPALL_IMPLEMENTATION
#include "flint.h"

static SpallProfile ctx;
static _Thread_local SpallBuffer muh_buffer;

void flintperf__start_thread(void) {
    size_t size = 4 * 1024 * 1024;
    muh_buffer = (SpallBuffer){ malloc(size), size };
    SpallBufferInit(&ctx, &muh_buffer);
}

void flintperf__stop_thread(void) {
    SpallBufferQuit(&ctx, &muh_buffer);
}

static void flintperf__start(void* user_data) {
    ctx = SpallInit((char*) user_data, 1.0 / 1000.0);
    flintperf__start_thread();
}

static void flintperf__stop(void* user_data) {
    flintperf__stop_thread();
    SpallQuit(&ctx);
}

static void flintperf__begin_plot(void* user_data, uint64_t nanos, const char* label) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    SpallTraceBeginTid(&ctx, &muh_buffer, nanos, label, tid);
}

static void flintperf__end_plot(void* user_data, uint64_t nanos) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    SpallTraceEndTid(&ctx, &muh_buffer, nanos, tid);
}

static Cuik_IProfiler flintperf_profiler = {
    .start      = flintperf__start,
    .stop       = flintperf__stop,
    .begin_plot = flintperf__begin_plot,
    .end_plot   = flintperf__end_plot,
};
