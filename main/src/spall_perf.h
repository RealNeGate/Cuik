#include <stdio.h>

#define SPALL_BUFFER_PROFILING
#define SPALL_BUFFER_PROFILING_GET_TIME() cuik_time_in_nanos()
#define SPALL_IMPLEMENTATION
#include "spall.h"

static SpallProfile ctx;
static _Thread_local SpallBuffer muh_buffer;

void spallperf__start_thread(void) {
    size_t size = 4 * 1024 * 1024;
    muh_buffer = (SpallBuffer){ malloc(size), size };
    SpallBufferInit(&ctx, &muh_buffer);
}

void spallperf__stop_thread(void) {
    SpallBufferQuit(&ctx, &muh_buffer);
}

static void spallperf__start(void* user_data) {
    ctx = SpallInit((char*) user_data, 1.0 / 1000.0);
    spallperf__start_thread();
}

static void spallperf__stop(void* user_data) {
    spallperf__stop_thread();
    SpallQuit(&ctx);
}

static void spallperf__begin_plot(void* user_data, uint64_t nanos, const char* label, const char* extra) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    // SpallTraceBeginLenTid(&ctx, &muh_buffer, label, strlen(label), tid, nanos);
    SpallTraceBeginLenArgsTidPid(&ctx, &muh_buffer, label, strlen(label), extra, strlen(extra), tid, 0, nanos);
}

static void spallperf__end_plot(void* user_data, uint64_t nanos) {
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    SpallTraceEndTid(&ctx, &muh_buffer, tid, nanos);
}

static Cuik_IProfiler spall_profiler = {
    .start      = spallperf__start,
    .stop       = spallperf__stop,
    .begin_plot = spallperf__begin_plot,
    .end_plot   = spallperf__end_plot,
};
