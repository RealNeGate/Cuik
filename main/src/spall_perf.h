#include <stdio.h>

#define SPALL_BUFFER_PROFILING
#define SPALL_BUFFER_PROFILING_GET_TIME() __rdtsc()

#ifdef CUIK_USE_SPALL_AUTO
#define SPALL_AUTO_IMPLEMENTATION
#include "spall_auto.h"
#endif

static SpallProfile ctx;
static _Thread_local SpallBuffer muh_buffer;

void spallperf__start_thread(void) {
    size_t size = 4 * 1024 * 1024;

    #ifdef CUIK_USE_SPALL_AUTO
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    spall_auto_thread_init(tid, size, 8*1024*1024);
    #else
    muh_buffer = (SpallBuffer){ malloc(size), size };
    SpallBufferInit(&ctx, &muh_buffer);
    #endif
}

void spallperf__stop_thread(void) {
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    #else
    SpallBufferQuit(&ctx, &muh_buffer);
    #endif
}

static void spallperf__start(void* user_data) {
    #ifndef CUIK_USE_SPALL_AUTO
    ctx = SpallInit((char*) user_data, 1.0 / 1000.0);
    spallperf__start_thread();
    #endif
}

static void spallperf__stop(void* user_data) {
    #ifndef CUIK_USE_SPALL_AUTO
    spallperf__stop_thread();
    SpallQuit(&ctx);
    #endif
}

static void spallperf__begin_plot(void* user_data, uint64_t nanos, const char* label, const char* extra) {
    #ifndef CUIK_USE_SPALL_AUTO
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    SpallTraceBeginLenTid(&ctx, &muh_buffer, label, strlen(label), tid, nanos);
    SpallTraceBeginLenArgsTidPid(&ctx, &muh_buffer, label, strlen(label), extra, strlen(extra), tid, 0, nanos);
    #endif
}

static void spallperf__end_plot(void* user_data, uint64_t nanos) {
    #ifndef CUIK_USE_SPALL_AUTO
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    SpallTraceEndTid(&ctx, &muh_buffer, tid, nanos);
    #endif
}

static Cuik_IProfiler spall_profiler = {
    .start      = spallperf__start,
    .stop       = spallperf__stop,
    .begin_plot = spallperf__begin_plot,
    .end_plot   = spallperf__end_plot,
};
