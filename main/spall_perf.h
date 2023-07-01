#include <stdio.h>

#ifndef _WIN32
#include <unistd.h>
#endif

#ifdef CUIK_USE_SPALL_AUTO
#define SPALL_BUFFER_PROFILING
#define SPALL_BUFFER_PROFILING_GET_TIME() __rdtsc()
#define SPALL_AUTO_IMPLEMENTATION
#include "spall_auto.h"
#else
#define SPALL_BUFFER_PROFILING
#define SPALL_BUFFER_PROFILING_GET_TIME() cuik_time_in_nanos()
#include "spall.h"
#endif

#ifdef _WIN32
__declspec(dllimport) unsigned long GetCurrentThreadId(void);
#endif

static SpallProfile ctx;
static _Thread_local SpallBuffer muh_buffer;

void spallperf__start_thread(void) {
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_init(1, 1ull<<28ull);
    #else
    if (cuikperf_is_active()) {
        size_t size = 4 * 1024 * 1024;
        muh_buffer = (SpallBuffer){ malloc(size), size };
        spall_buffer_init(&ctx, &muh_buffer);
    }
    #endif
}

void spallperf__stop_thread(void) {
    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    #else
    if (cuikperf_is_active()) {
        spall_buffer_quit(&ctx, &muh_buffer);
    }
    #endif
}

static void spallperf__start(void* user_data) {
    #ifndef CUIK_USE_SPALL_AUTO
    ctx = spall_init_file((char*) user_data, 1.0 / 1000.0);
    spallperf__start_thread();
    #endif
}

static void spallperf__stop(void* user_data) {
    #ifndef CUIK_USE_SPALL_AUTO
    spallperf__stop_thread();
    spall_quit(&ctx);
    #endif
}

static void spallperf__begin_plot(void* user_data, uint64_t nanos, const char* label, const char* extra) {
    #ifndef CUIK_USE_SPALL_AUTO
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    spall_buffer_begin_args(&ctx, &muh_buffer, label, strlen(label), extra, strlen(extra), nanos, tid, 0);
    #endif
}

static void spallperf__end_plot(void* user_data, uint64_t nanos) {
    #ifndef CUIK_USE_SPALL_AUTO
    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = getpid();
    #endif

    spall_buffer_end_ex(&ctx, &muh_buffer, nanos, tid, 0);
    #endif
}

static Cuik_IProfiler spall_profiler = {
    .start      = spallperf__start,
    .stop       = spallperf__stop,
    .begin_plot = spallperf__begin_plot,
    .end_plot   = spallperf__end_plot,
};
