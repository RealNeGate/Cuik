#include <stdio.h>
#include <threads.h>

static FILE* jsonperf__file;

static void jsonperf__start(void* user_data) {
    jsonperf__file = fopen((char*) user_data, "wb");
    fprintf(jsonperf__file, "{\"otherData\": {},\"traceEvents\":[");
}

static void jsonperf__stop(void* user_data) {
    fseek(jsonperf__file, -2, SEEK_CUR);
    fprintf(jsonperf__file, "\n]}");
    fclose(jsonperf__file);
}

static void jsonperf__plot(void* user_data, uint64_t start_ns, uint64_t end_ns, const char* label) {
    double elapsed_in_microseconds = (end_ns - start_ns) / 1000.0;
    double start_in_microseconds = start_ns / 1000.0;

    #if _WIN32
    uint32_t tid = GetCurrentThreadId();
    #else
    uint32_t tid = pthread_self();
    #endif

    fprintf(jsonperf__file,
        "{\"cat\":\"function\", "
        "\"dur\":%f, "
        "\"name\":\"%s\", "
        "\"ph\":\"X\", "
        "\"pid\":0, "
        "\"tid\": %u, "
        "\"ts\": %f},\n",
        elapsed_in_microseconds, label, tid,
        start_in_microseconds);
}

static Cuik_IProfiler jsonperf_profiler = {
    .start = jsonperf__start,
    .stop = jsonperf__stop,
    .plot = jsonperf__plot
};
