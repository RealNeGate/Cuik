#if _WIN32
#include "common.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <minidumpapiset.h>
#include <time.h>

#include <threads.h>

static mtx_t crash_mutex;

static LONG WINAPI unhandled_exception_handler(PEXCEPTION_POINTERS exception_ptrs) {
    mtx_lock(&crash_mutex);
    time_t now = time(NULL);

    char path[_MAX_PATH + 1];
    sprintf_s(path, _MAX_PATH + 1, "./crash_dump_%lld.dmp", now);

    char* new_path = malloc(MAX_PATH);
    if (GetFullPathNameA(path, MAX_PATH, new_path, NULL) == 0) {
        fprintf(stderr, "GetFullPathNameA broke while in the middle of a crash... yikes?");
        return EXCEPTION_EXECUTE_HANDLER;
    }

    fprintf(stderr, "A crash happened, please make a Github Issue or something i dont control you :p\n");
    fprintf(stderr, "%s\n\n", new_path);

    // This writes a dump file to the current dir.
    // To view this file open it up in Visual Studio
    // If source and symbols are available you should
    // along with the crash point.
    HANDLE crash_dump_file = CreateFileA(path,
        GENERIC_WRITE,
        0,
        NULL,
        CREATE_ALWAYS,
        FILE_ATTRIBUTE_NORMAL,
        NULL);

    MINIDUMP_EXCEPTION_INFORMATION mini_dump_info = {
        .ThreadId = GetCurrentThreadId(),
        .ExceptionPointers = exception_ptrs,
        .ClientPointers = TRUE};

    MiniDumpWriteDump(GetCurrentProcess(),
        GetCurrentProcessId(),
        crash_dump_file,
        (MINIDUMP_TYPE)(MiniDumpWithProcessThreadData | MiniDumpWithThreadInfo),
        &mini_dump_info,
        NULL,
        NULL);

    CloseHandle(crash_dump_file);
    ExitProcess(69420);
}

void hook_crash_handler() {
    mtx_init(&crash_mutex, mtx_plain);
    SetUnhandledExceptionFilter(unhandled_exception_handler);

    #if 0
	ULONG_PTR low, high;
	GetCurrentThreadStackLimits(&low, &high);

	printf("%f MiB\n", (high-low) / 1048576.0f);
    #endif
}

#else
// #error "Implement crash handler on this platform"
void hook_crash_handler() {
}
#endif
