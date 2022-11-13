#if _WIN32
#include "common.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <DbgHelp.h>
#include <time.h>
#pragma comment(lib, "Dbghelp.lib")

#include <threads.h>

static mtx_t crash_mutex;

static LONG WINAPI unhandled_exception_handler(PEXCEPTION_POINTERS exception_ptrs) {
    mtx_lock(&crash_mutex);

    HANDLE process = GetCurrentProcess();
    SymInitialize(process, NULL, TRUE);

    void* stack[100];
    uint16_t frames = CaptureStackBackTrace(0, 100, stack, NULL);
    SYMBOL_INFO* symbol = (SYMBOL_INFO*) calloc(sizeof(SYMBOL_INFO) + 256 * sizeof(char), 1);

    symbol->MaxNameLen   = 255;
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

    printf("\nCrash dump:\n");
    for (size_t i = 0; i < frames; i++) {
        SymFromAddr(process, (DWORD64) stack[i], 0, symbol);

        DWORD disp;
        IMAGEHLP_LINE64 line;
        if (SymGetLineFromAddr64(process, (DWORD64) stack[i], &disp, &line)) {
            printf("    %-40s - 0x%llX - %s:%lu%+ld\n", symbol->Name, symbol->Address, line.FileName, line.LineNumber, disp);
        } else {
            printf("    %-40s - 0x%llX\n", symbol->Name, symbol->Address);
        }
    }

    free(symbol);
    exit(1);

    return EXCEPTION_CONTINUE_SEARCH;
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
