#if _WIN32
#include "common.h"
#include <time.h>

// because we undef'd it earlier
#define VOID void
#define TokenStream FuckYouWindows
#include <DbgHelp.h>
#pragma comment(lib, "Dbghelp.lib")

#include <threads.h>

static mtx_t crash_mutex;

static LONG WINAPI unhandled_exception_handler(EXCEPTION_POINTERS* e) {
    mtx_lock(&crash_mutex);

    HANDLE process = GetCurrentProcess();
    SymInitialize(process, NULL, TRUE);

    void* stack[100];
    uint16_t frames = CaptureStackBackTrace(0, 100, stack, NULL);
    SYMBOL_INFO* symbol = (SYMBOL_INFO*) cuik_calloc(sizeof(SYMBOL_INFO) + 256 * sizeof(char), 1);

    symbol->MaxNameLen   = 255;
    symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

    switch (e->ExceptionRecord->ExceptionCode) {
        case EXCEPTION_ACCESS_VIOLATION: {
            uintptr_t addr = e->ExceptionRecord->ExceptionInformation[1];

            if (e->ExceptionRecord->ExceptionInformation[0] == 8) {
                printf("\nIllegal jump to %#llx\n", addr);
            } else {
                bool is_write = e->ExceptionRecord->ExceptionInformation[0] == 1;
                if (addr <= 0xFFFF) {
                    printf("\nIllegal %s from null pointer (%#llx)\n", is_write ? "write" : "read", addr);
                } else {
                    printf("\nIllegal %s from %#llx\n", is_write ? "write" : "read", addr);
                }
            }
            break;
        }

        default:
        printf("\nCrash dump:\n");
        break;
    }

    size_t i = 0;

    // skip exception stack frames
    for (; i < frames; i++) {
        SymFromAddr(process, (DWORD64) stack[i], 0, symbol);
        if (strcmp(symbol->Name, "KiUserExceptionDispatcher") == 0) {
            i += 1;
            break;
        }
    }

    for (; i < frames; i++) {
        SymFromAddr(process, (DWORD64) stack[i], 0, symbol);

        DWORD disp;
        IMAGEHLP_LINE64 line;
        if (SymGetLineFromAddr64(process, (DWORD64) stack[i], &disp, &line)) {
            printf("    %-40s - 0x%llX - %s:%lu%+ld\n", symbol->Name, symbol->Address, line.FileName, line.LineNumber, disp);
        } else {
            printf("    %-40s - 0x%llX\n", symbol->Name, symbol->Address);
        }

        if (strcmp(symbol->Name, "main") == 0) {
            break;
        }
    }

    exit(1);
    return EXCEPTION_CONTINUE_SEARCH;
}

void hook_crash_handler(void) {
    mtx_init(&crash_mutex, mtx_plain);
    SetUnhandledExceptionFilter(unhandled_exception_handler);
}
#else
// #error "Implement crash handler on this platform"
void hook_crash_handler(void) {
}
#endif
