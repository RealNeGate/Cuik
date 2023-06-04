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

static LONG WINAPI unhandled_exception_handler(PEXCEPTION_POINTERS exception_ptrs) {
    mtx_lock(&crash_mutex);

    HANDLE process = GetCurrentProcess();
    SymInitialize(process, NULL, TRUE);

    void* stack[100];
    uint16_t frames = CaptureStackBackTrace(0, 100, stack, NULL);
    SYMBOL_INFO* symbol = (SYMBOL_INFO*) cuik_calloc(sizeof(SYMBOL_INFO) + 256 * sizeof(char), 1);

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

    cuik_free(symbol);
    exit(1);

    return EXCEPTION_CONTINUE_SEARCH;
}

void hook_crash_handler(void) {
    mtx_init(&crash_mutex, mtx_plain);
    SetUnhandledExceptionFilter(unhandled_exception_handler);
}
#elif defined(__linux__) && defined(__amd64__)
#include <signal.h>
#include <ucontext.h>
#include <sys/auxv.h>
#include <tb_elf.h>

static _Thread_local jmp_buf error_handler;

void signal_handler(int num, siginfo_t* info, void* ctx) {
    longjmp(error_handler, 1);
}

void hook_crash_handler(void) {
    #if 1
    struct sigaction new_action;

    // Set up the structure to specify the new action
    new_action.sa_sigaction = signal_handler;
    sigemptyset (&new_action.sa_mask);
    new_action.sa_flags = SA_SIGINFO | SA_ONSTACK;

    sigaction(SIGHUP,  &new_action, NULL); // 1
    sigaction(SIGINT,  &new_action, NULL); // 2
    sigaction(SIGQUIT, &new_action, NULL); // 3
    sigaction(SIGILL,  &new_action, NULL); // 4
    sigaction(SIGTRAP, &new_action, NULL); // 5
    sigaction(SIGABRT, &new_action, NULL); // 6
    sigaction(SIGFPE,  &new_action, NULL); // 8
    sigaction(SIGSEGV, &new_action, NULL); // 11
    sigaction(SIGTERM, &new_action, NULL); // 15

    if (setjmp(error_handler) == 1) {
        cuikdg_dump_to_stderr(error_tokens);
    }
    #endif
}
#else
// #error "Implement crash handler on this platform"
void hook_crash_handler(void) {
}
#endif
