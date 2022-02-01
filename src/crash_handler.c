#if _WIN32
#include "common.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <DbgHelp.h>

static LONG WINAPI unhandled_exception_handler(PEXCEPTION_POINTERS exception_ptrs) {
	HANDLE process = GetCurrentProcess();
	SymInitialize(process, NULL, TRUE);
	
	void* stack[100];
	uint16_t frames = CaptureStackBackTrace(0, 100, stack, NULL);
	SYMBOL_INFO* symbol = (SYMBOL_INFO*) calloc(sizeof(SYMBOL_INFO) + 256 * sizeof(char), 1);
	
	symbol->MaxNameLen   = 255;
	symbol->SizeOfStruct = sizeof(SYMBOL_INFO);
	
	for (size_t i = 0; i < frames; i++) {
		SymFromAddr(process, (DWORD64)stack[i], 0, symbol);
		
		printf("%llu: %s - 0x%llX\n", frames - i - 1, symbol->Name, symbol->Address);
	}
	
	free(symbol);
	return EXCEPTION_EXECUTE_HANDLER;
}

void hook_crash_handler() {
    SetUnhandledExceptionFilter(unhandled_exception_handler);
}

#else
#error "Implement crash handler on this platform"
#endif
