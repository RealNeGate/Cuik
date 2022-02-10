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
	
	printf("\nCrash dump (put out an issue on GitHub or sum idk):\n");
	
	char buffer[512];
	for (size_t i = 0; i < frames; i++) {
		DWORD64 disp;
		SymFromAddr(process, (DWORD64)stack[i], &disp, symbol);
		
		size_t len = 0;
		DWORD disp2;
		IMAGEHLP_LINE line;
		if (SymGetLineFromAddr64(process, (DWORD64)stack[i], &disp2, &line)) {
			len = sprintf_s(buffer, 512, "    %s:%lu", line.FileName, line.LineNumber);
			
			// pad to 50
			if (len < 50) {
				for (int i = len; i < 50; i++) buffer[i] = ' ';
				len = 50;
			}
			
			len = sprintf_s(&buffer[len], 512 - len, "<%s+%lu>", symbol->Name, disp2);
		} else {
			len = sprintf_s(buffer, 512, "    %s", symbol->Name);
			
			// pad to 50
			if (len < 50) {
				for (int i = len; i < 50; i++) buffer[i] = ' ';
				len = 50;
			}
			
			len = sprintf_s(&buffer[len], 512 - len, "<0x%lu+%lu>", symbol->Address, disp);
		}
		
		printf("%s\n", buffer);
		if (strcmp(symbol->Name, "main") == 0) break;
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
