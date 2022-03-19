// Simple Win32 runtime support, just gets you from a windows 
// entry point to a standard C looking one
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

extern int main(int argc, char** argv);

int mainCRTStartup() {
	// Inject a nicer argv[0] because we might wanna avoid some odd crap being
	// pushed into it
	char exec_path[MAX_PATH];
	DWORD exec_path_length = GetModuleFileNameA(NULL, exec_path, MAX_PATH);
	exec_path[exec_path_length] = 0;
	
	// it's an error if we just can't retrieve an executable path
	if (exec_path_length == 0) return -69420;
	
	// TODO(NeGate): Parse command line arguments
	char* args[1] = { exec_path };
	return main(1, args);
}
