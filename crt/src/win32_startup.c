// Simple Win32 runtime support, just gets you from a windows 
// entry point to a standard C looking one
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

extern void printf(const char* fmt, ...);
extern int main(int argc, char** argv);
extern LPWSTR GetCommandLineW();
extern LPWSTR* CommandLineToArgvW(LPCWSTR lpCmdLine, int* pNumArgs);

static size_t count_wide_chars(const wchar_t* str) {
	size_t len = 0;
	while (str[len] != 0) len++;
	return len;
}

static bool convert_wide_chars_to_ansi(char* out, const wchar_t* str, size_t len) {
	for (size_t i = 0; i < len; i++) {
		wchar_t ch = *str++;
		if (ch <= 0 && ch > 0x7F) return false;
		
		*out++ = ch;
	}
	
	return true;
}

int mainCRTStartup() {
	// Inject a nicer argv[0] because we might wanna avoid some odd crap being
	// pushed into it
	char exec_path[MAX_PATH];
	DWORD exec_path_length = GetModuleFileNameA(NULL, exec_path, MAX_PATH);
	exec_path[exec_path_length] = 0;
	
	// it's an error if we just can't retrieve an executable path
	if (exec_path_length == 0) return -69420;
	
	int arg_count;
	LPWSTR* args_wide = CommandLineToArgvW(GetCommandLineW(), &arg_count);
	if (!args_wide) {
		printf("could not parse arguments\n");
		return -42069;
	}
	
	// We always provide a reliable argv[0]
	if (arg_count < 1) arg_count = 0;
	
	char** args = malloc(arg_count * sizeof(char*));
	args[0] = exec_path;
	
	// Convert wide chars into ANSI
	for (int i = 1; i < arg_count; i++) {
		size_t wide_len = count_wide_chars(args_wide[i]) + 1;
		char* ansi_str = malloc(wide_len);
		printf("Alloc: %p (%zu bytes)\n", ansi_str);
		
		convert_wide_chars_to_ansi(args[i], args_wide[i], wide_len);
		args[i] = ansi_str;
		
		printf("%S (%zu) -> %s\n", args_wide[i], wide_len, ansi_str);
	}
	
	//int arg_count = 1;
	//char* args[] = { exec_path, NULL };
	return main(arg_count, args);
}
