
//#include <stdbool.h>
//#include <stdint.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
 
// hacky
#ifdef __CUIKC__
void* __readgsqword(LONG offset) { return NULL; }
void __stosb(char* ptr, char src, size_t count) {}
void _chvalidator_l() {}
LONG InterlockedExchangeAdd(LONG volatile *Addend, LONG Value) { return 0; }
long long InterlockedExchangeAdd64(long long volatile *Addend, long long Value) { return 0; }
unsigned long long __shiftright128 ( unsigned long long _LowPart , unsigned long long _HighPart , unsigned char _Shift ) { return 0; }
LPUWSTR __stdcall uaw_CharUpperW(LPUWSTR String) { return NULL; }
#endif

int main(int argc, char** argv, char** env) {
	int a = (16 * 6) + 5;

	char* name = "Hello";
	
	WNDCLASSA wc = {};
	wc.hInstance = GetModuleHandleA(NULL);
	wc.lpfnWndProc = main_wnd_proc;
	wc.lpszClassName = name;
	RegisterClassA(&wc);
	
	HWND window = CreateWindowExA(262144, name,
									name, 0xCF0000,
									400, 400, 1600, 900,
								    NULL, NULL, NULL, NULL);

	GetLastError();

	// Display the window
	ShowWindow(window, SW_SHOWDEFAULT);
	SetFocus(window);
	
	while (1) {
		MSG msg;
		GetMessageA(&msg, NULL, 0, 0);
		TranslateMessage(&msg);
		DispatchMessageA(&msg);
	}
	return 0;
}

LRESULT main_wnd_proc(HWND wnd, UINT message, WPARAM wparam, LPARAM lparam) {
	if (message == WM_DESTROY) {
		ExitProcess(0);
	}
	
	return DefWindowProcA(wnd, message, wparam, lparam);
}
