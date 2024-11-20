#pragma comment(lib, "User32.Lib")
#pragma comment(lib, "Kernel32.Lib")

__declspec(dllimport) extern void* GetCurrentThread(void);

__declspec(dllimport) extern int MessageBoxA(
    void*        hWnd,
    const char*  lpText,
    const char*  lpCaption,
    unsigned int uType
);

void mainCRTStartup() {
    void* a = GetCurrentThread();
    MessageBoxA(0, "hello, world", "caption", 0);
}
