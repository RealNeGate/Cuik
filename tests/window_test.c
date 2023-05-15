#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#pragma comment (lib, "gdi32.lib")
#pragma comment (lib, "user32.lib")
#pragma comment (lib, "opengl32.lib")

LRESULT main_wnd_proc(HWND wnd, UINT message, WPARAM wparam, LPARAM lparam) {
    if (message == WM_DESTROY) {
        ExitProcess(0);
    }

    return DefWindowProcA(wnd, message, wparam, lparam);
}

int main(int argc, char** argv, char** env) {
    const char* name = "Hello, World!";

    WNDCLASSA wc = { 0 };
    wc.hInstance = GetModuleHandleA(NULL);
    wc.lpfnWndProc = main_wnd_proc;
    wc.lpszClassName = name;
    RegisterClassA(&wc);

    HWND window = CreateWindowExA(262144, name,
        name, 0xCF0000,
        400, 400, 1600, 900,
        NULL, NULL, NULL, NULL);

    // Display the window
    ShowWindow(window, SW_SHOWDEFAULT);
    SetFocus(window);

    for (;;) {
        MSG msg;
        GetMessageA(&msg, NULL, 0, 0);
        TranslateMessage(&msg);
        DispatchMessageA(&msg);
    }

    return 0;
}
