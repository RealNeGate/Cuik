#include <windows.h>

#pragma comment (lib, "kernel32")
#pragma comment (lib, "user32")
#pragma comment (lib, "gdi32")

struct the_baby
{
    DWORD     dwExStyle;
    LPCWSTR   lpClassName;
    LPCWSTR   lpWindowName;
    DWORD     dwStyle;
    int       X;
    int       Y;
    int       nWidth;
    int       nHeight;
    HWND      hWndParent;
    HMENU     hMenu;
    HINSTANCE hInstance;
    LPVOID    lpParam;
};

#define CREATE_DANGEROUS_WINDOW (WM_USER + 0x1337)
#define DESTROY_DANGEROUS_WINDOW (WM_USER + 0x1338)

static DWORD MainThreadID;

static LRESULT CALLBACK ServiceWndProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
    /* NOTE(casey): This is not really a window handler per se, it's actually just
       a remote thread call handler. Windows only really has blocking remote thread
       calls if you register a WndProc for them, so that's what we do.

       This handles CREATE_DANGEROUS_WINDOW and DESTROY_DANGEROUS_WINDOW, which are
       just calls that do CreateWindow and DestroyWindow here on this thread when
       some other thread wants that to happen.
    */

    LRESULT Result = 0;

    switch (Message)
    {
        case CREATE_DANGEROUS_WINDOW:
        {
            the_baby *Baby = (the_baby *)WParam;
            Result = (LRESULT)CreateWindowExW(Baby->dwExStyle,
                                                  Baby->lpClassName,
                                                  Baby->lpWindowName,
                                                  Baby->dwStyle,
                                                  Baby->X,
                                                  Baby->Y,
                                                  Baby->nWidth,
                                                  Baby->nHeight,
                                                  Baby->hWndParent,
                                                  Baby->hMenu,
                                                  Baby->hInstance,
                                                  Baby->lpParam);
        } break;

        case DESTROY_DANGEROUS_WINDOW:
        {
            DestroyWindow((HWND)WParam);
        } break;

        default:
        {
            Result = DefWindowProcW(Window, Message, WParam, LParam);
        } break;
    }

    return Result;
}

static LRESULT CALLBACK DisplayWndProc(HWND Window, UINT Message, WPARAM WParam, LPARAM LParam)
{
    /* NOTE(casey): This is an example of an actual window procedure. It doesn't do anything
       but forward things to the main thread, because again, all window messages now occur
       on the message thread, and presumably we would rather handle everything there.  You
       don't _have_ to do that - you could choose to handle some of the messages here.
       But if you did, you would have to actually think about whether there are race conditions
       with your main thread and all that.  So just PostThreadMessageW()'ing everything gets
       you out of having to think about it.
    */

    LRESULT Result = 0;

    switch (Message)
    {
        // NOTE(casey): Mildly annoying, if you want to specify a window, you have
        // to snuggle the params yourself, because Windows doesn't let you forward
        // a god damn window message even though the program IS CALLED WINDOWS. It's
        // in the name! Let me pass it!
        case WM_CLOSE:
        {
            PostThreadMessageW(MainThreadID, Message, (WPARAM)Window, LParam);
        } break;

        // NOTE(casey): Anything you want the application to handle, forward to the main thread
        // here.
        case WM_MOUSEMOVE:
        case WM_LBUTTONDOWN:
        case WM_LBUTTONUP:
        case WM_DESTROY:
        case WM_CHAR:
        {
            PostThreadMessageW(MainThreadID, Message, WParam, LParam);
        } break;

        default:
        {
            Result = DefWindowProcW(Window, Message, WParam, LParam);
        } break;
    }

    return Result;
}

static DWORD WINAPI MainThread(LPVOID Param)
{
    /* NOTE(Casey): This is your app code. Basically you just do everything the same,
       but instead of calling CreateWindow/DestroyWindow, you use SendMessage to
       do it on the other thread, using the CREATE_DANGEROUS_WINDOW and DESTROY_DANGEROUS_WINDOW
       user messages.  Otherwise, everything proceeds as normal.
    */
    HWND ServiceWindow = (HWND)Param;

    WNDCLASSEXW WindowClass = {};
    WindowClass.cbSize = sizeof(WindowClass);
    WindowClass.lpfnWndProc = &DisplayWndProc;
    WindowClass.hInstance = GetModuleHandleW(NULL);
    WindowClass.hIcon = LoadIconA(NULL, IDI_APPLICATION);
    WindowClass.hCursor = LoadCursorA(NULL, IDC_ARROW);
    WindowClass.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    WindowClass.lpszClassName = L"Dangerous Class";
    RegisterClassExW(&WindowClass);

    the_baby Baby = {};
    Baby.dwExStyle = 0;;
    Baby.lpClassName = WindowClass.lpszClassName;
    Baby.lpWindowName = L"Dangerous Window";
    Baby.dwStyle = WS_OVERLAPPEDWINDOW|WS_VISIBLE;
    Baby.X = CW_USEDEFAULT;
    Baby.Y = CW_USEDEFAULT;
    Baby.nWidth = CW_USEDEFAULT;
    Baby.nHeight = CW_USEDEFAULT;
    Baby.hInstance = WindowClass.hInstance;
    HWND ThisWouldBeTheHandleIfYouCared = (HWND)SendMessageW(ServiceWindow, CREATE_DANGEROUS_WINDOW, (WPARAM)&Baby, 0);

    int X = 0;
    for(;;)
    {
        MSG Message;
        while(PeekMessage(&Message, 0, 0, 0, PM_REMOVE))
        {
            switch(Message.message)
            {
                case WM_CHAR:
                {
                    SendMessageW(ServiceWindow, CREATE_DANGEROUS_WINDOW, (WPARAM)&Baby, 0);
                } break;

                case WM_CLOSE:
                {
                    SendMessageW(ServiceWindow, DESTROY_DANGEROUS_WINDOW, Message.wParam, 0);
                } break;
            }
        }

        int MidPoint = (X++%(64*1024))/64;

        int WindowCount = 0;
        for(HWND Window = FindWindowExW(0, 0, WindowClass.lpszClassName, 0);
            Window;
            Window = FindWindowExW(0, Window, WindowClass.lpszClassName, 0))
        {
            RECT Client;
            GetClientRect(Window, &Client);
            HDC DC = GetDC(Window);

            PatBlt(DC, 0, 0, MidPoint, Client.bottom, BLACKNESS);
            if(Client.right > MidPoint)
            {
                PatBlt(DC, MidPoint, 0, Client.right - MidPoint, Client.bottom, WHITENESS);
            }
            ReleaseDC(Window, DC);

            ++WindowCount;
        }

        if(WindowCount == 0)
        {
            break;
        }
    }

    ExitProcess(0);
}

int WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{
    /* NOTE(casey): At startup, you create one hidden window used to handle requests
       to create or destroy windows.  There's nothing special about this window; it
       only exists because Windows doesn't have a way to do a remote thread call without
       a window handler.  You could instead just do this with your own synchronization
       primitives if you wanted - this is just the easiest way to do it on Windows
       because they've already built it for you.
    */
    WNDCLASSEXW WindowClass = {};
    WindowClass.cbSize = sizeof(WindowClass);
    WindowClass.lpfnWndProc = &ServiceWndProc;
    WindowClass.hInstance = GetModuleHandleW(NULL);
    WindowClass.hIcon = LoadIconA(NULL, IDI_APPLICATION);
    WindowClass.hCursor = LoadCursorA(NULL, IDC_ARROW);
    WindowClass.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
    WindowClass.lpszClassName = L"DTCClass";
    RegisterClassExW(&WindowClass);

    HWND ServiceWindow = CreateWindowExW(0, WindowClass.lpszClassName, L"DTCService", 0,
                                         CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
                                         0, 0, WindowClass.hInstance, 0);

    // NOTE(casey): Once the service window is created, you can start the main thread,
    // which is where all your app code would actually happen.
    CreateThread(0, 0, MainThread, ServiceWindow, 0, &MainThreadID);

    // NOTE(casey): This thread can just idle for the rest of the run, forwarding
    // messages to the main thread that it thinks the main thread wants.
    for(;;)
    {
        MSG Message;
        GetMessageW(&Message, 0, 0, 0);
        TranslateMessage(&Message);
        if((Message.message == WM_CHAR) ||
           (Message.message == WM_KEYDOWN) ||
           (Message.message == WM_QUIT) ||
           (Message.message == WM_SIZE))
        {
            PostThreadMessageW(MainThreadID, Message.message, Message.wParam, Message.lParam);
        }
        else
        {
            DispatchMessageW(&Message);
        }
    }
}
