#pragma once

typedef void VOID;
typedef int BOOL;
typedef long LONG;
typedef unsigned int UINT;
typedef unsigned int DWORD;

typedef short ATOM;

typedef void* HWND;
typedef void* HICON;
typedef void* HMENU;
typedef void* HBRUSH;
typedef void* HCURSOR;
typedef void* HMODULE;
typedef void* LPVOID;
typedef DWORD* LPDWORD;
typedef void* LRESULT;
typedef void* HANDLE;
typedef const char* LPCSTR;

typedef int INT_PTR, *PINT_PTR;
typedef unsigned int UINT_PTR, *PUINT_PTR;

typedef long LONG_PTR, *PLONG_PTR;
typedef unsigned long ULONG_PTR, *PULONG_PTR;

typedef UINT_PTR            WPARAM;
typedef LONG_PTR            LPARAM;
typedef LONG_PTR            LRESULT;

typedef unsigned long long  SIZE_T;

typedef void* HINSTANCE;
typedef void(*WNDPROC)(HWND, UINT, WPARAM, LPARAM);

typedef DWORD (*PTHREAD_START_ROUTINE)(LPVOID lpThreadParameter);
typedef PTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE;

typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

typedef struct tagPOINT {
	LONG x;
	LONG y;
} POINT, *PPOINT;

typedef struct tagWNDCLASSA {
	UINT      style;
	WNDPROC   lpfnWndProc;
	int       cbClsExtra;
	int       cbWndExtra;
	HINSTANCE hInstance;
	HICON     hIcon;
	HCURSOR   hCursor;
	HBRUSH    hbrBackground;
	LPCSTR    lpszMenuName;
	LPCSTR    lpszClassName;
} WNDCLASSA, *PWNDCLASSA, *NPWNDCLASSA, *LPWNDCLASSA;

typedef struct tagMSG {
	HWND   hwnd;
	UINT   message;
	WPARAM wParam;
	LPARAM lParam;
	DWORD  time;
	POINT  pt;
	DWORD  lPrivate;
} MSG, *PMSG, *NPMSG, *LPMSG;

#define SW_HIDE             0
#define SW_SHOWNORMAL       1
#define SW_NORMAL           1
#define SW_SHOWMINIMIZED    2
#define SW_SHOWMAXIMIZED    3
#define SW_MAXIMIZE         3
#define SW_SHOWNOACTIVATE   4
#define SW_SHOW             5
#define SW_MINIMIZE         6
#define SW_SHOWMINNOACTIVE  7
#define SW_SHOWNA           8
#define SW_RESTORE          9
#define SW_SHOWDEFAULT      10
#define SW_FORCEMINIMIZE    11
#define SW_MAX              11

HMODULE GetModuleHandleA(LPCSTR lpModuleName);
LRESULT DefWindowProcA(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam);
BOOL GetMessageA(LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax);
BOOL TranslateMessage(const MSG *lpMsg);
BOOL ShowWindow(HWND hWnd, int nCmdShow);
ATOM RegisterClassA(const WNDCLASSA* wc);
BOOL SetFocus(HWND hWnd);
DWORD GetLastError();

HWND CreateWindowExA(DWORD dwExStyle, LPCSTR lpClassName, 
					 LPCSTR lpWindowName, DWORD dwStyle,
					 int X, int Y, int nWidth, int nHeight,
					 HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam);

LRESULT DispatchMessageA(const MSG *lpMsg);
LRESULT DispatchMessageW(const MSG *lpMsg);

BOOL PostThreadMessageA(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam);
BOOL PostThreadMessageW(DWORD idThread, UINT Msg, WPARAM wParam, LPARAM lParam);

#define _WIN32_WINNT                    10

#define WM_NULL                         0
#define WM_CREATE                       1
#define WM_DESTROY                      2
#define WM_MOVE                         3
#define WM_SIZE                         5
#define WM_ACTIVATE                     6

#define WM_SETFOCUS                     7
#define WM_KILLFOCUS                    8
#define WM_ENABLE                       10
#define WM_SETREDRAW                    11
#define WM_SETTEXT                      12
#define WM_GETTEXT                      13
#define WM_GETTEXTLENGTH                14
#define WM_PAINT                        15
#define WM_CLOSE                        16
#define WM_QUIT                         18
#define WM_ERASEBKGND                   20
#define WM_SYSCOLORCHANGE               21
#define WM_SHOWWINDOW                   24
#define WM_WININICHANGE                 26

#define WM_KEYFIRST                     256
#define WM_KEYDOWN                      256
#define WM_KEYUP                        257
#define WM_CHAR                         258
#define WM_DEADCHAR                     259
#define WM_SYSKEYDOWN                   260
#define WM_SYSKEYUP                     261
#define WM_SYSCHAR                      262
#define WM_SYSDEADCHAR                  263

HANDLE GetCurrentProcess(VOID);
DWORD GetCurrentProcessId(VOID);
VOID ExitProcess(UINT uExitCode);
BOOL TerminateProcess(HANDLE hProcess, UINT uExitCode);
BOOL GetExitCodeProcess(HANDLE hProcess, LPDWORD lpExitCode);
BOOL SwitchToThread(VOID);
HANDLE CreateThread(LPSECURITY_ATTRIBUTES lpThreadAttributes,
					SIZE_T dwStackSize,
					LPTHREAD_START_ROUTINE lpStartAddress,
					LPVOID lpParameter,
					DWORD dwCreationFlags,
					LPDWORD lpThreadId
					);
