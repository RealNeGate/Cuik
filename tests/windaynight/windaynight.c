#include <stdbool.h>

#define UNICODE
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shellapi.h>
#include <strsafe.h>



#define RUN_KEY L"Software\\Microsoft\\Windows\\CurrentVersion\\Run"
#define SUB_KEY L"SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
#define SYS_VAL L"SystemUsesLightTheme"
#define APP_VAL L"AppsUseLightTheme"



#define WDN_TITLE L"WinDayNight"
#define WDN_HELP  L"Command line options:\n\ttoggle\n\tlight\n\tdark\n\n"
#define WDN_URL   L"https://github.com/WalterPlinge/windaynight/"



enum Theme { DARK = 0, LIGHT = 1, TOGGLE, TRAY, INVALID = -1,  } typedef Theme;



static HANDLE app_icon;



#define STRING_EQUAL(A, B) (lstrcmpiW(A, B) == 0)



#if defined(_DEBUG)
	#define assert(x) do { if (!(x)) __debugbreak(); } while (0)
#else
	#define assert(x) (void)(x)
#endif

void print_error(LSTATUS status) {
	LPWSTR buf = 0;
	DWORD flags = FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER;
	FormatMessageW(flags, NULL, status, 0, (LPWSTR)&buf, 0, NULL);
	MessageBoxW(NULL, buf, 0, MB_ICONERROR);
	LocalFree(&buf);
}

#if defined(_DEBUG)
	#include <stdarg.h>
	void debug_printf(LPWSTR string, ...) {
		static WCHAR buffer[0xFFF] = {0};
		va_list args = {0};
		va_start(args, string);
		StringCbVPrintfW(buffer, sizeof buffer, string, args);
		MessageBoxW(NULL, buffer, 0, 0);
		va_end(args);
	}
#else
	void debug_printf(LPWSTR string, ...) { }
#endif



Theme parse_args(void) {
	Theme result = INVALID;
	int argc = 0;
	LPWSTR *argv = CommandLineToArgvW(GetCommandLineW(), &argc);
	if (argc == 1) {
		result = TRAY;
	} else
	if (argc == 2) {
		if (STRING_EQUAL(argv[1], L"toggle")) {
			result = TOGGLE;
		} else
		if (STRING_EQUAL(argv[1], L"light")) {
			result = LIGHT;
		} else
		if (STRING_EQUAL(argv[1], L"dark")) {
			result = DARK;
		}
	}
	LocalFree(argv);
	return result;
}



void set_theme(Theme theme) {
	LSTATUS status = 0;
	status = RegSetKeyValueW(HKEY_CURRENT_USER, SUB_KEY, SYS_VAL, REG_DWORD, &theme, sizeof(DWORD));
	if (status != ERROR_SUCCESS) {
		print_error(status);
	}
	status = RegSetKeyValueW(HKEY_CURRENT_USER, SUB_KEY, APP_VAL, REG_DWORD, &theme, sizeof(DWORD));
	if (status != ERROR_SUCCESS) {
		print_error(status);
	}
}

Theme get_theme(void) {
	DWORD data = INVALID;
	DWORD size = sizeof(DWORD);
	LSTATUS status = RegGetValueW(HKEY_CURRENT_USER, SUB_KEY, SYS_VAL, RRF_RT_REG_DWORD, NULL, &data, &size);
	if (status != ERROR_SUCCESS) {
		print_error(status);
		return INVALID;
	}
	if (data != DARK && data != LIGHT) {
		return INVALID;
	}
	return data;
}

void toggle_theme(void) {
	Theme current_theme = get_theme();
	if (current_theme != INVALID) {
		set_theme((current_theme + 1) % 2);
	}
}



static UINT WM_TASKBARCREATED;
#define     WM_WDN_ALREADY_RUNNING (WM_USER + 1)
#define     WM_WDN_COMMAND         (WM_USER + 2)

void add_tray_icon(HWND window) {
	NOTIFYICONDATAW data = {
		.cbSize = sizeof(data),
		.hWnd = window,
		.uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP,
		.uCallbackMessage = WM_WDN_COMMAND,
		.hIcon = app_icon,
	};
	StringCbCopyW(data.szTip, sizeof(data.szTip), WDN_TITLE);
	Shell_NotifyIconW(NIM_ADD, &data);
}

void remove_tray_icon(HWND window) {
	NOTIFYICONDATAW data = {
		.cbSize = sizeof(data),
		.hWnd = window,
	};
	Shell_NotifyIconW(NIM_DELETE, &data);
}

LRESULT window_proc(HWND window, UINT msg, WPARAM wparam, LPARAM lparam) {
	if (msg == WM_CREATE) {
		// BufferedPaintInit();
		add_tray_icon(window);
	} else

	if (msg == WM_CLOSE) {
		DestroyWindow(window);
	} else

	if (msg == WM_DESTROY) {
		remove_tray_icon(window);
		PostQuitMessage(0);
	} else

	if (msg == WM_TASKBARCREATED) {
		// in case taskbar was re-created (explorer.exe crashed) add our icon back
		add_tray_icon(window);
	} else

	if (msg == WM_WDN_ALREADY_RUNNING) {
		MessageBoxW(NULL, L"Already running!", WDN_TITLE, MB_SETFOREGROUND | MB_ICONEXCLAMATION);
	} else

	if (msg == WM_WDN_COMMAND) {
		// single primary click just toggles the theme
		if (LOWORD(lparam) == WM_LBUTTONUP) {
			toggle_theme();
		} else

		// single secondary click just brings up the menu
		if (LOWORD(lparam) == WM_RBUTTONUP) {

			// we need to know if startup is enabled so we can highlight it in the menu
			BOOL startup_enabled = false;
			HKEY startup_key = 0;
			LSTATUS startup_key_status = RegOpenKeyExW(
				HKEY_CURRENT_USER, RUN_KEY, 0,
				KEY_READ | KEY_WRITE, &startup_key);
			if (startup_key_status != ERROR_SUCCESS) {
				print_error(startup_key_status);
			} else {
				WCHAR name[0xFF] = {0};
				for (int i = 0; true; i += 1) {
					DWORD size = sizeof(name) / sizeof(WCHAR);
					LSTATUS status = RegEnumValueW(
						startup_key, i, name, &size,
						NULL, NULL, NULL, NULL);
					if (status == ERROR_NO_MORE_ITEMS) {
						// this is the end of the list of values
						break;
					}
					if (status == ERROR_MORE_DATA) {
						// we don't care about long names, it's not our value
						continue;
					}
					if (status == ERROR_SUCCESS && STRING_EQUAL(name, WDN_TITLE)) {
						startup_enabled = true;
						break;
					}
				}
			}

			// create and structure our menu
			HMENU menu = CreatePopupMenu();
			assert(menu);

			#define CMD_WDN     1
			#define CMD_STARTUP 2
			#define CMD_EXIT    3

			AppendMenuW(menu, MF_STRING, CMD_WDN, WDN_TITLE);
			AppendMenuW(menu, MF_STRING | (startup_enabled ? MF_CHECKED : 0), CMD_STARTUP, L"Run at startup");
			AppendMenuW(menu, MF_STRING, CMD_EXIT, L"Exit");

			// now we've created the menu, our popup window can be shown
			SetForegroundWindow(window);

			POINT mouse = {0};
			GetCursorPos(&mouse);
			UINT cmd_flags = TPM_RETURNCMD | TPM_NONOTIFY | TPM_LEFTBUTTON;
			BOOL cmd = TrackPopupMenuEx(menu, cmd_flags, mouse.x, mouse.y, window, NULL);

			if (cmd == CMD_WDN) {
				ShellExecuteW(NULL, L"open", WDN_URL, NULL, NULL, SW_NORMAL);
			} else

			if (cmd == CMD_STARTUP) {
				if (startup_enabled) {
					LSTATUS status = RegDeleteValueW(startup_key, WDN_TITLE);
					if (status != ERROR_SUCCESS) {
						print_error(status);
					}
				} else {
					// we get path name size in chars without nul
					WCHAR path[0xFFF] = {0};
					DWORD buffer_size_wchars = sizeof(path) / sizeof(WCHAR);
					DWORD path_size_wchars = GetModuleFileNameW(NULL, path, buffer_size_wchars);
					if (!path_size_wchars) {
						DWORD error = GetLastError();
						print_error(error);
					} else {
						// but we need to pass byte size with nul
						DWORD path_size_bytes = path_size_wchars * sizeof(WCHAR) + sizeof(WCHAR);
						LSTATUS status = RegSetValueExW(
							startup_key, WDN_TITLE, 0,
							REG_SZ, (LPSTR)path, path_size_bytes);
						if (status != ERROR_SUCCESS) {
							print_error(status);
						}
					}
				}
			} else

			if (cmd == CMD_EXIT) {
				DestroyWindow(window);
			}

			RegCloseKey(startup_key);
		}
	} else {
		return DefWindowProcW(window, msg, wparam, lparam);
	}

	return 0;
}



void WinMainCRTStartup(void) {
	// handle arguments for use in scripts or task scheduler
	Theme theme = parse_args();
	if (theme == INVALID) {
		MessageBoxW(NULL, WDN_HELP, WDN_TITLE L": Invalid Argument", 0);
		ExitProcess(1);
	} else
	if (theme != TRAY) {
		if (theme == TOGGLE) {
			toggle_theme();
		} else {
			set_theme(theme);
		}
		ExitProcess(0);
	}

	// this is in case taskbar was re-created (explorer.exe crashed) we can add our icon back
	WM_TASKBARCREATED = RegisterWindowMessageW(L"TaskbarCreated");
	assert(WM_TASKBARCREATED);

	HINSTANCE hinstance = GetModuleHandleW(NULL);

	app_icon = LoadImageW(hinstance, MAKEINTRESOURCE(1), IMAGE_ICON, 0, 0, LR_SHARED);
	assert(app_icon);

	WNDCLASSEXW window_class = {
		.cbSize = sizeof(window_class),
		.lpfnWndProc = &window_proc,
		.hInstance = hinstance,
		.hIcon = app_icon,
		.hbrBackground = (HBRUSH)(COLOR_WINDOW+1),
		.lpszClassName = WDN_TITLE,
	};

	// use running instance to notify user that app is already running
	HWND existing = FindWindowW(window_class.lpszClassName, NULL);
	if (existing) {
		PostMessageW(existing, WM_WDN_ALREADY_RUNNING, 0, 0);
		ExitProcess(0);
	}

	ATOM atom = RegisterClassExW(&window_class);
	assert(atom);

	// i think 0 and WS_POPUP mean the window is only tray icon
	HWND app_window = CreateWindowExW(
		0, window_class.lpszClassName, WDN_TITLE, WS_POPUP,
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
		NULL, NULL, window_class.hInstance, NULL);
	if (!app_window) {
		ExitProcess(0);
	}

	while (true) {
		MSG message = {0};
		BOOL result = GetMessageW(&message, NULL, 0, 0);
		if (result == 0) {
			ExitProcess(0);
		}
		assert(result > 0);
		TranslateMessage(&message);
		DispatchMessageW(&message);
	}
}
