#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#define ESC "\x1b"
#define CSI "\x1b["

// forwad decls :(
static void input_keyboard(int codepoint);
static void input_resize(int w, int h);
static void input_cursor(int x, int y);

// default to something
static int screen_w = 80, screen_h = 20;

// Platform dependent stuff
#ifdef _WIN32
typedef wchar_t* OS_String;
typedef wchar_t OS_Char;

#define OS_STR(x) L##x
#define OS_STR_FMT "S"

static uint64_t get_last_write_time(OS_String filepath) {
	WIN32_FIND_DATAW data;
	HANDLE handle = FindFirstFileW(filepath, &data);

	ULARGE_INTEGER i;
	i.LowPart = data.ftLastWriteTime.dwLowDateTime;
	i.HighPart = data.ftLastWriteTime.dwHighDateTime;

	FindClose(handle);
	return i.QuadPart;
}

static bool wait_for_file(OS_String filepath) {
	// wait for it to finish writing before trying to compile
	int ticks = 0;
	while (GetFileAttributesW(filepath) == INVALID_FILE_ATTRIBUTES) {
		SleepEx(1, FALSE);

		if (ticks++ > 100) {
			fprintf(stderr, "error: file could not be opened, we timed out!");
			return false;
		}
	}

	SleepEx(100, FALSE);
	return true;
}

static int invoke_compiler(OS_String cmd_line) {
	STARTUPINFOW si = {
		.cb = sizeof(STARTUPINFOW),
		.dwFlags = STARTF_USESTDHANDLES,
		.hStdInput = GetStdHandle(STD_INPUT_HANDLE),
		.hStdError = GetStdHandle(STD_ERROR_HANDLE),
		.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE)
	};
	PROCESS_INFORMATION pi = {};

	if (!CreateProcessW(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
		return EXIT_FAILURE;
	}

	// Wait until child process exits.
	int exit_code = WaitForSingleObject(pi.hProcess, 20000);

	// Close process and thread handles.
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);
	return exit_code;
}

static HANDLE input_handle, output_handle;
static DWORD fdw_old_mode;

static void deinit_terminal() {
	SetConsoleMode(input_handle, fdw_old_mode);
}

static void init_terminal() {
	output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (output_handle == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "error: GetStdHandle for STDOUT failed!\n");
		exit(1);
	}

	CONSOLE_SCREEN_BUFFER_INFO csbi;
	if (!GetConsoleScreenBufferInfo(output_handle, &csbi)) {
		fprintf(stderr, "error: GetConsoleScreenBufferInfo failed!\n");
		exit(1);
	}

	screen_w = csbi.srWindow.Right, screen_h = csbi.srWindow.Bottom;

	input_handle = GetStdHandle(STD_INPUT_HANDLE);
    if (input_handle == INVALID_HANDLE_VALUE) {
		fprintf(stderr, "error: GetStdHandle failed!\n");
		exit(1);
	}

    // Save the current input mode, to be restored on exit.
	if (!GetConsoleMode(input_handle, &fdw_old_mode)) {
        fprintf(stderr, "error: GetConsoleMode failed!\n");
		exit(1);
	}
	atexit(deinit_terminal);

    // Enable the window and mouse input events.
    DWORD fdw_mode = ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT;
    if (!SetConsoleMode(input_handle, fdw_mode)) {
        fprintf(stderr, "error: SetConsoleMode failed! %lu\n", GetLastError());
		exit(1);
	}
}

// calls the input_* functions on events
static void poll_events() {
	while (true) {
		DWORD count;
		INPUT_RECORD input;
		if (!ReadConsoleInput(input_handle, &input, 1, &count)) {
			fprintf(stderr, "error: ReadConsoleInput failed!\n");
			exit(1);
		}

		// nothing for now
		if (count == 0) return;

		switch (input.EventType) {
			case KEY_EVENT: {
				if (input.Event.KeyEvent.bKeyDown) {
					// Ctrl+C
					if ((input.Event.KeyEvent.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) != 0 &&
						input.Event.KeyEvent.wVirtualKeyCode == 'C') {
						exit(1);
					}

					input_keyboard(input.Event.KeyEvent.uChar.UnicodeChar);
				}
				break;
			}
			case MOUSE_EVENT: {
				COORD c = input.Event.MouseEvent.dwMousePosition;
				input_cursor(c.X, c.Y);
				break;
			}
			case WINDOW_BUFFER_SIZE_EVENT: {
				COORD c = input.Event.WindowBufferSizeEvent.dwSize;
				input_resize(c.X, c.Y);
				break;
			}
			default: break;
		}
	}
}
#else
typedef char* OS_String;
typedef char OS_Char;

#define OS_STR(x) x
#define OS_STR_FMT "s"

#error "TODO: Implement this"
#endif

static char options[300], option_cursor;

static void redraw_screen() {
	// Clear screen
	printf(CSI "1;1H");
	printf(CSI "2J");

	// Draw line at the bottom separating options input from assembly listing
	printf(CSI "%d;1H", screen_h-1);
	for (int i = 0; i < screen_w; i++) printf("#");
}

static void input_keyboard(int codepoint) {

}

static void input_resize(int w, int h) {
	screen_w = w;
	screen_h = h;

	redraw_screen();
}

static void input_cursor(int x, int y) {

}

#define O(string, ...) fprintf(stdout, string "\n", ##__VA_ARGS__)
static void print_help(const char* executable_path) {
	O("Usage: %s [<options>] <file> [<args>]", executable_path ? executable_path : "live-cuik");
}
#undef O

int main(int argc, char** argv) {
	if (argc < 2) {
		fprintf(stderr, "error: expected arguments\n");
		print_help(argc == 0 ? NULL : argv[0]);
		return EXIT_FAILURE;
	}

	init_terminal();
	redraw_screen();

	while (true) {
		poll_events();
	}

	return 0;
}
