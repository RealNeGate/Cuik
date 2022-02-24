#include "diagnostic.h"
#include <stdarg.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

static const char* report_names[] = {
	"verbose",
	"info",
	"warning",
	"error"
};

static int tally[REPORT_MAX] = {};

#if _WIN32
static HANDLE console_handle;
static WORD default_attribs;

const static int attribs[] = {
	FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY,
	FOREGROUND_GREEN | FOREGROUND_INTENSITY,
	FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
	FOREGROUND_RED | FOREGROUND_INTENSITY
};
#endif

bool report_using_thin_errors = false;

static void print_level_name(ReportLevel level) {
#if _WIN32
	SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | attribs[level]);
	printf("%s: ", report_names[level]);
	SetConsoleTextAttribute(console_handle, default_attribs);
#else
	printf("%s: ", report_names[level]);
#endif
}

void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...) {
	if (console_handle == NULL) {
		console_handle = GetStdHandle(STD_OUTPUT_HANDLE);
		
		CONSOLE_SCREEN_BUFFER_INFO info;
		GetConsoleScreenBufferInfo(console_handle, &info);
		
		default_attribs = info.wAttributes;
	}
	
	if (report_using_thin_errors) {
		printf("%s:%d:%d: ", loc->file, loc->line, loc->columns);
		print_level_name(level);
	} else {
		print_level_name(level);
		printf("%s:%d:%d:\n", loc->file, loc->line, loc->columns);
	}
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	
	if (!report_using_thin_errors) {
		// display line
		const char* line = (const char*)loc->line_str;
		while (*line && isspace(*line)) { line++; }
		size_t dist_from_line_start = line - (const char*)loc->line_str;
		
		if (*line != '\n') {
			const char* line_end = line;
			do { line_end++; } while (*line_end && *line_end != '\n');
			
			printf(" %5d| %.*s\n", loc->line, (int)(line_end - line), line);
		}
		printf("      | ");
		
#if _WIN32
		// TODO(NeGate): figure out how to make it green
		SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
#endif
		
		// idk man
		size_t start_pos = loc->columns > dist_from_line_start 
			? loc->columns - dist_from_line_start : loc->columns;
		
		// draw underline
		size_t tkn_len = loc->length;
		for (size_t i = 0; i < start_pos; i++) printf(" ");
		printf("^");
		for (size_t i = 1; i < tkn_len; i++) printf("~");
		printf("\n");
		
#if _WIN32
		SetConsoleTextAttribute(console_handle, default_attribs);
#endif
	}
	
	tally[level] += 1;
	if (level > REPORT_WARNING && tally[level] > 20) {
		printf("exceeded error limit of 20\n");
		abort();
	}
}

void crash_if_reports(ReportLevel min) {
	for (int i = min; i < REPORT_MAX; i++) {
		if (tally[i]) {
			printf("exitted with %d %s%s", tally[i], report_names[i], tally[i] > 1 ? "s" : "");
			abort();
		}
	}
}

void clear_any_reports() {
	memset(tally, 0, sizeof(tally));
}
