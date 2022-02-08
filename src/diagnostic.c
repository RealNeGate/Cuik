#include "diagnostic.h"
#include <stdarg.h>

#if _WIN32
#include <windows.h>
#endif

static const char* report_names[] = {
	"verbose",
	"info",
	"warning",
	"error"
};

static int tally[REPORT_MAX] = {};

void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...) {
	printf("%s:%d:%d: ", loc->file, loc->line, loc->columns);
	
#if _WIN32
	HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
	
	const static int attribs[] = {
		FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY,
		FOREGROUND_GREEN,
		FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
		FOREGROUND_RED | FOREGROUND_INTENSITY
	};
	
	SetConsoleTextAttribute(hConsole, attribs[level]);
	printf("%s: ", report_names[level]);
	SetConsoleTextAttribute(hConsole, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN);
#else
	printf("%s: ", report_names[level]);
#endif
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	
	// display line
	const char* line = (const char*)(loc->at - loc->columns);
	const char* line_end = line;
	do { line_end++; } while (*line_end && *line_end != '\n');
	
	printf("\t%.*s\n", (int)(line_end - line), line);
	
	// draw cursor
	printf("\t");
	
#if _WIN32
	SetConsoleTextAttribute(hConsole, FOREGROUND_GREEN);
#endif
	
	size_t start_pos = loc->columns;
	size_t tkn_len = loc->length;
	for (size_t i = 0; i < start_pos; i++) printf(" ");
	printf("^");
	for (size_t i = 1; i < tkn_len; i++) printf("~");
	printf("\n");
	
#if _WIN32
	SetConsoleTextAttribute(hConsole, FOREGROUND_RED | FOREGROUND_BLUE | FOREGROUND_GREEN);
#endif
	
	tally[level] += 1;
	if (level > REPORT_WARNING && tally[level] > 20) {
		printf("exceeded error limit of 20\n");
		abort();
	}
}

void crash_if_reports(ReportLevel min) {
	for (int i = min; i < REPORT_MAX; i++) {
		if (tally[i]) {
			printf("exitted with %d %ss", tally[i], report_names[i]);
			abort();
		}
	}
}
