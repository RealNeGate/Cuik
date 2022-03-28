#include "diagnostic.h"
#include <stdarg.h>
#include <ctype.h>

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

static void display_line(ReportLevel level, SourceLoc* loc) {
#if _WIN32
	if (console_handle == NULL) {
		console_handle = GetStdHandle(STD_OUTPUT_HANDLE);
		
		CONSOLE_SCREEN_BUFFER_INFO info;
		GetConsoleScreenBufferInfo(console_handle, &info);
		
		default_attribs = info.wAttributes;
	}
#endif
	
	if (report_using_thin_errors) {
		printf("%s:%d:%d: ", loc->file, loc->line, loc->columns);
		print_level_name(level);
	} else {
		print_level_name(level);
		printf("%s:%d:%d: ", loc->file, loc->line, loc->columns);
	}
}

static void tally_report_counter(ReportLevel level) {
	tally[level] += 1;
	if (level > REPORT_WARNING && tally[level] > 20) {
#if _WIN32
		SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_RED | FOREGROUND_INTENSITY);
#endif
		
		printf("EXCEEDED ERROR LIMIT OF 20\n");
		
#if _WIN32
		SetConsoleTextAttribute(console_handle, default_attribs);
#endif
		abort();
	}
}

static size_t draw_line(SourceLoc* loc) {
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
	return dist_from_line_start;
}

static void draw_line_horizontal_pad() {
	printf("      | ");
}

void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...) {
	display_line(level, loc);
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	
	if (!report_using_thin_errors) {
		size_t dist_from_line_start = draw_line(loc);
		
#if _WIN32
		SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
#endif
		
		// idk man
		size_t start_pos = loc->columns > dist_from_line_start 
			? loc->columns - dist_from_line_start : 0;
		
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
	
	tally_report_counter(level);
}

void report_two_spots(ReportLevel level, SourceLoc* loc, SourceLoc* loc2, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection) {
	if (!interjection && loc->line == loc2->line) {
		assert(loc->columns < loc2->columns);
		
		display_line(level, loc);
		printf("%s\n", msg);
		
		if (!report_using_thin_errors) {
			size_t dist_from_line_start = draw_line(loc);
			
#if _WIN32
			SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
#endif
			
			// draw underline
			size_t first_start_pos = loc->columns > dist_from_line_start 
				? loc->columns - dist_from_line_start : 0;
			size_t first_end_pos = first_start_pos + loc->length;
			
			size_t second_start_pos = loc2->columns > dist_from_line_start 
				? loc2->columns - dist_from_line_start : 0;
			size_t second_end_pos = second_start_pos + loc2->length;
			
			// First
			for (size_t i = 0; i < first_start_pos; i++) printf(" ");
			printf("^");
			for (size_t i = first_start_pos + 1; i < first_end_pos; i++) printf("~");
			
			// Second
			for (size_t i = first_end_pos; i < second_start_pos; i++) printf(" ");
			printf("^");
			for (size_t i = second_start_pos + 1; i < second_end_pos; i++) printf("~");
			printf("\n");
			
#if _WIN32
			SetConsoleTextAttribute(console_handle, default_attribs);
#endif
			
			draw_line_horizontal_pad();
			
			size_t loc_msg_len = strlen(loc_msg);
			//size_t loc_msg2_len = strlen(loc_msg2);
			
			for (size_t i = 0; i < first_start_pos; i++) printf(" ");
			printf("%s", loc_msg);
			for (size_t i = first_start_pos+loc_msg_len; i < second_start_pos; i++) printf(" ");
			printf("%s", loc_msg2);
			printf("\n");
		}
	} else {
		display_line(level, loc);
		printf("%s\n", msg);
		
		if (!report_using_thin_errors) {
			{
				size_t dist_from_line_start = draw_line(loc);
				
#if _WIN32
				SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
#endif
				
				// draw underline
				size_t start_pos = loc->columns > dist_from_line_start 
					? loc->columns - dist_from_line_start : 0;
				
				size_t tkn_len = loc->length;
				for (size_t i = 0; i < start_pos; i++) printf(" ");
				printf("^");
				for (size_t i = 1; i < tkn_len; i++) printf("~");
				printf("\n");
				
#if _WIN32
				SetConsoleTextAttribute(console_handle, default_attribs);
#endif
				
				if (loc_msg) {
					draw_line_horizontal_pad();
					for (size_t i = 0; i < start_pos; i++) printf(" ");
					printf("%s\n", loc_msg);
				}
			}
			
			if (interjection) {
				printf("  %s\n", interjection);
				draw_line_horizontal_pad();
				printf("\n");
			} else {
				draw_line_horizontal_pad();
				printf("\n");
			}
			
			{
				size_t dist_from_line_start = draw_line(loc2);
				
#if _WIN32
				SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
#endif
				
				// draw underline
				size_t start_pos = loc2->columns > dist_from_line_start 
					? loc2->columns - dist_from_line_start : 0;
				
				size_t tkn_len = loc2->length;
				for (size_t i = 0; i < start_pos; i++) printf(" ");
				printf("^");
				for (size_t i = 1; i < tkn_len; i++) printf("~");
				printf("\n");
				
#if _WIN32
				SetConsoleTextAttribute(console_handle, default_attribs);
#endif
				
				if (loc_msg2) {
					draw_line_horizontal_pad();
					for (size_t i = 0; i < start_pos; i++) printf(" ");
					printf("%s\n", loc_msg2);
				}
			}
		}
	}
	
	printf("\n");
	tally_report_counter(level);
}

void crash_if_reports(ReportLevel min) {
	for (int i = min; i < REPORT_MAX; i++) {
		if (tally[i]) {
			printf("exited with %d %s%s", tally[i], report_names[i], tally[i] > 1 ? "s" : "");
			
			abort();
		}
	}
}

void clear_any_reports() {
	memset(tally, 0, sizeof(tally));
}
