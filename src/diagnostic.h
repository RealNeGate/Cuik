#pragma once
#include "common.h"
#include "front/lexer.h"

typedef enum {
	REPORT_VERBOSE,
	REPORT_INFO,
	REPORT_WARNING,
	REPORT_ERROR,
	REPORT_MAX
} ReportLevel;

extern bool report_using_thin_errors;

void init_report_system();
void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...);

// loc_msg      | 
// loc_msg2     |> are all nullable
// interjection |
void report_two_spots(ReportLevel level, SourceLoc* loc, SourceLoc* loc2, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection);

void crash_if_reports(ReportLevel min);
void clear_any_reports();
