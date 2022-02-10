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

void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...);
void crash_if_reports(ReportLevel min);
