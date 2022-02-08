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

void report(ReportLevel level, SourceLoc* loc, const char* fmt, ...);
void crash_if_reports(ReportLevel min);
