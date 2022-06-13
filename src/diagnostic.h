#pragma once
#include "common.h"
#include "front/lexer.h"
#include <ext/threads.h>
#undef ERROR

typedef enum {
    REPORT_VERBOSE,
    REPORT_INFO,
    REPORT_WARNING,
    REPORT_ERROR,
    REPORT_MAX
} ReportLevel;

#define REPORT(lvl, loc, ...) report(REPORT_##lvl, &tu->tokens, loc, __VA_ARGS__)
#define REPORT_RANGED(lvl, a, b, ...) report_ranged(REPORT_##lvl, &tu->tokens a, b, __VA_ARGS__)
#define REPORT_EXPR(lvl, e, ...) report_ranged(REPORT_##lvl, &tu->tokens, (e)->start_loc, (e)->end_loc, __VA_ARGS__)
#define REPORT_STMT(lvl, s, ...) report(REPORT_##lvl, &tu->tokens, (s)->loc, __VA_ARGS__)

extern mtx_t report_mutex;
extern bool report_using_thin_errors;

void init_report_system();

// loc_msg      |
// loc_msg2     |> are all nullable
// interjection |
void report_two_spots(ReportLevel level, TokenStream* tokens, SourceLocIndex loc, SourceLocIndex loc2, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection);
void report(ReportLevel level, TokenStream* tokens, SourceLocIndex loc, const char* fmt, ...);
void report_ranged(ReportLevel level, TokenStream* tokens, SourceLocIndex start_loc, SourceLocIndex end_loc, const char* fmt, ...);

void crash_if_reports(ReportLevel min);
void clear_any_reports();
