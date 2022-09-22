#pragma once
#include "common.h"
#include "preproc/lexer.h"
#include <cuik.h>
#include <threads.h>
#undef ERROR

#define REPORT(lvl, loc, ...) report(REPORT_##lvl, tu->errors, &tu->tokens, loc, __VA_ARGS__)
#define REPORT_RANGED(lvl, a, b, ...) report_ranged(REPORT_##lvl, tu->errors, &tu->tokens, a, b, __VA_ARGS__)
#define REPORT_EXPR(lvl, e, ...) report_ranged(REPORT_##lvl, tu->errors, &tu->tokens, (e)->start_loc, (e)->end_loc, __VA_ARGS__)
#define REPORT_STMT(lvl, s, ...) report(REPORT_##lvl, tu->errors, &tu->tokens, (s)->loc, __VA_ARGS__)

extern mtx_t report_mutex;
extern bool report_using_thin_errors;

typedef struct {
    TokenStream* tokens;
    SourceLocIndex base;

    const char* line_start;
    const char* line_end;

    size_t dist_from_line_start;
    size_t cursor;
} DiagWriter;

DiagWriter diag_writer(TokenStream* tokens);
void diag_writer_highlight(DiagWriter* writer, SourceLocIndex loc);
bool diag_writer_is_compatible(DiagWriter* writer, SourceLocIndex loc);
void diag_writer_done(DiagWriter* writer);

void init_report_system(void);

// loc_msg      |
// loc_msg2     |> are all nullable
// interjection |
void report_two_spots(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc, SourceLocIndex loc2, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection);
void report(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc, const char* fmt, ...);
void report_ranged(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex start_loc, SourceLocIndex end_loc, const char* fmt, ...);
void report_fix(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc, const char* tip, const char* fmt, ...);

// Report primitives
void report_header(Cuik_ReportLevel level, const char* fmt, ...);
void report_line(TokenStream* tokens, SourceLocIndex loci, int indent);

bool has_reports(Cuik_ReportLevel min, Cuik_ErrorStatus* err);
