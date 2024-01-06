#pragma once
#include "common.h"
#include "preproc/lexer.h"
#include <cuik.h>
#include <threads.h>

typedef struct DiagFixit {
    SourceRange loc;
    int offset;
    const char* hint;
} DiagFixit;

typedef struct {
    TokenStream* tokens;
    ResolvedSourceLoc base;

    const char* line_start;
    const char* line_end;

    size_t dist_from_line_start;
    size_t cursor;
} DiagWriter;

struct Cuik_Diagnostics {
    Cuik_DiagCallback callback;
    void* userdata;

    TB_Arena* buffer;

    // We write the text output to a buffer such that we
    // can do ordered output.
    Cuik_Parser* parser;

    // Incremented atomically by the diagnostics engine
    _Atomic int error_tally;
};

typedef enum Cuik_ReportLevel {
    REPORT_VERBOSE,
    REPORT_INFO,
    REPORT_WARNING,
    REPORT_ERROR,
    REPORT_MAX
} Cuik_ReportLevel;

int cuikdg_error_count(TokenStream* s);
void cuikdg_tally_error(TokenStream* s);

Cuik_Diagnostics* cuikdg_make(Cuik_DiagCallback callback, void* userdata);
void cuikdg_free(Cuik_Diagnostics* diag);

////////////////////////////////
// Complex diagnostic builder
////////////////////////////////
void diag_header(TokenStream* tokens, DiagType type, const char* fmt, ...);

DiagWriter diag_writer(TokenStream* tokens);
void diag_writer_highlight(DiagWriter* writer, SourceRange loc);
bool diag_writer_is_compatible(DiagWriter* writer, SourceRange loc);
void diag_writer_done(DiagWriter* writer);

static void report_two_spots(Cuik_ReportLevel level, TokenStream* tokens, SourceLoc loc, SourceLoc loc2, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection) {}
static void report(Cuik_ReportLevel level, TokenStream* tokens, SourceLoc loc, const char* fmt, ...) {}
static void report_ranged(Cuik_ReportLevel level, TokenStream* tokens, SourceLoc start_loc, SourceLoc end_loc, const char* fmt, ...) {}
static void report_fix(Cuik_ReportLevel level, TokenStream* tokens, SourceLoc loc, const char* tip, const char* fmt, ...) {}

// Report primitives
static void report_header(Cuik_ReportLevel level, const char* fmt, ...) {}
static void report_line(TokenStream* tokens, SourceLoc loci, int indent) {}
