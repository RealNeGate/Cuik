#include "diagnostic.h"
#include <locale.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdatomic.h>
#include "preproc/lexer.h"

// We extend on stb_sprintf with support for a custom callback because
// diagnostic formats have extra options.
static const char* custom_diagnostic_format(uint32_t* out_length, char ch, va_list* va);

#define STB_SPRINTF_STATIC
#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

static const char* report_names[] = {
    "\x1b[32mnote\x1b[0m",
    "\x1b[33mwarn\x1b[0m",
    "\x1b[31merror\x1b[0m",
};

mtx_t report_mutex;
bool report_using_thin_errors = false;

// From types.c, we should factor this out into a public cuik function
size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type);

static char* sprintf_callback(const char* buf, void* user, int len) {
    fwrite(buf, len, 1, (FILE*) user);
    return NULL;
}

static const char* custom_diagnostic_format(uint32_t* out_length, char ch, va_list* va) {
    static _Thread_local char temp_string[1024];

    switch (ch) {
        case 'S': {
            String str = va_arg(*va, String);

            *out_length = str.length;
            return (const char*) str.data;
        }

        case 'T': {
            Cuik_Type* t = va_arg(*va, Cuik_Type*);
            type_as_string(sizeof(temp_string), temp_string, t);

            *out_length = strlen(temp_string);
            return temp_string;
        }

        default:
        fprintf(stderr, "diagnostic error: unknown format %%_%c", ch);
        abort();
        return NULL;
    }
}

#define RESET_COLOR     printf("\x1b[0m")
#define SET_COLOR_RED   printf("\x1b[31m")
#define SET_COLOR_GREEN printf("\x1b[32m")
#define SET_COLOR_WHITE printf("\x1b[37m")

void cuikdg_init(void) {
    // setvbuf(stdout, NULL, _IONBF, 0);
    // setvbuf(stderr, NULL, _IONBF, 0);
    setlocale(LC_ALL, ".UTF8");

    #if _WIN32
    // Enable ANSI/VT sequences on windows
    HANDLE output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
    if (output_handle != INVALID_HANDLE_VALUE) {
        DWORD old_mode;
        if (GetConsoleMode(output_handle, &old_mode)) {
            SetConsoleMode(output_handle, old_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
        }
    }
    #endif

    mtx_init(&report_mutex, mtx_plain | mtx_recursive);
}

// we use the call stack so we can print in reverse order
void print_include(TokenStream* tokens, SourceLoc loc) {
    ResolvedSourceLoc r = cuikpp_find_location(tokens, loc);

    if (r.file->include_site.raw != 0) {
        print_include(tokens, r.file->include_site);
    }

    fprintf(stderr, "Included from %s:%d\n", r.file->filename, r.line);
}

static void print_line(TokenStream* tokens, ResolvedSourceLoc start, size_t tkn_len) {
    const char* line_start = start.line_str;
    while (*line_start && isspace(*line_start)) line_start++;
    size_t dist_from_line_start = line_start - start.line_str;

    // line preview
    if (*line_start != '\r' && *line_start != '\n') {
        const char* line_end = line_start;
        do { line_end++; } while (*line_end && *line_end != '\n');

        fprintf(stderr, "%6d| %.*s\x1b[37m\n", start.line, (int)(line_end - line_start), line_start);
    }

    // underline
    size_t start_pos = start.column > dist_from_line_start ? start.column - dist_from_line_start : 0;
    fprintf(stderr, "        ");
    fprintf(stderr, "\x1b[32m");

    for (size_t i = 0; i < start_pos; i++) fprintf(stderr, " ");
    fprintf(stderr, "^");
    for (size_t i = 1; i < tkn_len; i++) fprintf(stderr, "~");

    fprintf(stderr, "\x1b[0m\n");
}

// end goes after start
static ptrdiff_t source_range_difference(TokenStream* tokens, Cuik_FileLoc s, Cuik_FileLoc e) {
    ResolvedSourceLoc l = cuikpp_find_location2(tokens, s);
    ResolvedSourceLoc end = cuikpp_find_location2(tokens, e);

    size_t tkn_len;
    if (end.file == l.file && end.line == l.line && end.column > l.column) {
        return end.column - l.column;
    } else {
        return 1;
    }
}

static void print_line_with_backtrace(TokenStream* tokens, SourceLoc loc, SourceLoc end) {
    // find the range of chars in this level
    Cuik_FileLoc l;
    size_t tkn_len;

    uint32_t macro_off = loc.raw & ((1u << SourceLoc_MacroOffsetBits) - 1);
    MacroInvoke* m = cuikpp_find_macro(tokens, loc);
    if (m != NULL) {
        l = cuikpp_find_location_in_bytes(tokens, m->def_site.start);
        l.pos += macro_off;

        assert(end.raw > loc.raw);
        tkn_len = end.raw - loc.raw;
    } else {
        l = cuikpp_find_location_in_bytes(tokens, loc);
        tkn_len = source_range_difference(tokens, l, cuikpp_find_location_in_bytes(tokens, end));
    }

    if (m != NULL) {
        Cuik_File* next_file = cuikpp_find_file(tokens, m->call_site);
        print_line_with_backtrace(tokens, m->call_site, offset_source_loc(m->call_site, m->name.length));
        if (next_file->filename != l.file->filename) {
            fprintf(stderr, "  expanded from %s:\n", l.file->filename);
        } else {
            fprintf(stderr, "  expanded from:\n");
        }
    }

    print_line(tokens, cuikpp_find_location2(tokens, l), tkn_len);
}

static void diag(DiagType type, TokenStream* tokens, SourceRange loc, const char* fmt, va_list ap) {
    size_t fixit_count = 0;
    DiagFixit fixits[3];
    while (*fmt == '#') {
        assert(fixit_count < 3);
        fixits[fixit_count++] = va_arg(ap, DiagFixit);
        fmt += 1;
    }

    SourceLoc loc_start = loc.start;
    // we wanna find the physical character
    for (MacroInvoke* m; (m = cuikpp_find_macro(tokens, loc_start)) != NULL;) {
        loc_start = m->call_site;
    }

    // print include stack
    ResolvedSourceLoc start = cuikpp_find_location(tokens, loc_start);
    if (start.file->include_site.raw != 0) {
        print_include(tokens, start.file->include_site);
    }

    char tmp[STB_SPRINTF_MIN];
    fprintf(stderr, "\x1b[37m");
    fprintf(stderr, "%s:%d:%d: ", start.file->filename, start.line, start.column);
    fprintf(stderr, "%s: ", report_names[type]);
    stbsp_vsprintfcb(sprintf_callback, stderr, tmp, fmt, ap);
    fprintf(stderr, "\n");

    SourceLoc loc_end = loc.end;
    // we wanna find the physical character
    for (MacroInvoke* m; (m = cuikpp_find_macro(tokens, loc_end)) != NULL;) {
        loc_end = m->call_site;
    }

    // print_line(tokens, start, tkn_len);
    print_line_with_backtrace(tokens, loc.start, loc.end);

    {
        const char* line_start = start.line_str;
        while (*line_start && isspace(*line_start)) line_start++;
        size_t dist_from_line_start = line_start - start.line_str;

        for (int i = 0; i < fixit_count; i++) {
            size_t start_pos = start.column > dist_from_line_start ? start.column - dist_from_line_start : 0;
            start_pos += fixits[i].offset;

            fprintf(stderr, "        \x1b[32m");
            for (size_t j = 0; j < start_pos; j++) fprintf(stderr, " ");
            fprintf(stderr, "%s\x1b[0m\n", fixits[i].hint);
        }
    }

    if (type == DIAG_ERR) {
        atomic_fetch_add((atomic_int*) tokens->error_tally, 1);
    }
}

void diag_header(DiagType type, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char tmp[STB_SPRINTF_MIN];
    fprintf(stderr, "\x1b[37m%s: ", report_names[type]);
    stbsp_vsprintfcb(sprintf_callback, stderr, tmp, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
}

void cuikdg_tally_error(TokenStream* s) {
    atomic_fetch_add((atomic_int*) s->error_tally, 1);
}

int cuikdg_error_count(TokenStream* s) {
    return atomic_load((atomic_int*) s->error_tally);
}

#define DIAG_FN(type, name) \
void name(TokenStream* tokens, SourceRange loc, const char* fmt, ...) { \
    va_list ap;                       \
    va_start(ap, fmt);                \
    diag(type, tokens, loc, fmt, ap); \
    va_end(ap);                       \
}

DIAG_FN(DIAG_NOTE, diag_note);
DIAG_FN(DIAG_WARN, diag_warn);
DIAG_FN(DIAG_ERR,  diag_err);

DiagWriter diag_writer(TokenStream* tokens) {
    return (DiagWriter){ .tokens = tokens };
}

static void diag_writer_write_upto(DiagWriter* writer, size_t pos) {
    if (writer->cursor < pos) {
        int l = pos - writer->cursor;
        for (int i = 0; i < l; i++) printf(" ");

        //printf("%.*s", (int)(pos - writer->cursor), writer->line_start + writer->cursor);
        writer->cursor = pos;
    }
}

void diag_writer_highlight(DiagWriter* writer, SourceRange loc) {
    ResolvedSourceLoc a = cuikpp_find_location(writer->tokens, loc.start);
    ResolvedSourceLoc b = cuikpp_find_location(writer->tokens, loc.end);

    assert(a.file == b.file && a.line == b.line);
    if (writer->base.file == NULL) {
        // display line
        const char* line_start = a.line_str;
        while (*line_start && isspace(*line_start)) {
            line_start++;
        }
        size_t dist_from_line_start = line_start - a.line_str;

        const char* line_end = line_start;
        do { line_end++; } while (*line_end && *line_end != '\n');

        writer->base = a;
        writer->line_start = line_start;
        writer->line_end = line_end;
        writer->dist_from_line_start = line_start - a.line_str;

        printf("  %s:%d\n", a.file->filename, a.line);
        printf("    ");
        printf("%.*s\n", (int) (line_end - line_start), line_start);
        printf("    ");
    }

    assert(b.column >= a.column);
    size_t start_pos = a.column > writer->dist_from_line_start ? a.column - writer->dist_from_line_start : 0;
    size_t tkn_len = b.column - a.column;
    if (tkn_len == 0) tkn_len = 1;

    diag_writer_write_upto(writer, start_pos);
    //printf("\x1b[7m");
    //diag_writer_write_upto(writer, start_pos + tkn_len);
    printf("\x1b[32m^");
    for (int i = 1; i < tkn_len; i++) printf("~");
    writer->cursor = start_pos + tkn_len;
    printf("\x1b[0m");
}

bool diag_writer_is_compatible(DiagWriter* writer, SourceRange loc) {
    if (writer->base.file == NULL) {
        return true;
    }

    ResolvedSourceLoc r = cuikpp_find_location(writer->tokens, loc.start);
    return writer->base.file == r.file && writer->base.line == r.line;
}

void diag_writer_done(DiagWriter* writer) {
    if (writer->base.file != NULL) {
        diag_writer_write_upto(writer, writer->line_end - writer->line_start);
        printf("\n");
    }
}

#if 0
static void highlight_line(TokenStream* tokens, SourceLocIndex loc_index, SourceLoc* loc) {
    SourceLine* line = GET_SOURCE_LOC(loc_index)->line;

    // display line
    const char* line_start = (const char*)line->line_str;
    while (*line_start && isspace(*line_start)) {
        line_start++;
    }
    size_t dist_from_line_start = line_start - (const char*)line->line_str;

    // Layout token
    size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;
    size_t tkn_len = loc->length;

    // Draw line preview
    if (*line_start != '\r' && *line_start != '\n') {
        const char* line_end = line_start;
        do {
            line_end++;
        } while (*line_end && *line_end != '\n');
        size_t line_len = line_end - line_start;

        printf("%4d| ", line->line);
        printf("%.*s", (int) start_pos, line_start);
        printf("%.*s", (int) tkn_len, line_start + start_pos);
        printf("\x1b[0m");
        printf("%.*s", (int) (line_len - (start_pos + tkn_len)), line_start + start_pos + tkn_len);
        printf("\n");
    }
}

static void preview_line(TokenStream* tokens, SourceLocIndex loc_index, SourceLoc* loc, const char* tip) {
    if (!report_using_thin_errors) {
        size_t dist_from_line_start = draw_line(tokens, loc_index);
        draw_line_horizontal_pad();

        SET_COLOR_GREEN;

        // idk man
        size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;
        size_t tkn_len = loc->length;
        if (tip) {
            start_pos += loc->length;
            tkn_len = strlen(tip);
        }

        // draw underline
        for (size_t i = 0; i < start_pos; i++) printf(" ");
        printf("^");
        for (size_t i = 1; i < tkn_len; i++) printf("~");
        printf("\n");

        if (tip) {
            draw_line_horizontal_pad();
            for (size_t i = 0; i < start_pos; i++) printf(" ");
            printf("%s\n", tip);
        }

        RESET_COLOR;
    }
}

static void preview_expansion(TokenStream* tokens, SourceLoc* loc) {
    if (loc->line->parent != 0) {
        SourceLoc* parent = GET_SOURCE_LOC(loc->line->parent);

        if (parent->expansion != 0) {
            SourceLoc* expansion = GET_SOURCE_LOC(parent->expansion);

            display_line(REPORT_INFO, tokens, expansion);
            printf("macro '%.*s' defined at\n", (int)expansion->length, expansion->line->line_str + expansion->columns);
            preview_line(tokens, parent->expansion, expansion, NULL);
        }
    }
    printf("\n");
}

void report_header(Cuik_ReportLevel level, const char* fmt, ...) {
    print_level_name(level);

    SET_COLOR_WHITE;
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);

    printf("\n");
    RESET_COLOR;
}

void report_line(TokenStream* tokens, SourceLocIndex loci, int indent) {
    SourceLoc* loc = GET_SOURCE_LOC(loci);
    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loci = loc->line->parent;
        loc = GET_SOURCE_LOC(loci);
    }

    for (int i = 0; i < indent; i++) printf(" ");
    printf("%s:%d:%d\n", loc->line->filepath, loc->line->line, loc->columns);
    highlight_line(tokens, loci, loc);
}

void report_ranged(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex start_loc, SourceLocIndex end_loc, const char* fmt, ...) {
    SourceLoc loc = merge_source_locations(tokens, start_loc, end_loc);

    mtx_lock(&report_mutex);
    if (!report_using_thin_errors && loc.line->parent != 0) {
        print_backtrace(tokens, loc.line->parent, loc.line);
    }

    display_line(level, tokens, &loc);

    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);

    printf("\n");
    RESET_COLOR;

    preview_line(tokens, start_loc, &loc, NULL);
    preview_expansion(tokens, &loc);

    tally_report_counter(level, err);
    mtx_unlock(&report_mutex);
}

void report(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc_index, const char* fmt, ...) {
    SourceLoc* loc = GET_SOURCE_LOC(loc_index);

    mtx_lock(&report_mutex);
    if (!report_using_thin_errors && loc->line->parent != 0) {
        print_backtrace(tokens, loc->line->parent, loc->line);
    }

    display_line(level, tokens, loc);

    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);

    printf("\n");
    RESET_COLOR;

    preview_line(tokens, loc_index, loc, NULL);
    preview_expansion(tokens, loc);

    tally_report_counter(level, err);
    mtx_unlock(&report_mutex);
}

void report_fix(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc_index, const char* tip, const char* fmt, ...) {
    SourceLoc* loc = GET_SOURCE_LOC(loc_index);

    mtx_lock(&report_mutex);
    if (!report_using_thin_errors && loc->line->parent != 0) {
        print_backtrace(tokens, loc->line->parent, loc->line);
    }

    display_line(level, tokens, loc);

    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);

    printf("\n");
    RESET_COLOR;

    preview_line(tokens, loc_index, loc, tip);
    preview_expansion(tokens, loc);

    if (loc->line->parent != 0) {
        SourceLoc* parent = GET_SOURCE_LOC(loc->line->parent);
        if (parent->expansion != 0) {
            report(level, err, tokens, parent->expansion, "Expanded from");
        }
    }

    tally_report_counter(level, err);
    mtx_unlock(&report_mutex);
}

void report_two_spots(Cuik_ReportLevel level, Cuik_ErrorStatus* err, TokenStream* tokens, SourceLocIndex loc_index, SourceLocIndex loc2_index, const char* msg, const char* loc_msg, const char* loc_msg2, const char* interjection) {
    SourceLoc* loc = GET_SOURCE_LOC(loc_index);
    SourceLoc* loc2 = GET_SOURCE_LOC(loc2_index);

    mtx_lock(&report_mutex);

    if (!interjection && loc->line->line == loc2->line->line) {
        assert(loc->columns < loc2->columns);

        display_line(level, tokens, loc);
        printf("%s\n", msg);
        RESET_COLOR;

        if (!report_using_thin_errors) {
            size_t dist_from_line_start = draw_line(tokens, loc_index);
            draw_line_horizontal_pad();

            // draw underline
            size_t first_start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;
            size_t first_end_pos = first_start_pos + loc->length;

            size_t second_start_pos = loc2->columns > dist_from_line_start ? loc2->columns - dist_from_line_start : 0;
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

            draw_line_horizontal_pad();

            size_t loc_msg_len = strlen(loc_msg);
            //size_t loc_msg2_len = strlen(loc_msg2);

            for (size_t i = 0; i < first_start_pos; i++) printf(" ");
            printf("%s", loc_msg);
            for (size_t i = first_start_pos + loc_msg_len; i < second_start_pos; i++) printf(" ");
            printf("%s", loc_msg2);
            printf("\n");
        }
    } else {
        display_line(level, tokens, loc);
        printf("%s\n", msg);

        if (!report_using_thin_errors) {
            {
                size_t dist_from_line_start = draw_line(tokens, loc_index);
                draw_line_horizontal_pad();

                // draw underline
                size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

                size_t tkn_len = loc->length;
                for (size_t i = 0; i < start_pos; i++) printf(" ");
                printf("^");
                for (size_t i = 1; i < tkn_len; i++) printf("~");
                printf("\n");

                if (loc_msg) {
                    draw_line_horizontal_pad();
                    for (size_t i = 0; i < start_pos; i++) printf(" ");
                    printf("%s\n", loc_msg);
                }
            }

            if (loc->line->filepath != loc2->line->filepath) {
                printf("  meanwhile in... %s\n", loc2->line->filepath);
                draw_line_horizontal_pad();
                printf("\n");
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
                size_t dist_from_line_start = draw_line(tokens, loc2_index);
                draw_line_horizontal_pad();

                // draw underline
                size_t start_pos = loc2->columns > dist_from_line_start
                    ? loc2->columns - dist_from_line_start : 0;

                size_t tkn_len = loc2->length;
                for (size_t i = 0; i < start_pos; i++) printf(" ");
                printf("^");
                for (size_t i = 1; i < tkn_len; i++) printf("~");
                printf("\n");

                if (loc_msg2) {
                    draw_line_horizontal_pad();
                    for (size_t i = 0; i < start_pos; i++) printf(" ");
                    printf("%s\n", loc_msg2);
                }
            }
        }
    }

    printf("\n\n");
    tally_report_counter(level, err);
    mtx_unlock(&report_mutex);
}

bool has_reports(Cuik_ReportLevel minimum, Cuik_ErrorStatus* err) {
    for (int i = minimum; i < REPORT_MAX; i++) {
        if (atomic_load((atomic_int*) &err->tally[i]) > 0) {
            //printf("exited with %d %s%s", tally[i], report_names[i], tally[i] > 1 ? "s" : "");
            return true;
        }
    }

    return false;
}
#endif
