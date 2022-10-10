#include "diagnostic.h"
#include <locale.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdatomic.h>
#include "preproc/lexer.h"

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#define DIAG(name, level_, fmt_, ...)                     \
DiagDesc cuikdg_ ## name = {                              \
    .level = level_,                                      \
    .format = fmt_,                                       \
    .arg_count = COUNTOF((DiagArgType[]){ __VA_ARGS__ }), \
    .args = { __VA_ARGS__ }                               \
};
#include "diagnostic_table.h"

static const char* report_names[] = {
    "verbose",
    "info",
    "warning",
    "error",
};

mtx_t report_mutex;

#if _WIN32
static HANDLE console_handle;
static WORD default_attribs;
#endif

static const char* const attribs[] = {
    "\x1b[0m",
    "\x1b[32m",
    "\x1b[31m",
    "\x1b[31m",
};

bool report_using_thin_errors = false;

#define RESET_COLOR     printf("\x1b[0m")
#define SET_COLOR_RED   printf("\x1b[31m")
#define SET_COLOR_GREEN printf("\x1b[32m")
#define SET_COLOR_WHITE printf("\x1b[37m")

void init_report_system(void) {
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
    setlocale(LC_ALL, ".UTF8");

    #if _WIN32
    if (console_handle == NULL) {
        console_handle = GetStdHandle(STD_OUTPUT_HANDLE);

        // Enable ANSI/VT sequences on windows
        HANDLE output_handle = GetStdHandle(STD_OUTPUT_HANDLE);
        if (output_handle != INVALID_HANDLE_VALUE) {
            DWORD old_mode;
            if (GetConsoleMode(output_handle, &old_mode)) {
                SetConsoleMode(output_handle, old_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            }
        }

        CONSOLE_SCREEN_BUFFER_INFO info;
        GetConsoleScreenBufferInfo(console_handle, &info);

        default_attribs = info.wAttributes;
    }

    #endif

    mtx_init(&report_mutex, mtx_plain | mtx_recursive);
}

static void print_diag_message(const DiagDesc* desc, va_list ap) {
    size_t used = 0;
    char str_pool[1024];
    String args[16];

    // Record argument messages
    assert(desc->arg_count < 16);
    for (int i = 0; i < desc->arg_count; i++) {
        switch (desc->args[i]) {
            case DIAG_CSTR: {
                args[i] = string_cstr(va_arg(ap, const char*));
                break;
            }
            case DIAG_STRING: {
                args[i] = va_arg(ap, String);
                break;
            }
            default: assert(0 && "TODO");
        }
    }

    // Print message
    const char* fmt = desc->format;
    for (;;) {
        // read & print any non-dollar sign text
        const char* start = fmt;
        while (*fmt && *fmt != '%') fmt++;

        fwrite(start, (fmt - start), 1, stderr);

        if (fmt[0] == 0) {
            break;
        } else if (fmt[1] == '%') {
            fwrite("%", 1, 1, stderr);
            continue;
        }

        // handle the fancy formats
        fmt += 1;

        int i = 0;
        for (; *fmt >= '0' && *fmt <= '9'; fmt++) {
            i *= 10;
            i += (*fmt - '0');
        }

        fprintf(stderr, "%.*s", (int) args[i].length, args[i].data);
    }
    fprintf(stderr, "\n");
}

void diag(TokenStream* tokens, SourceRange loc, const DiagDesc* desc, ...) {
    fprintf(stderr, "\x1b[37m");
    ResolvedSourceLoc start;
    if (cuikpp_find_location(tokens, loc.start, &start)) {
        fprintf(stderr, "%s:%d:%d: ", start.filename, start.line, start.column);
    } else {
        fprintf(stderr, "??:??:??: ");
    }
    fprintf(stderr, "%s%s:\x1b[0m ", attribs[desc->level], report_names[desc->level]);

    va_list ap;
    va_start(ap, desc);
    print_diag_message(desc, ap);
    va_end(ap);

    // Caret preview
    {
        const char* line_start = start.line_str;
        while (*line_start && isspace(*line_start)) {
            line_start++;
        }
        size_t dist_from_line_start = line_start - start.line_str;

        // line preview
        if (*line_start != '\r' && *line_start != '\n') {
            const char* line_end = line_start;
            do { line_end++; } while (*line_end && *line_end != '\n');

            fprintf(stderr, "%6d| %.*s\x1b[37m\n", start.line, (int)(line_end - line_start), line_start);
        }

        size_t tkn_len = 1;
        ResolvedSourceLoc end;
        if (cuikpp_find_location(tokens, loc.end, &end)) {
            if (end.filename == start.filename && end.line == start.line && end.column > start.column) {
                tkn_len = end.column - start.column;
            }
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
}

static void tally_report_counter(Cuik_ReportLevel level, Cuik_ErrorStatus* err) {
    if (err == NULL) {
        if (level >= REPORT_ERROR) {
            SET_COLOR_RED;
            printf("ABORTING!!! (no diagnostics callback)\n");
            RESET_COLOR;
            abort();
        }
    } else {
        atomic_fetch_add((atomic_int*) &err->tally[level], 1);
    }
}

static size_t draw_line(TokenStream* tokens, SourceLoc loc) {
    ResolvedSourceLoc r;
    if (!cuikpp_find_location(tokens, loc, &r)) {
        assert(0 && "cuikpp_find_location failed?");
    }

    // display line
    const char* line_start = r.line_str;
    while (*line_start && isspace(*line_start)) {
        line_start++;
    }
    size_t dist_from_line_start = line_start - r.line_str;

    // Draw line preview
    if (*line_start != '\r' && *line_start != '\n') {
        const char* line_end = line_start;
        do { line_end++; } while (*line_end && *line_end != '\n');

        printf("%6d| %.*s\n", r.line, (int)(line_end - line_start), line_start);
        RESET_COLOR;
    }

    return dist_from_line_start;
}

static void draw_line_horizontal_pad(void) {
    printf("        ");
}

static void print_backtrace(TokenStream* tokens, SourceLoc loc) {
    #if 0
    int line_bias = 0;
    if (line->parent != 0) {
        line_bias = print_backtrace(tokens, line->parent, line);
    }

    switch (loc->type) {
        case SOURCE_LOC_MACRO: {
            if (line->filepath[0] == '<') {
                printf("In macro '%.*s' expanded at line %d:\n", (int)loc->length, line->line_str + loc->columns, line_bias + line->line);
            } else {
                printf("In macro '%.*s' expanded at %s:%d:%d:\n", (int)loc->length, line->line_str + loc->columns, line->filepath, line->line, loc->columns);
            }

            if (!report_using_thin_errors) {
                // draw macro highlight
                size_t dist_from_line_start = draw_line(tokens, loc_index);
                draw_line_horizontal_pad();

                // idk man
                size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

                // draw underline
                SET_COLOR_GREEN;
                // printf("\x1b[32m");

                size_t tkn_len = loc->length;
                for (size_t i = 0; i < start_pos; i++) printf(" ");
                printf("^");
                for (size_t i = 1; i < tkn_len; i++) printf("~");

                // printf("\x1b[0m");
                printf("\n");
                RESET_COLOR;
            }
            return line_bias;
        }

        default:
        printf("In file %s:%d:\n", line->filepath, line->line);
        return line->line;
    }
    #endif
}

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
    ResolvedSourceLoc a, b;
    if (!cuikpp_find_location(writer->tokens, loc.start, &a) ||
        !cuikpp_find_location(writer->tokens, loc.end, &b)) {
        assert(0 && "cuikpp_find_location failed?");
    }

    assert(a.filename == b.filename && a.line == b.line);
    if (writer->base.filename == NULL) {
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

        printf("%s:%d\n", a.filename, a.line);
        draw_line_horizontal_pad();
        printf("%.*s\n", (int) (line_end - line_start), line_start);
        draw_line_horizontal_pad();
    }

    assert(b.column > a.column);
    size_t start_pos = a.column > writer->dist_from_line_start ? a.column - writer->dist_from_line_start : 0;
    size_t tkn_len = b.column - a.column;

    diag_writer_write_upto(writer, start_pos);
    //printf("\x1b[7m");
    //diag_writer_write_upto(writer, start_pos + tkn_len);
    printf("\x1b[32m^");
    for (int i = 1; i < tkn_len; i++) printf("~");
    writer->cursor = start_pos + tkn_len;
    printf("\x1b[0m");
}

bool diag_writer_is_compatible(DiagWriter* writer, SourceRange loc) {
    if (writer->base.filename == NULL) {
        return true;
    }

    ResolvedSourceLoc r;
    if (!cuikpp_find_location(writer->tokens, loc.start, &r)) {
        assert(0 && "cuikpp_find_location failed?");
    }

    return writer->base.filename == r.filename && writer->base.line == r.line;
}

void diag_writer_done(DiagWriter* writer) {
    if (writer->base.filename != NULL) {
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

            #if _WIN32
            SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
            #endif

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

            #if _WIN32
            SetConsoleTextAttribute(console_handle, default_attribs);
            #endif

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

                #if _WIN32
                SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
                #endif

                // draw underline
                size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

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
