#include "diagnostic.h"
#include <locale.h>
#include <ctype.h>
#include <preproc/cpp.h>
#include <stdarg.h>
#include <stdatomic.h>

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#define GET_SOURCE_LOC(loc) (&tokens->locations[SOURCE_LOC_GET_DATA(loc)])

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

const static int attribs[] = {
    FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY,
    FOREGROUND_GREEN | FOREGROUND_INTENSITY,
    FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_INTENSITY,
    FOREGROUND_RED | FOREGROUND_INTENSITY,
};
#endif

bool report_using_thin_errors = false;

void init_report_system(void) {
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

    SetConsoleOutputCP(CP_UTF8);
    #endif

    //setlocale(LC_ALL, ".utf8");
    mtx_init(&report_mutex, mtx_plain);
}

static void print_level_name(Cuik_ReportLevel level) {
    #if _WIN32
    SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | attribs[level]);
    printf("%s: ", report_names[level]);
    SetConsoleTextAttribute(console_handle, default_attribs);
    #else
    printf("%s: ", report_names[level]);
    #endif
}

static SourceLocIndex try_for_nicer_loc(TokenStream* tokens, SourceLocIndex loci) {
    const SourceLoc* loc = GET_SOURCE_LOC(loci);

    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loci = loc->line->parent;
        loc = GET_SOURCE_LOC(loci);
    }

    return loci;
}

static void display_line(Cuik_ReportLevel level, TokenStream* tokens, SourceLoc* loc) {
    SourceLocIndex loci = 0;
    while (loc->line->filepath[0] == '<' && loc->line->parent != 0) {
        loci = loc->line->parent;
        loc = GET_SOURCE_LOC(loci);
    }

    if (report_using_thin_errors) {
        printf("%s:%d:%d: ", loc->line->filepath, loc->line->line, loc->columns);
        print_level_name(level);
    } else {
        print_level_name(level);
        printf("%s:%d:%d: ", loc->line->filepath, loc->line->line, loc->columns);
    }
}

static void tally_report_counter(Cuik_ReportLevel level, Cuik_ErrorStatus* err) {
    if (err == NULL) {
        #if _WIN32
        SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_RED | FOREGROUND_INTENSITY);
        #endif

        printf("ABORTING!!!\n");

        #if _WIN32
        SetConsoleTextAttribute(console_handle, default_attribs);
        #endif
        abort();
    } else {
        int error_count = atomic_fetch_add((atomic_int*) &err->tally[level], 1);
        if (level > REPORT_WARNING && error_count > 20) {
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
}

static size_t draw_line(TokenStream* tokens, SourceLocIndex loc_index) {
    SourceLine* line = GET_SOURCE_LOC(loc_index)->line;

    // display line
    const char* line_start = (const char*)line->line_str;
    while (*line_start && isspace(*line_start)) {
        line_start++;
    }
    size_t dist_from_line_start = line_start - (const char*)line->line_str;

    // Draw line preview
    if (*line_start != '\n') {
        const char* line_end = line_start;
        printf("    ");
        do {
            putchar(*line_end != '\t' ? *line_end : ' ');
            line_end++;
        } while (*line_end && *line_end != '\n');
        printf("\n");
    }

    return dist_from_line_start;
}

static void draw_line_horizontal_pad() {
    printf("    ");
}

static SourceLoc merge_source_locations(TokenStream* tokens, SourceLocIndex starti, SourceLocIndex endi) {
    SourceLine* line = GET_SOURCE_LOC(starti)->line;

    starti = try_for_nicer_loc(tokens, starti);
    endi = try_for_nicer_loc(tokens, endi);

    const SourceLoc* start = GET_SOURCE_LOC(starti);
    const SourceLoc* end = GET_SOURCE_LOC(endi);

    if (start->line->filepath != end->line->filepath &&
        start->line->line != end->line->line) {
        return *start;
    }

    // We can only merge if it's on the same line... for now...
    size_t start_columns = start->columns;
    size_t end_columns = end->columns + end->length;
    if (start_columns >= end_columns) {
        return *start;
    }

    return (SourceLoc){line, start_columns, end_columns - start_columns};
}

static int print_backtrace(TokenStream* tokens, SourceLocIndex loc_index, SourceLine* kid) {
    SourceLoc* loc = GET_SOURCE_LOC(loc_index);
    SourceLine* line = loc->line;

    int line_bias = 0;
    if (line->parent != 0) {
        line_bias = print_backtrace(tokens, line->parent, line);
    }

    switch (SOURCE_LOC_GET_TYPE(loc_index)) {
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

                #if _WIN32
                SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
                #endif

                // idk man
                size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

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
            return line_bias;
        }

        default:
        printf("In file %s:%d:\n", line->filepath, line->line);
        return line->line;
    }
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

    if (!report_using_thin_errors) {
        size_t dist_from_line_start = draw_line(tokens, start_loc);
        draw_line_horizontal_pad();

        #if _WIN32
        SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
        #endif

        // idk man
        size_t start_pos = loc.columns > dist_from_line_start ? loc.columns - dist_from_line_start : 0;

        // draw underline
        size_t tkn_len = loc.length;
        for (size_t i = 0; i < start_pos; i++) printf(" ");
        printf("^");
        for (size_t i = 1; i < tkn_len; i++) printf("~");
        printf("\n");

        #if _WIN32
        SetConsoleTextAttribute(console_handle, default_attribs);
        #endif
        printf("\n");
    }

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

    if (!report_using_thin_errors) {
        size_t dist_from_line_start = draw_line(tokens, loc_index);
        draw_line_horizontal_pad();

        #if _WIN32
        SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
        #endif

        // idk man
        size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

        // draw underline
        size_t tkn_len = loc->length;
        for (size_t i = 0; i < start_pos; i++) printf(" ");
        printf("^");
        for (size_t i = 1; i < tkn_len; i++) printf("~");
        printf("\n");

        #if _WIN32
        SetConsoleTextAttribute(console_handle, default_attribs);
        #endif

        printf("\n");
    }

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

    if (!report_using_thin_errors) {
        size_t dist_from_line_start = draw_line(tokens, loc_index);
        draw_line_horizontal_pad();

        #if _WIN32
        SetConsoleTextAttribute(console_handle, (default_attribs & ~0xF) | FOREGROUND_GREEN);
        #endif

        // idk man
        size_t start_pos = loc->columns > dist_from_line_start ? loc->columns - dist_from_line_start : 0;

        // one after the token
        start_pos += loc->length;

        // draw underline
        for (size_t i = 0; i < start_pos; i++) printf(" ");
        printf("^");
        printf("\n");

        draw_line_horizontal_pad();
        for (size_t i = 0; i < start_pos; i++) printf(" ");
        printf("%s\n", tip);

        #if _WIN32
        SetConsoleTextAttribute(console_handle, default_attribs);
        #endif

        printf("\n");
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
