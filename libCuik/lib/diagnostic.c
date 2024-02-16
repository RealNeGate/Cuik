#include "diagnostic.h"
#include <ctype.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdarg.h>
#include "preproc/lexer.h"

typedef struct {
    va_list va;
} VaList;

// We extend on stb_sprintf with support for a custom callback because
// diagnostic formats have extra options.
static const char* custom_diagnostic_format(uint32_t* out_length, char ch, VaList* va);

#define STB_SPRINTF_STATIC
#define STB_SPRINTF_IMPLEMENTATION
#include "stb_sprintf.h"

#if _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

static const char* report_colors[] = { NULL, "\x1b[32m", "\x1b[33m", "\x1b[31m" };
static const char* report_names[]  = { NULL, "NOTE",     "WARN",     "ERROR"    };

// From types.c, we should factor this out into a public cuik function
size_t type_as_string(size_t max_len, char* buffer, Cuik_Type* type);

static char* sprintf_callback(const char* buf, void* user, int len) {
    void* dst = tb_arena_unaligned_alloc(user, len);
    memcpy(dst, buf, len);
    return NULL;
}

static const char* custom_diagnostic_format(uint32_t* out_length, char ch, VaList* va) {
    static _Thread_local char temp_string[1024];

    switch (ch) {
        case 'S': {
            String str = va_arg(va->va, String);

            *out_length = str.length;
            return (const char*) str.data;
        }

        case 'T': {
            Cuik_Type* t = va_arg(va->va, Cuik_Type*);
            type_as_string(sizeof(temp_string), temp_string, t);

            *out_length = strlen(temp_string);
            return temp_string;
        }

        default:
        fprintf(stderr, "\x1b[31mdiagnostic error\x1b[0m: unknown format %%_%c", ch);
        abort();
        return NULL;
    }
}

static int sprintfcb(Cuik_Diagnostics* d, char const *fmt, ...) {
    static _Thread_local char tmp[STB_SPRINTF_MIN];

    va_list ap;
    va_start(ap, fmt);
    int r = stbsp_vsprintfcb(sprintf_callback, d->buffer, tmp, fmt, ap);
    va_end(ap);
    return r;
}

#define RESET_COLOR     printf("\x1b[0m")
#define SET_COLOR_RED   printf("\x1b[31m")
#define SET_COLOR_GREEN printf("\x1b[32m")
#define SET_COLOR_WHITE printf("\x1b[37m")

Cuik_Diagnostics* cuikdg_make(Cuik_DiagCallback callback, void* userdata) {
    Cuik_Diagnostics* d = cuik_calloc(1, sizeof(Cuik_Diagnostics));
    d->callback = callback;
    d->userdata = userdata;
    d->buffer = tb_arena_create(TB_ARENA_MEDIUM_CHUNK_SIZE);
    return d;
}

void cuikdg_free(Cuik_Diagnostics* diag) {
    tb_arena_destroy(diag->buffer);
    cuik_free(diag);
}

Cuik_Parser* cuikdg_get_parser(Cuik_Diagnostics* diag) {
    return diag->parser;
}

CUIK_API void cuikdg_dump_to_stderr(TokenStream* tokens) {
    cuikdg_dump_to_file(tokens, stderr);
}

CUIK_API void cuikdg_dump_to_file(TokenStream* tokens, FILE* out) {
    TB_Arena* arena = tokens->diag->buffer;
    for (TB_Arena* c = arena; c; c = c->next) {
        fwrite(c->data, c->watermark - c->data, 1, out);
    }
}

// we use the call stack so we can print in reverse order
void print_include(TokenStream* tokens, SourceLoc loc) {
    ResolvedSourceLoc r = cuikpp_find_location(tokens, loc);

    if (r.file->include_site.raw != 0) {
        print_include(tokens, r.file->include_site);
    }

    sprintfcb(tokens->diag, "Included from %s:%d\n", r.file->filename, r.line);
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

static void print_line(TokenStream* tokens, ResolvedSourceLoc start, size_t tkn_len) {
    const char* line_start = start.line_str;
    while (*line_start && isspace(*line_start)) line_start++;
    size_t dist_from_line_start = line_start - start.line_str;

    // line preview
    if (*line_start != '\r' && *line_start != '\n') {
        const char* line_end = line_start;
        do { line_end++; } while (*line_end && *line_end != '\n');

        sprintfcb(tokens->diag, "     |\n");
        sprintfcb(tokens->diag, "%5d| %.*s\x1b[37m\n", start.line, (int)(line_end - line_start), line_start);
    }

    // underline
    size_t start_pos = start.column > dist_from_line_start ? start.column - dist_from_line_start : 0;
    sprintfcb(tokens->diag, "     | ");
    for (size_t i = 0; i < start_pos; i++) sprintfcb(tokens->diag, " ");
    sprintfcb(tokens->diag, "\x1b[32m");
    sprintfcb(tokens->diag, "^");
    for (size_t i = 1; i < tkn_len; i++) sprintfcb(tokens->diag, "~");

    sprintfcb(tokens->diag, "\x1b[0m\n");
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

        MacroInvoke* m2 = cuikpp_find_macro(tokens, end);
        if (m == m2) {
            tkn_len = end.raw < loc.raw ? 1 : (end.raw - loc.raw);
        } else {
            tkn_len = m->def_site.end.raw - m->def_site.start.raw;
        }
    } else {
        l = cuikpp_find_location_in_bytes(tokens, loc);

        if (cuikpp_find_macro(tokens, end) == NULL) {
            tkn_len = source_range_difference(tokens, l, cuikpp_find_location_in_bytes(tokens, end));
        } else {
            tkn_len = 1;
        }
    }

    if (m != NULL) {
        Cuik_FileEntry* next_file = cuikpp_find_file(tokens, m->call_site);
        print_line_with_backtrace(tokens, m->call_site, offset_source_loc(m->call_site, m->name.length));
        sprintfcb(tokens->diag, "     |\n");
        if (next_file->filename != l.file->filename) {
            sprintfcb(tokens->diag, "  expanded from %s:\n", l.file->filename);
        } else {
            sprintfcb(tokens->diag, "  expanded from:\n");
        }
    }

    print_line(tokens, cuikpp_find_location2(tokens, l), tkn_len);
}

static void diag(DiagType type, TokenStream* tokens, SourceRange loc, const char* fmt, va_list ap) {
    Cuik_Diagnostics* d = tokens->diag;
    assert(d != NULL);

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

    // retrofitting the rust style into C
    //
    // level[CODE]: message
    //   --> file.c:LL:CC
    //     |
    // 000 | code
    //     | ^~~~
    //     |
    //    got:  int
    //    need: char*
    char tmp[STB_SPRINTF_MIN];
    if (type == DIAG_ERR) {
        sprintfcb(d, "%s%s\x1b[0m[0000]: ", report_colors[type], report_names[type]);
    } else if (type == DIAG_NULL) {
        sprintfcb(d, "     ");
    } else {
        sprintfcb(d, "%s%s\x1b[0m: ", report_colors[type], report_names[type]);
    }
    stbsp_vsprintfcb(sprintf_callback, d->buffer, tmp, fmt, ap);

    // location summary
    if (loc_start.raw != 0) {
        sprintfcb(d, "\n   --> %s:%d:%d\n", start.file->filename, start.line, start.column + 1);

        SourceLoc loc_end = loc.end;
        // we wanna find the physical character
        for (MacroInvoke* m; (m = cuikpp_find_macro(tokens, loc_end)) != NULL;) {
            loc_end = m->call_site;
        }

        print_line_with_backtrace(tokens, loc.start, loc.end);
    }

    // fixits
    if (fixit_count > 0) {
        const char* line_start = start.line_str;
        while (*line_start && isspace(*line_start)) line_start++;
        size_t dist_from_line_start = line_start - start.line_str;

        for (size_t i = 0; i < fixit_count; i++) {
            size_t start_pos = start.column > dist_from_line_start ? start.column - dist_from_line_start : 0;
            start_pos += fixits[i].offset;

            sprintfcb(tokens->diag, "     | \x1b[32m");
            for (size_t j = 0; j < start_pos; j++) sprintfcb(tokens->diag, " ");
            sprintfcb(tokens->diag, "%s\x1b[0m\n", fixits[i].hint);
        }
    }

    if (loc_start.raw != 0) {
        sprintfcb(tokens->diag, "     |\n");
    } else {
        *(char*)tb_arena_unaligned_alloc(tokens->diag->buffer, 1) = '\n';
    }

    if (d->callback) d->callback(d, d->userdata, type);

    if (type == DIAG_ERR) {
        atomic_fetch_add(&tokens->diag->error_tally, 1);
    }
}

void cuikdg_tally_error(TokenStream* s) {
    atomic_fetch_add(&s->diag->error_tally, 1);
}

int cuikdg_error_count(TokenStream* s) {
    return atomic_load(&s->diag->error_tally);
}

void diag_extra(TokenStream* tokens, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    diag(DIAG_NULL, tokens, (SourceRange){ 0 }, fmt, ap);
    va_end(ap);
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

void diag_header(TokenStream* tokens, DiagType type, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char tmp[STB_SPRINTF_MIN];
    if (type == DIAG_ERR) {
        sprintfcb(tokens->diag, "%s%s\x1b[0m[0000]: ", report_colors[type], report_names[type]);
    } else {
        sprintfcb(tokens->diag, "%s%s\x1b[0m: ", report_colors[type], report_names[type]);
    }
    stbsp_vsprintfcb(sprintf_callback, &tokens->diag->buffer, tmp, fmt, ap);
    *(char*)tb_arena_unaligned_alloc(tokens->diag->buffer, 1) = '\n';
    va_end(ap);
}

static void diag_writer_write_upto(DiagWriter* writer, size_t pos) {
    if (writer->cursor < pos) {
        int l = pos - writer->cursor;
        memset(tb_arena_unaligned_alloc(writer->tokens->diag->buffer, l), ' ', l);

        //printf("%.*s", (int)(pos - writer->cursor), writer->line_start + writer->cursor);
        writer->cursor = pos;
    }
}

void diag_writer_highlight(DiagWriter* writer, SourceRange loc) {
    TokenStream* tokens = writer->tokens;
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
        writer->cursor = dist_from_line_start;
        writer->line_start = line_start;
        writer->line_end = line_end;
        writer->dist_from_line_start = dist_from_line_start;

        sprintfcb(tokens->diag, "  --> %s:%d\n", a.file->filename, a.line);
        sprintfcb(tokens->diag, "     |\n     | ");
        sprintfcb(tokens->diag, "%.*s\n", (int) (line_end - line_start), line_start);
        sprintfcb(tokens->diag, "     | ");
    }

    assert(b.column >= a.column);
    size_t start_pos = a.column > writer->dist_from_line_start ? a.column - writer->dist_from_line_start : 0;
    size_t tkn_len = b.column - a.column;
    if (tkn_len == 0) tkn_len = 1;

    diag_writer_write_upto(writer, start_pos);
    //printf("\x1b[7m");
    //diag_writer_write_upto(writer, start_pos + tkn_len);
    for (size_t i = 0; i < start_pos; i++) sprintfcb(tokens->diag, " ");
    sprintfcb(tokens->diag, "\x1b[32m^");
    for (size_t i = 1; i < tkn_len; i++) sprintfcb(tokens->diag, "~");
    writer->cursor = start_pos + tkn_len;
    sprintfcb(tokens->diag, "\x1b[0m");
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
        TokenStream* tokens = writer->tokens;
        diag_writer_write_upto(writer, writer->line_end - writer->line_start);
        sprintfcb(tokens->diag, "\n");
    }
}
