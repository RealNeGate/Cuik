#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#endif

static char* read_entire_file(const char* path, size_t* out_length) {
    FILE* file = fopen(path, "rb");
    if (!file) {
        return NULL;
    }

    int descriptor = fileno(file);
    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        return NULL;
    }

    size_t len = file_stats.st_size + 1;
    char* buffer = malloc(len);

    fseek(file, 0, SEEK_SET);
    len = fread(buffer, 1, len, file);
    buffer[len] = 0;
    fclose(file);

    if (out_length) *out_length = len;
    return buffer;
}

typedef struct {
    const char* start;
    const char* current;

    const char* token_start;
    const char* token_end;
} Lexer;

static bool lex(Lexer* restrict l) {
    for (;;) {
        l->current += (*l->current == ' ');

        // skip garbage
        while (isspace(*l->current)) l->current++;

        if (*l->current == 0) {
            return false;
        } else if (*l->current == '#') {
            // single-line comments
            while (*l->current && *l->current != '\n') l->current++;

            l->current += (*l->current == '\n');
            continue;
        }
        break;
    }

    // skip until more spaces
    l->token_start = l->current;
    while (*l->current && !isspace(*l->current)) l->current++;
    l->token_end = l->current;

    return true;
}

static bool lex_is_word(Lexer* restrict l) {
    const char* str = l->token_start;
    for (; str != l->token_end; str++) {
        if (*str == '%') return false;
        if (*str == '$') return false;
        if (*str == ':') return false;
    }

    return true;
}

static char* lex_newstr(Lexer* restrict l) {
    size_t len = l->token_end - l->token_start;
    char* dst = malloc(len);
    memcpy(dst, l->token_start, len);
    dst[len] = 0;
    return dst;
}

int main(void) {
    char* makefile = read_entire_file("test.txt", NULL);
    if (makefile == NULL) {
        fprintf(stderr, "make: *** No targets specified and no makefile found.  Stop.");
        return EXIT_FAILURE;
    }

    const char* line = makefile;
    int line_no = 1;
    for (; *line;) {
        const char* eol = strchr(line, '\n');
        if (eol == NULL) eol = line + strlen(line);

        // check for indentation
        //   GNU make doesn't allow for spaces, GNU make is also a bitch
        bool indented = isspace(line[0]);
        Lexer l = { line, line };

        printf("LINE '%.*s'\t%s\n", (int)(eol - line), line, indented?"INDENT":"UNINDENT");

        // if there's no indentation then it's either a target descriptor or a declaration
        //
        // declaration:
        //     WORD ':=' STRING+
        //     WORD  '=' STRING+
        //
        // target:
        //     WORD ':' WORD NEWLINE
        if (!lex(&l)) goto done_with_line;
        if (!lex_is_word(&l)) {
            fprintf(stderr, "line %d: expected name at the start for declaration or target.\n", line_no);
        }

        char* name = lex_newstr(&l);

        lex(&l);
        __debugbreak();

        done_with_line:
        line = *eol ? eol+1 : eol;
        line_no++;
    }

    printf("Hello, World!\n");
    __debugbreak();
    return 0;
}
