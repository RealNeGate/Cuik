#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

static uint8_t table[256][32];

static void emit_range(uint64_t min, uint64_t max, uint64_t old, uint64_t next) {
    for (size_t i = min; i <= max; i++) {
        table[i][old] = next;
    }
}

static void emit_chars(const char* str, uint64_t old, uint64_t next) {
    for (; *str; str++) {
        table[(unsigned char) *str][old] = next;
    }
}

static void emit_all_chars(const char* str, uint64_t old, uint64_t next) {
    for (size_t i = 0; i < 256; i++) {
        table[i][old] = next;
    }
}

// new state
static int table_id_counter = 1;
static int ns(void) {
    return table_id_counter++;
}

#define RANGE(old, new, ...) range_pattern(old, new, sizeof((const char*[]){ __VA_ARGS__ }) / sizeof(const char*), (const char*[]){ __VA_ARGS__ })
static uint64_t range_pattern(uint64_t old, uint64_t new, int c, const char* ranges[]) {
    for (int i = 0; i < c; i++) {
        unsigned char min = ranges[i][0], max = ranges[i][1];

        if (max == 0) {
            table[min][old] = new;
        } else {
            for (int j = min; j <= max; j++) {
                table[j][old] = new;
            }
        }
    }
    return new;
}

#define CHARS(old, new, ...) chars_pattern(old, new, __VA_ARGS__)
static uint64_t chars_pattern(uint64_t old, uint64_t new, const char* str) {
    emit_chars(str, old, new);
    return new;
}

int main(int argc, char** argv) {
    if (argc <= 1) {
        fprintf(stderr, "requires output path!\n");
        return 1;
    }

    int ident = ns();
    RANGE(0, ident, "AZ", "az", "_", "$", "\x80\xFF", "\\");
    RANGE(ident, ident, "AZ", "az", "_", "$", "09", "\x80\xFF", "\\");

    int num = ns();
    {
        // we handle the real number parsing in the lexer code
        CHARS(0, num, "0123456789");
        CHARS(CHARS(0, ns(), "."), num, "0123456789");
    }

    CHARS(0, ns(), "@?;:,(){}");

    // *= /= %= != &= ^= ~=
    int ops = CHARS(0, ns(), "*/%!=&^~");
    int eq = CHARS(ops, ns(), "=");

    // >>= <<= >= <= > <
    int s1 = CHARS(0, ns(), "><");
    int s2 = CHARS(s1, ns(), "><");
    int s3 = CHARS(s2, ns(), "=");
    CHARS(s1, eq, "=");

    // - -= -> --
    CHARS(CHARS(0, ns(), "-"), ns(), "=>-");

    int hash = CHARS(0, ns(), "&");
    CHARS(hash, ns(), "&");
    CHARS(hash, eq, "=");

    CHARS(CHARS(0, ns(), "+"), ns(), "+");
    CHARS(CHARS(0, ns(), "#"), ns(), "#");
    CHARS(0, ns(), "[");
    CHARS(0, ns(), "]");

    int plus = CHARS(0, ns(), "+"), plus_end = ns();
    CHARS(plus, plus_end, "+");
    CHARS(plus, plus_end, "=");

    int pipe = CHARS(0, ns(), "|"), pipe_end = ns();
    CHARS(pipe, pipe_end, "|");
    CHARS(pipe, pipe_end, "=");

    int dot = CHARS(0, ns(), ".");
    CHARS(dot, dot, ".");

    int str = CHARS(0, ns(), "\'\"");

    // string
    int wide_str = CHARS(0, ns(), "L");
    CHARS(wide_str, str, "\'\"");

    // wide string
    RANGE(
        wide_str, ident,
        "AZ", "az", "_", "$", "09", "\x80\xFF", "\\",
    );

    if (table_id_counter >= 32) {
        fprintf(stderr, "Failed to generate DFA (too many states)\n");
        return 1;
    }

    // printf("%d\n", table_id_counter);
    FILE* file = fopen(argv[1], "wb");
    fprintf(file, "enum {\n");
    fprintf(file, "    DFA_IDENTIFIER   = %d,\n", ident);
    fprintf(file, "    DFA_NUMBER       = %d,\n", num);
    fprintf(file, "    DFA_STRING       = %d,\n", str);
    fprintf(file, "    DFA_IDENTIFIER_L = %d,\n", wide_str);
    fprintf(file, "};\n");
    fprintf(file, "static uint8_t dfa[256][32] = {\n");
    for (int i = 0; i < 256; i++) {
        fprintf(file, "    { ");
        for (int j = 0; j < 32; j++) {
            if (j) fprintf(file, ", ");
            fprintf(file, "0x%02x", table[i][j]);
        }
        fprintf(file, " },\n");
    }
    fprintf(file, "};\n");
    fclose(file);
    return 0;
}
