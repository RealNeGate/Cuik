#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <inttypes.h>

static const char keywords[][16] = {
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Embed",
    "_Generic",
    "_Imaginary",
    "_Pragma",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
    "_Typeof",
    "_Vector",
    "__asm__",
    "__attribute__",
    "__cdecl",
    "__stdcall",
    "__declspec",

    // GLSL keywords
    "discard",
    "layout",
    "in",
    "out",
    "inout",
    "uint",
    "buffer",
    "uniform",
    "flat",
    "smooth",
    "noperspective",
    "vec2",
    "vec3",
    "vec4",
    "ivec2",
    "ivec3",
    "ivec4",
    "uvec2",
    "uvec3",
    "uvec4",
    "dvec2",
    "dvec3",
    "dvec4",
};

enum { num_keywords = sizeof(keywords) / sizeof(*keywords) };
uint64_t signatures[num_keywords];

uint64_t rand64(void) {
    uint64_t x = rand();
    x |= ((uint64_t) rand() << 16ull);
    x |= ((uint64_t) rand() << 32ull);
    x |= ((uint64_t) rand() << 48ull);
    return x;
}

int check(uint64_t a, uint8_t max_collisions) {
    uint8_t slot[256] = { 0 };
    for (int i = 0; i < num_keywords; i++) {
        uint8_t hash = (uint8_t)((a * signatures[i]) >> 56);
        if (slot[hash] > max_collisions) return 0;
        slot[hash]++;
    }
    return 1;
}

bool run_keyword_tablegen(FILE* f) {
    srand(0);

    for (int i = 0; i < num_keywords; i++) {
        const uint8_t *keyword = (const uint8_t*) keywords[i];
        size_t keyword_len = strlen(keywords[i]);

        uint64_t h = 0;
        for (size_t i = 0; i < keyword_len; i++) {
            h = (h << 5) + keyword[i];
            uint64_t g = h & 0xf0000000;

            if (g != 0) h = (h ^ (g >> 24)) ^ g;
        }

        for (int j = 0; j < i; j++) {
            if (signatures[j] == h) {
                fprintf(f, "Signature collision.\n");
                exit(1);
            }
        }

        signatures[i] = h;
    }

    // try to figure out a good hash
    uint64_t max_iterations = 1ull << 24;
    uint8_t max_collisions = 0;
    uint64_t a;
    for (uint64_t i = 0; i < max_iterations; i++) {
        a = rand64();
        if (!check(a, max_collisions)) continue;

        fprintf(f, "// a = %"PRIu64" (%d keywords, %"PRIu64" tries)\n", a, num_keywords, i);
        fprintf(f, "#define PERFECT_HASH_SEED %"PRIu64"ULL\n", a);
        fprintf(f, "static const uint8_t keywords_table[256] = {\n");
        for (int i = 0; i < num_keywords;) {
            int end = i + 4;

            fprintf(f, "    ");
            for (; i < end && i < num_keywords; i++) {
                uint8_t hash = (uint8_t)((a * signatures[i]) >> 56);
                fprintf(f, "[%d] = %d /* %s */, ", hash, i, keywords[i]);
            }
            fprintf(f, "\n");
        }
        fprintf(f, "};\n");

        /*for (int i = 0; i < num_keywords; i++) {
            const char* str = keywords[i];
            while (*str == '_') str++;

            printf("    TOKEN_KW_%s,\n", str);
        }*/

        return true;
    }

    printf("No hash function with max %d collisions found.\n", max_collisions);
    /*for (int i = 0; i < num_keywords; i++) {
        printf("  %016llx    %s\n", signatures[i], keywords[i]);
    }*/
    return false;
}

static uint8_t table[256][32];

// new state
static int table_id_counter = 1;
static int ns(void) { return table_id_counter++; }

#define RANGE(old, new, ...) range_pattern(old, new, sizeof((const char*[]){ __VA_ARGS__ }) / sizeof(const char*), (const char*[]){ __VA_ARGS__ })
static uint64_t range_pattern(uint64_t old, uint64_t new, int c, const char* ranges[]) {
    if (new < old) { fprintf(stderr, "error: our DFA can't travel backwards %llu -> %llu\n", old, new); abort(); }
    assert(new - old < 15);

    for (int i = 0; i < c; i++) {
        unsigned char min = ranges[i][0], max = ranges[i][1];

        if (max == 0) {
            max = min;
        }

        for (int j = min; j <= max; j++) {
            table[j][old] = new;
        }
    }
    return new;
}

#define CHARS(old, new, ...) chars_pattern(old, new, __VA_ARGS__)
static uint64_t chars_pattern(uint64_t old, uint64_t new, const char* str) {
    if (new < old) { fprintf(stderr, "error: our DFA can't travel backwards %llu %llu\n", old, new); abort(); }
    assert(new - old < 15);

    for (; *str; str++) {
        table[(unsigned char) *str][old] = new;
    }
    return new;
}

int main(int argc, char** argv) {
    FILE* file = fopen("libCuik/lib/preproc/keywords.h", "wb");
    for (int i = 0; i < num_keywords; i++) {
        const char* base = keywords[i];
        while (*base == '_') base++;

        int len = strlen(base);
        while (base[len - 1] == '_') len--;

        fprintf(file, "TOKEN_KW_%.*s", len, base);
        if (i == 0) {
            fprintf(file, " = 0x10000000,\n");
        } else {
            fprintf(file, ",\n");
        }
    }
    fclose(file);

    int wide_str = ns();
    int ident = ns();
    int str = CHARS(0, ns(), "\'\"");
    RANGE(0, ident, "AZ", "az", "_", "$", "\x80\xFF", "\\");
    CHARS(0, wide_str, "L");

    // string
    CHARS(wide_str, str, "\'\"");
    // ident
    RANGE(ident, ident, "AZ", "az", "_", "$", "09", "\x80\xFF", "\\");
    // wide string
    RANGE(
        wide_str, ident,
        "AZ", "az", "_", "$", "09", "\x80\xFF", "\\",
    );

    int num_dot = ns();
    int num = ns();
    {
        // we handle the real number parsing in the lexer code
        CHARS(0, num, "0123456789");
        CHARS(CHARS(0, num_dot, "."), num, "0123456789");
    }

    int sigils = CHARS(0, ns(), "@?;:,(){}.");

    // *= /= %= != ^= ~=
    int ops = CHARS(0, ns(), "*/%!=^~");

    // >>= <<= >= <= > <
    int s1 = CHARS(0, ns(), "><");

    // - -= -> --
    int minus = CHARS(0, ns(), "-");
    // + += ++
    int plus = CHARS(0, ns(), "+");
    int hash = CHARS(0, ns(), "#");
    int pipe = CHARS(0, ns(), "|");
    int amp = CHARS(0, ns(), "&");
    CHARS(0, ns(), "[]");

    CHARS(hash, ns(), "#");
    CHARS(plus, ns(), "+");

    int after_minus = ns();
    CHARS(minus, after_minus, "=>-");

    int eq = CHARS(ops, ns(), "=");
    int s2 = CHARS(s1, ns(), "><");
    int s3 = CHARS(s2, ns(), "=");
    CHARS(s1, eq, "=");

    int amp_end = ns();
    CHARS(amp, amp_end, "&");
    CHARS(amp, amp_end, "=");

    int plus_end = ns();
    CHARS(plus, plus_end, "+");
    CHARS(plus, plus_end, "=");

    int pipe_end = ns();
    CHARS(pipe, pipe_end, "|");
    CHARS(pipe, pipe_end, "=");

    if (table_id_counter >= 32) {
        fprintf(stderr, "Failed to generate DFA (too many states)\n");
        return 1;
    }

    file = fopen("libCuik/lib/preproc/dfa.h", "wb");
    run_keyword_tablegen(file);
    fprintf(file, "\nstatic const char keywords[][16] = {\n");
    for (int i = 0; i < num_keywords; i++) {
        fprintf(file, "    \"%s\",\n", keywords[i]);
    }
    fprintf(file, "};\n\n");
    fprintf(file, "enum {\n");
    fprintf(file, "    DFA_IDENTIFIER   = %d,\n", ident);
    fprintf(file, "    DFA_NUMBER       = %d,\n", num);
    fprintf(file, "    DFA_STRING       = %d,\n", str);
    fprintf(file, "    DFA_SIGILS       = %d,\n", sigils);
    fprintf(file, "    DFA_IDENTIFIER_L = %d,\n", wide_str);
    fprintf(file, "};\n");
    fprintf(file, "static uint64_t dfa[129][2] = {\n");
    for (int i = 0; i < 129; i++) {
        fprintf(file, "    { ");
        for (int j = 0; j < 32; j += 16) {
            if (j) fprintf(file, ", ");

            // construct packed states (16 per 64bit)
            uint64_t v = 0;
            for (int k = 0; k < 16; k++) {
                int l = j + k;
                uint64_t part = table[i][l];
                v |= (part ? part - l : 0xF) << (k*4);
            }

            fprintf(file, "0x%016llx", v);
        }
        fprintf(file, " },\n");
    }
    fprintf(file, "};\n");
    fclose(file);
    return 0;
}
