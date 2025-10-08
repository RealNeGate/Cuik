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
    uint64_t a = 7279593577997237667ULL;
    for (uint64_t i = 0; i < max_iterations; i++) {
        if (!check(a, max_collisions)) {
            a = rand64();
            continue;
        }

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
    if (new < old) {
        fprintf(stderr, "error: our DFA can't travel backwards %llu -> %llu\n",
            (long long unsigned) old,
            (long long unsigned) new);
        abort();
    }
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
    if (new < old) {
        fprintf(stderr, "error: our DFA can't travel backwards %llu %llu\n",
            (long long unsigned) old,
            (long long unsigned) new);
        abort();
    }
    assert(new - old < 15);

    for (; *str; str++) {
        table[(unsigned char) *str][old] = new;
    }
    return new;
}

enum {
    EQ_NONE,
    EQ_SIGIL, // @?;:,(){}[].
    EQ_OPS,   // *%/!=^~
    EQ_NUM,   // 0-9
    EQ_L,     // L
    EQ_IDENT, // _ A-Z a-z $
    EQ_DOT,   // .
    EQ_AMP,   // &
    EQ_PIPE,  // |
    EQ_LT,    // <
    EQ_GT,    // >
    EQ_PLUS,  // +
    EQ_MINUS, // -
    EQ_HASH,  // #
    EQ_EQUAL, // =
    EQ_QUOTE, // ' "
    EQ_MAX = 16,
};

enum { ACCEPT = 1 };
static const uint8_t dfa[20][EQ_MAX] = {
    // terminator state
    // ...

    // identifier: IDENT(IDENT | NUM)*
    [0][EQ_IDENT] = 2,
    [2][EQ_IDENT] = 2,
    [2][EQ_NUM]   = 2,
    [2][EQ_L]     = 2,

    // number: NUM+ (DOT NUM+ IDENT+)
    [0][EQ_NUM]   = 3,
    [3][EQ_NUM]   = 3,

    // sigils: just a single char
    [0][EQ_SIGIL] = 19,

    // . ...
    [0][EQ_DOT]   = 4,
    [4][EQ_DOT]   = 5,
    [5][EQ_DOT]   = 1,

    // & && &=
    [0][EQ_AMP]   = 6,
    [6][EQ_AMP]   = ACCEPT,
    [6][EQ_EQUAL] = ACCEPT,

    // | || |=
    [0][EQ_PIPE]  = 7,
    [7][EQ_PIPE]  = ACCEPT,
    [7][EQ_EQUAL] = ACCEPT,

    // + ++ +=
    [0][EQ_PLUS]  = 8,
    [8][EQ_PLUS]  = ACCEPT,
    [8][EQ_EQUAL] = ACCEPT,

    // OP | (OP EQUAL)
    [0][EQ_OPS]    = 9,
    [9][EQ_EQUAL] = ACCEPT,

    // quotes
    [0][EQ_QUOTE] = 10,

    // < << <= <<=
    [0][EQ_LT]     = 11,
    [11][EQ_LT]    = 12,
    [11][EQ_EQUAL] = ACCEPT,
    [12][EQ_EQUAL] = ACCEPT,

    // > >> >= >>=
    [0][EQ_GT]     = 13,
    [13][EQ_GT]    = 14,
    [13][EQ_EQUAL] = ACCEPT,
    [14][EQ_EQUAL] = ACCEPT,

    // - -- -= ->
    [0][EQ_MINUS]  = 15,
    [15][EQ_GT]    = ACCEPT,
    [15][EQ_MINUS] = ACCEPT,
    [15][EQ_EQUAL] = ACCEPT,

    // = ==
    [0][EQ_EQUAL]  = 16,
    [16][EQ_EQUAL] = ACCEPT,

    // # ##
    [0][EQ_HASH]   = 17,
    [17][EQ_HASH]  = ACCEPT,

    // L"string"
    [0][EQ_L]      = 18,
    [18][EQ_IDENT] = 2,
    [18][EQ_NUM]   = 2,
    [18][EQ_L]     = 2,
    [18][EQ_QUOTE] = 10,
};

// we compress this into 4bit entries so it's 128bytes of eq_class
static uint8_t eq_classes[256] = {
    // A-Z is added later
    ['a' ... 'z']   = EQ_IDENT,
    [0x80 ... 0xFF] = EQ_IDENT,
    ['_']           = EQ_IDENT,
    ['$']           = EQ_IDENT,
    ['\\']          = EQ_IDENT,

    ['*'] = EQ_OPS, ['%'] = EQ_OPS, ['!'] = EQ_OPS,
    ['^'] = EQ_OPS, ['~'] = EQ_OPS, ['/'] = EQ_OPS,

    ['@'] = EQ_SIGIL, ['?'] = EQ_SIGIL, [';'] = EQ_SIGIL,
    [':'] = EQ_SIGIL, [','] = EQ_SIGIL, ['('] = EQ_SIGIL,
    [')'] = EQ_SIGIL, ['{'] = EQ_SIGIL, ['}'] = EQ_SIGIL,
    ['['] = EQ_SIGIL, [']'] = EQ_SIGIL,

    ['0' ... '9'] = EQ_NUM,

    ['\''] = EQ_QUOTE, ['"'] = EQ_QUOTE,

    ['>']  = EQ_GT,
    ['<']  = EQ_LT,
    ['.']  = EQ_DOT,
    ['&']  = EQ_AMP,
    ['#']  = EQ_HASH,
    ['=']  = EQ_EQUAL,
    ['|']  = EQ_PIPE,
    ['+']  = EQ_PLUS,
    ['-']  = EQ_MINUS,
};

int main(int argc, char** argv) {
    FILE* file = fopen("cuik_pp/keywords.h", "wb");
    for (int i = 0; i < num_keywords; i++) {
        const char* base = keywords[i];
        while (*base == '_') base++;

        int len = strlen(base);
        while (base[len - 1] == '_') len--;

        fprintf(file, "TOKEN_KW_%.*s", len, base);
        if (i == 0) {
            fprintf(file, " = 0x800000,\n");
        } else {
            fprintf(file, ",\n");
        }
    }
    fclose(file);

    for (int i = 'A'; i <= 'Z'; i++) {
        eq_classes[i] = EQ_IDENT;
    }
    eq_classes['L'] = EQ_L;

    file = fopen("cuik_pp/dfa.h", "wb");
    run_keyword_tablegen(file);
    fprintf(file, "\nstatic const char keywords[][16] = {\n");
    for (int i = 0; i < num_keywords; i++) {
        fprintf(file, "    \"%s\",\n", keywords[i]);
    }
    fprintf(file, "};\n\n");
    // fprintf(file, "enum { EQ_MAX = %d };\n", EQ_MAX);
    fprintf(file, "static const uint64_t dfa[256][2] = {\n");
    for (int j = 0; j < 256; j++) {
        int cl = eq_classes[j];

        // for (int j = 0; j < EQ_MAX; j++) {
        // construct packed states (10 per 6bit)
        uint64_t v[2] = { 0 };
        for (int i = 0; i < 20; i++) {
            uint64_t next = dfa[i][cl];

            // printf("[%d][%d]: %llu * 6 (%llu) @ %d,%d\n", i, j, next, ((next>>1)*6) | (next & 1), i&1, (i>>1)*6);
            v[i & 1] |= (((next>>1) * 6) | (next & 1)) << ((i >> 1) * 6);
        }

        v[1] <<= 1;

        fprintf(file, "    { 0x%016llx, 0x%016llx },\n", (long long unsigned) v[0], (long long unsigned) v[1]);
    }
    fprintf(file, "};\n\n");
    /*fprintf(file, "static const uint8_t eq_classes[128] = {");
    for (int i = 0; i < 256; i += 2) {
        if (i % 16 == 0) fprintf(file, "\n    ");

        // construct packed states (2 per 8bit)
        uint8_t v = eq_classes[i];
        v |= eq_classes[i+1] << 4;

        fprintf(file, "0x%02x, ", v);
    }
    fprintf(file, "\n};\n");*/
    fclose(file);
    return 0;
}
