#define _CRT_SECURE_NO_WARNINGS
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

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
        const char *keyword = keywords[i];
        size_t keyword_len = strlen(keyword);
        uint64_t signature = 0xcbf29ce484222325ull;
        // The 7 high bytes of the signature are characters 0, 2, 4, etc.
        for (int j = 0; j < keyword_len && j < 7; j++) {
            signature = ((uint8_t)keyword[j] ^ signature) * 0x100000001b3ull;
        }

        // The low byte of the signature is the length.
        signature = (signature ^ keyword_len) * 0x100000001b3ull;

        for (int j = 0; j < i; j++) {
            if (signatures[j] == signature) {
                fprintf(f, "Signature collision.\n");
                exit(1);
            }
        }

        signatures[i] = signature;
    }

    // try to figure out a good hash
    uint64_t max_iterations = 1ull << 24;
    uint8_t max_collisions = 0;
    uint64_t a;
    for (uint64_t i = 0; i < max_iterations; i++) {
        a = rand64();
        if (!check(a, max_collisions)) continue;

        fprintf(f, "// a = %llu (%d keywords, %llu tries)\n", a, num_keywords, i);
        fprintf(f, "#define PERFECT_HASH_SEED %lluULL\n", a);
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
