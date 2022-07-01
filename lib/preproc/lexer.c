#include "lexer.h"
#include <x86intrin.h>

#ifdef __CUIKC__
#define ALWAYS_INLINE inline
#else
#define ALWAYS_INLINE __attribute__((always_inline))
#endif

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
    "_Generic",
    "_Imaginary",
    "_Pragma",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
    "_Typeof",
    "_Vector",
    "__cdecl",
    "__stdcall",
    "__declspec"
};

enum {
    CHAR_CLASS_NULL,
    // A-Z a-z 0-9 _
    CHAR_CLASS_IDENT,
    // 0-9
    CHAR_CLASS_NUMBER,
    // ;{}()
    CHAR_CLASS_SEPARATOR,
    // + ++ +=
    CHAR_CLASS_MULTICHAR1,
    // > >= >> >>= < <= << <<=
    CHAR_CLASS_MULTICHAR2,
    // - -> -- -=
    CHAR_CLASS_MULTICHAR3,
    // 'foo' "bar"
    CHAR_CLASS_STRING,
    // . ...
    CHAR_CLASS_DOT,
    // # ##
    CHAR_CLASS_HASH,
};

static _Alignas(64) uint8_t char_classes[256] = {
    ['A' ... 'Z'] = CHAR_CLASS_IDENT,
    ['a' ... 'z'] = CHAR_CLASS_IDENT,
    ['_']  = CHAR_CLASS_IDENT,
    ['$']  = CHAR_CLASS_IDENT,
    [0x80 ... 0xFF] = CHAR_CLASS_IDENT,

    ['0' ... '9'] = CHAR_CLASS_NUMBER,

    ['@'] = CHAR_CLASS_SEPARATOR,
    ['?'] = CHAR_CLASS_SEPARATOR,
    [';'] = CHAR_CLASS_SEPARATOR,
    [':'] = CHAR_CLASS_SEPARATOR,
    [','] = CHAR_CLASS_SEPARATOR,
    ['['] = CHAR_CLASS_SEPARATOR,
    [']'] = CHAR_CLASS_SEPARATOR,
    ['('] = CHAR_CLASS_SEPARATOR,
    [')'] = CHAR_CLASS_SEPARATOR,
    ['{'] = CHAR_CLASS_SEPARATOR,
    ['}'] = CHAR_CLASS_SEPARATOR,

    ['+'] = CHAR_CLASS_MULTICHAR1,
    ['*'] = CHAR_CLASS_MULTICHAR1,
    ['/'] = CHAR_CLASS_MULTICHAR1,
    ['%'] = CHAR_CLASS_MULTICHAR1,
    ['!'] = CHAR_CLASS_MULTICHAR1,
    ['='] = CHAR_CLASS_MULTICHAR1,
    ['&'] = CHAR_CLASS_MULTICHAR1,
    ['^'] = CHAR_CLASS_MULTICHAR1,
    ['|'] = CHAR_CLASS_MULTICHAR1,
    ['~'] = CHAR_CLASS_MULTICHAR1,

    ['>'] = CHAR_CLASS_MULTICHAR2,
    ['<'] = CHAR_CLASS_MULTICHAR2,

    ['-'] = CHAR_CLASS_MULTICHAR3,

    ['\"'] = CHAR_CLASS_STRING,
    ['\''] = CHAR_CLASS_STRING,

    ['.'] = CHAR_CLASS_DOT,

    ['#'] = CHAR_CLASS_HASH,
};

uint16_t hash_with_len(const void* data, size_t len) {
    uint8_t* p = (uint8_t*)data;
    uint16_t hash = 0;

    for (size_t i = 0; i < len; i++) {
        hash ^= (p[i] << (i % 8));
    }

    return hash;
}

TknType classify_ident(const unsigned char* restrict str, size_t len) {
    // Auto-generated with this small C program (MAKE SURE TO UPDATE THE
    // KEYWORDS ARRAY AND TOKEN TYPES)
    //
    // https://gist.github.com/RealNeGate/397db4aaace43e0499dc8f7b429ccc17
    //
    // BINARY SEARCH ARRAYS
    const static uint16_t keys[64] = {

        0x00A5,
        0x00BA,
        0x0165,
        0x0170,
        0x0205,
        0x0211,
        0x0223,
        0x022C,
        0x0232,
        0x0245,
        0x0259,
        0x02A7,
        0x0433,
        0x0495,
        0x04AA,
        0x04DF,
        0x054A,
        0x05CF,
        0x05DD,
        0x080D,
        0x081E,
        0x0820,
        0x084F,
        0x0851,
        0x088D,
        0x089D,
        0x09A9,
        0x0A4B,
        0x10A3,
        0x110E,
        0x1145,
        0x11AF,
        0x137D,
        0x145F,
        0x15EE,
        0x2155,
        0x21E5,
        0x21F0,
        0x22A1,
        0x22DD,
        0x2635,
        0x2681,
        0x2855,
        0x2A14,
        0x2B9C,
        0x2D8A,
        0x2DCD,
        0x2E11,
        0x34C2,
        0x3AC1,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,
        0xFFFF,

    };
    const static uint8_t values[64] = {
        15,
        7,
        17,
        13,
        10,
        14,
        0,
        31,
        18,
        2,
        9,
        3,
        33,
        29,
        1,
        37,
        12,
        22,
        4,
        16,
        8,
        21,
        25,
        24,
        11,
        23,
        27,
        26,
        45,
        28,
        36,
        41,
        47,
        46,
        6,
        35,
        39,
        32,
        40,
        30,
        5,
        48,
        34,
        20,
        19,
        49,
        38,
        42,
        43,
        44,
    };

    // HASH STRING
    uint16_t n = hash_with_len(str, len);

    // BRANCHLESS BINARY SEARCH
    size_t i = 0;
    i += (keys[i + 32] <= n) * 32;
    i += (keys[i + 16] <= n) * 16;
    i += (keys[i + 8] <= n) * 8;
    i += (keys[i + 4] <= n) * 4;
    i += (keys[i + 2] <= n) * 2;
    i += (keys[i + 1] <= n) * 1;
    size_t v = values[i];

    // VERIFY
    #if !USE_INTRIN
    return strcmp(str, keywords[v]) == 0 ? (640 + v) : TOKEN_IDENTIFIER;
    #else
    __m128i kw128 = _mm_loadu_si128((__m128i*)&keywords[v]);
    __m128i str128 = _mm_loadu_si128((__m128i*)str);

    int kw_len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(kw128, _mm_set1_epi8('\0')))) - 1;

    // NOTE(NeGate): Fancy x86 strcmp crap :)
    int result = _mm_cmpestri(kw128, kw_len,
        str128, len,
        _SIDD_UBYTE_OPS |
        _SIDD_CMP_EQUAL_EACH |
        _SIDD_NEGATIVE_POLARITY |
        _SIDD_UNIT_MASK);

    return result == 16 ? (640 + v) : TOKEN_IDENTIFIER;
    #endif
}

static int line_counter(size_t len, const unsigned char* str) {
    #if 1
    int line_count = 0;
    for (size_t i = 0; i < len; i++) {
        line_count += (str[i] == '\n');
    }

    return line_count;
    #else
    // TODO(NeGate): Test this out a bit before using it
    static unsigned char overhang_mask[32] = {
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    };

    size_t line_count = 0;
    size_t chunk_count = len / 16;
    while (chunk_count) {
        __m128i str128 = _mm_loadu_si128((__m128i*)str);
        str += 16;

        unsigned int lf_mask = _mm_movemask_epi8(_mm_cmpeq_epi8(str128, _mm_set1_epi8('\n')));
        line_count += __builtin_popcount(lf_mask);
    }

    size_t overhang = len % 16;
    __m128 str128 = _mm_and_si128(str, _mm_loadu_si128((__m128i*)&overhang_mask[16 - overhang]));
    unsigned int lf_mask = _mm_movemask_epi8(_mm_cmpeq_epi8(str128, _mm_set1_epi8('\n')));
    line_count += __builtin_popcount(lf_mask);

    return line_count;
    #endif
}

// NOTE(NeGate): The input string has a fat null terminator of 16bytes to allow
// for some optimizations overall, one of the important ones is being able to read
// a whole 16byte SIMD register at once for any SIMD optimizations.
void lexer_read(Lexer* restrict l) {
    if (l->line_current2) {
        l->line_current = l->line_current2;
        l->line_current2 = NULL;
    }

    const unsigned char* current = l->current;

    // Skip any whitespace and comments
    // branchless space skip
    current += (*current == ' ');

    // NOTE(NeGate): We canonicalized spaces \t \v
    // in the preprocessor so we don't need to handle them
    redo_lex: {
        if (*current == '\0') {
            // quit, we're done
            l->hit_line = true;
            l->token_type = '\0';
            return;
        } else if (*current == '\r' || *current == '\n') {
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;

            l->line_current = current;
            l->hit_line = true;
            l->current_line += 1;
            goto redo_lex;
        } else if (*current == ' ') {
            #if !USE_INTRIN
            current += 1;
            #else
            // SIMD space skip
            __m128i chars = _mm_loadu_si128((__m128i*)current);
            int len = __builtin_ffs(~_mm_movemask_epi8(_mm_cmpeq_epi8(chars, _mm_set1_epi8(' '))));

            current += len - 1;
            #endif

            goto redo_lex;
        } else if (*current == '/') {
            if (current[1] == '/') {
                do {
                    current++;
                } while (*current && *current != '\n');

                current += 1;
                l->line_current = current;
                l->hit_line = true;
                l->current_line += 1;
                goto redo_lex;
            } else if (current[1] == '*') {
                current++;

                const unsigned char* start = current;
                do {
                    current++;
                } while (*current && !(current[0] == '/' && current[-1] == '*'));
                current++;

                int lines_elapsed = line_counter(current - start, start);

                l->line_current = current;
                l->current_line += lines_elapsed;
                if (lines_elapsed > 0) l->hit_line = true;
                goto redo_lex;
            }
        } else if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
            // this happens when there's a backslash-newline that doesn't
            // necessarily need to join tokens but just joins the lines
            current += 1;
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;

            l->line_current = current;
            l->current_line += 1;
            goto redo_lex;
        }
    }

    ////////////////////////////////
    // Try to actually parse a token
    ////////////////////////////////
    const unsigned char* start = current;
    uint8_t initial_class = char_classes[*current++];

    // Hacky but yea
    bool slow_identifier_lexing = false;
    if (start[0] == 'L' && (start[1] == '\"' || start[1] == '\'')) {
        initial_class = CHAR_CLASS_STRING;
        current++;
    } else if (start[0] == '\\') {
        if (start[1] == 'U' || start[1] == 'u') {
            slow_identifier_lexing = true;
            initial_class = CHAR_CLASS_IDENT;
        }
    }

    switch (initial_class) {
        case CHAR_CLASS_NULL:
        break;

        case CHAR_CLASS_IDENT: {
            l->token_type = TOKEN_IDENTIFIER;

            while (char_classes[*current] == CHAR_CLASS_IDENT ||
                char_classes[*current] == CHAR_CLASS_NUMBER ||
                *current == '\\') {
                if (current[0] == '\\') {
                    if (current[0] == 'U' || current[0] == 'u') {
                        slow_identifier_lexing = true;
                    } else {
                        // exit... it's a wonky identifier
                        break;
                    }
                }

                current++;
            }

            if (!slow_identifier_lexing) break;

            // you don't wanna be here... basically if we spot \U in the identifier
            // we reparse it but this time with the correct handling of those details.
            // we also generate a new string in the arena to hold this stuff
            // at best it's as big as the raw identifier
            size_t oldstr_len = current - start;
            char* newstr = arena_alloc(&thread_arena, (oldstr_len + 15) & ~15u, 1);

            size_t i = 0, j = 0;
            while (i < oldstr_len) {
                if (start[i] == '\\' && (start[i+1] == 'U' || start[i+1] == 'u')) {
                    // parse codepoint
                    i += 2;

                    uint32_t code = 0;
                    while (i < oldstr_len) {
                        char ch = start[i];

                        if (ch >= 'A' && ch <= 'F') {
                            code <<= 4;
                            code |= (ch - 'A') + 0xA;
                        } else if (ch >= 'a' && ch <= 'f') {
                            code <<= 4;
                            code |= (ch - 'a') + 0xA;
                        } else if (ch >= '0' && ch <= '9') {
                            code <<= 4;
                            code |= (ch - '0');
                        } else break;

                        i += 1;
                    }

                    // convert into proper bytes
                    // https://gist.github.com/Miouyouyou/864130e8734afe3f806512b14022226f
                    if (code < 0x80) {
                        newstr[j++] = code;
                    } else if (code < 0x800) {   // 00000yyy yyxxxxxx
                        newstr[j++] = (0b11000000 | (code >> 6));
                        newstr[j++] = (0b10000000 | (code & 0x3f));
                    } else if (code < 0x10000) {  // zzzzyyyy yyxxxxxx
                        newstr[j++] = (0b11100000 | (code >> 12));         // 1110zzz
                        newstr[j++] = (0b10000000 | ((code >> 6) & 0x3f)); // 10yyyyy
                        newstr[j++] = (0b10000000 | (code & 0x3f));        // 10xxxxx
                    } else if (code < 0x200000) { // 000uuuuu zzzzyyyy yyxxxxxx
                        newstr[j++] = (0b11110000 | (code >> 18));          // 11110uuu
                        newstr[j++] = (0b10000000 | ((code >> 12) & 0x3f)); // 10uuzzzz
                        newstr[j++] = (0b10000000 | ((code >> 6)  & 0x3f)); // 10yyyyyy
                        newstr[j++] = (0b10000000 | (code & 0x3f));         // 10xxxxxx
                    } else {
                        assert(0);
                    }
                } else {
                    newstr[j++] = start[i++];
                }
            }

            l->token_start = (unsigned char*)newstr;
            l->token_end = (unsigned char*) &newstr[j];
            l->current = current;

            // place a temporary 'line_current' such that it doesn't break when doing
            // column calculations
            l->line_current = l->token_start;
            l->line_current2 = current;
            return;
        }
        case CHAR_CLASS_NUMBER: {
            if (current[-1] == '0' && current[0] == 'b') {
                current++;

                while (*current == '0' || *current == '1') {
                    current++;
                }

                l->token_type = TOKEN_INTEGER;
            } else if (current[-1] == '0' && current[0] == 'x') {
                int hex = 0;
                if(current[0] == 'x') {
                    hex = 1;
                }

                current++;

                while ((*current >= '0' && *current <= '9') ||
                    (*current >= 'A' && *current <= 'F')    ||
                    (*current >= 'a' && *current <= 'f')) {
                    current++;
                }

                l->token_type = TOKEN_INTEGER;
                if (*current == '.') {
                    // floats
                    l->token_type = TOKEN_FLOAT;
                    current++;

                    if (!hex) {
                        while (char_classes[*current] == CHAR_CLASS_NUMBER) {
                            current++;
                        }
                    } else {
                        while ((*current >= '0' && *current <= '9') ||
                            (*current >= 'A' && *current <= 'F')    ||
                            (*current >= 'a' && *current <= 'f')) {
                            current++;
                        }
                    }

                    if (*current == 'p') {
                        current++;
                        if (*current == '+' || *current == '-') current++;

                        while (char_classes[*current] == CHAR_CLASS_NUMBER) {
                            current++;
                        }
                    }
                }
            } else {
                while (char_classes[*current] == CHAR_CLASS_NUMBER) {
                    current++;
                }
                l->token_type = TOKEN_INTEGER;

                if (*current == '.') {
                    // floats
                    l->token_type = TOKEN_FLOAT;
                    current++;

                    while (char_classes[*current] == CHAR_CLASS_NUMBER) {
                        current++;
                    }
                }

                if (*current == 'e') {
                    // floats but cooler
                    l->token_type = TOKEN_FLOAT;

                    current++;
                    if (*current == '+' || *current == '-') current++;

                    while (char_classes[*current] == CHAR_CLASS_NUMBER) {
                        current++;
                    }
                }

                if (*current == 'f' || *current == 'd') {
                    l->token_type = TOKEN_FLOAT;
                    current++;
                }
            }

            // suffix
            if (*current == 'i') {
                current++;

                // at most it's two numbers
                current += (char_classes[*current] == CHAR_CLASS_NUMBER);
                current += (char_classes[*current] == CHAR_CLASS_NUMBER);
            } else if (*current == 'u') {
                current++;

                while (char_classes[*current] == CHAR_CLASS_IDENT) {
                    current++;
                }

                // at most it's two numbers
                current += (char_classes[*current] == CHAR_CLASS_NUMBER);
                current += (char_classes[*current] == CHAR_CLASS_NUMBER);
            } else {
                while (char_classes[*current] == CHAR_CLASS_IDENT) {
                    current++;
                }
            }
            break;
        }
        case CHAR_CLASS_SEPARATOR: {
            l->token_type = *start;
            break;
        }
        case CHAR_CLASS_MULTICHAR1: {
            l->token_type = *start;
            if (*current == '=') {
                l->token_type += 384;
                current++;
            } else if (*current == *start && *current != '*') {
                l->token_type += 256;
                current++;
            }
            break;
        }
        case CHAR_CLASS_MULTICHAR2: {
            l->token_type = *start;
            if (*current == '=') {
                l->token_type += 256;
                current++;
            } else if (*current == *start) {
                l->token_type += 384;
                current++;

                if (*current == '=') {
                    l->token_type += 128;
                    current++;
                }
            }
            break;
        }
        case CHAR_CLASS_MULTICHAR3: {
            l->token_type = *start;

            if (*current == '-') {
                l->token_type = TOKEN_DECREMENT;
                current++;
            } else if (*current == '=') {
                l->token_type = TOKEN_MINUS_EQUAL;
                current++;
            } else if (*current == '>') {
                l->token_type = TOKEN_ARROW;
                current++;
            }
            break;
        }
        case CHAR_CLASS_STRING: {
            char quote_type = current[-1] == '\'' ? '\'' : '\"';

            #if !USE_INTRIN
            do {
                if (*current == quote_type && current[-1] == '\\') break;
                if (*current == '\n') l->current_line += 1;

                current += 1;
            } while (*current);
            #else
            __m128i pattern = _mm_set1_epi8(quote_type);

            do {
                __m128i bytes = _mm_loadu_si128((__m128i*)current);

                // strings either end at the quote or are cut off early via a
                // newline unless you put a backslash-newline joiner.
                __m128i test_quote = _mm_cmpeq_epi8(bytes, pattern);
                __m128i test_newline = _mm_cmpeq_epi8(bytes, _mm_set1_epi8('\n'));
                __m128i test = _mm_or_si128(test_quote, test_newline);
                int len = __builtin_ffs(_mm_movemask_epi8(test));

                if (len) {
                    current += len;
                    l->current_line += (current[-1] == '\n');

                    // backslash join
                    if (current[-1] == '\n' && current[-2] == '\\') continue;

                    // escape + quote like \"
                    if (current[-1] == quote_type && current[-2] == '\\' && current[-3] != '\\') continue;

                    break;
                } else {
                    current += 16;
                }
            } while (*current);
            #endif

            l->token_type = quote_type;

            if (start[0] == 'L') {
                l->token_type += 256;
                start += 1;
            }
            break;
        }
        case CHAR_CLASS_DOT: {
            if (current[0] == '.' && current[1] == '.') {
                current += 2;

                l->token_type = TOKEN_TRIPLE_DOT;
                break;
            }

            l->token_type = '.';
            break;
        }
        case CHAR_CLASS_HASH: {
            if (*current == '#') {
                current++;
                l->token_type = TOKEN_DOUBLE_HASH;
                break;
            }

            l->token_type = TOKEN_HASH;
            break;
        }
        default:
        abort();
    }

    if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
        // TODO(NeGate): This code could use emotional help... if you're smart
        // and/or cool please consider providing it.

        // it increments the line counter but it doesn't mark a hit_line
        // because the line terminator technically is removed.
        l->current_line += 1;

        // save out the original token position
        l->token_start = start;
        l->token_end = current;

        // skip backslash and newline
        current += 1;
        current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;

        l->line_current2 = current;

        start = current;
        while (*current && *current != '\n' && *current != ' ') {
            char c0 = current[0], c1 = current[1];

            if (c0 == '\\' && c1 == '\n') current += 2;
            else current += 1;
        }

        // tally up lines
        l->current_line += line_counter(current - start, start);

        // generate buffer with conjoined string
        unsigned char* conjoined_buffer;
        size_t len = l->token_end - l->token_start;
        size_t len2 = current - start;

        {
            // len + len2 + 1 padded to 16bytes since all lexer strings have to be for
            // the SIMD related things
            conjoined_buffer = arena_alloc(&thread_arena, (len + len2 + 16) & ~15, 16);
            if (!conjoined_buffer) {
                printf("Lexer error: out of memory!");
                abort();
            }

            l->line_current = conjoined_buffer;

            memcpy(conjoined_buffer, l->token_start, len);
            memcpy(conjoined_buffer + len, start, len2);

            // null terminator to top it off
            conjoined_buffer[len + len2] = '\0';
        }

        // Relex the joined string:
        // Kinda recursive in a way but... shut up?
        Lexer joined_string_lexer = (Lexer){"", conjoined_buffer, conjoined_buffer, 1};
        lexer_read(&joined_string_lexer);

        l->token_start = joined_string_lexer.token_start;
        l->token_end = joined_string_lexer.token_end;

        // NOTE(NeGate): Basically take the remaining token stuff we didn't parse
        // and just pass that but the issue is that these are two separate buffers
        // so we do a little "magic?".
        l->current = start + ((l->token_end - l->token_start) - len);
    } else {
        l->token_start = start;
        l->token_end = current;
        l->current = current;
    }
}

uint64_t parse_int(size_t len, const char* str, Cuik_IntSuffix* out_suffix) {
    char* end;
    uint64_t i = strtoull(str, &end, 0);

    Cuik_IntSuffix suffix = INT_SUFFIX_NONE;
    if (end != &str[len]) {
        size_t remaining = &str[len] - end;
        if (remaining == 4) {
            if (memcmp(end, "ui16", 4) == 0) {
                goto success;
            } else if (memcmp(end, "ui32", 4) == 0) {
                suffix = INT_SUFFIX_U;
                goto success;
            } else if (memcmp(end, "ui64", 4) == 0) {
                suffix = INT_SUFFIX_ULL;
                goto success;
            }
        } else if (remaining == 3) {
            if (memcmp(end, "i16", 3) == 0) {
                goto success;
            } else if (memcmp(end, "i32", 3) == 0) {
                goto success;
            } else if (memcmp(end, "i64", 3) == 0) {
                suffix = INT_SUFFIX_LL;
                goto success;
            }
        } else if (remaining == 2 && memcmp(end, "i8", 2) == 0) {
            goto success;
        }

        do {
            switch (end[0]) {
                case 'u':
                case 'U':
                suffix |= 1;
                break;
                case 'l':
                case 'L':
                suffix += 2;
                break;
                default:
                break;
            }
            end++;
        } while (end != &str[len]);

        if (suffix >= 6) abort();
    }

    success:
    *out_suffix = suffix;
    return i;
}

double parse_float(size_t len, const char* str) {
    char* end;
    double i = strtod(str, &end);
    if (end != &str[len]) {
        if (*end != 'f' && *end != 'd' && *end != 'F' && *end != 'D') abort();
    }

    return i;
}

intptr_t parse_char(size_t len, const char* str, int* output) {
    if (str[0] != '\\') {
        *output = str[0];
        return 1;
    }

    // error: expected something after the backslash
    if (len < 1) return -1;

    int ch = 0;
    size_t i = 2;
    switch (str[1]) {
        // TODO(NeGate): Implement the rest of the C char variants
        // \U0001f34c
        case '0' ... '9': {
            unsigned int num = 0;

            while (i < len) {
                char ch = str[i];
                if (!(ch >= '0' && ch <= '9')) break;

                num *= 10;
                num += (ch - '0');
                i += 1;
            }

            ch = num;
            break;
        }
        case 'x':
        case 'X': {
            unsigned int num = 0;

            while (i < len) {
                char ch = str[i];

                if (ch >= 'A' && ch <= 'F') {
                    num <<= 4;
                    num |= (ch - 'A') + 0xA;
                } else if (ch >= 'a' && ch <= 'f') {
                    num <<= 4;
                    num |= (ch - 'a') + 0xA;
                } else if (ch >= '0' && ch <= '9') {
                    num <<= 4;
                    num |= (ch - '0');
                } else break;

                i += 1;
            }

            ch = num;
            break;
        }
        case '\\':
        ch = '\\';
        break;
        case 'a':
        ch = '\a';
        break;
        case 'b':
        ch = '\b';
        break;
        case 't':
        ch = '\t';
        break;
        case 'n':
        ch = '\n';
        break;
        case 'v':
        ch = '\v';
        break;
        case 'f':
        ch = '\f';
        break;
        case 'r':
        ch = '\r';
        break;
        case '\'':
        ch = '\'';
        break;
        case '\"':
        ch = '\"';
        break;
        default:
        return -1;
    }

    *output = ch;
    return i;
}
