#include "lexer.h"
#include <arena.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

#ifdef __CUIKC__
#define ALWAYS_INLINE inline
#else
#define ALWAYS_INLINE __attribute__((always_inline))
#endif

// Just a big table called 'dfa'
// https://gist.github.com/RealNeGate/9fd2886c56fa01049c5e85fc8f0fd9b7
#include "dfa.h"

static uint64_t hash_with_len(const void* data, size_t len) {
    const uint8_t* p = data;
    uint64_t h = 0;
    for (size_t j = 0; j < len && j < 7; j++) {
        h = p[j] + h*256;
    }
    // The low byte of the signature is the length.
    h = len + h*256;
    return h;
}

static TknType classify_ident(const unsigned char* restrict str, size_t len, bool is_glsl) {
    // Auto-generated with lexgen.c
    size_t v = (hash_with_len(str, len) * PERFECT_HASH_SEED) >> 56;
    v = keywords_table[v];

    if (!is_glsl && v >= FIRST_GLSL_KEYWORD - 0x10000000) {
        return TOKEN_IDENTIFIER;
    }

    // VERIFY
    #if USE_INTRIN && CUIK__IS_X64
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

    return result == 16 ? (0x10000000 + v) : TOKEN_IDENTIFIER;
    #else
    if (strlen(keywords[v]) != len) return TOKEN_IDENTIFIER;

    return memcmp((const char*) str, keywords[v], len) == 0 ? (0x10000000 + v) : TOKEN_IDENTIFIER;
    #endif
}

static unsigned char* slow_identifier_lexing(Lexer* restrict l, unsigned char* current, unsigned char* start) {
    // you don't wanna be here... basically if we spot \U in the identifier
    // we reparse it but this time with the correct handling of those details.
    // we also generate a new string in the arena to hold this stuff
    // at best it's as big as the raw identifier
    size_t oldstr_len = current - start;
    unsigned char* newstr = start;

    size_t i = 0, j = 0;
    while (i < oldstr_len) {
        if (start[i] == '\\' && (start[i+1] == 'U' || start[i+1] == 'u')) {
            // lowercase is \unnnn and uppercase is \Unnnnnnnn
            size_t uchar_len = start[i+1] == 'U' ? 8 : 4;
            // parse codepoint
            i += 2;

            uint32_t code = 0;
            size_t endpoint = i + uchar_len;
            if (endpoint > oldstr_len) endpoint = oldstr_len;

            while (i < endpoint) {
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

    memset(&newstr[j], ' ', oldstr_len - j);
    return (unsigned char*) &newstr[j];
}

// NOTE(NeGate): The input string has a fat null terminator of 16bytes to allow
// for some optimizations overall, one of the important ones is being able to read
// a whole 16byte SIMD register at once for any SIMD optimizations.
static Token lexer_read(Lexer* restrict l) {
    unsigned char* current = l->current;
    Token t = { 0 };

    // Skip any whitespace and comments
    // branchless space skip
    current += (*current == ' ');

    // NOTE(NeGate): We canonicalized spaces \t \v
    // in the preprocessor so we don't need to handle them
    redo_lex: {
        if (*current == '\0') {
            // quit, we're done
            t.hit_line = true;
            t.type = '\0';
            return (Token){ 0 };
        } else if (*current == '\r' || *current == '\n') {
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;

            t.hit_line = true;
            goto redo_lex;
        } else if (*current == ' ') {
            #if !USE_INTRIN
            current += 1;
            #else
            // SIMD space skip
            __m128i chars = _mm_loadu_si128((__m128i*) current);
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
                t.hit_line = true;
                goto redo_lex;
            } else if (current[1] == '*') {
                current++;

                unsigned char* start = current;
                do {
                    if (*current == '\n') t.hit_line = true;

                    current++;
                } while (*current && !(current[0] == '/' && current[-1] == '*'));
                current++;
                goto redo_lex;
            }
        } else if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
            // this happens when there's a backslash-newline that doesn't
            // necessarily need to join tokens but just joins the lines
            current += 1;
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
            goto redo_lex;
        }
    }

    ////////////////////////////////
    // Try to actually parse a token
    ////////////////////////////////
    unsigned char* start = current;
    uint64_t state = 0;

    for (;;) {
        uint8_t next = dfa[*current][state];
        if (next == 0) break;
        state = next, current += 1;
    }

    // generate valid token types
    switch (state) {
        case 0: {
            fprintf(stderr, "illegal lexer char: %c (%d)\n", *start, *start);
            current++;
            goto redo_lex;
        }
        case DFA_IDENTIFIER_L:
        case DFA_IDENTIFIER: {
            t.type = TOKEN_IDENTIFIER;
            if (current[-1] == '\\') {
                current -= 1;
            }

            #if !USE_INTRIN
            for (unsigned char* s = start; s != current; s++) {
                if (*s == '\\') {
                    current = slow_identifier_lexing(l, current, start);
                    break;
                }
            }
            #else
            // check for escapes
            __m128i pattern = _mm_set1_epi8('\\');
            size_t length = current - start;

            for (size_t i = 0; i < length; i += 16) {
                __m128i bytes = _mm_loadu_si128((__m128i*) &start[i]);
                __m128i test = _mm_cmpeq_epi8(bytes, pattern);
                unsigned int mask = _mm_movemask_epi8(test);

                if (__builtin_expect(mask, 0)) {
                    // slow identifier lexing since we might have a universal character
                    int endpoint = length - i;
                    int last_escape = __builtin_ctz(mask);

                    if (last_escape < endpoint) {
                        current = slow_identifier_lexing(l, current, start);
                        break;
                    }
                }
            }
            #endif
            break;
        }
        case DFA_NUMBER: {
            t.type = TOKEN_INTEGER;
            current = start;

            for (;;) {
                char a = *current;
                if (a == '.') {
                    current += 1;
                    t.type = TOKEN_FLOAT;
                } else if ((a == 'e' || a == 'E' || a == 'p' || a == 'P') && (current[1] == '+' || current[1] == '-')) {
                    current += 2;
                } else if ((a >= '0' && a <= '9') || (a >= 'a' && a <= 'z') || (a >= 'A' && a <= 'Z')) {
                    current += 1;
                } else {
                    break;
                }
            }
            break;
        }
        case DFA_STRING: {
            char quote_type = current[-1];

            for (; *current && *current != quote_type; current++) {
                // skip escape codes
                if (*current == '\\') {
                    // this will skip twice because of the for loop's next
                    //  \  "  . . .
                    //  ^     ^
                    //  old   new
                    current += 1;
                }
            }

            current += 1;
            t.type = quote_type;

            if (start[0] == 'L') {
                t.type += 256;
                start += 1;
            }
            break;
        }
        default: {
            // add chars together (max of 3)
            int length = current - start;
            if (length > 3) length = 3;

            uint32_t mask = UINT32_MAX >> ((4 - length) * 8);

            // potentially unaligned access :P
            uint32_t chars;
            memcpy(&chars, start, sizeof(uint32_t));

            t.type = chars & mask;
            break;
        }
    }

    // NOTE(NeGate): the lexer will modify code to allow for certain patterns
    // if we wanna get rid of this we should make virtual code regions
    if (__builtin_expect(current[0] == '\\' && (current[1] == '\r' || current[1] == '\n'), 0)) {
        // skip backslash and newline
        current += 1;
        current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;

        // find token boundary to shift things correctly
        unsigned char* next_token_bound = current;
        for (unsigned char* s = current;; s++) {
            if (s[0] == '\0' || s[0] == ' ' || s[0] == '\n') {
                next_token_bound = s;
                break;
            } else if (s[0] == '\\' && s[1] == '\n') {
                s += 1;
            }
        }

        // join backslash-newlines
        size_t length = (next_token_bound - start);
        for (size_t i = 0; i < length; i++) {
            if (start[i] == '\\' && (start[i + 1] == '\r' || start[i + 1] == '\n')) {
                size_t deletion_len = (start[i + 1] + start[i + 2] == '\r' + '\n') ? 3 : 2;

                memmove(start + i, start + i + deletion_len, length - (i + deletion_len));
                length -= deletion_len, i -= 1;
            }
        }

        // fill excess with spaces
        size_t space_count = (next_token_bound - start) - length;
        memset(start + length, ' ', space_count);

        // re-read token
        Lexer mini = { .start = start, .current = start };
        lexer_read(&mini);

        current = mini.current;
    }
    l->current = current;

    // encode token
    t.content = (String){ current - start, start };
    t.location = encode_file_loc(l->file_id, start - l->start);
    return t;
}

static uint64_t parse_int(size_t len, const char* str, Cuik_IntSuffix* out_suffix) {
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

static ptrdiff_t parse_char(size_t len, const char* str, int* output) {
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
        case 'a': ch = '\a'; break;
        case 'b': ch = '\b'; break;
        case 't': ch = '\t'; break;
        case 'n': ch = '\n'; break;
        case 'v': ch = '\v'; break;
        case 'f': ch = '\f'; break;
        case 'r': ch = '\r'; break;
        case '\'': ch = '\''; break;
        case '\"': ch = '\"'; break;
        case '\\': ch = '\\'; break;
        default:
        return -1;
    }

    *output = ch;
    return i;
}
