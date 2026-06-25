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

enum {
    KW_C = 1, KW_GLSL = 2,
};

#include "dfa.h"

static uint64_t hash_with_len(const void* data, size_t len) {
    const uint8_t* p = data;
    uint64_t h = 0;
    for (size_t i = 0; i < len && i < 8; i++) {
        h |= (uint64_t)p[i] << (uint64_t)(i*8);
    }
    return LEXER_KEYWORD_HASH(h);
}

TknType classify_ident(const unsigned char* restrict str, size_t len, bool is_glsl) {
    size_t v = hash_with_len(str, len);
    uint8_t search = is_glsl ? (KW_GLSL | KW_C) : KW_C;

    // keyword's not for our frontend
    if ((keyword_type[v] & search) == 0) {
        return TOKEN_IDENTIFIER;
    }

    // VERIFY
    #if USE_INTRIN && CUIK__IS_X64
    __m128i kw128 = _mm_loadu_si128((__m128i*)&keywords[v]);
    __m128i str128 = _mm_loadu_si128((__m128i*)str);

    int kw_len = __builtin_ffs(_mm_movemask_epi8(_mm_cmpeq_epi8(kw128, _mm_set1_epi8('\0')))) - 1;

    // NOTE(NeGate): Fancy x86 strcmp crap :)
    int result = _mm_cmpestri(kw128, kw_len, str128, len,
        _SIDD_UBYTE_OPS |
        _SIDD_CMP_EQUAL_EACH |
        _SIDD_NEGATIVE_POLARITY |
        _SIDD_UNIT_MASK
    );

    return result == 16 ? (0x800000 + v) : TOKEN_IDENTIFIER;
    #else
    if (strlen(keywords[v]) != len) return TOKEN_IDENTIFIER;

    return memcmp((const char*) str, keywords[v], len) == 0 ? (0x800000 + v) : TOKEN_IDENTIFIER;
    #endif
    return TOKEN_IDENTIFIER;
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

static unsigned char* backslash_join(Lexer* restrict l, unsigned char* start, unsigned char* current) {
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
    return mini.current;
}

uint64_t parse_int(size_t len, const char* str, Cuik_IntSuffix* out_suffix) {
    char* end;
    uint64_t num = 0;
    if (len >= 2 && str[1] == 'b') {
        size_t i = 2;
        for (; i < len && (str[i] == '0' || str[i] == '1'); i++) {
            num <<= 1;
            num |= str[i] != '0' ? 1 : 0;
        }
        end = (char*) str+i;
    } else {
        num = strtoull(str, &end, 0);
    }

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
    return num;
}

ptrdiff_t parse_char(size_t len, const char* str, int* output) {
    if (str[0] != '\\') {
        *output = str[0];
        return 1;
    }

    // error: expected something after the backslash
    if (len < 1) {
        return -1;
    }

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
