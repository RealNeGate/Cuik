#pragma once
#include <common.h>
#include <str.h>
#include <arena.h>
#include <cuik.h>
#include <ext/stb_ds.h>

// NOTE(NeGate): I originally called it TokenType but windows a bih on god
typedef enum TknType {
    TOKEN_ACCESSOR = '.',
    TOKEN_COMMA = ',',

    TOKEN_PLUS = '+',
    TOKEN_MINUS = '-',
    TOKEN_TIMES = '*',
    TOKEN_SLASH = '/',
    TOKEN_PERCENT = '%',
    TOKEN_ASSIGN = '=',

    TOKEN_AND = '&',
    TOKEN_XOR = '^',
    TOKEN_OR = '|',

    TOKEN_HASH = '#',
    TOKEN_AT = '@',

    TOKEN_COLON = ':',
    TOKEN_SEMICOLON = ';',

    TOKEN_LESS = '<',
    TOKEN_GREATER = '>',

    TOKEN_BRACKET_OPEN = '[',
    TOKEN_BRACKET_CLOSE = ']',

    TOKEN_PAREN_OPEN = '(',
    TOKEN_PAREN_CLOSE = ')',

    TOKEN_BRACE_OPEN = '{',
    TOKEN_BRACE_CLOSE = '}',

    TOKEN_STRING_SINGLE_QUOTE = '\'',
    TOKEN_STRING_DOUBLE_QUOTE = '\"',

    // L"hello"
    TOKEN_STRING_WIDE_SINGLE_QUOTE = '\'' + 256,
    TOKEN_STRING_WIDE_DOUBLE_QUOTE = '\"' + 256,

    TOKEN_IDENTIFIER = 256,
    TOKEN_INTEGER,
    TOKEN_FLOAT,
    TOKEN_TRIPLE_DOT,

    TOKEN_INVALID,

    TOKEN_ARROW,                   /* ->  */
    TOKEN_DOUBLE_HASH = '#' + 256, /* ##  */

    TOKEN_DOUBLE_AND = '&' + 256, /* &=  */
    TOKEN_DOUBLE_OR = '|' + 256,  /* |=  */

    TOKEN_PLUS_EQUAL = '+' + 384,    /* +=  */
    TOKEN_MINUS_EQUAL = '-' + 384,   /* -=  */
    TOKEN_TIMES_EQUAL = '*' + 384,   /* *=  */
    TOKEN_SLASH_EQUAL = '/' + 384,   /* /=  */
    TOKEN_PERCENT_EQUAL = '%' + 384, /* %=  */
    TOKEN_OR_EQUAL = '|' + 384,      /* |=  */
    TOKEN_AND_EQUAL = '&' + 384,     /* &=  */
    TOKEN_XOR_EQUAL = '^' + 384,     /* ^=  */
    TOKEN_NOT_EQUAL = '!' + 384,     /* !=  */
    TOKEN_EQUALITY = '=' + 384,      /* ==  */

    TOKEN_GREATER_EQUAL = '>' + 256, /* >=  */
    TOKEN_LESS_EQUAL = '<' + 256,    /* <=  */
    TOKEN_LEFT_SHIFT = '<' + 384,    /* <<  */
    TOKEN_RIGHT_SHIFT = '>' + 384,   /* >>  */

    TOKEN_LEFT_SHIFT_EQUAL = '<' + 512,  /* <<= */
    TOKEN_RIGHT_SHIFT_EQUAL = '>' + 512, /* >>= */
    TOKEN_INCREMENT = '+' + 256,         /* ++  */
    TOKEN_DECREMENT = '-' + 256,         /* --  */

    // this is hacky because ! is a single char
    // token (sometimes used in !=) but this way
    // it can share more rules with the rest of the
    // tokens (less cases in the lexer).
    TOKEN_DOUBLE_EXCLAMATION = '!' + 256, /* !!  */

    // Keywords
    TOKEN_KW_auto = 640,
    TOKEN_KW_break,
    TOKEN_KW_case,
    TOKEN_KW_char,
    TOKEN_KW_const,
    TOKEN_KW_continue,
    TOKEN_KW_default,
    TOKEN_KW_do,
    TOKEN_KW_double,
    TOKEN_KW_else,
    TOKEN_KW_enum,
    TOKEN_KW_extern,
    TOKEN_KW_float,
    TOKEN_KW_for,
    TOKEN_KW_goto,
    TOKEN_KW_if,
    TOKEN_KW_inline,
    TOKEN_KW_int,
    TOKEN_KW_long,
    TOKEN_KW_register,
    TOKEN_KW_restrict,
    TOKEN_KW_return,
    TOKEN_KW_short,
    TOKEN_KW_signed,
    TOKEN_KW_sizeof,
    TOKEN_KW_static,
    TOKEN_KW_struct,
    TOKEN_KW_switch,
    TOKEN_KW_typedef,
    TOKEN_KW_union,
    TOKEN_KW_unsigned,
    TOKEN_KW_void,
    TOKEN_KW_volatile,
    TOKEN_KW_while,
    TOKEN_KW_Alignas,
    TOKEN_KW_Alignof,
    TOKEN_KW_Atomic,
    TOKEN_KW_Bool,
    TOKEN_KW_Complex,
    TOKEN_KW_Generic,
    TOKEN_KW_Imaginary,
    TOKEN_KW_Pragma,
    TOKEN_KW_Noreturn,
    TOKEN_KW_Static_assert,
    TOKEN_KW_Thread_local,
    TOKEN_KW_Typeof,
    TOKEN_KW_Vector,
    TOKEN_KW_cdecl,
    TOKEN_KW_stdcall,
    TOKEN_KW_declspec,
} TknType;

typedef enum IntSuffix {
    //                u   l   l
    INT_SUFFIX_NONE = 0 + 0 + 0,
    INT_SUFFIX_U    = 1 + 0 + 0,
    INT_SUFFIX_L    = 0 + 2 + 0,
    INT_SUFFIX_UL   = 1 + 2 + 0,
    INT_SUFFIX_LL   = 0 + 2 + 2,
    INT_SUFFIX_ULL  = 1 + 2 + 2,
} IntSuffix;

#define SOURCE_LOC_GET_DATA(loc) ((loc) & ~0xC0000000u)
#define SOURCE_LOC_GET_TYPE(loc) (((loc)&0xC0000000u) >> 30u)
#define SOURCE_LOC_SET_TYPE(type, raw) (((type << 30) & 0xC0000000u) | ((raw) & ~0xC0000000u))

typedef enum SourceLocType {
    SOURCE_LOC_UNKNOWN = 0,
    SOURCE_LOC_NORMAL = 1,
    SOURCE_LOC_MACRO = 2,
    SOURCE_LOC_FILE = 3
} SourceLocType;

// structure to get some nice type checking
typedef uint32_t SourceLocIndex;

typedef struct SourceRange {
    SourceLocIndex start, end;
} SourceRange;

typedef struct SourceLine {
    const char* filepath;
    const unsigned char* line_str;
    SourceLocIndex parent;
    int line;
} SourceLine;

typedef struct SourceLoc {
    SourceLine* line;
    short columns;
    short length;
    int bias;
} SourceLoc;

typedef struct Token {
    TknType type;
    SourceLocIndex location;
    const unsigned char* start;
    const unsigned char* end;
} Token;

typedef struct {
    ////////////////////////////////
    // USER-PROVIDED
    ////////////////////////////////
    const char* filepath;
    const unsigned char* start;
    const unsigned char* current;
    int current_line;

    int column_bias; // it'll just be added to the column count when we're checking stuff

    ////////////////////////////////
    // INTERNALS
    ////////////////////////////////
    const unsigned char* line_current;
    const unsigned char* line_current2;

    // when reading it spotted a line or EOF, it must be manually reset
    bool hit_line;

    // current token info
    TknType token_type;
    const unsigned char* token_start;
    const unsigned char* token_end;
} Lexer;

// this is used by the preprocessor to scan tokens in
void lexer_read(Lexer* restrict l);

intptr_t parse_char(size_t len, const char* str, int* output);
uint64_t parse_int(size_t len, const char* str, IntSuffix* out_suffix);
double parse_float(size_t len, const char* str);
TknType classify_ident(const unsigned char* restrict str, size_t len);

inline static String lexer_get_string(Lexer* restrict l) {
    return string_from_range(l->token_start, l->token_end);
}

inline static bool lexer_match(Lexer* restrict l, size_t len, const char* str) {
    if ((l->token_end - l->token_start) != len) return false;

    return memcmp(l->token_start, str, len) == 0;
}

inline static SourceLocIndex tokens_get_last_location_index(TokenStream* restrict s) {
    return s->tokens[s->current - 1].location;
}

inline static SourceLocIndex tokens_get_location_index(TokenStream* restrict s) {
    return s->tokens[s->current].location;
}

inline static SourceLoc* tokens_get_last_location(TokenStream* restrict s) {
    return &s->locations[s->tokens[s->current - 1].location];
}

inline static SourceLoc* tokens_get_location(TokenStream* restrict s) {
    return &s->locations[s->tokens[s->current].location];
}

// this is used by the parser to get the next token
inline static Token* tokens_get(TokenStream* restrict s) {
    return &s->tokens[s->current];
}

// there should be a NULL token so as long as we can read [current]
// we can read one ahead.
inline static Token* tokens_peek(TokenStream* restrict s) {
    return &s->tokens[s->current + 1];
}

inline static void tokens_prev(TokenStream* restrict s) {
    assert(s->current > 0);
    s->current -= 1;
}

inline static void tokens_next(TokenStream* restrict s) {
    assert(s->current < arrlen(s->tokens));
    s->current += 1;
}
