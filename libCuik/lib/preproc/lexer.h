#pragma once
#include "../common.h"
#include "../str.h"
#include "../arena.h"
#include <dyn_array.h>
#include <cuik.h>

#define TKN2(x, y)                  (((y) << 8) | (x))
#define TKN3(x, y, z) (((z) << 16) | ((y) << 8) | (x))

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
    TOKEN_TRIPLE_DOT = TKN3('.', '.', '.'),

    TOKEN_INVALID,

    TOKEN_ARROW         = TKN2('-', '>'),
    TOKEN_DOUBLE_HASH   = TKN2('#', '#'),

    TOKEN_DOUBLE_AND    = TKN2('&', '&'),
    TOKEN_DOUBLE_OR     = TKN2('|', '|'),

    TOKEN_PLUS_EQUAL    = TKN2('+', '='),
    TOKEN_MINUS_EQUAL   = TKN2('-', '='),
    TOKEN_TIMES_EQUAL   = TKN2('*', '='),
    TOKEN_SLASH_EQUAL   = TKN2('/', '='),
    TOKEN_PERCENT_EQUAL = TKN2('%', '='),
    TOKEN_OR_EQUAL      = TKN2('|', '='),
    TOKEN_AND_EQUAL     = TKN2('&', '='),
    TOKEN_XOR_EQUAL     = TKN2('^', '='),
    TOKEN_NOT_EQUAL     = TKN2('!', '='),
    TOKEN_EQUALITY      = TKN2('=', '='),
    TOKEN_GREATER_EQUAL = TKN2('>', '='),
    TOKEN_LESS_EQUAL    = TKN2('<', '='),
    TOKEN_LEFT_SHIFT    = TKN2('<', '<'),
    TOKEN_RIGHT_SHIFT   = TKN2('>', '>'),

    TOKEN_LEFT_SHIFT_EQUAL  = TKN3('<', '<', '='),
    TOKEN_RIGHT_SHIFT_EQUAL = TKN3('>', '>', '='),
    TOKEN_INCREMENT         = TKN2('+', '+'),
    TOKEN_DECREMENT         = TKN2('-', '-'),

    // this is hacky because ! is a single char
    // token (sometimes used in !=) but this way
    // it can share more rules with the rest of the
    // tokens (less cases in the lexer).
    TOKEN_DOUBLE_EXCLAMATION = TKN2('!', '!'),

    // Keywords (they start far higher up to avoid problems)
    TOKEN_KW_auto = 0x10000000,
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
    TOKEN_KW_asm,
    TOKEN_KW_attribute,
    TOKEN_KW_cdecl,
    TOKEN_KW_stdcall,
    TOKEN_KW_declspec,
} TknType;

#undef TKN2
#undef TKN3

typedef struct {
} SourceLineManager;

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
uint64_t parse_int(size_t len, const char* str, Cuik_IntSuffix* out_suffix);
TknType classify_ident(const unsigned char* restrict str, size_t len);

inline static String lexer_get_string(Lexer* restrict l) {
    return string_from_range(l->token_start, l->token_end);
}

inline static bool lexer_match(Lexer* restrict l, size_t len, const char* str) {
    if ((l->token_end - l->token_start) != len) return false;

    return memcmp(l->token_start, str, len) == 0;
}

inline static bool tokens_peek_double_token(TokenStream* restrict s, TknType tkn) {
    return s->tokens[s->current].type == tkn && s->tokens[s->current + 1].type == tkn;
}

inline static SourceLocIndex tokens_get_last_location_index(TokenStream* restrict s) {
    return s->tokens[s->current - 1].location;
}

inline static SourceLocIndex tokens_get_location_index(TokenStream* restrict s) {
    return s->tokens[s->current].location;
}

inline static bool tokens_hit_line(TokenStream* restrict s) {
    return s->tokens[s->current].hit_line;
}

inline static int tokens_get_location_line(TokenStream* restrict s) {
    return s->locations[s->tokens[s->current].location].line->line;
}

inline static bool tokens_eof(TokenStream* restrict s) {
    return s->tokens[s->current].type == 0;
}

inline static bool tokens_is(TokenStream* restrict s, TknType type) {
    return s->tokens[s->current].type == type;
}

inline static bool tokens_match(TokenStream* restrict s, size_t len, const char* str) {
    Token* t = &s->tokens[s->current];
    if ((t->end - t->start) != len) return false;

    return memcmp(t->start, str, len) == 0;
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
    assert(s->current < dyn_array_length(s->tokens));
    s->current += 1;
}
