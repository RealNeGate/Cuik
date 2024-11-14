#pragma once
#include <common.h>
#include <arena.h>
#include "../str.h"
#include <dyn_array.h>
#include <cuik_lex.h>

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

    TOKEN_TILDE = '~',
    TOKEN_EXCLAMATION = '!',
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

    // it's like a string literal but no quotes
    TOKEN_MAGIC_EMBED_STRING,

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

    // Keywords (they start far higher up to avoid problems)
    #include "keywords.h"
} TknType;

enum {
    FIRST_GLSL_KEYWORD = TOKEN_KW_discard
};

#undef TKN2
#undef TKN3

typedef struct {
    uint32_t file_id;
    unsigned char* start;
    unsigned char* current;
} Lexer;

extern thread_local TB_Arena thread_arena;

// this is used by the preprocessor to scan tokens in
static Token lexer_read(Lexer* restrict l);
static Token lexer_read_inline(Lexer* restrict l);

ptrdiff_t parse_char(size_t len, const char* str, int* output);
uint64_t parse_int(size_t len, const char* str, Cuik_IntSuffix* out_suffix);
TknType classify_ident(const unsigned char* restrict str, size_t len, bool is_glsl);

static SourceLoc offset_source_loc(SourceLoc loc, uint32_t offset) {
    return (SourceLoc){ loc.raw + offset };
}

static SourceLoc encode_file_loc(uint32_t file_id, uint32_t file_offset) {
    // Big files take up several file IDs
    uint32_t real_file_id = file_id + (file_offset >> SourceLoc_FilePosBits);
    uint32_t real_file_offset = file_offset & ((1u << SourceLoc_FilePosBits) - 1);
    assert(real_file_id < (1u << SourceLoc_FileIDBits) && "Too many files!");

    return (SourceLoc){ (real_file_id << SourceLoc_FilePosBits) | real_file_offset };
}

static SourceLoc encode_macro_loc(uint32_t macro_id, uint32_t macro_offset) {
    assert(macro_id < (1u << SourceLoc_MacroIDBits) && "Too many macros!");
    // assert(macro_offset < (1u << SourceLoc_MacroOffsetBits) && "Macro too long!");

    if (macro_offset >= (1u << SourceLoc_MacroOffsetBits)) macro_offset = (1u << SourceLoc_MacroOffsetBits) - 1;
    return (SourceLoc){ SourceLoc_IsMacro | (macro_id << SourceLoc_MacroOffsetBits) | macro_offset };
}

static bool tokens_peek_double_token(TokenStream* restrict s, TknType tkn) {
    return s->list.tokens[s->list.current].type == tkn && s->list.tokens[s->list.current + 1].type == tkn;
}

static SourceRange get_token_range(Token* t) {
    return (SourceRange){ t->location, { t->location.raw + t->content.length } };
}

static SourceLoc get_end_location(Token* t) {
    return (SourceLoc){ t->location.raw + t->content.length };
}

static SourceLoc tokens_get_last_location(TokenStream* restrict s) {
    Token* t = &s->list.tokens[s->list.current - 1];
    return (SourceLoc){ t->location.raw + t->content.length };
}

static SourceLoc tokens_get_location(TokenStream* restrict s) {
    return s->list.tokens[s->list.current].location;
}

static SourceRange tokens_get_last_range(TokenStream* restrict s) {
    Token* t = &s->list.tokens[s->list.current - 1];
    return (SourceRange){ t->location, { t->location.raw + t->content.length } };
}

static SourceRange tokens_get_range(TokenStream* restrict s) {
    Token* t = &s->list.tokens[s->list.current];
    return (SourceRange){ t->location, { t->location.raw + t->content.length } };
}

static bool tokens_hit_line(TokenStream* restrict s) {
    return s->list.tokens[s->list.current].hit_line;
}

static bool tokens_eof(TokenStream* restrict s) {
    return s->list.current >= dyn_array_length(s->list.tokens) - 1;
    // return s->list.tokens[s->list.current].type == 0;
}

static bool tokens_is(TokenStream* restrict s, TknType type) {
    return s->list.tokens[s->list.current].type == type;
}

static bool tokens_match(TokenStream* restrict s, size_t len, const char* str) {
    return string_equals(&s->list.tokens[s->list.current].content, &(String){ len, (const unsigned char*) str });
}

// this is used by the parser to get the next token
static Token* tokens_get(TokenStream* restrict s) {
    return &s->list.tokens[s->list.current];
}

// there should be a NULL token so as long as we can read [current]
// we can read one ahead.
static Token* tokens_peek(TokenStream* restrict s) {
    return &s->list.tokens[s->list.current + 1];
}

static void tokens_prev(TokenStream* restrict s) {
    assert(s->list.current > 0);
    s->list.current -= 1;
}

static void tokens_next(TokenStream* restrict s) {
    assert(s->list.current < dyn_array_length(s->list.tokens));
    s->list.current += 1;
}
