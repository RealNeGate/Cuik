#include <cuik.h>
#include "../cuik_pp/lexer.h"

#include <common.h>
#include <arena.h>
#include <str.h>
#include <dyn_array.h>
#include <cuik_lex.h>
#include <cuik_symtab.h>

enum {
    LEXER_END_STMT = 1
};

static bool alphanum(int ch) {
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch == '_') || (ch >= '0' && ch <= '9');
}

static bool token_isa(Token t, const char* str) {
    size_t l = strlen(str);
    return l == t.content.length && memcmp(t.content.data, str, l) == 0;
}

static Token cuikgo_lex(Lexer* l) {
    unsigned char* current = l->current;

    // common case handled branchlessly
    bool leading_space = *current == ' ';
    current += leading_space;

    // skip WS
    Token t = { 0 };
    while (*current == ' ' || *current == '\n' || *current == '\t' || *current == '\t') {
        if (*current == '\n') {
            // insert semicolon
            if (l->flags & LEXER_END_STMT) {
                l->flags = 0;
                return (Token){ .type = ';', .hit_line = true, .content = { 1, (const unsigned char*) ";" } };
            }

            t.hit_line = true;
        }
        current++;
    }

    // parse token
    unsigned char* start = current;
    if (__builtin_expect(*start == '\0', 0)) {
        return (Token){ .content = { 0, current } };
    }

    l->flags = 0;
    switch (*current++) {
        case '_':
        case 'a' ... 'z':
        case 'A' ... 'Z':
        // identifier
        t.type = TOKEN_IDENTIFIER;
        while (alphanum(*current)) { current++; }
        l->flags = LEXER_END_STMT;
        break;

        case '0' ... '9':
        t.type = TOKEN_INTEGER;
        while (*current >= '0' && *current <= '9') { current++; }
        l->flags = LEXER_END_STMT;
        break;

        case ':':
        current += (*current == '=');
        break;

        default:
        break;
    }

    if (t.type == 0) {
        // add chars together (max of 3)
        int length = current - start;
        assert(length <= 3);

        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);

        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));

        t.type = chars & mask;
    }

    l->current = current;

    // encode token
    t.has_space = leading_space;
    t.content = (String){ current - start, start };
    // t.location = encode_file_loc(l->file_id, start - l->start);
    return t;
}

static bool cuikgo_try_eat(Lexer* l, int type) {
    Lexer old = *l;
    if (cuikgo_lex(l).type == type) {
        return true;
    }
    *l = old;
    return false;
}

static void cuikgo_eat_str(Lexer* l, const char* str) {
    Token t = cuikgo_lex(l);
    if (!token_isa(t, str)) { abort(); }
}

static void cuikgo_eat(Lexer* l, int type) {
    Token t = cuikgo_lex(l);
    if (t.type != type) { abort(); }
}

typedef struct CuikGo_Type CuikGo_Type;
struct CuikGo_Type {
    enum {
        CUIK_GO_VOID,

        // scalars
        CUIK_GO_FLOAT64,

        // aggregates
        CUIK_GO_MAP,
        CUIK_GO_STRUCT,
    } tag;

    uint32_t size, align;

    // Arrays are maps with a NULL key_type
    CuikGo_Type* key_type;
    CuikGo_Type* val_type;
};

static CuikGo_Type FLOAT64_TYPE = { CUIK_GO_FLOAT64, 8, 8 };

typedef struct {
    enum {
        WORD_DECL,
        WORD_FOR_RANGE,
    } tag;

    String name;
    CuikGo_Type* type;

    int arity;
} CuikGo_Word;

static int word_cnt;
static CuikGo_Word words[1024];
static Cuik_SymbolTable* syms;

static CuikGo_Word* submit_word(int tag) {
    assert(word_cnt + 1 < 1024);
    CuikGo_Word* w = &words[word_cnt++];
    *w = (CuikGo_Word){ tag };
    return w;
}

CuikGo_Type* cuikgo_type(CuikGo_Parser* ctx, Lexer* l) {
    CuikGo_Type* type = NULL;
    CuikGo_Type** dst = &type;

    Token t;
    retry: {
        t = cuikgo_lex(l);
        if (t.type == '[') {
            cuikgo_eat(l, ']');

            CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
            new_type->tag = CUIK_GO_MAP;
            new_type->size = 24;
            new_type->align = 8;

            *dst = new_type;
            dst  = &new_type->val_type;
            goto retry;
        }
    }

    if (token_isa(t, "float64")) {
        *dst = &FLOAT64_TYPE;
    } else {
        __debugbreak();
    }
    return type;
}

void cuikgo_name_list(CuikGo_Parser* ctx, Lexer* l) {
    // Name list
    Token t;
    do {
        t = cuikgo_lex(l);
        assert(t.type == TOKEN_IDENTIFIER);

        CuikGo_Word* w = submit_word(WORD_DECL);
        w->name = t.content;

        printf("VAR '%.*s'\n", (int)t.content.length, t.content.data);
    } while (cuikgo_try_eat(l, ','));
}

void cuikgo_expr(CuikGo_Parser* ctx, Lexer* l) {
    __debugbreak();
}

void cuikgo_stmt(CuikGo_Parser* ctx, Lexer* l) {
    Token t = cuikgo_lex(l);
    if (token_isa(t, "for")) {
        int top = word_cnt;
        cuikgo_name_list(ctx, l);
        int bot = word_cnt;

        t = cuikgo_lex(l);
        if (t.type == 0x3D3A) {
            // := range EXPR
            cuikgo_eat_str(l, "range");
            cuikgo_expr(ctx, l);

            CuikGo_Word* w = submit_word(WORD_FOR_RANGE);
            w->arity = (bot - top) + 1;
        } else {
            __debugbreak();
        }
    } else {
        __debugbreak();
    }
}

void cuikgo_var_decl(CuikGo_Parser* ctx, Lexer* l) {
    // Name list
    int top = word_cnt;
    cuikgo_name_list(ctx, l);

    CuikGo_Type* type = cuikgo_type(ctx, l);
    for (int i = top; i < word_cnt; i++) {
        words[i].type = type;
    }
}

void cuikgo_parse_file(CuikGo_Parser* ctx, Cuik_Path* filepath) {
    Cuik_FileResult main_file;
    CUIK_TIMED_BLOCK("load main file") {
        if (!ctx->fs(ctx->user_data, filepath, &main_file, ctx->case_insensitive)) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: file \"%s\" doesn't exist.\n", filepath->data);
            return;
        }
    }

    Lexer l = {
        .start = (unsigned char*) main_file.data,
        .current = (unsigned char*) main_file.data
    };

    ////////////////////////////////
    // Package
    ////////////////////////////////
    cuikgo_eat_str(&l, "package");
    Token package = cuikgo_lex(&l);
    cuikgo_eat(&l, ';');
    // Imports

    // Top-level decls
    syms = cuik_symtab_create(NULL);
    for (;;) {
        Token t = cuikgo_lex(&l);
        if (t.type == 0) { break; }

        if (token_isa(t, "func")) {
            String name = cuikgo_lex(&l).content;

            // Params
            cuikgo_eat(&l, '(');
            if (!cuikgo_try_eat(&l, ')')) {
                do {
                    cuikgo_var_decl(ctx, &l);
                } while (cuikgo_try_eat(&l, ','));
                cuikgo_eat(&l, ')');
            }

            // Body
            cuikgo_eat(&l, '{');
            if (!cuikgo_try_eat(&l, '}')) {
                do {
                    cuikgo_stmt(ctx, &l);
                } while (cuikgo_try_eat(&l, ';'));
                cuikgo_eat(&l, '}');
            }
        }

        if (t.hit_line) {
            printf("\n");
        }
        printf("'%.*s' ", (int)t.content.length, t.content.data);
    }

    __debugbreak();
}