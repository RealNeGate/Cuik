#include <cuik.h>
#include "../cuik_pp/lexer.h"
#include "../cuik_c/atoms.h"

#include <common.h>
#include <arena.h>
#include <str.h>
#include <dyn_array.h>
#include <cuik_lex.h>
#include <cuik_symtab.h>

#define __debugbreak() __builtin_debugtrap()

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

static Token cuikgo_peek(Lexer* l) {
    Lexer copy = *l;
    return cuikgo_lex(&copy);
}

static bool cuikgo_try_eat(Lexer* l, int type) {
    Lexer old = *l;
    if (cuikgo_lex(l).type == type) {
        return true;
    }
    *l = old;
    return false;
}

static bool cuikgo_try_eat_str(Lexer* l, const char* str) {
    Lexer old = *l;
    if (token_isa(cuikgo_lex(l), str)) {
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
        ////////////////////////////////
        // Expressions
        ////////////////////////////////
        // reference a defined DECL
        WORD_REF,
        // reference an undefined DECL
        WORD_FWD_REF,
        // map/array ops
        WORD_INDEX,
        // binary ops
        WORD_CONSUME,
        WORD_ASSIGN,
        WORD_PLUS,
        WORD_MINUS,
        WORD_TIMES,
        WORD_SLASH,
        WORD_PERCENT,
        WORD_AND,
        WORD_OR,
        WORD_XOR,
        WORD_SHL,
        WORD_SHR,
        WORD_CMPEQ,
        WORD_CMPNE,
        WORD_CMPGE,
        WORD_CMPGT,
        WORD_CMPLE,
        WORD_CMPLT,
        WORD_LOGICAL_AND,
        WORD_LOGICAL_OR,
        ////////////////////////////////
        // Declarations & Statements
        ////////////////////////////////
        WORD_DECL,
        WORD_FOR_RANGE,
        // if false, we skip to the word "ref"
        WORD_IF,
    } tag;

    Cuik_Atom name;
    CuikGo_Type* type;

    int arity;

    // reference to another word
    int ref;
} CuikGo_Word;

typedef struct {
    int def_word;
} CuikGo_Symbol;

static int word_cnt;
static CuikGo_Word words[1024];
static Cuik_SymbolTable* syms;

static void dump_words(void) {
    for (int i = 0; i < word_cnt; i++) {
        switch (words[i].tag) {
            case WORD_DECL:
            printf("%d:DECL(%s) ", i, words[i].name);
            break;

            case WORD_FOR_RANGE:
            printf("FOR-RANGE ");
            break;

            case WORD_REF:
            printf("REF(%d) ", words[i].ref);
            break;

            case WORD_FWD_REF:
            printf("FWD-REF(%d) ", words[i].ref);
            break;

            case WORD_IF:
            printf("IF(%d) ", words[i].ref);
            break;

            case WORD_INDEX: printf("INDEX "); break;
            case WORD_CMPGT: printf("CMPGT "); break;
            case WORD_ASSIGN: printf("ASSIGN "); break;
            case WORD_CONSUME: printf("CONSUME "); break;

            default:
            __debugbreak();
        }
    }
    printf("\n");
}

static CuikGo_Word* submit_word(int tag) {
    assert(word_cnt + 1 < 1024);
    CuikGo_Word* w = &words[word_cnt++];
    *w = (CuikGo_Word){ tag };
    return w;
}

static CuikGo_Symbol* def_name(String str, CuikGo_Type* type) {
    printf("VAR '%.*s'\n", (int)str.length, str.data);

    Cuik_Atom name = atoms_put(str.length, str.data);
    CuikGo_Word* w = submit_word(WORD_DECL);
    w->name = name;
    w->type = type;

    CuikGo_Symbol* sym = cuik_symtab_lookup(syms, name);
    if (sym != NULL) {
        // already referenced? backpatching time
        int i = sym->def_word;
        do {
            int next = words[i].ref;
            words[i].tag = WORD_REF;
            words[i].ref = word_cnt - 1;
            i = next;
        } while (i >= 0);
    } else {
        sym = cuik_symtab_put(syms, name, sizeof(CuikGo_Symbol));
    }

    sym->def_word = word_cnt-1;
    return sym;
}

static CuikGo_Symbol* ref_name(String str) {
    Cuik_Atom name = atoms_put(str.length, str.data);
    CuikGo_Word* w = submit_word(WORD_REF);
    CuikGo_Symbol* sym = cuik_symtab_lookup(syms, name);
    if (sym == NULL) {
        sym = cuik_symtab_put_global(syms, name, sizeof(CuikGo_Symbol));
        sym->def_word = word_cnt - 1;

        w->tag = WORD_FWD_REF;
        w->ref = -1;
    } else if (words[sym->def_word].tag == WORD_FWD_REF) {
        // are we referring to a back ref just append to it
        w->tag = WORD_FWD_REF;
        w->ref = sym->def_word;
        sym->def_word = word_cnt - 1;
    } else {
        w->ref = sym->def_word;
    }
    return sym;
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
        def_name(t.content, NULL);
    } while (cuikgo_try_eat(l, ','));
}

void cuikgo_expr(CuikGo_Parser* ctx, Lexer* l);
void cuikgo_atom(CuikGo_Parser* ctx, Lexer* l) {
    Token t = cuikgo_lex(l);
    if (t.type == TOKEN_IDENTIFIER) {
        ref_name(t.content);
    } else {
        __debugbreak();
    }

    retry: {
        Lexer saved = *l;
        Token t = cuikgo_lex(l);
        if (t.type == '[') {
            cuikgo_expr(ctx, l);
            cuikgo_eat(l, ']');

            CuikGo_Word* w = submit_word(WORD_INDEX);
            w->arity = 2;
            goto retry;
        }

        *l = saved;
    }
}

typedef struct {
    char prec;
    char op;
} ExprInfo;

static ExprInfo get_binop(TknType ty) {
    #define ON(k, prec, op) case TOKEN_ ## k: return (ExprInfo){ prec, WORD_ ## op };
    switch (ty) {
        ON(TIMES,         11,  TIMES);
        ON(SLASH,         11,  SLASH);
        ON(PERCENT,       11,  PERCENT);
        ON(PLUS,          10,  PLUS);
        ON(MINUS,         10,  MINUS);
        ON(LEFT_SHIFT,    9,   SHL);
        ON(RIGHT_SHIFT,   9,   SHR);
        ON(GREATER_EQUAL, 8,   CMPGE);
        ON(LESS_EQUAL,    8,   CMPLE);
        ON(GREATER,       8,   CMPGT);
        ON(LESS,          8,   CMPLT);
        ON(EQUALITY,      7,   CMPEQ);
        ON(NOT_EQUAL,     7,   CMPNE);
        ON(AND,           6,   AND);
        ON(XOR,           5,   XOR);
        ON(OR,            4,   OR);
        ON(DOUBLE_AND,    3,   LOGICAL_AND);
        ON(DOUBLE_OR,     2,   LOGICAL_OR);
        // zero means it's not a binary operator
        default: return (ExprInfo){ 0 };
    }
    #undef ON
}

void cuikgo_binop(CuikGo_Parser* ctx, Lexer* l, int min_prec) {
    cuikgo_atom(ctx, l);

    for (;;) {
        Lexer saved = *l;
        Token t = cuikgo_lex(l);
        ExprInfo binop = get_binop(t.type);
        if (binop.prec == 0 || binop.prec < min_prec) {
            *l = saved;
            break;
        }

        cuikgo_binop(ctx, l, binop.prec + 1);

        CuikGo_Word* w = submit_word(binop.op);
        w->arity = 2;
    }
}

void cuikgo_expr(CuikGo_Parser* ctx, Lexer* l) {
    cuikgo_binop(ctx, l, 0);

    if (cuikgo_try_eat(l, '=')) {
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(WORD_ASSIGN);
        w->arity = 2;
    }
}

void cuikgo_stmt(CuikGo_Parser* ctx, Lexer* l);
void cuikgo_block(CuikGo_Parser* ctx, Lexer* l) {
    cuik_scope_open(syms);
    cuikgo_eat(l, '{');
    if (!cuikgo_try_eat(l, '}')) {
        do {
            cuikgo_stmt(ctx, l);
        } while (cuikgo_try_eat(l, ';'));
        cuikgo_eat(l, '}');
    }
    cuik_scope_close(syms);
}

void cuikgo_stmt(CuikGo_Parser* ctx, Lexer* l) {
    Lexer saved = *l;
    Token t = cuikgo_lex(l);
    if (token_isa(t, "if")) {
        // TODO(NeGate): a simple statement can fit here
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(WORD_IF);
        w->arity = 1;

        // then case
        cuikgo_block(ctx, l);

        // else case
        w->ref = word_cnt;
        if (cuikgo_try_eat_str(l, "else")) {
            // TODO(NeGate): should be a block or if
            cuikgo_block(ctx, l);
        }

        dump_words();
    } else if (token_isa(t, "for")) {
        cuik_scope_open(syms);

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

            dump_words();
            cuikgo_block(ctx, l);
        } else {
            __debugbreak();
        }
        cuik_scope_close(syms);
    } else {
        *l = saved;
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(WORD_CONSUME);
        w->arity = 1;
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
            cuikgo_block(ctx, &l);
            continue;
        }
    }

    __debugbreak();
}
