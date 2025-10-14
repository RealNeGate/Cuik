#include <cuik.h>
#include "../cuik_pp/lexer.h"
#include "../cuik_pp/atoms.h"

#include <common.h>
#include <log.h>
#include <arena.h>
#include <new_hash_map.h>
#include <threads.h>
#include <str.h>
#include <dyn_array.h>
#include <cuik_lex.h>
#include <cuik_symtab.h>

#include <tb.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/mman.h>
#endif

#ifndef NDEBUG
#define TB_ASSERT_MSG(cond, ...) ((cond) ? 0 : (printf("\n" __FILE__ ":" STR(__LINE__) ": assertion failed: " #cond "\n  "), printf(__VA_ARGS__), __builtin_trap(), 0))
#define TB_ASSERT(cond)          ((cond) ? 0 : (printf("\n" __FILE__ ":" STR(__LINE__) ": assertion failed: " #cond "\n  "), __builtin_trap(), 0))
#else
#define TB_ASSERT_MSG(cond, ...) ((cond) ? 0 : (__builtin_unreachable(), 0))
#define TB_ASSERT(cond)          ((cond) ? 0 : (__builtin_unreachable(), 0))
#endif

#define __debugbreak() __builtin_debugtrap()

enum {
    LEXER_END_STMT = 1
};

static bool alphanum(int ch) {
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch == '_') || (ch >= '0' && ch <= '9');
}

#define X(name) static thread_local Atom atom_ ## name;
#include "go_atoms.h"

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
                return (Token){ .type = ';', .atom = atom_semicolon };
            }

            t.hit_line = true;
        }
        current++;
    }

    // parse token
    unsigned char* start = current;
    if (__builtin_expect(*start == '\0', 0)) {
        return (Token){ 0 };
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

        case '}':
        case ']':
        case ')':
        l->flags = LEXER_END_STMT;
        break;

        case '=':
        case '!':
        case '>':
        case '<':
        case ':':
        current += (*current == '=');
        break;

        default:
        break;
    }

    if (t.type == 0) {
        // add chars together (max of 3)
        int length = current - start;
        TB_ASSERT(length <= 3);

        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);

        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));

        t.type = chars & mask;
    }

    l->current = current;

    // encode token
    t.has_space = leading_space;
    t.atom      = atoms_put(current - start, start);
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

static bool cuikgo_try_eat_str(Lexer* l, Atom str) {
    Lexer old = *l;
    if (cuikgo_lex(l).atom == str) {
        return true;
    }
    *l = old;
    return false;
}

static void cuikgo_eat_str(Lexer* l, Atom str) {
    Token t = cuikgo_lex(l);
    if (t.atom != str) { abort(); }
}

static void cuikgo_eat(Lexer* l, int type) {
    Token t = cuikgo_lex(l);
    if (t.type != type) { abort(); }
}

typedef struct CuikGo_Type CuikGo_Type;
typedef struct {
    Atom name;
    CuikGo_Type* type;
    uint32_t offset;
} CuikGo_Field;

struct CuikGo_Type {
    enum {
        CUIKGO_UNRESOLVED,

        CUIKGO_VOID,

        // scalars
        CUIKGO_INT,
        CUIKGO_FLOAT64,

        // complex types
        CUIKGO_PTR,
        CUIKGO_FUNC,
        CUIKGO_MAP,
        CUIKGO_STRUCT,
    } tag;

    enum {
        CUIKGO_TYPE_STATUS_UNVISITED,
        CUIKGO_TYPE_STATUS_IN_PROGRESS,
        CUIKGO_TYPE_STATUS_COMPLETE,
    } status;

    uint32_t size, align;
    Atom name;

    // Slices are maps with a NULL key_type:
    //   Slices have a layout of ptr, len, cap.
    CuikGo_Type* key_type;
    CuikGo_Type* val_type;

    int param_count;

    // fields in a struct, params in a function
    DynArray(CuikGo_Field) fields;
};

static CuikGo_Type INT_TYPE     = { CUIKGO_INT, CUIKGO_TYPE_STATUS_COMPLETE, 8, 8 };
static CuikGo_Type FLOAT64_TYPE = { CUIKGO_FLOAT64, CUIKGO_TYPE_STATUS_COMPLETE, 8, 8 };

typedef struct CuikGo_Word CuikGo_Word;
struct CuikGo_Word {
    enum {
        ////////////////////////////////
        // Expressions
        ////////////////////////////////
        WORD_NIL,
        WORD_INT,
        // immediately define and push to stack
        WORD_DECL_REF,
        // reference a defined DECL
        WORD_REF,
        // reference an undefined DECL
        WORD_FWD_REF,
        // aggregate ops
        WORD_INDEX,
        WORD_FIELD,
        WORD_CALL,
        // alloc
        WORD_NEW,
        // binary ops
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
        // these words don't push values to the stack
        WORD_STMTS_PAST_THIS,

        WORD_CONSUME,
        WORD_RETURN,

        WORD_FUNC,
        WORD_PARAM,
        WORD_DECL,
        WORD_TYPE_DECL,
        WORD_FOR,
        WORD_FOR_RANGE,
        // if false, we skip to the word "ref"
        WORD_IF,
        // marks where the false case starts, and where it ends
        WORD_ELSE,

        WORD_END_OF_FILE,
        // holds a pointer to the next chunk
        WORD_END_OF_PAGE,
    } tag;
    int arity;

    CuikGo_Type* type;

    // reference to another word
    CuikGo_Word* ref;

    union {
        // WORD_DECL, WORD_PARAM, WORD_FUNC
        Cuik_Atom name;

        // WORD_NEW
        CuikGo_Type* src_type;

        // WORD_INT
        uint64_t iconst;

        // WORD_END_OF_PAGE
        CuikGo_Word* next_page;
    };

    // IR backing
    union {
        TB_Function* f;
        TB_Node* n;
    } ir;
};

typedef struct {
    CuikGo_Word* def_word;
    CuikGo_Type* type;
} CuikGo_Symbol;

struct CuikGo_WordPage {
    CuikGo_Word words[1024];
};

static CuikGo_Word* top_word(CuikGo_Parser* ctx) {
    return &ctx->curr->words[ctx->used - 1];
}

static CuikGo_Word* next_word(CuikGo_Word* w) {
    w += 1;
    return w->tag == WORD_END_OF_PAGE ? w->next_page : w;
}

static CuikGo_Word* submit_word(CuikGo_Parser* ctx, int tag) {
    if (ctx->curr == NULL || ctx->used == 1023) {
        CuikGo_WordPage* prev = ctx->curr;
        ctx->curr = cuik_malloc(sizeof(CuikGo_WordPage));
        ctx->used = 0;

        if (prev) {
            prev->words[1023] = (CuikGo_Word){ WORD_END_OF_PAGE, .next_page = ctx->curr->words };
        } else {
            // No previous, that means we're the first block
            ctx->base = ctx->curr;
        }
    }

    CuikGo_Word* w = &ctx->curr->words[ctx->used++];
    *w = (CuikGo_Word){ tag };
    return w;
}

static Cuik_SymbolTable* syms;

static void dump_words(CuikGo_Parser* ctx) {
    for (CuikGo_Word* w = ctx->base->words; w->tag != WORD_END_OF_FILE; w = next_word(w)) {
        switch (w->tag) {
            case WORD_INT:
            printf("INT(%"PRIu64") ", w->iconst);
            break;

            case WORD_PARAM:
            printf("%p:PARAM(%s) ", w, w->name);
            break;

            case WORD_DECL_REF:
            printf("%p:DECL-REF(%s) ", w, w->name);
            break;

            case WORD_DECL:
            printf("%p:DECL(%s) ", w, w->name);
            break;

            case WORD_TYPE_DECL:
            printf("TYPE-DECL(%p) ", w->type);
            break;

            case WORD_FIELD:
            printf("FIELD(%s) ", w->name);
            break;

            case WORD_FOR:
            printf("FOR(%p) ", w->ref);
            break;

            case WORD_FOR_RANGE:
            printf("FOR-RANGE(%p) ", w->ref);
            break;

            case WORD_REF:
            printf("REF(%p) ", w->ref);
            break;

            case WORD_FWD_REF:
            printf("FWD-REF(%p) ", w->ref);
            break;

            case WORD_IF:
            printf("IF(%p) ", w->ref);
            break;

            case WORD_ELSE:
            printf("ELSE(%p) ", w->ref);
            break;

            case WORD_NEW:
            printf("NEW(%p) ", w->src_type);
            break;

            case WORD_FUNC:
            printf("\nFUNC(%p) ", w->ref);
            break;

            case WORD_CALL: printf("CALL "); break;
            case WORD_NIL: printf("NIL "); break;
            case WORD_PLUS: printf("PLUS "); break;
            case WORD_MINUS: printf("MINUS "); break;
            case WORD_INDEX: printf("INDEX "); break;
            case WORD_CMPEQ: printf("CMPEQ "); break;
            case WORD_CMPNE: printf("CMPNE "); break;
            case WORD_CMPLT: printf("CMPLT "); break;
            case WORD_CMPGT: printf("CMPGT "); break;
            case WORD_CMPGE: printf("CMPGE "); break;
            case WORD_CMPLE: printf("CMPLE "); break;
            case WORD_ASSIGN: printf("ASSIGN "); break;
            case WORD_RETURN: printf("RETURN "); break;
            case WORD_CONSUME: printf("CONSUME "); break;

            default:
            __debugbreak();
        }
    }
    printf("\n");
}

static CuikGo_Symbol* def_name(CuikGo_Parser* ctx, int tag, Cuik_Atom name, CuikGo_Type* type) {
    CuikGo_Word* w = submit_word(ctx, tag);
    w->name = name;
    w->type = type;

    CuikGo_Symbol* sym = cuik_symtab_lookup(syms, name);
    if (sym != NULL) {
        // already referenced? backpatching time
        CuikGo_Word* curr = sym->def_word;
        do {
            CuikGo_Word* next = curr->ref;
            curr->tag = WORD_REF;
            curr->ref = w;
            curr = next;
        } while (curr);
    } else {
        sym = cuik_symtab_put(syms, name, sizeof(CuikGo_Symbol));
    }

    sym->def_word = w;
    w->ref = NULL;
    return sym;
}

static CuikGo_Symbol* ref_name(CuikGo_Parser* ctx, Cuik_Atom name) {
    CuikGo_Word* w = submit_word(ctx, WORD_REF);
    CuikGo_Symbol* sym = cuik_symtab_lookup(syms, name);
    if (sym == NULL) {
        sym = cuik_symtab_put_global(syms, name, sizeof(CuikGo_Symbol));
        sym->def_word = w;

        w->tag = WORD_FWD_REF;
        w->ref = NULL;
    } else if (sym->def_word->tag == WORD_FWD_REF) {
        // are we referring to a back ref just append to it
        TB_ASSERT(sym->type == NULL);
        w->tag = WORD_FWD_REF;
        w->ref = sym->def_word;
        sym->def_word = w;
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
        if (t.type == '*') {
            CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
            new_type->tag = CUIKGO_PTR;
            new_type->size  = 8;
            new_type->align = 8;

            *dst = new_type;
            dst  = &new_type->val_type;
            goto retry;
        } else if (t.type == '[') {
            // TODO(NeGate): support arrays and maps correctly
            cuikgo_eat(l, ']');

            CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
            new_type->tag = CUIKGO_MAP;
            new_type->size = 24;
            new_type->align = 8;

            *dst = new_type;
            dst  = &new_type->val_type;
            goto retry;
        }
    }

    if (t.atom == atom_float64) {
        *dst = &FLOAT64_TYPE;
    } else if (t.atom == atom_int) {
        *dst = &INT_TYPE;
    } else if (t.atom == atom_struct) {
        CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
        new_type->tag = CUIKGO_STRUCT;
        new_type->fields = dyn_array_create(CuikGo_Field, 8);

        cuikgo_eat(l, '{');
        while (cuikgo_peek(l).type != '}') {
            int top = dyn_array_length(new_type->fields);

            // Name list
            do {
                t = cuikgo_lex(l);
                TB_ASSERT(t.type == TOKEN_IDENTIFIER);

                dyn_array_put(new_type->fields, (CuikGo_Field){ t.atom });
            } while (cuikgo_try_eat(l, ','));

            // Broadcast the same type to the fields
            CuikGo_Type* kid_type = cuikgo_type(ctx, l);
            for (int i = top; i < dyn_array_length(new_type->fields); i++) {
                new_type->fields[i].type = kid_type;
            }
            cuikgo_eat(l, ';');
        }
        cuikgo_eat(l, '}');
        *dst = new_type;
    } else if (t.type == TOKEN_IDENTIFIER) {
        CuikGo_Symbol* sym = cuik_symtab_lookup(syms, t.atom);
        if (sym == NULL) {
            sym = cuik_symtab_put_global(syms, t.atom, sizeof(CuikGo_Symbol));

            *dst = calloc(1, sizeof(CuikGo_Type));
            (*dst)->tag = CUIKGO_UNRESOLVED;
            sym->type = *dst;
        } else {
            TB_ASSERT(sym->type);
            *dst = sym->type;
        }
    } else {
        __debugbreak();
    }
    return type;
}

CuikGo_Word* cuikgo_name_list(CuikGo_Parser* ctx, Lexer* l, int tag, int* out_count) {
    int count = 0;
    CuikGo_Word* w = NULL;

    // Name list
    Token t;
    do {
        t = cuikgo_lex(l);
        TB_ASSERT(t.type == TOKEN_IDENTIFIER);
        CuikGo_Word* def = def_name(ctx, tag, t.atom, NULL)->def_word;
        if (count == 0) {
            w = def;
        }
        count += 1;
    } while (cuikgo_try_eat(l, ','));

    *out_count = count;
    return w;
}

void cuikgo_expr(CuikGo_Parser* ctx, Lexer* l);
void cuikgo_atom(CuikGo_Parser* ctx, Lexer* l) {
    Token t = cuikgo_lex(l);
    if (t.atom == atom_nil) {
        submit_word(ctx, WORD_NIL);
    } else if (t.atom == atom_new) {
        cuikgo_eat(l, '(');
        CuikGo_Word* w = submit_word(ctx, WORD_NEW);
        w->src_type = cuikgo_type(ctx, l);
        cuikgo_eat(l, ')');
    } else if (t.type == TOKEN_INTEGER) {
        Cuik_IntSuffix suffix;
        uint64_t i = 0;
        const char* str = t.atom;
        while (*str) {
            i = (i * 10) + (*str - '0');
            str += 1;
        }

        CuikGo_Word* w = submit_word(ctx, WORD_INT);
        w->iconst = i;
    } else if (t.type == TOKEN_IDENTIFIER) {
        ref_name(ctx, t.atom);
    } else {
        __debugbreak();
    }

    retry: {
        Lexer saved = *l;
        Token t = cuikgo_lex(l);
        if (t.type == '.') {
            t = cuikgo_lex(l);
            TB_ASSERT(t.type == TOKEN_IDENTIFIER);

            CuikGo_Word* w = submit_word(ctx, WORD_FIELD);
            w->arity = 1;
            w->name  = t.atom;
            goto retry;
        } else if (t.type == '[') {
            cuikgo_expr(ctx, l);
            cuikgo_eat(l, ']');

            CuikGo_Word* w = submit_word(ctx, WORD_INDEX);
            w->arity = 2;
            goto retry;
        } else if (t.type == '(') {
            cuikgo_eat(l, ')');

            CuikGo_Word* w = submit_word(ctx, WORD_CALL);
            w->arity = 1;
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

        CuikGo_Word* w = submit_word(ctx, binop.op);
        w->arity = 2;
    }
}

void cuikgo_expr(CuikGo_Parser* ctx, Lexer* l) {
    cuikgo_binop(ctx, l, 0);

    if (cuikgo_try_eat(l, '=')) {
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(ctx, WORD_ASSIGN);
        w->arity = 2;
    }
}

void cuikgo_stmt(CuikGo_Parser* ctx, Lexer* l, bool simple);
void cuikgo_block(CuikGo_Parser* ctx, Lexer* l) {
    cuik_scope_open(syms);
    cuikgo_eat(l, '{');
    while (cuikgo_peek(l).type != '}') {
        cuikgo_stmt(ctx, l, false);
        cuikgo_eat(l, ';');
    }
    cuikgo_eat(l, '}');
    cuik_scope_close(syms);
}

void cuikgo_stmt(CuikGo_Parser* ctx, Lexer* l, bool simple) {
    Lexer saved = *l;
    Token t = cuikgo_lex(l);
    if (t.atom == atom_if) {
        // TODO(NeGate): a simple statement can fit here
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(ctx, WORD_IF);
        w->arity = 1;

        // then case
        cuikgo_block(ctx, l);

        // else case
        CuikGo_Word* else_w = submit_word(ctx, WORD_ELSE);
        w->ref = else_w;

        if (cuikgo_try_eat_str(l, "else")) {
            // TODO(NeGate): should be a block or if
            cuikgo_block(ctx, l);
        }

        CuikGo_Word* end_w = submit_word(ctx, WORD_CONSUME);
        else_w->ref = end_w;
    } else if (t.atom == atom_return) {
        cuikgo_expr(ctx, l);

        CuikGo_Word* w = submit_word(ctx, WORD_RETURN);
        w->arity = 1;
    } else if (t.atom == atom_for) {
        cuik_scope_open(syms);

        saved = *l;
        t = cuikgo_lex(l);

        Token ahead = cuikgo_lex(l);
        if (t.type == TOKEN_IDENTIFIER && (ahead.type == ',' || ahead.type == 0x3D3A)) {
            // identifier list? we're either an init stmt or range clause
            int count = 1;
            Atom names[16];
            names[0] = t.atom;

            if (ahead.type == ',') {
                do {
                    t = cuikgo_lex(l);
                    TB_ASSERT(t.type == TOKEN_IDENTIFIER);
                    TB_ASSERT(count != 16);
                    names[count++] = t.atom;
                } while (cuikgo_try_eat(l, ','));
                cuikgo_eat(l, 0x3D3A);
            }

            if (cuikgo_try_eat_str(l, atom_range)) {
                FOR_N(i, 0, count) {
                    CuikGo_Word* w = submit_word(ctx, WORD_DECL_REF);
                    w->name = names[i];

                    CuikGo_Symbol* sym = cuik_symtab_put(syms, names[i], sizeof(CuikGo_Symbol));
                    sym->def_word = w;
                }

                cuikgo_expr(ctx, l);

                CuikGo_Word* w = submit_word(ctx, WORD_FOR_RANGE);
                w->arity = 1 + count;
                cuikgo_block(ctx, l);

                CuikGo_Word* end_w = submit_word(ctx, WORD_CONSUME);
                w->ref = end_w;
                cuik_scope_close(syms);
                return;
            }

            // assign values to expressions
            FOR_N(i, 0, count) {
                cuikgo_expr(ctx, l);

                CuikGo_Word* w = submit_word(ctx, WORD_DECL);
                w->name  = names[i];
                w->type  = NULL;
                w->arity = 1;

                CuikGo_Symbol* sym = cuik_symtab_put(syms, names[i], sizeof(CuikGo_Symbol));
                sym->def_word = w;
            }

            cuikgo_eat(l, ';');
        } else if (cuikgo_try_eat(l, '{')) {
            // No Init, no post, just an infinite loop
            CuikGo_Word* w = submit_word(ctx, WORD_INT);
            w->iconst = 1;
            w = submit_word(ctx, WORD_FOR);

            cuikgo_block(ctx, l);

            CuikGo_Word* end_w = submit_word(ctx, WORD_CONSUME);
            w->ref = end_w;
            cuik_scope_close(syms);
            return;
        } else if (cuikgo_try_eat(l, ';')) {
            // empty InitStmt
        } else {
            __debugbreak();

            *l = saved;
            cuikgo_stmt(ctx, l, true);
        }

        // ForClause ::= InitStmt? ";" Condition? ";" PostStmt?
        if (cuikgo_try_eat(l, ';')) {
            // condition is just true
            CuikGo_Word* w = submit_word(ctx, WORD_INT);
            w->iconst = 1;
        }

        cuikgo_expr(ctx, l);
        CuikGo_Word* w = submit_word(ctx, WORD_FOR);
        w->arity = 1;

        if (cuikgo_try_eat(l, ';')) {
            cuikgo_stmt(ctx, l, true);
        }

        cuikgo_block(ctx, l);

        CuikGo_Word* end_w = submit_word(ctx, WORD_CONSUME);
        w->ref = end_w;
        cuik_scope_close(syms);
    } else {
        Token ahead = cuikgo_lex(l);
        if (t.type == TOKEN_IDENTIFIER && (ahead.type == ',' || ahead.type == 0x3D3A)) {
            // identifier list? this is a variable declaration
            int count = 1;
            Atom names[16];
            names[0] = t.atom;

            if (ahead.type == ',') {
                do {
                    t = cuikgo_lex(l);
                    TB_ASSERT(t.type == TOKEN_IDENTIFIER);
                    TB_ASSERT(count != 16);
                    names[count++] = t.atom;
                } while (cuikgo_try_eat(l, ','));
                cuikgo_eat(l, 0x3D3A);
            }

            // assign values to expressions
            FOR_N(i, 0, count) {
                cuikgo_expr(ctx, l);

                CuikGo_Word* w = submit_word(ctx, WORD_DECL);
                w->name  = names[i];
                w->type  = NULL;
                w->arity = 1;

                CuikGo_Symbol* sym = cuik_symtab_put(syms, names[i], sizeof(CuikGo_Symbol));
                sym->def_word = w;
            }
        } else {
            *l = saved;
            cuikgo_expr(ctx, l);

            CuikGo_Word* w = submit_word(ctx, WORD_CONSUME);
            w->arity = 1;
        }
    }
}

void cuikgo_var_decl(CuikGo_Parser* ctx, Lexer* l, bool is_param) {
    // Name list
    int count;
    CuikGo_Word* word = cuikgo_name_list(ctx, l, is_param ? WORD_PARAM : WORD_DECL, &count);

    CuikGo_Type* type = cuikgo_type(ctx, l);
    FOR_N(i, 0, count) {
        word->type = type;
        word = next_word(word);
    }
}

void cuikgo_visit_type(CuikGo_Parser* ctx, CuikGo_Type* type) {
    if (type->status == CUIKGO_TYPE_STATUS_COMPLETE) {
        return;
    } else if (type->status == CUIKGO_TYPE_STATUS_IN_PROGRESS) {
        // CYCLE!!!
        TB_ASSERT(0);
        return;
    }

    type->status = CUIKGO_TYPE_STATUS_IN_PROGRESS;
    if (type->tag == CUIKGO_STRUCT) {
        uint32_t size = 0, align = 0;
        dyn_array_for(i, type->fields) {
            cuikgo_visit_type(ctx, type->fields[i].type);

            uint32_t member_size  = type->fields[i].type->size;
            uint32_t member_align = type->fields[i].type->align;

            // align up, then increment
            size = (size + member_align - 1) & -member_align;

            type->fields[i].offset = size;
            size += member_size;
            if (align > member_align) {
                align = member_align;
            }
        }

        type->size = size, type->align = align;
    } else if (type->tag == CUIKGO_MAP || type->tag == CUIKGO_PTR) {
        type->status = CUIKGO_TYPE_STATUS_COMPLETE;

        // visit the pointers but we don't consider it an illegal cycle
        // for them to refer back to us.
        if (type->key_type) {
            cuikgo_visit_type(ctx, type->key_type);
        }
        cuikgo_visit_type(ctx, type->val_type);
    } else {
        dyn_array_for(i, type->fields) {
            cuikgo_visit_type(ctx, type->fields[i].type);
        }
    }
    type->status = CUIKGO_TYPE_STATUS_COMPLETE;
}

void cuikgo_sema(CuikGo_Parser* ctx, CuikGo_Word* start, CuikGo_Word* end);
CuikGo_Word* cuikgo_sema_word(CuikGo_Parser* ctx, CuikGo_Word* w, int arity, CuikGo_Word** args) {
    switch (w->tag) {
        case WORD_CONSUME: break;
        case WORD_ELSE: break;

        // type checked by whatever uses it
        case WORD_DECL_REF: break;

        case WORD_IF: {
            // TODO(NeGate): check that the value is boolean-able
            break;
        }

        case WORD_INT: {
            w->type = &INT_TYPE;
            break;
        }

        case WORD_REF: {
            CuikGo_Word* src = w->ref;

            cuikgo_visit_type(ctx, src->type);
            w->type = src->type;
            break;
        }

        case WORD_NEW: {
            TB_ASSERT(w->src_type->status == CUIKGO_TYPE_STATUS_COMPLETE);

            CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
            new_type->tag = CUIKGO_PTR;
            new_type->size  = 8;
            new_type->align = 8;
            new_type->status = CUIKGO_TYPE_STATUS_COMPLETE;
            new_type->val_type = w->src_type;
            w->type = new_type;
            break;
        }

        case WORD_DECL: {
            if (arity == 1) {
                CuikGo_Type* src_type = args[0]->type;
                TB_ASSERT(w->type == NULL || src_type == w->type);

                cuikgo_visit_type(ctx, src_type);
                w->type = src_type;
            }
            break;
        }

        case WORD_FOR_RANGE: {
            CuikGo_Type* container = args[2]->type;
            if (w->arity == 2) {
                TB_ASSERT(container->tag == CUIKGO_INT);
                args[0]->type = &INT_TYPE;
            } else if (w->arity == 3) {
                TB_ASSERT(container->tag == CUIKGO_MAP);
                args[0]->type = container->key_type ? container->key_type : &INT_TYPE;
                args[1]->type = container->val_type;
            }
            break;
        }

        case WORD_INDEX: {
            CuikGo_Type* container = args[0]->type;
            TB_ASSERT(container->tag == CUIKGO_MAP);

            CuikGo_Type* key_type = container->key_type ? container->key_type : &INT_TYPE;
            TB_ASSERT(args[1]->type == key_type);

            w->type = container->val_type;
            break;
        }

        case WORD_FIELD: {
            CuikGo_Type* container = args[0]->type;
            if (container->tag == CUIKGO_PTR) {
                container = container->val_type;
            }

            TB_ASSERT(container->tag == CUIKGO_STRUCT);
            dyn_array_for(i, container->fields) {
                if (container->fields[i].name == w->name) {
                    w->type = container->fields[i].type;
                    break;
                }
            }
            break;
        }

        case WORD_ASSIGN: {
            CuikGo_Type* lhs = args[0]->type;
            CuikGo_Type* rhs = args[1]->type;
            TB_ASSERT(lhs == rhs);
            w->type = lhs;
            break;
        }

        case WORD_CMPGT: {
            CuikGo_Type* lhs = args[0]->type;
            CuikGo_Type* rhs = args[1]->type;
            TB_ASSERT(lhs == rhs);
            w->type = &INT_TYPE;
            break;
        }

        case WORD_CALL: {
            CuikGo_Type* target = args[0]->type;
            TB_ASSERT(target->tag == CUIKGO_FUNC);

            __debugbreak();
            break;
        }

        default: __debugbreak();
    }
    return next_word(w);
}

void cuikgo_sema(CuikGo_Parser* ctx, CuikGo_Word* start, CuikGo_Word* end) {
    CuikGo_Word* stack[64];
    size_t top = 0;

    CuikGo_Word* w = start;
    while (w != end) {
        // once we know this we can organize the top slice of the stack as the inputs
        TB_ASSERT(top >= w->arity);
        top -= w->arity;
        CuikGo_Word** args = &stack[top];
        CuikGo_Word* next = cuikgo_sema_word(ctx, w, w->arity, args);

        // push to stack
        if (w->tag < WORD_STMTS_PAST_THIS) {
            TB_ASSERT(w->type == NULL || w->type->status == CUIKGO_TYPE_STATUS_COMPLETE);
            stack[top++] = w;
        }
        w = next;
    }
}

TB_JIT* jit;

#include "sched.h"
#include "gc.c"
#include "sched.c"
#include "go_irgen.c"

void cuikgo_parse_file(CuikGo_Parser* ctx, Cuik_Path* filepath) {
    Cuik_FileResult main_file;
    CUIK_TIMED_BLOCK("load main file") {
        if (!ctx->fs(ctx->user_data, filepath, &main_file, ctx->case_insensitive)) {
            fprintf(stderr, "\x1b[31merror\x1b[0m: file \"%s\" doesn't exist.\n", filepath->data);
            return;
        }
    }

    cuikperf_region_start("parse", NULL);
    Lexer l = {
        .start = (unsigned char*) main_file.data,
        .current = (unsigned char*) main_file.data
    };

    if (atom_semicolon == NULL) {
        #define X(name) atom_ ## name = atoms_putc(#name);
        #include "go_atoms.h"
    }

    ////////////////////////////////
    // Parsing
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

        if (t.atom == atom_var) {
            t = cuikgo_lex(&l);
            TB_ASSERT(t.type == TOKEN_IDENTIFIER);

            CuikGo_Type* type = cuikgo_type(ctx, &l);
            CuikGo_Word* w = def_name(ctx, WORD_DECL, t.atom, type)->def_word;
            w->ref  = w;
        } else if (t.atom == atom_type) {
            t = cuikgo_lex(&l);
            TB_ASSERT(t.type == TOKEN_IDENTIFIER);

            CuikGo_Type* type = cuikgo_type(ctx, &l);
            CuikGo_Symbol* sym = cuik_symtab_lookup(syms, t.atom);
            if (sym != NULL) {
                // already referenced? backpatching time
                TB_ASSERT(sym->type != NULL);
                *sym->type = *type;
            } else {
                sym = cuik_symtab_put(syms, t.atom, sizeof(CuikGo_Symbol));
                sym->type = type;
            }

            if (type->name == NULL) {
                type->name = t.atom;
            }

            CuikGo_Word* w = submit_word(ctx, WORD_TYPE_DECL);
            w->name = t.atom;
            w->type = type;
            w->ref  = w;
        } else if (t.atom == atom_func) {
            Cuik_Atom name = cuikgo_lex(&l).atom;
            CuikGo_Word* w = def_name(ctx, WORD_FUNC, name, NULL)->def_word;

            CuikGo_Type* new_type = calloc(1, sizeof(CuikGo_Type));
            new_type->tag = CUIKGO_FUNC;
            new_type->size = 8, new_type->align = 8;
            new_type->fields = dyn_array_create(CuikGo_Field, 4);

            // Params
            cuik_scope_open(syms);
            cuikgo_eat(&l, '(');
            if (!cuikgo_try_eat(&l, ')')) {
                do {
                    // Name list
                    int count;
                    CuikGo_Word* word = cuikgo_name_list(ctx, &l, WORD_PARAM, &count);

                    CuikGo_Type* type = cuikgo_type(ctx, &l);
                    FOR_N(i, 0, count) {
                        dyn_array_put(new_type->fields, (CuikGo_Field){ word->name, type });

                        word->type = type;
                        word = next_word(word);
                    }
                } while (cuikgo_try_eat(&l, ','));
                cuikgo_eat(&l, ')');
            }

            new_type->name = w->name;
            new_type->param_count = dyn_array_length(new_type->fields);
            w->type = new_type;

            // Body
            cuikgo_block(ctx, &l);
            cuik_scope_close(syms);
            w->ref = top_word(ctx);
            continue;
        }
    }
    CuikGo_Word* end_word = submit_word(ctx, WORD_END_OF_FILE);
    cuikperf_region_end();

    dump_words(ctx);

    ////////////////////////////////
    // Type checking
    ////////////////////////////////
    cuikperf_region_start("sema", NULL);
    for (CuikGo_Word* w = &ctx->base->words[0]; w != end_word;) {
        CuikGo_Word* next = next_word(w->ref);
        if (w->tag == WORD_TYPE_DECL || w->tag == WORD_DECL) {
            cuikgo_visit_type(ctx, w->type);
        } else if (w->tag == WORD_FUNC) {
            cuikgo_visit_type(ctx, w->type);

            // skip params
            do {
                w = next_word(w);
            } while (w->tag == WORD_PARAM);

            cuikgo_sema(ctx, w, next);
        } else {
            __debugbreak();
        }
        w = next;
    }
    cuikperf_region_end();

    ////////////////////////////////
    // IR generation
    ////////////////////////////////
    cuikperf_region_start("irgen", NULL);
    TB_Module* ir_mod = tb_module_create_for_host(true);
    jit = tb_jit_begin(ir_mod, 2*1024*1024);

    checkpoint_proto = tb_prototype_create(ir_mod, TB_TRAPCALL, 0, NULL, 0, NULL, false);
    checkpoint_fn    = tb_extern_create(ir_mod, -1, "checkpoint_handler", TB_EXTERNAL_SO_LOCAL);
    lvb_trap_fn      = tb_extern_create(ir_mod, -1, "lvb_handler",        TB_EXTERNAL_SO_LOCAL);

    gc_alloc_proto   = tb_prototype_create(ir_mod, TB_STDCALL, 2, (TB_PrototypeParam[2]){ { TB_TYPE_I64 }, { TB_TYPE_I32 } }, 1, &(TB_PrototypeParam){ TB_TYPE_GCREF }, false);
    gc_alloc_fn      = tb_extern_create(ir_mod, -1, "gc_alloc", TB_EXTERNAL_SO_LOCAL);

    // declaring a global big enough to hold our string
    TB_Global* expected_nmt_lut = tb_global_create(ir_mod, -1, "expected_nmt", NULL, TB_LINKAGE_PRIVATE);
    tb_global_set_storage(ir_mod, -1, expected_nmt_lut, sizeof(uint64_t), 1, 1);
    tb_module_use_cc_gc(ir_mod, (TB_Symbol*) expected_nmt_lut, lvb_trap_fn);

    expected_nmt = tb_jit_place_global(jit, expected_nmt_lut);
    log_debug("Placing expected_nmt @ %p", expected_nmt);

    void lvb_handler(void);
    void checkpoint_handler(void);
    tb_symbol_bind_ptr((TB_Symbol*) checkpoint_fn, checkpoint_handler);
    tb_symbol_bind_ptr((TB_Symbol*) lvb_trap_fn,   lvb_handler);
    tb_symbol_bind_ptr((TB_Symbol*) gc_alloc_fn,   gc_alloc);

    // IR alloc
    for (CuikGo_Word* w = &ctx->base->words[0]; w != end_word;) {
        if (w->tag == WORD_FUNC) {
            w->ir.f = tb_function_create(ir_mod, -1, w->name, TB_LINKAGE_PUBLIC);

            TB_FeatureSet features = tb_features_from_profile_str(ir_mod, "x86_64-v3");
            features.gen |= TB_FEATURE_STACK_MAPS;
            tb_function_set_features(w->ir.f, &features);
            w = next_word(w->ref);
        } else if (w->tag == WORD_DECL) {
            __debugbreak();
            w = next_word(w->ref);
        } else if (w->tag == WORD_TYPE_DECL) {
            w = next_word(w);
        } else {
            __debugbreak();
        }
    }

    // IR gen
    TB_Worklist* ws = tb_worklist_alloc();
    for (CuikGo_Word* w = &ctx->base->words[0]; w != end_word;) {
        TB_ASSERT(w->tag == WORD_TYPE_DECL || w->tag == WORD_FUNC || w->tag == WORD_DECL);

        CuikGo_Word* next = next_word(w->ref);
        if (w->tag == WORD_FUNC) {
            TB_FunctionPrototype* proto = tb_prototype_create(ir_mod, TB_STDCALL, 2, (TB_PrototypeParam[2]){ { TB_TYPE_GCREF }, { TB_TYPE_GCREF } }, 0, NULL, false);
            bool is_main = strcmp(w->name, "MaxArray") == 0;

            TB_Function* func = w->ir.f;
            TB_GraphBuilder* g = tb_builder_enter(func, tb_module_get_text(ir_mod), proto, NULL);
            // TB_GraphBuilder* g = tb_builder_enter_from_dbg(func, tb_module_get_text(ir_mod), dbg_type, NULL);

            // fill params
            int j = 1;
            w = next_word(w);
            while (w->tag == WORD_PARAM) {
                w->ir.n = tb_builder_get_var(g, j++);
                w = next_word(w);
            }

            cuikgo_ir_gen(ctx, g, w, next);
            if (tb_builder_label_get(g) != NULL) {
                tb_builder_ret(g, 0, 0, NULL);
            }

            tb_builder_exit(g);
            tb_opt(func, ws, false);

            TB_FunctionOutput* out = tb_codegen(func, TB_RA_ROGERS, ws, NULL, true);
            void* fnptr = tb_jit_place_function(jit, func);
            if (is_main) {
                go_stuff = fnptr;
            }
        } else if (w->tag != WORD_TYPE_DECL) {
            __debugbreak();
        }
        w = next;
    }
    cuikperf_region_end();

    tb_jit_dump_heap(jit);
    sched_main();

    // tb_jit_end(jit);
}

