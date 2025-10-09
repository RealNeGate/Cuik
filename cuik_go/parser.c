#include <cuik.h>
#include "../cuik_pp/lexer.h"
#include "../cuik_c/atoms.h"

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
        CUIKGO_VOID,

        // scalars
        CUIKGO_INT,
        CUIKGO_FLOAT64,

        // aggregates
        CUIKGO_MAP,
        CUIKGO_STRUCT,
    } tag;

    uint32_t size, align;

    // Slices are maps with a NULL key_type:
    //   Slices have a layout of ptr, len, cap.
    CuikGo_Type* key_type;
    CuikGo_Type* val_type;
};

static CuikGo_Type INT_TYPE     = { CUIKGO_INT, 8, 8 };
static CuikGo_Type FLOAT64_TYPE = { CUIKGO_FLOAT64, 8, 8 };

typedef struct {
    enum {
        ////////////////////////////////
        // Expressions
        ////////////////////////////////
        // immediately define and push to stack
        WORD_DECL_REF,
        // reference a defined DECL
        WORD_REF,
        // reference an undefined DECL
        WORD_FWD_REF,
        // map/array ops
        WORD_INDEX,
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

        WORD_FUNC,
        WORD_PARAM,
        WORD_DECL,
        WORD_FOR_RANGE,
        // if false, we skip to the word "ref"
        WORD_IF,
        // marks where the false case starts, and where it ends
        WORD_ELSE,
    } tag;

    Cuik_Atom name;
    CuikGo_Type* type;

    int arity;

    // reference to another word
    int ref;

    // IR backing
    union {
        TB_Function* f;
        TB_Node* n;
    } ir;
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
            case WORD_PARAM:
            printf("%d:PARAM(%s) ", i, words[i].name);
            break;

            case WORD_DECL_REF:
            printf("%d:DECL_REF(%s) ", i, words[i].name);
            break;

            case WORD_DECL:
            printf("%d:DECL(%s) ", i, words[i].name);
            break;

            case WORD_FOR_RANGE:
            printf("FOR-RANGE(%d) ", words[i].ref);
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

            case WORD_ELSE:
            printf("ELSE(%d) ", words[i].ref);
            break;

            case WORD_FUNC:
            printf("\nFUNC(%d) ", words[i].ref);
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
    TB_ASSERT(word_cnt + 1 < 1024);
    CuikGo_Word* w = &words[word_cnt++];
    *w = (CuikGo_Word){ tag };
    return w;
}

static CuikGo_Symbol* def_name(int tag, String str, CuikGo_Type* type) {
    printf("VAR '%.*s'\n", (int)str.length, str.data);

    Cuik_Atom name = atoms_put(str.length, str.data);
    CuikGo_Word* w = submit_word(tag);
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
            new_type->tag = CUIKGO_MAP;
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

void cuikgo_name_list(CuikGo_Parser* ctx, Lexer* l, int tag) {
    // Name list
    Token t;
    do {
        t = cuikgo_lex(l);
        TB_ASSERT(t.type == TOKEN_IDENTIFIER);
        def_name(tag, t.content, NULL);
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
        w = submit_word(WORD_ELSE);
        w->arity = 1;

        if (cuikgo_try_eat_str(l, "else")) {
            // TODO(NeGate): should be a block or if
            cuikgo_block(ctx, l);
        }

        w->ref = word_cnt;
    } else if (token_isa(t, "for")) {
        cuik_scope_open(syms);

        int top = word_cnt;
        cuikgo_name_list(ctx, l, WORD_DECL_REF);
        int bot = word_cnt;
        TB_ASSERT(bot - top == 2);

        t = cuikgo_lex(l);
        if (t.type == 0x3D3A) {
            // HACK(NeGate): this should be part of type checking
            words[top+0].type = &INT_TYPE;
            words[top+1].type = &FLOAT64_TYPE;

            // := range EXPR
            cuikgo_eat_str(l, "range");
            cuikgo_expr(ctx, l);

            CuikGo_Word* w = submit_word(WORD_FOR_RANGE);
            w->arity = 3;
            cuikgo_block(ctx, l);
            w->ref = word_cnt;
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

void cuikgo_var_decl(CuikGo_Parser* ctx, Lexer* l, bool is_param) {
    // Name list
    int top = word_cnt;
    cuikgo_name_list(ctx, l, is_param ? WORD_PARAM : WORD_DECL);

    CuikGo_Type* type = cuikgo_type(ctx, l);
    for (int i = top; i < word_cnt; i++) {
        words[i].type = type;
    }
}

typedef struct {
    enum {
        CUIKGO_RVALUE,

        // L-values
        CUIKGO_LVALUE,
        CUIKGO_LVALUE_SYM,
    } tag;
    CuikGo_Type* type;
    TB_Node* n;
} CuikGo_Val;

TB_JIT* jit;

#include "sched.h"
#include "gc.c"
#include "sched.c"

#define TB_TYPE_GCPTR TB_TYPE_PTRN(1)
#define TB_TYPE_GCREF TB_TYPE_PTRN(2)

static TB_Symbol* checkpoint_fn;
static TB_Symbol* lvb_trap_fn;
static TB_FunctionPrototype* checkpoint_proto;

// Checkpoints update root set references along with the NMT
static void insert_checkpoint(CuikGo_Parser* ctx, TB_GraphBuilder* g) {
    TB_Node* ptr = tb_builder_jit_thread_ptr(g);
    ptr = tb_builder_ptr_member(g, ptr, tb_jit_thread_userdata());

    TB_Node* merge = tb_builder_label_make(g);
    TB_Node* n = tb_builder_load(g, 0, true, TB_TYPE_I8, ptr, 1, false);

    TB_Node* paths[2];
    TB_Node* if_n = tb_builder_if(g, n, paths);
    TB_NODE_SET_EXTRA(if_n, TB_NodeIf, .prob = 0.0001f);
    {
        tb_builder_label_set(g, paths[0]);
        tb_builder_call(g, checkpoint_proto, 0, tb_builder_symbol(g, checkpoint_fn), 0, NULL);
        tb_builder_br(g, merge);
    }
    tb_builder_label_set(g, paths[1]);
    tb_builder_br(g, merge);

    tb_builder_label_set(g, merge);
}

static TB_Node* to_rval(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Val* v) {
    if (v->tag == CUIKGO_LVALUE) {
        TB_DataType dt = TB_TYPE_VOID;
        switch (v->type->tag) {
            case CUIKGO_INT:     dt = TB_TYPE_I64; break;
            case CUIKGO_FLOAT64: dt = TB_TYPE_F64; break;
            // this is a structure, we can only directly refer to it via pointer
            case CUIKGO_MAP:     dt = TB_TYPE_PTR; break;
            default: TB_ASSERT(0);
        }
        return tb_builder_load(g, 0, true, dt, v->n, v->type->align, false);
    } else if (v->tag == CUIKGO_RVALUE) {
        return v->n;
    } else {
        TB_ASSERT(0);
    }
}

void cuikgo_ir_gen(CuikGo_Parser* ctx, TB_GraphBuilder* g, int start, int end);
int cuikgo_ir_gen_word(CuikGo_Parser* ctx, TB_GraphBuilder* g, CuikGo_Word* w, int word_pos, int arity, CuikGo_Val* args) {
    switch (w->tag) {
        case WORD_CONSUME: break;

        case WORD_DECL:
        case WORD_DECL_REF: {
            CuikGo_Type* type = w->type;
            w->ir.n = tb_builder_local(g, type->size, type->align);

            if (w->tag == WORD_DECL_REF) {
                args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = w->type, .n = w->ir.n };
            }
            break;
        }

        case WORD_REF: {
            args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = words[w->ref].type, .n = words[w->ref].ir.n };
            break;
        }

        case WORD_CMPGT: {
            TB_Node* lhs = to_rval(ctx, g, &args[0]);
            TB_Node* rhs = to_rval(ctx, g, &args[1]);

            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = &INT_TYPE, .n = tb_builder_cmp(g, TB_CMP_ULT, rhs, lhs) };
            break;
        }

        case WORD_ASSIGN: {
            TB_Node* rhs = to_rval(ctx, g, &args[1]);
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE);

            tb_builder_store(g, 0, true, args[0].n, rhs, 8, false);
            args[0] = (CuikGo_Val){ CUIKGO_RVALUE, .type = args[1].type, .n = rhs };
            break;
        }

        case WORD_INDEX: {
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE);
            TB_Node* index = to_rval(ctx, g, &args[1]);

            // array indexing
            TB_ASSERT(args[0].type->tag == CUIKGO_MAP);
            CuikGo_Type* val_type = args[0].type->val_type;
            if (args[0].type->key_type != NULL) {
                TB_ASSERT(0);
            } else {
                TB_Node* base = tb_builder_load(g, 0, false, TB_TYPE_GCPTR, args[0].n, 8, false);

                TB_Node* add = tb_builder_ptr_array(g, base, index, val_type->size);
                args[0] = (CuikGo_Val){ CUIKGO_LVALUE, .type = val_type, .n = add };
                break;
            }
        }

        case WORD_FOR_RANGE: {
            // (key, value, map/array)
            TB_ASSERT(args[0].tag == CUIKGO_LVALUE && args[1].tag == CUIKGO_LVALUE);
            TB_Node* key = args[0].n;
            TB_Node* val = args[1].n;

            TB_Node* iter = args[2].n;
            TB_ASSERT(args[1].tag == CUIKGO_LVALUE);

            TB_Node* base  = tb_builder_load(g, 0, false, TB_TYPE_GCPTR, iter, 8, false);
            TB_Node* limit = tb_builder_load(g, 0, false, TB_TYPE_I64, tb_builder_ptr_member(g, iter, 8), 8, false);
            CuikGo_Type* val_type = &FLOAT64_TYPE;
            TB_Node* exit = tb_builder_label_make(g);

            int index_var = tb_builder_decl(g, tb_builder_label_get(g));
            tb_builder_set_var(g, index_var, tb_builder_sint(g, TB_TYPE_I64, 0));

            TB_Node* header = tb_builder_loop(g);
            TB_Node* loop   = tb_builder_label_clone(g, header);
            {
                TB_Node* paths[2];
                // while (has_next()) {
                TB_Node* index  = tb_builder_get_var(g, index_var);
                TB_Node* cond   = tb_builder_cmp(g, TB_CMP_ULT, index, limit);
                tb_builder_if(g, cond, paths);


                // exit path
                tb_builder_label_set(g, paths[1]);
                tb_builder_br(g, exit);
                tb_builder_label_kill(g, paths[1]);


                // in body
                tb_builder_label_set(g, paths[0]);
                insert_checkpoint(ctx, g);

                // fetch val
                index = tb_builder_get_var(g, index_var);
                TB_Node* val_ld = tb_builder_load(g, 0, true, TB_TYPE_F64, tb_builder_ptr_array(g, base, index, val_type->size), 8, false);
                tb_builder_store(g, 0, true, key, index,  val_type->align, false);
                tb_builder_store(g, 0, true, val, val_ld, val_type->align, false);
                cuikgo_ir_gen(ctx, g, word_pos+1, w->ref);
                // advance
                index = tb_builder_get_var(g, index_var);
                tb_builder_set_var(g, index_var, tb_builder_binop_int(g, TB_ADD, index, tb_builder_uint(g, TB_TYPE_I64, 1), 0));
                tb_builder_br(g, loop);
                tb_builder_label_kill(g, paths[0]);
            }
            tb_builder_label_kill(g, loop);
            tb_builder_label_kill(g, header);

            tb_builder_label_set(g, exit);
            return w->ref;
        }
        case WORD_IF: {
            TB_Node* paths[2];
            TB_Node* cond = to_rval(ctx, g, &args[0]);

            TB_Node* merge = tb_builder_label_make(g);
            tb_builder_if(g, cond, paths);
            { // then
                tb_builder_label_set(g, paths[0]);
                cuikgo_ir_gen(ctx, g, word_pos+1, w->ref);
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[0]);
            }
            TB_ASSERT(words[w->ref].tag == WORD_ELSE);
            int merge_pos = words[w->ref].ref;
            { // else
                tb_builder_label_set(g, paths[1]);
                if (w->ref+1 != merge_pos) {
                    cuikgo_ir_gen(ctx, g, w->ref+1, merge_pos);
                }
                tb_builder_br(g, merge);
                tb_builder_label_kill(g, paths[1]);
            }

            tb_builder_label_set(g, merge);

            // for whatever reason, neither path on the if joined back so this is just a dead label.
            if (merge->inputs[1]->input_count == 0) {
                tb_builder_label_kill(g, merge);
            }
            return merge_pos;
        }

        default: TB_ASSERT(0);
    }
    return word_pos+1;
}

void cuikgo_ir_gen(CuikGo_Parser* ctx, TB_GraphBuilder* g, int start, int end) {
    CuikGo_Val stack[64];
    size_t i = start, top = 0;
    while (i < end) {
        CuikGo_Word* w = &words[i];

        // once we know this we can organize the top slice of the stack as the inputs
        top -= w->arity;
        CuikGo_Val* args = &stack[top];
        i = cuikgo_ir_gen_word(ctx, g, w, i, w->arity, args);
        // push to stack
        if (w->tag < WORD_STMTS_PAST_THIS) {
            top += 1;
        }
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

        if (token_isa(t, "func")) {
            String name = cuikgo_lex(&l).content;

            def_name(WORD_FUNC, name, NULL);
            CuikGo_Word* w = &words[word_cnt - 1];

            // Params
            cuikgo_eat(&l, '(');
            if (!cuikgo_try_eat(&l, ')')) {
                do {
                    cuikgo_var_decl(ctx, &l, true);
                } while (cuikgo_try_eat(&l, ','));
                cuikgo_eat(&l, ')');
            }

            // Body
            cuikgo_block(ctx, &l);
            w->ref = word_cnt;
            continue;
        }
    }

    dump_words();

    ////////////////////////////////
    // Type checking
    ////////////////////////////////

    ////////////////////////////////
    // IR generation
    ////////////////////////////////
    TB_Module* ir_mod = tb_module_create_for_host(true);
    jit = tb_jit_begin(ir_mod, 2*1024*1024);

    checkpoint_proto = tb_prototype_create(ir_mod, TB_TRAPCALL, 0, NULL, 0, NULL, false);
    checkpoint_fn    = tb_extern_create(ir_mod, -1, "checkpoint_handler", TB_EXTERNAL_SO_LOCAL);
    lvb_trap_fn      = tb_extern_create(ir_mod, -1, "lvb_handler",        TB_EXTERNAL_SO_LOCAL);

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

    // IR alloc
    for (int i = 0; i < word_cnt;) {
        if (words[i].tag == WORD_FUNC) {
            words[i].ir.f = tb_function_create(ir_mod, -1, words[i].name, TB_LINKAGE_PUBLIC);

            TB_FeatureSet features = tb_features_from_profile_str(ir_mod, "x86_64-v3");
            features.gen |= TB_FEATURE_STACK_MAPS;
            tb_function_set_features(words[i].ir.f, &features);
        } else if (words[i].tag == WORD_DECL) {
            __debugbreak();
        }
        i = words[i].ref;
    }

    // IR gen
    TB_Worklist* ws = tb_worklist_alloc();
    for (int i = 0; i < word_cnt;) {
        TB_ASSERT(words[i].tag == WORD_FUNC || words[i].tag == WORD_DECL);

        int next = words[i].ref;
        if (words[i].tag == WORD_FUNC) {
            TB_FunctionPrototype* proto = tb_prototype_create(ir_mod, TB_STDCALL, 2, (TB_PrototypeParam[2]){ { TB_TYPE_GCREF }, { TB_TYPE_GCREF } }, 0, NULL, false);

            TB_Function* func = words[i].ir.f;
            TB_GraphBuilder* g = tb_builder_enter(func, tb_module_get_text(ir_mod), proto, NULL);
            // TB_GraphBuilder* g = tb_builder_enter_from_dbg(func, tb_module_get_text(ir_mod), dbg_type, NULL);

            // fill params
            i++;
            for (int j = 0; i < next && words[i].tag == WORD_PARAM; j++) {
                words[i++].ir.n = tb_builder_get_var(g, j+1);
            }

            cuikgo_ir_gen(ctx, g, i, next);
            if (tb_builder_label_get(g) != NULL) {
                tb_builder_ret(g, 0, 0, NULL);
            }

            tb_builder_exit(g);
            tb_opt(func, ws, false);

            TB_FunctionOutput* out = tb_codegen(func, TB_RA_ROGERS, ws, NULL, true);
            tb_output_print_asm(out, NULL);

            go_stuff = tb_jit_place_function(jit, func);
            tb_jit_tag_object(jit, go_stuff, func);
        } else {
            __debugbreak();
        }
        i = next;
    }

    tb_jit_dump_heap(jit);

    // Space+NMT
    //    10 0 => TRAP
    //    10 1 => TRAP
    *expected_nmt = (2 << 0b010) * 0x0101010101010101;
    go_spawn(jit);

    thrd_t n_thrds[1];
    thrd_t sched_thrd;
    FOR_N(i, 0, 1) {
        thrd_create(&n_thrds[i], sched_n_main, NULL);
    }
    thrd_create(&sched_thrd, gc_main, NULL);
    thrd_join(sched_thrd, NULL);

    // tb_jit_end(jit);
}

