// TODO list
//   - make sure all extensions are errors on pedantic mode
//
//   - enable some sort of error recovery when we encounter bad syntax within a
//   statement, basically just find the semicolon and head out :p
//
//   - ugly ass code
//
//   - make the expect(...) code be a little smarter, if it knows that it's expecting
//   a token for a specific operation... tell the user
#include "parser.h"
#include "../pp_map.h"
#include <targets/targets.h>

// winnt.h loves including garbage
#undef VOID

// HACK: this is defined in sema.c but because we sometimes call semantics stuff in the
// parser we need access to it to avoid some problems
extern thread_local Stmt* cuik__sema_function_stmt;

static const Cuik_Warnings DEFAULT_WARNINGS = { 0 };

// how big are the phase2 parse tasks
#define PARSE_MUNCH_SIZE (131072)

typedef struct {
    Atom key;
    Cuik_Type* value;
} TagEntry;

typedef struct {
    enum {
        PENDING_ALIGNAS,
        PENDING_BITWIDTH,
    } mode;
    int* dst;

    Cuik_Type* type;
    size_t start, end;
} PendingExpr;

thread_local static struct LexicalScope {
    // current values
    int local_start, local_count;
    int tag_count;

    // this is the first tag in the current scope
    int tag_scope_start;
} scope;

// starting point is used to disable the function literals
// from reading from their parent function
thread_local static Symbol* local_symbols;
thread_local static TagEntry* local_tags;

// Global symbol stuff
thread_local static DynArray(PendingExpr) pending_exprs;
thread_local static NL_Strmap(Stmt*) labels;

thread_local static Stmt* current_switch_or_case;
thread_local static Stmt* current_breakable;
thread_local static Stmt* current_continuable;

// we build a chain in that lets us know what symbols are used by a function
thread_local static Expr* symbol_chain_start;
thread_local static Expr* symbol_chain_current;

// we allocate nodes from here but once the threaded parsing stuff is complete we'll stitch this
// to the original AST arena so that it may be freed later
thread_local static Arena local_ast_arena;
thread_local static bool out_of_order_mode;

static void expect(TranslationUnit* tu, TokenStream* restrict s, char ch);
static bool expect_char(TokenStream* restrict s, char ch);
static bool expect_closing_paren(TokenStream* restrict s, SourceLoc opening);
static bool expect_with_reason(TokenStream* restrict s, char ch, const char* reason);
static Symbol* find_local_symbol(TokenStream* restrict s);

static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s);
static bool parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static Cuik_QualType parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* restrict tu, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static Expr* parse_initializer(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type);

static bool is_typename(Cuik_GlobalSymbols* syms, TokenStream* restrict s);

static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg);

static int type_cycles_dfs(TokenStream* restrict s, Cuik_Type* type);

// Usage:
//  LOCAL_SCOPE {
//      /* do parse work */
//  }
#define LOCAL_SCOPE \
for (struct LexicalScope saved = scope, *_i_ = (scope.tag_scope_start = saved.tag_count, &saved); _i_; _i_ = NULL, scope = saved)

static int align_up(int a, int b) {
    if (b == 0) return 0;

    return a + (b - (a % b)) % b;
}

static Stmt* make_stmt(TranslationUnit* tu, TokenStream* restrict s) {
    Stmt* stmt = arena_alloc(&local_ast_arena, sizeof(Stmt), _Alignof(Stmt));
    memset(stmt, 0, sizeof(Stmt));
    return stmt;
}

static Expr* make_expr(TranslationUnit* tu) {
    return ARENA_ALLOC(&local_ast_arena, Expr);
}

static Symbol* find_global_symbol(Cuik_GlobalSymbols* restrict syms, const char* name) {
    ptrdiff_t search = nl_strmap_get_cstr(syms->symbols, name);
    return (search >= 0) ? &syms->symbols[search] : NULL;
}

// ( SOMETHING )
// ( SOMETHING ,
static size_t skip_expression_in_parens(TokenStream* restrict s, TknType* out_terminator) {
    size_t saved = s->list.current;

    // by default we expect to exit with closing parens
    *out_terminator = ')';

    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            *out_terminator = '\0';
            break;
        } else if (t->type == '(') {
            depth++;
        } else if (t->type == ')') {
            depth--;
        } else if (t->type == ',' && depth == 1) {
            *out_terminator = ',';
            depth--;
        }

        tokens_next(s);
    }

    return saved;
}

// SOMETHING ,
// SOMETHING }
//
// this code sucks btw, idk why i just think it's lowkey ugly
static size_t skip_expression_in_enum(TokenStream* restrict s, TknType* out_terminator) {
    size_t saved = s->list.current;

    // our basic bitch expectations
    *out_terminator = ',';

    int depth = 1;
    while (depth) {
        Token* t = tokens_get(s);

        if (t->type == '\0') {
            *out_terminator = '\0';
            break;
        } else if (t->type == '{') {
            depth++;
        } else if (t->type == '}' && depth == 1) {
            *out_terminator = '}';
            break;
        } else if (t->type == ',' && depth == 1) {
            break;
        }

        tokens_next(s);
    }

    return saved;
}

struct Cuik_Parser {
    Cuik_ParseVersion version;
    TokenStream tokens;

    const Cuik_Target* target;
    // this refers to the `int` type, it comes from the target
    // but it's more convenient to access it from here.
    Cuik_Type* default_int;

    //  generated from #pragma comment(lib, "somelib.lib")
    // it's a linked list
    Cuik_ImportRequest* import_libs;
    DynArray(int) static_assertions;

    // this is all the globals including the static locals
    DynArray(Stmt*) local_static_storage_decls;

    // out-of-order parsing is only done while in global scope
    bool is_in_global_scope;

    DynArray(Stmt*) top_level_stmts;
    Cuik_TypeTable types;
    Cuik_GlobalSymbols globals;

    NL_Strmap(Diag_UnresolvedSymbol*) unresolved_symbols;

    // Once top-level parsing is complete we'll compute the TU (which stores
    // similar data to the parser but without the parser-specific details like
    // a static assertions list or unresolved symbols)
    TranslationUnit* tu;
};

typedef enum ParseResult {
    PARSE_WIT_ERRORS = -1,
    NO_PARSE         =  0,
    PARSE_SUCCESS    =  1,
} ParseResult;

static void diag_unresolved_symbol(Cuik_Parser* parser, Atom name, SourceLoc loc) {
    Diag_UnresolvedSymbol* d = ARENA_ALLOC(&local_ast_arena, Diag_UnresolvedSymbol);
    d->next = NULL;
    d->name = name;
    d->loc = (SourceRange){ loc, { loc.raw + strlen(name) } };

    // mtx_lock(&parser->diag_mutex);
    ptrdiff_t search = nl_strmap_get_cstr(parser->unresolved_symbols, name);
    if (search < 0) {
        search = nl_strmap_puti_cstr(parser->unresolved_symbols, name);
        parser->unresolved_symbols[search] = d;
    } else {
        Diag_UnresolvedSymbol* old = parser->unresolved_symbols[search];
        while (old->next != NULL) old = old->next;

        old->next = d;
    }
    // mtx_unlock(&parser->diag_mutex);
}

static Cuik_Type* find_tag(Cuik_Parser* restrict parser, const char* name, bool* is_in_scope) {
    Cuik_GlobalSymbols* restrict syms = &parser->globals;

    // try locals
    size_t i = scope.tag_count;
    while (i--) {
        if (strcmp((const char*) local_tags[i].key, name) == 0) {
            *is_in_scope = i >= scope.tag_scope_start;
            return local_tags[i].value;
        }
    }

    // try globals
    ptrdiff_t search = nl_strmap_get_cstr(syms->tags, name);

    *is_in_scope = search >= 0 && parser->is_in_global_scope;
    return (search >= 0) ? syms->tags[search] : NULL;
}

#define THROW_IF_ERROR() if ((r = cuikdg_error_count(s)) > 0) return (Cuik_ParseResult){ r };
#include "expr_parser.h"
#include "decl_parser.h"
#include "glsl_parser.h"
#include "stmt_parser.h"
#include "top_level_parser.h"
#include "ast_optimizer.h"

// 0 no cycles
// 1 cycles
// 2 cycles and we gave an error msg
static int type_cycles_dfs(TokenStream* restrict s, Cuik_Type* type) {
    // non-record types are always finished :P
    if (type->kind != KIND_STRUCT && type->kind != KIND_UNION) {
        return 0;
    }

    if (type->is_complete) {
        return 0;
    }

    // if (visited[o]) return true
    if (type->is_visited) {
        return 1;
    }

    type->is_visited = true;

    // for each m in members
    //   if (dfs(m)) return true
    for (size_t i = 0; i < type->record.kid_count; i++) {
        int c = type_cycles_dfs(s, cuik_canonical_type(type->record.kids[i].type));
        if (c) {
            // we already gave an error message, don't be redundant
            if (c != 2) {
                const char* name = type->record.name ? (const char*)type->record.name : "<unnamed>";

                diag_err(s, type->loc, "type %s has cycles", name);
                diag_note(s, type->record.kids[i].loc, "see here");
            }

            return 2;
        }
    }

    type->is_complete = true;
    return 0;
}

void type_layout(TranslationUnit* restrict tu, Cuik_Type* type, bool needs_complete) {
    if (type->kind == KIND_VOID || type->size != 0) return;
    if (type->is_progress) {
        diag_err(&tu->tokens, type->loc, "Type has a circular dependency");
        return;
    }

    type->is_progress = true;

    if (type->kind == KIND_ARRAY) {
        if (type->array_count_lexer_pos) {
            assert(0 && "Parserless type checker!!!");
        }

        // layout crap
        if (type->array_count != 0) {
            if (cuik_canonical_type(type->array_of)->size == 0) {
                type_layout(tu, cuik_canonical_type(type->array_of), true);
            }

            if (cuik_canonical_type(type->array_of)->size == 0) {
                diag_err(&tu->tokens, type->loc, "could not resolve type (ICE)");
                return;
            }
        }

        uint64_t result = cuik_canonical_type(type->array_of)->size * type->array_count;

        // size checks
        if (result >= INT32_MAX) {
            diag_err(&tu->tokens, type->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
        }

        type->size = result;
        type->align = cuik_canonical_type(type->array_of)->align;
    } else if (type->kind == KIND_ENUM) {
        assert(0 && "Parserless type checker!!!");
    } else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        bool is_union = (type->kind == KIND_UNION);

        size_t member_count = type->record.kid_count;
        Member* members = type->record.kids;

        // for unions this just represents the max size
        int offset = 0;
        int last_member_size = 0;
        int current_bit_offset = 0;
        // struct/union are aligned to the biggest member alignment
        int align = 0;

        for (size_t i = 0; i < member_count; i++) {
            Member* member = &members[i];

            if (cuik_canonical_type(member->type)->kind == KIND_FUNC) {
                diag_err(&tu->tokens, type->loc, "cannot put function types into a struct, try a function pointer");
            } else {
                type_layout(tu, cuik_canonical_type(member->type), true);
            }

            Cuik_Type* member_type = cuik_canonical_type(member->type);
            int member_align = member_type->align;
            int member_size = member_type->size;
            if (!is_union) {
                int new_offset = align_up(offset, member_align);

                // If we realign, reset the bit offset
                if (offset != new_offset) {
                    current_bit_offset = last_member_size = 0;
                }
                offset = new_offset;
            }

            member->offset = is_union ? 0 : offset;
            member->align = member_align;

            // bitfields
            if (member->is_bitfield) {
                int bit_width = member->bit_width;
                int bits_in_region = member_type->kind == KIND_BOOL ? 1 : (member_size * 8);
                if (bit_width > bits_in_region) {
                    diag_err(&tu->tokens, type->loc, "bitfield cannot fit in this type.");
                }

                if (current_bit_offset + bit_width > bits_in_region) {
                    current_bit_offset = 0;

                    offset = align_up(offset + member_size, member_align);
                    member->bit_offset = offset;
                }

                current_bit_offset += bit_width;
            } else {
                if (is_union) {
                    if (member_size > offset) offset = member_size;
                } else {
                    offset += member_size;
                }
            }

            // the total alignment of a struct/union is based on the biggest member
            last_member_size = member_size;
            if (member_align > align) align = member_align;
        }

        offset = align_up(offset, align);
        type->align = align;
        type->size = offset;
        type->is_complete = true;
    }

    type->is_progress = false;
}

void type_layout2(Cuik_Parser* parser, Cuik_Type* type, bool needs_complete) {
    if (type->kind == KIND_VOID || type->size != 0) return;
    if (type->is_progress) {
        diag_err(&parser->tokens, type->loc, "Type has a circular dependency");
        return;
    }

    type->is_progress = true;

    if (type->kind == KIND_ARRAY) {
        if (type->array_count_lexer_pos) {
            // run mini parser for array count
            TokenStream mini_lex = parser->tokens;
            mini_lex.list.current = type->array_count_lexer_pos;
            type->array_count = parse_const_expr2(parser, &mini_lex);
            expect_char(&mini_lex, ']');
        }

        // layout crap
        if (type->array_count != 0) {
            if (cuik_canonical_type(type->array_of)->size == 0) {
                type_layout2(parser, cuik_canonical_type(type->array_of), true);
            }

            if (cuik_canonical_type(type->array_of)->size == 0) {
                diag_err(&parser->tokens, type->loc, "could not resolve type (ICE)");
                return;
            }
        }

        uint64_t result = cuik_canonical_type(type->array_of)->size * type->array_count;

        // size checks
        if (result >= INT32_MAX) {
            diag_err(&parser->tokens, type->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
        }

        type->size = result;
        type->align = cuik_canonical_type(type->array_of)->align;
    } else if (type->kind == KIND_ENUM) {
        int cursor = 0;
        // if (type->enumerator.name && strcmp(type->enumerator.name, "D3D_DRIVER_TYPE") == 0) __debugbreak();

        for (int i = 0; i < type->enumerator.count; i++) {
            // if the value is undecided, best time to figure it out is now
            if (type->enumerator.entries[i].lexer_pos != 0) {
                // Spin up a mini expression parser here
                TokenStream mini_lex = parser->tokens;
                mini_lex.list.current = type->enumerator.entries[i].lexer_pos;

                cursor = parse_const_expr2(parser, &mini_lex);
                type->enumerator.entries[i].lexer_pos = 0;
            }

            type->enumerator.entries[i].value = cursor;
            cursor += 1;
        }

        type->size = 4;
        type->align = 4;
    } else if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
        bool is_union = (type->kind == KIND_UNION);

        size_t member_count = type->record.kid_count;
        Member* members = type->record.kids;

        // for unions this just represents the max size
        int offset = 0;
        int last_member_size = 0;
        int current_bit_offset = 0;
        // struct/union are aligned to the biggest member alignment
        int align = 0;

        for (size_t i = 0; i < member_count; i++) {
            Member* member = &members[i];

            if (cuik_canonical_type(member->type)->kind == KIND_FUNC) {
                diag_err(&parser->tokens, type->loc, "cannot put function types into a struct, try a function pointer");
            } else {
                type_layout2(parser, cuik_canonical_type(member->type), true);
            }

            Cuik_Type* member_type = cuik_canonical_type(member->type);
            int member_align = member_type->align;
            int member_size = member_type->size;
            if (!is_union) {
                int new_offset = align_up(offset, member_align);

                // If we realign, reset the bit offset
                if (offset != new_offset) {
                    current_bit_offset = last_member_size = 0;
                }
                offset = new_offset;
            }

            member->offset = is_union ? 0 : offset;
            member->align = member_align;

            // bitfields
            if (member->is_bitfield) {
                int bit_width = member->bit_width;
                int bits_in_region = member_type->kind == KIND_BOOL ? 1 : (member_size * 8);
                if (bit_width > bits_in_region) {
                    diag_err(&parser->tokens, type->loc, "bitfield cannot fit in this type.");
                }

                if (current_bit_offset + bit_width > bits_in_region) {
                    current_bit_offset = 0;

                    offset = align_up(offset + member_size, member_align);
                    member->bit_offset = offset;
                }

                current_bit_offset += bit_width;
            } else {
                if (is_union) {
                    if (member_size > offset) offset = member_size;
                } else {
                    offset += member_size;
                }
            }

            // the total alignment of a struct/union is based on the biggest member
            last_member_size = member_size;
            if (member_align > align) align = member_align;
        }

        offset = align_up(offset, align);
        type->align = align;
        type->size = offset;
    }

    type->is_complete = true;
    type->is_progress = false;
}

void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud) {
    void* old = tu->user_data;
    tu->user_data = ud;
    return old;
}

void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu) {
    return tu->user_data;
}

void cuik_destroy_translation_unit(TranslationUnit* restrict tu) {
    dyn_array_destroy(tu->top_level_stmts);

    arena_free(&tu->ast_arena);
    free_type_table(&tu->types);
    mtx_destroy(&tu->arena_mutex);
    cuik_free(tu);
}

Cuik_ImportRequest* cuik_translation_unit_import_requests(TranslationUnit* restrict tu) {
    return tu->import_libs;
}

TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu) {
    return tu->next;
}

Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu) {
    return tu->top_level_stmts;
}

size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu) {
    return dyn_array_length(tu->top_level_stmts);
}

static Symbol* find_local_symbol(TokenStream* restrict s) {
    Token* t = tokens_get(s);

    // Try local variables
    size_t i = scope.local_count;
    size_t start = scope.local_start;
    while (i-- > start) {
        const char* sym = local_symbols[i].name;
        size_t sym_length = strlen(sym);

        if (memeq(t->content.data, t->content.length, sym, sym_length)) {
            return &local_symbols[i];
        }
    }

    return NULL;
}

////////////////////////////////
// ERRORS
////////////////////////////////
static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg) {
    SourceLoc loc = tokens_get_location(s);

    report(REPORT_ERROR, s, loc, msg);
    abort();
}

static void expect(TranslationUnit* tu, TokenStream* restrict s, char ch) {
    if (tokens_get(s)->type != ch) {
        Token* t = tokens_get(s);
        SourceLoc loc = tokens_get_location(s);

        diag_err(s, (SourceRange){ loc, loc }, "expected '%c', got '%.*s'", ch, (int)t->content.length, t->content.data);
    } else {
        tokens_next(s);
    }
}

static bool expect_closing_paren(TokenStream* restrict s, SourceLoc opening) {
    if (tokens_get(s)->type != ')') {
        SourceRange loc = tokens_get_range(s);

        DiagFixit fixit = { loc, 0, ")" };
        diag_err(s, fixit.loc, "#expected closing paren", fixit);
        return false;
    } else {
        tokens_next(s);
        return true;
    }
}

static bool expect_with_reason(TokenStream* restrict s, char ch, const char* reason) {
    if (tokens_get(s)->type != ch) {
        SourceLoc loc = tokens_get_last_location(s);

        char fix[2] = { ch, '\0' };
        DiagFixit fixit = { { loc, loc }, 0, fix };
        diag_err(s, fixit.loc, "#expected '%c' for %s", fixit, ch, reason);
        return false;
    } else {
        tokens_next(s);
        return true;
    }
}
