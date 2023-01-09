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

// starting point is used to disable the function literals
// from reading from their parent function
thread_local static int local_symbol_start = 0;
thread_local static int local_symbol_count = 0;
thread_local static Symbol* local_symbols;

thread_local static int local_tag_count = 0;
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
static void parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s, Stmt* restrict node);
static bool parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static Cuik_QualType parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* restrict tu, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);
static Cuik_QualType parse_typename(TranslationUnit* tu, TokenStream* restrict s);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_initializer(TranslationUnit* tu, TokenStream* restrict s, Cuik_QualType type);
static bool parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* decl_node);

static bool is_typename(Cuik_GlobalSymbols* syms, TokenStream* restrict s);

static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg);

static int type_cycles_dfs(TokenStream* restrict s, Cuik_Type* type);

// saves the local_symbol_count and local_tag_count before entering
// and restores them when exiting since that'll allow us to open and
// close local scopes in the perspective of variable lookup...
//
// TODO(NeGate): add a field to keep track of "last local symbol count"
// such that we can give proper errors on shadowing
//
// Usage:
//  LOCAL_SCOPE {
//      /* do parse work */
//  }
#define LOCAL_SCOPE \
for (int saved = local_symbol_count, saved2 = local_tag_count, _i_ = 0; _i_ == 0; \
    _i_ += 1, local_symbol_count = saved, local_tag_count = saved2)

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

static Cuik_Type* find_tag(Cuik_GlobalSymbols* restrict syms, const char* name) {
    // try locals
    size_t i = local_tag_count;
    while (i--) {
        if (strcmp((const char*) local_tags[i].key, name) == 0) {
            return local_tags[i].value;
        }
    }

    // try globals
    ptrdiff_t search = nl_strmap_get_cstr(syms->tags, name);
    return (search >= 0) ? syms->tags[search] : NULL;
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

    // generated from #pragma comment(lib, "somelib.lib")
    // it's a linked list
    Cuik_ImportRequest* import_libs;
    DynArray(int) static_assertions;

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

#define THROW_IF_ERROR() if ((r = cuikdg_error_count(s)) > 0) return (Cuik_ParseResult){ r };
#include "expr_parser.h"
#include "expr_parser2.h"
#include "decl_parser.h"
#include "decl_parser2.h"
#include "glsl_parser.h"
#include "stmt_parser.h"
#include "top_level_parser.h"

typedef struct {
    // shared state, every run of phase2_parse_task will decrement this by one
    atomic_size_t* tasks_remaining;
    size_t start, end;

    TranslationUnit* tu;
    const TokenStream* base_token_stream;
} ParserTaskInfo;

// we have a bunch of thread locals and for the sake of it, we wanna reset em before
// a brand new parser on this thread
static void reset_global_parser_state() {
    labels = NULL;
    local_symbol_start = local_symbol_count = 0;
    current_switch_or_case = current_breakable = current_continuable = NULL;
    symbol_chain_start = symbol_chain_current = NULL;
}

static void parse_global_symbols(TranslationUnit* tu, size_t start, size_t end, TokenStream tokens) {
    CUIK_TIMED_BLOCK("phase 3") {
        out_of_order_mode = false;

        for (size_t i = start; i < end; i++) {
            Symbol* sym = &tu->globals.symbols[i];

            // don't worry about normal globals, those have been taken care of...
            if (sym->token_start != 0 && (sym->storage_class == STORAGE_STATIC_FUNC || sym->storage_class == STORAGE_FUNC)) {
                // Spin up a mini parser here
                tokens.list.current = sym->token_start;

                // intitialize use list
                symbol_chain_start = symbol_chain_current = NULL;

                // Some sanity checks in case a local symbol is leaked funny.
                assert(local_symbol_start == 0 && local_symbol_count == 0);
                parse_function_definition(tu, &tokens, sym->stmt);
                local_symbol_start = local_symbol_count = 0;

                // finalize use list
                sym->stmt->decl.first_symbol = symbol_chain_start;
            }
        }
    }
}

static void phase3_parse_task(void* arg) {
    ParserTaskInfo task = *((ParserTaskInfo*)arg);
    reset_global_parser_state();

    tls_init();

    // allocate the local symbol tables
    if (local_symbols == NULL) {
        local_symbols = realloc(local_symbols, sizeof(Symbol) * MAX_LOCAL_SYMBOLS);
        local_tags = realloc(local_tags, sizeof(TagEntry) * MAX_LOCAL_TAGS);
    }

    if (pending_exprs) {
        dyn_array_clear(pending_exprs);
    } else {
        pending_exprs = dyn_array_create(PendingExpr, 1024);
    }

    parse_global_symbols(task.tu, task.start, task.end, *task.base_token_stream);
    *task.tasks_remaining -= 1;

    // move local AST arena to TU's AST arena
    {
        mtx_lock(&task.tu->arena_mutex);

        arena_trim(&local_ast_arena);
        arena_append(&task.tu->ast_arena, &local_ast_arena);
        local_ast_arena = (Arena){0};

        mtx_unlock(&task.tu->arena_mutex);
    }
}

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

// returns 1 on errors
static int type_resolve_pending_align(TranslationUnit* restrict tu) {
    size_t pending_count = dyn_array_length(pending_exprs);
    for (size_t i = 0; i < pending_count; i++) {
        if (pending_exprs[i].mode == PENDING_ALIGNAS) {
            Cuik_Type* type = pending_exprs[i].type;

            TokenStream mini_lex = tu->tokens;
            mini_lex.list.current = pending_exprs[i].start;

            int align = 0;
            SourceLoc loc = tokens_get_location(&mini_lex);
            if (is_typename(&tu->globals, &mini_lex)) {
                Cuik_Type* new_align = cuik_canonical_type(parse_typename(tu, &mini_lex));
                if (new_align == NULL || new_align->align) {
                    diag_err(&tu->tokens, type->loc, "_Alignas cannot operate with incomplete");
                } else {
                    align = new_align->align;
                }
            } else {
                intmax_t new_align = parse_const_expr(tu, &mini_lex);
                if (new_align == 0) {
                    diag_err(&tu->tokens, type->loc, "_Alignas cannot be applied with 0 alignment", new_align);
                } else if (new_align >= INT16_MAX) {
                    diag_err(&tu->tokens, type->loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
                } else {
                    align = new_align;
                }
            }

            assert(align != 0);
            type->align = align;
            return 0;
        }
    }

    return 1;
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
            // run mini parser for array count
            TokenStream mini_lex = tu->tokens;
            mini_lex.list.current = type->array_count_lexer_pos;
            type->array_count = parse_const_expr(tu, &mini_lex);
            expect(tu, &mini_lex, ']');
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
        int cursor = 0;

        for (int i = 0; i < type->enumerator.count; i++) {
            // if the value is undecided, best time to figure it out is now
            if (type->enumerator.entries[i].lexer_pos != 0) {
                // Spin up a mini expression parser here
                TokenStream mini_lex = tu->tokens;
                mini_lex.list.current = type->enumerator.entries[i].lexer_pos;

                cursor = parse_const_expr(tu, &mini_lex);
                type->enumerator.entries[i].lexer_pos = 0;
            }

            type->enumerator.entries[i].value = cursor;
            cursor += 1;
        }

        type->size = 4;
        type->align = 4;
        type->is_complete = false;
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
    free(tu);
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
    size_t i = local_symbol_count;
    size_t start = local_symbol_start;
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
// STATEMENTS
////////////////////////////////
static bool parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* decl_node) {
    Cuik_Type* type = cuik_canonical_type(decl_node->decl.type);

    Param* param_list = type->func.param_list;
    size_t param_count = type->func.param_count;

    assert(local_symbol_start == local_symbol_count);
    if (param_count >= INT16_MAX) {
        diag_err(s, decl_node->loc, "Function parameter count cannot exceed %d (got %d)", param_count, MAX_LOCAL_SYMBOLS);
        return false;
    }

    for (size_t i = 0; i < param_count; i++) {
        Param* p = &param_list[i];

        if (p->name) {
            local_symbols[local_symbol_count++] = (Symbol){
                .name = p->name,
                .type = p->type,
                .storage_class = STORAGE_PARAM,
                .param_num = i
            };
        }
    }

    // skip { for parse_compound_stmt
    tokens_next(s);

    cuik__sema_function_stmt = decl_node;
    Stmt* body = make_stmt(tu, s);
    parse_compound_stmt(tu, s, body);
    cuik__sema_function_stmt = NULL;

    decl_node->op = STMT_FUNC_DECL;
    decl_node->decl.initial_as_stmt = body;

    nl_strmap_free(labels);
    return true;
}

static void parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s, Stmt* node) {
    LOCAL_SCOPE {
        node->loc.start = tokens_get_location(s);

        size_t kid_count = 0;
        Stmt** kids = tls_save();

        while (tokens_get(s)->type != '}') {
            if (tokens_get(s)->type == ';') {
                tokens_next(s);
            } else {
                Stmt* stmt = parse_stmt(tu, s);
                if (stmt) {
                    tls_push(sizeof(Stmt*));
                    kids[kid_count++] = stmt;
                } else {
                    // this will push the decl or expression if it catches one
                    parse_decl_or_expr(tu, s, &kid_count);
                }
            }
        }

        expect(tu, s, '}');
        node->loc.end = tokens_get_last_location(s);

        Stmt** permanent_storage = arena_alloc(&thread_arena, kid_count * sizeof(Stmt*), _Alignof(Stmt*));
        memcpy(permanent_storage, kids, kid_count * sizeof(Stmt*));

        node->compound = (struct StmtCompound){
            .kids = permanent_storage,
            .kids_count = kid_count,
        };

        tls_restore(kids);
    }
}

// Doesn't handle declarators or expression-statements
static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s) {
    // _Static_assert doesn't produce a statement, handle these first.
    // label declarations only produce a new statement if they haven't been used yet.
    SourceLoc start = tokens_get_location(s);
    while (tokens_get(s)->type == TOKEN_KW_Static_assert) {
        tokens_next(s);
        expect(tu, s, '(');

        intmax_t condition = parse_const_expr(tu, s);
        SourceLoc end = tokens_get_last_location(s);

        if (tokens_get(s)->type == ',') {
            tokens_next(s);

            Token* t = tokens_get(s);
            if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
                generic_error(tu, s, "static assertion expects string literal");
            }
            tokens_next(s);

            if (condition == 0) {
                diag_err(&tu->tokens, (SourceRange){ start, end }, "static assertion failed! %.*s", (int) t->content.length, t->content.data);
            }
        } else {
            if (condition == 0) {
                diag_err(&tu->tokens, (SourceRange){ start, end }, "static assertion failed!");
            }
        }

        expect(tu, s, ')');
        expect(tu, s, ';');
    }

    Stmt* n = NULL;
    SourceLoc loc_start = tokens_get_location(s);
    TknType peek = tokens_get(s)->type;
    if (peek == '{') {
        tokens_next(s);

        n = make_stmt(tu, s);
        parse_compound_stmt(tu, s, n);
    } else if (peek == TOKEN_KW_return) {
        n = make_stmt(tu, s);
        tokens_next(s);

        Expr* e = 0;
        if (tokens_get(s)->type != ';') {
            e = parse_expr(tu, s);
        }

        n->op = STMT_RETURN;
        n->return_ = (struct StmtReturn){ .expr = e };

        expect_with_reason(s, ';', "return");
    } else if (peek == TOKEN_KW_if) {
        n = make_stmt(tu, s);
        tokens_next(s);

        LOCAL_SCOPE {
            Expr* cond;
            {
                SourceLoc opening_loc = tokens_get_location(s);
                expect(tu, s, '(');

                cond = parse_expr(tu, s);

                expect_closing_paren(s, opening_loc);
            }

            Stmt* body;
            LOCAL_SCOPE {
                body = parse_stmt_or_expr(tu, s);
            }

            Stmt* next = 0;
            if (tokens_get(s)->type == TOKEN_KW_else) {
                tokens_next(s);

                LOCAL_SCOPE {
                    next = parse_stmt_or_expr(tu, s);
                }
            }

            n->op = STMT_IF;
            n->if_ = (struct StmtIf){
                .cond = cond,
                .body = body,
                .next = next,
            };
        }
    } else if (peek == TOKEN_KW_switch) {
        n = make_stmt(tu, s);
        tokens_next(s);

        LOCAL_SCOPE {
            expect(tu, s, '(');
            Expr* cond = parse_expr(tu, s);
            expect(tu, s, ')');

            n->op = STMT_SWITCH;
            n->switch_ = (struct StmtSwitch){ .condition = cond };

            // begin a new chain but keep the old one
            Stmt* old_switch = current_switch_or_case;
            current_switch_or_case = n;

            Stmt* old_breakable = current_breakable;
            current_breakable = n;
            LOCAL_SCOPE {
                Stmt* body = parse_stmt_or_expr(tu, s);
                n->switch_.body = body;
            }
            current_breakable = old_breakable;
            current_switch_or_case = old_switch;
        }
    } else if (peek == TOKEN_KW_case) {
        // TODO(NeGate): error messages
        n = make_stmt(tu, s);
        assert(current_switch_or_case);
        tokens_next(s);

        Stmt* top = n;
        top->op = STMT_CASE;

        intmax_t key = parse_const_expr(tu, s);
        if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
            // GNU extension, case ranges
            tokens_next(s);
            intmax_t key_max = parse_const_expr(tu, s);
            expect(tu, s, ':');

            assert(key_max > key);
            n->case_.key = key;

            for (intmax_t i = key; i < key_max; i++) {
                Stmt* curr = make_stmt(tu, s);
                curr->op = STMT_CASE;
                curr->case_ = (struct StmtCase){.key = i + 1};

                // Append to list
                n->case_.next = n->case_.body = curr;
                n = curr;
            }
        } else {
            expect(tu, s, ':');

            n->case_ = (struct StmtCase){
                .key = key, .body = 0, .next = 0
            };
        }

        switch (current_switch_or_case->op) {
            case STMT_CASE: current_switch_or_case->case_.next = top; break;
            case STMT_DEFAULT: current_switch_or_case->default_.next = top; break;
            case STMT_SWITCH: current_switch_or_case->switch_.next = top; break;
            default: __builtin_unreachable();
        }
        current_switch_or_case = n;

        Stmt* body = parse_stmt_or_expr(tu, s);
        n->case_.body = body;
    } else if (peek == TOKEN_KW_default) {
        // TODO(NeGate): error messages
        n = make_stmt(tu, s);
        assert(current_switch_or_case);
        tokens_next(s);

        switch (current_switch_or_case->op) {
            case STMT_CASE: current_switch_or_case->case_.next = n; break;
            case STMT_DEFAULT: current_switch_or_case->default_.next = n; break;
            case STMT_SWITCH: current_switch_or_case->switch_.next = n; break;
            default: __builtin_unreachable();
        }
        current_switch_or_case = n;
        expect(tu, s, ':');

        n->op = STMT_DEFAULT;
        n->default_ = (struct StmtDefault){
            .body = 0, .next = 0,
        };

        Stmt* body = parse_stmt_or_expr(tu, s);
        n->default_.body = body;
    } else if (peek == TOKEN_KW_break) {
        // TODO(NeGate): error messages
        n = make_stmt(tu, s);
        assert(current_breakable);

        tokens_next(s);
        expect(tu, s, ';');

        n->op = STMT_BREAK;
        n->break_ = (struct StmtBreak){
            .target = current_breakable,
        };
    } else if (peek == TOKEN_KW_continue) {
        // TODO(NeGate): error messages
        n = make_stmt(tu, s);
        assert(current_continuable);

        tokens_next(s);
        expect(tu, s, ';');

        n->op = STMT_CONTINUE;
        n->continue_ = (struct StmtContinue){
            .target = current_continuable,
        };
    } else if (peek == TOKEN_KW_while) {
        n = make_stmt(tu, s);
        tokens_next(s);

        LOCAL_SCOPE {
            expect(tu, s, '(');
            Expr* cond = parse_expr(tu, s);
            expect(tu, s, ')');

            // Push this as a breakable statement
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                body = parse_stmt_or_expr(tu, s);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            n->op = STMT_WHILE;
            n->while_ = (struct StmtWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_for) {
        n = make_stmt(tu, s);
        tokens_next(s);

        LOCAL_SCOPE {
            expect(tu, s, '(');

            // it's either nothing, a declaration, or an expression
            Stmt* first = NULL;
            if (tokens_get(s)->type == ';') {
                /* nothing */
                tokens_next(s);
            } else {
                // NOTE(NeGate): This is just a decl list or a single expression.
                first = make_stmt(tu, s);
                first->op = STMT_COMPOUND;

                size_t kid_count = 0;
                Stmt** kids = tls_save();
                {
                    parse_decl_or_expr(tu, s, &kid_count);
                }
                Stmt** permanent_storage = arena_alloc(&thread_arena, kid_count * sizeof(Stmt*), _Alignof(Stmt*));
                memcpy(permanent_storage, kids, kid_count * sizeof(Stmt*));

                first->compound = (struct StmtCompound){
                    .kids = permanent_storage,
                    .kids_count = kid_count,
                };
                tls_restore(kids);
            }

            Expr* cond = NULL;
            if (tokens_get(s)->type == ';') {
                /* nothing */
                tokens_next(s);
            } else {
                cond = parse_expr(tu, s);
                expect(tu, s, ';');
            }

            Expr* next = NULL;
            if (tokens_get(s)->type == ')') {
                /* nothing */
                tokens_next(s);
            } else {
                next = parse_expr(tu, s);
                expect(tu, s, ')');
            }

            // Push this as a breakable statement
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                body = parse_stmt_or_expr(tu, s);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            n->op = STMT_FOR;
            n->for_ = (struct StmtFor){
                .first = first,
                .cond = cond,
                .body = body,
                .next = next,
            };
        }
    } else if (peek == TOKEN_KW_do) {
        n = make_stmt(tu, s);
        tokens_next(s);

        // Push this as a breakable statement
        LOCAL_SCOPE {
            Stmt* body;
            LOCAL_SCOPE {
                Stmt* old_breakable = current_breakable;
                current_breakable = n;
                Stmt* old_continuable = current_continuable;
                current_continuable = n;

                body = parse_stmt_or_expr(tu, s);

                current_breakable = old_breakable;
                current_continuable = old_continuable;
            }

            if (tokens_get(s)->type != TOKEN_KW_while) {
                Token* t = tokens_get(s);

                REPORT(ERROR, t->location, "%s:%d: error: expected 'while' got '%.*s'", (int)t->content.length, t->content.data);
                abort();
            }
            tokens_next(s);

            expect(tu, s, '(');

            Expr* cond = parse_expr(tu, s);

            expect(tu, s, ')');
            expect(tu, s, ';');

            n->op = STMT_DO_WHILE;
            n->do_while = (struct StmtDoWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_goto) {
        n = make_stmt(tu, s);
        tokens_next(s);

        // read label name
        Token* t = tokens_get(s);
        SourceLoc loc = t->location;
        if (t->type != TOKEN_IDENTIFIER) {
            REPORT(ERROR, loc, "expected identifier for goto target name");
            return n;
        }

        Atom name = atoms_put(t->content.length, t->content.data);

        // skip to the semicolon
        tokens_next(s);

        Expr* target = make_expr(tu);
        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            *target = (Expr){
                .op = EXPR_SYMBOL,
                .loc = { loc, loc },
                .symbol = labels[search],
            };
        } else {
            // not defined yet, make a placeholder
            Stmt* label_decl = make_stmt(tu, s);
            label_decl->op = STMT_LABEL;
            label_decl->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, label_decl);

            *target = (Expr){
                .op = EXPR_SYMBOL,
                .loc = { loc, loc },
                .symbol = label_decl,
            };
        }

        n->op = STMT_GOTO;
        n->goto_ = (struct StmtGoto){
            .target = target,
        };

        expect(tu, s, ';');
    } else if (peek == TOKEN_IDENTIFIER || tokens_peek(s)->type == TOKEN_COLON) {
        // label amirite
        // IDENTIFIER COLON STMT
        n = make_stmt(tu, s);

        Token* t = tokens_get(s);
        Atom name = atoms_put(t->content.length, t->content.data);

        Stmt* n = NULL;
        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            n = labels[search];
        } else {
            n = make_stmt(tu, s);
            n->op = STMT_LABEL;
            n->loc = tokens_get_range(s);
            n->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, n);
        }

        n->label.placed = true;

        tokens_next(s);
        tokens_next(s);
    }

    if (n != NULL) {
        n->loc.start = loc_start;
        n->loc.end = tokens_get_last_location(s);
    }
    return n;
}

// Doesn't handle it's own error recovery, once we return false the caller should try
// what it feels is necessary to get it to another parsable unit.
static bool parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            diag_err(s, tokens_get_range(s), "pragma declaration expects string literal");
            return false;
        }
        tokens_next(s);

        expect(tu, s, ')');
        return true;
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
        return true;
    } else if (is_typename(&tu->globals, s)) {
        Attribs attr = { 0 };
        Cuik_QualType type = parse_declspec(tu, s, &attr);

        if (attr.is_typedef) {
            // don't expect one the first time
            bool expect_comma = false;
            while (tokens_get(s)->type != ';') {
                if (expect_comma) {
                    expect_with_reason(s, ',', "typedef");
                } else {
                    expect_comma = true;
                }

                Decl decl = parse_declarator(tu, s, type, false);
                if (decl.name) {
                    diag_err(s, decl.loc, "typedef declaration must be named");
                    continue;
                }

                // make typedef
                Stmt* n = make_stmt(tu, s);
                n->op = STMT_DECL;
                n->loc = decl.loc;
                n->decl = (struct StmtDecl){
                    .name = decl.name,
                    .type = decl.type,
                    .attrs = attr,
                };

                if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
                    diag_err(s, decl.loc, "local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
                    return false;
                }

                local_symbols[local_symbol_count++] = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .storage_class = STORAGE_TYPEDEF,
                };
            }

            expect(tu, s, ';');
        } else {
            bool expect_comma = false;
            while (tokens_get(s)->type != ';') {
                if (expect_comma) {
                    if (tokens_get(s)->type == '{') {
                        diag_err(s, tokens_get_range(s), "nested functions are not allowed... yet");
                        return false;
                    } else if (tokens_get(s)->type != ',') {
                        SourceLoc loc = tokens_get_last_location(s);
                        report_fix(REPORT_ERROR, s, loc, ";", "expected semicolon at the end of declaration");
                    } else {
                        tokens_next(s);
                    }
                } else {
                    expect_comma = true;
                }

                Decl decl = parse_declarator(tu, s, type, false);

                Stmt* n = make_stmt(tu, s);
                n->op = STMT_DECL;
                n->loc = decl.loc;
                n->decl = (struct StmtDecl){
                    .type = decl.type,
                    .name = decl.name,
                    .attrs = attr,
                    .initial = 0,
                };

                if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
                    diag_err(s, decl.loc, "Local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
                    return false;
                }

                if (decl.name != NULL) {
                    local_symbols[local_symbol_count++] = (Symbol){
                        .name = decl.name,
                        .type = decl.type,
                        .storage_class = STORAGE_LOCAL,
                        .stmt = n,
                    };
                }

                if (tokens_get(s)->type == '=') {
                    tokens_next(s);

                    if (tokens_get(s)->type == '{') {
                        tokens_next(s);

                        n->decl.initial = parse_initializer(tu, s, CUIK_QUAL_TYPE_NULL);
                    } else {
                        n->decl.initial = parse_expr_assign(tu, s);
                    }
                }

                *((Stmt**)tls_push(sizeof(Stmt*))) = n;
                *body_count += 1;
            }

            expect(tu, s, ';');
        }

        return true;
    } else {
        Stmt* n = make_stmt(tu, s);
        Expr* expr = parse_expr(tu, s);

        n->op = STMT_EXPR;
        n->loc = expr->loc;
        n->expr = (struct StmtExpr){ .expr = expr };

        *((Stmt**)tls_push(sizeof(Stmt*))) = n;
        *body_count += 1;

        expect(tu, s, ';');
        return true;
    }
}

static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            diag_err(s, tokens_get_range(s), "pragma declaration expects string literal");
            return NULL;
        }
        tokens_next(s);

        expect(tu, s, ')');
        return NULL;
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
        return NULL;
    } else {
        Stmt* stmt = parse_stmt(tu, s);
        if (stmt != NULL) {
            return stmt;
        } else {
            Stmt* n = make_stmt(tu, s);

            Expr* expr = parse_expr(tu, s);
            n->op = STMT_EXPR;
            n->loc = expr->loc;
            n->expr = (struct StmtExpr){ .expr = expr };

            expect(tu, s, ';');
            return n;
        }
    }
}

static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s) {
    Expr* folded = cuik__optimize_ast(tu, parse_expr_assign(tu, s));
    if (folded->op != EXPR_INT) {
        diag_err(s, folded->loc, "could not parse expression as constant.");
        return 0;
    }

    return (intmax_t) folded->int_num.num;
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
        SourceLoc loc = tokens_get_location(s);

        report_two_spots(
            REPORT_ERROR, s, opening, loc,
            "expected closing parenthesis", "open", "close?", NULL
        );
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
