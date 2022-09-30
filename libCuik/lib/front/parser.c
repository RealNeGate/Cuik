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
#include <targets/targets.h>

#undef VOID // winnt.h loves including garbage

#define OUT_OF_ORDER_CRAP 1

// HACK: this is defined in sema.c but because we sometimes call semantics stuff in the
// parser we need access to it to avoid some problems
extern thread_local Stmt* cuik__sema_function_stmt;

// how big are the phase2 parse tasks
#define PARSE_MUNCH_SIZE (131072)

typedef struct {
    Atom key;
    Cuik_Type* value;
} TagEntry;

typedef struct {
    enum {
        PENDING_ALIGNAS
    } mode;
    size_t expr_pos;
    int* dst;
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
static void expect_closing_paren(TranslationUnit* tu, TokenStream* restrict s, SourceLocIndex opening);
static void expect_with_reason(TranslationUnit* tu, TokenStream* restrict s, char ch, const char* reason);
static Symbol* find_local_symbol(TokenStream* restrict s);

static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s);
static void parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TranslationUnit* tu, TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static Cuik_Type* parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type, bool is_abstract, bool disabled_paren);
static Cuik_Type* parse_typename(TranslationUnit* tu, TokenStream* restrict s);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_initializer(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type);
static Expr* parse_function_literal(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type);
static void parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* n);
static Cuik_Type* parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, Cuik_Type* type, Atom name);

static bool is_typename(TranslationUnit* tu, TokenStream* restrict s);

static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg);

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

static String get_token_as_string(Lexer* l) {
    return (String){ .length = l->token_end - l->token_start, .data = l->token_start };
}

// allocated size is sizeof(struct StmtHeader) + extra_size
static Stmt* make_stmt(TranslationUnit* tu, TokenStream* restrict s, StmtOp op, size_t extra_size) {
    Stmt* stmt = arena_alloc(&local_ast_arena, sizeof(Stmt), _Alignof(max_align_t));

    memset(stmt, 0, offsetof(Stmt, backing.user_data) + sizeof(((Stmt*)0)->backing.user_data));

    stmt->op = op;
    stmt->loc = tokens_get_location_index(s);
    return stmt;
}

static Expr* make_expr(TranslationUnit* tu) {
    return ARENA_ALLOC(&local_ast_arena, Expr);
}

static Symbol* find_global_symbol(TranslationUnit* tu, const char* name) {
    ptrdiff_t search = nl_strmap_get_cstr(tu->global_symbols, name);
    return (search >= 0) ? &tu->global_symbols[search] : NULL;
}

static Cuik_Type* find_tag(TranslationUnit* tu, const char* name) {
    // try locals
    size_t i = local_tag_count;
    while (i--) {
        if (strcmp((const char*)local_tags[i].key, name) == 0) {
            return local_tags[i].value;
        }
    }

    // try globals
    ptrdiff_t search = nl_strmap_get_cstr(tu->global_tags, name);
    return (search >= 0) ? tu->global_tags[search] : NULL;
}

// ( SOMETHING )
// ( SOMETHING ,
static size_t skip_expression_in_parens(TokenStream* restrict s, TknType* out_terminator) {
    size_t saved = s->current;

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
    size_t saved = s->current;

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

static void diag_unresolved_symbol(TranslationUnit* tu, Atom name, SourceLocIndex loc) {
    Diag_UnresolvedSymbol* d = ARENA_ALLOC(&thread_arena, Diag_UnresolvedSymbol);
    d->next = NULL;
    d->name = name;
    d->loc = loc;

    mtx_lock(&tu->diag_mutex);
    ptrdiff_t search = nl_strmap_get_cstr(tu->unresolved_symbols, name);
    if (search < 0) {
        search = nl_strmap_puti_cstr(tu->unresolved_symbols, name);
        tu->unresolved_symbols[search] = d;
    } else {
        Diag_UnresolvedSymbol* old = tu->unresolved_symbols[search];
        while (old->next != NULL) old = old->next;

        old->next = d;
    }
    mtx_unlock(&tu->diag_mutex);
}

#include "expr_parser.h"
#include "decl_parser.h"

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
    CUIK_TIMED_BLOCK("phase 3: %zu-%zu", start, end) {
        out_of_order_mode = false;

        for (size_t i = start; i < end; i++) {
            Symbol* sym = &tu->global_symbols[i];

            // don't worry about normal globals, those have been taken care of...
            if (sym->current != 0 && (sym->storage_class == STORAGE_STATIC_FUNC || sym->storage_class == STORAGE_FUNC)) {
                // Spin up a mini parser here
                tokens.current = sym->current;

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
    atoms_init();

    // allocate the local symbol tables
    if (local_symbols == NULL) {
        local_symbols = realloc(local_symbols, sizeof(Symbol) * MAX_LOCAL_SYMBOLS);
        local_tags = realloc(local_tags, sizeof(TagEntry) * MAX_LOCAL_TAGS);
    }

    if (pending_exprs) {
        dyn_array_clear(pending_exprs);
    } else {
        pending_exprs = dyn_array_create(PendingExpr);
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
static int type_cycles_dfs(TranslationUnit* restrict tu, Cuik_Type* type, uint8_t* visited, uint8_t* finished) {
    // non-record types are always finished :P
    if (type->kind != KIND_STRUCT && type->kind != KIND_UNION) {
        return 0;
    }

    // if (finished[o]) return false
    int o = type->ordinal;
    if (finished[o / 8] & (1u << (o % 8))) {
        return 0;
    }

    // if (visited[o]) return true
    if (visited[o / 8] & (1u << (o % 8))) {
        return 1;
    }

    // visited[o] = true
    visited[o / 8] |= (1u << (o % 8));

    // for each m in members
    //   if (dfs(m)) return true
    for (size_t i = 0; i < type->record.kid_count; i++) {
        int c = type_cycles_dfs(tu, type->record.kids[i].type, visited, finished);
        if (c) {
            // we already gave an error message, don't be redundant
            if (c != 2) {
                const char* name = type->record.name ? (const char*)type->record.name : "<unnamed>";

                char tmp[256];
                sprintf_s(tmp, sizeof(tmp), "type %s has cycles", name);

                report_two_spots(REPORT_ERROR, tu->errors,
                    &tu->tokens, type->loc, type->record.kids[i].loc,
                    tmp, NULL, NULL, "on");
            }

            return 2;
        }
    }

    // finished[o] = true
    finished[o / 8] |= (1u << (o % 8));
    return 0;
}

static void type_resolve_pending_align(TranslationUnit* restrict tu, Cuik_Type* type) {
    size_t pending_count = dyn_array_length(pending_exprs);
    for (size_t i = 0; i < pending_count; i++) {
        if (pending_exprs[i].dst == &type->align) {
            assert(pending_exprs[i].mode == PENDING_ALIGNAS);

            TokenStream mini_lex = tu->tokens;
            mini_lex.current = pending_exprs[i].expr_pos;

            SourceLocIndex loc = tokens_get_location_index(&mini_lex);

            int align = 0;
            if (is_typename(tu, &mini_lex)) {
                Cuik_Type* new_align = parse_typename(tu, &mini_lex);
                if (new_align == NULL || new_align->align) {
                    REPORT(ERROR, loc, "_Alignas cannot operate with incomplete");
                } else {
                    align = new_align->align;
                }
            } else {
                intmax_t new_align = parse_const_expr(tu, &mini_lex);
                if (new_align == 0) {
                    REPORT(ERROR, loc, "_Alignas cannot be applied with 0 alignment", new_align);
                } else if (new_align >= INT16_MAX) {
                    REPORT(ERROR, loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
                } else {
                    align = new_align;
                }
            }

            assert(align != 0);
            type->align = align;
            return;
        }
    }

    abort();
}

void type_layout(TranslationUnit* restrict tu, Cuik_Type* type, bool needs_complete) {
    if (type->kind == KIND_VOID || type->size != 0) return;
    if (type->is_inprogress) {
        REPORT(ERROR, type->loc, "Type has a circular dependency");
        abort();
    }

    type->is_inprogress = true;

    if (type->kind == KIND_ARRAY) {
        if (type->array_count_lexer_pos) {
            // run mini parser for array count
            TokenStream mini_lex = tu->tokens;
            mini_lex.current = type->array_count_lexer_pos;
            type->array_count = parse_const_expr(tu, &mini_lex);
            expect(tu, &mini_lex, ']');
        }

        // layout crap
        if (type->array_count != 0) {
            if (type->array_of->size == 0) {
                type_layout(tu, type->array_of, true);
            }

            if (type->array_of->size == 0) {
                REPORT(ERROR, type->loc, "could not resolve type (ICE)");
                abort();
            }
        }

        uint64_t result = type->array_of->size * type->array_count;

        // size checks
        if (result >= INT32_MAX) {
            REPORT(ERROR, type->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
        }

        type->size = result;
        type->align = type->array_of->align;
    } else if (type->kind == KIND_QUALIFIED_TYPE) {
        if (type->qualified_ty->size == 0) {
            type_layout(tu, type->qualified_ty, needs_complete);
        }

        type->size = type->qualified_ty->size;
        type->align = type->qualified_ty->align;
    } else if (type->kind == KIND_ENUM) {
        int cursor = 0;

        for (int i = 0; i < type->enumerator.count; i++) {
            // if the value is undecided, best time to figure it out is now
            if (type->enumerator.entries[i].lexer_pos != 0) {
                // Spin up a mini expression parser here
                TokenStream mini_lex = tu->tokens;
                mini_lex.current = type->enumerator.entries[i].lexer_pos;

                cursor = parse_const_expr(tu, &mini_lex);
            }

            type->enumerator.entries[i].value = cursor;
            cursor += 1;
        }

        type->size = 4;
        type->align = 4;
        type->is_incomplete = false;
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

            if (member->type->kind == KIND_FUNC) {
                REPORT(ERROR, type->loc, "Cannot put function types into a struct, try a function pointer");
            } else {
                type_layout(tu, member->type, true);
            }

            int member_align = member->type->align;
            int member_size = member->type->size;
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
                int bits_in_region = member->type->kind == KIND_BOOL ? 1 : (member_size * 8);
                if (bit_width > bits_in_region) {
                    REPORT(ERROR, type->loc, "Bitfield cannot fit in this type.");
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
        type->is_incomplete = false;
    }

    type->is_inprogress = false;
}

static const Cuik_Warnings DEFAULT_WARNINGS = { 0 };

CUIK_API TranslationUnit* cuik_parse_translation_unit(const Cuik_TranslationUnitDesc* restrict desc) {
    // we can preserve this across multiple uses
    thread_local static NL_Strmap(Cuik_Type*) s_global_tags;
    thread_local static NL_Strmap(Symbol) s_global_symbols;

    if (cuik_is_profiling()) {
        cuik_profile_region_start(cuik_time_in_nanos(), "parse: %s", desc->tokens->filepath);
    }

    assert(desc->tokens != NULL);
    assert(desc->errors != NULL);
    memset(desc->errors, 0, sizeof(*desc->errors));

    // hacky but i don't wanna wrap it in a CUIK_TIMED_BLOCK
    uint64_t timer_start = cuik_time_in_nanos();

    TranslationUnit* tu = calloc(1, sizeof(TranslationUnit));
    tu->ref_count = 1;
    tu->filepath = desc->tokens->filepath;
    tu->is_windows_long = desc->target->sys == CUIK_SYSTEM_WINDOWS;
    tu->target = *desc->target;
    tu->tokens = *desc->tokens;
    tu->errors = desc->errors;
    tu->warnings = desc->warnings ? desc->warnings : &DEFAULT_WARNINGS;

    #ifdef CUIK_USE_TB
    tu->ir_mod = desc->ir_module;
    tu->has_tb_debug_info = desc->has_debug_info;
    #endif

    tls_init();
    atoms_init();
    mtx_init(&tu->arena_mutex, mtx_plain);
    mtx_init(&tu->diag_mutex, mtx_plain);

    reset_global_parser_state();

    // use the thread local ones (we'll set our resulting hash map back in
    // such that if it resized the first time we don't need to resize again
    // if used on that same thread, just avoiding allocations and such)
    tu->global_symbols = s_global_symbols;
    tu->global_tags = s_global_tags;

    nl_strmap_clear(tu->global_symbols);
    nl_strmap_clear(tu->global_tags);

    ////////////////////////////////
    // Parse translation unit
    ////////////////////////////////
    out_of_order_mode = true;
    DynArray(int) static_assertions = dyn_array_create(int);
    TokenStream* restrict s = desc->tokens;

    tu->top_level_stmts = dyn_array_create(Stmt*);

    if (pending_exprs) {
        dyn_array_clear(pending_exprs);
    } else {
        pending_exprs = dyn_array_create(PendingExpr);
    }

    // Phase 1: resolve all top level statements
    CUIK_TIMED_BLOCK("phase 1") {
        while (tokens_get(s)->type) {
            while (tokens_get(s)->type == ';') tokens_next(s);

            // TODO(NeGate): Correctly parse pragmas instead of ignoring them.
            if (tokens_get(s)->type == TOKEN_KW_Pragma) {
                tokens_next(s);
                expect(tu, s, '(');

                if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
                    generic_error(tu, s, "pragma declaration expects string literal");
                }

                // Slap it into a proper C string so we don't accidentally
                // walk off the end and go random places
                size_t len = (tokens_get(s)->end - tokens_get(s)->start) - 1;
                unsigned char* out = tls_push(len);
                {
                    const char* in = (const char*) tokens_get(s)->start;

                    size_t out_i = 0, in_i = 1;
                    while (in_i < len) {
                        int ch;
                        ptrdiff_t distance = parse_char(len - in_i, &in[in_i], &ch);
                        if (distance < 0) {
                            generic_error(tu, s, "failed to handle string literal");
                            abort();
                        }

                        out[out_i++] = ch;
                        in_i += distance;
                    }

                    assert(out_i <= len);
                    out[out_i++] = '\0';
                }

                Lexer pragma_lex = { "<temp>", out, out, 1 };
                lexer_read(&pragma_lex);

                String pragma_name = get_token_as_string(&pragma_lex);
                if (string_equals_cstr(&pragma_name, "comment")) {
                    // https://learn.microsoft.com/en-us/cpp/preprocessor/comment-c-cpp?view=msvc-170
                    //   'comment' '(' comment-type [ ',' "comment-string" ] ')'
                    // supported comment types:
                    //   lib - links library against final output
                    lexer_read(&pragma_lex);

                    if (pragma_lex.token_type != '(') {
                        generic_error(tu, s, "expected (");
                    }

                    lexer_read(&pragma_lex);
                    String comment_type = get_token_as_string(&pragma_lex);
                    lexer_read(&pragma_lex);

                    String comment_string;
                    if (pragma_lex.token_type == ',') {
                        lexer_read(&pragma_lex);
                        comment_string = get_token_as_string(&pragma_lex);
                        comment_string.length -= 2;
                        comment_string.data += 1;

                        lexer_read(&pragma_lex);
                    }

                    if (pragma_lex.token_type != ')') {
                        generic_error(tu, s, "expected )");
                    }

                    if (string_equals_cstr(&comment_type, "lib")) {
                        if (comment_string.length == 0) {
                            generic_error(tu, s, "pragma comment lib expected lib name");
                        }

                        Cuik_ImportRequest* import = ARENA_ALLOC(&thread_arena, Cuik_ImportRequest);
                        import->next = tu->import_libs;
                        import->lib_name = atoms_put(comment_string.length, comment_string.data);
                        tu->import_libs = import;
                    } else {
                        generic_error(tu, s, "unknown pragma comment option");
                    }
                }
                tokens_next(s);

                tls_restore(out);
                expect(tu, s, ')');
            } else if (tokens_get(s)->type == TOKEN_KW_Static_assert) {
                tokens_next(s);
                expect(tu, s, '(');

                TknType terminator;
                size_t current = skip_expression_in_parens(s, &terminator);
                dyn_array_put(static_assertions, current);

                tokens_prev(s);
                if (tokens_get(s)->type == ',') {
                    tokens_next(s);

                    Token* t = tokens_get(s);
                    if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
                        generic_error(tu, s, "static assertion expects string literal");
                    }
                    tokens_next(s);
                }

                expect(tu, s, ')');
            } else {
                Cuik_Attribute* attribute_list = parse_attributes(tu, s, NULL);
                SourceLocIndex loc = tokens_get_location_index(s);

                // must be a declaration since it's a top level statement
                Attribs attr = {0};
                Cuik_Type* type = parse_declspec(tu, s, &attr);
                if (type == NULL) {
                    REPORT(ERROR, loc, "Could not parse base type.");
                }

                if (attr.is_typedef) {
                    // declarator (',' declarator)+ ';'
                    while (true) {
                        size_t start_decl_token = s->current;
                        Decl decl = parse_declarator(tu, s, type, false, false);

                        // we wanna avoid getting stuck in infinite loops so if we dont
                        // do anything in an iteration then we want to exit with an error
                        bool possibly_bad_decl = (s->current == start_decl_token);

                        if (decl.name != NULL) {
                            // make typedef
                            Stmt* n = make_stmt(tu, s, STMT_DECL, sizeof(struct StmtDecl));
                            n->loc = decl.loc;
                            n->attr_list = parse_attributes(tu, s, attribute_list);
                            n->decl = (struct StmtDecl){
                                .name = decl.name,
                                .type = decl.type,
                                .attrs = attr,
                            };

                            // typedefs can't be roots ngl
                            n->decl.attrs.is_root = false;
                            dyn_array_put(tu->top_level_stmts, n);

                            // check for collision
                            Symbol* search = find_global_symbol(tu, decl.name);
                            if (search != NULL) {
                                if (search->storage_class != STORAGE_TYPEDEF) {
                                    report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, search->loc,
                                        "typedef overrides previous declaration.",
                                        "old", "new", NULL);
                                }

                                Cuik_Type* placeholder_space = search->type;
                                if (placeholder_space->kind != KIND_PLACEHOLDER && !type_equal(tu, decl.type, search->type)) {
                                    report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, search->loc,
                                        "typedef overrides previous declaration.",
                                        "old", "new", NULL);
                                }

                                // replace placeholder with actual entry
                                Atom old_name = decl.name;

                                *placeholder_space = (Cuik_Type){
                                    .kind = KIND_QUALIFIED_TYPE,
                                    .size = decl.type->size,
                                    .align = decl.type->align,
                                    .loc = decl.loc,
                                    .also_known_as = old_name,
                                    .qualified_ty = decl.type,
                                };
                            } else {
                                Cuik_Type* clone = new_qualified_type(tu, decl.type, decl.type->is_atomic, decl.type->is_const);
                                clone->also_known_as = decl.name;

                                // add new entry
                                Symbol sym = {
                                    .name = decl.name,
                                    .type = clone,
                                    .loc = decl.loc,
                                    .storage_class = STORAGE_TYPEDEF,
                                };
                                nl_strmap_put_cstr(tu->global_symbols, decl.name, sym);
                            }
                        }

                        if (tokens_get(s)->type == 0) {
                            REPORT(ERROR, loc, "declaration list ended with EOF instead of semicolon.");
                            break;
                        } else if (tokens_get(s)->type == '=') {
                            REPORT(ERROR, loc, "why did you just try that goofy shit wit me. You cannot assign a typedef.");
                            // error recovery
                        } else if (tokens_get(s)->type == ';') {
                            tokens_next(s);
                            break;
                        } else if (tokens_get(s)->type == ',') {
                            tokens_next(s);
                            continue;
                        } else if (possibly_bad_decl) {
                            REPORT(ERROR, loc, "Bad declaration");
                            tokens_next(s);
                            break;
                        }
                    }
                } else {
                    if (tokens_get(s)->type == ';') {
                        Stmt* n = make_stmt(tu, s, STMT_GLOBAL_DECL, sizeof(struct StmtDecl));
                        n->loc = loc;
                        n->attr_list = attribute_list;
                        n->decl = (struct StmtDecl){
                            .name = NULL,
                            .type = type,
                            .attrs = attr,
                        };
                        n->decl.attrs.is_root = true;
                        dyn_array_put(tu->top_level_stmts, n);

                        tokens_next(s);
                        continue;
                    }

                    // normal variable lists
                    // declarator (',' declarator )+ ';'
                    while (true) {
                        size_t start_decl_token = s->current;

                        SourceLocIndex decl_loc = tokens_get_location_index(s);
                        Decl decl = parse_declarator(tu, s, type, false, false);
                        if (decl.name == NULL) {
                            REPORT(ERROR, decl_loc, "Declaration has no name");
                            break;
                        }

                        // we wanna avoid getting stuck in infinite loops so if we dont
                        // do anything in an iteration then we want to exit with an error
                        bool possibly_bad_decl = (s->current == start_decl_token);

                        Stmt* n = make_stmt(tu, s, STMT_GLOBAL_DECL, sizeof(struct StmtDecl));
                        n->loc = decl.loc;
                        n->attr_list = attribute_list;
                        n->decl = (struct StmtDecl){
                            .name = decl.name,
                            .type = decl.type,
                            .attrs = attr,
                        };
                        dyn_array_put(tu->top_level_stmts, n);

                        Symbol* sym = find_global_symbol(tu, decl.name);
                        Symbol* old_definition = sym;
                        if (sym == NULL) {
                            // slap that bad boy into the symbol table
                            ptrdiff_t sym_index = nl_strmap_puti_cstr(tu->global_symbols, decl.name);
                            sym = &tu->global_symbols[sym_index];
                        }

                        *sym = (Symbol){
                            .name = decl.name,
                            .type = decl.type,
                            .loc = decl.loc,
                            .stmt = n,
                        };

                        if (decl.type->kind == KIND_FUNC) {
                            sym->storage_class = (attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC);
                        } else {
                            sym->storage_class = (attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL);
                        }

                        n->attr_list = parse_attributes(tu, s, n->attr_list);

                        bool requires_terminator = true;
                        if (tokens_get(s)->type == '=') {
                            tokens_next(s);

                            if (old_definition && old_definition->current != 0) {
                                report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, old_definition->stmt->loc,
                                    "Cannot redefine global declaration",
                                    NULL, NULL, "previous definition was:");
                            }

                            if (n->decl.attrs.is_inline) {
                                REPORT(ERROR, decl.loc, "Declaration cannot be inline");
                            }

                            n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;

                            if (tokens_get(s)->type == '{') {
                                sym->current = s->current;
                                sym->terminator = '}';

                                tokens_next(s);

                                int depth = 1;
                                while (depth) {
                                    Token* t = tokens_get(s);

                                    if (t->type == '\0') {
                                        REPORT(ERROR, decl.loc, "Declaration ended in EOF");

                                        // restore the token stream
                                        s->current = sym->current + 1;
                                        goto skip_declaration;
                                    } else if (t->type == '{') {
                                        depth++;
                                    } else if (t->type == ';' && depth == 1) {
                                        REPORT(ERROR, tokens_get_location_index(s), "Spurious semicolon");
                                        goto skip_declaration;
                                    } else if (t->type == '}') {
                                        if (depth == 0) {
                                            REPORT(ERROR, decl.loc, "Unbalanced brackets");
                                            goto skip_declaration;
                                        }

                                        depth--;
                                    }

                                    tokens_next(s);
                                }
                            } else {
                                // '=' EXPRESSION ','
                                // '=' EXPRESSION ';'
                                sym->current = s->current;
                                sym->terminator = ';';

                                int depth = 1;
                                while (depth) {
                                    Token* t = tokens_get(s);

                                    if (t->type == '\0') {
                                        REPORT(ERROR, decl.loc, "Declaration was never closed");

                                        // restore the token stream
                                        s->current = sym->current + 1;
                                        goto skip_declaration;
                                    } else if (t->type == '(') {
                                        depth++;
                                    } else if (t->type == ')') {
                                        depth--;

                                        if (depth == 0) {
                                            REPORT(ERROR, decl.loc, "Unbalanced parenthesis");

                                            s->current = sym->current + 1;
                                            goto skip_declaration;
                                        }
                                    } else if (t->type == ';' || t->type == ',') {
                                        if (depth > 1 && t->type == ';') {
                                            REPORT(ERROR, decl.loc, "Declaration's expression has a weird semicolon");
                                            goto skip_declaration;
                                        } else if (depth == 1) {
                                            sym->terminator = t->type;
                                            depth--;
                                        }
                                    }

                                    tokens_next(s);
                                }

                                // we ate the terminator but the code right below it
                                // does need to know what it is...
                                tokens_prev(s);
                            }
                        } else if (tokens_get(s)->type == '{') {
                            // function bodies dont end in semicolon or comma, it just terminates
                            // the declaration list
                            requires_terminator = false;

                            if (decl.type->kind != KIND_FUNC) {
                                REPORT(ERROR, decl.loc, "Somehow parsing a function body... on a non-function type?");
                            }

                            if (old_definition && old_definition->current != 0) {
                                report_two_spots(REPORT_ERROR, tu->errors, s, decl.loc, old_definition->stmt->loc,
                                    "Cannot redefine function declaration",
                                    NULL, NULL, "previous definition was:");
                            }

                            if (sym->name[0] == 'm' && strcmp(sym->name, "main") == 0) {
                                tu->entrypoint_status = CUIK_ENTRYPOINT_MAIN;
                            } else if (sym->name[0] == 'W' && strcmp(sym->name, "WinMain") == 0) {
                                tu->entrypoint_status = CUIK_ENTRYPOINT_WINMAIN;
                            }

                            //n->decl.type = decl.type;
                            //n->decl.attrs = attr;
                            n->decl.attrs.is_root = attr.is_tls || !(attr.is_static || attr.is_inline);

                            sym->terminator = '}';
                            sym->current = s->current;
                            tokens_next(s);

                            // we postpone parsing the function bodies
                            // balance some brackets: '{' SOMETHING '}'
                            int depth = 1;
                            while (depth) {
                                Token* t = tokens_get(s);

                                if (t->type == '\0') {
                                    SourceLocIndex l = tokens_get_last_location_index(s);
                                    report_fix(REPORT_ERROR, tu->errors, s, l, "}", "Function body ended in EOF");

                                    s->current = sym->current + 1;
                                    goto skip_declaration;
                                } else if (t->type == '{') {
                                    depth++;
                                } else if (t->type == '}') {
                                    depth--;
                                }

                                tokens_next(s);
                            }
                        } else {
                            if (decl.type->kind != KIND_FUNC) {
                                if (n->decl.attrs.is_inline) {
                                    REPORT(ERROR, decl.loc, "Declaration cannot be inline");
                                }

                                n->decl.attrs.is_root = !attr.is_extern && !attr.is_static;
                            }
                        }

                        if (!requires_terminator) {
                            // function bodies just end the declaration list
                            break;
                        }

                        if (tokens_get(s)->type == 0) {
                            REPORT(ERROR, loc, "declaration list ended with EOF instead of semicolon.");
                            break;
                        } else if (tokens_get(s)->type == ';') {
                            tokens_next(s);
                            break;
                        } else if (tokens_get(s)->type == ',') {
                            tokens_next(s);
                            continue;
                        } else if (possibly_bad_decl) {
                            REPORT(ERROR, loc, "Bad declaration");
                            break;
                        }
                    }

                    skip_declaration:;
                }
            }
        }
    }
    out_of_order_mode = false;

    // synchronize the thread symbol tables
    s_global_symbols = tu->global_symbols;
    s_global_tags = tu->global_tags;

    if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;

    // Phase 2: resolve top level types, layout records and anything else so that
    // we have a complete global symbol table
    CUIK_TIMED_BLOCK("phase 2") {
        ////////////////////////////////
        // first we wanna check for cycles
        ////////////////////////////////
        size_t type_count = 0;
        for (ArenaSegment* a = tu->type_arena.base; a != NULL; a = a->next) {
            for (size_t used = 0; used < a->used; used += sizeof(Cuik_Type)) {
                Cuik_Type* type = (Cuik_Type*)&a->data[used];
                if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                    type->ordinal = type_count++;
                } else if (type->kind == KIND_PLACEHOLDER) {
                    REPORT(ERROR, type->loc, "could not find type '%s'!", type->placeholder.name);
                }
            }
        }

        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;

        // bitvectors amirite
        size_t bitvec_bytes = (type_count + 7) / 8;
        uint8_t* visited = tls_push(bitvec_bytes);
        uint8_t* finished = tls_push(bitvec_bytes);

        memset(visited, 0, bitvec_bytes);
        memset(finished, 0, bitvec_bytes);

        // for each type, check for cycles
        for (ArenaSegment* a = tu->type_arena.base; a != NULL; a = a->next) {
            for (size_t used = 0; used < a->used; used += sizeof(Cuik_Type)) {
                Cuik_Type* type = (Cuik_Type*)&a->data[used];

                if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
                    // if cycles... quit lmao
                    if (type_cycles_dfs(tu, type, visited, finished)) goto parse_error;
                }
            }
        }

        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;

        // parse all global declarations
        nl_strmap_for(i, tu->global_symbols) {
            Symbol* sym = &tu->global_symbols[i];

            if (sym->current != 0 &&
                (sym->storage_class == STORAGE_STATIC_VAR || sym->storage_class == STORAGE_GLOBAL)) {
                // Spin up a mini parser here
                TokenStream mini_lex = *s;
                mini_lex.current = sym->current;

                // intitialize use list
                symbol_chain_start = symbol_chain_current = NULL;

                Expr* e;
                if (tokens_get(&mini_lex)->type == '@') {
                    // function literals are a Cuik extension
                    // TODO(NeGate): error messages
                    tokens_next(&mini_lex);

                    e = parse_function_literal(tu, &mini_lex, sym->type);
                } else if (tokens_get(&mini_lex)->type == '{') {
                    tokens_next(&mini_lex);

                    e = parse_initializer(tu, &mini_lex, NULL);
                } else {
                    e = parse_expr_l14(tu, &mini_lex);
                    expect(tu, &mini_lex, sym->terminator);
                }

                sym->stmt->decl.initial = e;

                // finalize use list
                sym->stmt->decl.first_symbol = symbol_chain_start;
            }
        }

        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;

        // do record layouts and shi
        for (ArenaSegment* a = tu->type_arena.base; a != NULL; a = a->next) {
            for (size_t used = 0; used < a->used; used += sizeof(Cuik_Type)) {
                Cuik_Type* type = (Cuik_Type*)&a->data[used];

                if (type->align == -1) {
                    // this means it's got a pending expression for an alignment
                    type_resolve_pending_align(tu, type);
                }

                if (type->size == 0) {
                    type_layout(tu, type, true);
                }
            }
        }

        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;
        dyn_array_destroy(pending_exprs);

        ////////////////////////////////
        // Resolve any static assertions
        ////////////////////////////////
        for (size_t i = 0, count = dyn_array_length(static_assertions); i < count; i++) {
            // Spin up a mini expression parser here
            size_t current_lex_pos = static_assertions[i];

            TokenStream mini_lex = *s;
            mini_lex.current = current_lex_pos;

            intmax_t condition = parse_const_expr(tu, &mini_lex);
            if (tokens_get(&mini_lex)->type == ',') {
                tokens_next(&mini_lex);

                Token* t = tokens_get(&mini_lex);
                if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
                    generic_error(tu, &mini_lex, "static assertion expects string literal");
                }
                tokens_next(&mini_lex);

                if (condition == 0) {
                    REPORT(ERROR, tokens_get_location_index(&mini_lex), "Static assertion failed: %.*s", (int)(t->end - t->start), t->start);
                }
            } else {
                if (condition == 0) {
                    REPORT(ERROR, tokens_get_location_index(&mini_lex), "Static assertion failed");
                }
            }
        }

        // we don't need to keep it afterwards
        tls_restore(visited);
    }
    dyn_array_destroy(static_assertions);

    // Phase 3: resolve all expressions or function bodies
    // This part is parallel because im the fucking GOAT
    CUIK_TIMED_BLOCK("phase 3") {
        // append any AST nodes we might've created in this thread
        arena_trim(&local_ast_arena);
        arena_append(&tu->ast_arena, &local_ast_arena);
        local_ast_arena = (Arena){0};

        // allocate the local symbol tables
        if (local_symbols == NULL) {
            local_symbols = realloc(local_symbols, sizeof(Symbol) * MAX_LOCAL_SYMBOLS);
            local_tags = realloc(local_tags, sizeof(TagEntry) * MAX_LOCAL_TAGS);
        }

        if (desc->thread_pool != NULL) {
            // disabled until we change the tables to arenas
            size_t count = nl_strmap_get_load(tu->global_symbols);
            size_t padded = (count + (PARSE_MUNCH_SIZE - 1)) & ~(PARSE_MUNCH_SIZE - 1);

            // passed to the threads to identify when things are done
            atomic_size_t tasks_remaining = (count + (PARSE_MUNCH_SIZE - 1)) / PARSE_MUNCH_SIZE;
            ParserTaskInfo* tasks = HEAP_ALLOC(sizeof(ParserTaskInfo) * tasks_remaining);

            size_t j = 0;
            for (size_t i = 0; i < padded; i += PARSE_MUNCH_SIZE) {
                size_t limit = i + PARSE_MUNCH_SIZE;
                if (limit > count) limit = count;

                ParserTaskInfo* task = &tasks[j++];
                *task = (ParserTaskInfo){
                    .tasks_remaining = &tasks_remaining,
                    .start = i,
                    .end = limit
                };

                // share these bad boys with the other threads, we manage them still they
                // just get limited access
                task->tu = tu;
                task->base_token_stream = s;

                CUIK_CALL(desc->thread_pool, submit, phase3_parse_task, task);
            }

            // since we can run more tasks on this thread (potentially those from other parser jobs)
            // until the thread pool we should dettach our copies of the global symbol table
            s_global_symbols = NULL;
            s_global_tags = NULL;

            // "highway robbery on steve jobs" job stealing amirite...
            while (tasks_remaining != 0) {
                CUIK_CALL(desc->thread_pool, work_one_job);
            }

            HEAP_FREE(tasks);
        } else {
            // single threaded mode
            parse_global_symbols(tu, 0, nl_strmap__get_header(tu->global_symbols)->load, *s);
        }

        // detach it because we're cool people
        tu->global_symbols = NULL;
        tu->global_tags = NULL;

        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;

        // check for any qualified types and resolve them correctly
        for (ArenaSegment* a = tu->type_arena.base; a != NULL; a = a->next) {
            for (size_t used = 0; used < a->used; used += sizeof(Cuik_Type)) {
                Cuik_Type* type = (Cuik_Type*)&a->data[used];

                if (type->kind == KIND_QUALIFIED_TYPE) {
                    SourceLocIndex loc = type->loc;
                    bool is_atomic = type->is_atomic;
                    bool is_const = type->is_const;
                    int align = type->align;
                    Atom aka = type->also_known_as;

                    Cuik_Type* based = type->qualified_ty;

                    // copy and replace the qualifier slots
                    memcpy(type, based, sizeof(Cuik_Type));
                    type->loc = loc;
                    type->also_known_as = aka;
                    type->based = based;
                    type->align = align;
                    type->is_const = is_const;
                    type->is_atomic = is_atomic;
                }
            }
        }

        dyn_array_destroy(tu->tokens.tokens);
    }

    #if 0
    printf("AST arena: %zu MB\n", arena_get_memory_usage(&tu->ast_arena) / (1024*1024));
    printf("Type arena: %zu MB\n", arena_get_memory_usage(&tu->type_arena) / (1024*1024));
    printf("Thread arena: %zu MB\n", arena_get_memory_usage(&thread_arena) / (1024*1024));
    atoms_dump_stats();
    #endif

    // output accumulated diagnostics
    nl_strmap_for(i, tu->unresolved_symbols) {
        Diag_UnresolvedSymbol* loc = tu->unresolved_symbols[i];
        report_header(REPORT_ERROR, "could not resolve symbol: %s", loc->name);

        DiagWriter d = diag_writer(&tu->tokens);
        for (; loc != NULL; loc = loc->next) {
            if (!diag_writer_is_compatible(&d, loc->loc)) {
                // end line
                diag_writer_done(&d);
                d = diag_writer(&tu->tokens);
            }

            diag_writer_highlight(&d, loc->loc);
        }
        diag_writer_done(&d);
        printf("\n");
    }

    // if we have unresolved symbols we can't type check
    if (nl_strmap_get_load(tu->unresolved_symbols) > 0) {
        goto parse_error;
    }

    // run type checker
    CUIK_TIMED_BLOCK("phase 4") {
        cuik__sema_pass(tu, NULL /* desc->thread_pool */);
        if (has_reports(REPORT_ERROR, tu->errors)) goto parse_error;
    }

    if (cuik_is_profiling()) cuik_profile_region_end();
    return tu;

    parse_error: {
        // free all translation unit resources because we failed :(
        dyn_array_destroy(pending_exprs);
        dyn_array_destroy(static_assertions);

        // free tokens
        dyn_array_destroy(tu->tokens.tokens);

        cuik_destroy_translation_unit(tu);
        if (cuik_is_profiling()) cuik_profile_region_end();
        return NULL;
    }
}

CUIK_API void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud) {
    void* old = tu->user_data;
    tu->user_data = ud;
    return old;
}

CUIK_API void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu) {
    return tu->user_data;
}

CUIK_API int cuik_acquire_translation_unit(TranslationUnit* restrict tu) {
    return atomic_fetch_add(&tu->ref_count, 1);
}

CUIK_API void cuik_release_translation_unit(TranslationUnit* restrict tu) {
    int r = --tu->ref_count;
    if (r == 0) {
        cuik_destroy_translation_unit(tu);
    }
}

CUIK_API void cuik_destroy_translation_unit(TranslationUnit* restrict tu) {
    dyn_array_destroy(tu->top_level_stmts);

    arena_free(&tu->ast_arena);
    arena_free(&tu->type_arena);
    mtx_destroy(&tu->arena_mutex);
    free(tu);
}

CUIK_API Cuik_ImportRequest* cuik_translation_unit_import_requests(TranslationUnit* restrict tu) {
    return tu->import_libs;
}

CUIK_API TranslationUnit* cuik_next_translation_unit(TranslationUnit* restrict tu) {
    return tu->next;
}

CUIK_API Cuik_TopLevelIter cuik_first_top_level_stmt(TranslationUnit* restrict tu) {
    return (Cuik_TopLevelIter){
        .limit_ = dyn_array_length(tu->top_level_stmts),
        .stmts_ = tu->top_level_stmts
    };
}

CUIK_API bool cuik_next_top_level_stmt(Cuik_TopLevelIter* it, int step) {
    size_t i = it->index_, limit = it->limit_;
    if (i >= limit) {
        return false;
    }

    if (i + step >= limit) {
        it->count = step - ((i + step) - limit);
    } else {
        it->count = step;
    }

    it->start = &it->stmts_[i];
    it->index_ = i + step;
    return true;
}

CUIK_API Stmt** cuik_get_top_level_stmts(TranslationUnit* restrict tu) {
    return tu->top_level_stmts;
}

CUIK_API size_t cuik_num_of_top_level_stmts(TranslationUnit* restrict tu) {
    return dyn_array_length(tu->top_level_stmts);
}

Stmt* resolve_unknown_symbol(TranslationUnit* tu, Expr* e) {
    Symbol* sym = find_global_symbol(tu, (char*)e->unknown_sym);
    if (sym != NULL) return 0;

    // Parameters are local and a special case how tf
    assert(sym->storage_class != STORAGE_PARAM);

    e->op = EXPR_SYMBOL;
    e->symbol = sym->stmt;
    return sym->stmt;
}

static Symbol* find_local_symbol(TokenStream* restrict s) {
    Token* t = tokens_get(s);
    const unsigned char* name = t->start;
    size_t length = t->end - t->start;

    // Try local variables
    size_t i = local_symbol_count;
    size_t start = local_symbol_start;
    while (i-- > start) {
        const char* sym = local_symbols[i].name;
        size_t sym_length = strlen(sym);

        if (sym_length == length && memcmp(name, sym, length) == 0) {
            return &local_symbols[i];
        }
    }

    return NULL;
}

////////////////////////////////
// STATEMENTS
////////////////////////////////
static void parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* n) {
    Cuik_Type* type = n->decl.type;

    Param* param_list = type->func.param_list;
    size_t param_count = type->func.param_count;

    assert(local_symbol_start == local_symbol_count);
    if (param_count >= INT16_MAX) {
        REPORT(ERROR, n->loc, "Function parameter count cannot exceed %d (got %d)", param_count, MAX_LOCAL_SYMBOLS);
        abort();
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

    // skip {
    tokens_next(s);

    cuik__sema_function_stmt = n;
    Stmt* body = parse_compound_stmt(tu, s);
    assert(body != NULL);
    cuik__sema_function_stmt = NULL;

    n->op = STMT_FUNC_DECL;
    n->decl.initial_as_stmt = body;

    // TODO(NeGate): redo the label look in a sec
    nl_strmap_free(labels);
}

static Stmt* parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s) {
    Stmt* node = NULL;

    LOCAL_SCOPE {
        node = make_stmt(tu, s, STMT_COMPOUND, sizeof(struct StmtCompound));

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

        node->end_loc = tokens_get_location_index(s);
        expect(tu, s, '}');

        Stmt** permanent_storage = arena_alloc(&thread_arena, kid_count * sizeof(Stmt*), _Alignof(Stmt*));
        memcpy(permanent_storage, kids, kid_count * sizeof(Stmt*));

        node->compound = (struct StmtCompound){
            .kids = permanent_storage,
            .kids_count = kid_count,
        };

        tls_restore(kids);
    }

    return node;
}

// TODO(NeGate): Doesn't handle declarators or expression-statements
static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s) {
    TknType peek = tokens_get(s)->type;

    if (peek == '{') {
        tokens_next(s);
        return parse_compound_stmt(tu, s);
    } else if (peek == TOKEN_KW_Static_assert) {
        tokens_next(s);
        expect(tu, s, '(');

        SourceLocIndex start = tokens_get_location_index(s);
        intmax_t condition = parse_const_expr(tu, s);
        SourceLocIndex end = tokens_get_last_location_index(s);

        if (tokens_get(s)->type == ',') {
            tokens_next(s);

            Token* t = tokens_get(s);
            if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
                generic_error(tu, s, "static assertion expects string literal");
            }
            tokens_next(s);

            if (condition == 0) {
                REPORT_RANGED(ERROR, start, end, "Static assertion failed! %.*s", (int) (t->end - t->start), t->start);
            }
        } else {
            if (condition == 0) {
                REPORT_RANGED(ERROR, start, end, "Static assertion failed!");
            }
        }

        expect(tu, s, ')');
        expect(tu, s, ';');

        // TAIL CALL
        return parse_stmt(tu, s);
    } else if (peek == TOKEN_KW_return) {
        tokens_next(s);

        Expr* e = 0;
        if (tokens_get(s)->type != ';') {
            e = parse_expr(tu, s);
        }

        Stmt* n = make_stmt(tu, s, STMT_RETURN, sizeof(struct StmtReturn));
        n->return_ = (struct StmtReturn){ .expr = e };

        expect_with_reason(tu, s, ';', "return");
        return n;
    } else if (peek == TOKEN_KW_if) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_IF, sizeof(struct StmtIf));

        LOCAL_SCOPE {
            Expr* cond;
            {
                SourceLocIndex opening_loc = tokens_get_location_index(s);
                expect(tu, s, '(');

                cond = parse_expr(tu, s);

                expect_closing_paren(tu, s, opening_loc);
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

            n->if_ = (struct StmtIf){
                .cond = cond,
                .body = body,
                .next = next,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_switch) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_SWITCH, sizeof(struct StmtSwitch));

        LOCAL_SCOPE {
            expect(tu, s, '(');
            Expr* cond = parse_expr(tu, s);
            expect(tu, s, ')');

            n->switch_ = (struct StmtSwitch){
                .condition = cond};

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
        return n;
    } else if (peek == TOKEN_KW_case) {
        // TODO(NeGate): error messages
        assert(current_switch_or_case);

        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_CASE, sizeof(struct StmtCase));
        Stmt* top = n;

        intmax_t key = parse_const_expr(tu, s);
        if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
            // GNU extension, case ranges
            tokens_next(s);
            intmax_t key_max = parse_const_expr(tu, s);
            expect(tu, s, ':');

            assert(key_max > key);
            n->case_.key = key;

            for (intmax_t i = key; i < key_max; i++) {
                Stmt* curr = make_stmt(tu, s, STMT_CASE, sizeof(struct StmtCase));
                curr->case_ = (struct StmtCase){.key = i + 1};

                // Append to list
                n->case_.next = n->case_.body = curr;
                n = curr;
            }
        } else {
            expect(tu, s, ':');

            n->case_ = (struct StmtCase){
                .key = key, .body = 0, .next = 0};
        }

        switch (current_switch_or_case->op) {
            case STMT_CASE:
            current_switch_or_case->case_.next = top;
            break;
            case STMT_DEFAULT:
            current_switch_or_case->default_.next = top;
            break;
            case STMT_SWITCH:
            current_switch_or_case->switch_.next = top;
            break;
            default:
            abort();
        }
        current_switch_or_case = n;

        Stmt* body = parse_stmt_or_expr(tu, s);
        n->case_.body = body;
        return top;
    } else if (peek == TOKEN_KW_default) {
        // TODO(NeGate): error messages
        assert(current_switch_or_case);

        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_DEFAULT, sizeof(struct StmtDefault));

        switch (current_switch_or_case->op) {
            case STMT_CASE:
            current_switch_or_case->case_.next = n;
            break;
            case STMT_DEFAULT:
            current_switch_or_case->default_.next = n;
            break;
            case STMT_SWITCH:
            current_switch_or_case->switch_.next = n;
            break;
            default:
            abort();
        }
        current_switch_or_case = n;
        expect(tu, s, ':');

        n->default_ = (struct StmtDefault){
            .body = 0, .next = 0,
        };

        Stmt* body = parse_stmt_or_expr(tu, s);
        n->default_.body = body;
        return n;
    } else if (peek == TOKEN_KW_break) {
        // TODO(NeGate): error messages
        assert(current_breakable);

        tokens_next(s);
        expect(tu, s, ';');

        Stmt* n = make_stmt(tu, s, STMT_BREAK, sizeof(struct StmtBreak));
        n->break_ = (struct StmtBreak){
            .target = current_breakable,
        };
        return n;
    } else if (peek == TOKEN_KW_continue) {
        // TODO(NeGate): error messages
        assert(current_continuable);

        tokens_next(s);
        expect(tu, s, ';');

        Stmt* n = make_stmt(tu, s, STMT_CONTINUE, sizeof(struct StmtContinue));
        n->continue_ = (struct StmtContinue){
            .target = current_continuable,
        };
        return n;
    } else if (peek == TOKEN_KW_while) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_WHILE, sizeof(struct StmtWhile));

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

            n->while_ = (struct StmtWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_for) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_FOR, sizeof(struct StmtFor));

        LOCAL_SCOPE {
            expect(tu, s, '(');

            // it's either nothing, a declaration, or an expression
            Stmt* first = NULL;
            if (tokens_get(s)->type == ';') {
                /* nothing */
                tokens_next(s);
            } else {
                // NOTE(NeGate): This is just a decl list or a single expression.
                first = make_stmt(tu, s, STMT_COMPOUND, sizeof(struct StmtCompound));

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

            n->for_ = (struct StmtFor){
                .first = first,
                .cond = cond,
                .body = body,
                .next = next,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_do) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_DO_WHILE, sizeof(struct StmtDoWhile));

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

                REPORT(ERROR, t->location, "%s:%d: error: expected 'while' got '%.*s'", (int)(t->end - t->start), t->start);
                abort();
            }
            tokens_next(s);

            expect(tu, s, '(');

            Expr* cond = parse_expr(tu, s);

            expect(tu, s, ')');
            expect(tu, s, ';');

            n->do_while = (struct StmtDoWhile){
                .cond = cond,
                .body = body,
            };
        }

        return n;
    } else if (peek == TOKEN_KW_goto) {
        tokens_next(s);
        Stmt* n = make_stmt(tu, s, STMT_GOTO, sizeof(struct StmtGoto));

        // read label name
        Token* t = tokens_get(s);
        SourceLocIndex loc = t->location;
        if (t->type != TOKEN_IDENTIFIER) {
            REPORT(ERROR, loc, "expected identifier for goto target name");
            return n;
        }

        Atom name = atoms_put(t->end - t->start, t->start);

        // skip to the semicolon
        tokens_next(s);

        Expr* target = make_expr(tu);
        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            *target = (Expr){
                .op = EXPR_SYMBOL,
                .start_loc = loc,
                .end_loc = loc,
                .symbol = labels[search],
            };
        } else {
            // not defined yet, make a placeholder
            Stmt* label_decl = make_stmt(tu, s, STMT_LABEL, sizeof(struct StmtLabel));
            label_decl->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, label_decl);

            *target = (Expr){
                .op = EXPR_SYMBOL,
                .start_loc = loc,
                .end_loc = loc,
                .symbol = label_decl,
            };
        }

        n->goto_ = (struct StmtGoto){
            .target = target,
        };

        expect(tu, s, ';');
        return n;
    } else if (peek == TOKEN_IDENTIFIER && tokens_peek(s)->type == TOKEN_COLON) {
        // label amirite
        // IDENTIFIER COLON STMT
        Token* t = tokens_get(s);
        Atom name = atoms_put(t->end - t->start, t->start);

        Stmt* n = NULL;
        ptrdiff_t search = nl_strmap_get_cstr(labels, name);
        if (search >= 0) {
            n = labels[search];
        } else {
            n = make_stmt(tu, s, STMT_LABEL, sizeof(struct StmtLabel));
            n->label = (struct StmtLabel){ .name = name };
            nl_strmap_put_cstr(labels, name, n);
        }

        n->label.placed = true;

        tokens_next(s);
        tokens_next(s);
        return n;
    } else {
        return 0;
    }
}

static void parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            generic_error(tu, s, "pragma declaration expects string literal");
        }
        tokens_next(s);

        expect(tu, s, ')');
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
    } else if (is_typename(tu, s)) {
        Attribs attr = {0};
        Cuik_Type* type = parse_declspec(tu, s, &attr);

        if (attr.is_typedef) {
            // don't expect one the first time
            bool expect_comma = false;
            while (tokens_get(s)->type != ';') {
                if (expect_comma) {
                    expect_with_reason(tu, s, ',', "typedef");
                } else expect_comma = true;

                Decl decl = parse_declarator(tu, s, type, false, false);
                assert(decl.name);

                // make typedef
                Stmt* n = make_stmt(tu, s, STMT_DECL, sizeof(struct StmtDecl));
                n->loc = decl.loc;
                n->decl = (struct StmtDecl){
                    .name = decl.name,
                    .type = decl.type,
                    .attrs = attr,
                };

                if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
                    REPORT(ERROR, decl.loc, "Local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
                    abort();
                }

                local_symbols[local_symbol_count++] = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .storage_class = STORAGE_TYPEDEF,
                };
            }

            expect(tu, s, ';');
        } else {
            // TODO(NeGate): Kinda ugly
            // don't expect one the first time
            bool expect_comma = false;
            while (tokens_get(s)->type != ';') {
                if (expect_comma) {
                    if (tokens_get(s)->type == '{') {
                        generic_error(tu, s, "nested functions are not allowed... yet");
                    } else if (tokens_get(s)->type != ',') {
                        SourceLocIndex loc = tokens_get_last_location_index(s);

                        report_fix(REPORT_ERROR, tu->errors, s, loc, ";", "expected semicolon at the end of declaration");
                    }

                    tokens_next(s);
                } else {
                    expect_comma = true;
                }

                Decl decl = parse_declarator(tu, s, type, false, false);

                Stmt* n = make_stmt(tu, s, STMT_DECL, sizeof(struct StmtDecl));
                n->loc = decl.loc;
                n->decl = (struct StmtDecl){
                    .type = decl.type,
                    .name = decl.name,
                    .attrs = attr,
                    .initial = 0,
                };

                if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
                    REPORT(ERROR, decl.loc, "Local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
                    abort();
                }

                local_symbols[local_symbol_count++] = (Symbol){
                    .name = decl.name,
                    .type = decl.type,
                    .storage_class = STORAGE_LOCAL,
                    .stmt = n,
                };

                Expr* initial = 0;
                if (tokens_get(s)->type == '=') {
                    tokens_next(s);

                    if (tokens_get(s)->type == '@') {
                        // function literals are a Cuik extension
                        // TODO(NeGate): error messages
                        tokens_next(s);
                        initial = parse_function_literal(tu, s, decl.type);
                    } else if (tokens_get(s)->type == '{') {
                        tokens_next(s);

                        initial = parse_initializer(tu, s, NULL);
                    } else {
                        initial = parse_expr_l14(tu, s);
                    }
                }

                n->decl.initial = initial;
                *((Stmt**)tls_push(sizeof(Stmt*))) = n;
                *body_count += 1;
            }

            expect(tu, s, ';');
        }
    } else {
        Stmt* n = make_stmt(tu, s, STMT_EXPR, sizeof(struct StmtExpr));
        Expr* expr = parse_expr(tu, s);

        n->expr = (struct StmtExpr){ .expr = expr };

        *((Stmt**)tls_push(sizeof(Stmt*))) = n;
        *body_count += 1;
        expect(tu, s, ';');
    }
}

static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s) {
    if (tokens_get(s)->type == TOKEN_KW_Pragma) {
        tokens_next(s);
        expect(tu, s, '(');

        if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
            generic_error(tu, s, "pragma declaration expects string literal");
        }
        tokens_next(s);

        expect(tu, s, ')');
        return 0;
    } else if (tokens_get(s)->type == ';') {
        tokens_next(s);
        return 0;
    } else {
        Stmt* stmt = parse_stmt(tu, s);

        if (stmt) {
            return stmt;
        } else {
            Stmt* n = make_stmt(tu, s, STMT_EXPR, sizeof(struct StmtExpr));

            Expr* expr = parse_expr(tu, s);
            n->expr = (struct StmtExpr){ .expr = expr };

            expect(tu, s, ';');
            return n;
        }
    }
}

static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s) {
    SourceLocIndex foo = tokens_get_location_index(s);
    Expr* folded = cuik__optimize_ast(tu, parse_expr_l14(tu, s));
    if (folded->op != EXPR_INT) {
        ((void) foo);
        REPORT_EXPR(ERROR, folded, "Could not parse expression as constant.");
        return 1;
    }

    return (intmax_t) folded->int_num.num;
}

////////////////////////////////
// ERRORS
////////////////////////////////
static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg) {
    SourceLocIndex loc = tokens_get_location_index(s);

    report(REPORT_ERROR, NULL, s, loc, msg);
    abort();
}

static void expect(TranslationUnit* tu, TokenStream* restrict s, char ch) {
    if (tokens_get(s)->type != ch) {
        Token* t = tokens_get(s);
        SourceLocIndex loc = tokens_get_last_location_index(s);

        char tmp[2] = { ch };
        report_fix(REPORT_ERROR, NULL, s, loc, tmp, "expected '%c' got '%.*s'", ch, (int)(t->end - t->start), t->start);
        abort();
    }

    tokens_next(s);
}

static void expect_closing_paren(TranslationUnit* tu, TokenStream* restrict s, SourceLocIndex opening) {
    if (tokens_get(s)->type != ')') {
        SourceLocIndex loc = tokens_get_location_index(s);

        report_two_spots(REPORT_ERROR, tu->errors, s, opening, loc,
            "expected closing parenthesis",
            "open", "close?", NULL
        );
        return;
    }

    tokens_next(s);
}

static void expect_with_reason(TranslationUnit* tu, TokenStream* restrict s, char ch, const char* reason) {
    if (tokens_get(s)->type != ch) {
        SourceLocIndex loc = tokens_get_last_location_index(s);

        char fix[2] = { ch, '\0' };
        report_fix(REPORT_ERROR, NULL, s, loc, fix, "expected '%c' for %s", ch, reason);
        return;
    }

    tokens_next(s);
}
