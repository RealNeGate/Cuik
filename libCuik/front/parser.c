// TODO list
//   - make sure all extensions are errors on pedantic mode
//
//   - enable some sort of error recovery when we encounter bad syntax within a
//   statement, basically just find the semicolon and head out :p
//
//   - ugly ass code
#include "parser.h"
#include "../targets/targets.h"

// winnt.h loves including garbage
#undef VOID

// HACK: this is defined in sema.c but because we sometimes call semantics stuff in the
// parser we need access to it to avoid some problems
extern thread_local Stmt* cuik__sema_function_stmt;

static const Cuik_Warnings DEFAULT_WARNINGS = { 0 };

// how big are the phase2 parse tasks
#define PARSE_MUNCH_SIZE (131072)

typedef struct {
    enum {
        PENDING_ALIGNAS,
        PENDING_BITWIDTH,
    } mode;
    int* dst;

    Cuik_Type* type;
    size_t start, end;
} PendingExpr;

// Global symbol stuff
thread_local static DynArray(PendingExpr) pending_exprs;
thread_local static NL_Strmap(Stmt*) labels;

thread_local static Stmt* current_switch_or_case;
thread_local static Stmt* current_breakable;
thread_local static Stmt* current_continuable;

// we build a chain in that lets us know what symbols are used by a function
thread_local static Cuik_Expr* symbol_chain_start;

static bool expect_char(TokenStream* restrict s, char ch);
static bool expect_closing_paren(TokenStream* restrict s, SourceLoc opening);
static bool expect_with_reason(TokenStream* restrict s, char ch, const char* reason);
static Symbol* find_symbol(Cuik_Parser* parser, TokenStream* restrict s);
static Cuik_Type* find_tag(Cuik_Parser* parser, Cuik_Atom name);

static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s);
static bool parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static Cuik_QualType parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* restrict tu, TokenStream* restrict s, Cuik_QualType type, bool is_abstract);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static bool is_typename(Cuik_Parser* restrict parser, TokenStream* restrict s);
static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg);

// Usage:
//  LOCAL_SCOPE {
//      /* do parse work */
//  }
#define LOCAL_SCOPE \
for (int i = (cuik_scope_open(parser->symbols), cuik_scope_open(parser->tags), 0); i != 1; i = 1, cuik_scope_close(parser->tags), cuik_scope_close(parser->symbols))

static int align_up(int a, int b) {
    if (b == 0) return 0;

    return a + (b - (a % b)) % b;
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

    // our basic bitch expectations of how this function will turn out.
    // it might flip on us but that's alright
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

// potential typedef conflicts
typedef struct TypeConflict {
    struct TypeConflict* next;
    Symbol *old_def, *new_def;
} TypeConflict;

struct Cuik_Parser {
    Cuik_Version version;
    TokenStream tokens;

    const Cuik_Target* target;
    // this refers to the `int` type, it comes from the target
    // but it's more convenient to access it from here.
    Cuik_Type* default_int;
    int pointer_byte_size;

    //  generated from #pragma comment(lib, "somelib.lib")
    // it's a linked list
    Cuik_ImportRequest* import_libs;
    DynArray(int) static_assertions;

    // out-of-order parsing is only done while in global scope
    bool is_in_global_scope;

    // there's 2 namespaces in C
    Cuik_SymbolTable* symbols;
    Cuik_SymbolTable* tags;

    TB_Arena* arena;
    DynArray(Stmt*) top_level_stmts;
    Cuik_TypeTable types;

    Cuik_Type* va_list;
    Cuik_Type* first_placeholder;
    TypeConflict* first_conflict;

    // used when expression building
    Cuik_Expr* expr;

    struct {
        // these are unique entries in the string interner so
        // we can cache it here
        #define X(name) Atom name;
        #include "glsl_keywords.h"
    } glsl;

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
    Diag_UnresolvedSymbol* d = TB_ARENA_ALLOC(parser->arena, Diag_UnresolvedSymbol);
    d->next = NULL;
    d->name = name;
    d->loc = (SourceRange){ loc, { loc.raw + strlen(name) } };

    // mtx_lock(&parser->diag_mutex);
    ptrdiff_t search = nl_map_get_cstr(parser->unresolved_symbols, name);
    if (search < 0) {
        nl_map_puti_cstr(parser->unresolved_symbols, name, search);
        parser->unresolved_symbols[search].v = d;
    } else {
        Diag_UnresolvedSymbol* old = parser->unresolved_symbols[search].v;
        while (old->next != NULL) old = old->next;

        old->next = d;
    }
    // mtx_unlock(&parser->diag_mutex);
}

static bool expect_char(TokenStream* restrict s, char ch) {
    if (tokens_eof(s)) {
        tokens_prev(s);

        diag_err(s, tokens_get_range(s), "expected '%c', got end-of-file", ch);
        return false;
    } else if (tokens_get(s)->type != ch) {
        diag_err(s, tokens_get_range(s), "expected '%c', got '%!S'", ch, tokens_get(s)->content);
        return false;
    } else {
        tokens_next(s);
        return true;
    }
}

// tls -> arena
static void* copy_out_temporary(TB_Arena* arena, void* src, size_t count, size_t size) {
    if (count == 0) {
        return NULL;
    }

    void* dst = tb_arena_alloc(arena, size * count);
    memcpy(dst, src, size * count);
    tls_restore(src);
    return dst;
}

// in glsl_parser.h
static Cuik_Type* parse_glsl_type(Cuik_Parser* restrict parser, TokenStream* restrict s);
static Cuik_GlslQuals* parse_glsl_qualifiers(Cuik_Parser* restrict parser, TokenStream* restrict s, Cuik_Qualifiers* quals);

#define THROW_IF_ERROR() if ((r = cuikdg_error_count(s)) > 0) return (Cuik_ParseResult){ r };

#include "expr_fold.h"
#include "expr_parser.h"
#include "decl_parser.h"
#include "stmt_parser.h"
#include "glsl_parser.h"
#include "top_level_parser.h"

void type_layout2(Cuik_Parser* restrict parser, TokenStream* restrict tokens, Cuik_Type* type) {
    if (CUIK_TYPE_IS_COMPLETE(type)) return;
    if (CUIK_TYPE_IS_PROGRESS(type)) {
        diag_err(tokens, type->loc, "Type has a circular dependency");
        return;
    }

    type->flags |= CUIK_TYPE_FLAG_PROGRESS;

    if (type->kind == KIND_CLONE) {
        type_layout2(parser, tokens, type->clone.of);

        Atom name = type->also_known_as;
        *type = *type->clone.of;
        type->also_known_as = name;
    } else if (type->kind == KIND_ARRAY) {
        if (type->array.count_lexer_pos) {
            assert(parser != NULL && "Parserless type checker!!!");

            // run mini parser for array count
            TokenStream mini_lex = *tokens;
            mini_lex.list.current = type->array.count_lexer_pos;
            type->array.count = parse_const_expr(parser, &mini_lex);
            expect_char(&mini_lex, ']');
        }

        // layout crap
        if (type->array.count != 0) {
            if (cuik_canonical_type(type->array.of)->size == 0) {
                type_layout2(parser, tokens, cuik_canonical_type(type->array.of));
            }

            if (cuik_canonical_type(type->array.of)->size == 0) {
                diag_err(tokens, type->loc, "could not resolve type (ICE)");
                return;
            }
        }

        uint64_t result = cuik_canonical_type(type->array.of)->size * type->array.count;

        // size checks
        if (result >= INT32_MAX) {
            diag_err(tokens, type->loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX or %zi)", result, result);
        }

        type->size = result;
        type->align = cuik_canonical_type(type->array.of)->align;
    } else if (type->kind == KIND_ENUM) {
        int cursor = 0;

        for (int i = 0; i < type->enumerator.count; i++) {
            // if the value is undecided, best time to figure it out is now
            if (type->enumerator.entries[i].lexer_pos != 0) {
                // Spin up a mini expression parser here
                TokenStream mini_lex = *tokens;
                mini_lex.list.current = type->enumerator.entries[i].lexer_pos;

                cursor = parse_const_expr(parser, &mini_lex);
                type->enumerator.entries[i].value = cursor;
                type->enumerator.entries[i].lexer_pos = 0;
                type->enumerator.entries[i].init = true;
            }

            if (type->enumerator.entries[i].init) {
                cursor = type->enumerator.entries[i].value;
            } else {
                type->enumerator.entries[i].value = cursor;
            }
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
                diag_err(tokens, type->loc, "cannot put function types into a struct, try a function pointer");
            } else {
                type_layout2(parser, tokens, cuik_canonical_type(member->type));
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
                    diag_err(tokens, type->loc, "bitfield cannot fit in this type.");
                }

                if (current_bit_offset + bit_width > bits_in_region) {
                    current_bit_offset = 0;

                    offset = align_up(offset + member_size, member_align);
                }

                member->bit_offset = current_bit_offset;
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

        // if it's a bitfield, we should finalize that field
        if (current_bit_offset) {
            offset += 1;
        }

        offset = align_up(offset, align);
        type->align = align;
        type->size = offset;
    }

    type->flags |= CUIK_TYPE_FLAG_COMPLETE;
    type->flags &= ~CUIK_TYPE_FLAG_PROGRESS;
}

void* cuik_set_translation_unit_user_data(TranslationUnit* restrict tu, void* ud) {
    void* old = tu->user_data;
    tu->user_data = ud;
    return old;
}

void* cuik_get_translation_unit_user_data(TranslationUnit* restrict tu) {
    return tu->user_data;
}

void cuik_set_tu_ordinal(TranslationUnit* restrict tu, int ordinal) {
    tu->local_ordinal = ordinal;
}

int cuik_get_tu_ordinal(TranslationUnit* restrict tu) {
    return tu->local_ordinal;
}

void cuik_destroy_translation_unit(TranslationUnit* restrict tu) {
    if (!tu->is_free) {
        tu->is_free = true;
        dyn_array_destroy(tu->top_level_stmts);
    }

    if (tu->parent == NULL) {
        cuik_free(tu);
    }
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

static Symbol* find_symbol(Cuik_Parser* parser, TokenStream* restrict s) {
    Token* t = tokens_get(s);
    return cuik_symtab_lookup(parser->symbols, atoms_put(t->content.length, t->content.data));
}

////////////////////////////////
// ERRORS
////////////////////////////////
static _Noreturn void generic_error(TranslationUnit* tu, TokenStream* restrict s, const char* msg) {
    SourceLoc loc = tokens_get_location(s);

    report(REPORT_ERROR, s, loc, msg);
    abort();
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
