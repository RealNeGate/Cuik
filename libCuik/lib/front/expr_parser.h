////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
// This file is included into parser.h, it's parser of the parser module and is
// completely static
static Expr* parse_expr_assign(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s);
static Expr* parse_expr(TranslationUnit* tu, TokenStream* restrict s);

static InitNode* make_init_node(TokenStream* restrict s, int mode) {
    InitNode* n = ARENA_ALLOC(&local_ast_arena, InitNode);
    *n = (InitNode){ .mode = mode };
    return n;
}

// new tail
static InitNode* append_to_init_list(TokenStream* restrict s, InitNode* parent, InitNode* tail, InitNode* elem) {
    if (tail != NULL) {
        tail->next = elem;
    } else {
        parent->kid = elem;
    }

    parent->kids_count += 1;
    return elem;
}
