// static ParseResult parse_glsl_decl(Cuik_Parser* restrict parser, TokenStream* restrict s) {}

Cuik_ParseResult cuikparse_run_glsl(Cuik_ParseVersion version, TokenStream* restrict s, Cuik_Target* target) {
    int r;
    Cuik_Parser parser = { 0 };
    parser.version = version;
    parser.tokens = *s;
    parser.target = target;
    parser.static_assertions = dyn_array_create(int, 2048);
    parser.types = init_type_table();

    // just a shorthand so it's faster to grab
    parser.default_int = (Cuik_Type*) &target->signed_ints[CUIK_BUILTIN_INT];
    parser.is_in_global_scope = true;
    parser.top_level_stmts = dyn_array_create(Stmt*, 1024);

    if (pending_exprs) {
        dyn_array_clear(pending_exprs);
    } else {
        pending_exprs = dyn_array_create(PendingExpr, 1024);
    }

    // Phase 1: resolve all top level statements
    CUIK_TIMED_BLOCK("phase 1") {
        while (!tokens_eof(s)) {
            // skip any top level "null" statements
            while (tokens_get(s)->type == ';') tokens_next(s);

            // if (parse_glsl_decl(&parser, s) != 0) continue;

            diag_err(s, tokens_get_range(s), "could not parse top level statement");
            tokens_next(s);
        }

        parser.is_in_global_scope = false;
    }
    THROW_IF_ERROR();

    return (Cuik_ParseResult){ 0 };
}
