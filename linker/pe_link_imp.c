////////////////////////////////
// Import file
////////////////////////////////
// These are small files which mostly just hold an import name, DLL path and an
// ordinal (which is optional but helpful i think?)
static bool fetch_imp_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    COFF_ImportHeader import;
    memcpy(&import, prefetch.data, sizeof(import));
    return prefetch.length > sizeof(COFF_ImportHeader) + import.size_of_data;
}

static void process_imp_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    COFF_ImportHeader import;
    memcpy(&import, &prefetch.data[0], sizeof(import));

    size_t import_size = sizeof(COFF_ImportHeader) + import.size_of_data;
    assert(import_size <= prefetch.length);

    const char* imported_symbol = (const char*) &prefetch.data[sizeof(COFF_ImportHeader)];
    const char* dll_path = (const char*) &prefetch.data[sizeof(COFF_ImportHeader) + strlen(imported_symbol) + 1];

    TB_Slice import_name = { 0 };
    if (import.name_type == 3) {
        // for odd reasons windows has some symbols with @ and underscores (C++ amirite)
        // and we have to strip them out.
        const char* leading = imported_symbol;
        const char* at = strchr(imported_symbol, '@');
        if (at == NULL) at = imported_symbol + strlen(imported_symbol);

        for (const char* s = imported_symbol; s != at; s++) {
            if (*s == '_') leading = s+1;
        }

        import_name.length = at - leading;
        import_name.data   = (const uint8_t*) leading;
    } else {
        import_name.length = strlen(imported_symbol);
        import_name.data   = (const uint8_t*) imported_symbol;
    }

    // Create import table early, but we'll only populate it during the marking phase
    ImportTable* table = tb_arena_alloc(&linker_perm_arena, sizeof(ImportTable));
    *table = (ImportTable){ .libpath = { (const uint8_t*) dll_path, strlen(dll_path) } };
    mtx_init(&table->lock, mtx_plain);

    ImportTable* old = namehs_intern(&l->imports, table);
    if (old != table) {
        tb_arena_free(&linker_perm_arena, table, sizeof(ImportTable));
        table = old;
    }

    // first time we're importing this symbol, swag
    // make __imp_ form which refers to raw address
    size_t newlen = import_name.length + sizeof("__imp_") - 1;
    uint8_t* newstr = tb_arena_alloc(&linker_perm_arena, newlen);
    memcpy(newstr, "__imp_", sizeof("__imp_"));
    memcpy(newstr + sizeof("__imp_") - 1, import_name.data, import_name.length);
    newstr[newlen] = 0;

    TB_LinkerSymbol* import_sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
    *import_sym = (TB_LinkerSymbol){
        .name   = { newstr, newlen },
        .tag    = TB_LINKER_SYMBOL_IMPORT,
        .import = { .table = table, .ordinal = import.ordinal_hint }
    };

    TB_LinkerSymbol* new_sym = tb_linker_symbol_insert(l, import_sym, true);
    if (new_sym == import_sym) {
        // make the thunk-like symbol
        TB_LinkerSymbol* sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
        *sym = (TB_LinkerSymbol){
            .name   = import_name,
            .tag    = TB_LINKER_SYMBOL_THUNK,
            .thunk  = import_sym
        };
        tb_linker_symbol_insert(l, sym, true);
    }
}
