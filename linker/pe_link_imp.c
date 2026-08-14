////////////////////////////////
// Import file
////////////////////////////////
static const char BIG_OBJ_MAGIC[] = {
    '\xc7', '\xa1', '\xba', '\xd1', '\xee', '\xba', '\xa9', '\x4b',
    '\xaf', '\x20', '\xfa', '\xf6', '\x6a', '\xa4', '\xdc', '\xb8',
};

typedef struct {
    uint16_t sig1;
    uint16_t sig2;
    uint16_t machine;
    uint32_t timestamp;

    uint8_t  uuid[16];
    uint32_t padding[4];

    uint32_t section_count;
    uint32_t symbol_table;
    uint32_t symbol_count;
} COFF_BigHeader;
_Static_assert(sizeof(COFF_BigHeader) == 56, "WOAH");

// These are small files which mostly just hold an import name, DLL path and an
// ordinal (which is optional but helpful i think?)
static bool fetch_imp_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    COFF_ImportHeader import;
    memcpy(&import, prefetch.data, sizeof(import));

    if (prefetch.length < 12+16) {
        // must be an import, too small for anything else
        return true;
    }

    // Check UUID, if it matches we're gonna transition to parsing an object file
    if (memcmp(BIG_OBJ_MAGIC, &prefetch.data[12], 16) == 0) {
        TB_ASSERT(prefetch.length >= sizeof(COFF_BigHeader));

        COFF_BigHeader header;
        memcpy(&header, prefetch.data, sizeof(header));
        size_t size_of_section_headers = header.section_count * sizeof(COFF_SectionHeader);
        size_t symstr_table_size = obj->size - header.symbol_table;

        obj->is_big = true;
        obj->symbol_count     = header.symbol_count;
        obj->symbol_table_pos = header.symbol_table;
        obj->section_count    = header.section_count;
        obj->process          = process_obj_file;

        obj->io_rem = 2;
        obj->sections = tb_linker_moar_mem(obj, size_of_section_headers);
        tb_linker_read_req(l, file_header_offset + sizeof(header), size_of_section_headers, obj->sections, obj);

        obj->symbol_table = tb_linker_moar_mem(obj, symstr_table_size);
        tb_linker_read_req(l, file_header_offset + header.symbol_table, symstr_table_size, obj->symbol_table, obj);
        return false;
    }

    size_t import_size = sizeof(COFF_ImportHeader) + import.size_of_data;
    if (prefetch.length < import_size) {
        #if 0
        // read request
        obj->io_rem = 1;
        obj->file_bottom = tb_linker_moar_mem(obj, import_size);
        tb_linker_read_req(l, file_header_offset, import_size, obj->file_bottom, obj);
        return false;
        #endif

        log_warn("Didn't load object %.*s", (int) obj->name.length, obj->name.data);
        tb_linker_job_done(l);
        return false;
    }

    // prefetch was enough
    return true;
}

static void process_imp_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    COFF_ImportHeader import;
    memcpy(&import, &prefetch.data[0], sizeof(import));

    size_t import_size = sizeof(COFF_ImportHeader) + import.size_of_data;
    assert(prefetch.length >= import_size);

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
