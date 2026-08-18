
typedef struct {
    TB_ObjectSymbol* sym;
    TB_LinkerSectionPiece* p;
    COFF_AuxSectionSymbol* aux;
} PendingCOMDAT;

static void pe_linker_parse_directives(TPool* pool, void** args);
static bool pe_linker_parse_import(TB_Linker* l, TB_Slice content);

static void pe_linker_parse_directives_io(TB_Linker* l, void* arg, TB_Slice content, bool io_thread) {
    void* args[3] = { l, (uint8_t*) content.data, (uint8_t*) content.data + content.length };
    if (io_thread) {
        tpool_io_forward(l->jobs.pool, true, pe_linker_parse_directives, 3, args);
    } else {
        tpool_add_task2(l->jobs.pool, pe_linker_parse_directives, 3, args);
    }
}

static int obj_symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static bool fetch_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    assert(prefetch.length >= sizeof(COFF_FileHeader));
    BCache_File* file = obj->file;

    // Object file:
    //   Load symbol table and string table (which takes up the remainder of the
    //   file after the symbols).
    COFF_FileHeader header;
    memcpy(&header, prefetch.data, sizeof(header));
    size_t size_of_section_headers = header.section_count * sizeof(COFF_SectionHeader);
    size_t symstr_table_size = obj->size - header.symbol_table;

    obj->symbol_count     = header.symbol_count;
    obj->symbol_table_pos = header.symbol_table;
    obj->section_count    = header.section_count;
    assert(sizeof(COFF_FileHeader) + size_of_section_headers <= obj->size);

    if (sizeof(COFF_FileHeader) + size_of_section_headers <= prefetch.length) {
        // File header & section headers fit within the prefetch? cool, don't
        // read more than we need then. We should tune these factors later
        obj->io_rem = 1;
        obj->sections = (uint8_t*) &prefetch.data[sizeof(COFF_FileHeader)];
    } else {
        obj->io_rem = 2;
        tb_linker_read_req(l, file, file_header_offset + sizeof(COFF_FileHeader), size_of_section_headers, (void**) &obj->sections, obj, NULL);
    }

    tb_linker_read_req(l, file, file_header_offset + header.symbol_table, symstr_table_size, (void**) &obj->symbol_table, obj, NULL);
    return false;
}

static TB_LinkerSectionPiece UNKNOWN_LEADER;
static void process_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    BCache_File* file = obj->file;

    size_t symbol_size      = obj->is_big ? sizeof(COFF_BigSymbol) : sizeof(COFF_Symbol);
    TB_COFF_Parser parser   = { obj->name, .is_big = obj->is_big };
    size_t string_table_pos = obj->symbol_table_pos + obj->symbol_count*symbol_size;

    parser.symbol_count     = obj->symbol_count;
    parser.symbol_table_pos = obj->symbol_table_pos;
    parser.section_count    = obj->section_count;
    parser.symbol_table = (TB_Slice){
        .length = obj->symbol_count * symbol_size,
        .data   = obj->symbol_table,
    };
    parser.string_table = (TB_Slice){
        .length = obj->size - string_table_pos,
        .data   = &obj->symbol_table[parser.symbol_table.length],
    };

    COFF_SectionHeader* sections = (COFF_SectionHeader*) obj->sections;

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_LinkerSectionPiece** sec2piece = tb_linker_alloc_local(parser.section_count * sizeof(TB_LinkerSectionPiece*));
    COFF_AuxSectionSymbol** comdat_sections = tb_linker_alloc_local(parser.section_count * sizeof(COFF_AuxSectionSymbol*));

    uint64_t order = obj->time;
    TB_LinkerSymbol** symbol_map = tb_arena_alloc(&linker_perm_arena, parser.symbol_count * sizeof(TB_LinkerSymbol*));

    uint8_t* raw_map = obj->file->raw_map;
    TB_LinkerSection* last_sec = NULL;
    CUIK_TIMED_BLOCK("parse sections") {
        FOR_N(i, 0, parser.section_count) {
            COFF_SectionHeader* sec = &sections[i];

            comdat_sections[i] = NULL;

            TB_Slice s_name = tb_coff_section_name(&parser, sec);
            int dollar = find_char(s_name, '$');
            size_t drectve_len = sizeof(".drectve")-1;
            if (dollar >= drectve_len && memcmp(s_name.data, ".drectve", drectve_len) == 0) {
                sec2piece[i] = NULL;

                if (sec->raw_data_size != 0) {
                    // Fork out a parallel task
                    l->jobs.count += 1;
                    tb_linker_read_req(l, file, file_header_offset + sec->raw_data_pos, sec->raw_data_size, NULL, obj, pe_linker_parse_directives_io);
                }
                continue;
            }

            // remove all the alignment flags, they don't appear in linker sections
            uint32_t sec_flags = sec->characteristics;
            uint32_t flags = sec_flags & ~0x00F00000;

            if (last_sec == NULL || last_sec->name.length != s_name.length || memcmp(last_sec->name.data, s_name.data, s_name.length) != 0) {
                last_sec = tb_linker_find_or_create_section(l, s_name.length, (const char*) s_name.data, flags);
            }
            TB_LinkerSection* ls = last_sec;

            if (sec_flags & (IMAGE_SCN_LNK_REMOVE | IMAGE_SCN_MEM_DISCARDABLE)) {
                ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
            }

            size_t sec_vsize = sec->raw_data_size;
            if (sec_vsize < sec->misc.virtual_size) {
                sec_vsize = sec->misc.virtual_size;
            }

            TB_LinkerSectionPiece* p = tb_linker_append_piece(ls, PIECE_BSS, sec_vsize, obj);
            if ((sec_flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                p->kind        = PIECE_FILE;
                p->file_offset = file_header_offset + sec->raw_data_pos;
                p->buffer_size = sec->raw_data_size;
                p->file        = obj->file;
            }
            if (sec_flags & 0x00F00000) {
                // go stare at the table, it'll make sense
                p->align_log2 = ((sec_flags >> 20) & 0xF) - 1;
            }
            // broadcast to all sections how to find symbols (for relocation resolution later)
            if (p != NULL) {
                p->symbol_map = symbol_map;
            }
            sec2piece[i] = p;
            p->order = order + i;
            p->flags = (sec_flags & IMAGE_SCN_MEM_EXECUTE) ? TB_LINKER_PIECE_CODE : 0;
            p->reloc_count = sec->num_reloc;
            p->reloc_pos = file_header_offset + sec->pointer_to_reloc;
            p->reloc_size = sec->num_reloc * sizeof(COFF_ImageReloc);
            p->relocs = NULL;

            // Init for later
            p->comdat_parent = &UNKNOWN_LEADER;

            if (sec->num_reloc) {
                p->relocs = &raw_map[p->reloc_pos];
            }

            if (sec_flags & IMAGE_SCN_LNK_COMDAT) {
                p->flags |= TB_LINKER_PIECE_COMDAT;
            }
        }
    }

    // append all symbols
    size_t sym_count = 0;
    TB_ObjectSymbol* syms = tb_linker_alloc_local(parser.symbol_count * sizeof(TB_ObjectSymbol));

    static _Thread_local DynArray(PendingCOMDAT) pending_indices = NULL;
    static _Thread_local DynArray(TB_ObjectSymbol*) weak_syms = NULL;

    CUIK_TIMED_BLOCK("apply symbols") {
        size_t i = 0;
        while (i < parser.symbol_count) {
            TB_ObjectSymbol* sym = &syms[sym_count++];
            size_t c = tb_coff_parse_symbol(&parser, i, sym);
            TB_ASSERT(c > 0);

            TB_LinkerSymbol* s = NULL;
            if (sym->section_num > 0) {
                assert(sym->section_num <= parser.section_count);
                COFF_SectionHeader* sec = &sections[sym->section_num - 1];
                TB_LinkerSectionPiece* p = sec2piece[sym->section_num - 1];

                bool is_section = false;
                if (sym->type == TB_OBJECT_SYMBOL_STATIC && sym->value == 0) {
                    TB_Slice sec_name = tb_coff_section_name(&parser, sec);
                    if (sec_name.length == sym->name.length && memcmp(sec_name.data, sym->name.data, sym->name.length) == 0) {
                        is_section = true;

                        // COMDAT is how linkers handle merging of inline functions in C++
                        if ((sec->characteristics & IMAGE_SCN_LNK_COMDAT)) {
                            COFF_AuxSectionSymbol* comdat_aux = sym->extra;
                            if (comdat_aux->selection != 5) {
                                // next symbol in this section is the COMDAT symbol
                                comdat_sections[sym->section_num - 1] = sym->extra;
                            } else {
                                TB_ASSERT(p != NULL);
                                PendingCOMDAT pending = { sym, p, comdat_aux };
                                dyn_array_put(pending_indices, pending);

                                // track the symbol index for later, all other
                                // user_data in this parsing is the symbol ptr
                                // but we don't have one yet
                                sym->user_data = (void*) i;
                                goto skip;
                            }
                        }

                        if (p == NULL) {
                            goto skip;
                        }
                    }
                }
                TB_ASSERT(p != NULL);

                s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { p, sym->value }
                };

                COFF_AuxSectionSymbol* comdat_aux = comdat_sections[sym->section_num - 1];
                TB_ASSERT(sym->type != TB_OBJECT_SYMBOL_WEAK_EXTERN);
                if (!is_section && comdat_aux) {
                    s->flags |= TB_LINKER_SYMBOL_COMDAT;
                    comdat_sections[sym->section_num - 1] = NULL;

                    // private COMDATs always win
                    if (sym->type == TB_OBJECT_SYMBOL_STATIC) {
                        p->comdat_parent = s->normal.piece;
                    }
                }
            } else if (sym->type == TB_OBJECT_SYMBOL_EXTERN || sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                // symbols without a section number are proper externals (ones defined somewhere
                // else that we might want)
                s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = sym->name,
                    .tag  = TB_LINKER_SYMBOL_UNKNOWN,
                };

                if (sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                    dyn_array_put(weak_syms, sym);
                }
            } else {
                // log_debug("skipped %.*s", (int) sym->name.length, sym->name.data);
            }

            // insert into global symbol table
            if (s != NULL && sym->type != TB_OBJECT_SYMBOL_STATIC) {
                TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s, true);
                if (s->flags & TB_LINKER_SYMBOL_COMDAT) {
                    TB_LinkerSectionPiece* p = s->normal.piece;
                    assert(s->tag == TB_LINKER_SYMBOL_NORMAL);
                    assert(p->comdat_parent == &UNKNOWN_LEADER);
                    p->comdat_parent = s == new_s ? p : NULL;
                }
                s = new_s;
            }
            sym->user_data = s;

            skip:;
            // write into symbol mapping (including whatever aux data "padding")
            symbol_map[i] = s;
            FOR_N(j, 1, c) { symbol_map[i+j] = NULL; }
            i += c;
        }
    }

    if (dyn_array_length(pending_indices) > 0) {
        dyn_array_for(i, pending_indices) {
            TB_ObjectSymbol* sym = pending_indices[i].sym;
            TB_LinkerSectionPiece* p = pending_indices[i].p;
            COFF_AuxSectionSymbol* comdat_aux = pending_indices[i].aux;
            TB_LinkerSectionPiece* leader = sec2piece[comdat_aux->number - 1]->comdat_parent;
            assert(leader != &UNKNOWN_LEADER);

            TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
            if (leader != NULL) {
                tb_linker_associate(l, leader, p);
                p->comdat_parent = p;

                // we won, so this associated symbol will be defined
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { p, sym->value }
                };
            } else {
                p->comdat_parent = NULL;
                p->size = 0;

                // we lost, since there's concurrency storm going on outside we'll insert an
                // undefined and it'll be subsumed by whoever actually won when they place their
                // symbol down.
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_UNKNOWN,
                };
            }

            if (sym->type != TB_OBJECT_SYMBOL_STATIC) {
                s = tb_linker_symbol_insert(l, s, true);
            }

            // Update symbol table directly to ideally save on lookups later
            symbol_map[(size_t) sym->user_data] = s;
        }
        dyn_array_clear(pending_indices);
    }

    if (dyn_array_length(weak_syms) > 0) {
        dyn_array_for(i, weak_syms) {
            TB_ObjectSymbol* src_symbol = weak_syms[i];

            // weak aux
            uint32_t* weak_sym = src_symbol->extra;
            TB_ObjectSymbol key = { .ordinal = *weak_sym };
            TB_ObjectSymbol* alt_sym = bsearch(&key, syms, sym_count, sizeof(TB_ObjectSymbol), obj_symbol_cmp);
            tb_linker_symbol_weak(l, src_symbol->user_data, alt_sym->user_data);
        }
        dyn_array_clear(weak_syms);
    }
    tb_linker_clear_local();
}

////////////////////////////////
// Parsing directives
////////////////////////////////
static bool strprefix(const char* str, const char* pre, size_t len) {
    size_t prelen = strlen(pre);
    return tb_string_case_cmp(pre, str, len < prelen ? len : prelen) == 0;
}

static bool strsuffix(const uint8_t* str, const char* suf, size_t len) {
    size_t suflen = strlen(suf);
    return len >= suflen && memcmp(&str[len - suflen], suf, suflen) == 0;
}

static void pe_linker_parse_directives(TPool* pool, void** args) {
    cuikperf_region_start("directives", NULL);

    TB_Linker* l = args[0];
    const uint8_t* curr = args[1];
    const uint8_t* end_directive = args[2];
    tb_linker_worker_init(l);

    while (curr != end_directive) {
        while (curr != end_directive && *curr == ' ') {
            curr++;
        }

        const uint8_t* end = curr;
        while (end != end_directive && *end != ' ') end++;
        if (*curr == 0 || curr == end_directive) {
            break;
        }

        // log_info("directive: %.*s", (int) (end - curr), curr);
        if (strprefix((const char*) curr, "/merge:", end - curr)) {
            curr += sizeof("/merge:")-1;

            // printf("merge: %.*s\n", (int) (end - curr), curr);

            const uint8_t* equals = curr;
            while (*equals && *equals != '=') equals++;

            if (*equals == '=') {
                TB_LinkerCmd cmd = {
                    .from = { curr, equals - curr },
                    .to   = { equals + 1, (end - equals) - 1 },
                };

                // low contention, don't care
                cuikperf_region_start("LOCK", NULL);
                mtx_lock(&l->lock);
                dyn_array_put(l->merges, cmd);
                mtx_unlock(&l->lock);
                cuikperf_region_end();
            }
        } else if (strprefix((const char*) curr, "/include:", end - curr)) {
            curr += sizeof("/include:")-1;

            int len = end - curr;
            if (curr[0] == '"') {
                curr += 1;
                len -= 2;
            }

            // forcibly include symbol
            tb_linker_import_symbol(l, (TB_Slice){ curr, len });
        } else if (strprefix((const char*) curr, "/disallowlib:", end - curr)) {
            curr += sizeof("/disallowlib:")-1;

            int len = end - curr;
            if (curr[0] == '"') {
                curr += 1;
                len -= 2;
            }

            // force the lib list to think this library is already opened
            char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
            memcpy(str, curr, len + 1);
            str[len] = 0;

            if (strhs_intern(&l->libs, str) != str) {
                tb_arena_free(&linker_perm_arena, str, len + 1);
            }
        } else if (strprefix((const char*) curr, "/defaultlib:", end - curr)) {
            curr += sizeof("/defaultlib:")-1;

            int len = end - curr;
            if (curr[0] == '"') {
                curr += 1;
                len -= 2;
            }

            char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
            memcpy(str, curr, len + 1);
            str[len] = 0;

            // low contention, don't care
            cuikperf_region_start("LOCK", NULL);
            mtx_lock(&l->lock);
            dyn_array_put(l->default_libs, str);
            mtx_unlock(&l->lock);
            cuikperf_region_end();
        } else if (strprefix((const char*) curr, "/alternatename:", end - curr)) {
            curr += sizeof("/alternatename:")-1;

            // If the symbol isn't defined yet
            const uint8_t* equals = curr;
            while (*equals && *equals != '=') equals++;

            if (*equals == '=') {
                // printf("alternate: %.*s\n", (int) (end - curr), curr);

                TB_LinkerCmd cmd = {
                    .from = { curr, equals - curr },
                    .to   = { equals + 1, (end - equals) - 1 },
                };

                // low contention, don't care
                cuikperf_region_start("LOCK", NULL);
                mtx_lock(&l->lock);
                dyn_array_put(l->alternate_names, cmd);
                mtx_unlock(&l->lock);
                cuikperf_region_end();
            }
        } else {
            // log_warn("unknown linker directive: %.*s", (int) (end - curr), curr);
        }
        curr = end;
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

