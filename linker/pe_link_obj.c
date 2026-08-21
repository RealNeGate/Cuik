
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

static bool pe_linker_parse_directives(TB_Linker* l, BCache_Job* job, void* arg);

static int obj_symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static void process_obj_file(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj);
static bool step_obj_file(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj, TB_Slice prefetch) {
    BCache_File* file = obj->file;
    size_t file_header_offset = obj->offset + obj->skip_header;

    switch (job->state) {
        case 0: {
            // Object file:
            //   Load symbol table and string table (which takes up the remainder of the
            //   file after the symbols).
            size_t header_size;
            if (obj->is_big) {
                assert(prefetch.length >= sizeof(COFF_BigHeader));

                COFF_BigHeader header;
                memcpy(&header, prefetch.data, sizeof(header));
                header_size = sizeof(header);

                obj->symbol_count     = header.symbol_count;
                obj->symbol_table_pos = header.symbol_table;
                obj->section_count    = header.section_count;
            } else {
                assert(prefetch.length >= sizeof(COFF_FileHeader));

                COFF_FileHeader header;
                memcpy(&header, prefetch.data, sizeof(header));
                header_size = sizeof(header);

                obj->symbol_count     = header.symbol_count;
                obj->symbol_table_pos = header.symbol_table;
                obj->section_count    = header.section_count;
            }

            size_t size_of_section_headers = obj->section_count * sizeof(COFF_SectionHeader);
            assert(header_size + size_of_section_headers <= obj->size);
            JOB_READ(1, file_header_offset + header_size, size_of_section_headers, &obj->sections);
        }

        case 1: {
            size_t symstr_table_size = obj->size - obj->symbol_table_pos;
            JOB_READ(2, file_header_offset + obj->symbol_table_pos, symstr_table_size, &obj->symbol_table);
        }

        case 2: {
            process_obj_file(l, job, obj);
            return true;
        }
    }

    tb_todo();
}

typedef struct {
    TB_COFF_Parser* parser;
    size_t file_header_offset;
    TB_LinkerSymbol** symbol_map;
    TB_LinkerSection* last_sec;
} SectionParser;

static TB_LinkerSectionPiece* load_section_piece(TB_Linker* l, TB_LinkerObject* obj, SectionParser* parser, COFF_SectionHeader* sec, uint64_t order) {
    TB_Slice s_name = tb_coff_section_name(parser->parser, sec);
    TB_LinkerSection* ls = parser->last_sec;

    uint32_t sec_flags = sec->characteristics;
    uint32_t flags = sec_flags & ~0x00F00000;
    if (ls == NULL || ls->name.length != s_name.length || memcmp(ls->name.data, s_name.data, s_name.length) != 0) {
        parser->last_sec = ls = tb_linker_find_or_create_section(l, s_name.length, (const char*) s_name.data, flags);
    }

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
        p->file_offset = parser->file_header_offset + sec->raw_data_pos;
        p->buffer_size = sec->raw_data_size;
        p->file        = obj->file;
    }
    if (sec_flags & 0x00F00000) {
        // go stare at the table, it'll make sense
        p->align_log2 = ((sec_flags >> 20) & 0xF) - 1;
    }
    // broadcast to all sections how to find symbols (for relocation resolution later)
    if (p != NULL) {
        p->symbol_map = parser->symbol_map;
    }
    p->order = order;
    p->flags = (sec_flags & IMAGE_SCN_MEM_EXECUTE) ? TB_LINKER_PIECE_CODE : 0;
    p->reloc_count = sec->num_reloc;
    p->reloc_pos = parser->file_header_offset + sec->pointer_to_reloc;
    p->reloc_size = sec->num_reloc * sizeof(COFF_ImageReloc);
    p->relocs = NULL;
    if (p->reloc_count) {
        p->relocs = &p->obj->file->raw_map[p->reloc_pos];
    }
    if (sec_flags & IMAGE_SCN_LNK_COMDAT) {
        p->flags |= TB_LINKER_PIECE_COMDAT;
    }
    return p;
}

static TB_LinkerSectionPiece UNKNOWN_LEADER;
static void process_obj_file(TB_Linker* l, BCache_Job* job, TB_LinkerObject* obj) {
    BCache_File* file = obj->file;
    size_t file_header_offset = obj->offset + obj->skip_header;

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
    TB_LinkerSymbol** symbol_map = tb_linker_moar_mem(parser.symbol_count * sizeof(TB_LinkerSymbol*));

    SectionParser sec_parser = {
        &parser, file_header_offset, symbol_map
    };

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
                    uint8_t* curr = &file->raw_map[file_header_offset + sec->raw_data_pos];
                    uint8_t* end_directive = curr + sec->raw_data_size;

                    // Fork out a parallel task
                    BCache_Job* job = tb_linker_job_new(l, obj->file, pe_linker_parse_directives, curr, sizeof(void*));
                    *((void**) job->extra) = end_directive;
                    tb_linker_job_read(l, job, file_header_offset + sec->raw_data_pos, sec->raw_data_size, NULL);
                }
                continue;
            }

            uint32_t sec_flags = sec->characteristics;
            if (sec_flags & IMAGE_SCN_LNK_COMDAT) {
                // Delay creation of comdat sections
                sec2piece[i] = &UNKNOWN_LEADER;
                continue;
            }

            // remove all the alignment flags, they don't appear in linker sections
            uint32_t flags = sec_flags & ~0x00F00000;
            sec2piece[i] = load_section_piece(l, obj, &sec_parser, sec, order + i);
        }
    }

    // append all symbols
    size_t sym_count = 0;
    TB_ObjectSymbol* syms = tb_linker_alloc_local(parser.symbol_count * sizeof(TB_ObjectSymbol));

    static _Thread_local DynArray(TB_ObjectSymbol*) pending_indices = NULL;
    static _Thread_local DynArray(TB_ObjectSymbol*) weak_syms = NULL;

    CUIK_TIMED_BLOCK("reserve") {
        dyn_array_reserve(pending_indices, parser.symbol_count);
    }

    CUIK_TIMED_BLOCK("apply symbols") {
        size_t i = 0;
        while (i < parser.symbol_count) {
            assert(sym_count < parser.symbol_count);
            TB_ObjectSymbol* sym = &syms[sym_count++];
            size_t c = tb_coff_parse_symbol(&parser, i, sym);
            TB_ASSERT(c > 0);

            TB_LinkerSymbol* s = NULL;
            if (sym->section_num > 0) {
                assert(sym->section_num <= parser.section_count);
                size_t section_idx = sym->section_num - 1;
                COFF_SectionHeader* sec = &sections[section_idx];
                TB_LinkerSectionPiece* p = sec2piece[section_idx];

                s = tb_linker_moar_mem(sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { p, sym->value } // , order + section_idx }
                };

                #if 0
                static const char sss[] = "$stateUnwindMap$?catch$0@?0???2@YAPEAX_KW4align_val_t@std@@AEBUnothrow_t@1@@Z@4HA";
                if (sym->name.length == sizeof(sss)-1 && memcmp(sym->name.data, sss, sizeof(sss)-1) == 0) {
                    __builtin_debugtrap();
                }
                #endif

                COFF_AuxSectionSymbol* comdat_aux = comdat_sections[section_idx];
                TB_ASSERT(sym->type != TB_OBJECT_SYMBOL_WEAK_EXTERN);
                if (comdat_aux) {
                    s->flags |= TB_LINKER_SYMBOL_COMDAT;
                    comdat_sections[section_idx] = NULL;

                    // private COMDATs always win
                    TB_LinkerSymbol* new_s = s;
                    if (sym->type != TB_OBJECT_SYMBOL_STATIC) {
                        new_s = tb_linker_symbol_insert(l, s, true);
                    }

                    // construct section piece now, dedup this code
                    if (s == new_s) {
                        // remove all the alignment flags, they don't appear in linker sections
                        sec2piece[section_idx] = s->normal.piece = load_section_piece(l, obj, &sec_parser, sec, order + i);
                    } else {
                        sec2piece[section_idx] = NULL;
                        s = new_s;
                    }
                    goto skip;
                } else if (p == &UNKNOWN_LEADER) {
                    COFF_AuxSectionSymbol* comdat_aux = sym->extra;
                    if (comdat_aux && comdat_aux->selection != 5) {
                        // next symbol in this section is the COMDAT symbol
                        comdat_sections[section_idx] = sym->extra;
                    }

                    assert(sym->ordinal == i);
                    dyn_array_put(pending_indices, sym);
                    goto skip;
                } else if (p == NULL) {
                    // If we're not COMDAT bs we should have a section piece
                    goto skip;
                }
            } else if (sym->type == TB_OBJECT_SYMBOL_EXTERN || sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                // symbols without a section number are proper externals (ones defined somewhere
                // else that we might want)
                s = tb_linker_moar_mem(sizeof(TB_LinkerSymbol));
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
                assert((s->flags & TB_LINKER_SYMBOL_COMDAT) == 0);
                s = tb_linker_symbol_insert(l, s, true);
            }

            skip:;
            // write into symbol mapping (including whatever aux data "padding")
            sym->user_data = s;
            symbol_map[i] = s;
            FOR_N(j, 1, c) { symbol_map[i+j] = NULL; }
            i += c;
        }
    }

    if (dyn_array_length(pending_indices) > 0) {
        cuikperf_region_start("COMDAT", NULL);

        #if 0
        static const char sss[] = "PassBuilder.cpp.obj";
        if (obj->name.length == sizeof(sss)-1 && memcmp(obj->name.data, sss, sizeof(sss)-1) == 0) {
            __builtin_debugtrap();
            printf("Pending: %zu\n", dyn_array_length(pending_indices));
        }
        #endif

        dyn_array_for(i, pending_indices) {
            TB_ObjectSymbol* sym = pending_indices[i];
            COFF_AuxSectionSymbol* comdat_aux = sym->extra;

            size_t symbol_idx = sym->ordinal;
            size_t section_idx = sym->section_num - 1;

            TB_LinkerSymbol* s = tb_linker_moar_mem(sizeof(TB_LinkerSymbol));
            if (comdat_aux != NULL && comdat_aux->selection == 5) {
                TB_LinkerSectionPiece* leader = sec2piece[comdat_aux->number - 1];
                assert(leader != &UNKNOWN_LEADER);

                if (leader != NULL) {
                    COFF_SectionHeader* sec = &sections[section_idx];
                    TB_LinkerSectionPiece* p = load_section_piece(l, obj, &sec_parser, sec, order + section_idx);

                    tb_linker_associate(l, leader, p);
                    sec2piece[section_idx] = p;

                    // we won, so this associated symbol will be defined
                    *s = (TB_LinkerSymbol){
                        .name   = sym->name,
                        .tag    = TB_LINKER_SYMBOL_NORMAL,
                        .normal = { p, sym->value }
                    };
                } else {
                    sec2piece[section_idx] = NULL;

                    // we lost, since there's concurrency storm going on outside we'll insert an
                    // undefined and it'll be subsumed by whoever actually won when they place their
                    // symbol down.
                    *s = (TB_LinkerSymbol){
                        .name   = sym->name,
                        .tag    = TB_LINKER_SYMBOL_UNKNOWN,
                    };
                }
            } else if (sec2piece[section_idx] != NULL) {
                if (sec2piece[section_idx] == &UNKNOWN_LEADER) {
                    symbol_map[symbol_idx] = NULL;
                    continue;
                }

                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { sec2piece[section_idx], sym->value }
                };
            }

            if (sym->type != TB_OBJECT_SYMBOL_STATIC) {
                s = tb_linker_symbol_insert(l, s, true);
            }

            // Update symbol table directly to ideally save on lookups later
            symbol_map[symbol_idx] = s;
        }
        dyn_array_clear(pending_indices);
        cuikperf_region_end();
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

static bool pe_linker_parse_directives(TB_Linker* l, BCache_Job* job, void* arg) {
    COFF_SectionHeader* sec = arg;
    cuikperf_region_start("directives", NULL);
    tb_linker_worker_init(l);

    BCache_File* file = job->file;
    const uint8_t* curr = arg;
    const uint8_t* end_directive = *((void**) job->extra);

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
    return true;
}

