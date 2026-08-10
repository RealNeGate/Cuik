
static void pe_linker_parse_directives(TPool* pool, void** args);
static bool pe_linker_parse_import(TB_Linker* l, TB_Slice content);

static int obj_symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static void tb_linker_ack_read(TPool* pool, void** args) {
    TB_Linker* l = args[0];
    tb_linker_job_done(l);
}

static bool fetch_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    assert(prefetch.length >= sizeof(COFF_FileHeader));

    // Object file:
    //   Load symbol table and string table (which takes up the remainder of the
    //   file after the symbols).
    COFF_FileHeader header = *(COFF_FileHeader*) &prefetch.data[0];
    size_t end_of_section_headers = sizeof(COFF_FileHeader) + (header.section_count * sizeof(COFF_SectionHeader));
    size_t symstr_table_size = obj->size - header.symbol_table;

    if (end_of_section_headers <= prefetch.length) {
        // File header & section headers fit within the prefetch? cool, don't
        // read more than we need then. We should tune these factors later
        obj->io_rem = 1;
        obj->file_bottom = (uint8_t*) &prefetch.data[0];
    } else {
        obj->io_rem = 2;
        obj->file_bottom = tb_linker_moar_mem(end_of_section_headers);
        tb_linker_read_req(l, false, file_header_offset, end_of_section_headers, obj->file_bottom, obj);
    }

    obj->symbol_table = tb_linker_moar_mem(symstr_table_size);
    tb_linker_read_req(l, false, file_header_offset + header.symbol_table, symstr_table_size, obj->symbol_table, obj);
    return false;
}

static void process_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    TB_COFF_Parser parser = { obj->name };
    COFF_FileHeader* header = (COFF_FileHeader*) &obj->file_bottom[0];

    // locate string table (it spans until the end of the file)
    size_t string_table_pos = header->symbol_count * sizeof(COFF_Symbol);
    parser.symbol_count = header->symbol_count;
    parser.symbol_table_pos = header->symbol_table;
    parser.section_count = header->section_count;
    parser.symbol_table = (TB_Slice){
        .length = header->symbol_count * sizeof(COFF_Symbol),
        .data   = obj->symbol_table,
    };
    parser.string_table = (TB_Slice){
        .length = obj->size - string_table_pos,
        .data   = &obj->symbol_table[string_table_pos],
    };

    TB_ArenaSavepoint sp = tb_arena_save(&linker_tmp_arena);
    COFF_SectionHeader* sections = (COFF_SectionHeader*) &obj->file_bottom[sizeof(COFF_FileHeader)];

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_LinkerSectionPiece *text_piece = NULL, *pdata_piece = NULL;

    static TB_LinkerSectionPiece UNKNOWN_LEADER;
    TB_LinkerSectionPiece** comdat_parent = tb_arena_alloc(&linker_tmp_arena, parser.section_count * sizeof(TB_LinkerSectionPiece*));
    TB_LinkerSectionPiece** sec2piece = tb_arena_alloc(&linker_tmp_arena, parser.section_count * sizeof(TB_LinkerSectionPiece*));

    // Compute where the relocations are, since they're usually next to each other
    // and they're likely to overlap the same file blocks.
    size_t reloc_lo = SIZE_MAX, reloc_hi = 0, reloc_used = 0;

    uint64_t order = obj->time;
    TB_LinkerSymbol** symbol_map = tb_arena_alloc(&linker_perm_arena, parser.symbol_count * sizeof(TB_LinkerSymbol*));
    CUIK_TIMED_BLOCK("parse sections") {
        FOR_N(i, 0, parser.section_count) {
            COFF_SectionHeader* sec = &sections[i];

            // Init for later
            comdat_parent[i] = &UNKNOWN_LEADER;

            TB_Slice s_name = tb_coff_section_name(&parser, sec);
            int dollar = find_char(s_name, '$');
            size_t drectve_len = sizeof(".drectve")-1;
            if (dollar >= drectve_len && memcmp(s_name.data, ".drectve", drectve_len) == 0) {
                sec2piece[i] = NULL;

                if (sec->raw_data_size != 0) {
                    uint8_t* buf = tb_linker_moar_mem(sec->raw_data_size);

                    // Fork out a parallel task
                    l->jobs.count += 1;
                    tb_linker_read_req2(l, false, obj->fd, file_header_offset + sec->raw_data_pos, sec->raw_data_size, buf, pe_linker_parse_directives);
                }
                continue;
            }

            // remove all the alignment flags, they don't appear in linker sections
            uint32_t sec_flags = sec->characteristics;
            TB_LinkerSection* ls = tb_linker_find_or_create_section(l, s_name.length, (const char*) s_name.data, sec_flags & ~0x00F00000);

            if (sec_flags & (IMAGE_SCN_LNK_REMOVE | IMAGE_SCN_MEM_DISCARDABLE)) {
                ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
            }

            size_t sec_vsize = sec->raw_data_size;
            if (sec_vsize < sec->misc.virtual_size) {
                sec_vsize = sec->misc.virtual_size;
            }

            TB_LinkerSectionPiece* p;
            p = tb_linker_append_piece(ls, PIECE_BSS, sec_vsize, obj);
            if ((sec_flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                p->kind        = PIECE_FILE;
                p->file_offset = obj->offset + sec->raw_data_pos;
                p->file_size   = sec->raw_data_size;
                p->fd          = obj->fd;
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
            p->relocs = NULL; // &parser.file.data[sec->pointer_to_reloc];

            if (sec->num_reloc) {
                size_t end_of_reloc = p->reloc_pos + p->reloc_size;
                reloc_lo = TB_MIN(reloc_lo, p->reloc_pos);
                reloc_hi = TB_MAX(reloc_hi, end_of_reloc);
                reloc_used += p->reloc_size;
                // printf("  %zu %zu\n", p->reloc_pos, end_of_reloc);
            }

            if (sec_flags & IMAGE_SCN_LNK_COMDAT) {
                p->flags |= TB_LINKER_PIECE_COMDAT;
            } else {
                if (dollar == 5 && memcmp(s_name.data, ".text", 5) == 0) {
                    // assert(text_piece == NULL);
                    text_piece = p;
                } else if (dollar == 6 && memcmp(s_name.data, ".pdata", 6) == 0) {
                    // assert(pdata_piece == NULL);
                    pdata_piece = p;
                } else if (s_name.length == 8 && memcmp(s_name.data, ".debug$S", 8) == 0) {
                    obj->debug_s = p;
                } else if (s_name.length == 8 && memcmp(s_name.data, ".debug$T", 8) == 0) {
                    obj->debug_t = p;
                }
            }
        }
    }

    size_t reloc_pages = (reloc_hi - reloc_lo) / 4096;
    if (reloc_pages < 64) {
        obj->reloc_lo = reloc_lo & ~4095;
        obj->reloc_hi = (reloc_hi + 4095) & ~4095;
        obj->reloc_cache = tb_linker_moar_mem(obj->reloc_hi - obj->reloc_lo);

        // request to load all the relocation data early, this way it can also
        // happen in the background before the mark phase.
        l->jobs.count += 1;
        tb_linker_read_req3(l, true, obj->fd, obj->reloc_lo, obj->reloc_hi, obj->reloc_cache, tb_linker_ack_read, obj, NULL);

        FOR_N(i, 0, parser.section_count) {
            TB_LinkerSectionPiece* p = sec2piece[i];
            if (p != NULL) {
                p->relocs = &obj->reloc_cache[reloc_lo - obj->reloc_lo];
            }
        }
    }

    // double usage = (reloc_used / (double) (reloc_hi - reloc_lo)) * 100.0;
    // printf("A %.*s %zu %zu %zu (%.2f %%)\n", (int) obj->name.length, obj->name.data, reloc_lo, reloc_hi, (reloc_hi - reloc_lo) / 4096, usage);

    // associate the debug and pdata with the text
    if (text_piece && pdata_piece) {
        tb_linker_associate(l, text_piece, pdata_piece);
    }

    // append all symbols
    size_t sym_count = 0;
    TB_ObjectSymbol* syms = tb_arena_alloc(&linker_tmp_arena, parser.symbol_count * sizeof(TB_ObjectSymbol));

    DynArray(PendingCOMDAT) pending_indices = NULL;
    DynArray(TB_ObjectSymbol*) weak_syms = NULL;
    NL_Map(int, COFF_AuxSectionSymbol*) comdat_sections = NULL;
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
                            if (comdat_aux->selection == 5) {
                                TB_ASSERT(p != NULL);
                                PendingCOMDAT pending = { p, comdat_aux };
                                dyn_array_put(pending_indices, pending);
                            } else {
                                // next symbol in this section is the COMDAT symbol
                                nl_map_put(comdat_sections, sym->section_num, sym->extra);
                            }
                        }

                        // sections without a piece are ok
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

                ptrdiff_t search = nl_map_get(comdat_sections, sym->section_num);
                COFF_AuxSectionSymbol* comdat_aux = search >= 0 ? comdat_sections[search].v : NULL;

                TB_ASSERT(sym->type != TB_OBJECT_SYMBOL_WEAK_EXTERN);
                if (!is_section && comdat_aux) {
                    if (comdat_aux->selection == 1) {
                        s->comdat = TB_LINKER_COMDAT_NODUP;
                    } else {
                        s->comdat = TB_LINKER_COMDAT_ANY;
                    }
                    nl_map_remove(comdat_sections, sym->section_num);

                    // private COMDATs just always win
                    if (sym->type == TB_OBJECT_SYMBOL_STATIC) {
                        comdat_parent[sym->section_num - 1] = s->normal.piece;
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
                if (s->comdat != TB_LINKER_COMDAT_NONE) {
                    assert(s->tag == TB_LINKER_SYMBOL_NORMAL);
                    comdat_parent[sym->section_num - 1] = s == new_s ? s->normal.piece : NULL;
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
    nl_map_free(comdat_sections);

    if (dyn_array_length(pending_indices) > 0) {
        dyn_array_for(i, pending_indices) {
            COFF_AuxSectionSymbol* comdat_aux = pending_indices[i].aux;
            TB_LinkerSectionPiece* leader = comdat_parent[comdat_aux->number - 1];
            assert(leader != &UNKNOWN_LEADER);

            if (leader != NULL) {
                tb_linker_associate(l, leader, pending_indices[i].piece);
            } else {
                // ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
            }
        }
        dyn_array_destroy(pending_indices);
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
        dyn_array_destroy(weak_syms);
    }
    tb_arena_restore(&linker_tmp_arena, sp);
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
    tb_linker_worker_init(l);

    const uint8_t* curr = args[1];
    const uint8_t* end_directive = args[2];

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

            // forcibly include symbol
            tb_linker_import_symbol(l, (TB_Slice){ curr, end - curr });
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

