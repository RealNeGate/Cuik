
typedef struct {
    TB_ObjectSymbol* sym;
    TB_LinkerSectionPiece* p;
    COFF_AuxSectionSymbol* aux;
} PendingCOMDAT;

static void pe_linker_parse_directives(TPool* pool, void** args);
static bool pe_linker_parse_import(TB_Linker* l, TB_Slice content);

static void pe_linker_parse_directives_io(TPool* pool, TPool_ReadReq* req) {
    void* args[3] = { req->args[0], req->data, ((char*)req->data) + req->size };
    tpool_io_forward(pool, true, pe_linker_parse_directives, 3, args);
}

static int obj_symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static void tb_linker_ack_read(TPool* pool, TPool_ReadReq* req) {
    tb_linker_job_done(req->args[0]);
}

static int compare_cache_ranges(const void* a, const void* b) {
    const TB_CacheRange* aa = (const TB_CacheRange*) a;
    const TB_CacheRange* bb = (const TB_CacheRange*) b;
    return aa->offset - bb->offset;
}

static bool fetch_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    assert(prefetch.length >= sizeof(COFF_FileHeader));

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

    if (sizeof(COFF_FileHeader) + size_of_section_headers <= prefetch.length) {
        // File header & section headers fit within the prefetch? cool, don't
        // read more than we need then. We should tune these factors later
        obj->io_rem = 1;
        obj->sections = (uint8_t*) &prefetch.data[sizeof(COFF_FileHeader)];
    } else {
        obj->io_rem = 2;
        obj->sections = tb_linker_moar_mem(size_of_section_headers);
        tb_linker_read_req(l, file_header_offset + sizeof(COFF_FileHeader), size_of_section_headers, obj->sections, obj);
    }

    obj->symbol_table = tb_linker_moar_mem(symstr_table_size);
    tb_linker_read_req(l, file_header_offset + header.symbol_table, symstr_table_size, obj->symbol_table, obj);
    return false;
}

static TB_LinkerSectionPiece UNKNOWN_LEADER;
static void process_obj_file(TB_Linker* l, TB_LinkerObject* obj, TB_Slice prefetch, size_t file_header_offset) {
    /* if (obj->fetch != fetch_obj_file) {
    __builtin_debugtrap();
    } */

    size_t symbol_size      = obj->is_big ? sizeof(COFF_BigSymbol) : sizeof(COFF_Symbol);
    TB_COFF_Parser parser   = { obj->name, .is_big = obj->is_big };
    size_t string_table_pos = obj->symbol_count * symbol_size;

    parser.symbol_count     = obj->symbol_count;
    parser.symbol_table_pos = obj->symbol_table_pos;
    parser.section_count    = obj->section_count;
    parser.symbol_table = (TB_Slice){
        .length = obj->symbol_count * symbol_size,
        .data   = obj->symbol_table,
    };
    parser.string_table = (TB_Slice){
        .length = obj->size - string_table_pos,
        .data   = &obj->symbol_table[string_table_pos],
    };

    TB_ArenaSavepoint sp = tb_arena_save(&linker_tmp_arena);
    COFF_SectionHeader* sections = (COFF_SectionHeader*) obj->sections;

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_LinkerSectionPiece *text_piece = NULL, *pdata_piece = NULL;

    TB_LinkerSectionPiece** sec2piece = tb_arena_alloc(&linker_tmp_arena, parser.section_count * sizeof(TB_LinkerSectionPiece*));
    COFF_AuxSectionSymbol** comdat_sections = tb_arena_alloc(&linker_tmp_arena, parser.section_count * sizeof(COFF_AuxSectionSymbol*));

    size_t cache_lo = SIZE_MAX, cache_hi = 0;
    uint64_t order = obj->time;
    TB_LinkerSymbol** symbol_map = tb_arena_alloc(&linker_perm_arena, parser.symbol_count * sizeof(TB_LinkerSymbol*));
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
                    uint8_t* buf = tb_linker_moar_mem(sec->raw_data_size);

                    // Fork out a parallel task
                    l->jobs.count += 1;
                    tb_linker_read_req2(l, obj->fd, file_header_offset + sec->raw_data_pos, sec->raw_data_size, buf, pe_linker_parse_directives_io);
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

            TB_LinkerSectionPiece* p = tb_linker_append_piece(ls, PIECE_BSS, sec_vsize, obj);
            if ((sec_flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                p->kind        = PIECE_FILE;
                p->file_offset = file_header_offset + sec->raw_data_pos;
                p->buffer_size = sec->raw_data_size;
                p->fd          = obj->fd;

                if (p->buffer_size) {
                    size_t end_of_data = p->file_offset + p->buffer_size;
                    cache_lo = TB_MIN(cache_lo, p->file_offset);
                    cache_hi = TB_MAX(cache_hi, end_of_data);

                    TB_CacheRange r = { p->file_offset, p->buffer_size };
                    dyn_array_put(obj->cache_ranges, r);
                }
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
                size_t end_of_reloc = p->reloc_pos + p->reloc_size;
                cache_lo = TB_MIN(cache_lo, p->reloc_pos);
                cache_hi = TB_MAX(cache_hi, end_of_reloc);

                TB_CacheRange r = { p->reloc_pos, p->reloc_size };
                dyn_array_put(obj->cache_ranges, r);
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
                }
            }
        }
    }

    // Initialize the block cache
    CUIK_TIMED_BLOCK("initialize cache") {
        obj->cache_lo = cache_lo & -FILE_BLOCK_SIZE;
        obj->cache_hi = (cache_hi + FILE_BLOCK_SIZE - 1) & -FILE_BLOCK_SIZE;
        obj->cache_data = tb_linker_moar_mem(obj->cache_hi - obj->cache_lo);

        size_t cache_blocks = (obj->cache_hi - obj->cache_lo) / FILE_BLOCK_SIZE;
        if (cache_blocks <= 16) {
            obj->fully_resident = true;

            l->jobs.count += 1;
            tb_linker_read_req2(l, obj->fd, obj->cache_lo, cache_blocks*FILE_BLOCK_SIZE, obj->cache_data, tb_linker_ack_read);
        } else {
            // Sort the ranges
            qsort(obj->cache_ranges, dyn_array_length(obj->cache_ranges), sizeof(TB_CacheRange), compare_cache_ranges);

            dyn_array_for(i, obj->cache_ranges) {
                uint32_t page_start = obj->cache_ranges[i].offset / FILE_BLOCK_SIZE;
                uint32_t page_end   = (obj->cache_ranges[i].offset + obj->cache_ranges[i].size + FILE_BLOCK_SIZE - 1) / FILE_BLOCK_SIZE;

                obj->cache_ranges[i].io_rem = page_end - page_start;
            }

            // Allocate bitmaps
            obj->bitmap_size = (cache_blocks + 63) / 64;
            obj->reserve = tb_linker_moar_mem(obj->bitmap_size * sizeof(uint64_t));
        }

        // setup pointers early
        FOR_N(i, 0, parser.section_count) {
            TB_LinkerSectionPiece* p = sec2piece[i];
            if (p != NULL) {
                if (p->reloc_size > 0) {
                    p->relocs = &obj->cache_data[p->reloc_pos - obj->cache_lo];
                }
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
                    if (comdat_aux->selection == 1) {
                        s->comdat = TB_LINKER_COMDAT_NODUP;
                    } else {
                        s->comdat = TB_LINKER_COMDAT_ANY;
                    }
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
                if (s->comdat != TB_LINKER_COMDAT_NONE) {
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

        // log_info("directive: %e*s", (int) (end - curr), curr);
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

