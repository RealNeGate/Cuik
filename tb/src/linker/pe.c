#define NL_STRING_MAP_IMPL
#include "linker.h"
#include "../objects/coff.h"
#include "../objects/lib_parse.h"

#include <ctype.h>
#include <file_map.h>

enum { IMP_PREFIX_LEN = sizeof("__imp_") - 1 };

const static uint8_t dos_stub[] = {
    // header
    0x4d,0x5a,0x78,0x00,0x01,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x00,0x00,0x00,

    // machine code
    0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,0x54,0x68,
    0x69,0x73,0x20,0x70,0x72,0x6f,0x67,0x72,0x61,0x6d,0x20,0x63,0x61,0x6e,0x6e,0x6f,
    0x74,0x20,0x62,0x65,0x20,0x72,0x75,0x6e,0x20,0x69,0x6e,0x20,0x44,0x4f,0x53,0x20,
    0x6d,0x6f,0x64,0x65,0x2e,0x24,0x00,0x00
};

static int symbol_cmp(const void* a, const void* b) {
    const TB_ObjectSymbol* sym_a = (const TB_ObjectSymbol*)a;
    const TB_ObjectSymbol* sym_b = (const TB_ObjectSymbol*)b;

    return sym_a->ordinal - sym_b->ordinal;
}

static int compare_rva(const void* a, const void* b) {
    const uint32_t* aa = a;
    const uint32_t* bb = b;

    if (*aa < *bb) return -1;
    if (*aa > *bb) return 1;
    return 0;
}

// true if we replace the old one
static bool process_comdat(int select, TB_LinkerSectionPiece* old_p, TB_LinkerSectionPiece* new_p) {
    switch (select) {
        case 2: return false;                     // any
        case 6: return new_p->size > old_p->size; // largest
        default: tb_todo();
    }
}

// Musl's impl for this
static int string_case_cmp(const char *_l, const char *_r, size_t n) {
    const unsigned char *l=(void *)_l, *r=(void *)_r;
    if (!n--) return 0;
    for (; *l && *r && n && (*l == *r || tolower(*l) == tolower(*r)); l++, r++, n--);
    return tolower(*l) - tolower(*r);
}

static bool strprefix(const char* str, const char* pre, size_t len) {
    size_t prelen = strlen(pre);
    return string_case_cmp(pre, str, len < prelen ? len : prelen) == 0;
}

static void pe_append_module(TB_Linker* l, TB_Module* m) {
    m->visited = false;

    // Also resolves internal call patches which is cool
    ExportList exports;
    CUIK_TIMED_BLOCK("layout section") {
        m->exports = tb_module_layout_sections(m);
    }

    // We don't *really* care about this info beyond nicer errors
    TB_LinkerObject* obj_file = tb_platform_heap_alloc(sizeof(TB_LinkerObject));
    *obj_file = (TB_LinkerObject){ .module = m };

    // Convert module into sections which we can then append to the output
    DynArray(TB_ModuleSection) sections = m->sections;
    dyn_array_for(i, sections) {
        uint32_t flags = TB_COFF_SECTION_READ;
        if (sections[i].flags & TB_MODULE_SECTION_WRITE) flags |= TB_COFF_SECTION_WRITE;
        if (sections[i].flags & TB_MODULE_SECTION_EXEC)  flags |= TB_COFF_SECTION_EXECUTE | IMAGE_SCN_CNT_CODE;
        if (sections[i].comdat.type != 0) flags |= TB_COFF_SECTION_COMDAT;

        tb_linker_append_module_section(l, obj_file, &sections[i], flags);
    }

    const ICodeGen* restrict code_gen = tb_codegen_info(m);
    if (m->compiled_function_count > 0 && code_gen->emit_win64eh_unwind_info) {
        TB_LinkerSection* rdata = tb_linker_find_or_create_section(l, sizeof(".rdata") - 1, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

        CUIK_TIMED_BLOCK("generate xdata") {
            TB_Emitter xdata = { 0 };

            dyn_array_for(i, sections) {
                DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
                dyn_array_for(j, funcs) {
                    TB_FunctionOutput* out_f = funcs[j];
                    out_f->win32_unwind_info = xdata.count;
                    code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->stack_usage);
                }
            }

            TB_LinkerSectionPiece* x = tb_linker_append_piece(rdata, PIECE_BUFFER, xdata.count, obj_file);
            x->buffer = xdata.data;

            TB_LinkerSection* pdata = tb_linker_find_or_create_section(l, sizeof(".pdata") - 1, ".pdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
            TB_LinkerSectionPiece* pdata_piece = tb_linker_append_piece(pdata, PIECE_PDATA, m->compiled_function_count * 12, obj_file);

            tb_linker_associate(l, pdata_piece, x);

            dyn_array_for(i, sections) {
                DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
                if (dyn_array_length(funcs) && sections[i].piece) {
                    tb_linker_associate(l, sections[i].piece, pdata_piece);
                }
            }
            m->xdata = x;
        }
    }

    tb_linker_append_module_symbols(l, m);
}

void pe_append_object(TPool* pool, ImportObjTask* task) {
    size_t slash = 0;
    FOR_REV_N(i, 0, task->obj_name.length) {
        if (task->obj_name.data[i] == '/' || task->obj_name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("object", task->obj_name.length - slash, (const char*) task->obj_name.data + slash);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_Linker* l = task->linker;
    TB_Slice obj_name = task->obj_name;
    TB_Slice content = task->content;

    TB_COFF_Parser parser = { obj_name, content };
    tb_coff_parse_init(&parser);

    // We don't *really* care about this info beyond nicer errors (use an arena tho)
    TB_LinkerObject* obj_file = tb_platform_heap_alloc(sizeof(TB_LinkerObject));
    *obj_file = (TB_LinkerObject){ .name = obj_name };

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_LinkerSectionPiece *text_piece = NULL, *pdata_piece = NULL;
    TB_ObjectSection* sections = tb_platform_heap_alloc(parser.section_count * sizeof(TB_ObjectSection));

    uint64_t order1 = task->time;
    CUIK_TIMED_BLOCK("parse sections") {
        FOR_N(i, 0, parser.section_count) {
            TB_ObjectSection* restrict s = &sections[i];
            tb_coff_parse_section(&parser, i, s);

            // trim the dollar sign (if applies)
            uint64_t order0 = 0;
            FOR_N(j, 0, s->name.length) {
                if (s->name.data[j] == '$') {
                    // convert letters into score
                    FOR_N(k, j + 1, s->name.length) {
                        order0 <<= 8;
                        order0 += s->name.data[k];
                    }

                    s->name.length = j;
                    break;
                }
            }

            size_t drectve_len = sizeof(".drectve")-1;

            if (s->name.length == drectve_len && memcmp(s->name.data, ".drectve", drectve_len) == 0 && s->raw_data.length > 3) {
                const uint8_t* curr = s->raw_data.data;
                const uint8_t* end_directive = s->raw_data.data + s->raw_data.length;

                while (curr != end_directive && *curr == ' ') curr++;

                while (curr != end_directive) {
                    const uint8_t* end = curr;
                    while (end != end_directive && *end != ' ') end++;

                    // log_info("directive: %.*s", (int) (end - curr), curr);

                    if (curr == end_directive) {
                        break;
                        // } else if (strprefix((const char*) curr, "/merge:", end - curr)) {
                        /* curr += sizeof("/merge:")-1;

                        const uint8_t* equals = curr;
                        while (*equals && *equals != '=') equals++;

                        if (*equals == '=') {
                            TB_LinkerCmd cmd = {
                                .from = { curr, equals - curr },
                                .to   = { equals + 1, (end - equals) - 1 },
                            };
                            dyn_array_put(info->merges, cmd);
                        } */
                        // } else if (strprefix((const char*) curr, "/defaultlib:", end - curr)) {
                        // curr += sizeof("/defaultlib:")-1;
                        // TB_LinkerMsg m = { .tag = TB_LINKER_MSG_IMPORT, .import_path = { end - curr, (const char*) curr } };
                        // tb_linker_send_msg(l, &m);
                        // } else if (strprefix((const char*) curr, "/alternatename:", end - curr)) {
                        /* curr += sizeof("/alternatename:")-1;

                        const uint8_t* equals = curr;
                        while (*equals && *equals != '=') equals++;

                        if (*equals == '=') {
                            TB_LinkerCmd cmd = {
                                .from = { curr, equals - curr },
                                .to   = { equals + 1, (end - equals) - 1 },
                            };
                            dyn_array_put(info->alternates, cmd);
                        } */
                    } else {
                        // log_warn("unknown linker directive: %.*s", (int) (end - curr), curr);
                        break;
                    }

                    curr = end+1;
                }

                continue;
            }

            // remove all the alignment flags, they don't appear in linker sections
            TB_LinkerSection* ls = tb_linker_find_or_create_section(l, s->name.length, (const char*) s->name.data, s->flags & ~0x00F00000);

            /* if (s->flags & IMAGE_SCN_LNK_REMOVE) {
                ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
            } */

            TB_LinkerSectionPiece* p;
            p = tb_linker_append_piece(ls, PIECE_BUFFER, s->raw_data.length, obj_file);
            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                p = tb_linker_append_piece(ls, PIECE_BUFFER, s->raw_data.length, obj_file);
                p->buffer = s->raw_data.data;
                p->buffer_size = s->raw_data.length;
            }
            s->user_data = p;
            p->order[0] = order0;
            p->order[1] = order1 + i;
            p->flags = (s->flags & IMAGE_SCN_MEM_EXECUTE) ? TB_LINKER_PIECE_CODE : 0;
            if (s->flags & IMAGE_SCN_LNK_COMDAT) {
                p->flags |= TB_LINKER_PIECE_COMDAT;
            }

            if (s->name.length == 5 && memcmp(s->name.data, ".text", 5) == 0) {
                text_piece = p;
            } else if (s->name.length == 6 && memcmp(s->name.data, ".pdata", 6) == 0) {
                pdata_piece = p;
            }
        }
    }

    // associate the pdata with the text
    if (text_piece) {
        tb_linker_associate(l, text_piece, pdata_piece);
    }

    // append all symbols
    size_t sym_count = 0;
    TB_ObjectSymbol* syms = tb_platform_heap_alloc(sizeof(TB_ObjectSymbol) * parser.symbol_count);

    NL_Map(int, COFF_AuxSectionSymbol*) comdat_sections = NULL;
    CUIK_TIMED_BLOCK("apply symbols") {
        size_t i = 0;
        while (i < parser.symbol_count) {
            TB_ObjectSymbol* restrict sym = &syms[sym_count++];
            i += tb_coff_parse_symbol(&parser, i, sym);

            TB_LinkerSymbol* s = NULL;
            if (sym->section_num > 0) {
                TB_ObjectSection* sec = &sections[sym->section_num - 1];

                TB_Slice trimmed_name = sym->name;
                if (sym->type == TB_OBJECT_SYMBOL_STATIC && sym->value == 0) {
                    FOR_N(j, 0, sym->name.length) {
                        if (sym->name.data[j] == '$') {
                            trimmed_name.length = j;
                            break;
                        }
                    }

                    if (sec->name.length == trimmed_name.length && memcmp(sec->name.data, trimmed_name.data, trimmed_name.length) == 0) {
                        // COMDAT is how linkers handle merging of inline functions in C++
                        if ((sec->flags & IMAGE_SCN_LNK_COMDAT)) {
                            // the next symbol in the section is the COMDAT symbol
                            nl_map_put(comdat_sections, sym->section_num, sym->extra);
                        }

                        // sections without a piece are ok
                        if (sec->user_data == NULL) {
                            continue;
                        }
                    }
                }

                TB_LinkerSectionPiece* p = sec->user_data;
                TB_ASSERT(p != NULL);

                s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name   = sym->name,
                    .tag    = TB_LINKER_SYMBOL_NORMAL,
                    .normal = { p, sym->value }
                };

                ptrdiff_t search = nl_map_get(comdat_sections, sym->section_num);
                COFF_AuxSectionSymbol* comdat_aux = search >= 0 ? comdat_sections[search].v : NULL;

                if (sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                    if (comdat_aux) {
                        TB_ASSERT(0 && "COMDAT and weak external?");
                        nl_map_remove(comdat_sections, sym->section_num);
                    }
                } else if (sym->type == TB_OBJECT_SYMBOL_EXTERN) {
                    if (comdat_aux) {
                        nl_map_remove(comdat_sections, sym->section_num);
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
            } else {
                // log_debug("skipped %.*s", (int) sym->name.length, sym->name.data);
            }

            // insert into global symbol table
            if (s != NULL && sym->type != TB_OBJECT_SYMBOL_STATIC) {
                /* if (s->tag != TB_LINKER_SYMBOL_UNKNOWN) {
                    log_debug("Extern: %.*s", (int) sym->name.length, sym->name.data);
                } */

                cuikperf_region_start("insert", NULL);
                TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
                if (new_s != s) {
                    tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
                    s = new_s;
                }
                cuikperf_region_end();
            }

            sym->user_data = s;
        }
    }

    CUIK_TIMED_BLOCK("parse relocations") FOR_N(i, 0, parser.section_count) {
        TB_ObjectSection* restrict s = &sections[i];
        TB_LinkerSectionPiece* restrict p = s->user_data;

        // some relocations point to sections within the same object file, we resolve
        // their symbols early.
        FOR_N(j, 0, s->relocation_count) {
            TB_ObjectReloc* restrict reloc = &s->relocations[j];

            // resolve address used in relocation, symbols are sorted so we can binary search
            TB_ObjectSymbol key = { .ordinal = reloc->symbol_index };
            TB_ObjectSymbol* restrict src_symbol = bsearch(&key, syms, sym_count, sizeof(TB_ObjectSymbol), symbol_cmp);
            if (src_symbol->user_data == NULL) {
                continue;
            }

            TB_LinkerReloc r = {
                .type       = reloc->type,
                .addend     = reloc->addend,
                .target     = src_symbol->user_data,
                .src_offset = reloc->virtual_address,
            };

            if (reloc->type == TB_OBJECT_RELOC_ADDR64) {
                if (src_symbol->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                    // weak aux
                    uint32_t* weak_sym = src_symbol->extra;
                    TB_ObjectSymbol* restrict alt_sym = bsearch(
                        &(TB_ObjectSymbol){ .ordinal = *weak_sym },
                        syms, sym_count, sizeof(TB_ObjectSymbol),
                        symbol_cmp
                    );

                    r.alt = alt_sym->user_data;
                }
            } else {
                if (src_symbol->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                    tb_todo();
                }
            }

            dyn_array_put(p->relocs, r);
        }

        if (p && p->relocs) {
            qsort(p->relocs, dyn_array_length(p->relocs), sizeof(TB_LinkerReloc), compare_rva);
        }
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }

    cuikperf_region_end();
}

static void pe_append_library(TPool* pool, ImportObjTask* task) {
    size_t slash = 0;
    FOR_REV_N(i, 0, task->obj_name.length) {
        if (task->obj_name.data[i] == '/' || task->obj_name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    cuikperf_region_start2("library", task->obj_name.length - slash, (const char*) task->obj_name.data + slash);
    log_debug("linking against %.*s", (int) task->obj_name.length, task->obj_name.data);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_Linker* l = task->linker;
    TB_Slice ar_file = task->content;

    TB_ArchiveFileParser ar_parser = { 0 };
    if (!tb_archive_parse(ar_file, &ar_parser)) {
        cuikperf_region_end();
        return;
    }

    TB_ArchiveEntry* entries = tb_platform_heap_alloc(ar_parser.member_count * sizeof(TB_ArchiveEntry));
    size_t new_count;
    CUIK_TIMED_BLOCK("parse_entries") {
        new_count = tb_archive_parse_entries(&ar_parser, 0, ar_parser.member_count, entries);
    }

    // we get a lot of imports to the same table in the same LIB (think of
    // kernel32.lib being completely kernel32.dll imports), because of this
    // we keep the lock open across archive entry iteration for like 1000
    // ops, it's manual lock elision.
    int imp_ticker = 0;
    ImportTable* imp_cache = NULL;

    // we can dispatch these object parsing jobs into separate threads
    uint64_t time = task->time;
    int obj_file_count = 0;
    FOR_N(i, 0, new_count) {
        TB_ArchiveEntry* restrict e = &entries[i];

        if (imp_cache) {
            imp_ticker -= 1;
            if (imp_ticker == 0) {
                cuikperf_region_end();
                mtx_unlock(&imp_cache->lock);
                imp_cache = NULL;
            }
        }

        if (e->import_name.length) {
            // import from DLL:
            //   we don't lock up to insert a new import table but we
            //   do to fill it up, this is probably fine since the filling
            //   process is most likely done by one thread while multiple
            //   threads might be poking multiple separate import tables.
            ImportTable* imp = tb_arena_alloc(&linker_perm_arena, sizeof(ImportTable));
            *imp = (ImportTable){ .libpath = e->name };
            mtx_init(&imp->lock, mtx_plain);

            if (imp_cache == NULL || imp_cache->libpath.length != e->name.length || memcmp(imp_cache->libpath.data, e->name.data, e->name.length) != 0) {
                if (imp_cache != NULL) {
                    mtx_unlock(&imp_cache->lock);
                }

                ImportTable* old = nbhs_intern(&l->imports, imp);
                if (old != imp) {
                    tb_arena_free(&linker_perm_arena, imp, sizeof(ImportTable));
                    imp = old;
                }

                // insert thunk
                mtx_lock(&imp->lock);
                imp_cache = imp;
                imp_ticker = 1000;

                cuikperf_region_start("lock", NULL);
                if (imp->thunks == NULL) {
                    imp->thunks = dyn_array_create(TB_LinkerSymbol*, 4096);
                }
            } else {
                imp = imp_cache;
            }

            // make __imp_ form which refers to raw address
            size_t newlen = e->import_name.length + sizeof("__imp_") - 1;
            uint8_t* newstr = tb_arena_alloc(&linker_perm_arena, newlen);
            memcpy(newstr, "__imp_", sizeof("__imp_"));
            memcpy(newstr + sizeof("__imp_") - 1, e->import_name.data, e->import_name.length);
            newstr[newlen] = 0;

            // log_debug("Import: %.*s (%d)", (int) e->import_name.length, e->import_name.data, e->ordinal);

            CUIK_TIMED_BLOCK_ARGS("archive", (char*) newstr) {
                TB_LinkerSymbol* import_sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *import_sym = (TB_LinkerSymbol){
                    .name   = { newstr, newlen },
                    .tag    = TB_LINKER_SYMBOL_IMPORT,
                    .import = { .table = imp, .ordinal = e->ordinal }
                };
                import_sym = tb_linker_symbol_insert(l, import_sym);

                // make the thunk-like symbol
                TB_LinkerSymbol* sym = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *sym = (TB_LinkerSymbol){
                    .name   = e->import_name,
                    .tag    = TB_LINKER_SYMBOL_THUNK,
                    .thunk  = import_sym
                };
                sym = tb_linker_symbol_insert(l, sym);

                TB_ArchiveEntry* entries = tb_platform_heap_alloc(ar_parser.member_count * sizeof(TB_ArchiveEntry));
                dyn_array_put(imp->thunks, import_sym);
            }
        } else {
            ImportObjTask t = { l, e->name, e->content, time + obj_file_count*65536 };
            obj_file_count += 1;

            if (l->jobs.pool != NULL) {
                tpool_add_task(l->jobs.pool, (tpool_task_proc*) l->vtbl.append_object, sizeof(t), &t);
                l->jobs.count += 1;
            } else {
                l->vtbl.append_object(NULL, &t);
            }
        }
    }

    if (imp_cache) {
        mtx_unlock(&imp_cache->lock);
        cuikperf_region_end();
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }
    cuikperf_region_end();
}

// returns the two new section pieces for the IAT and ILT
static COFF_ImportDirectory* gen_imports(TB_Linker* l, PE_ImageDataDirectory* imp_dir, PE_ImageDataDirectory* iat_dir) {
    // cull unused imports
    size_t j = 0;
    size_t import_entry_count = 0;
    nbhs_for(e, &l->imports) {
        ImportTable* imp = *e;
        DynArray(TB_LinkerSymbol*) tbl = imp->thunks;

        // prune dead thunks
        size_t cnt = 0;
        dyn_array_for(k, tbl) if (tbl[k]->flags & TB_LINKER_SYMBOL_USED) {
            tbl[cnt++] = tbl[k];
        }
        dyn_array_set_length(tbl, cnt);
        imp->thunks = tbl;

        if (cnt > 0) {
            // there's an extra NULL terminator for the import entry lists
            import_entry_count += cnt + 1;
        }
    }

    if (import_entry_count == 0) {
        *imp_dir = (PE_ImageDataDirectory){ 0 };
        *iat_dir = (PE_ImageDataDirectory){ 0 };
        return NULL;
    }

    // Generate import thunks
    uint32_t thunk_id_counter = 0;
    l->trampolines = (TB_Emitter){ 0 };
    nbhs_for(e, &l->imports) {
        ImportTable* imp = *e;

        dyn_array_for(j, imp->thunks) {
            imp->thunks[j]->import.ds_address = l->trampolines.count;
            imp->thunks[j]->import.thunk_id = thunk_id_counter++;

            // TODO(NeGate): This trampoline is x64 specific, we should
            // implement a system to separate this from the core PE export
            // code.
            tb_out1b(&l->trampolines, 0xFF);
            tb_out1b(&l->trampolines, 0x25);
            // we're gonna relocate this to map onto an import thunk later
            tb_out4b(&l->trampolines, 0);
        }
    }

    ////////////////////////////////
    // Generate import table
    ////////////////////////////////
    size_t import_dir_size = (1 + nbhs_count(&l->imports)) * sizeof(COFF_ImportDirectory);
    size_t iat_size = import_entry_count * sizeof(uint64_t);
    size_t total_size = import_dir_size + 2*iat_size;
    nbhs_for(e, &l->imports) {
        ImportTable* imp = *e;
        if (dyn_array_length(imp->thunks) == 0) { continue; }

        total_size += imp->libpath.length + 1;
        dyn_array_for(j, imp->thunks) {
            TB_LinkerSymbol* t = imp->thunks[j];
            total_size += (t->name.length - IMP_PREFIX_LEN) + 3;
        }
    }

    uint8_t* output = tb_platform_heap_alloc(total_size);

    COFF_ImportDirectory* import_dirs = (COFF_ImportDirectory*) &output[0];
    uint64_t* iat = (uint64_t*) &output[import_dir_size];
    uint64_t* ilt = (uint64_t*) &output[import_dir_size + iat_size];
    size_t strtbl_pos = import_dir_size + iat_size*2;

    // We put both the IAT and ILT into the rdata, the PE loader doesn't care but it
    // means the user can't edit these... at least not easily
    TB_LinkerSection* rdata = tb_linker_find_or_create_section(l, sizeof(".rdata") - 1, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

    TB_LinkerSectionPiece* import_piece = tb_linker_append_piece(rdata, PIECE_BUFFER, total_size, 0);
    import_piece->flags |= TB_LINKER_PIECE_LIVE;
    import_piece->buffer_size = total_size;
    import_piece->buffer = output;
    import_piece->offset = rdata->size;
    rdata->size += total_size;

    *imp_dir = (PE_ImageDataDirectory){ import_piece->offset, import_dir_size };
    *iat_dir = (PE_ImageDataDirectory){ import_piece->offset + import_dir_size, iat_size };

    size_t p = 0;
    size_t import_dir_count = 0;
    nbhs_for(e, &l->imports) {
        ImportTable* imp = *e;
        if (dyn_array_length(imp->thunks) == 0) { continue; }

        COFF_ImportDirectory* header = &import_dirs[import_dir_count++];
        TB_Slice lib = imp->libpath;

        // after we resolve RVAs we need to backpatch stuff
        imp->header = header;
        imp->iat = &iat[p], imp->ilt = &ilt[p];

        *header = (COFF_ImportDirectory){
            .import_lookup_table  = import_piece->offset + import_dir_size + iat_size + p*sizeof(uint64_t),
            .import_address_table = import_piece->offset + import_dir_size + p*sizeof(uint64_t),
            .name = import_piece->offset + strtbl_pos,
        };

        memcpy(&output[strtbl_pos], lib.data, lib.length), strtbl_pos += lib.length;
        output[strtbl_pos++] = 0;

        dyn_array_for(j, imp->thunks) {
            TB_LinkerSymbol* t = imp->thunks[j];

            const uint8_t* name = t->name.data + IMP_PREFIX_LEN;
            size_t len = t->name.length - IMP_PREFIX_LEN;

            // import-by-name
            uint64_t value = import_piece->offset + strtbl_pos;
            memcpy(&output[strtbl_pos], &t->import.ordinal, sizeof(uint16_t)), strtbl_pos += 2;
            memcpy(&output[strtbl_pos], name, len), strtbl_pos += len;
            output[strtbl_pos++] = 0;

            // both the ILT and IAT are practically identical at this point
            iat[p] = ilt[p] = value, p++;
        }

        // NULL terminated
        iat[p] = ilt[p] = 0, p++;
    }
    TB_ASSERT(p == import_entry_count);

    // add NULL import directory at the end
    import_dirs[import_dir_count] = (COFF_ImportDirectory){ 0 };

    // trampolines require .text section
    if (l->trampolines.count) {
        TB_LinkerSection* text = tb_linker_find_section(l, ".text");
        TB_ASSERT(text != NULL);

        TB_LinkerSectionPiece* piece = tb_linker_append_piece(text, PIECE_BUFFER, l->trampolines.count, 0);
        piece->flags |= TB_LINKER_PIECE_LIVE;
        piece->buffer = l->trampolines.data;
        piece->offset = text->size;
        text->size += l->trampolines.count;

        l->trampoline_pos = piece->offset;
    }

    return import_dirs;
}

static void pe_init(TB_Linker* l) {
    l->entrypoint = "mainCRTStartup";
    l->subsystem = TB_WIN_SUBSYSTEM_CONSOLE;

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    {
        TB_LinkerSymbol* sym = tb_linker_new_symbol(l, sizeof("__ImageBase") - 1, "__ImageBase");
        sym->tag = TB_LINKER_SYMBOL_IMAGEBASE;
    }

    #define symbol(name_) (tb_linker_new_symbol(l, sizeof(name_) - 1, name_)->tag = TB_LINKER_SYMBOL_ABSOLUTE)
    // This is practically just ripped from LLD
    //   https://github.com/llvm/llvm-project/blob/3d0a5bf7dea509f130c51868361a38daeee7816a/lld/COFF/Driver.cpp#L2192
    symbol("__AbsoluteZero");
    symbol("__volatile_metadata");
    // add_abs("__volatile_metadata");
    // add_abs("__guard_memcpy_fptr");
    symbol("__guard_fids_count");
    symbol("__guard_fids_table");
    symbol("__guard_flags");
    symbol("__guard_iat_count");
    symbol("__guard_iat_table");
    symbol("__guard_longjmp_count");
    symbol("__guard_longjmp_table");
    // Needed for MSVC 2017 15.5 CRT.
    symbol("__enclave_config");
    // Needed for MSVC 2019 16.8 CRT.
    symbol("__guard_eh_cont_count");
    symbol("__guard_eh_cont_table");
    #undef symbol
}
static DynArray(PE_BaseReloc) find_base_relocs(TB_Linker* l, DynArray(TB_LinkerSection*)* inout_sections) {
    DynArray(TB_LinkerSection*) sections = *inout_sections;

    // compile all absolute relocations
    size_t reloc_section_size = 0;
    DynArray(PE_BaseReloc) base_relocs = NULL;
    dyn_array_for(i, sections) {
        size_t section_rva = sections[i]->address;

        uint32_t last_page = UINT32_MAX;

        TB_LinkerSectionPiece* p = atomic_load_explicit(&sections[i]->list, memory_order_relaxed);
        size_t start = dyn_array_length(base_relocs);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if ((p->flags & TB_LINKER_PIECE_LIVE) == 0) { continue; }

            dyn_array_for(j, p->relocs) {
                TB_LinkerReloc* restrict r = &p->relocs[j];
                if (r->type != TB_OBJECT_RELOC_ADDR64) continue;

                PE_BaseReloc br = { p->offset + r->src_offset, sections[i] };
                if (last_page != (br.rva & ~4095)) {
                    if ((reloc_section_size & 3) != 0) {
                        reloc_section_size += 2;
                    }

                    last_page = br.rva & ~4095;
                    reloc_section_size += 8;
                }

                dyn_array_put(base_relocs, br);
                reloc_section_size += 2;
            }
        }
    }

    if (reloc_section_size > 0) {
        TB_LinkerSection* reloc_sec = tb_linker_find_or_create_section(l, sizeof(".reloc") - 1, ".reloc", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        l->main_reloc = tb_linker_append_piece(reloc_sec, PIECE_BUFFER, reloc_section_size, 0);
        l->main_reloc->flags |= TB_LINKER_PIECE_LIVE;
        reloc_sec->size += reloc_section_size;

        dyn_array_put(sections, reloc_sec);
        *inout_sections = sections;
    }

    return base_relocs;
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static bool pe_export(TB_Linker* l, const char* file_name) {
    cuikperf_region_start("linker", NULL);

    if (l->jobs.pool != NULL) {
        // finish up parsing all the object file tasks
        uint64_t old;
        while (old = l->jobs.done, old < l->jobs.count) {
            futex_wait(&l->jobs.done, old);
        }
    }

    PE_ImageDataDirectory imp_dir, iat_dir;
    COFF_ImportDirectory* import_dirs;

    /* for (TB_LinkerThreadInfo* restrict info = l->first_thread_info; info; info = info->next) {
        dyn_array_for(i, info->alternates) {
            TB_Slice to = info->alternates[i].to;
            TB_Slice from = info->alternates[i].from;

            TB_LinkerInternStr to_str = tb_linker_intern_string(l, to.length, (const char*) to.data);
            TB_LinkerInternStr from_str = tb_linker_intern_string(l, from.length, (const char*) from.data);

            TB_LinkerSymbol* old = tb_linker_find_symbol(l, to_str);
            if (old) {
                TB_LinkerSymbol* sym = tb_linker_find_symbol(l, from_str);
                TB_ASSERT(sym->tag == TB_LINKER_SYMBOL_UNKNOWN);

                *sym = *old;
                sym->name = from_str;
            }
        }

        dyn_array_for(i, info->merges) {
            NL_Slice to_name = { info->merges[i].to.length, info->merges[i].to.data };
            ptrdiff_t to = nl_map_get(l->sections, to_name);
            if (to < 0) continue;

            NL_Slice from_name = { info->merges[i].from.length, info->merges[i].from.data };
            ptrdiff_t from = nl_map_get(l->sections, from_name);
            if (from < 0) continue;

            tb__merge_sections(l, l->sections[from].v, l->sections[to].v);
        }
    }*/

    // personally like merging these
    TB_LinkerSection* rdata = tb_linker_find_section(l, ".rdata");
    /* tb_linker_merge_sections(l, tb_linker_find_section(l, ".00cfg"), rdata);
    tb_linker_merge_sections(l, tb_linker_find_section(l, ".idata"), rdata);
    tb_linker_merge_sections(l, tb_linker_find_section(l, ".xdata"), rdata); */

    CUIK_TIMED_BLOCK("resize barrier") {
        nbhs_resize_barrier(&l->symbols);
        nbhs_resize_barrier(&l->sections);
        nbhs_resize_barrier(&l->imports);
    }

    // this will resolve the sections, GC any pieces which aren't used and
    // resolve symbols.
    CUIK_TIMED_BLOCK("Resolve & GC") {
        tb_linker_push_named(l, "_load_config_used");
        tb_linker_push_named(l, "_tls_used");
        tb_linker_mark_live(l);
    }

    DynArray(TB_LinkerSection*) sections = tb__finalize_sections(l);
    if (sections == NULL) {
        cuikperf_region_end();
        return false;
    }

    CUIK_TIMED_BLOCK("generate imports") {
        import_dirs = gen_imports(l, &imp_dir, &iat_dir);
    }

    DynArray(PE_BaseReloc) base_relocs;
    CUIK_TIMED_BLOCK("base relocs") {
        base_relocs = find_base_relocs(l, &sections);
    }

    size_t final_section_count = 0;
    dyn_array_for(i, sections) {
        final_section_count += (sections[i]->generic_flags & TB_LINKER_SECTION_DISCARD) == 0;
    }

    size_t size_of_headers = sizeof(dos_stub)
        + sizeof(uint32_t) // PE magic number
        + sizeof(COFF_FileHeader)
        + sizeof(PE_OptionalHeader64)
        + (final_section_count * sizeof(PE_SectionHeader));

    size_of_headers = align_up(size_of_headers, 512);

    size_t pe_code_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_CODE
    size_t pe_init_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_INITIALIZED_DATA
    size_t pe_uninit_size = 0; // bytes in total marked as IMAGE_SCN_CNT_UNINITIALIZED_DATA

    size_t section_content_size = 0;
    uint64_t virt_addr = align_up(size_of_headers, 4096); // this area is reserved for the PE header stuff
    CUIK_TIMED_BLOCK("layout sections") {
        dyn_array_for(i, sections) {
            TB_LinkerSection* s = sections[i];
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

            if (s->flags & IMAGE_SCN_CNT_CODE) pe_code_size += s->size;
            if (s->flags & IMAGE_SCN_CNT_INITIALIZED_DATA) pe_init_size += s->size;
            if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) pe_uninit_size += s->size;

            s->offset = size_of_headers + section_content_size;
            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                section_content_size += align_up(s->size, 512);
            }

            s->address = virt_addr;
            virt_addr += align_up(s->size, 4096);

            log_debug("Section %.*s: %#x - %#x", (int) s->name.length, s->name.data, s->address, s->address + s->size - 1);
        }
    }

    if (0) {
        // printf("RVA        Name\n");
        nbhs_for(e, &l->symbols) {
            TB_LinkerSymbol* sym = tb_linker_symbol_find(*e);
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL && (sym->flags & TB_LINKER_SYMBOL_USED)) {
                uint64_t rva = tb__get_symbol_rva(l, sym);
                printf("%#08llx   %.*s (%zu : %zu, %zu)\n", rva, (int) sym->name.length, sym->name.data, sym->normal.piece->order[0], sym->normal.piece->order[1], sym->normal.piece->size);
            }
        }
    }

    if (l->main_reloc != NULL) {
        CUIK_TIMED_BLOCK(".reloc") {
            TB_Emitter relocs = { .capacity = l->main_reloc->size, .data = tb_platform_heap_alloc(l->main_reloc->size) };

            // generates .reloc for everyone
            uint32_t last_page = UINT32_MAX;
            int count_offset = 0;
            dyn_array_for(i, base_relocs) {
                size_t actual_pos  = base_relocs[i].section->address + base_relocs[i].rva;
                size_t actual_page = actual_pos & ~4095;

                if (last_page != actual_page) {
                    if ((relocs.count & 3) != 0) {
                        // pad to 4 bytes
                        tb_out2b(&relocs, 0x0);
                        *((uint32_t*) &relocs.data[count_offset]) += 2;
                    }

                    last_page = actual_page;
                    count_offset = relocs.count + 4;

                    tb_out4b(&relocs, actual_page);
                    tb_out4b(&relocs, 8); // block size field (includes RVA and itself)
                }

                tb_out2b(&relocs, 0xA000 | (actual_pos & 4095));
                *((uint32_t*) &relocs.data[count_offset]) += 2;
            }

            assert(l->main_reloc->size == relocs.count);
            l->main_reloc->buffer = relocs.data;
            l->main_reloc->buffer_size = relocs.count;
        }
    }

    TB_LinkerSection* text = tb_linker_find_section(l, ".text");
    CUIK_TIMED_BLOCK("sort .pdata") {
        TB_LinkerSection* pdata = tb_linker_find_section(l, ".pdata");
        uint8_t* pdata_buffer   = tb_platform_heap_alloc(pdata->size);

        TB_LinkerSectionPiece* p = atomic_load_explicit(&pdata->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            assert(p->kind == PIECE_BUFFER);

            size_t rem = 0;
            uint8_t* out = &pdata_buffer[p->offset];
            if (p->buffer) {
                memcpy(out, p->buffer, p->buffer_size);
                rem = p->buffer_size;
            }

            // zero the remaining space (or CC if it's code)
            if (rem < p->size) {
                int b = (p->flags & TB_LINKER_PIECE_CODE) ? 0xCC : 0;
                memset(&out[rem], b, p->size - rem);
            }

            tb_linker_apply_reloc(l, p, out, pdata->address, text->address + l->trampoline_pos, 0, 0, p->size);
        }
        qsort(pdata_buffer, pdata->size / sizeof(uint32_t[3]), sizeof(uint32_t[3]), compare_rva);

        TB_LinkerSectionPiece* piece = tb_platform_heap_alloc(sizeof(TB_LinkerSectionPiece));
        *piece = (TB_LinkerSectionPiece){
            .kind        = PIECE_BUFFER,
            .flags       = TB_LINKER_PIECE_LIVE,
            .parent      = pdata,
            .size        = pdata->size,
            .buffer      = pdata_buffer,
            .buffer_size = pdata->size,
        };
        pdata->list = piece;
        pdata->piece_count = 1;
    }

    TB_LinkerSection* data = tb_linker_find_section(l, ".data");
    if (import_dirs != NULL) {
        iat_dir.virtual_address += rdata->address;
        imp_dir.virtual_address += rdata->address;
        CUIK_TIMED_BLOCK("relocate imports and trampolines") {
            nbhs_for(e, &l->imports) {
                ImportTable* imp = *e;
                if (dyn_array_length(imp->thunks) == 0) { continue; }

                COFF_ImportDirectory* header = imp->header;
                header->import_lookup_table  += rdata->address;
                header->import_address_table += rdata->address;
                header->name += rdata->address;

                uint64_t *ilt = imp->ilt, *iat = imp->iat;
                uint64_t iat_rva = header->import_address_table;
                uint64_t trampoline_rva = text->address + l->trampoline_pos;

                dyn_array_for(j, imp->thunks) {
                    if (iat[j] != 0) {
                        iat[j] += rdata->address;
                        ilt[j] += rdata->address;
                    }

                    // Relocate trampoline entries to point into the IAT, the PE loader will
                    // fill these slots in with absolute addresses of the symbols.
                    int32_t* trampoline_dst = (int32_t*) &l->trampolines.data[imp->thunks[j]->import.ds_address + 2];
                    TB_ASSERT(*trampoline_dst == 0 && "We set this earlier... why isn't it here?");

                    uint32_t target_rva = iat_rva + j*8;
                    uint32_t source_pos = trampoline_rva + imp->thunks[j]->import.ds_address + 6;
                    (*trampoline_dst) += target_rva - source_pos;
                }
            }
        }
        l->iat_pos = iat_dir.virtual_address;
    }

    size_t output_size = size_of_headers + section_content_size;
    COFF_FileHeader header = {
        .machine = 0x8664,
        .section_count = final_section_count,
        .timestamp = 1056582000u, // my birthday since that's consistent :P
        .symbol_table = 0,
        .symbol_count = 0,
        .optional_header_size = sizeof(PE_OptionalHeader64),
        .flags = 0x2 | 0x0020 /* executable, >2GB */
    };

    if (l->subsystem == TB_WIN_SUBSYSTEM_EFI_APP) {
        header.flags |= 0x2000; // DLL
    }

    static const uint32_t subsys[] = {
        [TB_WIN_SUBSYSTEM_WINDOWS] = IMAGE_SUBSYSTEM_WINDOWS_GUI,
        [TB_WIN_SUBSYSTEM_CONSOLE] = IMAGE_SUBSYSTEM_WINDOWS_CUI,
        [TB_WIN_SUBSYSTEM_EFI_APP] = IMAGE_SUBSYSTEM_EFI_APPLICATION,
    };

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,
        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x140000000,

        .size_of_code = pe_code_size,
        .size_of_initialized_data = pe_init_size,
        .size_of_uninitialized_data = pe_uninit_size,

        .size_of_image = virt_addr,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = subsys[l->subsystem],
        .dll_characteristics = 0x40 | 0x20 | 0x0100 | 0x8000, /* dynamic base, high entropy, nx compat, terminal server aware */

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = imp_dir,
            [IMAGE_DIRECTORY_ENTRY_IAT] = iat_dir,
        }
    };

    if (l->subsystem != TB_WIN_SUBSYSTEM_EFI_APP) {
        opt_header.major_os_ver = 6;
        opt_header.minor_os_ver = 0;
        opt_header.major_subsystem_ver = 6;
        opt_header.minor_subsystem_ver = 0;
    }

    TB_LinkerSymbol* tls_used_sym = tb_linker_find_symbol2(l, "_tls_used");
    if (tls_used_sym && (tls_used_sym->flags & TB_LINKER_SYMBOL_USED)) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_TLS] = (PE_ImageDataDirectory){ tb__get_symbol_rva(l, tls_used_sym), sizeof(PE_TLSDirectory) };
    }

    TB_LinkerSymbol* load_config_used_sym = tb_linker_find_symbol2(l, "_load_config_used");
    if (load_config_used_sym && (load_config_used_sym->flags & TB_LINKER_SYMBOL_USED)) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG] = (PE_ImageDataDirectory){ tb__get_symbol_rva(l, load_config_used_sym), 0x140 };
    }

    TB_LinkerSection* pdata = tb_linker_find_section(l, ".pdata");
    if (pdata) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_EXCEPTION] = (PE_ImageDataDirectory){ pdata->address, pdata->size };
    }

    TB_LinkerSection* reloc = tb_linker_find_section(l, ".reloc");
    if (reloc) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_BASERELOC] = (PE_ImageDataDirectory){ reloc->address, reloc->size };
    }

    // text section crap
    if (text) {
        opt_header.base_of_code = text->address;
        opt_header.size_of_code = align_up(text->size, 4096);

        TB_LinkerSymbol* sym = tb_linker_find_symbol2(l, l->entrypoint);
        if (sym) {
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
                opt_header.entrypoint = text->address + sym->normal.piece->offset + sym->normal.secrel;
            } else if (sym->tag == TB_LINKER_SYMBOL_TB) {
                opt_header.entrypoint = text->address + sym->tb.piece->offset + tb__get_symbol_pos(sym->tb.sym);
            } else {
                tb_todo();
            }
        } else {
            printf("tblink: could not find entrypoint! %s\n", l->entrypoint);
        }
    }

    // do the final exporting, we can thread this
    {
        HANDLE file = CreateFileA(file_name, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);

        HANDLE mapping = CreateFileMappingA(file, NULL, PAGE_READWRITE, 0, output_size, NULL);
        if (mapping == NULL) {
            printf("tblink: could not map file! %s", file_name);
            cuikperf_region_end();
            return false;
        }

        uint8_t* restrict output = MapViewOfFileEx(mapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, output_size, NULL);
        if (output == NULL) {
            printf("tblink: could not view mapped file! %s", file_name);
            cuikperf_region_end();
            return false;
        }

        size_t write_pos = 0;
        uint32_t pe_magic = 0x00004550;
        WRITE(dos_stub,    sizeof(dos_stub));
        WRITE(&pe_magic,   sizeof(pe_magic));
        WRITE(&header,     sizeof(header));
        WRITE(&opt_header, sizeof(opt_header));
        dyn_array_for(i, sections) {
            TB_LinkerSection* s = sections[i];
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) { continue; }

            PE_SectionHeader sec_header = {
                .virtual_size    = s->size,
                .virtual_address = s->address,
                .characteristics = s->flags & ~IMAGE_SCN_LNK_COMDAT,
            };

            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                sec_header.pointer_to_raw_data = s->offset;
                sec_header.size_of_raw_data = align_up(s->size, 512);
            }

            size_t len = s->name.length;
            memcpy(sec_header.name, s->name.data, len > 8 ? 8 : len);
            WRITE(&sec_header, sizeof(sec_header));
        }
        write_pos = tb__pad_file(output, write_pos, 0x00, 0x200);

        if (l->jobs.pool != NULL) {
            l->jobs.done = 0;
            l->jobs.count = 0;

            // each of the pieces can be exported in parallel
            cuikperf_region_start("submitting", NULL);
            dyn_array_for(i, sections) {
                TB_LinkerSectionPiece* p = atomic_load_explicit(&sections[i]->list, memory_order_relaxed);
                int c = 0;
                while (p != NULL) {
                    if (p->flags & TB_LINKER_PIECE_LIVE) {
                        ExportTask task = { l, output, p };
                        tpool_add_task(l->jobs.pool, (tpool_task_proc*) tb_linker_export_piece, sizeof(task), &task);
                        c += 1;
                    }
                    p = atomic_load_explicit(&p->next, memory_order_relaxed);
                }
                l->jobs.count += c;
            }
            cuikperf_region_end();

            // finish up exporting
            futex_wait_eq(&l->jobs.done, l->jobs.count);
        } else {
            dyn_array_for(i, sections) {
                size_t section_file_offset = sections[i]->offset;
                size_t section_rva = sections[i]->address;

                TB_LinkerSectionPiece* p = atomic_load_explicit(&sections[i]->list, memory_order_relaxed);
                while (p != NULL) {
                    if (p->flags & TB_LINKER_PIECE_LIVE) {
                        ExportTask task = { l, output, p };
                        tb_linker_export_piece(NULL, &task);
                    }
                    p = atomic_load_explicit(&p->next, memory_order_relaxed);
                }
            }
        }

        UnmapViewOfFile(output);
        CloseHandle(mapping);
        CloseHandle(file);
    }

    cuikperf_region_end();
    return true;
}

TB_LinkerVtbl tb__linker_pe = {
    .init           = pe_init,
    .append_object  = pe_append_object,
    .append_library = pe_append_library,
    .export         = pe_export
};
