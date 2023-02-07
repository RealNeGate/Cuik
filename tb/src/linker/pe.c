#define NL_STRING_MAP_IMPL
#include "linker.h"
#include "../objects/coff.h"
#include "../objects/lib_parse.h"

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

void append_object(TB_Linker* l, TB_Slice obj_name, TB_ObjectFile* obj) {
    CUIK_TIMED_BLOCK("sort sections") {
        qsort(obj->sections, obj->section_count, sizeof(TB_ObjectSection), compare_sections);
    }

    // Apply all sections (generate lookup for sections based on ordinals)
    TB_ObjectSection** og_sort = tb_platform_heap_alloc(obj->section_count * sizeof(TB_ObjectSection*));
    FOREACH_N(i, 0, obj->section_count) {
        TB_ObjectSection* s = &obj->sections[i];
        og_sort[s->ordinal] = s;

        if (s->name.length && s->name.data[0] == '_') continue;
        if (s->name.length >= 8) continue;

        // trim the dollar sign (if applies)
        FOREACH_N(j, 0, s->name.length) {
            if (s->name.data[j] == '$') {
                s->name.length = j;
                break;
            }
        }

        TB_LinkerSection* ls = tb__find_or_create_section2(l, s->name.length, s->name.data, s->flags);
        TB_LinkerSectionPiece* p = tb__append_piece(ls, PIECE_NORMAL, s->raw_data.length, s->raw_data.data, NULL);
        s->user_data = p;
        p->vsize = s->virtual_size;
    }

    // Append all symbols
    CUIK_TIMED_BLOCK("apply symbols") {
        FOREACH_N(i, 0, obj->symbol_count) {
            TB_ObjectSymbol* restrict sym = &obj->symbols[i];

            if (sym->section_num > 0) {
                // TB_Slice sec_name = og_sort[sym->section_num - 1]->name;
                // printf("    %.*s = %.*s:%d\n", (int) sym->name.length, sym->name.data, (int) sec_name.length, sec_name.data, sym->value);

                TB_LinkerSymbol s = {
                    .name = sym->name,
                    .tag = TB_LINKER_SYMBOL_NORMAL,
                    .object_name = obj_name,
                    .normal = { og_sort[sym->section_num - 1]->user_data, sym->value }
                };
                sym->user_data = tb__append_symbol(&l->symtab, &s);
            }
        }
    }

    CUIK_TIMED_BLOCK("parse relocations") {
        FOREACH_N(i, 0, obj->section_count) {
            TB_ObjectSection* restrict s = &obj->sections[i];

            // some relocations point to sections within the same object file, we resolve
            // their symbols early.
            FOREACH_N(j, 0, s->relocation_count) {
                TB_ObjectReloc* restrict reloc = &s->relocations[j];

                TB_ObjectSymbol* restrict src_symbol = &obj->symbols[reloc->symbol_index];
                if (src_symbol->type == TB_OBJECT_STORAGE_STATIC || src_symbol->type == TB_OBJECT_STORAGE_EXTERN) {
                    tb_todo();
                }
            }
        }
    }
}

static void append_library(TB_Linker* l, TB_Slice ar_file) {
    TB_ArchiveFileParser ar_parser = { 0 };
    if (!tb_archive_parse(ar_file, &ar_parser)) {
        return;
    }

    TB_ArchiveEntry* restrict entries = tb_platform_heap_alloc(ar_parser.member_count * sizeof(TB_ArchiveEntry));
    size_t new_count = tb_archive_parse_entries(&ar_parser, 0, ar_parser.member_count, entries);

    FOREACH_N(i, 0, new_count) {
        TB_ArchiveEntry* restrict e = &entries[i];

        if (e->import_name.length) {
            // import from DLL
            TB_Slice libname = e->name;
            ptrdiff_t import_index = -1;
            dyn_array_for(j, l->imports) {
                ImportTable* table = &l->imports[j];

                if (table->libpath.length == libname.length &&
                    memcmp(table->libpath.data, libname.data, libname.length) == 0) {
                    import_index = j;
                    break;
                }
            }

            if (import_index < 0) {
                // we haven't used this DLL yet, make an import table for it
                import_index = dyn_array_length(l->imports);

                ImportTable t = {
                    .libpath = libname,
                    .thunks = dyn_array_create(ImportThunk, 4096)
                };
                dyn_array_put(l->imports, t);
            }

            TB_LinkerSymbol sym = {
                .name = e->import_name,
                .tag = TB_LINKER_SYMBOL_IMPORT,
                // .object_name = obj_name,
                .import = { import_index, e->ordinal }
            };
            tb__append_symbol(&l->symtab, &sym);
        } else {
            // fprintf(stderr, "%.*s\n", (int) e->name.length, e->name.data);
            CUIK_TIMED_BLOCK("append object file") {
                append_object(l, e->name, e->obj);
                tb_object_free(e->obj);
            }
        }
    }

    tb_platform_heap_free(entries);
}

static void append_module(TB_Linker* l, TB_Module* m) {
    // Convert module into sections which we can then append to the output
    TB_LinkerSection* text = tb__find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    m->linker.text = tb__append_piece(text, PIECE_TEXT, tb__layout_text_section(m), NULL, m);

    if (m->data_region_size > 0) {
        TB_LinkerSection* data = tb__find_or_create_section(l, ".data", IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        m->linker.data = tb__append_piece(data, PIECE_DATA, m->data_region_size, NULL, m);
    }

    TB_LinkerSection* rdata = NULL;
    if (m->rdata_region_size > 0) {
        rdata = tb__find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        m->linker.rdata = tb__append_piece(rdata, PIECE_RDATA, m->rdata_region_size, NULL, m);
    }

    if (m->compiled_function_count > 0) {
        if (rdata) {
            rdata = tb__find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        }

        CUIK_TIMED_BLOCK("generate xdata") {
            TB_Emitter xdata = { 0 };
            const ICodeGen* restrict code_gen = tb__find_code_generator(m);

            TB_FOR_FUNCTIONS(f, m) {
                TB_FunctionOutput* out_f = f->output;

                if (out_f != NULL) {
                    out_f->unwind_info = xdata.count;
                    code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->prologue_epilogue_metadata, out_f->stack_usage);
                }
            }

            TB_LinkerSectionPiece* x = tb__append_piece(rdata, PIECE_NORMAL, xdata.count, xdata.data, m);
            TB_FOR_FUNCTIONS(f, m) {
                TB_FunctionOutput* out_f = f->output;

                if (out_f != NULL) out_f->unwind_info += x->offset;
            }
        }

        TB_LinkerSection* pdata = tb__find_or_create_section(l, ".pdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        tb__append_piece(pdata, PIECE_PDATA, m->compiled_function_count * 12, NULL, m);
    }

    if (m->data_region_size > 0) CUIK_TIMED_BLOCK(".reloc") {
        uint32_t last_page = 0, reloc_size = 0;
        FOREACH_N(i, 0, m->max_threads) {
            pool_for(TB_Global, g, m->thread_info[i].globals) {
                TB_Initializer* init = g->init;

                FOREACH_N(k, 0, init->obj_count) {
                    size_t actual_page = g->pos + init->objects[k].offset;

                    if (init->objects[k].type == TB_INIT_OBJ_RELOC) {
                        if (last_page != actual_page) {
                            last_page = actual_page;
                            reloc_size += 8;
                        }

                        reloc_size += 2;
                    }
                }
            }
        }

        if (reloc_size > 0) {
            TB_LinkerSection* reloc = tb__find_or_create_section(l, ".reloc", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
            tb__append_piece(reloc, PIECE_RELOC, reloc_size, NULL, m);
        }
    }

    CUIK_TIMED_BLOCK("apply symbols") {
        static const enum TB_SymbolTag tags[] = { TB_SYMBOL_FUNCTION, TB_SYMBOL_GLOBAL };
        TB_Slice obj_name = { sizeof("<tb module>")-1, (const uint8_t*) "<tb module>" };

        FOREACH_N(i, 0, COUNTOF(tags)) {
            enum TB_SymbolTag tag = tags[i];
            TB_LinkerSectionPiece* piece = i ? m->linker.data : m->linker.text;

            for (TB_Symbol* sym = m->first_symbol_of_tag[tag]; sym != NULL; sym = sym->next) {
                TB_LinkerSymbol ls = {
                    .name = { strlen(sym->name), (const uint8_t*) sym->name },
                    .tag = TB_LINKER_SYMBOL_TB,
                    .object_name = obj_name,
                    .tb = { piece, sym }
                };
                tb__append_symbol(&l->symtab, &ls);
            }
        }
    }

    dyn_array_put(l->ir_modules, m);
}

void tb__apply_external_relocs(TB_Linker* l, TB_Module* m, uint8_t* output) {
    // ptrdiff_t global_data_rva = 0x1000 + import_table.count;
    TB_LinkerSection* text  = tb__find_section(l, ".text",  IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* data  = tb__find_section(l, ".data",  IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSection* rdata = tb__find_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

    FOREACH_N(i, 0, m->max_threads) {
        uint64_t text_piece_rva = text->address + m->linker.text->offset;
        uint64_t text_piece_file = text->offset + m->linker.text->offset;

        uint64_t trampoline_rva = text->address + l->trampoline_pos;

        uint64_t data_piece_rva = 0;
        if (m->linker.data) {
            data_piece_rva = data->address + m->linker.data->offset;
        }

        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].symbol_patches)) {
            TB_SymbolPatch* patch = &m->thread_info[i].symbol_patches[j];

            if (patch->target->tag == TB_SYMBOL_EXTERNAL) {
                TB_FunctionOutput* out_f = patch->source->output;
                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                ImportThunk* thunk = patch->target->address;
                assert(thunk != NULL);

                int32_t p = (trampoline_rva + (thunk->thunk_id * 6)) - actual_pos;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                (*dst) += p;
            } else if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                // internal patching has already handled this
            } else if (patch->target->tag == TB_SYMBOL_GLOBAL) {
                TB_FunctionOutput* out_f = patch->source->output;
                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                TB_Global* global = (TB_Global*) patch->target;
                (void)global;
                assert(global->super.tag == TB_SYMBOL_GLOBAL);

                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];
                if (global->storage == TB_STORAGE_DATA) {
                    int32_t p = (data_piece_rva + global->pos) - actual_pos;

                    (*dst) += p;
                } else if (global->storage == TB_STORAGE_TLS) {
                    (*dst) += (data_piece_rva + global->pos);
                } else {
                    tb_todo();
                }
            } else {
                tb_todo();
            }
        }

        if (m->linker.rdata) {
            uint64_t rdata_piece_rva = rdata->address + m->linker.rdata->offset;

            FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                TB_ConstPoolPatch* patch = &m->thread_info[i].const_patches[j];
                TB_FunctionOutput* out_f = patch->source->output;

                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                // relocations add not replace
                (*dst) += (rdata_piece_rva - actual_pos);
            }
        }
    }
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

// returns the two new section pieces for the IAT and ILT
static COFF_ImportDirectory* gen_imports(TB_Linker* l, PE_ImageDataDirectory* imp_dir, PE_ImageDataDirectory* iat_dir) {
    int errors = 0;

    CUIK_TIMED_BLOCK("generate thunks from TB modules") {
        dyn_array_for(j, l->ir_modules) {
            TB_Module* m = l->ir_modules[j];

            // Find all the imports & place them into the right buckets
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    TB_Slice name = { strlen(ext->super.name), (uint8_t*) ext->super.name };
                    TB_LinkerSymbol* sym = tb__find_symbol(&l->symtab, name);
                    if (sym == NULL) {
                        if (strcmp(ext->super.name, "_tls_index") == 0) {
                            continue;
                        } else {
                            printf("tblink: unresolved external: %s\n", ext->super.name);
                            errors++;
                            continue;
                        }
                    }

                    if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
                        ImportTable* table = &l->imports[sym->import.id];
                        ImportThunk t = { .name = name, .ordinal = sym->import.ordinal };

                        dyn_array_put(table->thunks, t);
                        ext->super.address = &table->thunks[dyn_array_length(table->thunks) - 1];
                    } else {
                        tb_todo();
                    }
                }
            }
        }
    }

    if (errors > 0) {
        // tb_panic("Failed to link with errors!\n");
    }

    // cull any import directory
    size_t j = 0;
    size_t import_entry_count = 0;
    dyn_array_for(i, l->imports) {
        if (dyn_array_length(l->imports[i].thunks) != 0) {
            if (i != j) {
                l->imports[j] = l->imports[i];
            }
            j += 1;

            // there's an extra NULL terminator for the import entry lists
            import_entry_count += dyn_array_length(l->imports[i].thunks) + 1;
        }
    }
    dyn_array_set_length(l->imports, j); // trimmed

    // Generate import thunks
    uint32_t thunk_id_counter = 0;
    l->trampolines = (TB_Emitter){ 0 };
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];

        dyn_array_for(j, imp->thunks) {
            imp->thunks[j].ds_address = l->trampolines.count;
            imp->thunks[j].thunk_id = thunk_id_counter++;

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
    size_t import_dir_size = (1 + dyn_array_length(l->imports)) * sizeof(COFF_ImportDirectory);
    size_t iat_size = import_entry_count * sizeof(uint64_t);
    size_t total_size = import_dir_size + 2*iat_size;
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];
        total_size += imp->libpath.length + 1;

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            total_size += t->name.length + 3;
        }
    }

    uint8_t* output = tb_platform_heap_alloc(total_size);

    COFF_ImportDirectory* import_dirs = (COFF_ImportDirectory*) &output[0];
    uint64_t* iat = (uint64_t*) &output[import_dir_size];
    uint64_t* ilt = (uint64_t*) &output[import_dir_size + iat_size];
    size_t strtbl_pos = import_dir_size + iat_size*2;

    // We put both the IAT and ILT into the rdata, the PE loader doesn't care but it
    // means the user can't edit these... at least not easily
    TB_LinkerSection* rdata = tb__find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSectionPiece* import_piece = tb__append_piece(rdata, PIECE_NORMAL, total_size, output, NULL);

    *imp_dir = (PE_ImageDataDirectory){ import_piece->offset, import_dir_size };
    *iat_dir = (PE_ImageDataDirectory){ import_piece->offset + import_dir_size, iat_size };

    size_t p = 0;
    dyn_array_for(i, l->imports) {
        ImportTable* imp = &l->imports[i];
        COFF_ImportDirectory* header = &import_dirs[i];
        TB_Slice lib = imp->libpath;

        // after we resolve RVAs we need to backpatch stuff
        imp->iat = &iat[p], imp->ilt = &ilt[p];

        *header = (COFF_ImportDirectory){
            .import_lookup_table  = import_piece->offset + import_dir_size + iat_size + p*sizeof(uint64_t),
            .import_address_table = import_piece->offset + import_dir_size + p*sizeof(uint64_t),
            .name = import_piece->offset + strtbl_pos,
        };

        memcpy(&output[strtbl_pos], lib.data, lib.length), strtbl_pos += lib.length;
        output[strtbl_pos++] = 0;

        dyn_array_for(j, imp->thunks) {
            ImportThunk* t = &imp->thunks[j];
            // printf("  %.*s\n", (int) t->name.length, t->name.data);

            // import-by-name
            uint64_t value = import_piece->offset + strtbl_pos;
            memcpy(&output[strtbl_pos], &t->ordinal, sizeof(uint16_t)), strtbl_pos += 2;
            memcpy(&output[strtbl_pos], t->name.data, t->name.length), strtbl_pos += t->name.length;
            output[strtbl_pos++] = 0;

            // both the ILT and IAT are practically identical at this point
            iat[p] = ilt[p] = value, p++;
        }

        // NULL terminated
        iat[p] = ilt[p] = 0, p++;
    }
    assert(p == import_entry_count);

    // add NULL import directory at the end
    import_dirs[dyn_array_length(l->imports)] = (COFF_ImportDirectory){ 0 };

    {
        TB_LinkerSection* text = tb__find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
        TB_LinkerSectionPiece* piece = tb__append_piece(text, PIECE_NORMAL, l->trampolines.count, l->trampolines.data, NULL);
        l->trampoline_pos = piece->offset;
    }
    return import_dirs;
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static TB_Exports export(TB_Linker* l) {
    PE_ImageDataDirectory imp_dir, iat_dir;
    COFF_ImportDirectory* import_dirs;

    if (l->entrypoint < 0) {
        TB_Slice name = { sizeof("mainCRTStartup") - 1, (const uint8_t*) "mainCRTStartup" };
        TB_LinkerSymbol* sym = tb__find_symbol(&l->symtab, name);
        if (sym) {
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
                l->entrypoint = sym->normal.piece->offset + sym->normal.secrel;
            } else if (sym->tag == TB_LINKER_SYMBOL_TB) {
                l->entrypoint = sym->tb.piece->offset + tb__get_symbol_pos(sym->tb.sym);
            }
        }
    }

    CUIK_TIMED_BLOCK("generate imports") {
        import_dirs = gen_imports(l, &imp_dir, &iat_dir);
    }

    size_t size_of_headers =
        sizeof(dos_stub)
        + sizeof(uint32_t) // PE magic number
        + sizeof(COFF_FileHeader)
        + sizeof(PE_OptionalHeader64)
        + (nl_strmap_get_load(l->sections) * sizeof(PE_SectionHeader));

    size_of_headers = align_up(size_of_headers, 512);

    size_t pe_code_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_CODE
    size_t pe_init_size   = 0; // bytes in total marked as IMAGE_SCN_CNT_INITIALIZED_DATA
    size_t pe_uninit_size = 0; // bytes in total marked as IMAGE_SCN_CNT_UNINITIALIZED_DATA

    size_t section_content_size = 0;
    uint64_t virt_addr = 0x1000; // this area is reserved for the PE header stuff
    CUIK_TIMED_BLOCK("layout sections") {
        nl_strmap_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i];
            if (s->flags & IMAGE_SCN_CNT_CODE) pe_code_size += s->total_size;
            if (s->flags & IMAGE_SCN_CNT_INITIALIZED_DATA) pe_init_size += s->total_size;
            if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) pe_uninit_size += s->total_size;

            s->offset = size_of_headers + section_content_size;
            section_content_size += align_up(s->total_size, 512);

            s->address = virt_addr;
            virt_addr += align_up(s->total_size, 4096);
        }
    }

    TB_LinkerSection* text  = tb__find_or_create_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* rdata = tb__find_or_create_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSection* data  = tb__find_or_create_section(l, ".data", IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    iat_dir.virtual_address += rdata->address;
    imp_dir.virtual_address += rdata->address;
    CUIK_TIMED_BLOCK("relocate imports and trampolines") {
        dyn_array_for(i, l->imports) {
            ImportTable* imp = &l->imports[i];
            COFF_ImportDirectory* header = &import_dirs[i];
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
                int32_t* trampoline_dst = (int32_t*) &l->trampolines.data[imp->thunks[j].ds_address + 2];
                assert(*trampoline_dst == 0 && "We set this earlier... why isn't it here?");

                (*trampoline_dst) += (iat_rva + j*8) - (trampoline_rva + imp->thunks[j].ds_address + 6);
            }
        }
    }

    size_t output_size = size_of_headers + section_content_size;
    COFF_FileHeader header = {
        .machine = 0x8664,
        .section_count = nl_strmap_get_load(l->sections),
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .optional_header_size = sizeof(PE_OptionalHeader64),
        .flags = 0x2 /* executable */
    };

    PE_OptionalHeader64 opt_header = {
        .magic = 0x20b,
        .section_alignment = 0x1000,
        .file_alignment = 0x200,

        .image_base = 0x140000000,

        .size_of_code = pe_code_size,
        .size_of_initialized_data = pe_init_size,
        .size_of_uninitialized_data = pe_uninit_size,

        // 6.0
        .major_os_ver = 6,
        .minor_os_ver = 0,

        // 6.0
        .major_subsystem_ver = 6,
        .minor_subsystem_ver = 0,

        .size_of_image = virt_addr,
        .size_of_headers = (size_of_headers + 0x1FF) & ~0x1FF,
        .subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI,
        .dll_characteristics = 0x40 | 0x20, /* dynamic base, high entropy */

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = imp_dir,
            [IMAGE_DIRECTORY_ENTRY_IAT] = iat_dir,
        }
    };

    TB_LinkerSection* pdata = tb__find_section(l, ".pdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    if (pdata) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_EXCEPTION] = (PE_ImageDataDirectory){ pdata->address, pdata->total_size };
    }

    TB_LinkerSection* reloc = tb__find_section(l, ".reloc", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    if (reloc) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_BASERELOC] = (PE_ImageDataDirectory){ reloc->address, reloc->total_size };
    }

    // text section crap
    if (text) {
        opt_header.base_of_code = text->address;
        opt_header.size_of_code = align_up(text->total_size, 4096);

        if (l->entrypoint >= 0) {
            opt_header.entrypoint = text->address + l->entrypoint;
        } else {
            printf("tblink: could not find entrypoint!\n");
        }
    }

    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    uint32_t pe_magic = 0x00004550;
    WRITE(dos_stub,    sizeof(dos_stub));
    WRITE(&pe_magic,   sizeof(pe_magic));
    WRITE(&header,     sizeof(header));
    WRITE(&opt_header, sizeof(opt_header));

    nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        PE_SectionHeader sec_header = {
            .virtual_size = align_up(s->total_size, 4096),
            .virtual_address = s->address,
            .pointer_to_raw_data = s->offset,
            .size_of_raw_data = s->total_size,
            .characteristics = s->flags,
        };

        assert(s->name.length < 8);
        memcpy(sec_header.name, s->name.data, s->name.length);
        sec_header.name[s->name.length] = 0;

        WRITE(&sec_header, sizeof(sec_header));
    }
    write_pos = tb__pad_file(output, write_pos, 0x00, 0x200);

    tb__apply_section_contents(l, output, write_pos, text, data, rdata, 512);

    // TODO(NeGate): multithread this too
    CUIK_TIMED_BLOCK("apply final relocations") {
        dyn_array_for(i, l->ir_modules) {
            tb__apply_external_relocs(l, l->ir_modules[i], output);
        }
    }

    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

TB_LinkerVtbl tb__linker_pe = {
    .append_object  = append_object,
    .append_library = append_library,
    .append_module  = append_module,
    .export         = export
};
