#define NL_STRING_MAP_IMPL
#include "linker.h"
#include "../objects/coff.h"
#include "../objects/lib_parse.h"

typedef struct {
    uint16_t page_rva;
    uint16_t block_size; // includes the header
    uint16_t payload[];
} BaseRelocSegment;

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

// fuck you. *de__imp_ your names*
static TB_Slice deimp_your_names(TB_Slice name, bool* is_thunk) {
    // convert __imp_ names into DLL compatible ones
    size_t imp_prefix_len = sizeof("__imp_")-1;
    if (name.length >= imp_prefix_len && memcmp(name.data, "__imp_", imp_prefix_len) == 0) {
        name.data += imp_prefix_len;
        name.length -= imp_prefix_len;
        *is_thunk = true;
    }

    return name;
}

void append_object(TB_Linker* l, TB_Slice obj_name, TB_ObjectFile* obj) {
    // Apply all sections (generate lookup for sections based on ordinals)
    TB_ObjectSection** og_sort = tb_platform_heap_alloc(obj->section_count * sizeof(TB_ObjectSection*));
    FOREACH_N(i, 0, obj->section_count) {
        TB_ObjectSection* s = &obj->sections[i];
        og_sort[s->ordinal] = s;

        // trim the dollar sign (if applies)
        uint32_t order = 0;
        FOREACH_N(j, 0, s->name.length) {
            if (s->name.data[j] == '$') {
                // convert letters into score
                FOREACH_N(k, j + 1, s->name.length) {
                    order <<= 8;
                    order += s->name.data[i];
                }

                s->name.length = j;
                break;
            }
        }

        // remove all the alignment flags, they don't appear in linker sections
        TB_LinkerSection* ls = tb__find_or_create_section2(
            l, s->name.length, s->name.data, s->flags & ~0x00F00000
        );

        if (s->flags & IMAGE_SCN_LNK_REMOVE) {
            ls->generic_flags |= TB_LINKER_SECTION_DISCARD;
        }

        if (s->flags & IMAGE_SCN_LNK_COMDAT) {
            ls->generic_flags |= TB_LINKER_SECTION_COMDAT;
        }

        const void* raw_data = s->raw_data.data;
        if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) {
            raw_data = NULL;
        }

        TB_LinkerSectionPiece* p = tb__append_piece(ls, PIECE_NORMAL, s->raw_data.length, raw_data, NULL);
        s->user_data = p;

        p->order = order;
        p->flags = 1;
        p->vsize = s->virtual_size;
    }

    // Append all symbols
    CUIK_TIMED_BLOCK("apply symbols") {
        COFF_AuxSectionSymbol* comdat_aux = NULL;

        FOREACH_N(i, 0, obj->symbol_count) {
            TB_ObjectSymbol* restrict sym = &obj->symbols[i];

            if (sym->section_num > 0) {
                // TB_Slice sec_name = og_sort[sym->section_num - 1]->name;
                // printf("    %.*s = %.*s:%d\n", (int) sym->name.length, sym->name.data, (int) sec_name.length, sec_name.data, sym->value);

                TB_ObjectSection* sec = og_sort[sym->section_num - 1];
                if (sec->name.length == sym->name.length && memcmp(sym->name.data, sec->name.data, sym->name.length) == 0) {
                    // we're a section symbol
                    // COMDAT is how linkers handle merging of inline functions in C++
                    if ((sec->flags & IMAGE_SCN_LNK_COMDAT)) {
                        // the next symbol is the actual COMDAT symbol
                        comdat_aux = sym->extra;
                    }
                } else {
                    TB_LinkerSectionPiece* p = sec->user_data;
                    assert(p != NULL);

                    TB_LinkerSymbol s = {
                        .name = sym->name,
                        .tag = TB_LINKER_SYMBOL_NORMAL,
                        .object_name = obj_name,
                        .normal = { p, sym->value }
                    };

                    TB_LinkerSymbol* lnk_s = NULL;
                    if (sym->type == TB_OBJECT_SYMBOL_STATIC) {
                        TB_LinkerSymbol* lnk_s = tb_platform_heap_alloc(sizeof(s));
                        *lnk_s = s;
                    } else if (sym->type == TB_OBJECT_SYMBOL_EXTERN || sym->type == TB_OBJECT_SYMBOL_WEAK_EXTERN) {
                        if (comdat_aux) {
                            s.flags |= TB_LINKER_SYMBOL_COMDAT;

                            // check if it already exists as a COMDAT
                            TB_LinkerSymbol* old = tb__find_symbol(&l->symtab, sym->name);
                            if (old && (old->flags & TB_LINKER_SYMBOL_COMDAT)) {
                                if (comdat_aux->selection == 2) {
                                    // comdat any, any duplicates will be ignored
                                    p->size = 0;
                                } else {
                                    tb_todo();
                                }
                            } else {
                                lnk_s = tb__append_symbol(&l->symtab, &s);
                            }

                            comdat_aux = NULL;
                        } else {
                            lnk_s = tb__append_symbol(&l->symtab, &s);
                        }
                    }

                    // add to the section piece's symbol list
                    if (lnk_s) {
                        lnk_s->next = p->first_sym;
                        p->first_sym = lnk_s;

                        sym->user_data = lnk_s;
                    }
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("parse relocations") {
        FOREACH_N(i, 0, obj->section_count) {
            TB_ObjectSection* restrict s = &obj->sections[i];
            TB_LinkerSectionPiece* restrict p = s->user_data;

            // some relocations point to sections within the same object file, we resolve
            // their symbols early.
            FOREACH_N(j, 0, s->relocation_count) {
                TB_ObjectReloc* restrict reloc = &s->relocations[j];

                // resolve address used in relocation, symbols are sorted so we can binary search
                TB_ObjectSymbol key = { .ordinal = reloc->symbol_index };
                TB_ObjectSymbol* restrict src_symbol = bsearch(&key, obj->symbols, obj->symbol_count, sizeof(TB_ObjectSymbol), symbol_cmp);

                if (reloc->type == TB_OBJECT_RELOC_ADDR64) {
                    TB_LinkerReloc r = {
                        .type    = reloc->type,
                        .addend  = reloc->addend,
                        .is_weak = (src_symbol->type == TB_OBJECT_SYMBOL_WEAK_EXTERN),
                        .target  = src_symbol->user_data,
                        .source  = {
                            .piece = p,
                            .offset = reloc->virtual_address
                        },
                        .obj_name = obj_name
                    };

                    if (r.target == NULL) {
                        bool is_thunk;
                        r.name = deimp_your_names(src_symbol->name, &is_thunk);
                    }
                    dyn_array_put(l->relocations, r);
                } else if (src_symbol->section_num == i) {
                    // relocation is section-local so we can resolve it early.
                    assert(p->kind == PIECE_NORMAL);

                    // make the section data mutable to allow some relocations to be applied
                    if (p->flags & 1) {
                        p->flags &= ~1;

                        void* new_data = tb_platform_heap_alloc(p->size);
                        memcpy(new_data, p->data, p->size);
                        p->data = new_data;
                    }

                    uint32_t* dst = (uint32_t*) &p->data[reloc->virtual_address];
                    *dst += src_symbol->value - (reloc->virtual_address + reloc->addend);
                } else {
                    TB_LinkerReloc r = {
                        .type    = reloc->type,
                        .addend  = reloc->addend,
                        .is_weak = (src_symbol->type == TB_OBJECT_SYMBOL_WEAK_EXTERN),
                        .target  = src_symbol->user_data,
                        .source  = {
                            .piece = p,
                            .offset = reloc->virtual_address
                        },
                        .obj_name = obj_name
                    };

                    if (r.target == NULL) {
                        bool is_thunk;
                        r.name = deimp_your_names(src_symbol->name, &is_thunk);
                        if (is_thunk) r.is_thunk = true;
                    }
                    dyn_array_put(l->relocations, r);
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
        uint32_t last_page = UINT32_MAX, reloc_size = 0;
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
    TB_LinkerSection* text  = tb__find_section(l, ".text",  IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* data  = tb__find_section(l, ".data",  IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    TB_LinkerSection* rdata = tb__find_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

    uint64_t trampoline_rva = text->address + l->trampoline_pos;
    FOREACH_N(i, 0, m->max_threads) {
        uint64_t text_piece_rva = text->address + m->linker.text->offset;
        uint64_t text_piece_file = text->offset + m->linker.text->offset;

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

    uint32_t iat_pos = l->iat_pos;
    dyn_array_for(i, l->relocations) {
        TB_LinkerReloc* restrict r = &l->relocations[i];
        TB_LinkerSection* restrict s = r->source.piece->parent;
        if (r->type == TB_OBJECT_RELOC_ADDR64) continue;
        if (r->target == NULL || (s->generic_flags & TB_LINKER_SECTION_DISCARD)) continue;

        int32_t* dst = (int32_t*) &output[s->offset + r->source.piece->offset + r->source.offset];

        uint32_t target_rva = 0;
        TB_LinkerSymbol* sym = r->target;
        if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            if (r->is_thunk) {
                target_rva = iat_pos + (sym->import.thunk->thunk_id * 8);
            } else {
                target_rva = trampoline_rva + (sym->import.thunk->thunk_id * 6);
            }
        } else if (sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
            target_rva = 0;
        } else {
            target_rva = tb__get_symbol_rva(l, sym);
        }

        switch (r->type) {
            case TB_OBJECT_RELOC_ADDR32NB: {
                (*dst) += target_rva;
                break;
            }
            case TB_OBJECT_RELOC_SECTION: {
                if (sym->tag != TB_LINKER_SYMBOL_IMPORT && sym->tag != TB_LINKER_SYMBOL_ABSOLUTE && sym->tag != TB_LINKER_SYMBOL_IMAGEBASE) {
                    (*dst) += sym->normal.piece->parent->number;
                }
                break;
            }
            case TB_OBJECT_RELOC_SECREL: {
                if (sym->tag != TB_LINKER_SYMBOL_IMPORT && sym->tag != TB_LINKER_SYMBOL_ABSOLUTE && sym->tag != TB_LINKER_SYMBOL_IMAGEBASE) {
                    (*dst) += (target_rva - sym->normal.piece->parent->address);
                }
                break;
            }
            case TB_OBJECT_RELOC_REL32: {
                uint32_t actual_pos = s->address + r->source.piece->offset + r->source.offset;
                (*dst) += target_rva - (actual_pos + r->addend);
                break;
            }
            default: tb_todo();
        }
    }
}

static void align_up_emitter(TB_Emitter* e, size_t u) {
    size_t pad = align_up(e->count, u) - e->count;
    while (pad--) tb_out1b(e, 0x00);
}

// returns the two new section pieces for the IAT and ILT
static COFF_ImportDirectory* gen_imports(TB_Linker* l, PE_ImageDataDirectory* imp_dir, PE_ImageDataDirectory* iat_dir) {
    CUIK_TIMED_BLOCK("generate thunks from TB modules") {
        dyn_array_for(j, l->ir_modules) {
            TB_Module* m = l->ir_modules[j];

            // Find all the imports & place them into the right buckets
            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    TB_Slice name = { strlen(ext->super.name), (uint8_t*) ext->super.name };
                    TB_LinkerSymbol* sym = tb__find_symbol(&l->symtab, name);
                    if (sym == NULL) {
                        tb__unresolved_symbol(l, name)->ext = ext;
                        continue;
                    }

                    if (sym->tag != TB_LINKER_SYMBOL_IMPORT) continue;
                    ext->super.address = tb__find_or_create_import(l, sym);
                }
            }
        }

        dyn_array_for(i, l->relocations) {
            TB_LinkerReloc* restrict r = &l->relocations[i];

            // resolve any by-name symbols
            TB_LinkerSymbol* sym = r->target;
            if (sym == NULL) {
                r->target = sym = tb__find_symbol(&l->symtab, r->name);
                if (sym == NULL && !r->is_weak) {
                    tb__unresolved_symbol(l, r->name)->reloc = r;
                    continue;
                }
            }

            if (!sym || sym->tag != TB_LINKER_SYMBOL_IMPORT) continue;
            sym->import.thunk = tb__find_or_create_import(l, sym);
        }
    }

    if (nl_strmap_get_load(l->unresolved_symbols) > 0) {
        nl_strmap_for(i, l->unresolved_symbols) {
            TB_UnresolvedSymbol* u = l->unresolved_symbols[i];

            fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved external: %.*s\n", (int) u->name.length, u->name.data);
            size_t i = 0;
            for (; u && i < 5; u = u->next, i++) {
                if (u->ext) {
                    fprintf(stderr, "  in <tb-module>\n");
                } else {
                    TB_Slice obj_name = u->reloc->obj_name;

                    size_t last = 0;
                    FOREACH_N(i, 0, obj_name.length) {
                        if (obj_name.data[i] == '/' || obj_name.data[i] == '\\') {
                            last = i+1;
                        }
                    }

                    obj_name.data += last;
                    obj_name.length -= last;

                    fprintf(stderr, "  in %.*s\n", (int) obj_name.length, obj_name.data);
                }
            }

            if (u) {
                // count the rest
                while (u) u = u->next, i++;

                fprintf(stderr, "  ...and %llu more...\n", i - 5);
            }
            fprintf(stderr, "\n");
        }

        // nl_strmap_free(l->unresolved_symbols);
        //return NULL;
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

    if (l->imports) {
        dyn_array_set_length(l->imports, j); // trimmed
    }

    if (import_entry_count == 0) {
        *imp_dir = (PE_ImageDataDirectory){ 0 };
        *iat_dir = (PE_ImageDataDirectory){ 0 };
        return NULL;
    }

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

static void* gen_reloc_section(TB_Linker* l) {
    // generates .reloc for all object files
    uint32_t reloc_sec_size = 0;
    uint32_t last_page = UINT32_MAX;
    TB_LinkerSectionPiece* last_piece = NULL;

    dyn_array_for(i, l->relocations) {
        TB_LinkerReloc* restrict r = &l->relocations[i];
        if (r->type != TB_OBJECT_RELOC_ADDR64) continue;
        if (r->target == NULL || r->target->tag == TB_LINKER_SYMBOL_IMPORT || r->target->tag == TB_LINKER_SYMBOL_ABSOLUTE) continue;

        TB_LinkerSection* s = r->source.piece->parent;
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

        size_t actual_pos = r->source.piece->offset + r->source.offset;
        size_t actual_page = actual_pos & ~4095;

        if (last_piece != r->source.piece && last_page != actual_page) {
            last_piece = r->source.piece;
            last_page = actual_page;

            reloc_sec_size += 8;
        }

        reloc_sec_size += 2;
    }

    if (reloc_sec_size) {
        void* reloc = tb_platform_heap_alloc(reloc_sec_size);

        TB_LinkerSection* reloc_sec = tb__find_or_create_section(l, ".reloc", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
        l->main_reloc = tb__append_piece(reloc_sec, PIECE_NORMAL, reloc_sec_size, reloc, NULL);

        return reloc;
    } else {
        return NULL;
    }
}

#define CSTRING(str) { sizeof(str)-1, (const uint8_t*) str }
static void init(TB_Linker* l) {
    tb__append_symbol(&l->symtab, &(TB_LinkerSymbol){
            .name = CSTRING("__ImageBase"),
            .tag = TB_LINKER_SYMBOL_IMAGEBASE,
        });

    // This is practically just ripped from LLD
    //   https://github.com/llvm/llvm-project/blob/3d0a5bf7dea509f130c51868361a38daeee7816a/lld/COFF/Driver.cpp#L2192
    #define add_abs(name_) tb__append_symbol(&l->symtab, &(TB_LinkerSymbol){ .name = CSTRING(name_) })
    add_abs("__volatile_metadata");
    add_abs("__guard_memcpy_fptr");
    add_abs("__guard_fids_count");
    add_abs("__guard_fids_table");
    add_abs("__guard_flags");
    add_abs("__guard_iat_count");
    add_abs("__guard_iat_table");
    add_abs("__guard_longjmp_count");
    add_abs("__guard_longjmp_table");
    // Needed for MSVC 2017 15.5 CRT.
    add_abs("__enclave_config");
    // Needed for MSVC 2019 16.8 CRT.
    add_abs("__guard_eh_cont_count");
    add_abs("__guard_eh_cont_table");
}

static int compare_linker_sections(const void* a, const void* b) {
    const TB_LinkerSectionPiece* sec_a = *(const TB_LinkerSectionPiece**) a;
    const TB_LinkerSectionPiece* sec_b = *(const TB_LinkerSectionPiece**) b;

    return sec_a->order - sec_b->order;
}

#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
static TB_Exports export(TB_Linker* l) {
    PE_ImageDataDirectory imp_dir, iat_dir;
    COFF_ImportDirectory* import_dirs;

    // Merge sections into .rdata
    TB_LinkerSection* rdata = tb__find_section(l, ".rdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    {
        tb__merge_sections(l, tb__find_section(l, ".gfids", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA), rdata);
        tb__merge_sections(l, tb__find_section(l, ".00cfg", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA), rdata);
        tb__merge_sections(l, tb__find_section(l, ".rtc",   IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA), rdata);
        tb__merge_sections(l, tb__find_section(l, ".xdata", IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA), rdata);
        tb__merge_sections(l, tb__find_section(l, ".CRT",   IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA), rdata);
    }

    CUIK_TIMED_BLOCK("GC sections") {
        DynArray(TB_LinkerSectionPiece*) stack = NULL;

        // find roots
        TB_LinkerSectionPiece* entry = tb__get_piece(l, tb__find_symbol_cstr(&l->symtab, "mainCRTStartup"));
        if (entry) {
            dyn_array_put(stack, entry);
        }

        // mark
        while (dyn_array_length(stack)) {
            TB_LinkerSectionPiece* p = dyn_array_pop(stack);

            for (TB_LinkerSymbol* sym = p->first_sym; sym != NULL; sym = sym->next) {
                TB_LinkerSectionPiece* kid = tb__get_piece(l, sym);

                if (kid->size > 0 && !(kid->parent->generic_flags & TB_LINKER_SECTION_DISCARD) && !(kid->flags & 2)) {
                    kid->flags |= 2;
                    dyn_array_put(stack, kid);
                }
            }
        }

        dyn_array_destroy(stack);
    }

    CUIK_TIMED_BLOCK("sort sections") {
        TB_LinkerSectionPiece** array_form = NULL;

        nl_strmap_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i];
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

            size_t piece_count = s->piece_count;

            ////////////////////////////////
            // Sort sections
            ////////////////////////////////
            // convert into array
            assert(s->piece_count != 0);
            array_form = tb_platform_heap_realloc(array_form, piece_count * sizeof(TB_LinkerSectionPiece*));

            size_t j = 0;
            for (TB_LinkerSectionPiece* p = s->first; p != NULL; p = p->next) {
                if (p->size != 0 && (p->flags & 2)) {
                    array_form[j++] = p;
                }
            }
            fprintf(stderr, "%.*s: %zu -> %zu\n", (int) s->name.length, s->name.data, piece_count, j);
            piece_count = j;

            if (piece_count == 0) {
                s->generic_flags |= TB_LINKER_SECTION_DISCARD;
                continue;
            }

            // sort
            CUIK_TIMED_BLOCK("sort section") {
                qsort(array_form, piece_count, sizeof(TB_LinkerSectionPiece*), compare_linker_sections);
            }

            // convert back into linked list
            array_form[0]->offset = 0;

            size_t offset = array_form[0]->size;
            for (j = 1; j < piece_count; j++) {
                array_form[j]->offset = offset;
                offset += array_form[j]->size;

                array_form[j-1]->next = array_form[j];
            }
            s->total_size = offset;

            s->first = array_form[0];
            s->last = array_form[piece_count - 1];
            array_form[piece_count - 1]->next = NULL;
        }
        tb_platform_heap_free(array_form);
    }

    CUIK_TIMED_BLOCK("generate imports") {
        import_dirs = gen_imports(l, &imp_dir, &iat_dir);
    }

    CUIK_TIMED_BLOCK("generate .reloc") {
        gen_reloc_section(l);
    }

    size_t final_section_count = 0;
    nl_strmap_for(i, l->sections) {
        l->sections[i]->number = i;
        final_section_count += (l->sections[i]->generic_flags & TB_LINKER_SECTION_DISCARD) == 0;
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
        nl_strmap_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i];
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

            if (s->flags & IMAGE_SCN_CNT_CODE) pe_code_size += s->total_size;
            if (s->flags & IMAGE_SCN_CNT_INITIALIZED_DATA) pe_init_size += s->total_size;
            if (s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) pe_uninit_size += s->total_size;

            s->offset = size_of_headers + section_content_size;
            if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
                section_content_size += align_up(s->total_size, 512);
            }

            s->address = virt_addr;
            virt_addr += align_up(s->total_size, 4096);
        }
    }

    TB_LinkerSection* text = tb__find_section(l, ".text", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* data = tb__find_section(l, ".data", IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);

    if (import_dirs != NULL) {
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
        l->iat_pos = iat_dir.virtual_address;
    }

    size_t output_size = size_of_headers + section_content_size;
    COFF_FileHeader header = {
        .machine = 0x8664,
        .section_count = final_section_count,
        .timestamp = time(NULL),
        .symbol_table = 0,
        .symbol_count = 0,
        .optional_header_size = sizeof(PE_OptionalHeader64),
        .flags = 0x2 | 0x0020 /* executable, >2GB */
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
        .dll_characteristics = 0x40 | 0x20 | 0x0100 | 0x8000, /* dynamic base, high entropy, nx compat, terminal server aware */

        .size_of_stack_reserve = 2 << 20,
        .size_of_stack_commit = 4096,

        .rva_size_count = IMAGE_NUMBEROF_DIRECTORY_ENTRIES,
        .data_directories = {
            [IMAGE_DIRECTORY_ENTRY_IMPORT] = imp_dir,
            [IMAGE_DIRECTORY_ENTRY_IAT] = iat_dir,
        }
    };

    // TB_LinkerSection* tls = tb__find_section(l, ".tls", IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSymbol* tls_used_sym = tb__find_symbol_cstr(&l->symtab, "_tls_used");
    if (tls_used_sym) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_TLS] = (PE_ImageDataDirectory){ tb__get_symbol_rva(l, tls_used_sym), sizeof(PE_TLSDirectory) };
    }

    TB_LinkerSymbol* load_config_used_sym = tb__find_symbol_cstr(&l->symtab, "_load_config_used");
    if (load_config_used_sym) {
        opt_header.data_directories[IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG] = (PE_ImageDataDirectory){ tb__get_symbol_rva(l, load_config_used_sym), 0x140 };
    }

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

        TB_LinkerSymbol* sym = tb__find_symbol_cstr(&l->symtab, "mainCRTStartup");
        if (sym) {
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
                opt_header.entrypoint = text->address + sym->normal.piece->offset + sym->normal.secrel;
            } else if (sym->tag == TB_LINKER_SYMBOL_TB) {
                opt_header.entrypoint = text->address + sym->tb.piece->offset + tb__get_symbol_pos(sym->tb.sym);
            } else {
                tb_todo();
            }
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
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

        PE_SectionHeader sec_header = {
            .virtual_size = align_up(s->total_size, 4096),
            .virtual_address = s->address,
            .characteristics = s->flags,
        };

        if ((s->flags & IMAGE_SCN_CNT_UNINITIALIZED_DATA) == 0) {
            sec_header.pointer_to_raw_data = s->offset;
            sec_header.size_of_raw_data = s->total_size;
        }

        memcpy(sec_header.name, s->name.data, s->name.length >= 8 ? 8 : s->name.length);
        WRITE(&sec_header, sizeof(sec_header));
    }
    write_pos = tb__pad_file(output, write_pos, 0x00, 0x200);

    tb__apply_section_contents(l, output, write_pos, text, data, rdata, 512, opt_header.image_base);

    // TODO(NeGate): multithread this too
    CUIK_TIMED_BLOCK("apply final relocations") {
        dyn_array_for(i, l->ir_modules) {
            tb__apply_external_relocs(l, l->ir_modules[i], output);
        }
    }

    // now that we've laid out the final output, we can initialize the massive .reloc chunk
    if (l->main_reloc) {
        uint8_t* p_start = (uint8_t*) &output[l->main_reloc->parent->offset + l->main_reloc->offset];
        uint8_t* p_out = p_start;

        uint32_t last_page = 0xFFFFFFFF;
        uint32_t* last_block = NULL;
        TB_LinkerSectionPiece* last_piece = NULL;
        dyn_array_for(i, l->relocations) {
            TB_LinkerReloc* restrict r = &l->relocations[i];
            if (r->type != TB_OBJECT_RELOC_ADDR64) continue;
            if (r->target == NULL || r->target->tag == TB_LINKER_SYMBOL_IMPORT) continue;

            TB_LinkerSection* s = r->source.piece->parent;
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

            size_t actual_pos = r->source.piece->offset + r->source.offset;
            if (r->target->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
                uint64_t* reloc = (uint64_t*) &output[s->offset + actual_pos];
                *reloc += r->target->absolute;
                continue;
            }

            size_t actual_page = actual_pos & ~4095;
            size_t page_offset = actual_pos - actual_page;

            if (last_piece != r->source.piece && last_page != actual_page) {
                last_piece = r->source.piece;
                last_page  = actual_page;
                last_block = (uint32_t*) p_out;

                last_block[0] = s->address + actual_page;
                last_block[1] = 8; // block size field (includes RVA field and itself)
                p_out += 8;
            }

            // compute RVA
            uint32_t file_pos = s->offset + actual_pos;
            uint64_t* reloc = (uint64_t*) &output[file_pos];
            *reloc += opt_header.image_base + tb__get_symbol_rva(l, r->target);

            // emit relocation
            uint16_t payload = (10 << 12) | page_offset; // (IMAGE_REL_BASED_DIR64 << 12) | offset
            *((uint16_t*) p_out) = payload, p_out += 2;
            last_block[1] += 2;
        }

        size_t actual_reloc_size = p_out - p_start;
        assert(actual_reloc_size == l->main_reloc->size);
    }

    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

TB_LinkerVtbl tb__linker_pe = {
    .init           = init,
    .append_object  = append_object,
    .append_library = append_library,
    .append_module  = append_module,
    .export         = export
};
