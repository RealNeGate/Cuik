#define NL_STRING_MAP_IMPL
#include "linker.h"

extern TB_LinkerVtbl tb__linker_pe, tb__linker_elf;

TB_API TB_ExecutableType tb_system_executable_format(TB_System s) {
    switch (s) {
        case TB_SYSTEM_WINDOWS: return TB_EXECUTABLE_PE;
        case TB_SYSTEM_LINUX:   return TB_EXECUTABLE_ELF;
        default: tb_todo();     return TB_EXECUTABLE_UNKNOWN;
    }
}

TB_API TB_Linker* tb_linker_create(TB_ExecutableType exe, TB_Arch arch) {
    TB_Linker* l = tb_platform_heap_alloc(sizeof(TB_Linker));
    memset(l, 0, sizeof(TB_Linker));
    l->target_arch = arch;

    l->symtab.exp = 14;
    CUIK_TIMED_BLOCK("tb_platform_valloc") {
        l->symtab.ht = tb_platform_valloc((1u << l->symtab.exp) * sizeof(TB_LinkerSymbol));
    }

    switch (exe) {
        case TB_EXECUTABLE_PE:  l->vtbl = tb__linker_pe;  break;
        case TB_EXECUTABLE_ELF: l->vtbl = tb__linker_elf; break;
        default: break;
    }

    l->vtbl.init(l);
    return l;
}

TB_API void tb_linker_append_object(TB_Linker* l, TB_Slice obj_name, TB_ObjectFile* obj) {
    l->vtbl.append_object(l, obj_name, obj);
}

TB_API void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    l->vtbl.append_module(l, m);
}

TB_API void tb_linker_append_library(TB_Linker* l, TB_Slice ar_name, TB_Slice ar_file) {
    l->vtbl.append_library(l, ar_name, ar_file);
}

TB_API TB_Exports tb_linker_export(TB_Linker* l) {
    return l->vtbl.export(l);
}

TB_API void tb_linker_destroy(TB_Linker* l) {
    tb_platform_heap_free(l);
}

TB_LinkerSectionPiece* tb__get_piece(TB_Linker* l, TB_LinkerSymbol* restrict sym) {
    if (sym && (sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB)) {
        return sym->normal.piece;
    }

    return NULL;
}

size_t tb__get_symbol_pos(TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_FUNCTION) {
        return ((TB_Function*) s)->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        return ((TB_Global*) s)->pos;
    } else {
        tb_todo();
    }
}

// also finds the entrypoints (kill two birds amirite)
size_t tb__layout_text_section(TB_Module* m) {
    size_t size = 0;
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f == NULL) continue;

        out_f->code_pos = size;
        size += out_f->code_size;
    }

    return size;
}

uint64_t tb__get_symbol_rva(TB_Linker* l, TB_LinkerSymbol* sym) {
    if (sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
        return 0;
    } else if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE) {
        return sym->imagebase;
    }

    // normal or TB
    assert(sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB);
    TB_LinkerSectionPiece* piece = sym->normal.piece;

    uint32_t rva = piece->parent->address + piece->offset;
    if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
        return rva + sym->normal.secrel;
    }

    TB_Symbol* s = sym->tb.sym;
    if (s->tag == TB_SYMBOL_FUNCTION) {
        TB_Function* f = (TB_Function*) s;
        assert(f->output != NULL);

        return rva + f->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        return rva + ((TB_Global*) s)->pos;
    } else {
        tb_todo();
    }
}

uint64_t tb__compute_rva(TB_Linker* l, TB_Module* m, const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_FUNCTION) {
        ptrdiff_t search = nl_strmap_get_cstr(l->sections, ".text");
        TB_LinkerSection* text = (search >= 0 ? l->sections[search] : NULL);

        TB_Function* f = (TB_Function*) s;
        assert(f->output != NULL);
        return text->address + m->text.piece->offset + f->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        ptrdiff_t search = nl_strmap_get_cstr(l->sections, ".data");
        TB_LinkerSection* data = (search >= 0 ? l->sections[search] : NULL);

        TB_Global* g = (TB_Global*) s;
        return data->address + m->data.piece->offset + g->pos;
    } else {
        tb_todo();
        return 0;
    }
}

size_t tb__pad_file(uint8_t* output, size_t write_pos, char pad, size_t align) {
    size_t align_mask = align - 1;
    size_t end = (write_pos + align_mask) & ~align_mask;
    if (write_pos != end) {
        memset(output + write_pos, 0, end - write_pos);
        write_pos = end;
    }
    return write_pos;
}

void tb__merge_sections(TB_Linker* linker, TB_LinkerSection* from, TB_LinkerSection* to) {
    if (from == NULL || to == NULL) return;
    if (from->generic_flags & TB_LINKER_SECTION_DISCARD) return;

    // remove 'from' from final output
    from->generic_flags |= TB_LINKER_SECTION_DISCARD;

    if (to->last) to->last->next = from->first;
    else to->first = from->first;
    to->last = from->last;

    #if 1
    // we don't care about fixing up the offsets because they'll be properly laid out
    // after this is all done
    to->total_size += from->total_size;
    to->piece_count += from->piece_count;

    for (TB_LinkerSectionPiece* p = from->first; p != NULL; p = p->next) {
        p->parent = to;
    }
    #else
    if (from->last) {
        size_t offset = to->total_size;

        for (TB_LinkerSectionPiece* p = from->first; p != NULL; p = p->next) {
            p->parent = to;
            p->offset = offset;
            offset += p->size;
        }

        to->total_size += from->total_size;
        to->piece_count += from->piece_count;
        assert(offset == to->total_size);
    }
    #endif
}

void tb__append_module_section(TB_Linker* l, TB_Module* mod, TB_ModuleSection* section, const char* name, uint32_t flags) {
    if (section->total_size > 0) {
        TB_LinkerSection* ls = tb__find_or_create_section(l, name, flags);
        section->piece = tb__append_piece(ls, PIECE_MODULE_SECTION, section->total_size, section, mod);
    }
}

size_t tb__apply_section_contents(TB_Linker* l, uint8_t* output, size_t write_pos, TB_LinkerSection* text, TB_LinkerSection* data, TB_LinkerSection* rdata, size_t section_alignment, size_t image_base) {
    // write section contents
    // TODO(NeGate): we can actually parallelize this part of linking
    CUIK_TIMED_BLOCK("write sections") nl_strmap_for(i, l->sections) {
        TB_LinkerSection* s = l->sections[i];
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) continue;

        assert(s->offset == write_pos);
        for (TB_LinkerSectionPiece* p = s->first; p != NULL; p = p->next) {
            uint8_t* p_out = &output[write_pos];
            TB_Module* m = p->module;

            switch (p->kind) {
                case PIECE_NORMAL: {
                    if (p->data == NULL) goto skip;

                    memcpy(p_out, p->data, p->size);
                    break;
                }
                case PIECE_MODULE_SECTION: {
                    tb_helper_write_section(m, 0, (TB_ModuleSection*) p->data, p_out, 0);
                    break;
                }
                case PIECE_PDATA: {
                    uint32_t* p_out32 = (uint32_t*) p_out;

                    uint32_t text_rva = text->address + m->text.piece->offset;
                    uint32_t rdata_rva = rdata->address + m->rdata.piece->offset;

                    TB_FOR_FUNCTIONS(f, m) {
                        TB_FunctionOutput* out_f = f->output;
                        if (out_f != NULL) {
                            // both into the text section
                            *p_out32++ = text_rva + out_f->code_pos;
                            *p_out32++ = text_rva + out_f->code_pos + out_f->code_size;

                            // refers to rdata section
                            *p_out32++ = rdata_rva + out_f->unwind_info;
                        }
                    }
                    break;
                }
                case PIECE_RELOC: {
                    // TODO(NeGate): currently windows only
                    uint32_t data_rva  = data->address + m->data.piece->offset;
                    uint32_t data_file = data->offset  + m->data.piece->offset;

                    uint32_t last_page = 0xFFFFFFFF;
                    uint32_t* last_block = NULL;
                    TB_FOR_GLOBALS(g, m) {
                        FOREACH_N(k, 0, g->obj_count) {
                            size_t actual_pos  = g->pos + g->objects[k].offset;
                            size_t actual_page = actual_pos & ~4095;
                            size_t page_offset = actual_pos - actual_page;

                            if (g->objects[k].type != TB_INIT_OBJ_RELOC) {
                                continue;
                            }

                            const TB_Symbol* s = g->objects[k].reloc;
                            if (last_page != actual_page) {
                                last_page  = data_rva + actual_page;
                                last_block = (uint32_t*) p_out;

                                last_block[0] = data_rva + actual_page;
                                last_block[1] = 8; // block size field (includes RVA field and itself)
                                p_out += 8;
                            }

                            // compute RVA
                            uint32_t file_pos = data_file + actual_pos;
                            *((uint64_t*) &output[file_pos]) = tb__compute_rva(l, m, s) + image_base;

                            // emit relocation
                            uint16_t payload = (10 << 12) | page_offset; // (IMAGE_REL_BASED_DIR64 << 12) | offset
                            *((uint16_t*) p_out) = payload, p_out += sizeof(uint16_t);
                            last_block[1] += 2;
                        }
                    }
                    break;
                }
                default: tb_todo();
            }

            write_pos += p->size;
            skip:;
        }

        write_pos = tb__pad_file(output, write_pos, 0x00, section_alignment);
    }

    return write_pos;
}

TB_LinkerSection* tb__find_section(TB_Linker* linker, const char* name, uint32_t flags) {
    ptrdiff_t search = nl_strmap_get_cstr(linker->sections, name);
    return search >= 0 ? linker->sections[search] : NULL;
}

TB_LinkerSection* tb__find_or_create_section(TB_Linker* linker, const char* name, uint32_t flags) {
    // allocate new section if one doesn't exist already
    ptrdiff_t search = nl_strmap_get_cstr(linker->sections, name);
    if (search >= 0) {
        // assert(linker->sections[search]->flags == flags);
        return linker->sections[search];
    }

    TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
    *s = (TB_LinkerSection){ .name = { strlen(name), (const uint8_t*) name }, .flags = flags };
    nl_strmap_put_cstr(linker->sections, name, s);
    return s;
}

TB_LinkerSection* tb__find_or_create_section2(TB_Linker* linker, size_t name_len, const uint8_t* name_str, uint32_t flags) {
    // allocate new section if one doesn't exist already
    NL_Slice name = { name_len, name_str };
    ptrdiff_t search = nl_strmap_get(linker->sections, name);

    if (search >= 0) {
        // assert(linker->sections[search]->flags == flags);
        return linker->sections[search];
    }

    TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
    *s = (TB_LinkerSection){ .name = name, .flags = flags };

    nl_strmap_put(linker->sections, name, s);
    return s;
}

TB_LinkerSectionPiece* tb__append_piece(TB_LinkerSection* section, int kind, size_t size, const void* data, TB_Module* mod) {
    // allocate some space for it, we might wanna make the total_size increment atomic
    TB_LinkerSectionPiece* piece = tb_platform_heap_alloc(sizeof(TB_LinkerSectionPiece));
    *piece = (TB_LinkerSectionPiece){
        .kind   = kind,
        .parent = section,
        .offset = section->total_size,
        .size   = size,
        .vsize  = size,
        .data   = data,
        .module = mod
    };
    section->total_size += size;
    section->piece_count += 1;

    if (section->last == NULL) {
        section->first = section->last = piece;
    } else {
        section->last->next = piece;
        section->last = piece;
    }
    return piece;
}

// murmur3 32-bit without UB unaligned accesses
// https://github.com/demetri/scribbles/blob/master/hashing/ub_aware_hash_functions.c
static uint32_t murmur(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k;
        memcpy(&k, &((char*) key)[i * 4], sizeof(k));

        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    }

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}

ImportThunk* tb__find_or_create_import(TB_Linker* l, TB_LinkerSymbol* restrict sym) {
    assert(sym->tag == TB_LINKER_SYMBOL_IMPORT);
    TB_Slice sym_name = { sym->name.length - (sizeof("__imp_") - 1), sym->name.data + sizeof("__imp_") - 1 };

    ImportTable* table = &l->imports[sym->import.id];
    dyn_array_for(i, table->thunks) {
        if (table->thunks[i].name.length == sym_name.length &&
            memcmp(table->thunks[i].name.data, sym_name.data, sym_name.length) == 0) {
            return &table->thunks[i];
        }
    }

    ImportThunk t = { .name = sym_name, .ordinal = sym->import.ordinal };
    dyn_array_put(table->thunks, t);
    return &table->thunks[dyn_array_length(table->thunks) - 1];
}

TB_LinkerSymbol* tb__find_symbol_cstr(TB_SymbolTable* restrict symtab, const char* name) {
    return tb__find_symbol(symtab, (TB_Slice){ strlen(name), (const uint8_t*) name });
}

TB_LinkerSymbol* tb__find_symbol(TB_SymbolTable* restrict symtab, TB_Slice name) {
    uint32_t mask = (1u << symtab->exp) - 1;
    uint32_t hash = murmur(name.data, name.length);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - symtab->exp)) | 1;
        i = (i + step) & mask;

        if (symtab->ht[i].name.length == 0) {
            return NULL;
        } else if (name.length == symtab->ht[i].name.length && memcmp(name.data, symtab->ht[i].name.data, name.length) == 0) {
            return &symtab->ht[i];
        }
    }
}

TB_LinkerSymbol* tb__append_symbol(TB_SymbolTable* restrict symtab, const TB_LinkerSymbol* sym) {
    TB_Slice name = sym->name;

    uint32_t mask = (1u << symtab->exp) - 1;
    uint32_t hash = murmur(name.data, name.length);
    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t step = (hash >> (32 - symtab->exp)) | 1;
        i = (i + step) & mask;

        if (symtab->ht[i].name.length == 0) {
            // empty slot
            if (symtab->len > mask) {
                printf("Symbol table: out of memory!\n");
                abort();
            }

            symtab->len++;
            memcpy(&symtab->ht[i], sym, sizeof(TB_LinkerSymbol));
            return &symtab->ht[i];
        } else if (name.length == symtab->ht[i].name.length && memcmp(name.data, symtab->ht[i].name.data, name.length) == 0) {
            // proper collision... this is a linker should we throw warnings?
            // memcpy(&symtab->ht[i], sym, sizeof(TB_LinkerSymbol));
            return &symtab->ht[i];
        }
    }
}

TB_UnresolvedSymbol* tb__unresolved_symbol(TB_Linker* l, TB_Slice name) {
    TB_UnresolvedSymbol* d = tb_platform_heap_alloc(sizeof(TB_UnresolvedSymbol));
    *d = (TB_UnresolvedSymbol){ .name = name };

    // mtx_lock(&parser->diag_mutex);
    NL_Slice name2 = { name.length, name.data };
    ptrdiff_t search = nl_strmap_get(l->unresolved_symbols, name2);
    if (search < 0) {
        search = nl_strmap_puti(l->unresolved_symbols, name2);
        l->unresolved_symbols[search] = d;
    } else {
        TB_UnresolvedSymbol* old = l->unresolved_symbols[search];
        while (old->next != NULL) old = old->next;

        old->next = d;
    }
    // mtx_unlock(&parser->diag_mutex);

    return d;
}

void tb__apply_module_relocs(TB_Linker* l, TB_Module* m, uint8_t* output) {
    TB_LinkerSection* text  = tb__find_section(l, ".text",  IMAGE_SCN_MEM_READ  | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_CNT_CODE);
    TB_LinkerSection* data  = tb__find_section(l, ".data",  IMAGE_SCN_MEM_WRITE | IMAGE_SCN_MEM_READ | IMAGE_SCN_CNT_INITIALIZED_DATA);
    // TB_LinkerSection* rdata = tb__find_section(l, ".rdata", IMAGE_SCN_MEM_READ  | IMAGE_SCN_CNT_INITIALIZED_DATA);

    uint64_t trampoline_rva = text->address + l->trampoline_pos;
    FOREACH_N(i, 0, m->max_threads) {
        uint64_t text_piece_rva = text->address + m->text.piece->offset;
        uint64_t text_piece_file = text->offset + m->text.piece->offset;

        uint64_t data_piece_rva = 0;
        if (m->data.piece) {
            data_piece_rva = data->address + m->data.piece->offset;
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
                if (global->parent->kind != TB_MODULE_SECTION_TLS) {
                    int32_t p = (data_piece_rva + global->pos) - actual_pos;

                    (*dst) += p;
                } else {
                    (*dst) += (data_piece_rva + global->pos);
                }
            } else {
                tb_todo();
            }
        }

        /*if (m->linker.rdata) {
            uint64_t rdata_piece_rva = rdata->address + m->linker.rdata->offset;

            FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
                TB_ConstPoolPatch* patch = &m->thread_info[i].const_patches[j];
                TB_FunctionOutput* out_f = patch->source->output;

                size_t actual_pos = text_piece_rva + out_f->code_pos + out_f->prologue_length + patch->pos + 4;
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + out_f->prologue_length + patch->pos];

                // relocations add not replace
                (*dst) += (rdata_piece_rva - actual_pos);
            }
        }*/
        tb_todo();
    }
}
