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
    l->entrypoint = -1;

    l->symtab.exp = 14;
    CUIK_TIMED_BLOCK("tb_platform_valloc") {
        l->symtab.ht = tb_platform_valloc((1u << l->symtab.exp) * sizeof(TB_LinkerSymbol));
    }

    switch (exe) {
        case TB_EXECUTABLE_PE:  l->vtbl = tb__linker_pe;  break;
        case TB_EXECUTABLE_ELF: l->vtbl = tb__linker_elf; break;
        default: break;
    }
    return l;
}

TB_API void tb_linker_append_object(TB_Linker* l, TB_ObjectFile* obj) {
    l->vtbl.append_object(l, obj);
}

TB_API void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    l->vtbl.append_module(l, m);
}

TB_API void tb_linker_append_library(TB_Linker* l, TB_Slice ar_file) {
    l->vtbl.append_library(l, ar_file);
}

TB_API TB_Exports tb_linker_export(TB_Linker* l) {
    return l->vtbl.export(l);
}

TB_API void tb_linker_destroy(TB_Linker* l) {
    tb_platform_heap_free(l);
}

// also finds the entrypoints (kill two birds amirite)
size_t tb__layout_text_section(TB_Module* m, ptrdiff_t* restrict entrypoint, const char* entrypoint_name) {
    char entrypoint_name_first_char = entrypoint_name[0];

    size_t size = 0;
    *entrypoint = -1;

    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f == NULL) continue;

        if (f->super.name[0] == entrypoint_name_first_char && strcmp(f->super.name, entrypoint_name) == 0) {
            *entrypoint = size;
        }

        out_f->code_pos = size;
        size += out_f->code_size;
    }

    return size;
}

uint64_t tb__compute_rva(TB_Linker* l, TB_Module* m, const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_FUNCTION) {
        ptrdiff_t search = nl_strmap_get_cstr(l->sections, ".text");
        TB_LinkerSection* text = (search >= 0 ? l->sections[search] : NULL);

        TB_Function* f = (TB_Function*) s;
        assert(f->output != NULL);
        return text->address + m->linker.text->offset + f->output->code_pos;
    }

    tb_todo();
    return 0;
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

size_t tb__apply_section_contents(TB_Linker* l, uint8_t* output, size_t write_pos, TB_LinkerSection* text, TB_LinkerSection* data, TB_LinkerSection* rdata, size_t section_alignment) {
    // write section contents
    // TODO(NeGate): we can actually parallelize this part of linking
    CUIK_TIMED_BLOCK("write sections") {
        nl_strmap_for(i, l->sections) {
            TB_LinkerSection* s = l->sections[i];
            assert(s->offset == write_pos);

            for (TB_LinkerSectionPiece* p = s->first; p != NULL; p = p->next) {
                uint8_t* p_out = &output[write_pos];
                TB_Module* m = p->module;

                switch (p->kind) {
                    case PIECE_NORMAL: {
                        memcpy(p_out, p->data, p->size);
                        break;
                    }
                    case PIECE_TEXT: {
                        // Target specific: resolve internal call patches
                        tb__find_code_generator(m)->emit_call_patches(m);

                        TB_FOR_FUNCTIONS(func, m) {
                            TB_FunctionOutput* out_f = func->output;
                            if (out_f) {
                                memcpy(p_out, out_f->code, out_f->code_size);
                                p_out += out_f->code_size;
                            }
                        }
                        break;
                    }
                    case PIECE_DATA: {
                        memset(p_out, 0, p->size);
                        FOREACH_N(i, 0, m->max_threads) {
                            pool_for(TB_Global, g, m->thread_info[i].globals) {
                                if (g->storage != TB_STORAGE_DATA) continue;
                                TB_Initializer* init = g->init;

                                // clear out space
                                memset(&p_out[g->pos], 0, init->size);

                                FOREACH_N(k, 0, init->obj_count) {
                                    if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                                        memcpy(
                                            &p_out[g->pos + init->objects[k].offset],
                                            init->objects[k].region.ptr,
                                            init->objects[k].region.size
                                        );
                                    } else {
                                        // tb_todo();
                                    }
                                }
                            }
                        }
                        break;
                    }
                    case PIECE_RDATA: {
                        memset(p_out, 0, p->size);
                        FOREACH_N(i, 0, m->max_threads) {
                            dyn_array_for(j, m->thread_info[i].const_patches) {
                                TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                                memcpy(&p_out[p->rdata_pos], p->data, p->length);
                            }
                        }
                        break;
                    }
                    case PIECE_PDATA: {
                        uint32_t* p_out32 = (uint32_t*) p_out;

                        uint32_t text_rva = text->address + m->linker.text->offset;
                        uint32_t rdata_rva = rdata->address + m->linker.rdata->offset;

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
                        uint32_t data_rva  = data->address + m->linker.data->offset;
                        uint32_t data_file = data->offset  + m->linker.data->offset;

                        uint32_t last_page = 0;
                        uint32_t* last_block = NULL;
                        FOREACH_N(i, 0, m->max_threads) {
                            pool_for(TB_Global, g, m->thread_info[i].globals) {
                                TB_Initializer* init = g->init;

                                FOREACH_N(k, 0, init->obj_count) {
                                    size_t actual_pos  = g->pos + init->objects[k].offset;
                                    size_t actual_page = actual_pos & ~4095;
                                    size_t page_offset = actual_pos - actual_page;

                                    if (init->objects[k].type == TB_INIT_OBJ_RELOC) {
                                        const TB_Symbol* s = init->objects[k].reloc;

                                        if (last_page != actual_page) {
                                            last_page  = data_rva + actual_page;
                                            last_block = (uint32_t*) p_out;

                                            last_block[0] = data_rva + actual_page;
                                            last_block[1] = 8; // block size field (includes RVA field and itself)
                                            p_out += 8;
                                        }

                                        // compute RVA
                                        uint32_t file_pos = data_file + actual_pos;
                                        *((uint64_t*) &output[file_pos]) = tb__compute_rva(l, m, s);

                                        // emit relocation
                                        uint16_t payload = (10 << 12) | page_offset; // (IMAGE_REL_BASED_DIR64 << 12) | offset
                                        *((uint16_t*) p_out) = payload, p_out += sizeof(uint16_t);
                                        last_block[1] += 2;
                                    }
                                }
                            }
                        }
                        break;
                    }
                    default: tb_todo();
                }

                write_pos += p->size;
            }

            write_pos = tb__pad_file(output, write_pos, 0x00, section_alignment);
        }
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
    if (search < 0) {
        TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
        *s = (TB_LinkerSection){ .name = { strlen(name), (const uint8_t*) name }, .flags = flags };

        nl_strmap_put_cstr(linker->sections, name, s);
        return s;
    } else {
        // assert(linker->sections[search]->flags == flags);
        return linker->sections[search];
    }
}

TB_LinkerSection* tb__find_or_create_section2(TB_Linker* linker, size_t name_len, const uint8_t* name_str, uint32_t flags) {
    // allocate new section if one doesn't exist already
    NL_Slice name = { name_len, name_str };
    ptrdiff_t search = nl_strmap_get(linker->sections, name);
    if (search < 0) {
        TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
        *s = (TB_LinkerSection){ .name = name, .flags = flags };

        nl_strmap_put(linker->sections, name, s);
        return s;
    } else {
        // assert(linker->sections[search]->flags == flags);
        return linker->sections[search];
    }
}

TB_LinkerSectionPiece* tb__append_piece(TB_LinkerSection* section, int kind, size_t size, const void* data, TB_Module* mod) {
    // allocate some space for it, we might wanna make the total_size increment atomic
    TB_LinkerSectionPiece* piece = tb_platform_heap_alloc(sizeof(TB_LinkerSectionPiece));
    *piece = (TB_LinkerSectionPiece){
        .kind   = kind,
        .offset = section->total_size,
        .size   = size,
        .vsize  = size,
        .data   = data,
        .module = mod
    };
    section->total_size += size;

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

// returns true if it replaces a slot
bool tb__append_symbol(TB_SymbolTable* restrict symtab, const TB_LinkerSymbol* sym) {
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
            return false;
        } else if (name.length == symtab->ht[i].name.length && memcmp(name.data, symtab->ht[i].name.data, name.length) == 0) {
            // proper collision... this is a linker should we throw warnings?
            // memcpy(&symtab->ht[i], sym, sizeof(TB_LinkerSymbol));
            // __debugbreak();
            return true;
        }
    }
}


