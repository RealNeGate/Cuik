// Halfway through implementing this ELF64 exporter i realized that my new best friend is
// COFF, ELF is a bitch.
#include "elf64.h"

struct TB_ModuleExporter {
    size_t write_pos;
};

static void put_symbol(TB_Emitter* strtbl, TB_Emitter* stab, const char* name, uint8_t sym_info, Elf64_Half section_index, Elf64_Addr value, Elf64_Xword size) {
    // Fill up the symbol's string table
    size_t name_len = strlen(name);
    size_t name_pos = strtbl->count;

    tb_out_reserve(strtbl, name_len + 1);
    tb_outs_UNSAFE(strtbl, name_len + 1, (uint8_t*)name);

    // Emit symbol
    Elf64_Sym sym = {
        .st_name  = name_pos,
        .st_info  = sym_info,
        .st_shndx = section_index,
        .st_value = value,
        .st_size  = size
    };

    tb_outs(stab, sizeof(Elf64_Sym), (uint8_t*)&sym);
}

#define WRITE(data, length_) write_data(&e, output, length_, data)
static void write_data(TB_ModuleExporter* restrict e, uint8_t* restrict output, size_t length, const void* data) {
    memcpy(output + e->write_pos, data, length);
    e->write_pos += length;
}

static void zero_data(TB_ModuleExporter* restrict e, uint8_t* restrict output, size_t length) {
    memset(output + e->write_pos, 0, length);
    e->write_pos += length;
}

TB_API TB_Exports tb_elf64obj_write_output(TB_Module* m, const IDebugFormat* dbg) {
    // used by the sections array
    enum {
        S_NULL,
        S_STRTAB,
        S_TEXT,
        S_TEXT_REL,
        S_DATA,
        S_DATA_REL,
        S_RODATA,
        S_BSS,
        S_STAB,
        S_MAX
    };

    TB_ModuleExporter e = { 0 };

    CUIK_TIMED_BLOCK("layout section") {
        tb_module_layout_sections(m);
    }

    // tally up .data relocations
    /*uint32_t data_relocation_count = 0;

    FOREACH_N(t, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[t].globals) {
            TB_Initializer* init = g->init;
            FOREACH_N(k, 0, init->obj_count) {
                data_relocation_count += (init->objects[k].type != TB_INIT_OBJ_REGION);
            }
        }
    }*/

    // mark each with a unique id
    uint32_t function_sym_start = S_MAX;
    size_t unique_id_counter = function_sym_start;

    // all the local function symbols
    TB_FOR_FUNCTIONS(f, m) {
        if (f->linkage != TB_LINKAGE_PUBLIC) {
            f->compiled_symbol_id = unique_id_counter++;
        }
    }

    // TODO(NeGate): Doing all these iterations like this probably isn't good... speed up?
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            if (g->linkage != TB_LINKAGE_PUBLIC) {
                g->super.symbol_id = unique_id_counter++;
            }
        }
    }

    // public symbols need to fit at the end
    uint32_t first_nonlocal_symbol_id = unique_id_counter;
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            if (g->linkage == TB_LINKAGE_PUBLIC) {
                g->super.symbol_id = unique_id_counter;
                unique_id_counter += 1;
            }
        }
    }

    // all the nonlocal function symbols
    // uint32_t last_nonlocal_global_id = unique_id_counter;
    TB_FOR_FUNCTIONS(f, m) {
        if (f->linkage == TB_LINKAGE_PUBLIC) {
            f->compiled_symbol_id = unique_id_counter;
            unique_id_counter += 1;
        }
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) {
            ext->super.address = (void*) (uintptr_t) unique_id_counter;
            unique_id_counter += 1;
        }
    }

    uint16_t machine = 0;
    switch (m->target_arch) {
        case TB_ARCH_X86_64: machine = EM_X86_64; break;
        case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
        default: tb_todo();
    }

    Elf64_Ehdr header = {
        .e_ident = {
            [EI_MAG0]       = 0x7F, // magic number
            [EI_MAG1]       = 'E',
            [EI_MAG2]       = 'L',
            [EI_MAG3]       = 'F',
            [EI_CLASS]      = 2, // 64bit ELF file
            [EI_DATA]       = 1, // little-endian
            [EI_VERSION]    = 1, // 1.0
            [EI_OSABI]      = 0,
            [EI_ABIVERSION] = 0
        },
        .e_type = ET_REL, // relocatable
        .e_version = 1,
        .e_machine = machine,
        .e_entry = 0,

        // section headers go at the end of the file
        // and are filed in later.
        .e_shoff = 0,
        .e_flags = 0,

        .e_ehsize = sizeof(Elf64_Ehdr),

        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum     = S_MAX,
        .e_shstrndx  = 1
    };

    Elf64_Shdr sections[S_MAX] = {
        [S_STRTAB] = {
            .sh_type = SHT_STRTAB,
            .sh_flags = 0,
            .sh_addralign = 1
        },
        [S_TEXT] = {
            .sh_type = SHT_PROGBITS,
            .sh_flags = SHF_EXECINSTR | SHF_ALLOC,
            .sh_addralign = 16
        },
        [S_TEXT_REL] = {
            .sh_type = SHT_RELA,
            .sh_flags = SHF_INFO_LINK,
            .sh_link = 7,
            .sh_info = S_TEXT,
            .sh_addralign = 16,
            .sh_entsize = sizeof(Elf64_Rela)
        },
        [S_DATA] = {
            .sh_type = SHT_PROGBITS,
            .sh_flags = SHF_ALLOC | SHF_WRITE,
            .sh_addralign = 16
        },
        [S_DATA_REL] = {
            .sh_type = SHT_RELA,
            .sh_flags = SHF_INFO_LINK,
            .sh_link = 7,
            .sh_info = S_DATA,
            .sh_addralign = 16,
            .sh_entsize = sizeof(Elf64_Rela)
        },
        [S_RODATA] = {
            .sh_type = SHT_PROGBITS,
            .sh_flags = SHF_ALLOC,
            .sh_addralign = 16
        },
        [S_BSS] = {
            .sh_type = SHT_NOBITS,
            .sh_flags = SHF_ALLOC | SHF_WRITE,
            .sh_addralign = 16
        },
        [S_STAB] = {
            .sh_type = SHT_SYMTAB,
            .sh_flags = 0, .sh_addralign = 1,
            .sh_link = 1, .sh_info = first_nonlocal_symbol_id,
            .sh_entsize = sizeof(Elf64_Sym)
        }
    };

    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    static const char* SECTION_NAMES[] = {
        NULL, ".strtab", ".text", ".rela.text", ".data", ".rela.data", ".rodata", ".bss", ".symtab"
    };

    // Section string table:
    TB_Emitter strtbl = { 0 };
    {
        tb_out_reserve(&strtbl, 1024);
        tb_out1b(&strtbl, 0); // null string in the table
        FOREACH_N(i, 1, S_MAX) {
            sections[i].sh_name = tb_outstr_nul_UNSAFE(&strtbl, SECTION_NAMES[i]);
        }
    }

    // Code section
    sections[S_TEXT].sh_size = m->text.total_size;

    // Target specific: resolve internal call patches
    size_t local_patch_count = code_gen->emit_call_patches(m);

    FOREACH_N(i, 0, m->max_threads) {
        sections[S_TEXT_REL].sh_size += dyn_array_length(m->thread_info[i].symbol_patches) * sizeof(Elf64_Rela);
        // sections[S_TEXT_REL].sh_size += dyn_array_length(m->thread_info[i].const_patches) * sizeof(Elf64_Rela);
    }
    sections[S_TEXT_REL].sh_size -= local_patch_count * sizeof(Elf64_Rela);

    FOREACH_N(t, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[t].globals) {
            FOREACH_N(k, 0, g->obj_count) {
                sections[S_DATA_REL].sh_size += (g->objects[k].type != TB_INIT_OBJ_REGION) * sizeof(Elf64_Rela);
            }
        }
    }

    // write symbol table
    TB_Emitter stab = { 0 };

    // NULL symbol
    tb_out_zero(&stab, sizeof(Elf64_Sym));
    FOREACH_N(i, 1, S_MAX) {
        put_symbol(&strtbl, &stab, SECTION_NAMES[i], ELF64_ST_INFO(ELF64_STB_LOCAL, ELF64_STT_SECTION), i, 0, 0);
    }

    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;

        if (out_f != NULL && f->linkage != TB_LINKAGE_PUBLIC) {
            put_symbol(&strtbl, &stab, f->super.name, ELF64_ST_INFO(ELF64_STB_GLOBAL, ELF64_STT_FUNC), 2, out_f->code_pos, out_f->code_size);
        }
    }

    // static-linkage globals
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            if (g->linkage != TB_LINKAGE_PUBLIC) {
                put_symbol(&strtbl, &stab, g->super.name, ELF64_ST_INFO(ELF64_STB_LOCAL, ELF64_STT_OBJECT), S_DATA, g->pos, 0);
            }
        }
    }

    // nonlocal globals
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            if (g->linkage == TB_LINKAGE_PUBLIC) {
                put_symbol(&strtbl, &stab, g->super.name, ELF64_ST_INFO(ELF64_STB_GLOBAL, ELF64_STT_OBJECT), S_DATA, g->pos, 0);
            }
        }
    }

    // nonlocal functions
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f != NULL && f->linkage == TB_LINKAGE_PUBLIC) {
            put_symbol(&strtbl, &stab, f->super.name, ELF64_ST_INFO(ELF64_STB_GLOBAL, ELF64_STT_FUNC), 2, out_f->code_pos, out_f->code_size);
        }
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, external, m->thread_info[i].externals) {
            put_symbol(&strtbl, &stab, external->super.name, ELF64_ST_INFO(ELF64_STB_GLOBAL, 0), 0, 0, 0);
        }
    }

    // set some sizes and pass the stab and string table to the context
    sections[S_STAB].sh_size   = stab.count;
    sections[S_STRTAB].sh_size = strtbl.count;
    sections[S_DATA].sh_size   = m->data.total_size;
    sections[S_RODATA].sh_size = m->rdata.total_size;

    // Calculate file offsets
    size_t output_size = sizeof(Elf64_Ehdr);
    FOREACH_N(i, 0, S_MAX) {
        sections[i].sh_offset = output_size;
        output_size += sections[i].sh_size;
    }

    // section headers
    header.e_shoff = output_size;
    output_size += S_MAX * sizeof(Elf64_Shdr);

    // Allocate memory now
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    // Write contents
    {
        WRITE(&header, sizeof(Elf64_Ehdr));
        WRITE(strtbl.data, strtbl.count);

        // TEXT section
        e.write_pos = tb_helper_write_section(e.write_pos, &m->text, output, sections[S_TEXT].sh_offset);

        // TEXT patches
        {
            assert(e.write_pos == sections[S_TEXT_REL].sh_offset);

            TB_FIXED_ARRAY(Elf64_Rela) relocs = {
                .cap = sections[S_TEXT_REL].sh_size / sizeof(Elf64_Rela),
                .elems = (Elf64_Rela*) &output[sections[S_TEXT_REL].sh_offset]
            };

            FOREACH_N(i, 0, m->max_threads) {
                dyn_array_for(j, m->thread_info[i].symbol_patches) {
                    TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];
                    size_t symbol_id = p->target->symbol_id;
                    assert(symbol_id != 0);

                    TB_FunctionOutput* out_f = p->source->output;
                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;

                    if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                        Elf64_Rela rela = {
                            .r_offset = actual_pos,
                            .r_info   = ELF64_R_INFO(symbol_id, p->is_function ? R_X86_64_PLT32 : R_X86_64_GOTPCREL),
                            .r_addend = -4
                        };
                        TB_FIXED_ARRAY_APPEND(relocs, rela);
                    } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
                        TB_Global* global = (TB_Global*) p->target;
                        ((void) global);
                        assert(global->super.tag == TB_SYMBOL_GLOBAL);

                        Elf64_Rela rela = {
                            .r_offset = actual_pos,
                            .r_info   = ELF64_R_INFO(symbol_id, R_X86_64_PC32),
                            .r_addend = -4
                        };
                        TB_FIXED_ARRAY_APPEND(relocs, rela);
                    } else {
                        tb_todo();
                    }
                }
            }

            WRITE(relocs.elems, relocs.count * sizeof(Elf64_Rela));
        }

        e.write_pos = tb_helper_write_section(e.write_pos, &m->data, output, sections[S_DATA].sh_offset);

        // write DATA patches
        {
            assert(e.write_pos == sections[S_DATA_REL].sh_offset);
            uint8_t* data = &output[sections[S_DATA].sh_offset];
            TB_FIXED_ARRAY(Elf64_Rela) relocs = {
                .cap = sections[S_TEXT_REL].sh_size / sizeof(Elf64_Rela),
                .elems = (Elf64_Rela*) &output[sections[S_DATA_REL].sh_offset]
            };

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    FOREACH_N(k, 0, g->obj_count) {
                        size_t actual_pos = g->pos + g->objects[k].offset;

                        // load the addend from the buffer
                        uint64_t addend;
                        memcpy(&addend, &data[actual_pos], sizeof(addend));

                        if (g->objects[k].type == TB_INIT_OBJ_RELOC) {
                            const TB_Symbol* s = g->objects[k].reloc;

                            switch (s->tag) {
                                case TB_SYMBOL_GLOBAL: {
                                    const TB_Global* g = (const TB_Global*) s;

                                    Elf64_Rela rela = {
                                        .r_offset = actual_pos,
                                        .r_info   = ELF64_R_INFO(g->super.symbol_id, R_X86_64_64),
                                        .r_addend = addend,
                                    };
                                    TB_FIXED_ARRAY_APPEND(relocs, rela);
                                    break;
                                }
                                case TB_SYMBOL_EXTERNAL: {
                                    const TB_External* e = (const TB_External*) s;

                                    Elf64_Rela rela = {
                                        .r_offset = actual_pos,
                                        .r_info   = ELF64_R_INFO(e->super.address, R_X86_64_64),
                                        .r_addend = addend,
                                    };
                                    TB_FIXED_ARRAY_APPEND(relocs, rela);
                                    break;
                                }
                                case TB_SYMBOL_FUNCTION: {
                                    const TB_Function* f = (const TB_Function*) s;

                                    Elf64_Rela rela = {
                                        .r_offset = actual_pos,
                                        .r_info   = ELF64_R_INFO(f->compiled_symbol_id, R_X86_64_64),
                                        .r_addend = addend,
                                    };
                                    TB_FIXED_ARRAY_APPEND(relocs, rela);
                                    break;
                                }
                                default: break;
                            }
                        }
                    }
                }
            }

            WRITE(relocs.elems, relocs.count * sizeof(Elf64_Rela));
        }

        e.write_pos = tb_helper_write_section(e.write_pos, &m->rdata, output, sections[S_RODATA].sh_offset);

        assert(e.write_pos == sections[S_STAB].sh_offset);
        WRITE(stab.data, stab.count);

        assert(e.write_pos == header.e_shoff);
        WRITE(sections, S_MAX * sizeof(Elf64_Shdr));
    }

    // Done
    tb_platform_heap_free(strtbl.data);
    tb_platform_heap_free(stab.data);

    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}
