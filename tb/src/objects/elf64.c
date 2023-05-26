// Halfway through implementing this ELF64 exporter i realized that my new best friend is
// COFF, ELF is a bitch.
#include "../tb_internal.h"
#include <tb_elf.h>

typedef struct TB_ModuleExporter {
    size_t write_pos;
} TB_ModuleExporter;

static void put_symbol(TB_Emitter* strtbl, TB_Emitter* stab, const char* name, uint8_t sym_info, uint16_t section_index, uint64_t value, uint64_t size) {
    // Fill up the symbol's string table
    size_t name_len = strlen(name);
    size_t name_pos = strtbl->count;

    tb_out_reserve(strtbl, name_len + 1);
    tb_outs_UNSAFE(strtbl, name_len + 1, (uint8_t*)name);

    // Emit symbol
    TB_Elf64_Sym sym = {
        .name  = name_pos,
        .info  = sym_info,
        .shndx = section_index,
        .value = value,
        .size  = size
    };

    tb_outs(stab, sizeof(sym), (uint8_t*)&sym);
}

#define APPEND_SECTION(sec) if (sec.total_size) { dyn_array_put(sections, &sec); }
#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
TB_Exports tb_elf64obj_write_output(TB_Module* m, const IDebugFormat* dbg) {
    CUIK_TIMED_BLOCK("layout section") {
        tb_module_layout_sections(m);
    }

    const ICodeGen* restrict code_gen = tb__find_code_generator(m);

    uint16_t machine = 0;
    switch (m->target_arch) {
        case TB_ARCH_X86_64: machine = EM_X86_64; break;
        case TB_ARCH_AARCH64: machine = EM_AARCH64; break;
        default: tb_todo();
    }

    TB_Emitter strtbl = { 0 };
    tb_out_reserve(&strtbl, 1024);
    tb_out1b(&strtbl, 0); // null string in the table

    TB_Elf64_Ehdr header = {
        .ident = {
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
        .type = ET_REL, // relocatable
        .version = 1,
        .machine = machine,
        .entry = 0,

        // section headers go at the end of the file
        // and are filed in later.
        .shoff = 0,
        .flags = 0,

        .ehsize = sizeof(TB_Elf64_Ehdr),

        .shentsize = sizeof(TB_Elf64_Shdr),
        .shstrndx  = 1,
    };

    // accumulate all sections
    DynArray(TB_ModuleSection*) sections = NULL;
    APPEND_SECTION(m->text);
    APPEND_SECTION(m->data);
    APPEND_SECTION(m->rdata);
    APPEND_SECTION(m->tls);

    int dbg_section_count = (dbg ? dbg->number_of_debug_sections(m) : 0);
    int section_count = 1 + dyn_array_length(sections) + dbg_section_count;

    // calculate relocation layout
    size_t output_size = sizeof(TB_Elf64_Ehdr);
    dyn_array_for(i, sections) {
        sections[i]->section_num = 1 + i;
        sections[i]->raw_data_pos = output_size;
        output_size += sections[i]->total_size;
    }

    // each section with relocations needs a matching .rel section
    output_size = tb__layout_relocations(m, sections, code_gen, output_size, sizeof(TB_Elf64_Rela), false);
    dyn_array_for(i, sections) {
        if (sections[i]->reloc_count > 0) {
            section_count += 1;
            tb_outs(&strtbl, 5, ".rela");
        }
        sections[i]->name_pos = tb_outstr_nul_UNSAFE(&strtbl, sections[i]->name);
    }

    TB_Elf64_Shdr strtab = {
        .name = tb_outstr_nul_UNSAFE(&strtbl, ".strtab"),
        .type = SHT_STRTAB,
        .flags = 0,
        .addralign = 1,
        .size = strtbl.count,
        .offset = output_size,
    };
    output_size += strtbl.count;

    header.shoff = output_size;
    header.shnum = section_count + 1;
    // sections plus the NULL section at the start
    output_size += (1 + section_count) * sizeof(TB_Elf64_Shdr);

    ////////////////////////////////
    // write output
    ////////////////////////////////
    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    WRITE(&header, sizeof(header));

    // write section content
    dyn_array_for(i, sections) {
        write_pos = tb_helper_write_section(m, write_pos, sections[i], output, sections[i]->raw_data_pos);
    }

    // write relocation arrays
    dyn_array_for(i, sections) if (sections[i]->reloc_count > 0) {
        assert(sections[i]->reloc_pos == write_pos);
        TB_Elf64_Rela* rels = (TB_Elf64_Rela*) &output[write_pos];

        // a

        write_pos += sections[i]->reloc_count * sizeof(TB_Elf64_Rela);
    }

    WRITE(strtbl.data, strtbl.count);

    // write section header
    memset(&output[write_pos], 0, sizeof(TB_Elf64_Shdr)), write_pos += sizeof(TB_Elf64_Shdr);
    WRITE(&strtab, sizeof(strtab));
    dyn_array_for(i, sections) {
        TB_Elf64_Shdr sec = {
            .name = sections[i]->name_pos,
            .type = SHT_PROGBITS,
            .flags = SHF_ALLOC | ((sections[i] == &m->text) ? SHF_EXECINSTR : SHF_WRITE),
            .addralign = 16,
            .size = sections[i]->total_size,
            .offset = sections[i]->raw_data_pos,
        };
        WRITE(&sec, sizeof(sec));
    }

    dyn_array_for(i, sections) if (sections[i]->reloc_count) {
        TB_Elf64_Shdr sec = {
            .name = sections[i]->name_pos - 5,
            .type = SHT_RELA,
            .flags = SHF_INFO_LINK,
            .addralign = 16,
            .info = 1 + i,
            .size = sections[i]->reloc_count * sizeof(TB_Elf64_Rela),
            .offset = sections[i]->reloc_pos,
            .entsize = sizeof(TB_Elf64_Rela)
        };
        WRITE(&sec, sizeof(sec));
    }

    tb_todo();

    assert(write_pos == output_size);
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

#if 0
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
    size_t text_reloc_count = 0;
    TB_FOR_FUNCTIONS(f, m) {
        text_reloc_count += f->patch_count;

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

    sections[S_TEXT_REL].sh_size = text_reloc_count * sizeof(Elf64_Rela);
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
        e.write_pos = tb_helper_write_section(m, e.write_pos, &m->text, output, sections[S_TEXT].sh_offset);

        // TEXT patches
        {
            assert(e.write_pos == sections[S_TEXT_REL].sh_offset);

            TB_FIXED_ARRAY(Elf64_Rela) relocs = {
                .cap = sections[S_TEXT_REL].sh_size / sizeof(Elf64_Rela),
                .elems = (Elf64_Rela*) &output[sections[S_TEXT_REL].sh_offset]
            };

            TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
                for (TB_SymbolPatch* p = f->last_patch; p; p = p->prev) {
                    size_t symbol_id = p->target->symbol_id;
                    assert(symbol_id != 0);

                    TB_FunctionOutput* out_f = p->source->output;
                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;

                    if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                        Elf64_Rela rela = {
                            .r_offset = actual_pos,
                            // check when we should prefer R_X86_64_GOTPCREL
                            .r_info   = ELF64_R_INFO(symbol_id, R_X86_64_PLT32),
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

        e.write_pos = tb_helper_write_section(m, e.write_pos, &m->data, output, sections[S_DATA].sh_offset);

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

        e.write_pos = tb_helper_write_section(m, e.write_pos, &m->rdata, output, sections[S_RODATA].sh_offset);

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
#endif
