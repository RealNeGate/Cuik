#include "../tb_internal.h"
#include <tb_elf.h>
#include <log.h>

static bool is_nonlocal(const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_GLOBAL) {
        return ((TB_Global*) s)->linkage == TB_LINKAGE_PUBLIC;
    } else if (s->tag == TB_SYMBOL_FUNCTION) {
        return ((TB_Function*) s)->linkage == TB_LINKAGE_PUBLIC;
    } else {
        return true;
    }
}

static void put_symbol(TB_Emitter* stab, uint32_t name, uint8_t sym_info, uint16_t section_index, uint64_t value, uint64_t size) {
    // Emit symbol
    TB_Elf64_Sym sym = {
        .name  = name,
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
        case TB_ARCH_X86_64: machine = TB_EM_X86_64; break;
        case TB_ARCH_AARCH64: machine = TB_EM_AARCH64; break;
        default: tb_todo();
    }

    TB_Emitter strtbl = { 0 };
    tb_out_reserve(&strtbl, 1024);
    tb_out1b(&strtbl, 0); // null string in the table

    TB_Elf64_Ehdr header = {
        .ident = {
            [TB_EI_MAG0]       = 0x7F, // magic number
            [TB_EI_MAG1]       = 'E',
            [TB_EI_MAG2]       = 'L',
            [TB_EI_MAG3]       = 'F',
            [TB_EI_CLASS]      = 2, // 64bit ELF file
            [TB_EI_DATA]       = 1, // little-endian
            [TB_EI_VERSION]    = 1, // 1.0
            [TB_EI_OSABI]      = 0,
            [TB_EI_ABIVERSION] = 0
        },
        .type = TB_ET_REL, // relocatable
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
    int section_count = 2 + dyn_array_length(sections) + dbg_section_count;

    size_t output_size = sizeof(TB_Elf64_Ehdr);
    dyn_array_for(i, sections) {
        sections[i]->section_num = 3 + i;
        sections[i]->raw_data_pos = output_size;
        output_size += sections[i]->total_size;
    }

    // calculate relocation layout
    // each section with relocations needs a matching .rel section
    output_size = tb__layout_relocations(m, sections, code_gen, output_size, sizeof(TB_Elf64_Rela), false);
    dyn_array_for(i, sections) {
        if (sections[i]->reloc_count > 0) {
            section_count += 1;
            tb_outs(&strtbl, 5, ".rela");
        }

        sections[i]->name_pos = tb_outstr_nul_UNSAFE(&strtbl, sections[i]->name);
    }

    // calculate symbol IDs
    TB_Emitter local_symtab = { 0 }, global_symtab = { 0 };
    tb_out_zero(&local_symtab, sizeof(TB_Elf64_Sym));
    dyn_array_for(i, sections) {
        put_symbol(&local_symtab, sections[i]->name_pos, TB_ELF64_ST_INFO(TB_ELF64_STB_LOCAL, TB_ELF64_STT_SECTION), 1 + i, 0, 0);
    }

    dyn_array_for(i, sections) if (sections[i]->reloc_count) {
        put_symbol(&local_symtab, sections[i]->name_pos - 5, TB_ELF64_ST_INFO(TB_ELF64_STB_LOCAL, TB_ELF64_STT_SECTION), 1 + i, 0, 0);
    }

    assert(dbg_section_count == 0);

    int text_sec_num = m->text.section_num;
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f == NULL) continue;

        uint32_t name = f->super.name ? tb_outstr_nul_UNSAFE(&strtbl, f->super.name) : 0;
        int t = (f->linkage == TB_LINKAGE_PUBLIC) ? TB_ELF64_STB_GLOBAL : TB_ELF64_STB_LOCAL;

        TB_Emitter* stab = (f->linkage == TB_LINKAGE_PUBLIC) ? &global_symtab : &local_symtab;
        f->super.symbol_id = stab->count / sizeof(TB_Elf64_Sym);

        put_symbol(stab, name, TB_ELF64_ST_INFO(t, TB_ELF64_STT_FUNC), text_sec_num, out_f->code_pos, out_f->code_size);
    }

    int counter = 0;
    TB_FOR_GLOBALS(g, m) {
        uint32_t name = 0;
        if (g->super.name) {
            name = tb_outstr_nul_UNSAFE(&strtbl, g->super.name);
        } else {
            char buf[8];
            snprintf(buf, 8, "$%06d", counter++);
            name = tb_outstr_nul_UNSAFE(&strtbl, buf);
        }

        int t = (g->linkage == TB_LINKAGE_PUBLIC) ? TB_ELF64_STB_GLOBAL : TB_ELF64_STB_LOCAL;

        int sec_num = g->parent->section_num;
        TB_Emitter* stab = (g->linkage == TB_LINKAGE_PUBLIC) ? &global_symtab : &local_symtab;
        g->super.symbol_id = stab->count / sizeof(TB_Elf64_Sym);

        put_symbol(stab, name, TB_ELF64_ST_INFO(t, TB_ELF64_STT_OBJECT), sec_num, g->pos, 0);
    }

    TB_FOR_EXTERNALS(ext, m) if (ext->super.name) {
        uint32_t name = tb_outstr_nul_UNSAFE(&strtbl, ext->super.name);
        ext->super.symbol_id = global_symtab.count / sizeof(TB_Elf64_Sym);

        put_symbol(&global_symtab, name, TB_ELF64_ST_INFO(TB_ELF64_STB_GLOBAL, 0), 0, 0, 0);
    }

    uint32_t symtab_name = tb_outstr_nul_UNSAFE(&strtbl, ".symtab");
    TB_Elf64_Shdr strtab = {
        .name = tb_outstr_nul_UNSAFE(&strtbl, ".strtab"),
        .type = TB_SHT_STRTAB,
        .flags = 0,
        .addralign = 1,
        .size = strtbl.count,
        .offset = output_size,
    };
    output_size += strtbl.count;

    TB_Elf64_Shdr symtab = {
        .name = symtab_name,
        .type = TB_SHT_SYMTAB,
        .flags = 0, .addralign = 1,
        .link = 1, .info = local_symtab.count / sizeof(TB_Elf64_Sym), /* first non-local */
        .size = local_symtab.count + global_symtab.count,
        .entsize = sizeof(TB_Elf64_Sym),
        .offset = output_size,
    };
    output_size += local_symtab.count;
    output_size += global_symtab.count;

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
    size_t local_sym_count = local_symtab.count / sizeof(TB_Elf64_Sym);
    dyn_array_for(i, sections) if (sections[i]->reloc_count > 0) {
        assert(sections[i]->reloc_pos == write_pos);
        TB_Elf64_Rela* rels = (TB_Elf64_Rela*) &output[write_pos];

        if (sections[i]->kind == TB_MODULE_SECTION_TEXT) {
            TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
                for (TB_SymbolPatch* p = f->last_patch; p; p = p->prev) {
                    if (p->internal) continue;

                    TB_FunctionOutput* out_f = p->source->output;
                    size_t actual_pos = out_f->prologue_length + p->pos;
                    if (f->comdat.type == TB_COMDAT_NONE) {
                        actual_pos += out_f->code_pos;
                    }

                    size_t symbol_id = p->target->symbol_id;
                    if (is_nonlocal(p->target)) {
                        symbol_id += local_sym_count;
                    }
                    assert(symbol_id != 0);
                    log_debug("%d", symbol_id);

                    TB_ELF_RelocType type = p->target->tag == TB_SYMBOL_GLOBAL ? TB_ELF_X86_64_PC32 : TB_ELF_X86_64_PLT32;
                    *rels++ = (TB_Elf64_Rela){
                        .offset = actual_pos,
                        // check when we should prefer R_X86_64_GOTPCREL
                        .info   = TB_ELF64_R_INFO(symbol_id, type),
                        .addend = -4
                    };
                }
            }
        } else {

        }

        write_pos += sections[i]->reloc_count * sizeof(TB_Elf64_Rela);
    }

    assert(write_pos == strtab.offset);
    WRITE(strtbl.data, strtbl.count);

    assert(write_pos == symtab.offset);
    WRITE(local_symtab.data, local_symtab.count);
    WRITE(global_symtab.data, global_symtab.count);

    // write section header
    memset(&output[write_pos], 0, sizeof(TB_Elf64_Shdr)), write_pos += sizeof(TB_Elf64_Shdr);
    WRITE(&strtab, sizeof(strtab));
    WRITE(&symtab, sizeof(symtab));
    dyn_array_for(i, sections) {
        TB_Elf64_Shdr sec = {
            .name = sections[i]->name_pos,
            .type = TB_SHT_PROGBITS,
            .flags = TB_SHF_ALLOC | ((sections[i] == &m->text) ? TB_SHF_EXECINSTR : TB_SHF_WRITE),
            .addralign = 16,
            .size = sections[i]->total_size,
            .offset = sections[i]->raw_data_pos,
        };
        WRITE(&sec, sizeof(sec));
    }

    dyn_array_for(i, sections) if (sections[i]->reloc_count) {
        TB_Elf64_Shdr sec = {
            .name = sections[i]->name_pos - 5,
            .type = TB_SHT_RELA,
            .flags = TB_SHF_INFO_LINK,
            .addralign = 16,
            .info = 3 + i,
            .link = 2,
            .size = sections[i]->reloc_count * sizeof(TB_Elf64_Rela),
            .offset = sections[i]->reloc_pos,
            .entsize = sizeof(TB_Elf64_Rela)
        };
        WRITE(&sec, sizeof(sec));
    }

    log_debug("TODO");
    // tb_todo();

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
            .sh_type = TB_SHT_STRTAB,
            .sh_flags = 0,
            .sh_addralign = 1
        },
        [S_TEXT] = {
            .sh_type = TB_SHT_PROGBITS,
            .sh_flags = TB_SHF_EXECINSTR | TB_SHF_ALLOC,
            .sh_addralign = 16
        },
        [S_TEXT_REL] = {
            .sh_type = TB_SHT_RELA,
            .sh_flags = TB_SHF_INFO_LINK,
            .sh_link = 7,
            .sh_info = S_TEXT,
            .sh_addralign = 16,
            .sh_entsize = sizeof(Elf64_Rela)
        },
        [S_DATA] = {
            .sh_type = TB_SHT_PROGBITS,
            .sh_flags = TB_SHF_ALLOC | TB_SHF_WRITE,
            .sh_addralign = 16
        },
        [S_DATA_REL] = {
            .sh_type = TB_SHT_RELA,
            .sh_flags = TB_SHF_INFO_LINK,
            .sh_link = 7,
            .sh_info = S_DATA,
            .sh_addralign = 16,
            .sh_entsize = sizeof(Elf64_Rela)
        },
        [S_RODATA] = {
            .sh_type = TB_SHT_PROGBITS,
            .sh_flags = TB_SHF_ALLOC,
            .sh_addralign = 16
        },
        [S_BSS] = {
            .sh_type = TB_SHT_NOBITS,
            .sh_flags = TB_SHF_ALLOC | TB_SHF_WRITE,
            .sh_addralign = 16
        },
        [S_STAB] = {
            .sh_type = TB_SHT_SYMTAB,
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
