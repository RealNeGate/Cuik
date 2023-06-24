#include "coff.h"
#include <cuik_perf.h>

static int compare_relocs(const void* a, const void* b) {
    const COFF_ImageReloc* aa = (const COFF_ImageReloc*) a;
    const COFF_ImageReloc* bb = (const COFF_ImageReloc*) b;

    return aa->VirtualAddress - bb->VirtualAddress;
}

static COFF_Symbol section_sym(const char* name, int num, int sc) {
    COFF_Symbol s = { .section_number = num, .storage_class = sc, .aux_symbols_count = 1 };
    strncpy((char*) s.short_name, name, 8);
    return s;
}

static COFF_AuxSectionSymbol section_aux_sym(COFF_SectionHeader* s, int num) {
    return (COFF_AuxSectionSymbol){
        .length = s->raw_data_size,
        .reloc_count = s->num_reloc,
        .number = num,
    };
}

#define APPEND_SECTION(sec) if (sec.total_size) { dyn_array_put(sections, &sec); }
#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
TB_Exports tb_coff_write_output(TB_Module* m, const IDebugFormat* dbg) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    CUIK_TIMED_BLOCK("layout section") {
        tb_module_layout_sections(m);
    }

    // mark correct flags on sections
    m->text.flags = COFF_CHARACTERISTICS_TEXT;
    m->data.flags = COFF_CHARACTERISTICS_DATA;
    m->rdata.flags = COFF_CHARACTERISTICS_RODATA;
    m->tls.flags = COFF_CHARACTERISTICS_DATA;

    // accumulate all sections
    DynArray(TB_ModuleSection*) sections = NULL;
    APPEND_SECTION(m->text);
    APPEND_SECTION(m->data);
    APPEND_SECTION(m->rdata);
    APPEND_SECTION(m->tls);

    int dbg_section_count = (dbg ? dbg->number_of_debug_sections(m) : 0);
    int section_count = dyn_array_length(sections) + dbg_section_count;
    size_t normal_function_count = m->compiled_function_count - m->comdat_function_count;

    // .pdata and .xdata
    size_t xdata_section_num = section_count*2 + 2;
    section_count += 2;

    TB_Emitter xdata = { 0 };
    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    if (code_gen->emit_win64eh_unwind_info) {
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;

            if (out_f != NULL) {
                out_f->unwind_info = xdata.count;
                code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->prologue_epilogue_metadata, out_f->stack_usage);
            }
        }
    } else {
        tb_panic("write_xdata_section: emit_win64eh_unwind_info is required.");
    }

    // mark each with a unique id
    size_t unique_id_counter = section_count * 2;
    {
        section_count += m->comdat_function_count;

        CUIK_TIMED_BLOCK("AllocSymbolIDs") {
            TB_FOR_FUNCTIONS(f, m) if (f->output && f->comdat.type != TB_COMDAT_NONE) {
                f->super.symbol_id = unique_id_counter + 2;
                unique_id_counter += 3;
            }

            TB_FOR_FUNCTIONS(f, m) if (f->output && f->comdat.type == TB_COMDAT_NONE) {
                f->super.symbol_id = unique_id_counter++;
            }

            TB_FOR_EXTERNALS(ext, m) ext->super.symbol_id = unique_id_counter++;
            TB_FOR_GLOBALS(g, m) g->super.symbol_id = unique_id_counter++;
        }
    }

    size_t string_table_cap = unique_id_counter;
    const char** string_table = tb_platform_heap_alloc(string_table_cap * sizeof(char*));

    // COFF file header & section headers
    COFF_FileHeader header = {
        .section_count = section_count,
        .timestamp = 1056582000u, // my birthday since that's consistent :P
        .symbol_count = unique_id_counter,
        .symbol_table = 0,
        .flags = IMAGE_FILE_LINE_NUMS_STRIPPED
    };

    switch (m->target_arch) {
        case TB_ARCH_X86_64:  header.machine = COFF_MACHINE_AMD64; break;
        case TB_ARCH_AARCH64: header.machine = COFF_MACHINE_ARM64; break;
        default: tb_todo();
    }

    // calculate output size & layout object
    size_t output_size = sizeof(COFF_FileHeader);
    output_size += section_count * sizeof(COFF_SectionHeader);
    dyn_array_for(i, sections) {
        sections[i]->section_num = 1 + i;
        sections[i]->raw_data_pos = output_size;
        output_size += sections[i]->total_size;
    }

    TB_SectionGroup debug_sections = { 0 };
    if (dbg) {
        debug_sections = dbg->generate_debug_info(m, tls, code_gen, "fallback.obj");
    }

    FOREACH_N(i, 0, debug_sections.length) {
        debug_sections.data[i].virtual_address = output_size;
        output_size += debug_sections.data[i].raw_data.length;
    }

    // pdata + xdata
    size_t pdata_patch_count = m->compiled_function_count * 3;
    size_t pdata_pos = output_size, pdata_size = pdata_patch_count * sizeof(uint32_t);
    output_size += pdata_size;

    size_t xdata_pos = output_size;
    output_size += xdata.count;

    // calculate relocation layout
    output_size = tb__layout_relocations(m, sections, code_gen, output_size, sizeof(COFF_ImageReloc), false);

    FOREACH_N(i, 0, debug_sections.length) {
        size_t reloc_count = debug_sections.data[i].relocation_count;

        // we're storing the relocation array's file pos here
        debug_sections.data[i].user_data = (void*) (uintptr_t) output_size;
        output_size += reloc_count * sizeof(COFF_ImageReloc);
    }

    size_t pdata_patch_pos = output_size;
    output_size += pdata_patch_count * sizeof(COFF_ImageReloc);

    header.symbol_table = output_size;
    output_size += header.symbol_count * sizeof(COFF_Symbol);

    // compute string table size
    size_t string_table_size = 4;
    size_t string_table_pos = output_size;
    CUIK_TIMED_BLOCK("StringTblLayout") {
        TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
            size_t name_len = strlen(f->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }

        TB_FOR_EXTERNALS(ext, m) if (ext->super.name) {
            size_t name_len = strlen(ext->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }

        TB_FOR_GLOBALS(g, m) if (g->super.name) {
            size_t name_len = strlen(g->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }
    }

    output_size += string_table_size;

    ////////////////////////////////
    // write output
    ////////////////////////////////
    size_t write_pos = 0;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    CUIK_TIMED_BLOCK("write output") {
        // write COFF header
        WRITE(&header, sizeof(header));

        // write sections headers
        dyn_array_for(i, sections) {
            COFF_SectionHeader header = {
                .characteristics = sections[i]->flags,
                .raw_data_size = sections[i]->total_size - sections[i]->total_comdat,
                .raw_data_pos = sections[i]->raw_data_pos,
                .pointer_to_reloc = sections[i]->reloc_pos
            };

            size_t len = strlen(sections[i]->name);
            memcpy(header.name, sections[i]->name, len > 8 ? 8 : len);

            if (sections[i]->reloc_count >= 0xFFFF) {
                header.num_reloc = 0xFFFF;
                header.characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            } else {
                header.num_reloc = sections[i]->reloc_count;
            }

            WRITE(&header, sizeof(header));
        }

        FOREACH_N(i, 0, debug_sections.length) {
            COFF_SectionHeader header = {
                .characteristics = COFF_CHARACTERISTICS_DEBUG,
                .raw_data_size = debug_sections.data[i].raw_data.length,
                .raw_data_pos = debug_sections.data[i].virtual_address,
                .pointer_to_reloc = (uintptr_t) debug_sections.data[i].user_data
            };

            /*if (dbg == &tb__codeview_debug_format) {
                header.characteristics |= IMAGE_SCN_MEM_DISCARDABLE;
            }*/

            TB_Slice name = debug_sections.data[i].name;
            assert(name.length <= 8);
            memcpy(header.name, name.data, name.length);

            size_t reloc_count = debug_sections.data[i].relocation_count;
            if (reloc_count >= 0xFFFF) {
                header.num_reloc = 0xFFFF;
                header.characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            } else {
                header.num_reloc = reloc_count;
            }

            WRITE(&header, sizeof(header));
        }

        // pdata then xdata
        FOREACH_N(i, 0, 2) {
            COFF_SectionHeader header = {
                .characteristics = COFF_CHARACTERISTICS_RODATA,
                .raw_data_size = i ? xdata.count : pdata_size,
                .raw_data_pos = i ? xdata_pos : pdata_pos,
                .pointer_to_reloc = i ? 0 : pdata_patch_pos,
                .num_reloc = i ? 0 : pdata_patch_count,
            };
            strncpy(header.name, i ? ".xdata" : ".pdata", 8);

            if (i == 0 && pdata_patch_count >= 0xFFFF) {
                header.num_reloc = 0xFFFF;
                header.characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            }

            WRITE(&header, sizeof(header));
        }

        TB_FOR_FUNCTIONS(f, m) if (f->output && f->comdat.type != TB_COMDAT_NONE) {
            COFF_SectionHeader header = {
                .characteristics = m->text.flags | IMAGE_SCN_LNK_COMDAT,
                .raw_data_size = f->output->code_size,
                .raw_data_pos = m->text.raw_data_pos + f->output->code_pos,
                .pointer_to_reloc = f->patch_pos,
                .num_reloc = f->patch_count,
            };
            strncpy(header.name, ".text", 8);

            /*if (sections[i]->reloc_count >= 0xFFFF) {
                header.num_reloc = 0xFFFF;
                header.characteristics |= IMAGE_SCN_LNK_NRELOC_OVFL;
            } else {
                header.num_reloc = sections[i]->reloc_count;
            }*/

            WRITE(&header, sizeof(header));
        }

        // write raw data
        dyn_array_for(i, sections) {
            write_pos = tb_helper_write_section(
                m, write_pos, sections[i], output, sections[i]->raw_data_pos
            );
        }

        FOREACH_N(i, 0, debug_sections.length) {
            assert(write_pos == debug_sections.data[i].virtual_address);

            memcpy(&output[write_pos], debug_sections.data[i].raw_data.data, debug_sections.data[i].raw_data.length);
            write_pos += debug_sections.data[i].raw_data.length;
        }

        // write pdata & xdata
        {
            uint32_t* pdata = (uint32_t*) &output[write_pos];
            write_pos += pdata_patch_count * sizeof(uint32_t);

            size_t j = 0;
            TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
                TB_FunctionOutput* out_f = f->output;
                uint32_t pos = f->comdat.type != TB_COMDAT_NONE ? 0 : out_f->code_pos;

                pdata[j+0] = pos;
                pdata[j+1] = pos + out_f->code_size;
                pdata[j+2] = out_f->unwind_info;
                j += 3;
            }

            memcpy(&output[write_pos], xdata.data, xdata.count);
            write_pos += xdata.count;
        }

        // write relocations
        dyn_array_for(i, sections) {
            assert(write_pos == sections[i]->reloc_pos);

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
                        if (p->target->tag == TB_SYMBOL_FUNCTION || p->target->tag == TB_SYMBOL_EXTERNAL) {
                            assert(symbol_id != 0);
                            COFF_ImageReloc r = {
                                .Type = IMAGE_REL_AMD64_REL32,
                                .SymbolTableIndex = symbol_id,
                                .VirtualAddress = actual_pos
                            };
                            WRITE(&r, sizeof(r));
                        } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
                            assert(symbol_id != 0);
                            COFF_ImageReloc r = {
                                .Type = ((TB_Global*) p->target)->parent->kind == TB_MODULE_SECTION_TLS ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                                .SymbolTableIndex = symbol_id,
                                .VirtualAddress = actual_pos
                            };
                            WRITE(&r, sizeof(r));
                        } else {
                            tb_todo();
                        }
                    }
                }
            } else {
                dyn_array_for(j, sections[i]->globals) {
                    TB_Global* restrict g = sections[i]->globals[j];

                    FOREACH_N(k, 0, g->obj_count) {
                        size_t actual_pos = g->pos + g->objects[k].offset;

                        if (g->objects[k].type == TB_INIT_OBJ_RELOC) {
                            const TB_Symbol* s = g->objects[k].reloc;

                            COFF_ImageReloc r = {
                                .Type = IMAGE_REL_AMD64_ADDR64,
                                .SymbolTableIndex = s->symbol_id,
                                .VirtualAddress = actual_pos
                            };
                            WRITE(&r, sizeof(r));
                        }
                    }
                }
            }
        }

        FOREACH_N(i, 0, debug_sections.length) {
            assert(write_pos == (uintptr_t) debug_sections.data[i].user_data);
            FOREACH_N(j, 0, debug_sections.data[i].relocation_count) {
                TB_ObjectReloc* in_reloc = &debug_sections.data[i].relocations[j];

                COFF_ImageReloc r = {
                    .SymbolTableIndex = in_reloc->symbol_index,
                    .VirtualAddress = in_reloc->virtual_address
                };

                switch (in_reloc->type) {
                    case TB_OBJECT_RELOC_SECREL:  r.Type = IMAGE_REL_AMD64_SECREL; break;
                    case TB_OBJECT_RELOC_SECTION: r.Type = IMAGE_REL_AMD64_SECTION; break;
                    default: tb_todo();
                }
                WRITE(&r, sizeof(r));
            }
        }

        // write pdata & xdata
        {
            assert(write_pos == pdata_patch_pos);
            COFF_ImageReloc* relocs = (COFF_ImageReloc*) &output[write_pos];
            write_pos += pdata_patch_count * sizeof(COFF_ImageReloc);

            size_t j = 0;
            if (pdata_patch_count >= 0xFFFF) {
                relocs[j++] = (COFF_ImageReloc){
                    .VirtualAddress = pdata_patch_count
                };
            }

            TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
                TB_FunctionOutput* out_f = f->output;
                uint32_t sym = f->comdat.type != TB_COMDAT_NONE ? f->super.symbol_id : 0;

                relocs[j + 0] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = sym,
                    .VirtualAddress = j * 4
                };

                relocs[j + 1] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = sym,
                    .VirtualAddress = (j * 4) + 4
                };

                relocs[j + 2] = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = xdata_section_num, // xdata section
                    .VirtualAddress = (j * 4) + 8
                };
                j += 3;
            }
        }

        // write symbols
        assert(write_pos == header.symbol_table);
        size_t symbol_count = 1;
        dyn_array_for(i, sections) {
            COFF_Symbol s = {
                .section_number = symbol_count,
                .storage_class = IMAGE_SYM_CLASS_STATIC,
                .aux_symbols_count = 1
            };
            strncpy((char*) s.short_name, sections[i]->name, 8);
            WRITE(&s, sizeof(s));

            COFF_AuxSectionSymbol aux = {
                .length = sections[i]->total_size,
                .reloc_count = sections[i]->reloc_count,
                .number = symbol_count,
            };
            WRITE(&aux, sizeof(aux));
            symbol_count++;
        }

        FOREACH_N(i, 0, debug_sections.length) {
            COFF_Symbol s = {
                .section_number = symbol_count,
                .storage_class = IMAGE_SYM_CLASS_STATIC,
                .aux_symbols_count = 1
            };

            int l = debug_sections.data[i].name.length;
            if (l > 8) l = 8;
            strncpy((char*) s.short_name, (const char*) debug_sections.data[i].name.data, l);

            WRITE(&s, sizeof(s));

            COFF_AuxSectionSymbol aux = {
                .length = debug_sections.data[i].raw_data.length,
                .reloc_count = debug_sections.data[i].relocation_count,
                .number = symbol_count,
            };
            WRITE(&aux, sizeof(aux));
            symbol_count++;
        }

        FOREACH_N(i, 0, 2) {
            COFF_Symbol s = {
                .section_number = symbol_count,
                .storage_class = IMAGE_SYM_CLASS_STATIC,
                .aux_symbols_count = 1
            };
            strncpy((char*) s.short_name, i ? ".xdata" : ".pdata", 8);
            WRITE(&s, sizeof(s));

            COFF_AuxSectionSymbol aux = {
                .length = i ? xdata.count : pdata_size,
                .reloc_count = i ? 0 : pdata_patch_count,
                .number = symbol_count,
            };
            WRITE(&aux, sizeof(aux));
            symbol_count++;
        }

        // each COMDAT function needs a section symbol and function symbol
        uint32_t string_table_mark = 4;
        size_t string_table_length = 0;
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;
            if (out_f == NULL || f->comdat.type == TB_COMDAT_NONE) continue;

            // write section symbol
            {
                COFF_Symbol s = {
                    .short_name = { ".text" },
                    .section_number = symbol_count,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                };
                WRITE(&s, sizeof(s));

                COFF_AuxSectionSymbol aux = {
                    .length = out_f->code_size,
                    .reloc_count = f->patch_count,
                    .selection = 2, // pick any
                };
                WRITE(&aux, sizeof(aux));
            }

            bool is_extern = out_f->linkage == TB_LINKAGE_PUBLIC;
            COFF_Symbol sym = {
                .value = 0,
                .section_number = symbol_count,
                .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
            };

            const char* name = f->super.name;
            size_t name_len = strlen(name);
            assert(name_len < UINT16_MAX);
            if (name_len >= 8) {
                sym.long_name[0] = 0; // this value is 0 for long names
                sym.long_name[1] = string_table_mark;

                string_table[string_table_length++] = (char*)name;
                string_table_mark += name_len + 1;
            } else {
                memcpy(sym.short_name, name, name_len + 1);
            }

            WRITE(&sym, sizeof(sym));
            symbol_count++;
        }

        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;
            if (out_f == NULL || f->comdat.type != TB_COMDAT_NONE) continue;

            bool is_extern = out_f->linkage == TB_LINKAGE_PUBLIC;
            COFF_Symbol sym = {
                .value = out_f->code_pos,
                .section_number = 1,
                .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
            };

            const char* name = f->super.name;
            size_t name_len = strlen(name);
            assert(name_len < UINT16_MAX);
            if (name_len >= 8) {
                sym.long_name[0] = 0; // this value is 0 for long names
                sym.long_name[1] = string_table_mark;

                string_table[string_table_length++] = (char*)name;
                string_table_mark += name_len + 1;
            } else {
                memcpy(sym.short_name, name, name_len + 1);
            }

            WRITE(&sym, sizeof(sym));
        }

        TB_FOR_EXTERNALS(ext, m) {
            COFF_Symbol sym = {
                .value = 0,
                .section_number = 0,
                .storage_class = IMAGE_SYM_CLASS_EXTERNAL
            };

            size_t name_len = strlen(ext->super.name);
            assert(name_len < UINT16_MAX);

            if (name_len >= 8) {
                sym.long_name[0] = 0; // this value is 0 for long names
                sym.long_name[1] = string_table_mark;

                string_table[string_table_length++] = ext->super.name;
                string_table_mark += name_len + 1;
            } else {
                memcpy(sym.short_name, ext->super.name, name_len + 1);
            }

            WRITE(&sym, sizeof(sym));
        }

        TB_FOR_GLOBALS(g, m) {
            bool is_extern = g->linkage == TB_LINKAGE_PUBLIC;
            COFF_Symbol sym = {
                .value = g->pos,
                .section_number = g->parent->section_num,
                .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
            };

            if (g->super.name) {
                size_t name_len = strlen(g->super.name);
                assert(name_len < UINT16_MAX);

                if (name_len >= 8) {
                    sym.long_name[0] = 0; // this value is 0 for long names
                    sym.long_name[1] = string_table_mark;

                    string_table[string_table_length++] = g->super.name;
                    string_table_mark += name_len + 1;
                } else {
                    memcpy(sym.short_name, g->super.name, name_len + 1);
                }
            } else {
                snprintf((char*) sym.short_name, 8, "$%06zu", g->super.symbol_id);
            }

            WRITE(&sym, sizeof(sym));
        }

        // write string table
        {
            (void) string_table_pos;
            assert(write_pos == string_table_pos);

            WRITE(&string_table_mark, sizeof(string_table_mark));
            FOREACH_N(i, 0, string_table_length) {
                const char* s = string_table[i];
                size_t l = strlen(s) + 1;

                WRITE(s, l);
            }
        }
    }

    assert(write_pos == output_size);
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}
