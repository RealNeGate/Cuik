#include "coff.h"

// my section numbers in TB_ModuleExporterCOFF.sections
enum {
    S_TEXT,
    S_RDATA,
    S_DATA,
    S_PDATA,
    S_XDATA,
    S_TLS,
    S_MAX
};

struct TB_ModuleExporter {
    const ICodeGen* code_gen;
    size_t write_pos;

    // String table array, stores the strings which will be put
    // into the string table
    uint32_t string_table_length;
    uint32_t string_table_mark;
    uint32_t string_table_cap;
    char** string_table;

    size_t string_table_pos;
    size_t tls_section_num;

    TB_SectionGroup debug_sections;
    COFF_SectionHeader* debug_section_headers;
};

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

static size_t append_section_sym(COFF_SymbolUnion* symbols, size_t count, COFF_SectionHeader* section, const char* name, int sc) {
    symbols[count + 0].s = section_sym(name, (count / 2) + 1, sc);
    symbols[count + 1].a = section_aux_sym(section, (count / 2) + 1);
    return count + 2;
}

#define APPEND_SECTION(sec) if (sec.total_size) { dyn_array_put(sections, &sec); }
#define WRITE(data, size) (memcpy(&output[write_pos], data, size), write_pos += (size))
TB_API TB_Exports tb_coff_write_output(TB_Module* m, const IDebugFormat* dbg) {
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

    int section_count = dyn_array_length(sections) + (dbg ? dbg->number_of_debug_sections(m) : 0);

    // mark each with a unique id
    size_t function_sym_start = (section_count * 2);
    size_t external_sym_start = function_sym_start + m->compiled_function_count;

    size_t unique_id_counter = 0;
    TB_FOR_EXTERNALS(ext, m) {
        ext->super.symbol_id = external_sym_start + unique_id_counter;
        unique_id_counter += 1;
    }

    TB_FOR_GLOBALS(g, m) {
        g->super.symbol_id = external_sym_start + unique_id_counter;
        unique_id_counter += 1;
    }

    size_t string_table_cap = unique_id_counter + m->compiled_function_count;
    const char** string_table = tb_platform_heap_alloc(string_table_cap * sizeof(char*));

    // COFF file header & section headers
    COFF_FileHeader header = {
        .section_count = section_count,
        .timestamp = 1056582000u, // my birthday since that's consistent :P
        .symbol_count = 0,
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

    const ICodeGen* restrict code_gen = tb__find_code_generator(m);
    TB_SectionGroup debug_sections = { 0 };
    if (dbg) {
        debug_sections = dbg->generate_debug_info(m, tls, code_gen, "fallback.obj");
    }

    FOREACH_N(i, 0, debug_sections.length) {
        debug_sections.data[i].virtual_address = output_size;
        output_size += debug_sections.data[i].raw_data.length;
    }

    // calculate relocation count
    size_t text_reloc_count = 0;
    FOREACH_N(i, 0, m->max_threads) {
        text_reloc_count += dyn_array_length(m->thread_info[i].symbol_patches);
    }
    text_reloc_count -= code_gen->emit_call_patches(m);

    // calculate symbols
    {
        header.symbol_count = (section_count * 2) + m->compiled_function_count;

        // total externals + total globals
        header.symbol_count += unique_id_counter;
    }

    dyn_array_for(i, sections) {
        size_t reloc_count = 0;
        switch (sections[i]->kind) {
            case TB_MODULE_SECTION_TEXT:
            reloc_count = text_reloc_count;
            break;

            case TB_MODULE_SECTION_DATA:
            case TB_MODULE_SECTION_TLS:
            dyn_array_for(j, sections[i]->globals) {
                TB_Global* restrict g = sections[i]->globals[j];
                FOREACH_N(k, 0, g->obj_count) {
                    reloc_count += (g->objects[k].type == TB_INIT_OBJ_RELOC);
                }
            }
            break;

            default:
            tb_todo();
            break;
        }
        sections[i]->reloc_count = reloc_count;
        sections[i]->reloc_pos = output_size;

        output_size += reloc_count * sizeof(COFF_ImageReloc);
    }

    FOREACH_N(i, 0, debug_sections.length) {
        size_t reloc_count = debug_sections.data[i].relocation_count;

        // we're storing the relocation array's file pos here
        debug_sections.data[i].user_data = (void*) (uintptr_t) output_size;
        output_size += reloc_count * sizeof(COFF_ImageReloc);
    }

    header.symbol_table = output_size;
    output_size += header.symbol_count * sizeof(COFF_Symbol);

    // compute string table size
    size_t string_table_size = 4;
    size_t string_table_pos = output_size;
    TB_FOR_FUNCTIONS(f, m) if (f->super.name) {
        size_t name_len = strlen(f->super.name);
        if (name_len >= 8) string_table_size += name_len + 1;
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) if (ext->super.name) {
            size_t name_len = strlen(ext->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) if (g->super.name) {
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

    // write COFF header
    WRITE(&header, sizeof(header));

    // write sections headers
    dyn_array_for(i, sections) {
        COFF_SectionHeader header = {
            .characteristics = sections[i]->flags,
            .raw_data_size = sections[i]->total_size,
            .raw_data_pos = sections[i]->raw_data_pos,
            .pointer_to_reloc = sections[i]->reloc_pos
        };
        strncpy(header.name, sections[i]->name, 8);

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
            .pointer_to_reloc = (uintptr_t) debug_sections.data[i].user_data
        };

        if (dbg == &tb__codeview_debug_format) {
            header.characteristics |= IMAGE_SCN_MEM_DISCARDABLE;
        }

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

    // write raw data
    FOREACH_N(i, 0, section_count) {
        write_pos = tb_helper_write_section(
            m, write_pos, sections[i], output, sections[i]->raw_data_pos
        );
    }

    FOREACH_N(i, 0, debug_sections.length) {
        assert(write_pos == debug_sections.data[i].virtual_address);

        memcpy(&output[write_pos], debug_sections.data[i].raw_data.data, debug_sections.data[i].raw_data.length);
        write_pos += debug_sections.data[i].raw_data.length;
    }

    // write relocations
    dyn_array_for(i, sections) {
        assert(write_pos == sections[i]->reloc_pos);

        if (sections[i]->kind == TB_MODULE_SECTION_TEXT) {
            FOREACH_N(i, 0, m->max_threads) {
                dyn_array_for(j, m->thread_info[i].symbol_patches) {
                    TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;
                    size_t symbol_id = p->target->symbol_id;

                    if (p->target->tag == TB_SYMBOL_FUNCTION) {
                        // internal patch, already handled
                    } else if (p->target->tag == TB_SYMBOL_EXTERNAL) {
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

    // write symbols
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

    uint32_t string_table_mark = 4;
    size_t string_table_length = 0;
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f == NULL) continue;

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
            snprintf((char*) sym.short_name, 8, "$%07zu", g->super.symbol_id);
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

    assert(write_pos == output_size);
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}

#if 0
TB_API TB_Exports tb_coff_write_output(TB_Module* m, const IDebugFormat* dbg) {
    TB_ModuleExporter* e = tb_platform_heap_alloc(sizeof(TB_ModuleExporter));
    memset(e, 0, sizeof(TB_ModuleExporter));

    e->code_gen = tb__find_code_generator(m);
    e->string_table_mark = 4;

    CUIK_TIMED_BLOCK("layout section") {
        tb_module_layout_sections(m);
    }

    const char* path = "fallback.obj";

    ////////////////////////////////
    // Generate .xdata section (unwind info)
    ////////////////////////////////
    TB_Emitter xdata = { 0 };
    if (e->code_gen->emit_win64eh_unwind_info) {
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;

            if (out_f != NULL) {
                out_f->unwind_info = xdata.count;
                e->code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->prologue_epilogue_metadata, out_f->stack_usage);
            }
        }
    }

    int number_of_sections = 5
        + (m->tls.total_size ? 1 : 0)
        + (dbg != NULL ? dbg->number_of_debug_sections(m) : 0);

    // mark each with a unique id
    size_t function_sym_start = (number_of_sections * 2);
    size_t external_sym_start = function_sym_start + m->compiled_function_count;

    size_t text_section_size = m->text.total_size;
    size_t unique_id_counter = 0;
    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) {
            ext->super.symbol_id = external_sym_start + unique_id_counter;
            unique_id_counter += 1;
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) {
            g->super.symbol_id = external_sym_start + unique_id_counter;
            unique_id_counter += 1;
        }
    }

    e->string_table_cap += unique_id_counter;
    e->string_table_cap += m->compiled_function_count;
    e->string_table = tb_platform_heap_alloc(e->string_table_cap * sizeof(const char*));

    // TODO(NeGate): We might do alphabetical sorting for consistent binary output
    const ICodeGen* restrict code_gen = e->code_gen;

    ////////////////////////////////
    // create headers
    ////////////////////////////////
    size_t num_of_relocs[S_MAX] = { 0 };
    FOREACH_N(t, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[t].globals) {
            FOREACH_N(k, 0, g->obj_count) {
                num_of_relocs[S_DATA] += (g->objects[k].type != TB_INIT_OBJ_REGION);
            }
        }
    }

    // COFF file header & section headers
    COFF_FileHeader header = {
        .section_count = number_of_sections,
        .timestamp = time(NULL),
        .symbol_count = 0,
        .symbol_table = 0,
        .flags = IMAGE_FILE_LINE_NUMS_STRIPPED
    };

    num_of_relocs[S_PDATA] = m->compiled_function_count * 3;
    COFF_SectionHeader sections[S_MAX] = {
        [S_TEXT] = {
            .name = { ".text" }, // .text
            .characteristics = COFF_CHARACTERISTICS_TEXT,
            .raw_data_size = text_section_size,
        },
        [S_RDATA] = (COFF_SectionHeader){
            .name = { ".rdata" }, // .rdata
            .characteristics = COFF_CHARACTERISTICS_RODATA,
            .raw_data_size = m->rdata.total_size
        },
        [S_DATA] = (COFF_SectionHeader){
            .name = { ".data" }, // .data
            .characteristics = COFF_CHARACTERISTICS_DATA,
            .raw_data_size = m->data.total_size
        },
        [S_PDATA] = (COFF_SectionHeader){
            .name = { ".pdata" }, // .pdata
            .characteristics = COFF_CHARACTERISTICS_RODATA,
            .raw_data_size = m->compiled_function_count * 12,
        },
        [S_XDATA] = (COFF_SectionHeader){
            .name = { ".xdata" }, // .xdata
            .characteristics = COFF_CHARACTERISTICS_RODATA,
            .raw_data_size = xdata.count,
            .num_reloc = 0,
        },
        [S_TLS] = (COFF_SectionHeader){
            .name = { ".tls$" },
            .characteristics = COFF_CHARACTERISTICS_DATA,
            .raw_data_size = m->tls.total_size
        },
    };

    switch (m->target_arch) {
        case TB_ARCH_X86_64:  header.machine = COFF_MACHINE_AMD64; break;
        case TB_ARCH_AARCH64: header.machine = COFF_MACHINE_ARM64; break;
        default: tb_todo();
    }

    if (dbg != NULL) {
        TB_TemporaryStorage* tls = tb_tls_allocate();

        e->debug_sections = dbg->generate_debug_info(m, tls, code_gen, path);
        e->debug_section_headers = tb_platform_heap_alloc(e->debug_sections.length * sizeof(COFF_SectionHeader));

        FOREACH_N(i, 0, e->debug_sections.length) {
            e->debug_section_headers[i] = (COFF_SectionHeader) { 0 };
            e->debug_section_headers[i].characteristics = COFF_CHARACTERISTICS_CV;

            size_t reloc_count = e->debug_sections.data[i].relocation_count;
            e->debug_section_headers[i].num_reloc = reloc_count >= 0xFFFF ? 0xFFFF : reloc_count;

            TB_Slice name = e->debug_sections.data[i].name;
            if (name.length > 8) {
                // we'd need to put it into the string table... im lazy
                // so i wont do the logic for it yet
                tb_todo();
            } else {
                memcpy(e->debug_section_headers[i].name, name.data, name.length);
                if (name.length < 8) e->debug_section_headers[i].name[name.length] = 0;
            }
        }
    }

    // Target specific: resolve internal call patches
    size_t local_patch_count = code_gen->emit_call_patches(m);

    // symbols
    {
        header.symbol_count = (number_of_sections * 2) + m->compiled_function_count;

        // total externals + total globals
        header.symbol_count += unique_id_counter;
    }

    // relocation
    {
        num_of_relocs[S_TEXT] = 0;
        FOREACH_N(i, 0, m->max_threads) {
            num_of_relocs[S_TEXT] += dyn_array_length(m->thread_info[i].symbol_patches);
        }
        num_of_relocs[S_TEXT] -= local_patch_count;
    }

    // layout sections & relocations
    {
        size_t counter = sizeof(COFF_FileHeader) + (number_of_sections * sizeof(COFF_SectionHeader));

        FOREACH_N(i, 0, S_MAX) {
            sections[i].raw_data_pos = counter;
            counter += sections[i].raw_data_size;
        }

        FOREACH_N(i, 0, e->debug_sections.length) {
            e->debug_section_headers[i].raw_data_size = e->debug_sections.data[i].raw_data.length;
            e->debug_section_headers[i].raw_data_pos = tb_post_inc(&counter, e->debug_section_headers[i].raw_data_size);
        }

        // Do the relocation lists next
        FOREACH_N(i, 0, S_MAX) {
            sections[i].pointer_to_reloc = counter;
            sections[i].num_reloc = num_of_relocs[i] >= 0xFFFF ? 0xFFFF : num_of_relocs[i];
            sections[i].characteristics |= (num_of_relocs[i] >= 0xFFFF ? IMAGE_SCN_LNK_NRELOC_OVFL : 0);

            counter += num_of_relocs[i] * sizeof(COFF_ImageReloc);
            // relocation overflow adds a dummy relocation
            counter += (num_of_relocs[i] >= 0xFFFF ? sizeof(COFF_ImageReloc) : 0);
        }

        FOREACH_N(i, 0, e->debug_sections.length) {
            e->debug_section_headers[i].pointer_to_reloc = counter;

            size_t reloc_count = e->debug_sections.data[i].relocation_count;
            e->debug_section_headers[i].num_reloc = reloc_count >= 0xFFFF ? 0xFFFF : reloc_count;
            e->debug_section_headers[i].characteristics |= (reloc_count >= 0xFFFF ? IMAGE_SCN_LNK_NRELOC_OVFL : 0);

            counter += reloc_count * sizeof(COFF_ImageReloc);
            // relocation overflow adds a dummy relocation
            counter += (reloc_count >= 0xFFFF ? sizeof(COFF_ImageReloc) : 0);
        }

        header.symbol_table = tb_post_inc(&counter, header.symbol_count * sizeof(COFF_Symbol));
        e->string_table_pos = counter;
    }

    size_t string_table_size = 4;
    TB_FOR_FUNCTIONS(f, m) if (f->super.name) {
        size_t name_len = strlen(f->super.name);
        if (name_len >= 8) string_table_size += name_len + 1;
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) if (ext->super.name) {
            size_t name_len = strlen(ext->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) if (g->super.name) {
            size_t name_len = strlen(g->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }
    }

    // Allocate memory now
    size_t output_size = e->string_table_pos + string_table_size;
    uint8_t* restrict output = tb_platform_heap_alloc(output_size);

    // Write contents
    {
        WRITE(&header, sizeof(COFF_FileHeader));

        // figure out how many section headers to write out
        int count = (header.section_count - e->debug_sections.length);
        WRITE(sections, count * sizeof(COFF_SectionHeader));
        WRITE(e->debug_section_headers, e->debug_sections.length * sizeof(COFF_SectionHeader));

        e->write_pos = tb_helper_write_section(e->write_pos, &m->text, output, sections[S_TEXT].raw_data_pos);
        e->write_pos = tb_helper_write_section(e->write_pos, &m->rdata, output, sections[S_RDATA].raw_data_pos);
        e->write_pos = tb_helper_write_section(e->write_pos, &m->data, output, sections[S_DATA].raw_data_pos);

        // PDATA section
        {
            assert(e->write_pos == sections[S_PDATA].raw_data_pos);
            uint32_t* pdata = (uint32_t*) &output[e->write_pos];
            e->write_pos += m->compiled_function_count * 12;

            size_t j = 0;
            TB_FOR_FUNCTIONS(f, m) {
                TB_FunctionOutput* out_f = f->output;
                if (out_f != NULL) {
                    pdata[j+0] = out_f->code_pos;
                    pdata[j+1] = out_f->code_pos + out_f->code_size;
                    pdata[j+2] = out_f->unwind_info;
                    j += 3;
                }
            }
        }

        // XDATA section
        {
            assert(e->write_pos == sections[S_XDATA].raw_data_pos);
            WRITE(xdata.data, xdata.count);
        }

        // TLS section
        e->write_pos = tb_helper_write_section(e->write_pos, &m->tls, output, sections[S_TLS].raw_data_pos);

        // write DEBUG sections
        FOREACH_N(i, 0, e->debug_sections.length) {
            assert(e->write_pos == e->debug_section_headers[i].raw_data_pos);
            WRITE(e->debug_sections.data[i].raw_data.data, e->debug_sections.data[i].raw_data.length);
        }

        TB_ModuleSection* sections = &m->text;
        assert(sections + 3 == &m->tls);

        // bucket section patches
        DynArray(COFF_ImageReloc) relocs[4] = { 0 };
        FOREACH_N(i, 0, m->max_threads) {
            dyn_array_for(j, m->thread_info[i].symbol_patches) {
                TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];
                TB_FunctionOutput* out_f = p->source->output;

                size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;
                size_t symbol_id = p->target->symbol_id;

                if (p->target->tag == TB_SYMBOL_FUNCTION) {
                    tb_todo();
                } else if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                    assert(symbol_id != 0);
                    COFF_ImageReloc r = {
                        .Type = IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = symbol_id,
                        .VirtualAddress = actual_pos
                    };
                    dyn_array_put(relocs, r);
                } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
                    assert(symbol_id != 0);
                    COFF_ImageReloc r = {
                        .Type = ((TB_Global*) p->target)->parent->is_tls ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = symbol_id,
                        .VirtualAddress = actual_pos
                    };
                    dyn_array_put(relocs, r);
                } else {
                    tb_todo();
                }
            }
        }

        // write all patches
        FOREACH_N(i, 0, 4) {
            assert(e->write_pos == sections[S_TEXT].pointer_to_reloc);

            size_t reloc_count = dyn_array_length(relocs[i]);
            memcpy(&output[e->write_pos], &relocs[i][0], reloc_count * sizeof(COFF_ImageReloc));
            e->write_pos += reloc_count * sizeof(COFF_ImageReloc);

            dyn_array_destroy(relocs[i]);
        }

        // write TEXT patches
        {
            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .cap = num_of_relocs[S_TEXT],
                .elems = (COFF_ImageReloc*) &output[sections[S_TEXT].pointer_to_reloc]
            };

            assert(e->write_pos == sections[S_TEXT].pointer_to_reloc);
            FOREACH_N(i, 0, m->max_threads) {
                dyn_array_for(j, m->thread_info[i].symbol_patches) {
                    TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;

                    size_t symbol_id = p->target->symbol_id;

                    if (p->target->tag == TB_SYMBOL_FUNCTION) {
                        // empty
                    } else if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                        assert(symbol_id != 0);
                        COFF_ImageReloc r = {
                            .Type = IMAGE_REL_AMD64_REL32,
                            .SymbolTableIndex = symbol_id,
                            .VirtualAddress = actual_pos
                        };
                        TB_FIXED_ARRAY_APPEND(relocs, r);
                    } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
                        assert(symbol_id != 0);
                        COFF_ImageReloc r = {
                            .Type = ((TB_Global*) p->target)->parent->is_tls ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                            .SymbolTableIndex = symbol_id,
                            .VirtualAddress = actual_pos
                        };
                        TB_FIXED_ARRAY_APPEND(relocs, r);
                    } else {
                        tb_todo();
                    }
                }
            }

            assert(relocs.count == relocs.cap);
            e->write_pos += relocs.count * sizeof(COFF_ImageReloc);
        }

        // write DATA patches
        {
            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .cap = num_of_relocs[S_DATA],
                .elems = (COFF_ImageReloc*) &output[sections[S_DATA].pointer_to_reloc]
            };

            assert(e->write_pos == sections[S_DATA].pointer_to_reloc);
            /*FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    TB_Initializer* init = g->init;

                    FOREACH_N(k, 0, init->obj_count) {
                        size_t actual_pos = g->pos + init->objects[k].offset;

                        if (init->objects[k].type == TB_INIT_OBJ_RELOC) {
                            const TB_Symbol* s = init->objects[k].reloc;

                            COFF_ImageReloc r = {
                                .Type = IMAGE_REL_AMD64_ADDR64,
                                .SymbolTableIndex = s->symbol_id,
                                .VirtualAddress = actual_pos
                            };
                            TB_FIXED_ARRAY_APPEND(relocs, r);
                        }
                    }
                }
            }*/

            tb_todo();
            assert(relocs.count == relocs.cap);
            e->write_pos += relocs.count * sizeof(COFF_ImageReloc);
        }

        // write PDATA patches
        {
            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .cap = num_of_relocs[S_PDATA] + (num_of_relocs[S_PDATA] >= 0xFFFF ? 1 : 0),
                .elems = (COFF_ImageReloc*) &output[sections[S_PDATA].pointer_to_reloc]
            };

            if (num_of_relocs[S_PDATA] >= 0xFFFF) {
                // because we have more than 65535 relocations the first relocation
                // stores the 32bit number of relocations in the VirtualAddress field
                COFF_ImageReloc r = {
                    .VirtualAddress = num_of_relocs[S_PDATA]
                };
                TB_FIXED_ARRAY_APPEND(relocs, r);
            }

            size_t j = 0;
            assert(e->write_pos == sections[S_PDATA].pointer_to_reloc);
            TB_FOR_FUNCTIONS(f, m) {
                TB_FunctionOutput* out_f = f->output;
                if (!out_f) continue;

                COFF_ImageReloc r = {
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 0, // text section
                    .VirtualAddress = (j * 12)
                };
                TB_FIXED_ARRAY_APPEND(relocs, r);

                r = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 0, // text section
                    .VirtualAddress = (j * 12) + 4
                };
                TB_FIXED_ARRAY_APPEND(relocs, r);

                r = (COFF_ImageReloc){
                    .Type = IMAGE_REL_AMD64_ADDR32NB,
                    .SymbolTableIndex = 8, // xdata section
                    .VirtualAddress = (j * 12) + 8
                };
                TB_FIXED_ARRAY_APPEND(relocs, r);
                j += 1;
            }

            assert(relocs.count == relocs.cap);
            e->write_pos += relocs.count * sizeof(COFF_ImageReloc);
        }

        if (e->debug_sections.length > 0) {
            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .elems = (COFF_ImageReloc*) &output[e->write_pos]
            };

            FOREACH_N(i, 0, e->debug_sections.length) {
                relocs.cap += e->debug_sections.data[i].relocation_count;
            }
            assert(e->write_pos == e->debug_section_headers[0].pointer_to_reloc);

            FOREACH_N(i, 0, e->debug_sections.length) {
                FOREACH_N(j, 0, e->debug_sections.data[i].relocation_count) {
                    TB_ObjectReloc* in_reloc = &e->debug_sections.data[i].relocations[j];

                    COFF_ImageReloc r = {
                        .SymbolTableIndex = in_reloc->symbol_index,
                        .VirtualAddress = in_reloc->virtual_address
                    };

                    switch (in_reloc->type) {
                        case TB_OBJECT_RELOC_SECREL:  r.Type = IMAGE_REL_AMD64_SECREL; break;
                        case TB_OBJECT_RELOC_SECTION: r.Type = IMAGE_REL_AMD64_SECTION; break;
                        default: tb_todo();
                    }
                    TB_FIXED_ARRAY_APPEND(relocs, r);
                }
            }

            assert(relocs.count == relocs.cap);
            e->write_pos += relocs.count * sizeof(COFF_ImageReloc);
        }

        // Emit section symbols
        {
            // COFF_AuxSectionSymbol is the same size as COFF_Symbol
            assert(e->write_pos == header.symbol_table);

            size_t count = 0, capacity = header.symbol_count;
            COFF_SymbolUnion* symbols = (COFF_SymbolUnion*) &output[e->write_pos];
            (void)capacity;

            count = append_section_sym(symbols, count, &sections[S_TEXT],  ".text",  IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &sections[S_RDATA], ".rdata", IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &sections[S_DATA],  ".data",  IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &sections[S_PDATA], ".pdata", IMAGE_SYM_CLASS_STATIC);
            count = append_section_sym(symbols, count, &sections[S_XDATA], ".xdata", IMAGE_SYM_CLASS_STATIC);

            m->rdata.section_num = 2;
            m->data.section_num = 3;

            if (m->tls.total_size) {
                m->tls.section_num = (count / 2) + 1;
                count = append_section_sym(symbols, count, &sections[S_TLS], ".tls$", IMAGE_SYM_CLASS_STATIC);
            }

            FOREACH_N(i, 0, e->debug_sections.length) {
                COFF_Symbol sym = {
                    .section_number = (count / 2) + 1,
                    .storage_class = IMAGE_SYM_CLASS_STATIC,
                    .aux_symbols_count = 1
                };

                TB_Slice name = e->debug_sections.data[i].name;
                if (name.length < 8) {
                    strncpy((char*) sym.short_name, (const char*) name.data, name.length);
                } else {
                    memcpy((char*) sym.short_name, (const char*) name.data, 8);
                }

                symbols[count++].s = sym;
                symbols[count++].a = (COFF_AuxSectionSymbol){
                    .length      = e->debug_section_headers[i].raw_data_size,
                    .reloc_count = e->debug_section_headers[i].num_reloc,
                    .number      = sym.section_number
                };
            }

            TB_FOR_FUNCTIONS(f, m) {
                TB_FunctionOutput* out_f = f->output;
                if (out_f == NULL) continue;

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
                    sym.long_name[1] = e->string_table_mark;

                    e->string_table[e->string_table_length++] = (char*)name;
                    e->string_table_mark += name_len + 1;
                } else {
                    memcpy(sym.short_name, name, name_len + 1);
                }

                assert(count < capacity);
                symbols[count++].s = sym;
            }

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_External, ext, m->thread_info[i].externals) {
                    COFF_Symbol sym = {
                        .value = 0,
                        .section_number = 0,
                        .storage_class = IMAGE_SYM_CLASS_EXTERNAL
                    };

                    size_t name_len = strlen(ext->super.name);
                    assert(name_len < UINT16_MAX);

                    if (name_len >= 8) {
                        sym.long_name[0] = 0; // this value is 0 for long names
                        sym.long_name[1] = e->string_table_mark;

                        e->string_table[e->string_table_length++] = ext->super.name;
                        e->string_table_mark += name_len + 1;
                    } else {
                        memcpy(sym.short_name, ext->super.name, name_len + 1);
                    }

                    assert(count < capacity);
                    symbols[count++].s = sym;
                }

                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    bool is_extern = g->linkage == TB_LINKAGE_PUBLIC;
                    COFF_Symbol sym = {
                        .value = g->pos,
                        .section_number = g->parent->section_num,
                        .storage_class = is_extern ? IMAGE_SYM_CLASS_EXTERNAL : IMAGE_SYM_CLASS_STATIC
                    };

                    size_t name_len = strlen(g->super.name);
                    assert(name_len < UINT16_MAX);

                    if (name_len >= 8) {
                        sym.long_name[0] = 0; // this value is 0 for long names
                        sym.long_name[1] = e->string_table_mark;

                        e->string_table[e->string_table_length++] = g->super.name;
                        e->string_table_mark += name_len + 1;
                    } else {
                        memcpy(sym.short_name, g->super.name, name_len + 1);
                    }

                    assert(count < capacity);
                    symbols[count++].s = sym;
                }
            }

            assert(count == capacity);
            e->write_pos += count * sizeof(COFF_Symbol);
        }

        // Emit string table
        {
            assert(e->write_pos == e->string_table_pos);

            char *start = (char*) &output[e->write_pos], *buffer = start;
            memcpy(buffer, &e->string_table_mark, sizeof(uint32_t));
            buffer += 4;

            FOREACH_N(i, 0, e->string_table_length) {
                const char* s = e->string_table[i];
                size_t l = strlen(s) + 1;

                memcpy(buffer, s, l);
                buffer += l;
            }

            assert((buffer - start) == e->string_table_mark);
            e->write_pos += e->string_table_mark;
        }
    }

    // TODO(NeGate): we have a lot of shit being freed... maybe
    // we wanna think of smarter allocation schemes
    tb_platform_heap_free(e->debug_section_headers);
    tb_platform_heap_free(e->string_table);
    tb_platform_heap_free(xdata.data);
    tb_platform_heap_free(e);
    return (TB_Exports){ .count = 1, .files = { { output_size, output } } };
}
#endif
