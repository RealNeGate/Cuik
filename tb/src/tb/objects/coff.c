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

#define WRITE(data, length_) write_data(e, output, length_, data)
static void write_data(TB_ModuleExporter* e, uint8_t* output, size_t length, const void* data) {
    memcpy(output + e->write_pos, data, length);
    e->write_pos += length;
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

static size_t append_section_sym(COFF_SymbolUnion* symbols, size_t count, COFF_SectionHeader* section, const char* name, int sc) {
    symbols[count + 0].s = section_sym(name, (count / 2) + 1, sc);
    symbols[count + 1].a = section_aux_sym(section, (count / 2) + 1);
    return count + 2;
}

TB_API TB_Exports tb_coff_write_output(TB_Module* m, const IDebugFormat* dbg) {
    TB_ModuleExporter* e = tb_platform_heap_alloc(sizeof(TB_ModuleExporter));
    memset(e, 0, sizeof(TB_ModuleExporter));

    e->code_gen = tb__find_code_generator(m);
    e->string_table_mark = 4;

    const char* path = "fallback.obj";

    ////////////////////////////////
    // Generate .xdata section (unwind info)
    ////////////////////////////////
    TB_Emitter xdata = { 0 };
    {
        if (e->code_gen->emit_win64eh_unwind_info == NULL) {
            tb_panic("write_xdata_section: emit_win64eh_unwind_info is required.");
        }

        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;

            if (out_f != NULL) {
                out_f->unwind_info = xdata.count;
                e->code_gen->emit_win64eh_unwind_info(&xdata, out_f, out_f->prologue_epilogue_metadata, out_f->stack_usage);
            }
        }
    }

    int number_of_sections = 5
        + (m->tls_region_size ? 1 : 0)
        + (dbg != NULL ? dbg->number_of_debug_sections(m) : 0);

    // mark each with a unique id
    size_t function_sym_start = (number_of_sections * 2);
    size_t external_sym_start = function_sym_start + m->compiled_function_count;

    size_t text_section_size = tb_helper_get_text_section_layout(m, function_sym_start);
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
            TB_Initializer* init = g->init;
            FOREACH_N(k, 0, init->obj_count) {
                num_of_relocs[S_DATA] += (init->objects[k].type != TB_INIT_OBJ_REGION);
            }
        }
    }

    // COFF file header & section headers
    COFF_FileHeader header = {
        .num_sections = number_of_sections,
        .timestamp = time(NULL),
        .symbol_count = 0,
        .symbol_table = 0,
        .characteristics = IMAGE_FILE_LINE_NUMS_STRIPPED
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
            .raw_data_size = m->rdata_region_size
        },
        [S_DATA] = (COFF_SectionHeader){
            .name = { ".data" }, // .data
            .characteristics = COFF_CHARACTERISTICS_DATA,
            .raw_data_size = m->data_region_size
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
            .raw_data_size = m->tls_region_size,
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
            num_of_relocs[S_TEXT] += dyn_array_length(m->thread_info[i].const_patches);
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
    TB_FOR_FUNCTIONS(f, m) {
        size_t name_len = strlen(f->super.name);
        if (name_len >= 8) string_table_size += name_len + 1;
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_External, ext, m->thread_info[i].externals) {
            size_t name_len = strlen(ext->super.name);
            if (name_len >= 8) string_table_size += name_len + 1;
        }

        pool_for(TB_Global, g, m->thread_info[i].globals) {
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
        int count = (header.num_sections - e->debug_sections.length);
        WRITE(sections, count * sizeof(COFF_SectionHeader));
        WRITE(e->debug_section_headers, e->debug_sections.length * sizeof(COFF_SectionHeader));

        // TEXT section
        e->write_pos = tb_helper_write_text_section(e->write_pos, m, output, sections[S_TEXT].raw_data_pos);
        e->write_pos = tb_helper_write_rodata_section(e->write_pos, m, output, sections[S_RDATA].raw_data_pos);
        e->write_pos = tb_helper_write_data_section(e->write_pos, m, output, sections[S_DATA].raw_data_pos);

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
        {
            assert(e->write_pos == sections[S_TLS].raw_data_pos);
            uint8_t* tls = &output[e->write_pos];
            e->write_pos += m->tls_region_size;

            FOREACH_N(i, 0, m->max_threads) {
                pool_for(TB_Global, g, m->thread_info[i].globals) {
                    if (g->storage != TB_STORAGE_TLS) continue;

                    TB_Initializer* init = g->init;

                    // clear out space
                    memset(&tls[g->pos], 0, init->size);

                    FOREACH_N(k, 0, init->obj_count) {
                        const TB_InitObj* o = &init->objects[k];
                        if (o->type == TB_INIT_OBJ_REGION) {
                            memcpy(&tls[g->pos + o->offset], o->region.ptr, o->region.size);
                        }
                    }
                }
            }
        }

        // write DEBUG sections
        FOREACH_N(i, 0, e->debug_sections.length) {
            assert(e->write_pos == e->debug_section_headers[i].raw_data_pos);
            WRITE(e->debug_sections.data[i].raw_data.data, e->debug_sections.data[i].raw_data.length);
        }

        // write TEXT patches
        {
            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .cap = num_of_relocs[S_TEXT],
                .elems = (COFF_ImageReloc*) &output[sections[S_TEXT].pointer_to_reloc]
            };

            assert(e->write_pos == sections[S_TEXT].pointer_to_reloc);
            FOREACH_N(i, 0, m->max_threads) {
                dyn_array_for(j, m->thread_info[i].const_patches) {
                    TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;
                    COFF_ImageReloc r = {
                        .Type = IMAGE_REL_AMD64_REL32,
                        .SymbolTableIndex = 2, // rdata section
                        .VirtualAddress = actual_pos
                    };
                    TB_FIXED_ARRAY_APPEND(relocs, r);
                }

                dyn_array_for(j, m->thread_info[i].symbol_patches) {
                    TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos;

                    size_t symbol_id = p->target->symbol_id;
                    assert(symbol_id != 0);

                    if (p->target->tag == TB_SYMBOL_FUNCTION) {
                        // empty
                    } else if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                        COFF_ImageReloc r = {
                            .Type = IMAGE_REL_AMD64_REL32,
                            .SymbolTableIndex = symbol_id,
                            .VirtualAddress = actual_pos
                        };
                        TB_FIXED_ARRAY_APPEND(relocs, r);
                    } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
                        COFF_ImageReloc r = {
                            .Type = ((TB_Global*) p->target)->storage == TB_STORAGE_TLS ? IMAGE_REL_AMD64_SECREL : IMAGE_REL_AMD64_REL32,
                            .SymbolTableIndex = p->target->symbol_id,
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
            FOREACH_N(i, 0, m->max_threads) {
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
            }

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
            size_t capacity = 0;
            FOREACH_N(i, 0, e->debug_sections.length) {
                capacity += e->debug_sections.data[i].relocation_count;
            }

            TB_FIXED_ARRAY(COFF_ImageReloc) relocs = {
                .cap = capacity / sizeof(COFF_ImageReloc),
                .elems = (COFF_ImageReloc*) &output[e->write_pos]
            };
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

            assert(count == capacity);
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

            e->tls_section_num = (count / 2) + 1;
            if (m->tls_region_size) {
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
                        .section_number = g->storage == TB_STORAGE_TLS ? e->tls_section_num : 3, // data or tls section
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
