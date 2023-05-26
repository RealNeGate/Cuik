#include "tb_internal.h"

TB_Exports tb_coff_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_macho_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_elf64obj_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_wasm_write_output(TB_Module* restrict m, const IDebugFormat* dbg);

static const IDebugFormat* find_debug_format(TB_DebugFormat debug_fmt) {
    switch (debug_fmt) {
        // case TB_DEBUGFMT_DWARF: return &tb__dwarf_debug_format;
        // case TB_DEBUGFMT_CODEVIEW: return &tb__codeview_debug_format;
        default: return NULL;
    }
}

TB_API TB_Exports tb_module_object_export(TB_Module* m, TB_DebugFormat debug_fmt){
    typedef TB_Exports ExporterFn(TB_Module* restrict m, const IDebugFormat* dbg);

    // map target systems to exporters (maybe we wanna decouple this later)
    static ExporterFn* const fn[TB_SYSTEM_MAX] = {
        [TB_SYSTEM_WINDOWS] = tb_coff_write_output,
        // [TB_SYSTEM_MACOS]   = tb_macho_write_output,
        // [TB_SYSTEM_LINUX]   = tb_elf64obj_write_output,
        // [TB_SYSTEM_WEB]     = tb_wasm_write_output,
    };

    assert(fn[m->target_system] != NULL && "TODO");
    TB_Exports e;
    CUIK_TIMED_BLOCK("export") {
        e = fn[m->target_system](m, find_debug_format(debug_fmt));
    }
    return e;
}

TB_API void tb_exporter_free(TB_Exports exports) {
    FOREACH_N(i, 0, exports.count) {
        tb_platform_heap_free(exports.files[i].data);
    }
}

static void layout_section(TB_ModuleSection* restrict section) {
    if (section->laid_out) {
        return;
    }

    CUIK_TIMED_BLOCK_ARGS("layout section", section->name) {
        uint64_t offset = 0;
        dyn_array_for(i, section->globals) {
            TB_Global* g = section->globals[i];

            g->pos = offset;
            offset = align_up(offset + g->size, g->align);
        }
        section->total_size = offset;
        section->laid_out = true;
    }
}

static int compare_symbols(const void* a, const void* b) {
    const TB_Symbol* sym_a = *(const TB_Symbol**) a;
    const TB_Symbol* sym_b = *(const TB_Symbol**) b;

    return sym_a->ordinal - sym_b->ordinal;
}

static int compare_functions(const void* a, const void* b) {
    const TB_Function* sym_a = *(const TB_Function**) a;
    const TB_Function* sym_b = *(const TB_Function**) b;

    // comdat functions are pushed to the end
    int diff = sym_a->comdat.type - sym_b->comdat.type;
    if (diff) return diff;

    return sym_a->super.ordinal - sym_b->super.ordinal;
}

TB_API void tb_module_layout_sections(TB_Module* m) {
    // text section is special because it holds code
    TB_Symbol** array_form = NULL;
    FOREACH_N(tag, 0, TB_SYMBOL_MAX) {
        if (m->symbol_count[tag] < 1) continue;

        CUIK_TIMED_BLOCK("sort") {
            size_t count = m->symbol_count[tag];
            array_form = tb_platform_heap_realloc(array_form, count * sizeof(TB_Symbol*));

            size_t i = 0;
            CUIK_TIMED_BLOCK("convert to array") {
                for (TB_Symbol* s = m->first_symbol_of_tag[tag]; s != NULL; s = s->next) {
                    array_form[i++] = s;
                }
                assert(i == m->symbol_count[tag]);
            }

            // functions have special rules on ordering but other than that, ordinals go brr
            CUIK_TIMED_BLOCK("sort by ordinal") qsort(
                array_form, i, sizeof(TB_Symbol*),
                tag == TB_SYMBOL_FUNCTION ? compare_functions : compare_symbols
            );

            CUIK_TIMED_BLOCK("convert back to list") {
                m->first_symbol_of_tag[tag] = array_form[0];
                m->last_symbol_of_tag[tag] = array_form[i - 1];

                FOREACH_N(j, 1, i) {
                    array_form[j-1]->next = array_form[j];
                }
                array_form[i-1]->next = NULL;
            }
        }
    }
    tb_platform_heap_free(array_form);

    CUIK_TIMED_BLOCK("layout code") {
        size_t offset = 0, comdat = 0, comdat_count = 0, comdat_relocs = 0;
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;
            if (out_f != NULL) {
                out_f->code_pos = offset;
                offset += out_f->code_size;

                if (f->comdat.type != TB_COMDAT_NONE) {
                    comdat += out_f->code_size;
                    comdat_relocs += f->patch_count;
                    comdat_count++;
                }
            }
        }

        m->comdat_function_count = comdat_count;

        m->text.total_size = offset;
        m->text.total_comdat = comdat;
        m->text.total_comdat_relocs = comdat_relocs;
        m->text.laid_out = true;
    }

    layout_section(&m->data);
    layout_section(&m->rdata);
    layout_section(&m->tls);
}

size_t tb_helper_write_section(TB_Module* m, size_t write_pos, TB_ModuleSection* section, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);
    uint8_t* data = &output[pos];

    switch (section->kind) {
        case TB_MODULE_SECTION_TEXT:
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;

            if (out_f != NULL) {
                memcpy(data + out_f->code_pos, out_f->code, out_f->code_size);
            }
        }
        break;

        case TB_MODULE_SECTION_DATA:
        case TB_MODULE_SECTION_TLS:
        dyn_array_for(i, section->globals) {
            TB_Global* restrict g = section->globals[i];

            memset(&data[g->pos], 0, g->size);
            FOREACH_N(k, 0, g->obj_count) {
                if (g->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
                }
            }
        }
        break;

        default:
        tb_todo();
        break;
    }

    return write_pos + section->total_size;
}

size_t tb__layout_relocations(TB_Module* m, DynArray(TB_ModuleSection*) sections, const ICodeGen* restrict code_gen, size_t output_size, size_t reloc_size, bool sizing) {
    // calculate relocation layout
    dyn_array_for(i, sections) {
        size_t reloc_count = 0;
        switch (sections[i]->kind) {
            case TB_MODULE_SECTION_TEXT: {
                // emit_call_patches will also give us the reloc_count
                size_t locals = code_gen->emit_call_patches(m);
                reloc_count = sections[i]->reloc_count;
                reloc_count -= locals;
                reloc_count -= sections[i]->total_comdat_relocs;
                break;
            }

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

        if (sizing) {
            output_size += reloc_count * reloc_size;
            output_size += sections[i]->total_comdat_relocs * reloc_size;
        } else {
            sections[i]->reloc_pos = output_size;
            output_size += reloc_count * reloc_size;

            if (sections[i]->total_comdat_relocs) {
                TB_FOR_FUNCTIONS(f, m) if (f->super.name && f->output) {
                    f->patch_pos = output_size;

                    for (TB_SymbolPatch* p = f->last_patch; p; p = p->prev) {
                        if (!p->internal && f->comdat.type != TB_COMDAT_NONE) output_size += reloc_size;
                    }
                }
            }
        }
    }

    return output_size;
}
