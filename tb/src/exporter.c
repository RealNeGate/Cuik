#include "tb_internal.h"

TB_Exports tb_coff_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_macho_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_elf64obj_write_output(TB_Module* restrict m, const IDebugFormat* dbg);
TB_Exports tb_wasm_write_output(TB_Module* restrict m, const IDebugFormat* dbg);

static const IDebugFormat* find_debug_format(TB_DebugFormat debug_fmt) {
    switch (debug_fmt) {
        //case TB_DEBUGFMT_DWARF: return &tb__dwarf_debug_format;
        case TB_DEBUGFMT_CODEVIEW: return &tb__codeview_debug_format;
        default: return NULL;
    }
}

TB_API TB_Exports tb_module_object_export(TB_Module* m, TB_DebugFormat debug_fmt){
    typedef TB_Exports ExporterFn(TB_Module* restrict m, const IDebugFormat* dbg);

    // map target systems to exporters (maybe we wanna decouple this later)
    static ExporterFn* const fn[TB_SYSTEM_MAX] = {
        [TB_SYSTEM_WINDOWS] = tb_coff_write_output,
        [TB_SYSTEM_MACOS]   = tb_macho_write_output,
        [TB_SYSTEM_LINUX]   = tb_elf64obj_write_output,
        [TB_SYSTEM_WEB]     = tb_wasm_write_output,
    };

    assert(fn[m->target_system] != NULL && "TODO");
    return fn[m->target_system](m, find_debug_format(debug_fmt));
}

TB_API void tb_exporter_free(TB_Exports exports) {
    FOREACH_N(i, 0, exports.count) {
        tb_platform_heap_free(exports.files[i].data);
    }
}

static void layout_section(TB_ModuleSection* restrict section) {
    if (!section->dirty) {
        return;
    }

    uint64_t offset = 0;
    dyn_array_for(i, section->globals) {
        TB_Global* g = section->globals[i];

        if (g->super.name) {
            g->pos = offset;
            offset = align_up(offset + g->size, g->align);
        }
    }
    section->total_size = offset;
    section->dirty = false;
}

TB_API void tb_module_layout_sections(TB_Module* m) {
    // text section is special because it holds code
    {
        size_t offset = 0;
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;
            if (out_f != NULL) {
                out_f->code_pos = offset;
                offset += out_f->code_size;
            }
        }

        m->text.total_size = offset;
        m->text.dirty = false;
    }

    layout_section(&m->data);
    layout_section(&m->rdata);
    layout_section(&m->tls);
}
