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
