#include "tb_internal.h"

// Generate forward declarations
#define X(name) \
TB_Exports tb_ ## name ## _write_output(TB_Module* restrict m, const IDebugFormat* dbg);
#include "objects/export_formats.h"

static const IDebugFormat* find_debug_format(TB_DebugFormat debug_fmt) {
    switch (debug_fmt) {
        //case TB_DEBUGFMT_DWARF: return &tb__dwarf_debug_format;
        case TB_DEBUGFMT_CODEVIEW: return &tb__codeview_debug_format;
        default: return NULL;
    }
}

TB_API TB_Exports tb_exporter_write_output(TB_Module* m, TB_OutputFlavor flavor, TB_DebugFormat debug_fmt) {
    typedef TB_Exports ExporterFn(TB_Module* restrict m, const IDebugFormat* dbg);

    // map target systems to exporters (maybe we wanna decouple this later)
    static ExporterFn* const fn[TB_FLAVOR_EXECUTABLE + 1][TB_SYSTEM_MAX] = {
        [TB_FLAVOR_OBJECT] = {
            [TB_SYSTEM_WINDOWS] = tb_coff_write_output,
            [TB_SYSTEM_MACOS]   = tb_macho_write_output,
            [TB_SYSTEM_LINUX]   = tb_elf64obj_write_output,
            [TB_SYSTEM_WEB]     = tb_wasm_write_output,
        }
    };

    assert(fn[flavor][m->target_system] != NULL && "TODO");
    return fn[flavor][m->target_system](m, find_debug_format(debug_fmt));
}

TB_API bool tb_exporter_write_files(TB_Module* m, TB_OutputFlavor flavor, TB_DebugFormat debug_fmt, size_t path_count, const char* paths[]) {
    TB_Exports exports = tb_exporter_write_output(m, flavor, debug_fmt);
    if (exports.count > path_count) {
        fprintf(stderr, "tb_exporter_write_files: Not enough filepaths for the exports (provided %zu, needed %zu)\n", path_count, exports.count);
        tb_exporter_free(exports);
        return false;
    }

    bool success = true;
    FOREACH_N(i, 0, exports.count) {
        FILE* file = fopen(paths[i], "wb");
        if (file == NULL) {
            fprintf(stderr, "tb_exporter_write_files: Could not open file for writing! %s", paths[i]);
            tb_platform_heap_free(exports.files[i].data);
            success = false;
            continue;
        }

        fwrite(exports.files[i].data, 1, exports.files[i].length, file);
        fclose(file);

        tb_platform_heap_free(exports.files[i].data);
    }

    return success;
}

TB_API bool tb_linker_export_files(TB_Linker* l, size_t path_count, const char* paths[]) {
    TB_Exports exports = tb_linker_export(l);
    if (exports.count > path_count) {
        fprintf(stderr, "tb_exporter_write_files: Not enough filepaths for the exports (provided %zu, needed %zu)\n", path_count, exports.count);
        tb_exporter_free(exports);
        return false;
    }

    bool success = true;
    FOREACH_N(i, 0, exports.count) {
        FILE* file = fopen(paths[i], "wb");
        if (file == NULL) {
            fprintf(stderr, "tb_exporter_write_files: Could not open file for writing! %s", paths[i]);
            tb_platform_heap_free(exports.files[i].data);
            success = false;
            continue;
        }

        fwrite(exports.files[i].data, 1, exports.files[i].length, file);
        fclose(file);

        tb_platform_heap_free(exports.files[i].data);
    }

    return success;
}

TB_API void tb_exporter_free(TB_Exports exports) {
    FOREACH_N(i, 0, exports.count) {
        tb_platform_heap_free(exports.files[i].data);
    }
}
