#include "../tb_internal.h"

// builtin primitives (the custom types start at 0x100)
//
// if the top bit is set, we're using a pointer to these
// types rather than a direct type.
typedef enum {
    SDG_TYPE_VOID,

    // builtin bools
    SDG_TYPE_BOOL8, SDG_TYPE_BOOL16, SDG_TYPE_BOOL32, SDG_TYPE_BOOL64,

    // builtin char
    SDG_TYPE_CHAR8, SDG_TYPE_CHAR16, SDG_TYPE_CHAR32,

    // builtin integers
    SDG_TYPE_INT8,  SDG_TYPE_UINT8,
    SDG_TYPE_INT16, SDG_TYPE_UINT16,
    SDG_TYPE_INT32, SDG_TYPE_UINT32,
    SDG_TYPE_INT64, SDG_TYPE_UINT64,

    // builtin floats
    SDG_TYPE_FLOAT, SDG_TYPE_DOUBLE, SDG_TYPE_LONG_DOUBLE,

    // NOTE(NeGate): if set, the type is now a pointer to the described type.
    SDG_TYPE_POINTER = 0x80,

    CUSTOM_TYPE_START = 0x100,
} SDG_TypeIndex;

// symbol table
typedef enum {
    // normal symbols
    SDG_SYMBOL_PROC,
    SDG_SYMBOL_DATA,

    // magic symbols
    SDG_SYMBOL_FILE,
    SDG_SYMBOL_MODULE,
} SDG_SymbolTag;

typedef struct {
    uint32_t tag;
    // number of bytes to skip to reach the first element in the body of the symbol.
    // this only applies for procedures because they have nested elements.
    uint8_t content_ptr;
    // 0 if doesn't have one
    uint32_t next;
} SDG_Symbol;

typedef struct {
    SDG_Symbol super;
    // type
    SDG_TypeIndex type;
    // in program's memory
    uint32_t rva, size;
    char name[];
} SDG_NormalSymbol;

typedef struct {
    SDG_Symbol super;
    // used to track changes
    uint32_t last_write;
    char name[];
} SDG_File;

typedef struct {
    SDG_Symbol super;
    char name[];
} SDG_Module;

static SDG_TypeIndex sdg_get_type_from_dt(TB_DataType dt) {
    // assert(dt.width == 0 && "TODO: implement vector types in CodeView output");
    switch (dt.type) {
        case TB_INT: {
            if (dt.data <= 0)  return SDG_TYPE_VOID;
            if (dt.data <= 1)  return SDG_TYPE_BOOL8;
            if (dt.data <= 8)  return SDG_TYPE_UINT8;
            if (dt.data <= 16) return SDG_TYPE_UINT16;
            if (dt.data <= 32) return SDG_TYPE_UINT32;
            if (dt.data <= 64) return SDG_TYPE_UINT64;
            return SDG_TYPE_VOID;
        }
        case TB_FLOAT: {
            if (dt.data == TB_FLT_32) return SDG_TYPE_FLOAT;
            if (dt.data == TB_FLT_64) return SDG_TYPE_DOUBLE;

            assert(0 && "Unknown float type");
        }
        case TB_PTR: {
            return SDG_TYPE_POINTER | SDG_TYPE_INT8;
        }
        default: return tb_assert(0, "todo: missing type in CodeView output");
    }
}

static bool sdg_supported_target(TB_Module* m)        { return true; }
static int sdg_number_of_debug_sections(TB_Module* m) { return 1; }

// there's quite a few places that mark the next field for symbols
#define MARK_NEXT(patch_pos) (((SDG_Symbol*) tb_out_get(&symtab, mod_length_patch))->next = symtab.count)
static TB_SectionGroup sdg_generate_debug_info(TB_Module* m, TB_TemporaryStorage* tls) {
    TB_ObjectSection* sections = tb_platform_heap_alloc(1 * sizeof(TB_ObjectSection));
    sections[0] = (TB_ObjectSection){ gimme_cstr_as_slice(".sdg$S") };

    TB_Emitter symtab = { 0 };

    // we only store one module so we never fill the next
    SDG_Module mod = { { SDG_SYMBOL_MODULE } };
    size_t mod_length_patch = tb_outs(&symtab, sizeof(mod), &mod);
    tb_outstr_nul(&symtab, "fallback.o");

    // emit file table into symbol table.
    // skip the NULL file entry
    size_t file_count = dyn_array_length(m->files);
    FOREACH_N(i, 0, file_count) {
        size_t len = m->files[i].length;
        const char* data = strlen(m->files[i].data);

        SDG_File file = { SDG_SYMBOL_FILE };
        size_t field_length_patch = tb_outs(&symtab, sizeof(file), &file);

        tb_outstr_nul(&symtab, m->files[i].data);
        MARK_NEXT(file_length_patch);
    }

    // functions
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (!out_f) continue;

        SDG_NormalSymbol sym = { SDG_SYMBOL_PROC };
        size_t sym_next_patch = tb_outs(&symtab, sizeof(sym), &sym);
        tb_outstr_nul(&symtab, f->super.name);

        // fill RVA
        TB_ObjectReloc r = {
            TB_OBJECT_RELOC_ADDR32NB, f->super.symbol_id,
            sym_next_patch + offsetof(SDG_NormalSymbol, rva)
        };
        add_reloc(&sections[0], &r, );

        MARK_NEXT(sym_next_patch);
    }

    /*TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (!out_f) continue;
    }*/

    MARK_NEXT(mod_length_patch);

    sections[0].raw_data = (TB_Slice){ symtab.count, symtab.data };
    return (TB_SectionGroup) { 1, sections };
}

IDebugFormat tb__sdg_debug_format = {
    "SDG",
    sdg_supported_target,
    sdg_number_of_debug_sections,
    sdg_generate_debug_info
};
