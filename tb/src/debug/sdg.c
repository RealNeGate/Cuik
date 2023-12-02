#include "../tb_internal.h"

// builtin primitives (the custom types start at 0x100)
//
// if the top bit is set, we're using a pointer to these
// types rather than a direct type.
typedef enum {
    SDG_PRIM_VOID,

    // builtin bools
    SDG_PRIM_BOOL8, SDG_PRIM_BOOL16, SDG_PRIM_BOOL32, SDG_PRIM_BOOL64,

    // builtin char
    SDG_PRIM_CHAR8, SDG_PRIM_CHAR16, SDG_PRIM_CHAR32,

    // builtin integers
    SDG_PRIM_INT8,  SDG_PRIM_UINT8,
    SDG_PRIM_INT16, SDG_PRIM_UINT16,
    SDG_PRIM_INT32, SDG_PRIM_UINT32,
    SDG_PRIM_INT64, SDG_PRIM_UINT64,

    // builtin floats
    SDG_PRIM_FLOAT, SDG_PRIM_DOUBLE, SDG_PRIM_LONG_DOUBLE,

    // NOTE(NeGate): if set, the type is now a pointer to the described type.
    SDG_PRIM_POINTER = 0x80,

    CUSTOM_TYPE_START = 0x100,
} SDG_TypeIndex;

typedef struct {
    enum {
        SDG_TYPE_PTR,
        SDG_TYPE_FUNC,
    } tag;

    int arg_count;
    SDG_TypeIndex base;
    SDG_TypeIndex args[];
} SDG_Type;

// symbol table
typedef enum {
    // normal symbols
    SDG_SYMBOL_PROC,
    SDG_SYMBOL_GLOBAL,

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
    uint32_t type_table;
    uint32_t type_count;
    char name[];
} SDG_Module;

typedef struct {
    TB_Emitter e;
    size_t count;
} SDG_Types;

static SDG_TypeIndex sdg_get_type(SDG_Types* types, TB_DebugType* type) {
    if (type->type_id != 0) {
        return type->type_id;
    }

    switch (type->tag) {
        case TB_DEBUG_TYPE_VOID: return (type->type_id = SDG_PRIM_VOID);
        case TB_DEBUG_TYPE_BOOL: return (type->type_id = SDG_PRIM_BOOL8);

        case TB_DEBUG_TYPE_INT:
        case TB_DEBUG_TYPE_UINT: {
            bool is_signed = (type->tag == TB_DEBUG_TYPE_INT);

            if (type->int_bits <= 8)  return is_signed ? SDG_PRIM_INT8 : SDG_PRIM_UINT8;
            if (type->int_bits <= 16) return is_signed ? SDG_PRIM_INT16 : SDG_PRIM_UINT16;
            if (type->int_bits <= 32) return is_signed ? SDG_PRIM_INT32 : SDG_PRIM_UINT32;
            if (type->int_bits <= 64) return is_signed ? SDG_PRIM_INT64 : SDG_PRIM_UINT64;
            assert(0 && "Unsupported int type");
        }

        case TB_DEBUG_TYPE_FLOAT: {
            switch (type->float_fmt) {
                case TB_FLT_32: return (type->type_id = T_REAL32);
                case TB_FLT_64: return (type->type_id = T_REAL64);
                default: assert(0 && "Unknown float type");
            }
        }

        case TB_DEBUG_TYPE_FUNCTION: {
            __debugbreak();
            return (type->type_id = 0);
        }

        case TB_DEBUG_TYPE_POINTER: {
            SDG_TypeIndex ty = 0x100 + types->count++;

            SDG_Type t = {
                .tag = SDG_TYPE_PTR,
                .base = sdg_get_type(types, type->ptr_to)
            };
            tb_outs(&types->e, sizeof(t), &t);

            return (type->type_id = ty);
        }

        default: tb_todo();
    }
}

static SDG_TypeIndex sdg_get_type_from_dt(TB_DataType dt) {
    // assert(dt.width == 0 && "TODO: implement vector types in CodeView output");
    switch (dt.type) {
        case TB_INT: {
            if (dt.data <= 0)  return SDG_PRIM_VOID;
            if (dt.data <= 1)  return SDG_PRIM_BOOL8;
            if (dt.data <= 8)  return SDG_PRIM_UINT8;
            if (dt.data <= 16) return SDG_PRIM_UINT16;
            if (dt.data <= 32) return SDG_PRIM_UINT32;
            if (dt.data <= 64) return SDG_PRIM_UINT64;
            return SDG_PRIM_VOID;
        }
        case TB_FLOAT: {
            if (dt.data == TB_FLT_32) return SDG_PRIM_FLOAT;
            if (dt.data == TB_FLT_64) return SDG_PRIM_DOUBLE;

            assert(0 && "Unknown float type");
        }
        case TB_PTR: {
            return SDG_PRIM_POINTER | SDG_PRIM_VOID;
        }
        default: return tb_assert(0, "todo: missing type in CodeView output");
    }
}

static bool sdg_supported_target(TB_Module* m)        { return true; }
static int sdg_number_of_debug_sections(TB_Module* m) { return 2; }

static size_t write_normie_sym(TB_Emitter* e, TB_ObjectSection* section, int tag, const char* name, size_t sym_id, size_t code_size) {
    SDG_NormalSymbol sym = { { tag }, .size = code_size };

    size_t sym_next_patch = tb_outs(e, sizeof(sym), &sym);
    tb_outs(e, strlen(name) + 1, name);

    // fill RVA
    section->relocations[section->relocation_count++] = (TB_ObjectReloc){
        TB_OBJECT_RELOC_ADDR32NB, sym_id,
        sym_next_patch + offsetof(SDG_NormalSymbol, rva)
    };
    return sym_next_patch;
}

// there's quite a few places that mark the next field for symbols
#define MARK_NEXT(patch_pos) (((SDG_Symbol*) tb_out_get(&symtab, patch_pos))->next = symtab.count)
static TB_SectionGroup sdg_generate_debug_info(TB_Module* m, TB_TemporaryStorage* tls) {
    TB_ObjectSection* sections = tb_platform_heap_alloc(1 * sizeof(TB_ObjectSection));
    sections[0] = (TB_ObjectSection){ gimme_cstr_as_slice(".sdg$S") };
    sections[1] = (TB_ObjectSection){ gimme_cstr_as_slice(".sdg$T") };

    size_t reloc_cap = m->symbol_count[TB_SYMBOL_GLOBAL] + m->compiled_function_count;
    sections[0].relocations = tb_platform_heap_alloc(reloc_cap * sizeof(TB_ObjectReloc));

    TB_Emitter symtab = { 0 };
    SDG_Types types = { 0 };

    // we only store one module so we never fill the next
    SDG_Module mod = { { SDG_SYMBOL_MODULE } };
    size_t type_table_patch = tb_outs(&symtab, sizeof(mod), &mod);

    tb_outs(&symtab, sizeof("fallback.o"), "fallback.o");

    // emit file table into symbol table.
    // skip the NULL file entry
    size_t file_count = nl_map__get_header(m->files)->count;
    nl_map_for_str(i, m->files) {
        size_t len = m->files[i].v->len;
        const uint8_t* data = m->files[i].v->path;

        SDG_File file = { { SDG_SYMBOL_FILE } };
        size_t file_length_patch = tb_outs(&symtab, sizeof(file), &file);

        tb_outs(&symtab, len, data);
        tb_out1b(&symtab, 0);

        MARK_NEXT(file_length_patch);
    }

    // functions
    dyn_array_for(i, m->sections) {
        DynArray(TB_FunctionOutput*) funcs = m->sections[i].funcs;
        dyn_array_for(j, funcs) {
            TB_FunctionOutput* out_f = funcs[j];
            const char* name = out_f->parent->super.name;
            size_t func_id = out_f->parent->super.symbol_id;

            SDG_TypeIndex ty = 0;
            {
                const TB_FunctionPrototype* proto = out_f->parent->prototype;
                ty = 0x100 + types.count++;

                SDG_Type t = { SDG_TYPE_FUNC, proto->param_count };
                if (proto->return_count == 1) {
                    const TB_PrototypeParam* ret = &proto->params[proto->param_count];
                    if (ret->debug_type) {
                        t.base = sdg_get_type(&types, ret->debug_type);
                    } else {
                        t.base = sdg_get_type_from_dt(ret->dt);
                    }
                } else {
                    t.base = SDG_PRIM_VOID;
                }
                tb_outs(&types.e, sizeof(t), &t);

                FOREACH_N(i, 0, proto->param_count) {
                    TB_DebugType* param_ty = proto->params[i].debug_type;
                    SDG_TypeIndex param = param_ty ? sdg_get_type(&types, param_ty) : sdg_get_type_from_dt(proto->params[i].dt);
                    tb_outs(&types.e, sizeof(param), &param);
                }
            }

            size_t sym_next_patch = write_normie_sym(&symtab, &sections[0], SDG_SYMBOL_PROC, name, func_id, out_f->code_size);
            MARK_NEXT(sym_next_patch);
        }

        DynArray(TB_Global*) globals = m->sections[i].globals;
        dyn_array_for(j, globals) {
            TB_Global* g = globals[j];
            if (g->super.name[0] == 0) {
                continue;
            }

            size_t sym_next_patch = write_normie_sym(&symtab, &sections[0], SDG_SYMBOL_GLOBAL, g->super.name, g->super.symbol_id, g->size);
            MARK_NEXT(sym_next_patch);
        }
    }

    // write type table patch (it'll just follow immediately after the symbols)
    ((SDG_Module*) &symtab.data[0])->type_table = symtab.count;
    ((SDG_Module*) &symtab.data[0])->type_count = types.count;

    sections[0].raw_data = (TB_Slice){ symtab.count, symtab.data };
    sections[1].raw_data = (TB_Slice){ types.e.count, types.e.data };
    return (TB_SectionGroup) { 2, sections };
}

IDebugFormat tb__sdg_debug_format = {
    "SDG",
    sdg_supported_target,
    sdg_number_of_debug_sections,
    sdg_generate_debug_info
};
