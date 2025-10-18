#include "tb.h"
#include "tb_internal.h"
#include "host.h"
#include <hashes.h>

#ifndef TB_NO_THREADS
static once_flag tb_global_init = ONCE_FLAG_INIT;
#else
static bool tb_global_init;
#endif

static uint32_t symbolhs_hash(const void* a) {
    const TB_Symbol *aa = a;

    if (aa->linkage == TB_LINKAGE_PRIVATE) {
        return ((uintptr_t) aa * 11400714819323198485llu) >> 32llu;
    }

    return tb__murmur3_32(aa->name, aa->name_length);
}

static bool symbolhs_cmp(const void* a, const void* b) {
    const TB_Symbol *aa = a, *bb = b;

    // private symbols don't compare based on names, you could have 100 private
    // symbols all named the same.
    if (aa->linkage == TB_LINKAGE_PRIVATE || bb->linkage == TB_LINKAGE_PRIVATE) {
        return aa == bb;
    }

    return aa->name_length == bb->name_length && memcmp(aa->name, bb->name, aa->name_length) == 0;
}

#define NBHS_FN(n) symbolhs_ ## n
#include <nbhs.h>

ICodeGen tb_codegen_families[TB_ARCH_MAX];

static bool has_divrem(TB_Module* m) {
    return m->target_arch != TB_ARCH_WASM32;
}

static void init_codegen_families(void) {
    #ifdef TB_HAS_X64
    extern ICodeGen tb__x64_codegen;
    tb_codegen_families[TB_ARCH_X86_64] = tb__x64_codegen;
    #endif

    #ifdef TB_HAS_AARCH64
    extern ICodeGen tb__aarch64_codegen;
    tb_codegen_families[TB_ARCH_AARCH64] = tb__aarch64_codegen;
    #endif

    #ifdef TB_HAS_MIPS
    extern ICodeGen tb__mips32_codegen;
    extern ICodeGen tb__mips64_codegen;
    tb_codegen_families[TB_ARCH_MIPS32] = tb__mips32_codegen;
    tb_codegen_families[TB_ARCH_MIPS64] = tb__mips64_codegen;
    #endif

    #ifdef TB_HAS_WASM
    extern ICodeGen tb__wasm32_codegen;
    tb_codegen_families[TB_ARCH_WASM32] = tb__wasm32_codegen;
    #endif

    // allows me to initialize all the fun tables
    FOR_N(i, 0, TB_ARCH_MAX) {
        if (tb_codegen_families[i].global_init) {
            CUIK_TIMED_BLOCK("arch global init") {
                tb_codegen_families[i].global_init();
            }
        }
    }
}

ICodeGen* tb_codegen_info(TB_Module* m) { return &tb_codegen_families[m->target_arch]; }

TB_ThreadInfo* tb_thread_info(TB_Module* m) {
    static thread_local TB_ThreadInfo* chain;
    static thread_local mtx_t lock;
    static thread_local bool init;

    if (!init) {
        init = true;
        mtx_init(&lock, mtx_plain);
    }

    // there shouldn't really be contention here
    mtx_lock(&lock);

    // almost always refers to one TB_ThreadInfo, but
    // we can't assume the user has merely on TB_Module
    // per thread.
    TB_ThreadInfo* info = chain;
    while (info != NULL) {
        if (info->owner == m) {
            goto done;
        }
        info = info->next;
    }

    CUIK_TIMED_BLOCK("alloc thread info") {
        info = cuik_malloc(sizeof(TB_ThreadInfo));
        *info = (TB_ThreadInfo){ .owner = m, .chain = &chain, .lock = &lock };

        // allocate memory for it
        tb_arena_create(&info->perm_arena, "IR");
        tb_arena_create(&info->tmp_arena, "Tmp");

        // thread local so it doesn't need to synchronize
        info->next = chain;
        if (chain != NULL) {
            chain->prev = info;
        }
        chain = info;

        // link to the TB_Module* (we need to this to free later)
        TB_ThreadInfo* old_top;
        do {
            old_top = atomic_load(&m->first_info_in_module);
            info->next_in_module = old_top;
        } while (!atomic_compare_exchange_strong(&m->first_info_in_module, &old_top, info));
    }

    done:
    mtx_unlock(&lock);
    return info;
}

void tb_module_use_cc_gc(TB_Module* m, TB_Symbol* phase_control_sym, TB_Symbol* lvb_trap_fn) {
    m->ccgc.phase_control = phase_control_sym;
    m->ccgc.lvb_trap      = lvb_trap_fn;
}

TB_DataType tb_data_type_ptr_int(TB_Module* m) {
    switch (m->codegen->pointer_size) {
        case 16: return TB_TYPE_I16;
        case 32: return TB_TYPE_I32;
        case 64: return TB_TYPE_I64;
        default: tb_todo();
    }
}

int tb_data_type_bit_size(TB_Module* m, uint8_t type) {
    static const int arr[16] = {
        [TB_TAG_BOOL] = 1,
        [TB_TAG_I8]   = 8,
        [TB_TAG_I16]  = 16,
        [TB_TAG_I32]  = 32,
        [TB_TAG_I64]  = 64,
        [TB_TAG_F32]  = 32,
        [TB_TAG_F64]  = 64,
        [TB_TAG_V64]  = 64,
        [TB_TAG_V128] = 128,
        [TB_TAG_V256] = 256,
        [TB_TAG_V512] = 512,
    };

    if (type == TB_TAG_PTR) {
        TB_ASSERT(m);
        return m->codegen->pointer_size;
    }

    TB_ASSERT(type < 16);
    TB_ASSERT_MSG(arr[type], "this type has no size");
    return arr[type];
}

int tb_data_type_byte_size(TB_Module* m, uint8_t type) {
    int s = tb_data_type_bit_size(m, type);
    return (s + m->codegen->minimum_addressable_size - 1) / m->codegen->minimum_addressable_size;
}

// we don't modify these strings
char* tb__arena_strdup(TB_Module* m, ptrdiff_t len, const char* src) {
    if (len < 0) len = src ? strlen(src) : 0;
    if (len == 0) return (char*) "";

    char* newstr = tb_arena_alloc(get_permanent_arena(m), len + 1);
    memcpy(newstr, src, len);
    newstr[len] = 0;
    return newstr;
}

int tb_features_parse(TB_FeatureSet* out, TB_Module* m, const char* str) {
    *out = (TB_FeatureSet) { 0 };

    switch (m->target_arch) {
        #ifdef TB_HAS_X64
        case TB_ARCH_X86_64:
        if (TB_X86_FeatureSet__parse(&out->x86, str)) {
            return 1;
        }
        break;
        #endif

        default:
        break;
    }

    return 0;
}

TB_Module* tb_module_create_for_host(bool is_jit) {
    #if defined(TB_HOST_X86_64)
    TB_Arch arch = TB_ARCH_X86_64;
    #else
    TB_Arch arch = TB_ARCH_UNKNOWN;
    tb_panic("tb_module_create_for_host: cannot detect host platform");
    #endif

    #if defined(TB_HOST_WINDOWS)
    TB_System sys = TB_SYSTEM_WINDOWS;
    #elif defined(TB_HOST_OSX)
    TB_System sys = TB_SYSTEM_MACOS;
    #elif defined(TB_HOST_LINUX)
    TB_System sys = TB_SYSTEM_LINUX;
    #else
    tb_panic("tb_module_create_for_host: cannot detect host platform");
    #endif

    return tb_module_create(arch, sys, is_jit);
}

TB_ModuleSectionHandle tb_module_create_section(TB_Module* m, ptrdiff_t len, const char* name, TB_ModuleSectionFlags flags, TB_ComdatType comdat) {
    size_t i = dyn_array_length(m->sections);
    dyn_array_put_uninit(m->sections, 1);

    TB_ModuleSection* sec = &m->sections[i];
    *sec = (TB_ModuleSection){
        .name = tb__arena_strdup(m, len, name),
        .flags = flags,
        .comdat = { comdat },
    };
    return i;
}

TB_Module* tb_module_create(TB_Arch arch, TB_System sys, bool is_jit) {
    #ifndef TB_NO_THREADS
    call_once(&tb_global_init, init_codegen_families);
    #else
    if (!tb_global_init) {
        tb_global_init = true;
        init_codegen_families();
    }
    #endif

    TB_Module* m = cuik_malloc(sizeof(TB_Module));
    if (m == NULL) {
        fprintf(stderr, "tb_module_create: Out of memory!\n");
        return NULL;
    }
    memset(m, 0, sizeof(TB_Module));

    m->is_jit = is_jit;

    m->target_abi = (sys == TB_SYSTEM_WINDOWS) ? TB_ABI_WIN64 : TB_ABI_SYSTEMV;
    m->target_arch = arch;
    m->target_system = sys;
    m->codegen = tb_codegen_info(m);

    mtx_init(&m->lock, mtx_plain);
    m->symbols = nbhs_alloc(256);

    if (!is_jit) {
        bool win = sys == TB_SYSTEM_WINDOWS;
        tb_module_create_section(m, -1, ".text",                    TB_MODULE_SECTION_EXEC,                          TB_COMDAT_NONE);
        tb_module_create_section(m, -1, ".data",                    TB_MODULE_SECTION_WRITE,                         TB_COMDAT_NONE);
        tb_module_create_section(m, -1, win ? ".rdata" : ".rodata", 0,                                               TB_COMDAT_NONE);
        tb_module_create_section(m, -1, win ? ".tls$"  : ".tls",    TB_MODULE_SECTION_WRITE | TB_MODULE_SECTION_TLS, TB_COMDAT_NONE);
    }

    tb__lattice_init(m);
    return m;
}

void tb_module_enable_chkstk(TB_Module* m) {
    // AOT uses sections to know where to organize things in an executable file,
    // JIT does placement on the fly.
    if (m->target_system == TB_SYSTEM_WINDOWS) {
        if (!m->is_jit) {
            m->chkstk_extern = (TB_Symbol*) tb_extern_create(m, -1, "__chkstk", TB_EXTERNAL_SO_LOCAL);
        } else {
            #ifdef _WIN32
            extern void __chkstk(void);

            // fill it with whatever MSVC/Clang gave us
            m->chkstk_extern = (TB_Symbol*) tb_extern_create(m, -1, "__chkstk", TB_EXTERNAL_SO_LOCAL);
            m->chkstk_extern->address = __chkstk;
            #endif
        }
    }
}

TB_FunctionOutput* tb_codegen(TB_Function* f, TB_CodegenRA ra, TB_Worklist* ws, TB_Arena* code_arena, bool emit_asm) {
    if (code_arena == NULL) {
        code_arena = &f->arena;
    }

    TB_Module* m = f->super.module;
    f->worklist = ws;

    TB_FunctionOutput* func_out = tb_arena_alloc(code_arena, sizeof(TB_FunctionOutput));
    *func_out = (TB_FunctionOutput){ .parent = f, .section = f->section, .linkage = f->super.linkage };
    m->codegen->compile_function(f, ra, func_out, code_arena, emit_asm);
    atomic_fetch_add(&m->compiled_function_count, 1);

    f->output = func_out;
    f->worklist = NULL;

    return func_out;
}

void tb_output_print_asm(TB_FunctionOutput* out, FILE* fp) {
    if (fp == NULL) { fp = stdout; }

    TB_Assembly* a = tb_output_get_asm(out);
    for (; a; a = a->next) {
        fwrite(a->data, a->length, 1, fp);
    }
}

TB_Location* tb_output_get_locations(TB_FunctionOutput* out, size_t* out_count) {
    *out_count = dyn_array_length(out->locations);
    return &out->locations[0];
}

uint8_t* tb_output_get_code(TB_FunctionOutput* out, size_t* out_length) {
    *out_length = out->code_size;
    return out->code;
}

TB_Assembly* tb_output_get_asm(TB_FunctionOutput* out) {
    return out->asm_out;
}

TB_Arena* tb_function_get_arena(TB_Function* f, int i) {
    TB_ASSERT(i == 0 || i == 1);
    return i ? &f->tmp_arena : &f->arena;
}

void tb_module_destroy(TB_Module* m) {
    // free thread info's arena
    TB_ThreadInfo* info = atomic_load(&m->first_info_in_module);
    while (info != NULL) {
        TB_ThreadInfo* next = info->next_in_module;

        tb_arena_destroy(&info->tmp_arena);
        tb_arena_destroy(&info->perm_arena);

        // unlink, this needs to be synchronized in case another thread is
        // accessing while we're freeing.
        mtx_lock(info->lock);
        if (info->prev == NULL) {
            *info->chain = info->next;
        } else {
            info->prev->next = info->next;
        }
        mtx_unlock(info->lock);

        cuik_free(info);
        info = next;
    }

    nbhs_free(&m->symbols);
    nbhs_free(&m->lattice_elements);
    dyn_array_destroy(m->files);
    cuik_free(m);
}

TB_SourceFile* tb_get_source_file(TB_Module* m, ptrdiff_t len, const char* path) {
    mtx_lock(&m->lock);

    if (len <= 0) {
        len = path ? strlen(path) : 0;
    }

    NL_Slice key = {
        .length = len,
        .data = (const uint8_t*) path,
    };

    TB_SourceFile* file;
    ptrdiff_t search = nl_map_get(m->files, key);
    if (search < 0) {
        file = tb_arena_alloc(get_permanent_arena(m), sizeof(TB_SourceFile) + key.length + 1);
        file->id = -1;
        file->len = key.length;

        memcpy(file->path, key.data, key.length);
        file->path[key.length] = 0;
        key.data = file->path;

        nl_map_put(m->files, key, file);
    } else {
        file = m->files[search].v;
    }
    mtx_unlock(&m->lock);
    return file;
}

TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv cc, size_t param_count, const TB_PrototypeParam* params, size_t return_count, const TB_PrototypeParam* returns, bool has_varargs) {
    size_t size = sizeof(TB_FunctionPrototype) + ((param_count + return_count) * sizeof(TB_PrototypeParam));
    TB_FunctionPrototype* p = tb_arena_alloc(get_permanent_arena(m), size);

    p->call_conv = cc;
    p->param_count = param_count;
    p->has_varargs = has_varargs;
    if (param_count > 0) {
        memcpy(p->params, params, param_count * sizeof(TB_PrototypeParam));
    }
    if (return_count > 0 && !TB_IS_VOID_TYPE(returns[0].dt)) {
        memcpy(p->params + param_count, returns, return_count * sizeof(TB_PrototypeParam));
        p->return_count = return_count;
    } else {
        p->return_count = 0;
    }
    return p;
}

TB_Function* tb_function_create(TB_Module* m, ptrdiff_t len, const char* name, TB_Linkage linkage) {
    TB_Function* f = (TB_Function*) tb_symbol_alloc(m, TB_SYMBOL_FUNCTION, len, name, linkage, sizeof(TB_Function));
    return f;
}

void tb_symbol_set_name(TB_Symbol* s, ptrdiff_t len, const char* name) {
    s->name = tb__arena_strdup(s->module, len, name);
}

const char* tb_symbol_get_name(TB_Symbol* s) {
    return s->name;
}

void tb_function_set_attrs(TB_Function* f, TB_FunctionAttribs attrs) {
    f->attrs = attrs;
}

void tb_function_set_prototype(TB_Function* f, TB_ModuleSectionHandle section, TB_FunctionPrototype* p) {
    TB_ASSERT(f->prototype == NULL);
    size_t param_count = p->param_count;

    f->gvn_nodes = nl_hashset_alloc(32);

    tb_arena_create(&f->arena,     "FuncPerm");
    tb_arena_create(&f->tmp_arena, "FuncTmp");

    f->section = section;
    f->node_count = 0;
    TB_Node* root = f->root_node = tb_alloc_node_dyn(f, TB_ROOT, TB_TYPE_TUPLE, 2, 4, 0);

    f->param_count = param_count;
    f->params = tb_arena_alloc(&f->arena, (3 + param_count) * sizeof(TB_Node*));

    // fill in acceleration structure
    f->params[0] = tb__make_proj(f, TB_TYPE_CONTROL, f->root_node, 0);
    f->params[1] = tb__make_proj(f, TB_TYPE_MEMORY, f->root_node, 1);
    f->params[2] = tb__make_proj(f, TB_TYPE_PTR, f->root_node, 2);

    // create parameter projections
    TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(p);
    FOR_N(i, 0, param_count) {
        TB_DataType dt = p->params[i].dt;
        f->params[3+i] = tb__make_proj(f, dt, f->root_node, 3+i);
    }

    // create callgraph node
    TB_Node* callgraph = tb_alloc_node_dyn(f, TB_CALLGRAPH, TB_TYPE_VOID, 1, 8, sizeof(TB_NodeRegion));
    set_input(f, callgraph, root, 0);
    set_input(f, root, callgraph, 0);

    // create return node
    TB_Node* ret = tb_alloc_node(f, TB_RETURN, TB_TYPE_CONTROL, 3 + p->return_count, 0);
    set_input(f, root, ret, 1);

    // fill return crap
    {
        TB_Node* region = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 0, 4, sizeof(TB_NodeRegion));
        TB_Node* mem_phi = tb_alloc_node_dyn(f, TB_PHI, TB_TYPE_MEMORY, 1, 5, 0);
        set_input(f, mem_phi, region, 0);

        set_input(f, ret, region, 0);
        set_input(f, ret, mem_phi, 1);
        set_input(f, ret, f->params[2], 2);

        TB_PrototypeParam* returns = TB_PROTOTYPE_RETURNS(p);
        FOR_N(i, 0, p->return_count) {
            TB_Node* phi = tb_alloc_node_dyn(f, TB_PHI, returns[i].dt, 1, 5, 0);
            set_input(f, phi, region, 0);
            set_input(f, ret, phi, i + 3);
        }

        TB_NODE_SET_EXTRA(region, TB_NodeRegion, .mem_in = mem_phi, .tag = "ret");
    }

    f->prototype = p;
}

TB_FunctionPrototype* tb_function_get_prototype(TB_Function* f) {
    return f->prototype;
}

void tb_function_destroy(TB_Function* f) {
    f->super.tag = TB_SYMBOL_DEAD;
    nl_hashset_free(f->gvn_nodes);
    tb_arena_destroy(&f->tmp_arena);
    tb_arena_destroy(&f->arena);
}

void* tb_global_add_region(TB_Module* m, TB_Global* g, size_t offset, size_t size) {
    TB_ASSERT(offset == (uint32_t)offset);
    TB_ASSERT(size == (uint32_t)size);
    TB_ASSERT(g->obj_count < g->obj_capacity);

    void* ptr = cuik_malloc(size);
    g->objects[g->obj_count++] = (TB_InitObj) {
        .type = TB_INIT_OBJ_REGION, .offset = offset, .region = { .size = size, .ptr = ptr }
    };

    return ptr;
}

void tb_global_add_symbol_reloc(TB_Module* m, TB_Global* g, size_t offset, TB_Symbol* symbol) {
    TB_ASSERT(offset == (uint32_t) offset);
    TB_ASSERT(g->obj_count + 1 <= g->obj_capacity);
    TB_ASSERT(symbol != NULL);

    g->objects[g->obj_count++] = (TB_InitObj) { .type = TB_INIT_OBJ_RELOC, .offset = offset, .reloc = symbol };
}

TB_Symbol* tb_symbol_alloc(TB_Module* m, TB_SymbolTag tag, ptrdiff_t len, const char* name, TB_Linkage linkage, size_t size) {
    TB_ASSERT(tag != TB_SYMBOL_NONE);
    cuikperf_region_start("symbol_alloc", NULL);

    TB_ThreadInfo* info = tb_thread_info(m);
    if (len < 0) {
        len = strlen(name);
    }

    TB_Symbol* s = tb_arena_alloc(&info->perm_arena, size);
    s->tag = tag;
    s->linkage = linkage;
    s->name_length = len;
    s->name = tb__arena_strdup(m, len, name);
    s->module = m;
    if (size > sizeof(TB_Symbol)) {
        memset(&s[1], 0, size - sizeof(TB_Symbol));
    }

    TB_Symbol* old_s = symbolhs_intern(&m->symbols, s);
    if (s != old_s) {
        if (s->tag == TB_SYMBOL_EXTERNAL) {
            tb_arena_free(&info->perm_arena, s, size);
            s = old_s;
        } else if (old_s->tag == TB_SYMBOL_EXTERNAL && s->tag != TB_SYMBOL_EXTERNAL) {
            TB_Symbol* expected = NULL;
            TB_External* old_e = (TB_External*) old_s;
            if (!atomic_compare_exchange_strong(&old_e->resolved, &expected, s)) {
                tb_panic("tb: symbol collision");
            }
        } else {
            tb_panic("tb: symbol collision");
        }
    }

    cuikperf_region_end();
    return s;
}

TB_Symbol* tb_extern_create(TB_Module* m, ptrdiff_t len, const char* name, TB_ExternalType type) {
    TB_External* e = (TB_External*) tb_symbol_alloc(m, TB_SYMBOL_EXTERNAL, len, name, TB_LINKAGE_PUBLIC, sizeof(TB_External));
    e->type = type;
    return &e->super;
}

TB_Global* tb_global_create(TB_Module* m, ptrdiff_t len, const char* name, TB_DebugType* dbg_type, TB_Linkage linkage) {
    TB_Global* g = (TB_Global*) tb_symbol_alloc(m, TB_SYMBOL_GLOBAL, len, name, linkage, sizeof(TB_Global));
    g->dbg_type = dbg_type;
    return g;
}

TB_Function* tb_symbol_as_function(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_FUNCTION ? (TB_Function*) s : NULL;
}

TB_External* tb_symbol_as_external(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_EXTERNAL ? (TB_External*) s : NULL;
}

TB_Global* tb_symbol_as_global(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_GLOBAL ? (TB_Global*) s : NULL;
}

void tb_global_set_storage(TB_Module* m, TB_ModuleSectionHandle section, TB_Global* global, size_t size, size_t align, size_t max_objects) {
    TB_ASSERT(size > 0 && align > 0 && tb_is_power_of_two(align));
    global->parent = section;
    global->pos = 0;
    global->size = size;
    global->align = align;
    global->obj_count = 0;
    global->obj_capacity = max_objects;
    global->objects = TB_ARENA_ARR_ALLOC(get_permanent_arena(m), max_objects, TB_InitObj);
}

TB_Global* tb__small_data_intern(TB_Module* m, size_t len, const void* data) {
    assert(len <= 32);

    // copy into SmallConst
    SmallConst c = { .len = len };
    memcpy(c.data, data, c.len);

    mtx_lock(&m->lock);
    ptrdiff_t search = nl_map_get(m->global_interns, c);

    TB_Global* g;
    if (search >= 0) {
        g = m->global_interns[search].v;
    } else {
        g = tb_global_create(m, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
        g->super.ordinal = *((uint64_t*) &c.data);
        tb_global_set_storage(m, tb_module_get_rdata(m), g, len, len, 1);

        char* buffer = tb_global_add_region(m, g, 0, len);
        memcpy(buffer, data, len);
        nl_map_put(m->global_interns, c, g);
    }
    mtx_unlock(&m->lock);
    return g;
}

TB_Safepoint* tb_safepoint_get(TB_Function* f, uint32_t relative_ip) {
    size_t left = 0;
    size_t right = dyn_array_length(f->output->safepoints);

    uint32_t ip = relative_ip;
    TB_Safepoint** arr = f->output->safepoints;
    while (left < right) {
        size_t middle = (left + right) / 2;

        if (arr[middle]->ip == ip) return arr[middle];
        if (arr[middle]->ip < ip) left = middle + 1;
        else right = middle;
    }

    return NULL;
}

TB_ModuleSectionHandle tb_module_get_text(TB_Module* m)  { return 0; }
TB_ModuleSectionHandle tb_module_get_data(TB_Module* m)  { return 1; }
TB_ModuleSectionHandle tb_module_get_rdata(TB_Module* m) { return 2; }
TB_ModuleSectionHandle tb_module_get_tls(TB_Module* m)   { return 3; }

void tb_module_set_tls_index(TB_Module* m, ptrdiff_t len, const char* name) {
    if (atomic_flag_test_and_set(&m->is_tls_defined)) {
        m->tls_index_extern = (TB_Symbol*) tb_extern_create(m, len, name, TB_EXTERNAL_SO_LOCAL);
    }
}

void tb_symbol_bind_ptr(TB_Symbol* s, void* ptr) {
    s->address = ptr;
}

TB_ExternalType tb_extern_get_type(TB_External* e) {
    return e->type;
}

void tb_emit_symbol_patch(TB_FunctionOutput* func_out, TB_Symbol* target, size_t pos, TB_ObjectRelocType type) {
    TB_Module* m = func_out->parent->super.module;
    TB_SymbolPatch* p = tb_arena_alloc(get_permanent_arena(m), sizeof(TB_SymbolPatch));

    // function local, no need to synchronize
    *p = (TB_SymbolPatch){ .target = target, .pos = pos, .type = type };
    if (func_out->first_patch == NULL) {
        func_out->first_patch = func_out->last_patch = p;
    } else {
        func_out->last_patch->next = p;
        func_out->last_patch = p;
    }
    func_out->patch_count += 1;
}
