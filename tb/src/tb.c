#include "tb_internal.h"
#include "host.h"

thread_local Arena tb__arena;
thread_local Arena tb__arena2;

static thread_local uint8_t* tb_thread_storage;
static thread_local int tid;
static tb_atomic_int total_tid;

TB_Arena* tb_node_arena(void);
static void* alloc_from_node_arena(TB_Function* f, size_t necessary_size);

ICodeGen* tb__find_code_generator(TB_Module* m) {
    switch (m->target_arch) {
        case TB_ARCH_X86_64: return &tb__x64_codegen;
        // case TB_ARCH_AARCH64: return &tb__aarch64_codegen;
        // case TB_ARCH_WASM32: return &tb__wasm32_codegen;
        default: return NULL;
    }
}

int tb__get_local_tid(void) {
    // the value it spits out is zero-based, but
    // the TIDs consider zero as a NULL space.
    if (tid == 0) {
        int new_id = tb_atomic_int_add(&total_tid, 1);
        tid = new_id + 1;
    }

    return tid - 1;
}

bool tb_symbol_is_comdat(const TB_Symbol* s) {
    return s->tag == TB_SYMBOL_FUNCTION && ((const TB_Function*) s)->comdat.type != TB_COMDAT_NONE;
}

char* tb__arena_strdup(TB_Module* m, const char* src) {
    if (src == NULL) return NULL;

    char* newstr;
    CUIK_TIMED_BLOCK("lock") {
        mtx_lock(&m->lock);

        size_t length = strlen(src);
        newstr = arena_alloc(&m->arena, length + 1, 1);
        memcpy(newstr, src, length + 1);

        mtx_unlock(&m->lock);
    }

    return newstr;
}

static TB_CodeRegion* get_or_allocate_code_region(TB_Module* m, int tid) {
    if (m->code_regions[tid] == NULL) {
        m->code_regions[tid] = tb_platform_valloc(CODE_REGION_BUFFER_SIZE / total_tid);
        if (m->code_regions[tid] == NULL) tb_panic("could not allocate code region!");

        m->code_regions[tid]->capacity = CODE_REGION_BUFFER_SIZE / total_tid;
    }

    return m->code_regions[tid];
}

TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width) {
    assert(tb_is_power_of_two(width));
    return (TB_DataType) { .type = type, .width = tb_ffs(width) - 1 };
}

TB_API TB_Module* tb_module_create_for_host(const TB_FeatureSet* features, bool is_jit) {
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

    return tb_module_create(arch, sys, features, is_jit);
}

TB_API TB_Module* tb_module_create(TB_Arch arch, TB_System sys, const TB_FeatureSet* features, bool is_jit) {
    TB_Module* m = tb_platform_heap_alloc(sizeof(TB_Module));
    if (m == NULL) {
        fprintf(stderr, "tb_module_create: Out of memory!\n");
        return NULL;
    }
    memset(m, 0, sizeof(TB_Module));

    m->max_threads = TB_MAX_THREADS;
    m->is_jit = is_jit;

    m->target_abi = (sys == TB_SYSTEM_WINDOWS) ? TB_ABI_WIN64 : TB_ABI_SYSTEMV;
    m->target_arch = arch;
    m->target_system = sys;
    if (features == NULL) {
        m->features = (TB_FeatureSet){ 0 };
    } else {
        m->features = *features;
    }

    dyn_array_put(m->files, (TB_File){ 0 });

    // we start a little off the start just because
    mtx_init(&m->lock, mtx_plain);

    m->text.name  = tb__arena_strdup(m, ".text");
    m->text.kind  = TB_MODULE_SECTION_TEXT;
    m->data.name  = tb__arena_strdup(m, ".data");
    m->rdata.name = tb__arena_strdup(m, sys == TB_SYSTEM_WINDOWS ? ".rdata" : ".rodata");
    m->tls.name   = tb__arena_strdup(m, sys == TB_SYSTEM_WINDOWS ? ".tls$"  : ".tls");
    m->tls.kind   = TB_MODULE_SECTION_TLS;
    return m;
}

TB_API TB_FunctionOutput* tb_module_compile_function(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode) {
    ICodeGen* restrict code_gen = tb__find_code_generator(m);

    // Machine code gen
    int id = tb__get_local_tid();
    assert(id < TB_MAX_THREADS);

    TB_CodeRegion* region = get_or_allocate_code_region(m, id);

    size_t align_mask = _Alignof(TB_FunctionOutput) - 1;
    region->size = (region->size + align_mask) & ~align_mask;
    assert(region->size + sizeof(TB_FunctionOutput) < region->capacity);

    // allocate the TB_FunctionOutput in the code region
    TB_FunctionOutput* func_out = (TB_FunctionOutput*) &region->data[region->size];
    region->size += sizeof(TB_FunctionOutput);

    if (isel_mode == TB_ISEL_COMPLEX && code_gen->complex_path == NULL) {
        log_warn("tb warning: complex path is missing, defaulting to fast path");
        isel_mode = TB_ISEL_FAST;
    }

    CUIK_TIMED_BLOCK_ARGS("compile func", f->super.name) {
        *func_out = (TB_FunctionOutput){ .parent = f, .linkage = f->linkage };

        uint8_t* local_buffer = &region->data[region->size];
        size_t local_capacity = region->capacity - region->size;

        if (isel_mode == TB_ISEL_COMPLEX) {
            code_gen->complex_path(f, func_out, &m->features, local_buffer, local_capacity);
        } else {
            code_gen->fast_path(f, func_out, &m->features, local_buffer, local_capacity);
            assert(func_out->code);
        }
    }

    // prologue & epilogue insertion
    CUIK_TIMED_BLOCK_ARGS("pro & epi insertion", f->super.name) {
        uint8_t buffer[PROEPI_BUFFER];
        uint8_t* base = &region->data[region->size];
        size_t body_size = func_out->code_size;
        assert(func_out->code == base);

        uint64_t meta = func_out->prologue_epilogue_metadata;
        size_t prologue_len = code_gen->emit_prologue(buffer, meta, func_out->stack_usage);

        // shift body up & place prologue
        memmove(base + prologue_len, base, body_size);
        memcpy(base, buffer, prologue_len);

        // place epilogue
        size_t epilogue_len = code_gen->emit_epilogue(buffer, meta, func_out->stack_usage);
        memcpy(base + prologue_len + body_size, buffer, epilogue_len);

        func_out->prologue_length = prologue_len;
        func_out->epilogue_length = epilogue_len;
        func_out->code_size += (prologue_len + epilogue_len);
    }

    tb_atomic_size_add(&m->compiled_function_count, 1);
    region->size += func_out->code_size;

    f->output = func_out;
    return func_out;
}

TB_API size_t tb_module_get_function_count(TB_Module* m) {
    return m->symbol_count[TB_SYMBOL_FUNCTION];
}

TB_API void tb_module_kill_symbol(TB_Module* m, TB_Symbol* sym) {
    sym->tag = TB_SYMBOL_TOMBSTONE;
}

TB_API void tb_module_destroy(TB_Module* m) {
    arena_free(&m->arena);

    TB_Symbol* s = m->first_symbol_of_tag[TB_SYMBOL_FUNCTION];
    while (s) {
        TB_Symbol* next = s->next;
        tb_module_kill_symbol(m, s);
        s = next;
    }

    FOREACH_N(i, 0, m->max_threads) {
        if (m->code_regions[i] != NULL) {
            tb_platform_vfree(m->code_regions[i], m->code_regions[i]->capacity);
            m->code_regions[i] = NULL;
        }
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_destroy(m->thread_info[i].globals);
        pool_destroy(m->thread_info[i].externals);
        pool_destroy(m->thread_info[i].debug_types);
    }

    dyn_array_destroy(m->files);
    tb_platform_heap_free(m);
}

TB_API TB_FileID tb_file_create(TB_Module* m, const char* path) {
    mtx_lock(&m->lock);

    // skip the NULL file entry
    // TODO(NeGate): we should introduce a hash map
    FOREACH_N(i, 1, dyn_array_length(m->files)) {
        if (strcmp(m->files[i].path, path) == 0) {
            mtx_unlock(&m->lock);
            return i;
        }
    }

    // Allocate string (for now they all live to the end of the module, we might
    // allow for changing this later, doesn't matter for AOT... which is the usecase
    // of this?)
    size_t len = strlen(path);
    char* newstr = arena_alloc(&m->arena, len + 1, 1);
    memcpy(newstr, path, len);

    TB_File f = { .path = newstr };
    TB_FileID id = dyn_array_length(m->files);
    dyn_array_put(m->files, f);

    mtx_unlock(&m->lock);
    return id;
}

TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv cc, size_t param_count, const TB_PrototypeParam* params, size_t return_count, const TB_PrototypeParam* returns, bool has_varargs) {
    size_t size = sizeof(TB_FunctionPrototype) + ((param_count + return_count) * sizeof(TB_PrototypeParam));
    TB_FunctionPrototype* p = arena_alloc(&tb__arena2, size, _Alignof(TB_FunctionPrototype));

    p->call_conv = cc;
    p->return_count = return_count;
    p->param_count = param_count;
    p->has_varargs = has_varargs;
    memcpy(p->params, params, param_count * sizeof(TB_PrototypeParam));
    memcpy(p->params + param_count, returns, return_count * sizeof(TB_PrototypeParam));
    return p;
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_Linkage linkage, TB_ComdatType comdat) {
    TB_Function* f = (TB_Function*) tb_symbol_alloc(m, TB_SYMBOL_FUNCTION, name, sizeof(TB_Function));
    f->linkage = linkage;
    f->comdat.type = comdat;
    return f;
}

TB_API void tb_symbol_set_name(TB_Symbol* s, const char* name) {
    s->name = tb__arena_strdup(s->module, name);
}

TB_API const char* tb_symbol_get_name(TB_Symbol* s) {
    return s->name;
}

TB_API void tb_function_set_prototype(TB_Function* f, TB_FunctionPrototype* p, TB_Arena* arena) {
    assert(f->prototype == NULL);
    const ICodeGen* restrict code_gen = tb__find_code_generator(f->super.module);

    size_t param_count = p->param_count;
    size_t extra_size = sizeof(TB_NodeRegion) + (param_count * sizeof(TB_Node*));

    if (arena == NULL) {
        f->arena = tb_node_arena();
    } else {
        f->arena = arena;
    }

    f->control_node_count = 1;
    f->active_control_node = f->start_node = tb_alloc_node(f, TB_START, TB_TYPE_TUPLE, 0, extra_size);

    TB_NodeRegion* start = TB_NODE_GET_EXTRA(f->start_node);
    start->succ_count = 0;
    start->succ = NULL;
    start->proj_count = param_count;
    start->projs = f->arena->alloc(f->arena, param_count * sizeof(TB_Node*), _Alignof(TB_Node*));

    // create parameter projections
    TB_PrototypeParam* rets = TB_PROTOTYPE_RETURNS(p);
    FOREACH_N(i, 0, param_count) {
        TB_DataType dt = p->params[i].dt;

        TB_Node* proj = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
        proj->inputs[0] = f->start_node;
        TB_NODE_SET_EXTRA(proj, TB_NodeProj, .index = i);

        // fill in acceleration structure
        start->projs[i] = proj;
    }

    f->prototype = p;
}

TB_API TB_FunctionPrototype* tb_function_get_prototype(TB_Function* f) {
    return f->prototype;
}

TB_API void* tb_global_add_region(TB_Module* m, TB_Global* g, size_t offset, size_t size) {
    assert(offset == (uint32_t)offset);
    assert(size == (uint32_t)size);
    assert(g->obj_count + 1 <= g->obj_capacity);

    void* ptr = tb_platform_heap_alloc(size);
    g->objects[g->obj_count++] = (TB_InitObj) {
        .type = TB_INIT_OBJ_REGION, .offset = offset, .region = { .size = size, .ptr = ptr }
    };

    return ptr;
}

TB_API void tb_global_add_symbol_reloc(TB_Module* m, TB_Global* g, size_t offset, const TB_Symbol* symbol) {
    assert(offset == (uint32_t) offset);
    assert(g->obj_count + 1 <= g->obj_capacity);
    assert(symbol != NULL);

    g->objects[g->obj_count++] = (TB_InitObj) { .type = TB_INIT_OBJ_RELOC, .offset = offset, .reloc = symbol };
}

TB_API TB_Global* tb_global_create(TB_Module* m, const char* name, TB_DebugType* dbg_type, TB_Linkage linkage) {
    int tid = tb__get_local_tid();

    TB_Global* g = pool_put(m->thread_info[tid].globals);
    *g = (TB_Global){
        .super = {
            .tag = TB_SYMBOL_GLOBAL,
            .name = tb__arena_strdup(m, name),
            .module = m,
        },
        .dbg_type = dbg_type,
        .linkage = linkage
    };
    tb_symbol_append(m, (TB_Symbol*) g);

    return g;
}

TB_API void tb_global_set_storage(TB_Module* m, TB_ModuleSection* section, TB_Global* global, size_t size, size_t align, size_t max_objects) {
    assert(size > 0 && align > 0 && tb_is_power_of_two(align));
    global->parent = section;
    global->pos = 0;
    global->size = size;
    global->align = align;
    global->obj_count = 0;
    global->obj_capacity = max_objects;

    CUIK_TIMED_BLOCK("lock") {
        mtx_lock(&m->lock);
        global->objects = ARENA_ARR_ALLOC(&m->arena, max_objects, TB_InitObj);
        dyn_array_put(section->globals, global);
        mtx_unlock(&m->lock);
    }
}

TB_API TB_Safepoint* tb_safepoint_get(TB_Function* f, uint32_t relative_ip) {
    size_t left = 0;
    size_t right = f->safepoint_count;

    uint32_t ip = relative_ip - f->output->prologue_length;

    const TB_SafepointKey* keys = f->output->safepoints;
    while (left < right) {
        size_t middle = (left + right) / 2;

        if (keys[middle].ip == ip) return keys[left].sp;
        if (keys[middle].ip < ip) left = middle + 1;
        else right = middle;
    }

    return NULL;
}

TB_API TB_ModuleSection* tb_module_get_text(TB_Module* m) {
    return &m->text;
}

TB_API TB_ModuleSection* tb_module_get_rdata(TB_Module* m) {
    return &m->rdata;
}

TB_API TB_ModuleSection* tb_module_get_data(TB_Module* m) {
    return &m->data;
}

TB_API TB_ModuleSection* tb_module_get_tls(TB_Module* m) {
    return &m->tls;
}

TB_API void tb_module_set_tls_index(TB_Module* m, const char* name) {
    if (tb_atomic_int_cmpxchg(&m->is_tls_defined, 0, 1)) {
        m->tls_index_extern = (TB_Symbol*) tb_extern_create(m, "_tls_index", TB_EXTERNAL_SO_LOCAL);
    }
}

TB_API void tb_symbol_bind_ptr(TB_Symbol* s, void* ptr) {
    s->address = ptr;
}

TB_API TB_ExternalType tb_extern_get_type(TB_External* e) {
    return e->type;
}

TB_API void* tb_function_get_jit_pos(TB_Function* f) {
    return f->compiled_pos;
}

TB_API TB_External* tb_extern_create(TB_Module* m, const char* name, TB_ExternalType type) {
    assert(name != NULL);
    int tid = tb__get_local_tid();

    TB_External* e = pool_put(m->thread_info[tid].externals);
    *e = (TB_External){
        .super = {
            .tag = TB_SYMBOL_EXTERNAL,
            .name = tb__arena_strdup(m, name),
            .module = m,
        },
        .type = type,
    };
    tb_symbol_append(m, (TB_Symbol*) e);
    return e;
}

TB_API TB_Function* tb_first_function(TB_Module* m) {
    return (TB_Function*) m->first_symbol_of_tag[TB_SYMBOL_FUNCTION];
}

TB_API TB_Function* tb_next_function(TB_Function* f) {
    return (TB_Function*) f->super.next;
}

TB_API TB_External* tb_first_external(TB_Module* m) {
    return (TB_External*) m->first_symbol_of_tag[TB_SYMBOL_EXTERNAL];
}

TB_API TB_External* tb_next_external(TB_External* e) {
    return (TB_External*) e->super.next;
}

//
// TLS - Thread local storage
//
// Certain backend elements require memory but we would prefer to avoid
// making any heap allocations when possible to there's a preallocated
// block per thread that can run TB.
//
void tb_free_thread_resources(void) {
    if (tb_thread_storage != NULL) {
        tb_platform_vfree(tb_thread_storage, TB_TEMPORARY_STORAGE_SIZE);
        tb_thread_storage = NULL;
    }

    arena_free(&tb__arena);
}

TB_TemporaryStorage* tb_tls_allocate() {
    if (tb_thread_storage == NULL) {
        tb_thread_storage = tb_platform_valloc(TB_TEMPORARY_STORAGE_SIZE);
        if (tb_thread_storage == NULL) {
            tb_panic("out of memory");
        }
    }

    TB_TemporaryStorage* store = (TB_TemporaryStorage*)tb_thread_storage;
    store->used = 0;
    return store;
}

TB_TemporaryStorage* tb_tls_steal() {
    if (tb_thread_storage == NULL) {
        tb_thread_storage = tb_platform_valloc(TB_TEMPORARY_STORAGE_SIZE);
        if (tb_thread_storage == NULL) {
            tb_panic("out of memory");
        }
    }

    return (TB_TemporaryStorage*)tb_thread_storage;
}

bool tb_tls_can_fit(TB_TemporaryStorage* store, size_t size) {
    return (sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);
}

void* tb_tls_try_push(TB_TemporaryStorage* store, size_t size) {
    if (sizeof(TB_TemporaryStorage) + store->used + size >= TB_TEMPORARY_STORAGE_SIZE) {
        return NULL;
    }

    void* ptr = &store->data[store->used];
    store->used += size;
    return ptr;
}

void* tb_tls_push(TB_TemporaryStorage* store, size_t size) {
    assert(sizeof(TB_TemporaryStorage) + store->used + size < TB_TEMPORARY_STORAGE_SIZE);

    void* ptr = &store->data[store->used];
    store->used += size;
    return ptr;
}

void* tb_tls_pop(TB_TemporaryStorage* store, size_t size) {
    assert(sizeof(TB_TemporaryStorage) + store->used > size);

    store->used -= size;
    return &store->data[store->used];
}

void* tb_tls_peek(TB_TemporaryStorage* store, size_t distance) {
    assert(sizeof(TB_TemporaryStorage) + store->used > distance);

    return &store->data[store->used - distance];
}

void tb_tls_restore(TB_TemporaryStorage* store, void* ptr) {
    size_t i = ((uint8_t*)ptr) - store->data;
    assert(i <= store->used);

    store->used = i;
}

void tb_emit_symbol_patch(TB_FunctionOutput* func_out, const TB_Symbol* target, size_t pos) {
    TB_Module* m = func_out->parent->super.module;
    TB_SymbolPatch* p = ARENA_ALLOC(&tb__arena2, TB_SymbolPatch);

    // doesn't need to be atomic
    *p = (TB_SymbolPatch){ .prev = func_out->last_patch, .source = func_out->parent, .target = target, .pos = pos };
    func_out->last_patch = p;
    func_out->patch_count += 1;
}

// EMITTER CODE:
//   Simple linear allocation for the
//   backend's to output code with.
void* tb_out_reserve(TB_Emitter* o, size_t count) {
    if (o->count + count >= o->capacity) {
        if (o->capacity == 0) {
            o->capacity = 64;
        } else {
            o->capacity += count;
            o->capacity *= 2;
        }

        o->data = tb_platform_heap_realloc(o->data, o->capacity);
        if (o->data == NULL) tb_todo();
    }

    return &o->data[o->count];
}

void tb_out_commit(TB_Emitter* o, size_t count) {
    assert(o->count + count < o->capacity);
    o->count += count;
}

size_t tb_out_get_pos(TB_Emitter* o, void* p) {
    return (uint8_t*)p - o->data;
}

void* tb_out_grab(TB_Emitter* o, size_t count) {
    void* p = tb_out_reserve(o, count);
    o->count += count;

    return p;
}

void* tb_out_get(TB_Emitter* o, size_t pos) {
    return &o->data[o->count];
}

size_t tb_out_grab_i(TB_Emitter* o, size_t count) {
    tb_out_reserve(o, count);

    size_t old = o->count;
    o->count += count;
    return old;
}

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i) {
    assert(o->count + 1 < o->capacity);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_out1b(TB_Emitter* o, uint8_t i) {
    tb_out_reserve(o, 1);

    o->data[o->count] = i;
    o->count += 1;
}

void tb_out2b(TB_Emitter* o, uint16_t i) {
    tb_out_reserve(o, 2);

    *((uint16_t*)&o->data[o->count]) = i;
    o->count += 2;
}

void tb_out4b(TB_Emitter* o, uint32_t i) {
    tb_out_reserve(o, 4);

    *((uint32_t*)&o->data[o->count]) = i;
    o->count += 4;
}

void tb_patch1b(TB_Emitter* o, uint32_t pos, uint8_t i) {
    *((uint8_t*)&o->data[pos]) = i;
}

void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i) {
    *((uint16_t*)&o->data[pos]) = i;
}

void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i) {
    *((uint32_t*)&o->data[pos]) = i;
}

void tb_patch8b(TB_Emitter* o, uint32_t pos, uint64_t i) {
    *((uint64_t*)&o->data[pos]) = i;
}

uint8_t tb_get1b(TB_Emitter* o, uint32_t pos) {
    return *((uint8_t*)&o->data[pos]);
}

uint16_t tb_get2b(TB_Emitter* o, uint32_t pos) {
    return *((uint16_t*)&o->data[pos]);
}

uint32_t tb_get4b(TB_Emitter* o, uint32_t pos) {
    return *((uint32_t*)&o->data[pos]);
}

void tb_out8b(TB_Emitter* o, uint64_t i) {
    tb_out_reserve(o, 8);

    *((uint64_t*)&o->data[o->count]) = i;
    o->count += 8;
}

void tb_out_zero(TB_Emitter* o, size_t len) {
    tb_out_reserve(o, len);
    memset(&o->data[o->count], 0, len);
    o->count += len;
}

size_t tb_outstr_nul_UNSAFE(TB_Emitter* o, const char* str) {
    size_t start = o->count;

    for (; *str; str++) {
        o->data[o->count++] = *str;
    }

    o->data[o->count++] = 0;
    return start;
}

size_t tb_outstr_nul(TB_Emitter* o, const char* str) {
    size_t start = o->count;
    size_t len = strlen(str) + 1;
    tb_out_reserve(o, len);

    memcpy(&o->data[o->count], str, len);
    return start;
}

void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
    while (*str) o->data[o->count++] = *str++;
}

size_t tb_outs(TB_Emitter* o, size_t len, const void* str) {
    tb_out_reserve(o, len);
    size_t start = o->count;

    memcpy(&o->data[o->count], str, len);
    o->count += len;
    return start;
}

void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const void* str) {
    memcpy(&o->data[o->count], str, len);
    o->count += len;
}
