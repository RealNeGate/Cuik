#include "tb_internal.h"
#include "host.h"
#include "coroutine.h"

enum { BATCH_SIZE = 8192 };

static thread_local uint8_t* tb_thread_storage;
static thread_local int tid;
static tb_atomic_int total_tid;

ICodeGen* tb__find_code_generator(TB_Module* m) {
    switch (m->target_arch) {
        #if 0
        // work in progress
        case TB_ARCH_X86_64: return &tb__x64v2_codegen;
        #else
        case TB_ARCH_X86_64: return &tb__x64_codegen;
        #endif
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

/*
inline static uint64_t foo(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ((long long)ts.tv_sec * 1000000000LL) + ts.tv_nsec;
}

#define TELL_ME_WHY() for (uint64_t __start = foo(), __i = 0; __i < 1; __i++, printf("Took: %f microseconds\n", (foo() - __start) / 1000.0))
*/

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

    m->prototypes_arena = tb_platform_valloc(PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));
    if (m->prototypes_arena == NULL) {
        fprintf(stderr, "tb_module_create: Out of memory!\n");
        return NULL;
    }

    m->files.count = 1;
    m->files.capacity = 64;
    m->files.data = tb_platform_heap_alloc(64 * sizeof(TB_File));
    m->files.data[0] = (TB_File) { 0 };

    FOREACH_N(i, 0, TB_MAX_THREADS) {
        m->thread_info[i].const_patches  = dyn_array_create(TB_ConstPoolPatch, 4096);
        m->thread_info[i].symbol_patches = dyn_array_create(TB_SymbolPatch, 4096);
    }

    // we start a little off the start just because
    m->rdata_region_size = 16;

    tb_platform_arena_init();
    return m;
}

TB_API bool tb_module_compile_function(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode) {
    assert(f->output == NULL);
    ICodeGen* restrict code_gen = tb__find_code_generator(m);

    // Machine code gen
    int id = tb__get_local_tid();
    assert(id < TB_MAX_THREADS);

    TB_CodeRegion* region = get_or_allocate_code_region(m, id);
    TB_FunctionOutput* func_out = tb_platform_arena_alloc(sizeof(TB_FunctionOutput));

    if (isel_mode == TB_ISEL_COMPLEX && code_gen->complex_path == NULL) {
        // TODO(NeGate): we need better logging...
        fprintf(stderr, "TB warning: complex path is missing, defaulting to fast path.\n");
        isel_mode = TB_ISEL_FAST;
    }

    uint8_t* local_buffer = &region->data[region->size];
    size_t local_capacity = region->capacity - region->size;
    if (isel_mode == TB_ISEL_COMPLEX) {
        *func_out = code_gen->complex_path(f, &m->features, local_buffer, local_capacity, id);
    } else {
        *func_out = code_gen->fast_path(f, &m->features, local_buffer, local_capacity, id);
    }

    // prologue & epilogue insertion
    {
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
    return true;
}

TB_API size_t tb_module_get_function_count(TB_Module* m) {
    return m->symbol_count[TB_SYMBOL_FUNCTION];
}

TB_API void tb_module_kill_symbol(TB_Module* m, TB_Symbol* sym) {
    switch (sym->tag) {
        case TB_SYMBOL_TOMBSTONE: break;
        case TB_SYMBOL_SYMLINK: break;
        case TB_SYMBOL_FUNCTION: {
            TB_Function* f = (TB_Function*) sym;

            tb_platform_heap_free(f->bbs);
            tb_platform_heap_free(f->nodes);
            tb_platform_heap_free(f->attrib_pool);
            tb_platform_heap_free(f->vla.data);
            break;
        }
        case TB_SYMBOL_EXTERNAL: break;
        case TB_SYMBOL_GLOBAL: break;
        default: tb_unreachable();
    }

    sym->tag = TB_SYMBOL_TOMBSTONE;
}

TB_API void tb_module_destroy(TB_Module* m) {
    tb_platform_arena_free();
    tb_platform_string_free();

    {
        TB_Symbol* s = m->first_symbol_of_tag[TB_SYMBOL_FUNCTION];

        if (s != NULL) {
            enum TB_SymbolTag tag = s->tag;

            do {
                TB_Symbol* next = s->next;
                tb_assume(tag == s->tag);

                // TODO(NeGate): probably wanna have a custom heap for the symbol table
                tb_platform_heap_free(s);
                s = next;
            } while (s != NULL);
        }
    }

    FOREACH_N(i, 0, m->max_threads) {
        if (m->code_regions[i] != NULL) {
            tb_platform_vfree(m->code_regions[i], m->code_regions[i]->capacity);
            m->code_regions[i] = NULL;
        }
    }

    if (m->jit_region) {
        tb_platform_vfree(m->jit_region, m->jit_region_size);
        m->jit_region = NULL;
    }

    FOREACH_N(i, 0, m->max_threads) {
        pool_destroy(m->thread_info[i].globals);
        pool_destroy(m->thread_info[i].externals);
        pool_destroy(m->thread_info[i].debug_types);

        dyn_array_destroy(m->thread_info[i].symbol_patches);
        dyn_array_destroy(m->thread_info[i].const_patches);
    }

    tb_platform_vfree(m->prototypes_arena, PROTOTYPES_ARENA_SIZE * sizeof(uint64_t));

    tb_platform_heap_free(m->files.data);
    tb_platform_heap_free(m);
}

TB_API TB_FileID tb_file_create(TB_Module* m, const char* path) {
    // skip the NULL file entry
    FOREACH_N(i, 1, m->files.count) {
        if (strcmp(m->files.data[i].path, path) == 0) return i;
    }

    if (m->files.count + 1 >= m->files.capacity) {
        m->files.capacity *= 2;
        m->files.data = tb_platform_heap_realloc(m->files.data, m->files.capacity * sizeof(TB_File));
    }

    char* str = tb_platform_string_alloc(path);

    size_t r = m->files.count++;
    m->files.data[r] = (TB_File) { .path = str };
    return r;
}

void tb_function_reserve_nodes(TB_Function* f, size_t extra) {
    if (f->node_count + extra >= f->node_capacity) {
        f->node_capacity = (f->node_count + extra) * 2;

        f->nodes = tb_platform_heap_realloc(f->nodes, sizeof(TB_Node) * f->node_capacity);
        if (f->nodes == NULL) tb_panic("Out of memory");
    }
}

TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, TB_DebugType* return_type, int num_params, bool has_varargs) {
    assert(num_params == (uint32_t)num_params);

    size_t space_needed = (sizeof(TB_FunctionPrototype) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);
    space_needed += ((num_params * sizeof(TB_PrototypeParam)) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);

    size_t len = tb_atomic_size_add(&m->prototypes_arena_size, space_needed);
    if (len + space_needed >= PROTOTYPES_ARENA_SIZE) {
        tb_panic("Prototype arena: out of memory!\n");
    }

    TB_FunctionPrototype* p = (TB_FunctionPrototype*)&m->prototypes_arena[len];
    p->call_conv = conv;
    p->param_capacity = num_params;
    p->param_count = 0;
    p->return_dt = return_dt;
    p->return_type = return_type;
    p->has_varargs = has_varargs;
    return p;
}

TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt) {
    assert(p->param_count + 1 <= p->param_capacity);
    p->params[p->param_count++] = (TB_PrototypeParam){ dt };
}

TB_API void tb_prototype_add_param_named(TB_FunctionPrototype* p, TB_DataType dt, const char* name, TB_DebugType* debug_type) {
    assert(p->param_count + 1 <= p->param_capacity);
    p->params[p->param_count++] = (TB_PrototypeParam){ dt, tb_platform_string_alloc(name), debug_type };
}

TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_Linkage linkage) {
    TB_Function* f = (TB_Function*) tb_symbol_alloc(m, TB_SYMBOL_FUNCTION, name, sizeof(TB_Function));
    f->linkage = linkage;

    f->bb_capacity = 4;
    f->bb_count = 1;
    f->bbs = tb_platform_heap_alloc(f->bb_capacity * sizeof(TB_BasicBlock));

    f->node_capacity = 64;
    f->node_count = 2;
    f->nodes = tb_platform_heap_alloc(f->node_capacity * sizeof(TB_Node));

    f->attrib_pool_capacity = 64;
    f->attrib_pool_count = 1; // 0 is reserved
    f->attrib_pool = tb_platform_heap_alloc(64 * sizeof(TB_Attrib));

    // Null slot
    f->nodes[0] = (TB_Node) { .next = 0 };

    // this is just a dummy slot so that things like parameters can anchor to
    f->nodes[1] = (TB_Node) { .next = 0 };
    f->bbs[0] = (TB_BasicBlock){ 1, 1 };
    return f;
}

TB_API void tb_symbol_set_name(TB_Symbol* s, const char* name) {
    s->name = tb_platform_string_alloc(name);
}

TB_API const char* tb_symbol_get_name(TB_Symbol* s) {
    return s->name;
}

TB_API void tb_function_set_prototype(TB_Function* f, const TB_FunctionPrototype* p) {
    size_t old_param_count = f->prototype != NULL ? f->prototype->param_count : 0;
    size_t new_param_count = p->param_count;

    const ICodeGen* restrict code_gen = tb__find_code_generator(f->super.module);

    f->params = tb_platform_heap_realloc(f->params, sizeof(TB_Reg) * new_param_count);
    if (new_param_count > 0 && f->params == NULL) {
        tb_panic("tb_function_set_prototype: Out of memory!");
    }

    // walk to the end of the param list (it starts directly after the entry label)
    TB_Reg prev = 1, r = 1;
    size_t count = 0;
    FOREACH_N(i, 0, old_param_count) {
        // reassign these old slots
        TB_DataType dt = p->params[i].dt;
        TB_CharUnits size, align;
        code_gen->get_data_type_size(dt, &size, &align);

        assert(r != TB_NULL_REG);
        // fill in acceleration structure
        f->params[count++] = r;

        // reinitialize node
        f->nodes[r].type = TB_PARAM;
        f->nodes[r].dt = dt;
        f->nodes[r].param = (struct TB_NodeParam) { .id = i, .size = size };

        prev = r;
        r = f->nodes[r].next;
    }

    FOREACH_N(i, old_param_count, new_param_count) {
        TB_Reg new_reg = tb_function_insert_after(f, 0, prev);
        TB_Node* new_node = &f->nodes[new_reg];

        TB_DataType dt = p->params[i].dt;
        TB_CharUnits size, align;
        code_gen->get_data_type_size(dt, &size, &align);

        // fill in acceleration structure
        f->params[count++] = new_reg;

        // initialize node
        new_node->type = TB_PARAM;
        new_node->dt = dt;
        new_node->param = (struct TB_NodeParam){ .id = i, .size = size };
        prev = new_reg;
    }

    f->prototype = p;
}

TB_API const TB_FunctionPrototype* tb_function_get_prototype(TB_Function* f) {
    return f->prototype;
}

TB_API TB_Initializer* tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects) {
    tb_assume(size == (uint32_t)size);
    tb_assume(align == (uint32_t)align);
    tb_assume(max_objects == (uint32_t)max_objects);

    size_t space_needed = (sizeof(TB_Initializer) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);
    space_needed += ((max_objects * sizeof(TB_InitObj)) + (sizeof(uint64_t) - 1)) / sizeof(uint64_t);

    TB_Initializer* init = tb_platform_heap_alloc(sizeof(TB_Initializer) + (space_needed * sizeof(uint64_t)));
    init->size = size;
    init->align = align;
    init->obj_capacity = max_objects;
    init->obj_count = 0;
    return init;
}

TB_API void* tb_initializer_add_region(TB_Module* m, TB_Initializer* init, size_t offset, size_t size) {
    assert(offset == (uint32_t)offset);
    assert(size == (uint32_t)size);
    assert(init->obj_count + 1 <= init->obj_capacity);

    void* ptr = tb_platform_heap_alloc(size);
    init->objects[init->obj_count++] = (TB_InitObj) {
        .type = TB_INIT_OBJ_REGION, .offset = offset, .region = { .size = size, .ptr = ptr }
    };

    return ptr;
}

TB_API void tb_initializer_add_symbol_reloc(TB_Module* m, TB_Initializer* init, size_t offset, const TB_Symbol* symbol) {
    assert(offset == (uint32_t)offset);
    assert(init->obj_count + 1 <= init->obj_capacity);
    assert(symbol != NULL);

    init->objects[init->obj_count++] = (TB_InitObj) { .type = TB_INIT_OBJ_RELOC, .offset = offset, .reloc = symbol };
}

TB_API TB_Global* tb_global_create(TB_Module* m, const char* name, TB_StorageClass storage, TB_DebugType* dbg_type, TB_Linkage linkage) {
    int tid = tb__get_local_tid();

    TB_Global* g = pool_put(m->thread_info[tid].globals);
    *g = (TB_Global){
        .super = {
            .tag = TB_SYMBOL_GLOBAL,
            .name = tb_platform_string_alloc(name),
            .module = m,
        },
        .dbg_type = dbg_type,
        .linkage = linkage,
        .storage = storage
    };
    tb_symbol_append(m, (TB_Symbol*) g);

    return g;
}

TB_API void tb_global_set_initializer(TB_Module* m, TB_Global* global, TB_Initializer* init) {
    tb_atomic_size_t* region_size = global->storage == TB_STORAGE_TLS ? &m->tls_region_size : &m->data_region_size;
    size_t pos = tb_atomic_size_add(region_size, init->size + init->align);

    // TODO(NeGate): Assert on non power of two alignment
    size_t align_mask = init->align - 1;
    pos = (pos + align_mask) & ~align_mask;

    assert(init);
    assert(pos < UINT32_MAX && "Cannot fit global into space");
    assert((pos + init->size) < UINT32_MAX && "Cannot fit global into space");

    global->pos = pos;
    global->init = init;
}

TB_API void tb_module_set_tls_index(TB_Module* m, TB_Symbol* e) {
    m->tls_index_extern = e;
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
            .name = tb_platform_string_alloc(name),
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

void tb_emit_symbol_patch(TB_Module* m, TB_Function* source, const TB_Symbol* target, size_t pos, bool is_function, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);

    TB_SymbolPatch p = { .source = source, .target = target, .is_function = is_function, .pos = pos };
    dyn_array_put(m->thread_info[local_thread_id].symbol_patches, p);
}

uint32_t tb_emit_const_patch(TB_Module* m, TB_Function* source, size_t pos, const void* ptr, size_t len, size_t local_thread_id) {
    assert(pos == (uint32_t)pos);
    assert(len == (uint32_t)len);

    size_t align = len > 8 ? 16 : 0;
    size_t alloc_pos = tb_atomic_size_add(&m->rdata_region_size, len + align);

    size_t rdata_pos = len > 8 ? align_up(alloc_pos, 16) : alloc_pos;
    TB_ConstPoolPatch p = {
        .source = source, .pos = pos, .rdata_pos = rdata_pos, .data = ptr, .length = len
    };
    dyn_array_put(m->thread_info[local_thread_id].const_patches, p);

    assert(rdata_pos == (uint32_t)rdata_pos);
    return rdata_pos;
}

//
// OBJECT FILE
//
void tb_object_free(TB_ObjectFile* obj) {
    FOREACH_N(i, 0, obj->section_count) {
        free(obj->sections[i].relocations);
    }
    free(obj);
}

//
// EMITTER CODE
//
// Simple linear allocation for the backend's to output code with
//
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

void tb_outstr_UNSAFE(TB_Emitter* o, const char* str) {
    while (*str) o->data[o->count++] = *str++;
}

void tb_outs(TB_Emitter* o, size_t len, const void* str) {
    tb_out_reserve(o, len);
    memcpy(&o->data[o->count], str, len);
    o->count += len;
}

void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const void* str) {
    memcpy(&o->data[o->count], str, len);
    o->count += len;
}
