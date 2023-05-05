#include "../tb_internal.h"
#include "../host.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

size_t tb_helper_write_text_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);
size_t tb_helper_write_data_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);
size_t tb_helper_write_rodata_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos);

enum {
    SLAB_SIZE = 0x1000,
    BITMAP_GRANULARITY = 16,

    USED_BITMAP_COUNT = (SLAB_SIZE / BITMAP_GRANULARITY) / 8,
};

typedef struct Slab {
    // protect is what it's supposed to be, but active_protect is what it's actually
    TB_MemProtect protect, active_protect;
    uint64_t used_bitmap[USED_BITMAP_COUNT];
} Slab;

typedef struct {
    size_t capacity;
    uint8_t* block;

    size_t slab_count;
    Slab* slabs;
} TB_JITHeap;

struct TB_JITContext {
    // we have a special thunk for relocations we haven't filled
    void* null_thunk;

    // all our globals, import tables and functions go here
    TB_JITHeap heap;
};

static TB_JITHeap tb_jitheap_create(size_t size) {
    assert(size == (uint32_t) size);

    // align to page size
    size = (size + SLAB_SIZE - 1) & ~(SLAB_SIZE - 1);
    uint32_t slab_count = size / SLAB_SIZE;

    TB_JITHeap h = { 0 };
    h.capacity = size;
    h.block = tb_platform_valloc(size);
    h.slab_count = slab_count;
    h.slabs = tb_platform_heap_alloc(slab_count * sizeof(Slab));

    FOREACH_N(i, 0, slab_count) {
        h.slabs[i] = (Slab){ i * SLAB_SIZE };
    }
    return h;
}

// done with changes, apply protection again
static void tb_jitheap_unlock(TB_JITHeap* c, void* ptr, size_t size) {
    ptrdiff_t count = (size + (SLAB_SIZE - 1)) / SLAB_SIZE;
    ptrdiff_t page = ((uint8_t*) ptr - c->block) / SLAB_SIZE;

    FOREACH_N(i, page, page + count) {
        Slab* s = &c->slabs[i];

        if (s->active_protect != s->protect) {
            s->active_protect = s->protect;
            tb_platform_vprotect(c->block + (i * SLAB_SIZE), SLAB_SIZE, TB_PAGE_READEXECUTE);
        }
    }
}

// locks memory it allocates
static void* tb_jitheap_alloc_region(TB_JITHeap* c, size_t s, TB_MemProtect protect) {
    // align to alloc granularity
    size_t block_count = (s + 15) / 16;
    assert(block_count < 64 && "TODO: support bigger allocations");

    FOREACH_N(i, 0, c->slab_count) {
        Slab* restrict s = &c->slabs[i];
        if (s->protect != TB_PAGE_INVALID) {
            if (s->protect != protect) continue;
        } else {
            s->protect = protect;
            s->active_protect = TB_PAGE_INVALID;
        }

        // by default, pages are mapped as writable until the user finalizes their changes
        if (s->active_protect != TB_PAGE_READWRITE) {
            s->active_protect = TB_PAGE_READWRITE;
            tb_platform_vprotect(c->block + (i * SLAB_SIZE), SLAB_SIZE, TB_PAGE_READWRITE);
        }

        // find first free slot
        uint64_t* bitmap = s->used_bitmap;
        size_t j = 0;
        for (; j < USED_BITMAP_COUNT; j++) {
            if (bitmap[j] != UINT64_MAX) break;
        }

        // find empty bit
        assert(j != USED_BITMAP_COUNT);
        uint64_t bits = bitmap[j];

        size_t k = bits ? tb_ffs64(~bits) - 1 : 0;
        if (k + block_count >= 64) {
            // goes across uint64 chunks
            tb_todo();
        } else {
            uint64_t mask = ((1u << block_count) - 1) << k;
            if ((bits & mask) == 0) {
                // it's free
                bitmap[j] |= mask;

                // printf("Allocated to [%zu][%zu]\n", i, j*64 + k);
                return c->block + (i * SLAB_SIZE) + (j * 64 * BITMAP_GRANULARITY) + (k * BITMAP_GRANULARITY);
            }
        }
    }

    return NULL;
}

void tb_jitheap_free_region(TB_JITHeap* c, void* ptr, size_t s) {
    ptrdiff_t offset = ((uint8_t*) ptr) - c->block;
    assert(offset >= 0 && offset < c->capacity);

    size_t block_count = (s + 15) / 16;
    assert(block_count < 64 && "TODO: bigger freeing operations");

    size_t slab_id = (offset / SLAB_SIZE);
    size_t bitmap_id = offset % USED_BITMAP_COUNT;
    size_t bit_id = (offset / BITMAP_GRANULARITY) % 64;

    uint64_t mask = ((1u << block_count) - 1) << bit_id;
    c->slabs[slab_id].used_bitmap[bitmap_id] &= ~mask;
}

TB_API void* tb_module_apply_function(TB_JITContext* jit, TB_Function* f) {
    TB_FunctionOutput* out_f = f->output;

    HINSTANCE crt_dll;
    GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS|GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT, NULL, &crt_dll);

    // copy machine code
    char* dst = tb_jitheap_alloc_region(&jit->heap, out_f->code_size, TB_PAGE_READEXECUTE);
    memcpy(dst, out_f->code, out_f->code_size);

    // apply relocations, any leftovers are mapped to thunks
    for (TB_SymbolPatch* p = f->last_patch; p; p = p->prev) {
        if (p->internal) continue;

        size_t actual_pos = out_f->prologue_length + p->pos;
        if (p->target->tag == TB_SYMBOL_FUNCTION || p->target->tag == TB_SYMBOL_EXTERNAL) {
            void* addr = GetProcAddress(crt_dll, p->target->name);
            ptrdiff_t rel = (intptr_t)addr - (intptr_t)dst;
            int32_t rel32 = rel;
            if (rel == rel32) {
                memcpy(dst + actual_pos, &rel32, sizeof(ptrdiff_t));
            } else {
                // generate thunk to make far call
                char* thunk = tb_jitheap_alloc_region(&jit->heap, 6 + sizeof(void*), TB_PAGE_READEXECUTE);
                thunk[0] = 0xFF; // JMP addr
                thunk[1] = 0x25;
                thunk[2] = 0x00;
                thunk[3] = 0x00;
                thunk[4] = 0x00;
                thunk[5] = 0x00;

                // write final address into the thunk
                memcpy(thunk + 6, &addr, sizeof(void*));

                int32_t* patch = (int32_t*) &dst[actual_pos];
                int32_t rel32 = (intptr_t)thunk - ((intptr_t)patch + 4);
                *patch += rel32;
            }
        } else if (p->target->tag == TB_SYMBOL_GLOBAL) {
            TB_Global* g = (TB_Global*) p->target;
            if (g->address == NULL) {
                // lazy init globals
                tb_module_apply_global(jit, g);
            }

            int32_t* patch = (int32_t*) &dst[actual_pos];
            int32_t rel32 = (intptr_t)g->address - ((intptr_t)patch + 4);
            *patch += rel32;
        } else {
            tb_todo();
        }
    }

    f->compiled_pos = dst;
    return dst;
}

static void* get_symbol_address(const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_GLOBAL) {
        return ((TB_Global*) s)->address;
    } else if (s->tag == TB_SYMBOL_FUNCTION) {
        return ((TB_Function*) s)->compiled_pos;
    } else {
        tb_todo();
    }
}

TB_API void* tb_module_apply_global(TB_JITContext* jit, TB_Global* g) {
    char* data = tb_jitheap_alloc_region(&jit->heap, g->size, TB_PAGE_READEXECUTE);

    memset(data, 0, g->size);
    FOREACH_N(k, 0, g->obj_count) {
        if (g->objects[k].type == TB_INIT_OBJ_REGION) {
            memcpy(&data[g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
        }
    }

    FOREACH_N(k, 0, g->obj_count) {
        if (g->objects[k].type == TB_INIT_OBJ_RELOC) {
            uintptr_t addr = (uintptr_t) get_symbol_address(g->objects[k].reloc);

            uintptr_t* dst = (uintptr_t*) &data[g->objects[k].offset];
            *dst += addr;
        }
    }

    g->address = data;
    return data;
}

TB_API void tb_module_ready_jit(TB_JITContext* jit) {
    FOREACH_N(i, 0, jit->heap.slab_count) {
        Slab* restrict s = &jit->heap.slabs[i];

        if (s->active_protect != s->protect) {
            s->active_protect = s->protect;
            tb_platform_vprotect(jit->heap.block + (i * SLAB_SIZE), SLAB_SIZE, s->protect);
        }
    }
}

TB_API TB_JITContext* tb_module_begin_jit(TB_Module* m, size_t jit_heap_capacity) {
    if (jit_heap_capacity == 0) jit_heap_capacity = 4*1024*1024;
    // ICodeGen* restrict codegen = tb__find_code_generator(m);

    TB_JITContext* jit = tb_platform_heap_alloc(sizeof(TB_JITContext));
    jit->heap = tb_jitheap_create(jit_heap_capacity);

    // just a bunch of int3
    jit->null_thunk = tb_jitheap_alloc_region(&jit->heap, 16, TB_PAGE_READEXECUTE);
    memset(jit->null_thunk, 0xCC, 16);
    tb_jitheap_unlock(&jit->heap, jit->null_thunk, 16);

    return jit;
}

TB_API void tb_module_end_jit(TB_JITContext* jit) {
    tb_platform_vfree(jit->heap.block, jit->heap.capacity);
    tb_platform_heap_free(jit);
}
