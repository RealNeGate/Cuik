#include "../tb_internal.h"
#include "../host.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define NL_STRING_MAP_INLINE
#define NL_STRING_MAP_IMPL
#include <string_map.h>

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
    NL_Strmap(void*) loaded_funcs;

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
        h.slabs[i] = (Slab){ 0 };
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
            tb_platform_vprotect(c->block + (i * SLAB_SIZE), SLAB_SIZE, s->protect);
        }
    }
}

static bool find_free_sequence(uint64_t* bitmap, size_t bitmap_cap, size_t bits_needed, size_t* out_i) {
    size_t i = 0;

    // biggest bit sequence
    size_t start_bit  = 0;
    size_t bit_length = 0;

    for (; i < bitmap_cap; i++) {
        uint64_t bits = bitmap[i];
        size_t free_bit_local = (bits ? tb_ffs64(~bits) - 1 : 0);
        size_t free_bit = i*64 + free_bit_local;

        // after the first free bit, we need to find out how long it is
        uint64_t remainder = bits >> free_bit_local;
        size_t seq_len = remainder ? tb_ffs64(remainder) - 1 : 64 - free_bit_local;

        // is it connected to the old bit sequence
        if (start_bit + bit_length == free_bit) {
            bit_length += seq_len;
        } else {
            // restart sequence
            start_bit = free_bit;
            bit_length = seq_len;
        }

        if (bit_length >= bits_needed) {
            // fill entire bit sequence
            size_t start_word = start_bit / 64;
            size_t end_word = (start_bit + bits_needed + 63) / 64;

            if (start_word + 1 == end_word) {
                size_t start_bit_local = start_bit % 64;
                uint64_t mask = (UINT64_C(1) << bits_needed) - 1;
                bitmap[start_word] |= mask << start_bit_local;

                *out_i = start_bit;
                return true;
            } else {
                // extend from start bit to the end of the word
                size_t start_bit_local = start_bit % 64;
                uint64_t mask = UINT64_MAX << start_bit_local;
                bitmap[start_word] |= mask;

                FOREACH_N(j, start_word + 1, end_word - 1) {
                    bitmap[j] |= UINT64_MAX;
                }

                size_t end_bit_local = (start_bit + bits_needed) % 64;
                mask = UINT64_MAX >> (64 - end_bit_local);
                bitmap[end_word - 1] |= mask;

                *out_i = start_bit;
                return true;
            }
        }
    }

    return false;
}

// locks memory it allocates
static void* tb_jitheap_alloc_region(TB_JITHeap* c, size_t size, TB_MemProtect protect) {
    // align to alloc granularity
    size_t block_count = (size + 15) / 16;
    assert(block_count < 64 && "TODO: support bigger allocations");

    // printf("JIT: allocate %zu blocks\n", block_count);

    FOREACH_N(i, 0, c->slab_count) {
        Slab* restrict s = &c->slabs[i];
        if (s->protect != TB_PAGE_INVALID) {
            if (s->protect != protect) continue;
        } else {
            s->protect = protect;
            s->active_protect = TB_PAGE_INVALID;
        }

        // by default, pages are mapped as writable until the user finalizes their changes
        uint8_t* page = c->block + (i * SLAB_SIZE);
        if (s->active_protect != TB_PAGE_READWRITE) {
            s->active_protect = TB_PAGE_READWRITE;
            tb_platform_vprotect(page, SLAB_SIZE, TB_PAGE_READWRITE);
        }

        uint64_t* bitmap = s->used_bitmap;
        size_t bitmap_count = (block_count + 63) / 64;

        // find first free slot
        size_t j = 0;
        while (j < USED_BITMAP_COUNT && bitmap[j] == UINT64_MAX) j++;

        // find empty bit sequence
        assert(j != USED_BITMAP_COUNT);

        size_t k;
        if (find_free_sequence(&bitmap[j], USED_BITMAP_COUNT - j, block_count, &k)) {
            // printf("  alloc [%zu][%zu][%zu] (%zu blocks)\n", i, j, k, block_count);
            return &page[((j * 64) + k) * BITMAP_GRANULARITY];
        }

        tb_todo();
    }

    return NULL;
}

void tb_jitheap_free_region(TB_JITHeap* c, void* ptr, size_t s) {
    size_t offset = ((uint8_t*) ptr) - c->block;
    assert(offset < c->capacity);

    size_t start_bit = offset / 16;
    size_t block_count = (s + 15) / 16;
    Slab* restrict slab = &c->slabs[offset / SLAB_SIZE];

    // extend from start bit to the end of the word
    size_t start_word = start_bit / 64;
    size_t end_word = (start_bit + block_count + 63) / 64;

    size_t start_bit_local = start_bit % 64;
    uint64_t mask = UINT64_MAX << start_bit_local;
    slab->used_bitmap[start_word] &= ~mask;

    FOREACH_N(j, start_word + 1, end_word - 1) {
        slab->used_bitmap[j] = 0;
    }

    size_t end_bit_local = (start_bit + block_count) % 64;
    mask = UINT64_MAX >> (64 - end_bit_local);
    slab->used_bitmap[end_word - 1] &= ~mask;
}

static void* get_proc(TB_JITContext* jit, const char* name) {
    static HMODULE kernel32, user32, gdi32, opengl32;
    if (user32 == NULL) {
        kernel32 = LoadLibrary("kernel32.dll");
        user32   = LoadLibrary("user32.dll");
        gdi32    = LoadLibrary("gdi32.dll");
        opengl32 = LoadLibrary("opengl32.dll");
    }

    // check cache first
    ptrdiff_t search = nl_strmap_get_cstr(jit->loaded_funcs, name);
    if (search >= 0) return jit->loaded_funcs[search];

    void* addr = GetProcAddress(NULL, name);
    if (addr == NULL) addr = GetProcAddress(kernel32, name);
    if (addr == NULL) addr = GetProcAddress(user32, name);
    if (addr == NULL) addr = GetProcAddress(gdi32, name);
    if (addr == NULL) addr = GetProcAddress(opengl32, name);

    // printf("JIT: loaded %s (%p)\n", name, addr);
    nl_strmap_put_cstr(jit->loaded_funcs, name, addr);
    return addr;
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

TB_API void* tb_module_apply_function(TB_JITContext* jit, TB_Function* f) {
    TB_FunctionOutput* out_f = f->output;

    // copy machine code
    char* dst = tb_jitheap_alloc_region(&jit->heap, out_f->code_size, TB_PAGE_READEXECUTE);
    memcpy(dst, out_f->code, out_f->code_size);

    // printf("JIT: apply function %s (%p)\n", f->super.name, dst);

    // apply relocations, any leftovers are mapped to thunks
    for (TB_SymbolPatch* p = f->last_patch; p; p = p->prev) {
        size_t actual_pos = out_f->prologue_length + p->pos;
        enum TB_SymbolTag tag = p->target->tag;

        int32_t* patch = (int32_t*) &dst[actual_pos];
        if (tag == TB_SYMBOL_FUNCTION) {
            TB_Function* f = (TB_Function*) p->target;
            void* addr = f->compiled_pos;
            if (addr == NULL) {
                addr = tb_module_apply_function(jit, f);
            }

            int32_t rel32 = (intptr_t)addr - ((intptr_t)patch + 4);
            *patch += rel32;
        } else if (tag == TB_SYMBOL_EXTERNAL) {
            void* addr = get_proc(jit, p->target->name);
            if (addr == NULL) {
                __debugbreak();
            }

            ptrdiff_t rel = (intptr_t)addr - ((intptr_t)patch + 4);
            int32_t rel32 = rel;
            if (rel == rel32) {
                memcpy(dst + actual_pos, &rel32, sizeof(int32_t));
            } else {
                // generate thunk to make far call
                char* thunk = tb_jitheap_alloc_region(&jit->heap, 6 + sizeof(void*), TB_PAGE_READEXECUTE);
                thunk[0] = 0xFF; // jmp qword [rip]
                thunk[1] = 0x25;
                thunk[2] = 0x00;
                thunk[3] = 0x00;
                thunk[4] = 0x00;
                thunk[5] = 0x00;

                // write final address into the thunk
                memcpy(thunk + 6, &addr, sizeof(void*));

                int32_t rel32 = (intptr_t)thunk - ((intptr_t)patch + 4);
                *patch += rel32;
            }
        } else if (tag == TB_SYMBOL_GLOBAL) {
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

TB_API void* tb_module_apply_global(TB_JITContext* jit, TB_Global* g) {
    // printf("JIT: apply global %s\n", g->super.name ? g->super.name : "<unnamed>");
    char* data = tb_jitheap_alloc_region(&jit->heap, g->size, TB_PAGE_READWRITE);

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
    *jit = (TB_JITContext){ .heap = tb_jitheap_create(jit_heap_capacity) };

    return jit;
}

TB_API void tb_module_end_jit(TB_JITContext* jit) {
    tb_platform_vfree(jit->heap.block, jit->heap.capacity);
    tb_platform_heap_free(jit);
}
