#include "../tb_internal.h"
#include "../host.h"

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

                printf("Allocated to [%zu][%zu]\n", i, j*64 + k);
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

    // copy machine code
    void* dst = tb_jitheap_alloc_region(&jit->heap, out_f->code_size, TB_PAGE_READEXECUTE);
    memcpy(dst, out_f->code, out_f->code_size);

    // apply relocations, any leftovers are mapped to thunks
    // __debugbreak();
    return dst;
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

    #if 0
    /*TB_JITHeap heap = tb_jitheap_create(4*1024*1024);
    tb_jitheap_alloc_region(&heap, 256, true);
    void* a = tb_jitheap_alloc_region(&heap, 42, true);
    tb_jitheap_alloc_region(&heap, 81, false);
    tb_jitheap_alloc_region(&heap, 10, true);
    tb_jitheap_free_region(&heap, a, 42);
    tb_jitheap_alloc_region(&heap, 10, true);
    __debugbreak();*/

    size_t page_size = 4096;
    size_t text_section_size = tb_helper_get_text_section_layout(m, 0);

    // Target specific: resolve internal call patches
    codegen->emit_call_patches(m);

    size_t rdata_section_size = align_up(m->rdata_region_size, page_size);

    size_t external_count = 0;
    FOREACH_N(i, 0, m->max_threads) {
        external_count += pool_popcount(m->thread_info[i].externals);
    }
    rdata_section_size += align_up(external_count * sizeof(void*), page_size);

    typedef struct {
        size_t offset;
        size_t size;
        TB_MemProtect protect;
    } Section;

    enum {
        S_RDATA, S_TEXT, S_DATA
    };

    int section_count = 3;
    Section sections[] = {
        [S_RDATA] = { .size = rdata_section_size,  .protect = TB_PAGE_READONLY    }, // .rdata
        [S_TEXT]  = { .size = text_section_size,   .protect = TB_PAGE_READEXECUTE }, // .text
        [S_DATA]  = { .size = m->data_region_size, .protect = TB_PAGE_READWRITE   }, // .data
    };

    // Layout sections
    size_t jit_region_size = 0;
    FOREACH_N(i, 0, section_count) {
        sections[i].offset = jit_region_size;
        jit_region_size = align_up(jit_region_size + sections[i].size, page_size);
    }
    uint8_t* jit_region = tb_platform_valloc(jit_region_size);

    // .RDATA
    size_t write_pos = 0;
    {
        write_pos = tb_helper_write_rodata_section(write_pos, m, jit_region, sections[S_RDATA].offset);

        // last region is a jump table
        uint8_t* import_table = jit_region + align_up(m->rdata_region_size, page_size);
        size_t count = 0;
        FOREACH_N(i, 0, m->max_threads) {
            pool_for(TB_External, ext, m->thread_info[i].externals) {
                // replace the ext->address with the jump table
                void* old = ext->super.address;
                void* new = import_table + (count * sizeof(void*));

                memcpy(new, old, sizeof(void*));
                ext->super.address = new;
                count += 1;
            }
        }

        write_pos += count * sizeof(void*);
    }

    // .TEXT
    {
        uint8_t* text_section = jit_region + sections[S_TEXT].offset;
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;
            if (out_f != NULL) {
                f->compiled_pos = &text_section[out_f->code_pos];
                memcpy(&text_section[out_f->code_pos], out_f->code, out_f->code_size);
            }
        }

        // Emit external patches
        // These have dealt with the jump table so none of our relocations should
        // cross the 2GB limit.
        FOREACH_N(i, 0, m->max_threads) {
            dyn_array_for(j, m->thread_info[i].symbol_patches) {
                TB_SymbolPatch* p = &m->thread_info[i].symbol_patches[j];

                if (p->target->tag == TB_SYMBOL_EXTERNAL) {
                    TB_FunctionOutput* out_f = p->source->output;

                    size_t actual_pos = out_f->code_pos + out_f->prologue_length + p->pos + 4;

                    ptrdiff_t displacement = (uint8_t*)p->target->address - &text_section[actual_pos];
                    int32_t disp32 = displacement;

                    assert(displacement == disp32);
                    memcpy(&text_section[actual_pos], &disp32, sizeof(disp32));
                }
            }
        }
    }

    // .DATA
    write_pos = tb_helper_write_data_section(sections[S_DATA].offset, m, jit_region, sections[S_DATA].offset);
    FOREACH_N(i, 0, section_count) {
        tb_platform_vprotect(jit_region + sections[i].offset, sections[i].size, sections[i].protect);
    }

    m->jit_region_size = jit_region_size;
    m->jit_region = jit_region;
    #endif
}

TB_API void tb_module_end_jit(TB_JITContext* jit) {
    tb_platform_vfree(jit->heap.block, jit->heap.capacity);
    tb_platform_heap_free(jit);
}

