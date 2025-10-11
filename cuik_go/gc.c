
enum {
    GC_PAGE_SIZE = 2*1024*1024,
    // how many bits are in the alloc bitmap (a bitmap
    // that tracks the first word of an allocation).
    GC_ALLOC_BITMAP_SIZE = GC_PAGE_SIZE / 8,
};

typedef _Atomic uint64_t atomic_word;

atomic_word gc_mid_reloc;
NL_Table fwd_table;

static atomic_bool nmt_target;
static atomic_word* expected_nmt;
static atomic_word gc_checkpoint_trigger;
static atomic_word gc_checkpoint_time;

static void* alloc_page(void) {
    #ifdef _WIN32
    #if NTDDI_VERSION >= NTDDI_WIN10_RS4
    // natural alignment stack because it makes it easy to always find
    // the base.
    MEM_EXTENDED_PARAMETER param = {
        .Type = MemExtendedParameterAddressRequirements,
        .Pointer = &(MEM_ADDRESS_REQUIREMENTS){ .Alignment = GC_PAGE_SIZE }
    };

    return VirtualAlloc2(GetCurrentProcess(), NULL, GC_PAGE_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE, &param, 1);
    #else
    return NULL;
    // return VirtualAlloc(NULL, GC_PAGE_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    #endif /* NTDDI_VERSION >= NTDDI_WIN10_RS4 */
    #else
    return mmap(NULL, GC_PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    #endif
}

// Bottom 3 bits: SSN
//
// (S)pace bits, (N)MT
//
// Possible space options:
//   00 - Null/C ptr
//   01 - Young gen
//   10 - Old gen
//   11 - Unused
typedef uintptr_t GC_Ref;

enum {
    GC_SPACE_NULL  = 0b00,
    GC_SPACE_YOUNG = 0b01,
    GC_SPACE_OLD   = 0b10,
};

typedef struct {
    GC_Ref base;
    size_t len;
    size_t cap;
} Slice;

////////////////////////////////
// GC logging
////////////////////////////////
typedef struct {
    uint64_t type : 8;
    uint64_t time : 56;
    uint64_t x, y;
} GC_LogEntry;

typedef enum GC_LogEnum {
    GC_LOG_MARK_PHASE,
    GC_LOG_RELOCATE_PHASE,

    GC_LOG_CROSS_CHECKPOINT,
    GC_LOG_PHASE_SHIFT,
    GC_LOG_CLEAR_MARK,
    GC_LOG_CHECKPOINT,

    GC_LOG_ALLOC_PAGE,
    GC_LOG_RECYCLE_PAGE,
    GC_LOG_FREE_PAGE,
    GC_LOG_KILL_PAGE,
    GC_LOG_UNSEE_PAGE,
    GC_LOG_SEE_PAGE,
    GC_LOG_COMPACT_PAGE,

    GC_LOG_LVB,
    GC_LOG_ALLOC_OBJ,
    GC_LOG_MARK_OBJ,
    GC_LOG_REMAP_OBJ,
    GC_LOG_FWD_OBJ,
    GC_LOG_COPY_OBJ,
} GC_LogEnum;

static const char* gc_log_fmts[] = {
    "==== MARK/REMAP ====",
    "===== RELOCATE =====",

    "cross",
    "phase-shift: PROT=%zu, NMT=%zu",
    "clear mark",
    "checkpoint",

    "alloc page %p",
    "recycle page %p",
    "free page %p",
    "kill page %p",
    "unsee page %p",
    "see page %p",
    "compact page %p",

    "alloc %p",
    "LVB   %p",
    "mark  %p",
    "remap %p => %p",
    "fwd   %p",
    "copy  %p (%zu bytes)",
};

GC_LogEnum keep_alive;

thread_local size_t entry_tail;
thread_local GC_LogEntry entries[256];

static void gc_log_flush(void) {
    #if _WIN32
    int tid = GetCurrentThreadId();
    #else
    int tid = getpid();
    #endif

    int head = entry_tail > 255 ? ((entry_tail+1) % 256) : 0;
    int tail = entry_tail;
    for (int i = head; i != tail; i = (i + 1) % 256) {
        printf("%-5d,%-10.5f,", tid, entries[i].time / 1000000000.0);
        printf(gc_log_fmts[entries[i].type], entries[i].x, entries[i].y);
        printf("\n");
    }
    entry_tail = 0;
}

static void gc_log_submit(int type, uint64_t x, uint64_t y) {
    #if 0
    uint64_t t = cuik_time_in_nanos();
    int i = entry_tail++ % 256;
    entries[i] = (GC_LogEntry){ type, t, x, y };
    // log_debug(gc_log_fmts[type], x, y);
    #endif
}

////////////////////////////////
// Mark bitmap
////////////////////////////////
// 256GiB max heap size
typedef struct {
    // 2MiB chunk of bits which spans 128MiB of heap space
    //   uint64_t[262144][2048]
    _Atomic(atomic_word*) chunks[2048];
} HeapBitmap;

static HeapBitmap mark_bitmap;

static void heap_bitmap_clear(HeapBitmap* bitmap) {
    FOR_N(i, 0, 2048) {
        if (bitmap->chunks[i] == NULL) {
            continue;
        }
        memset(bitmap->chunks[i], 0, 2*1024*1024);
    }
}

static bool heap_bitmap_set(HeapBitmap* bitmap, uintptr_t addr) {
    size_t chunk_i = (addr >> 27) & 0x7FF;   // 2^11
    size_t word_i  = (addr >> 9)  & 0x3FFFF; // 2^18
    size_t bit_i   = (addr >> 3)  & 0x3F;    // 2^3

    atomic_word* chunk = atomic_load_explicit(&bitmap->chunks[chunk_i], memory_order_acquire);
    if (chunk == NULL) {
        atomic_word* new_chunk = alloc_page();
        if (atomic_compare_exchange_strong(&bitmap->chunks[chunk_i], &chunk, new_chunk)) {
            chunk = new_chunk;
        }
    }

    uint64_t curr = atomic_load_explicit(&chunk[word_i], memory_order_acquire);
    // Probably a contention nightmare
    for (;;) {
        // already set? just return
        if ((curr >> bit_i) & 1) {
            return false;
        }

        uint64_t next = curr | (1ull << bit_i);
        if (atomic_compare_exchange_weak(&chunk[word_i], &curr, next)) {
            return true;
        }
    }
}

static bool heap_bitmap_test(HeapBitmap* bitmap, uintptr_t addr) {
    size_t chunk_i = (addr >> 27) & 0x7FF;   // 2^11
    size_t word_i  = (addr >> 9)  & 0x3FFFF; // 2^18
    size_t bit_i   = (addr >> 3)  & 0x3F;    // 2^3

    atomic_word* chunk = atomic_load_explicit(&bitmap->chunks[chunk_i], memory_order_acquire);
    if (chunk == NULL) {
        return false;
    }

    uint64_t curr = atomic_load_explicit(&chunk[word_i], memory_order_acquire);
    return (curr >> bit_i) & 1;
}

////////////////////////////////
// Marklist
////////////////////////////////
typedef struct {
    size_t size;
    void* buffer[];
} GC_RingBuffer;

typedef struct {
    atomic_int top;
    atomic_int bottom;
    _Atomic(GC_RingBuffer*) ring;
} GC_Marklist;

static GC_Marklist gc_marklist_init(void) {
    GC_RingBuffer* ring = cuik_calloc(1, sizeof(GC_RingBuffer) + 4096*sizeof(void*));
    ring->size = 4096;
    return (GC_Marklist){ 0, 0, ring };
}

void gc_marklist_push(GC_Marklist* list, void* val) {
    int bot                 = atomic_load_explicit(&list->bottom, memory_order_relaxed);
    int top                 = atomic_load_explicit(&list->top,    memory_order_acquire);
    GC_RingBuffer* cur_ring = atomic_load_explicit(&list->ring,   memory_order_relaxed);

    int size = bot - top;
    if (size > (cur_ring->size - 1)) {
        // Queue is full, resize
        size_t new_cap = cur_ring->size * 2;
        GC_RingBuffer* new_ring = cuik_calloc(1, sizeof(GC_RingBuffer) + new_cap*sizeof(void*));
        new_ring->size = new_cap;
        FOR_N(i, 0, cur_ring->size) {
            new_ring->buffer[i] = cur_ring->buffer[i];
        }

        cur_ring = new_ring;
        atomic_store_explicit(&list->ring, cur_ring, memory_order_release);
    }
    cur_ring->buffer[bot & (cur_ring->size - 1)] = val;

    atomic_thread_fence(memory_order_release);
    atomic_store_explicit(&list->bottom, bot + 1, memory_order_relaxed);
}

void* gc_marklist_pop(GC_Marklist* list) {
    int bot                 = atomic_load_explicit(&list->bottom, memory_order_relaxed) - 1;
    GC_RingBuffer* cur_ring = atomic_load_explicit(&list->ring, memory_order_relaxed);
    atomic_store_explicit(&list->bottom, bot, memory_order_relaxed);
    atomic_thread_fence(memory_order_seq_cst);

    int top = atomic_load_explicit(&list->top, memory_order_relaxed);
    if (top > bot) {
        atomic_store_explicit(&list->bottom, bot + 1, memory_order_relaxed);
        return NULL;
    }

    // Queue is not empty
    void* res = cur_ring->buffer[bot % cur_ring->size];
    if (top == bot) {
        // Only one entry left in queue
        if (!atomic_compare_exchange_strong_explicit(&list->top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
            // Race failed
            atomic_store_explicit(&list->bottom, bot + 1, memory_order_relaxed);
            return NULL;
        }

        atomic_store_explicit(&list->bottom, bot + 1, memory_order_relaxed);
    }

    // We got a task without hitting a race
    return res;
}

static char GC_MARKLIST_RACE_FAIL;
void* gc_marklist_steal(GC_Marklist* list) {
    int top = atomic_load_explicit(&list->top, memory_order_acquire);
    atomic_thread_fence(memory_order_seq_cst);
    int bot = atomic_load_explicit(&list->bottom, memory_order_acquire);
    if (top >= bot) {
        return NULL;
    }

    // Queue is not empty
    GC_RingBuffer* cur_ring = atomic_load_explicit(&list->ring, memory_order_consume);
    void* res = cur_ring->buffer[top % cur_ring->size];
    if (!atomic_compare_exchange_strong_explicit(&list->top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
        // Race failed
        return &GC_MARKLIST_RACE_FAIL;
    } else {
        return res;
    }
}

////////////////////////////////
// GC Pages
////////////////////////////////
// header before any GC allocation, there is only one mark bit
// per GC_Object, that means any interior pointers will keep the
// entire thing alive.
typedef struct {
    uint32_t tag;
    uint32_t size;
    char data[];
} GC_Object;

typedef struct GC_Page {
    struct GC_Page* next;

    // we ref count the pages themselves
    _Atomic(uint32_t) refs;
    _Atomic(uint32_t) live;
    uint32_t used;

    // This means the page got moved
    bool from_space;

    // Acceleration structure for finding
    // allocations from interior ptrs.
    uint64_t alloc_bits[GC_ALLOC_BITMAP_SIZE / 64];

    // Card table per page
    uint8_t card_table[GC_PAGE_SIZE / 512];

    char data[];
} GC_Page;

// Mark/Remap can free pages but they can't be
// used that same GC cycle.
static _Atomic(GC_Page*) gc_freed_pages;
static _Atomic(GC_Page*) gc_recycled_pages;

// Pages added which won't be marked this cycle
static _Atomic(GC_Page*) gc_fresh_pages;
static NL_HashSet gc_all_pages;

// TLAB (thread-local allocation buffer)
static thread_local GC_Page* gc_tlab;
static void* gc_rawptr(GC_Ref ref) {
    return (void*) (ref >> 4ull);
}

static GC_Page* gc_alloc_page(void) {
    GC_Page* top = atomic_load_explicit(&gc_recycled_pages, memory_order_relaxed);
    for (;;) {
        if (top == NULL) {
            top = alloc_page();
            gc_log_submit(GC_LOG_ALLOC_PAGE, (uintptr_t) top, 0);
            break;
        }

        if (atomic_compare_exchange_weak(&gc_recycled_pages, &top, top->next)) {
            gc_log_submit(GC_LOG_RECYCLE_PAGE, (uintptr_t) top, 0);
            memset(top, 0, sizeof(GC_Page));
            break;
        }
    }

    // append to fresh list
    GC_Page* head = atomic_load_explicit(&gc_fresh_pages, memory_order_relaxed);
    do {
        top->next = head;
    } while (atomic_compare_exchange_weak(&gc_fresh_pages, &head, top));

    return top;
}

static void gc_free_page(GC_Page* page) {
    gc_log_submit(GC_LOG_FREE_PAGE, (uintptr_t) page, 0);

    // append to be freed
    GC_Page* head = atomic_load_explicit(&gc_freed_pages, memory_order_relaxed);
    do {
        page->next = head;
    } while (!atomic_compare_exchange_weak(&gc_freed_pages, &head, page));
}

static GC_Object* gc_find_obj(uintptr_t addr) {
    GC_Page* page = (GC_Page*) (addr & -GC_PAGE_SIZE);
    size_t start = ((char*)addr - page->data) / 8;

    // skip backwards until we find the next word with bits
    size_t i = start/64;
    while (i && page->alloc_bits[i] == 0) {
        i -= 1;
    }

    uint64_t word = page->alloc_bits[i];
    uint64_t mask = ~(UINT64_MAX << ((start+1) % 64));
    if (i == start/64 && mask != 0) {
        // mask bits above the mid point
        word &= mask;
    }
    TB_ASSERT(word);

    size_t offset = __builtin_clzll(word) ^ 63;
    return (GC_Object*) &page->data[i*512 + offset*8];
}

static GC_Marklist marklist_n;
static GC_Ref gc_alloc(size_t size, uint32_t tag) {
    size += sizeof(GC_Object);
    // should be aligned to 8B by default
    assert((size % 8) == 0);
    // only support small space allocs for now
    assert(size <= 64*1024);

    bool nmt = nmt_target;
    if (gc_tlab == NULL || gc_tlab->used + size >= GC_PAGE_SIZE - sizeof(GC_Page)) {
        gc_tlab = gc_alloc_page();
    }

    // submit to alloc bitmap
    uintptr_t offset = gc_tlab->used / 8;
    gc_tlab->alloc_bits[offset / 64] |= 1ull << (offset % 64);

    // bump alloc
    GC_Object* obj = (GC_Object*) &gc_tlab->data[gc_tlab->used];
    gc_tlab->used += size;
    gc_tlab->live += size;
    obj->tag  = tag;
    obj->size = size;

    // objects begin life marked
    heap_bitmap_set(&mark_bitmap, (uintptr_t) obj);

    GC_Ref ref = (((GC_Ref) obj->data) << 4ull) | (GC_SPACE_YOUNG<<2ull) | nmt;
    gc_log_submit(GC_LOG_ALLOC_OBJ, ref, 0);
    return ref;
}

////////////////////////////////
// GC Main
////////////////////////////////
static uintptr_t bit_test(uintptr_t x, uintptr_t y) {
    return (x >> y) & 1;
}

static void gc_mark_obj(GC_Ref addr) {
    GC_Object* base = gc_find_obj(addr);
    if (heap_bitmap_set(&mark_bitmap, (uintptr_t) base)) {
        gc_log_submit(GC_LOG_MARK_OBJ, (uintptr_t) base, 0);

        GC_Page* page = (GC_Page*) (addr & -GC_PAGE_SIZE);
        size_t live = atomic_fetch_add(&page->live, base->size);
        TB_ASSERT(live + base->size <= page->used);
        gc_marklist_push(&marklist_n, base);
    }
}

static int TO_BE_FORWARDED;
static GC_Ref remap_ptr(GC_Ref addr) {
    _Atomic(uintptr_t)* fwd = (_Atomic(uintptr_t)*) nl_table_getp(&fwd_table, ((GC_Object*) addr) - 1);
    if (fwd != NULL) {
        // if the object is unmoved, do that
        uintptr_t fwd_ptr = atomic_load_explicit(fwd, memory_order_relaxed);

        // try to move it
        if (fwd_ptr == (uintptr_t) &TO_BE_FORWARDED && atomic_compare_exchange_strong(fwd, &fwd_ptr, 1)) {
            // winner must copy
            GC_Object* src_ptr = ((GC_Object*) addr) - 1;
            TB_ASSERT(src_ptr->size <= 64*1024);
            gc_log_submit(GC_LOG_COPY_OBJ, addr, src_ptr->size);

            void* dst_ptr = gc_rawptr(gc_alloc(src_ptr->size, src_ptr->tag));
            memcpy(dst_ptr, src_ptr, src_ptr->size - sizeof(GC_Object));

            // notify the other threads that we've moved
            atomic_store_explicit(fwd, (uintptr_t) dst_ptr, memory_order_release);
            fwd_ptr = (uintptr_t) dst_ptr;

            // once the last object is moved the page, we can recycle it
            GC_Page* page = (GC_Page*) (addr & -GC_PAGE_SIZE);
            if (atomic_fetch_sub(&page->refs, 1) == 1) {
                gc_free_page(page);
            }
        } else if (fwd_ptr == 1) {
            // losers must wait... futex?
            while (fwd_ptr = atomic_load_explicit(fwd, memory_order_acquire), fwd_ptr == 1) {
            }
        }

        gc_log_submit(GC_LOG_REMAP_OBJ, addr, fwd_ptr);
        return fwd_ptr;
    }
    return addr;
}

static GC_Ref visit_ref(GC_Ref old, GC_Ref exp) {
    GC_Ref addr = remap_ptr(old >> 4ull);

    // mark object, if it's not marked yet push to marklist
    gc_mark_obj(addr);

    // transition NMT+PROT to the current good state
    uint64_t space    = (old >> 2) & 3;
    uint64_t expected = ~(exp >> (space*4)) & 0xF;
    uint64_t trigger  = __builtin_ffsll(expected) - 1;
    return (old & -4ull) | trigger;
}

GC_Ref c_lvb(_Atomic(GC_Ref)* ref) {
    GC_Ref old = *ref;
    GC_Ref exp = *expected_nmt;
    gc_log_submit(GC_LOG_LVB, old, 0);

    // Double-check that things didn't stop being a trap
    if (bit_test(exp, old & 63)) {
        GC_Ref new = visit_ref(old, exp);
        // self-heal, might fail due to a fellow writer
        atomic_compare_exchange_strong(ref, &old, new);
        return new;
    }
    return old;
}

static GC_Marklist marklist_gc;
static void gc_cross_checkpoint(bool mark_roots) {
    gc_checkpoint_trigger = 0;

    uint64_t total_checkpoints = 0;
    for (TB_Stacklet* task = sched_g_first(); task; task = sched_g_get(task)->next) {
        Sched_G* g = sched_g_get(task);
        atomic_store_explicit(&g->pause, 1, memory_order_relaxed);
        total_checkpoints += 1;
    }

    // TODO(NeGate): futex?
    uint64_t old;
    while (old = gc_checkpoint_trigger, old != total_checkpoints) {
        // printf("Checkpoint: %"PRId64" %"PRId64"\n", old, total_checkpoints);
    }

    gc_log_submit(GC_LOG_CROSS_CHECKPOINT, 0, 0);
}

// traps if bits don't match pattern, NULL space never traps
static void set_expected_nmt(int space, int prot, int nmt) {
    gc_log_submit(GC_LOG_PHASE_SHIFT, prot, nmt);

    uint64_t pattern  = (space << 2) | (prot << 1) | nmt;
    uint64_t expected = ~((1ull << pattern) | 0b1111) & 0xFFFF;
    atomic_store_explicit(expected_nmt, expected * 0x0001000100010001, memory_order_release);
}

static bool prot_expected = false;
static void gc_mark_remap(void) {
    gc_log_submit(GC_LOG_MARK_PHASE, 0, 0);

    // Flip NMT, the LUT will trap if NMT is nmt_dir.
    // This event might not be acknowledged immediately by
    // mutator threads which leads to some NMT "throbbing"
    nmt_target = !nmt_target;
    set_expected_nmt(GC_SPACE_YOUNG, prot_expected, nmt_target);

    bool progress;
    do {
        progress = false;

        // (1) Checkpoints for local root scan
        gc_cross_checkpoint(true);

        // (2) Mark objects in dirty cards
        // (3) Global root scan

        // Mark & remap recursively
        uint64_t mark_time = cuik_time_in_nanos();
        bool added;
        do {
            added = false;

            // populate GC marklist with the N thread marklists
            void* obj;
            size_t i = 0;
            while (obj = gc_marklist_steal(&marklist_n), obj != NULL && i < 1000) {
                if (obj != &GC_MARKLIST_RACE_FAIL) {
                    gc_marklist_push(&marklist_gc, obj);
                    added = true;
                }
            }

            // walk whatever is on our marklist
            while (obj = gc_marklist_pop(&marklist_gc), obj != NULL) {
                // printf("[GC] mark %p\n", obj);
            }

            if (added) {
                progress = true;
            }
        } while (added);

        mark_time = cuik_time_in_nanos() - mark_time;
    } while (progress);
}

static void gc_relocate(void) {
    gc_log_submit(GC_LOG_RELOCATE_PHASE, 0, 0);

    // make freed pages in the last phase available to the new relocations
    GC_Page* page = atomic_exchange(&gc_freed_pages, NULL);
    while (page) {
        GC_Page* next = page->next;
        gc_log_submit(GC_LOG_UNSEE_PAGE, (uintptr_t) page, 0);
        nl_hashset_remove(&gc_all_pages, page);

        // append to be freed
        GC_Page* head = atomic_load_explicit(&gc_recycled_pages, memory_order_relaxed);
        do {
            page->next = head;
        } while (!atomic_compare_exchange_weak(&gc_recycled_pages, &head, page));
        page = next;
    }

    // if all objects die in a page, no relocation is needed
    // but we'd like to reuse that page now.
    nl_hashset_for(e, &gc_all_pages) {
        GC_Page* page = *e;
        if (page->live == 0) {
            gc_log_submit(GC_LOG_KILL_PAGE, (uintptr_t) page, 0);
            *e = NL_HASHSET_TOMB;
            gc_all_pages.count -= 1;

            page->from_space = true;

            // append to be freed
            GC_Page* head = atomic_load_explicit(&gc_recycled_pages, memory_order_relaxed);
            do {
                page->next = head;
            } while (!atomic_compare_exchange_weak(&gc_recycled_pages, &head, page));
            continue;
        }
    }

    // find free space
    nl_table_clear(&fwd_table);

    GC_Page* to_page = NULL;
    nl_hashset_for(e, &gc_all_pages) {
        GC_Page* page    = *e;
        GC_Object* obj   = (GC_Object*) page->data;
        GC_Object* limit = (GC_Object*) (page->data + page->used);

        float live_space = ((float) page->live / (float) page->used);
        log_debug("[GC] Page %p (%u / %u, %.1f%% live)", page, page->live, page->used, live_space * 100.0f);

        // move objects into the to_page if the page is fragmented enough
        if (live_space < 0.5f) {
            gc_log_submit(GC_LOG_COMPACT_PAGE, (uintptr_t) page, 0);

            size_t refs = 0;
            while (obj != limit) {
                bool obj_live = heap_bitmap_test(&mark_bitmap, (uintptr_t) obj);
                if (obj_live) {
                    gc_log_submit(GC_LOG_FWD_OBJ, (uintptr_t) obj, 0);
                    nl_table_put(&fwd_table, obj, &TO_BE_FORWARDED);
                    refs += 1;
                }
                obj += obj->size / sizeof(GC_Object);
            }
            page->refs = refs;
            page->from_space = true;
        }

        // reset for next GC cycle
        page->live = 0;
    }

    // Trap on wrong-PROT
    prot_expected = !prot_expected;

    // before the relocation can actually happen, we need to
    // make sure all threads acknowledge the NMT flip... they'll
    // LVB storm up for a bit but we'll tank it
    gc_mid_reloc = 1;
    {
        set_expected_nmt(GC_SPACE_YOUNG, prot_expected, nmt_target);
        gc_cross_checkpoint(false);

        // No LVB traps are being hit right now, it's the perfect time to reset the bitmap
        // and recognize any new pages
        GC_Page* list = atomic_exchange(&gc_fresh_pages, NULL);
        for (; list; list = list->next) {
            gc_log_submit(GC_LOG_SEE_PAGE, (uintptr_t) list, 0);

            list->live = 0;
            nl_hashset_put(&gc_all_pages, list);
        }

        heap_bitmap_clear(&mark_bitmap);
        gc_log_submit(GC_LOG_CLEAR_MARK, 0, 0);
    }
    gc_mid_reloc = 0;
}

static int gc_main(void* arg) {
    keep_alive = GC_LOG_COPY_OBJ;

    marklist_gc = gc_marklist_init();
    fwd_table = nl_table_alloc(4096);
    gc_all_pages = nl_hashset_alloc(256);

    for (;;) {
        uint64_t start = cuik_time_in_nanos();

        gc_checkpoint_time = 0;
        gc_mark_remap();
        gc_relocate();
        log_debug("[GC] Cycle took %.4fms (%.4fms blocked)", (cuik_time_in_nanos() - start) / 1000000.0, gc_checkpoint_time / 1000000.0);

        // sleep 100ms
        thrd_sleep(&(struct timespec){ .tv_nsec = 100000000 }, NULL);
    }
}
