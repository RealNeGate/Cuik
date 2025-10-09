
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
    uint32_t live, used;

    // Acceleration structure for finding
    // allocations from interior ptrs.
    uint64_t alloc_bits[GC_ALLOC_BITMAP_SIZE / 64];

    // Card table per page
    uint8_t card_table[GC_PAGE_SIZE / 512];

    char data[];
} GC_Page;

// Pages added which won't be marked this cycle
static _Atomic(GC_Page*) gc_fresh_pages;
static NL_HashSet gc_all_pages;

// TLAB (thread-local allocation buffer)
static thread_local GC_Page* gc_tlab;
static GC_Ref gc_alloc(size_t size, uint32_t tag) {
    size += sizeof(GC_Object);
    // should be aligned to 8B by default
    assert((size % 8) == 0);
    // only support small space allocs for now
    assert(size < 64*1024);

    bool nmt = nmt_target;
    if (gc_tlab == NULL || gc_tlab->used + size >= GC_PAGE_SIZE - sizeof(GC_Page)) {
        GC_Page* new_page = gc_tlab = alloc_page();
        printf("[GC] Alloc Page: %p\n", new_page);

        // append to fresh list
        GC_Page* head = atomic_load_explicit(&gc_fresh_pages, memory_order_relaxed);
        do {
            new_page->next = head;
        } while (atomic_compare_exchange_weak(&gc_fresh_pages, &head, new_page));
    }

    // submit to alloc bitmap
    uintptr_t offset = gc_tlab->used / 8;
    gc_tlab->alloc_bits[offset / 64] |= 1ull << (offset % 64);

    // bump alloc
    GC_Object* obj = (GC_Object*) &gc_tlab->data[gc_tlab->used];
    gc_tlab->used += size;
    obj->tag  = tag;
    obj->size = size;

    GC_Ref ref = (((GC_Ref) obj->data) << 4ull) | (GC_SPACE_YOUNG<<2ull) | nmt;
    printf("[GC] Alloc object: %016"PRIXPTR"\n", ref);
    return ref;
}

static void* gc_rawptr(GC_Ref ref) {
    return (void*) (ref >> 4ull);
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
    if (i == start/64) {
        // mask bits above the mid point
        word &= UINT64_MAX >> (64 - (start % 64));
    }
    TB_ASSERT(word);

    size_t offset = __builtin_clzll(word) ^ 63;
    return (GC_Object*) &page->data[i*512 + offset*8];
}

////////////////////////////////
// GC Main
////////////////////////////////
static uintptr_t bit_test(uintptr_t x, uintptr_t y) {
    return (x >> y) & 1;
}

static GC_Marklist marklist_n;
GC_Ref c_lvb(_Atomic(GC_Ref)* ref) {
    GC_Ref old = *ref;

    GC_Ref new = old;
    GC_Ref addr = old >> 4ull;

    GC_Ref exp = *expected_nmt;
    if (bit_test(exp, old & 63)) {
        uint64_t space    = (old >> 2) & 3;
        uint64_t expected = __builtin_ctzll(~(exp >> (space*4)) & 0xF);

        // FLIP NMT+PROT
        new ^= expected;

        // mark object, if it's not marked yet push to marklist
        if (heap_bitmap_set(&mark_bitmap, addr)) {
            void* base = gc_find_obj(addr);
            printf("[GC]   mark obj: %p\n", base);

            gc_marklist_push(&marklist_n, base);
        }
    }

    // self-heal, might fail due to a fellow writer
    atomic_compare_exchange_strong(ref, &old, new);
    return new;
}

static GC_Marklist marklist_gc;
static void gc_cross_checkpoint(bool mark_roots) {
    gc_checkpoint_time = 0;
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

    printf("[GC] time in checkpoints: %.4f ms\n", gc_checkpoint_time / 1000000.0);
}

// traps if bits don't match pattern, NULL space never traps
static void set_expected_nmt(int space, int prot, int nmt) {
    uint64_t pattern  = (space << 2) | (prot << 1) | nmt;
    uint64_t expected = ~((1ull << pattern) | 0b1111);

    // we're matching 4bits, that means there
    atomic_store_explicit(expected_nmt, expected * 0x0001000100010001, memory_order_release);
}

static int gc_main(void* arg) {
    marklist_gc = gc_marklist_init();
    fwd_table = nl_table_alloc(4096);
    gc_all_pages = nl_hashset_alloc(256);

    bool prot_expected = false;
    for (;;) {
        ////////////////////////////////
        // Mark/Remap
        ////////////////////////////////
        {
            // No LVB traps are being hit right now, it's the perfect time to reset the bitmap
            heap_bitmap_clear(&mark_bitmap);

            // Flip NMT, the LUT will trap if NMT is nmt_dir.
            // This event might not be acknowledged immediately by
            // mutator threads which leads to some NMT "throbbing"
            nmt_target = !nmt_target;
            set_expected_nmt(GC_SPACE_YOUNG, prot_expected, nmt_target);
            printf("\n[GC] Flip NMT! %d\n", nmt_target);

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
                        printf("[GC] mark %p\n", obj);
                    }

                    if (added) {
                        progress = true;
                    }
                } while (added);

                mark_time = cuik_time_in_nanos() - mark_time;
                printf("[GC] marked all!\n");
            } while (progress);

            // Acknowledge any fresh pages, we do this late to avoid accidentally missing
            // newly allocated pages. Technically we can miss them, they'll just be handled next
            // cycle but that's lame :p
            GC_Page* list = atomic_exchange(&gc_fresh_pages, NULL);
            for (; list; list = list->next) {
                nl_hashset_put(&gc_all_pages, list);
            }
        }

        ////////////////////////////////
        // Relocate
        ////////////////////////////////
        // find free space
        nl_table_clear(&fwd_table);
        nl_hashset_for(e, &gc_all_pages) {
            GC_Page* page = *e;
            printf("PAGE: %p\n", page);

            GC_Object* obj   = (GC_Object*) page->data;
            GC_Object* limit = (GC_Object*) (page->data + page->used);
            while (obj != limit) {
                printf("  OBJ: %p (%s)\n", obj, heap_bitmap_test(&mark_bitmap, (uintptr_t) obj) ? "LIVE" : "DEAD");
                obj += obj->size / sizeof(GC_Object);
            }
        }

        // Trap on wrong-PROT
        prot_expected = !prot_expected;
        set_expected_nmt(GC_SPACE_YOUNG, prot_expected, nmt_target);

        // before the relocation can actually happen, we need to
        // make sure all threads acknowledge the NMT flip... they'll
        // LVB storm up for a bit but we'll tank
        gc_mid_reloc = 1;
        gc_cross_checkpoint(false);
        gc_mid_reloc = 0;

        // sleep 500ms
        thrd_sleep(&(struct timespec){ .tv_nsec = 500000000 }, NULL);
    }
}
