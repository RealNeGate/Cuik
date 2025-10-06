
enum {
    GC_PAGE_SIZE = 2*1024*1024
};

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
typedef _Atomic uint64_t atomic_word;
typedef struct {
    // 2MiB chunk of bits which spans 128MiB of heap space
    //   uint64_t[262144][2048]
    _Atomic(atomic_word*) chunks[2048];
} HeapBitmap;

static HeapBitmap mark_bitmap;

static void heap_bitmap_clear(HeapBitmap* bitmap, uintptr_t addr) {
    FOR_N(i, 0, 2048) {
        if (bitmap->chunks[i] == NULL) {
            continue;
        }

        memset(bitmap->chunks[i], 0, 2*2048*2048);
    }
}

static bool heap_bitmap_mark(HeapBitmap* bitmap, uintptr_t addr) {
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

        atomic_word next = curr | (1ull << bit_i);
        if (atomic_compare_exchange_weak(&chunk[word_i], &curr, next)) {
            return true;
        }
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

typedef struct {
    uint32_t live, used;

    // Acceleration structure for finding
    // allocations from interior ptrs.
    struct {
        uint32_t cap, cnt;
        uint32_t* arr;
    } allocs;

    _Alignas(16) char data[];
} GC_Page;

static void* gc_page_push(size_t size) {
    return NULL;
}

// TLAB (thread-local allocation buffer)
static thread_local GC_Page* gc_tlab;
static GC_Ref gc_alloc(size_t size, uint32_t tag) {
    size += sizeof(GC_Object);
    // should be aligned to 8B by default
    assert((size % 8) == 0);
    // only support small space allocs for now
    assert(size < 64*1024);

    if (gc_tlab == NULL || gc_tlab->used + size >= GC_PAGE_SIZE - sizeof(GC_Page)) {
        gc_tlab = alloc_page();
        gc_tlab->allocs.cap = 1024;
        gc_tlab->allocs.arr = cuik_malloc(1024 * sizeof(uint32_t));
    }

    // submit to sorted alloc records
    if (gc_tlab->allocs.cnt == gc_tlab->allocs.cap) {
        gc_tlab->allocs.cap *= 2;
        gc_tlab->allocs.arr = cuik_realloc(gc_tlab->allocs.arr, gc_tlab->allocs.cap * sizeof(uint32_t));
    }
    gc_tlab->allocs.arr[gc_tlab->allocs.cnt++] = gc_tlab->used;

    // bump alloc
    GC_Object* obj = (GC_Object*) &gc_tlab->data[gc_tlab->used];
    gc_tlab->used += size;
    obj->tag  = tag;
    obj->size = size;
    return ((GC_Ref) obj->data) | (GC_SPACE_YOUNG<<1ull);
}

static void* gc_rawptr(GC_Ref ref) {
    return (void*) (ref & -4ll);
}

static GC_Object* gc_find_obj(uintptr_t addr) {
    GC_Page* page = (GC_Page*) (addr & -GC_PAGE_SIZE);
    uint32_t key  = (char*)addr - page->data;

    size_t left = 0, right = page->allocs.cnt;
    while (left != right) {
        size_t i = (left + right) / 2;
        if (page->allocs.arr[i] >= key) { right = i; }
        else { left = i + 1; }
    }

    key = page->allocs.arr[left];
    return (GC_Object*) &page->data[key];
}

////////////////////////////////
// GC Main
////////////////////////////////
static atomic_word* expected_nmt;

static void gc_main(void* arg) {
    for (;;) {
        ////////////////////////////////
        // Mark/Remap
        ////////////////////////////////
        bool nmt_target = false;
        {
            // Flip NMT, the LUT will trap if NMT is nmt_dir.
            // This event might not be acknowledged immediately by
            // mutator threads which leads to some NMT "throbbing"
            nmt_target = !nmt_target;
            {
                uint64_t expected = !nmt_target << 0b010;
                // broadcast 8x
                expected *= 0x0101010101010101;
                atomic_store_explicit(expected_nmt, expected, memory_order_release);
            }

            bool progress = true;
            do {
                // (1) Checkpoints for local root scan
                // (2) Mark objects in dirty cards
                // (3) Global root scan

                // Mark & remap recursively
            } while (progress);
        }

        ////////////////////////////////
        // Relocate
        ////////////////////////////////
    }
}
