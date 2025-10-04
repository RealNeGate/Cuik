
enum {
    GC_PAGE_SIZE = 2*1024*1024
};

static void* alloc_page(void) {
    #ifdef _WIN32
    return VirtualAlloc(NULL, GC_PAGE_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    #else
    return mmap(NULL, GC_PAGE_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    #endif
}

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

typedef struct {
    size_t live, used;
} GC_Page;

static void* gc_page_push(size_t size) {
    return NULL;
}

////////////////////////////////
// GC Main
////////////////////////////////
static atomic_word expected_nmt;

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
                uint64_t expected = 0b010 | !nmt_target;
                // broadcast 8x
                expected *= 0x0101010101010101;
                atomic_store_explicit(&expected_nmt, expected, memory_order_release);
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
