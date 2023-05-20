// These are resources that exist as temporary and thread-local (mostly) so
// we try to reuse memory a lot to avoid OS churn.
enum {
    // C preprocessor
    CUIK_POOL_CPP_STACK,
    CUIK_POOL_CPP_HT_KEYS,
    CUIK_POOL_CPP_HT_VALS,
    CUIK_POOL_CPP_SHTUFF,

    CUIK_POOL_MAX
};

static _Thread_local size_t pool_cap[CUIK_POOL_MAX];
static _Thread_local void*  pool_cache[CUIK_POOL_MAX];
static _Thread_local bool   pool_in_use[CUIK_POOL_MAX];

extern uint64_t cuik__page_size;
extern uint64_t cuik__page_mask;

static void* cuik__pool_alloc(int id, size_t size) {
    if (cuik__page_size == 0) {
        #ifdef _WIN32
        // unsupported... sadge
        SYSTEM_INFO si;
        GetSystemInfo(&si);

        cuik__page_size = si.dwPageSize;
        cuik__page_mask = si.dwPageSize - 1;
        #else
        cuik__page_size = 4096;
        cuik__page_mask = 4096 - 1;
        #endif
    }

    // round size to page size
    size = (size + cuik__page_mask) & ~cuik__page_mask;

    if (pool_in_use[id]) {
        return (pool_cache[id] = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE));
    }

    if (pool_cap[id] >= size) {
        // recommit
        memset(pool_cache[id], 0, size);
        pool_in_use[id] = true;
        return pool_cache[id];
    }

    if (pool_cache[id] == NULL) {
        // allocate for the first time
        pool_cap[id] = size;
        pool_in_use[id] = true;
        return (pool_cache[id] = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE));
    } else {
        // free old
        VirtualFree(pool_cache[id], 0, MEM_RELEASE);

        // alloc new
        pool_cap[id] = size;
        pool_in_use[id] = true;
        return (pool_cache[id] = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE));
    }
}

// This is how we tell the pool to keep the memory until someone else
// on this thread wants it
static void cuik__pool_drop(int id, void* ptr, size_t size) {
    if (ptr != pool_cache[id]) {
        // this is new memory we allocated because the first alloc was in use
        VirtualFree(ptr, 0, MEM_RELEASE);
    } else {
        pool_in_use[id] = false;
    }
}

static void cuik__pool_collect(void) {
    for (size_t i = 0; i < CUIK_POOL_MAX; i++) {
        VirtualFree(pool_cache[i], 0, MEM_RELEASE);
        pool_cache[i] = 0;
        pool_cap[i] = 0;
    }
}
