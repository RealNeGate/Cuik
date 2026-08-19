// Each worker thread is given a paired I/O thread which just waits
// for io_uring or other async I/O completion, the reason for it being
// a separate paired thread is because it means each queue can submit
// in parallel and I/O tasks can finish without needing to wait for the
// current task to end.
#include "linker.h"
#include <hashes.h>
#include <file_map.h>
#include "../tb/objects/parse_prelude.h"

#if __STDC_VERSION__ < 201112L || defined(__STDC_NO_ATOMICS__)
#error "Missing C11 support for stdatomic.h"
#endif

#include <stdatomic.h>
#include "xxhash.h"

static uint32_t namehs_hash(const void* a) {
    const TB_Slice* sym = a;
    return tb__murmur3_32(sym->data, sym->length);
}

static bool namehs_cmp(const void* a, const void* b) {
    const TB_Slice* aa = a;
    const TB_Slice* bb = b;
    return aa->length == bb->length && memcmp(aa->data, bb->data, aa->length) == 0;
}

static uint32_t symhs_hash(const void* a) {
    const TB_Slice* sym = a;
    //     return XXH64(sym->data, sym->length, 0);
    return tb__murmur3_32(sym->data, sym->length);
}

static bool symhs_cmp(const void* a, const void* b) {
    const TB_Slice* aa = a;
    const TB_Slice* bb = b;
    return aa->length == bb->length && memcmp(aa->data, bb->data, aa->length) == 0;
}

static uint32_t objhs_hash(const void* a) {
    const TB_LinkerObject* obj = a;

    uint32_t h = 0;
    h = tb__murmur3_mix(h, obj->file->fd);
    h = tb__murmur3_mix(h, obj->offset);
    return tb__murmur3_finalize(h, 8);
}

static bool objhs_cmp(const void* a, const void* b) {
    const TB_LinkerObject* aa = a;
    const TB_LinkerObject* bb = b;
    return aa->file == bb->file && aa->offset == bb->offset;
}

static uint32_t strhs_hash(const void* a) {
    uint32_t h = 0x811C9DC5;
    const char* str = a;
    while (*str) {
        h = ((uint8_t) *str++ ^ h) * 0x01000193;
    }
    return h;
}

static bool strhs_cmp(const void* a, const void* b) {
    return strcmp(a, b) == 0;
}

#define NBHM_IS_SET
#define NBHM_FN(n) strhs_ ## n
#include <nbhm.h>

#define NBHM_IS_SET
#define NBHM_FN(n) namehs_ ## n
#include <nbhm.h>

#define NBHM_FN(n) symhs_ ## n
#include <nbhm.h>

#define NBHM_IS_SET
#define NBHM_FN(n) objhs_ ## n
#include <nbhm.h>

thread_local bool linker_thread_init;
thread_local TB_Arena linker_tmp_arena;
thread_local TB_Arena linker_perm_arena;

#include "archives.c"
#include "ld_script.c"
#include "pe_linker.c"
#include "elf_linker.c"

TB_API TB_ExecutableType tb_system_executable_format(TB_System s) {
    switch (s) {
        case TB_SYSTEM_WINDOWS: return TB_EXECUTABLE_PE;
        case TB_SYSTEM_LINUX:   return TB_EXECUTABLE_ELF;
        default: tb_todo();     return TB_EXECUTABLE_UNKNOWN;
    }
}

////////////////////////////////
// Symbols
////////////////////////////////
static thread_local char* linker_bump_base;
static thread_local char* linker_bump_mark;

static thread_local char* linker_tmp_bump_base;
static thread_local char* linker_tmp_bump_mark;

char* tb_linker_local_push(void) {
    return linker_tmp_bump_mark;
}

void tb_linker_local_pop(char* savepoint) {
    if (savepoint == NULL) {
        savepoint = linker_tmp_bump_base;
    }

    log_trace("CLEAR %.3f KiB", (linker_tmp_bump_mark - savepoint) / 1024.0);
    linker_tmp_bump_mark = savepoint;
}

enum { TMP_BUFFER_CAP = 32<<20 };
void* tb_linker_alloc_local(size_t size) {
    cuikperf_region_start("alloc", NULL);
    if (linker_tmp_bump_base == NULL) {
        linker_tmp_bump_base = cuik__valloc(TMP_BUFFER_CAP);
        linker_tmp_bump_mark = linker_tmp_bump_base;
    }

    TB_ASSERT(linker_tmp_bump_mark - linker_tmp_bump_base < TMP_BUFFER_CAP);
    size = (size + 63) & ~63ull;

    char* ptr = linker_tmp_bump_mark;
    linker_tmp_bump_mark += size;
    cuikperf_region_end();
    return ptr;
}

void* tb_linker_moar_mem(size_t size) {
    if (linker_bump_base == NULL) {
        linker_bump_base = cuik__valloc(8ull << 30ull);
        linker_bump_mark = linker_bump_base;
    }

    TB_ASSERT(linker_bump_mark - linker_bump_base < (8ull << 30ull));
    size = (size + 63) & ~63ull;

    char* ptr = linker_bump_mark;
    linker_bump_mark += size;
    return ptr;
}

void tb_linker_free_mem(void* ptr, size_t size) {
    size = (size + 63) & ~63ull;

    char* tail = ((char*) ptr) + size;
    if (tail == linker_bump_mark) {
        linker_bump_mark = ptr;
    }
}

static void* bcache_hash_key(int fd, uint64_t offset) {
    assert(offset < UINT32_MAX);
    return (void*) (((uint64_t) fd << 32ull) | offset);
}

static bool block_range_overlaps(size_t offset, size_t size, size_t pos, size_t size2) {
    TB_ASSERT((pos & FILE_BLOCK_SIZE - 1) == 0);
    return pos <= offset + size - 1 && offset <= pos + size2 - 1;
}

static BCache_Job JOB_TOMBSTONE;

static uint32_t bcache_snoop_hash(BCache_File* file, uint32_t row_i) {
    uint32_t h = 0;
    h = tb__murmur3_mix(h, file->fd);
    h = tb__murmur3_mix(h, row_i);
    return tb__murmur3_finalize(h, 8) % BCACHE_MAX_SNOOPERS;
}

static void bcache_job_lock(BCache_Job* job) {
    int old = job->wait.lock;
    while (old == 0 && !atomic_compare_exchange_strong(&job->wait.lock, &old, 1)) {
    }
}

static void bcache_job_unlock(BCache_Job* job) {
    job->wait.lock = 0;
}

static bool bcache_job_notify(TB_Linker* l, BCache_Job* job, size_t row_i, int snoop_slot, bool io_thread);
static void bcache_block_response(TPool* pool, TPool_ReadReq* req) {
    cuikperf_region_start("resp", NULL);
    TB_Linker* l  = req->args[0];
    BCache_File* file = req->args[1];
    size_t offset = req->offset;

    // publish page before walking entries
    size_t block = (offset / FILE_BLOCK_SIZE);
    uint64_t group_mask = UINT64_MAX >> (64 - (req->size / FILE_BLOCK_SIZE));
    uint64_t mask = group_mask << (block % 64);
    size_t row_i  = block / 64;

    uint64_t commit = atomic_fetch_or(&file->rows[row_i].commit, mask) | mask;
    printf("  COMP %d:[%08lx %08lx]\n", file->fd, offset, offset + req->size - 1);

    // notify all snoopers
    uint32_t first = bcache_snoop_hash(file, row_i), i = first;
    do {
        BCache_Job* k = l->bcache.entries[i];
        if (k == NULL) {
            break;
        } else if (k != &JOB_TOMBSTONE && k->file == file) {
            bcache_job_notify(l, k, row_i, i, true);
        }

        i = (i + 1) % BCACHE_MAX_SNOOPERS;
    } while (first != i);

    cuikperf_region_end();
    tb_linker_job_done(l);
}

static int bcache_issue_raw_read(TB_Linker* l, BCache_Job* job, uint32_t first_block, uint32_t last_block) {
    size_t row_i = first_block / 64;
    BCache_File* file = job->file;
    assert(row_i < file->row_count);

    // base is megablock aligned
    int base = first_block & -64;
    int bits_lo = first_block - base;
    int bits_hi = last_block  - base;
    if (bits_lo < 0)  { bits_lo = 0;  }
    if (bits_hi > 64) { bits_hi = 64; }

    uint64_t mask = (UINT64_MAX >> (64 - (bits_hi - bits_lo))) << bits_lo;

    job->wait.snoop_row = row_i;
    // fence
    job->wait.row_target = mask;

    // insert snooper
    int slot = -1;
    uint32_t first = bcache_snoop_hash(file, row_i), i = first;
    do {
        BCache_Job* k = l->bcache.entries[i];
        if ((k == NULL || k == &JOB_TOMBSTONE) && atomic_compare_exchange_strong(&l->bcache.entries[i], &k, job)) {
            slot = i;
            break;
        }

        i = (i + 1) % BCACHE_MAX_SNOOPERS;
    } while (first != i);

    assert(slot >= 0);

    // if reserve is 0, we flip it and issue the read.
    uint64_t curr = file->rows[row_i].reserve;
    while ((curr & mask) != mask) {
        uint64_t next = curr | mask;
        if (atomic_compare_exchange_strong(&file->rows[row_i].reserve, &curr, next)) {
            uint64_t fresh = ~curr & mask;
            while (fresh) {
                size_t j = __builtin_ffsll(fresh) - 1;
                size_t width = fresh == UINT64_MAX ? 64 : __builtin_ffsll(~(fresh >> j)) - 1;

                uint64_t block = row_i*64 + j;
                uint64_t group_mask = UINT64_MAX >> (64 - width);

                // issued, clear bit
                fresh &= ~(group_mask << j);

                size_t offset = block*FILE_BLOCK_SIZE;
                uint8_t* buf = &file->raw_map[block*FILE_BLOCK_SIZE];
                printf("  READ %d:[%08lx %08lx]\n", file->fd, offset, offset + width*FILE_BLOCK_SIZE - 1);

                CUIK_TIMED_BLOCK("issue") {
                    l->jobs.count += 1;
                    tpool_io_read(l->jobs.pool, file->fd, offset, width*FILE_BLOCK_SIZE, buf, bcache_block_response, l, file, NULL);
                }
            }
            break;
        }
    }

    return slot;
}

static void bcache_job_forward(TPool* pool, void** args) {
    TB_Linker* l    = args[0];
    BCache_Job* job = args[1];
    if (job->fn(l, job, job->arg)) {
        tb_linker_job_done(l);
    }
}

static void bcache_notify_forward(TPool* pool, void** args) {
    TB_Linker* l    = args[0];
    BCache_Job* job = args[1];
    uintptr_t head_tail = (uintptr_t) args[2];

    uint32_t head = head_tail >> 32ull;
    uint32_t tail = head_tail & 0xFFFFFFFF;

    size_t snoop_slot = bcache_issue_raw_read(l, job, head, tail);
    if (bcache_job_notify(l, job, head / 64, snoop_slot, false)) {
        if (job->fn(l, job, job->arg)) {
            tb_linker_job_done(l);
        }
    }
}

static bool bcache_job_notify(TB_Linker* l, BCache_Job* job, size_t row_i, int snoop_slot, bool io_thread) {
    for (;;) {
        bcache_job_lock(job);
        if (job->wait.snoop_row != row_i) {
            bcache_job_unlock(job);
            return false;
        }

        BCache_File* file = job->file;
        assert(job->wait.snoop_row < file->row_count);
        uint64_t curr   = file->rows[job->wait.snoop_row].commit;
        uint64_t target = job->wait.row_target;

        // Reset row_target when we match, if we lose the CAS then
        // someone else must've done the same thing.
        if (target == 0 || (curr & target) != target) {
            bcache_job_unlock(job);
            return false;
        }

        int delta = tb_popcount64(target);
        job->wait.row_target = 0;
        job->wait.head += delta;
        bcache_job_unlock(job);

        // Remove job from snoop list
        l->bcache.entries[snoop_slot] = &JOB_TOMBSTONE;

        uint32_t head = job->wait.head;
        uint32_t tail = job->wait.tail;
        if (head == tail) {
            // We've completed the read request, resume
            printf("NOTIFY %p\n", job->arg);

            void* args[2] = { l, job };
            bool hi_prio = false;
            if (io_thread) {
                tpool_io_forward(l->jobs.pool, hi_prio, bcache_job_forward, 2, args);
            }
            return true;
        } else {
            // The I/O response cannot directly produce IO requests, we'll forward
            // to a worker which can as a high prio task
            if (io_thread) {
                uintptr_t head_tail = ((uintptr_t) head << 32ull) | ((uintptr_t) tail);
                void* args[3] = { l, job, (void*) head_tail };
                tpool_io_forward(l->jobs.pool, true, bcache_notify_forward, 3, args);
                return false;
            } else {
                // If we're doing a big read then this is the time to re-install
                // a new snoop row.
                snoop_slot = bcache_issue_raw_read(l, job, head, tail);
            }
        }
    }
}

bool tb_linker_input_fn(TB_Linker* l, BCache_Job* job, void* arg) {
    TB_LinkerObject* obj = arg;
    tb_linker_worker_init(l);

    if (!obj->classify) {
        cuikperf_region_start("classify", NULL);
        bool ready = l->vtbl.classify_input(l, job, obj);
        cuikperf_region_end();

        if (!ready) {
            return false;
        }
        obj->classify = true;
    }

    TB_Slice name = tb_linker_get_base_name(obj->name);
    cuikperf_region_start2("step", name.length, (const char*) name.data);
    log_debug("Step '%.*s' (%#"PRIx64")", (int) name.length, (const char*) name.data, obj->offset);

    TB_Slice content = { &obj->file->raw_map[obj->offset + obj->skip_header], PREFETCH_BLOCK_SIZE - obj->skip_header };
    char* sp  = tb_linker_local_push();
    bool done = obj->step(l, job, obj, content);
    tb_linker_local_pop(sp);
    cuikperf_region_end();

    return done;
}

// Begin job in an idle state
BCache_Job* tb_linker_new_job(TB_Linker* l, BCache_File* file, BCache_Fn* fn, void* arg) {
    l->jobs.count += 1;
    BCache_Job* job = tb_linker_moar_mem(sizeof(BCache_Job));
    *job = (BCache_Job){ .file = file, .fn = fn, .arg = arg };
    return job;
}

// Ask to read in the background
void tb_linker_job_prefetch(TB_Linker* l, BCache_Job* job, BCache_File* file, size_t offset, size_t size) {

}

bool tb_linker_job_read_FAST(TB_Linker* l, BCache_Job* job, size_t offset, size_t size, void** buffer) {
    BCache_File* file = job->file;
    if (buffer) {
        *buffer = &file->raw_map[offset];
    }

    // Read in aligned blocks
    size_t first_block = offset / FILE_BLOCK_SIZE;
    size_t last_block  = (offset + size + FILE_BLOCK_SIZE - 1) / FILE_BLOCK_SIZE;

    // Readahead logic, this works by just issuing loads to file blocks we think
    // we'll be accessing soon.
    static thread_local BCache_File* last_read_file;
    static thread_local size_t last_read_start;
    static thread_local size_t last_read_end;

    /* if (last_read_file == ) {

    }
    last_read_file = 0; */

    // Issue megablock load
    job->wait.head = first_block;
    job->wait.tail = last_block;
    int slot = bcache_issue_raw_read(l, job, first_block, last_block);

    // If there's in-progress writes to blocks we care about then just snooping
    // won't catch those completions, we need to at least check that the base. If
    // we're late to notifying the first snoop then we can gave up here because whoever
    // saw it clearly... saw it so we can leave him responsible.
    if (!bcache_job_notify(l, job, first_block / 64, slot, false)) {
        printf("WAIT %p\n", job->arg);
        return false;
    }

    // tpool_add_task2(l->jobs.pool, hi_prio, bcache_job_forward, 2, args);
    return true;
}

void tb_linker_issue_prefetch(TB_Linker* l, TB_LinkerObject* obj) {
    BCache_Job* job = tb_linker_new_job(l, obj->file, tb_linker_input_fn, obj);

    size_t limit = obj->offset + PREFETCH_BLOCK_SIZE;
    if (limit > obj->file->size) {
        limit = obj->file->size;
    }
    assert(obj->offset < limit);

    size_t size = limit - obj->offset;
    if (tb_linker_job_read_FAST(l, job, obj->offset, size, NULL)) {
        void* args[2] = { l, job };
        tpool_add_task2(l->jobs.pool, true, bcache_job_forward, 2, args);
    }
}

void tb_linker_worker_init(TB_Linker* l) {
    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }
}

void tb_linker_job_submit_1(TB_Linker* l, tpool_task_proc* fn, void* arg) {
    if (l->jobs.pool != NULL) {
        #if CUIK_ALLOW_THREADS
        l->jobs.count += 1;
        tpool_add_task(l->jobs.pool, false, fn, arg);
        #else
        abort(); // Unreachable
        #endif
    } else {
        fn(NULL, &arg);
    }
}

void tb_linker_job_submit_N(TB_Linker* l, tpool_task_proc* fn, int count, void** args) {
    if (l->jobs.pool != NULL) {
        #if CUIK_ALLOW_THREADS
        l->jobs.count += 1;
        tpool_add_task2(l->jobs.pool, false, fn, count, args);
        #else
        abort(); // Unreachable
        #endif
    } else {
        fn(NULL, args);
    }
}

void tb_linker_job_done(TB_Linker* l) {
    if (l->jobs.pool != NULL) {
        int old = ++l->jobs.done;
        assert(old <= l->jobs.count);
        futex_signal(&l->jobs.done);
    }
}

TB_Linker* tb_linker_create(TB_ExecutableType exe, TB_Arch arch, TPool* tp) {
    TB_Linker* l = cuik_aligned_alloc(sizeof(TB_Linker), alignof(TB_Linker));
    memset(l, 0, sizeof(TB_Linker));
    l->target_arch = arch;
    l->jobs.pool = tp;
    mtx_init(&l->lock, mtx_plain);

    CUIK_TIMED_BLOCK("I/O thread prep") {
        tpool_io_prep_all(l->jobs.pool);
    }

    CUIK_TIMED_BLOCK("Alloc tables") {
        l->symbols  = nbhm_alloc(16384 * 2);
        l->sections = nbhs_alloc(16);
        l->imports  = nbhs_alloc(256);
        l->libs     = nbhs_alloc(32);
        l->objects  = nbhs_alloc(256);
        l->unresolved_symbols = nbhs_alloc(16);
    }

    switch (exe) {
        case TB_EXECUTABLE_PE: l->vtbl = tb__linker_pe; break;
        case TB_EXECUTABLE_ELF: l->vtbl = tb__linker_elf; break;
        default: break;
    }

    l->vtbl.init(l);
    return l;
}

void tb_linker_set_subsystem(TB_Linker* l, TB_WindowsSubsystem subsystem) {
    l->subsystem = subsystem;
}

void tb_linker_set_entrypoint(TB_Linker* l, const char* name) {
    l->entrypoint = name;
}

static char* linker_newstr(size_t len, const char* path) {
    char* newstr = cuik_malloc(len + 1);
    memcpy(newstr, path, len);
    newstr[len] = 0;
    return newstr;
}

void tb_linker_add_libpath(TB_Linker* l, const char* path) {
    dyn_array_put(l->libpaths, linker_newstr(strlen(path), path));
}

static void print_name(TB_Slice name) {
    size_t slash = 0;
    FOR_REV_N(i, 0, name.length) {
        if (name.data[i] == '/' || name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }

    printf("%.*s", (int) (name.length - slash), (const char*) name.data + slash);
}

static bool is_symbol_defined(TB_LinkerSymbol* sym) {
    return sym->tag != TB_LINKER_SYMBOL_UNKNOWN && sym->tag != TB_LINKER_SYMBOL_LAZY;
}

void tb_linker_print_map(TB_Linker* l) {
    printf(" Start         Length     Name                   Class\n");
    dyn_array_for(i, l->sections_arr) {
        TB_LinkerSection* s = l->sections_arr[i];

        uint32_t mask = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE;
        bool is_code = (s->flags & mask) == mask;

        int len = printf(" %04x:%08zx %08zxH %.*s", s->segment->number, s->offset, s->size, (int) s->name.length, s->name.data);
        printf("%*s%s\n", 49 - len, "", is_code ? "CODE" : "DATA");
    }

    DynArray(TB_LinkerSymbol*) symbols = NULL;
    NBHM_FOR(e, &l->symbols) {
        TB_LinkerSymbol* sym = tb_linker_root_symbol(l, e.v);
        if ((sym->tag == TB_LINKER_SYMBOL_NORMAL) && (sym->normal.piece->flags & TB_LINKER_PIECE_LIVE)) {
            dyn_array_put(symbols, sym);
        }
    }
    qsort(symbols, dyn_array_length(symbols), sizeof(TB_LinkerSymbol*), compare_symbols);

    TB_LinkerSection* text  = tb_linker_find_section(l, ".text");
    uint32_t trampoline_rva = text->segment->address + l->trampoline_pos;

    printf("\n  Address         Publics by Value              Rva+Base               Lib:Object\n\n");
    dyn_array_for(i, symbols) {
        TB_LinkerSymbol* sym = symbols[i];

        uint32_t secidx = 0;
        uint32_t secrel = 0;
        if (sym->tag == TB_LINKER_SYMBOL_NORMAL && sym->normal.piece->parent->segment) {
            secidx = sym->normal.piece->parent->segment->number;
            secrel = sym->normal.piece->offset + sym->normal.piece->parent->offset + sym->normal.secrel;
        } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            secrel = sym->import.ordinal;
        }

        int len = printf(" %04"PRIx32":%08"PRIx32"       %.*s", secidx, secrel, (int) sym->name.length, sym->name.data);
        // add padding
        printf("%*s", len < 80 ? 80 - len : 1, "");
        if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
            print_name(sym->normal.piece->obj->name);
            printf(" (%#"PRIx64")", sym->normal.piece->order);
        } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            printf(" (%"PRIx32", %d)", trampoline_rva + sym->import.ds_address, sym->import.thunk_id);
        }
        printf("\n");
    }
    dyn_array_destroy(symbols);
}

bool tb__linker_is_library_new(TB_Linker* l, const char* file_name) {
    tb_linker_worker_init(l);

    size_t len = strlen(file_name);
    char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
    memcpy(str, file_name, len);
    str[len] = 0;

    bool found = strhs_get(&l->libs, str);
    tb_arena_free(&linker_perm_arena, str, len + 1);
    return !found;
}

TB_Slice tb_linker_get_base_name(TB_Slice name) {
    FOR_REV_N(i, 0, name.length) {
        if (name.data[i] == '/' || name.data[i] == '\\') {
            return (TB_Slice){ name.data + i + 1, name.length - (i + 1) };
        }
    }
    return name;
}

static BCache_File* bcache_open(int fd, size_t size) {
    assert(fd >= 0);

    size_t blocks = (size + FILE_BLOCK_SIZE - 1) / FILE_BLOCK_SIZE;
    size_t word_count = (blocks + 63) / 64;

    BCache_File* file = tb_linker_moar_mem(sizeof(BCache_File) + word_count*sizeof(BCache_Row));
    file->fd   = fd;
    file->size = size;
    file->row_count = word_count;
    file->raw_map = cuik__valloc(blocks * FILE_BLOCK_SIZE); // mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);

    // file->raw_map = mmap(NULL, size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    // log_debug("OPEN %p %p", file->raw_map, file->raw_map + size - 1);
    return file;
}

void tb_linker_forward_input(TPool* pool, void** args) {
    cuikperf_region_start("forward", NULL);
    TB_LinkerObject* obj = args[0];
    TB_Linker* l = obj->linker;
    const char* file_name = (const char*) obj->name.data;
    tb_linker_worker_init(obj->linker);
    log_info("Loading input: %s", file_name);

    size_t size;
    int fd = open_file(file_name, &size);
    if (fd < 0) {
        printf("tblink: could not find file: %s\n", file_name);
        goto done;
    }

    obj->size = size;
    obj->file = bcache_open(fd, size);
    tb_linker_issue_prefetch(l, obj);

    done:
    cuikperf_region_end();
    tb_linker_job_done(l);
}

static void linker_job_find_lib(TPool* pool, void** args) {
    TB_Linker* l = args[0];
    char* str = args[1];
    uint64_t t = (uint64_t) args[2];

    cuikperf_region_start("find lib", str);
    tb_linker_worker_init(l);

    size_t size;
    char resolved_path[FILENAME_MAX];
    int fd = l->vtbl.find_lib(l, str, resolved_path, &size);
    if (fd < 0) {
        goto done;
    }
    log_info("Loading input: %s", resolved_path);

    size_t newlen = strlen(resolved_path);
    char* newstr = linker_newstr(newlen, resolved_path);

    TB_LinkerArchive* lib_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerArchive));
    *lib_file = (TB_LinkerArchive){ {
            .name   = { (const uint8_t*) newstr, newlen },
            .linker = l,
            .time   = t,
            .size   = size,
        } };

    lib_file->header.size = size;
    lib_file->header.file = bcache_open(fd, size);
    tb_linker_issue_prefetch(l, &lib_file->header);

    done:
    cuikperf_region_end();
    tb_linker_job_done(l);
}

void tb_linker_append_object(TB_Linker* l, const char* file_name) {
    CUIK_TIMED_BLOCK("append_obj") {
        TB_LinkerObject* obj = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
        *obj = (TB_LinkerObject){
            .name   = { (const uint8_t*) file_name, strlen(file_name) },
            .linker = l,
            .time   = atomic_fetch_add(&l->time, 0x100000000),
        };

        if (l->jobs.pool != NULL && tpool_num_threads(l->jobs.pool) > 1) {
            // Push to a worker to distribute the I/O better
            tb_linker_job_submit_1(l, tb_linker_forward_input, obj);
        } else {
            l->jobs.count += 1;

            void* arg = obj;
            tb_linker_forward_input(l->jobs.pool, &arg);
        }
    }
}

void tb_linker_append_library(TB_Linker* l, const char* file_name) {
    tb_linker_worker_init(l);

    size_t len = strlen(file_name);
    char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
    memcpy(str, file_name, len);
    str[len] = 0;

    if (strhs_intern(&l->libs, str) != str) {
        tb_arena_free(&linker_perm_arena, str, len + 1);
        return;
    }

    uint64_t time = atomic_fetch_add(&l->time, 0x100000000);
    void* args[] = { l, str, (void*) time };
    tb_linker_job_submit_N(l, linker_job_find_lib, 3, args);
}

void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    #ifdef CONFIG_HAS_TB
    /* tb_linker_worker_init(l);

    size_t newlen = sizeof("Module")-1;
    char* newstr = linker_newstr(newlen, "Module");

    TB_LinkerObject* obj = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
    *obj = (TB_LinkerObject){
    .name   = { (const uint8_t*) newstr, newlen },
    .linker = l,
    .module = m,
    .time   = atomic_fetch_add(&l->time, 0x100000000),
    };
    tb_linker_job_submit_1(l, l->vtbl.append_module, obj);
    #else */
    assert(0 && "Not supported");
    #endif
}

bool tb_linker_export(TB_Linker* l, const char* file_name) {
    return l->vtbl.export(l, file_name);
}

static int compare_str(const void* a, const void* b) {
    const char* aa = *(const char**) a;
    const char* bb = *(const char**) b;
    return strcmp(aa, bb);
}

void tb_linker_barrier(TB_Linker* l) {
    // finish up parsing all the object file tasks
    cuikperf_region_start("barrier", NULL);
    if (l->jobs.pool != NULL) {
        #if CUIK_ALLOW_THREADS
        assert(dyn_array_length(l->worklist) == 0);
        tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
        #else
        abort(); // Unreachable
        #endif
    } else {
        abort();
    }
    cuikperf_region_end();
}

void tb_linker_complete_appends(TB_Linker* l) {
    cuikperf_region_start("complete", NULL);
    bool repeat;
    do {
        tb_linker_barrier(l);
        break;

        log_info("LINKER BARRIER!");

        mtx_lock(&l->lock);
        qsort(l->default_libs, dyn_array_length(l->default_libs), sizeof(const char*), compare_str);

        dyn_array_for(i, l->alternate_names) {
            TB_LinkerCmd cmd = l->alternate_names[i];

            // if "from" is not defined, we used "to". if it's been defined
            // at this point, we don't add the weak link.
            TB_LinkerSymbol* from = tb_linker_find_symbol(l, cmd.from);
            if (from == NULL || from->tag == TB_LINKER_SYMBOL_UNKNOWN || from->tag == TB_LINKER_SYMBOL_LAZY) {
                if (from == NULL) {
                    from = tb_linker_import_symbol(l, cmd.from);
                }

                TB_LinkerSymbol* to = tb_linker_import_symbol(l, cmd.to);
                tb_linker_symbol_weak(l, from, to);
            }
        }

        // once we've completed whatever jobs we can do another round of defaultlibs
        dyn_array_for(i, l->default_libs) {
            tb_linker_append_library(l, l->default_libs[i]);
        }
        repeat = dyn_array_length(l->worklist) > 0 || dyn_array_length(l->default_libs) > 0;
        dyn_array_clear(l->default_libs);
        // dyn_array_clear(l->alternate_names);
        mtx_unlock(&l->lock);
    } while (repeat);
    cuikperf_region_end();
}

void tb_linker_destroy(TB_Linker* l) {
    cuik_free(l);
}

TB_LinkerSectionPiece* tb_linker_get_piece(TB_Linker* l, TB_LinkerSymbol* restrict sym) {
    if (sym && (sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB)) {
        return sym->normal.piece;
    }

    return NULL;
}

size_t tb__get_symbol_pos(TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_FUNCTION) {
        return ((TB_Function*) s)->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        return ((TB_Global*) s)->pos;
    } else {
        tb_todo();
    }
}

uint64_t tb__get_symbol_rva(TB_LinkerSymbol* sym) {
    if (sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
        return 0;
    } else if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE) {
        return sym->imagebase;
    } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
        return 0x80000000 + sym->import.thunk_id;
    }

    // normal or TB
    assert(sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB);
    TB_LinkerSectionPiece* piece = sym->normal.piece;

    uint32_t rva = piece->parent->segment->address + piece->parent->offset + piece->offset;
    if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
        return rva + sym->normal.secrel;
    }

    TB_Symbol* s = sym->tb.sym;
    if (s->tag == TB_SYMBOL_FUNCTION) {
        TB_Function* f = (TB_Function*) s;
        assert(f->output != NULL);

        return rva + f->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        return rva + ((TB_Global*) s)->pos;
    } else {
        tb_todo();
    }
}

size_t tb__pad_file(uint8_t* output, size_t write_pos, char pad, size_t align) {
    size_t align_mask = align - 1;
    size_t end = (write_pos + align_mask) & ~align_mask;
    if (write_pos != end) {
        memset(output + write_pos, 0, end - write_pos);
        write_pos = end;
    }
    return write_pos;
}

// TODO(NeGate): I'm 99% sure this doesn't need locks, because both section pieces are being exclusively
// owned by the same object file (and thus the one thread constructing them).
void tb_linker_associate(TB_Linker* l, TB_LinkerSectionPiece* a, TB_LinkerSectionPiece* b) {
    dyn_array_put(a->assoc, b);
}

TB_LinkerSegment* tb_linker_find_segment(TB_Linker* linker, const char* name) {
    DynArray(TB_LinkerSegment*) segments = linker->segments;

    size_t length = strlen(name);
    size_t i = 0, segment_count = dyn_array_length(segments);
    for (; i < segment_count; i++) {
        if (segments[i]->name.length == length && memcmp(segments[i]->name.data, name, length) == 0) {
            return segments[i];
        }
    }

    return NULL;
}

TB_LinkerSymbol* tb_linker_root_symbol(TB_Linker* l, TB_LinkerSymbol* sym) {
    // private/static symbols don't have an entry in the global symbol table, which
    // also makes their pointers stable and their data immutable.
    //
    // Defined Non-COMDAT symbols can never be redefined so whatever is in the symbol
    // map is the correct answer
    if (sym == NULL || (sym->flags & TB_LINKER_SYMBOL_GLOBAL) == 0) {
        return sym;
    }

    #if 1
    TB_LinkerSymbol* root = atomic_load_explicit(&sym->root, memory_order_acquire);
    if (root == NULL) {
        return sym;
    }

    cuikperf_region_start("lookup", NULL);
    // this really doesn't need to be acquire, maybe relaxed?
    int path_length = 0;
    TB_LinkerSymbol *base = sym, *next;
    while (next = atomic_load_explicit(&sym->root, memory_order_acquire), next) {
        sym = next, path_length++;
    }

    if (path_length > 1) {
        // path compression
        while (next = atomic_load_explicit(&base->root, memory_order_acquire), next) {
            atomic_store_explicit(&base->root, sym, memory_order_release);
            base = next;
        }
    }

    // printf("%d\n", path_length);
    cuikperf_region_end();
    #else
    TB_LinkerSymbol* root = atomic_load_explicit(&sym->root, memory_order_acquire);
    if (root == NULL) {
        root = symhs_geth(&l->symbols, &sym->name, sym->hash_cache);

        // the answer should be deterministic at this point so we could just
        // relaxed store and hope for the best.
        atomic_store_explicit(&sym->root, root, memory_order_release);
    }
    #endif

    return sym;
}

TB_LinkerSection* tb_linker_find_section(TB_Linker* l, const char* name) {
    TB_Slice str = { (const uint8_t*) name, strlen(name) };
    return namehs_get(&l->sections, &str);
}

TB_LinkerSection* tb_linker_find_or_create_section(TB_Linker* l, size_t name_len, const char* name, uint32_t flags) {
    TB_LinkerSection* s = tb_linker_moar_mem(sizeof(TB_LinkerSection));
    *s = (TB_LinkerSection){ .name = { (const uint8_t*) name, name_len }, .flags = flags };

    TB_LinkerSection* k = namehs_intern(&l->sections, s);
    if (s != k) {
        tb_linker_free_mem(s, sizeof(TB_LinkerSymbol));
        return k;
    }
    return s;
}

TB_LinkerSectionPiece* tb_linker_append_piece(TB_LinkerSection* section, int kind, size_t size, TB_LinkerObject* obj) {
    TB_LinkerSectionPiece* piece = tb_linker_moar_mem(sizeof(TB_LinkerSectionPiece));
    *piece = (TB_LinkerSectionPiece){
        .kind   = kind,
        .parent = section,
        .obj    = obj,
        .size   = size,
    };
    atomic_fetch_add(&section->piece_count, 1);

    // normal LL insert
    TB_LinkerSectionPiece* old_top;
    do {
        old_top = atomic_load(&section->list);
        piece->next = old_top;
    } while (!atomic_compare_exchange_strong(&section->list, &old_top, piece));

    return piece;
}

void tb_linker_merge_sections(TB_Linker* linker, TB_LinkerSection* from, TB_LinkerSection* to) {
    if (from == NULL) {
        return;
    } else if (from->list != NULL) {
        // move all parents (probably slow too, infrequent enough tho)
        TB_LinkerSectionPiece* list = from->list;
        TB_LinkerSectionPiece* last = NULL;
        while (list != NULL) {
            last = list;
            list->parent = to;
            list = atomic_load_explicit(&list->next, memory_order_relaxed);
        }

        if (last != NULL) {
            last->next = to->list;
            to->list = from->list;
        }
    }

    to->piece_count += from->piece_count;
    from->generic_flags |= TB_LINKER_SECTION_DISCARD;
    from->piece_count = 0;
    from->list = NULL;
}

TB_LinkerSymbol* tb_linker_find_symbol(TB_Linker* l, TB_Slice name) {
    return symhs_get(&l->symbols, &name);
}

TB_LinkerSymbol* tb_linker_find_symbol2(TB_Linker* l, const char* name) {
    TB_Slice str = { (const uint8_t*) name, strlen(name) };
    return symhs_get(&l->symbols, &str);
}

TB_LinkerSymbol* tb_linker_import_symbol(TB_Linker* l, TB_Slice name) {
    TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
    *s = (TB_LinkerSymbol){ .name = name, .tag = TB_LINKER_SYMBOL_UNKNOWN };
    return tb_linker_symbol_insert(l, s, true);
}

TB_LinkerSymbol* tb_linker_new_symbol(TB_Linker* l, size_t len, const char* name) {
    TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
    *s = (TB_LinkerSymbol){ .name = { (const uint8_t*) name, len }, .tag = TB_LINKER_SYMBOL_NORMAL };
    return tb_linker_symbol_insert(l, s, true);
}

void tb_linker_symbol_weak(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerSymbol* alt) {
    log_debug("WEAK %.*s", (int) sym->name.length, sym->name.data);

    // there's two symbol types which
    assert(sym->flags & TB_LINKER_SYMBOL_GLOBAL);
    if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
        TB_LinkerSymbol* old = atomic_load_explicit(&sym->weak_alt, memory_order_acquire);
        if (old == NULL && atomic_compare_exchange_strong(&sym->weak_alt, &old, alt)) {
            return;
        }

        // error if it's not what we wanted because that means there's two weak alts
        if (old != alt) {
            // TODO(NeGate): write a good error plz
            abort();
        }
    }
}

static const char* tag_name(int tag) {
    switch (tag) {
        case TB_LINKER_SYMBOL_UNKNOWN: return "unknown";
        case TB_LINKER_SYMBOL_NORMAL: return "normal";
        case TB_LINKER_SYMBOL_LAZY: return "lazy";
        case TB_LINKER_SYMBOL_IMPORT: return "import";
        case TB_LINKER_SYMBOL_TB: return "tb";
        default: return "???";
    }
}

// returns true if A > B
static bool tb_linker_symbol_merge(TB_Linker* l, TB_LinkerSymbol* a, TB_LinkerSymbol* b) {
    if (a->tag == TB_LINKER_SYMBOL_IMPORT && b->tag == TB_LINKER_SYMBOL_IMPORT) {
        // pick the smaller ordinal
        return a->import.ordinal < b->import.ordinal;
    } else if (a->tag == TB_LINKER_SYMBOL_LAZY && b->tag == TB_LINKER_SYMBOL_LAZY) {
        // just take the earlier symbol, this might count as a duplicate sym error ngl
        return false;
    } else if (a->tag == TB_LINKER_SYMBOL_LAZY || b->tag == TB_LINKER_SYMBOL_LAZY) {
        // if one side is defined, we'll use that.
        //
        // keep the old symbol since we've triggered the move by the transition
        // by keeping the old symbol we guarentee that a symbol which is LAZY or
        // UNDEF first is never replaced by the respective copy (thus keeping the
        // logic of assigning a weak sym a lot simpler).
        if (a->tag == TB_LINKER_SYMBOL_UNKNOWN && b->tag == TB_LINKER_SYMBOL_LAZY) {
            tb_linker_lazy_resolve(l, b);
            return true;
        } else if (a->tag == TB_LINKER_SYMBOL_LAZY && b->tag == TB_LINKER_SYMBOL_UNKNOWN) {
            tb_linker_lazy_resolve(l, a);
            return false;
        } else {
            return is_symbol_defined(b);
        }
    } else if (a->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        // if we're both unresolved then we don't need to do shit yet
        return b->tag != TB_LINKER_SYMBOL_UNKNOWN;
    } else if (b->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        return false;
    } else if ((a->flags | b->flags) & TB_LINKER_SYMBOL_COMDAT) {
        // if only one is COMDAT, we always pick the COMDAT one
        if ((a->flags & TB_LINKER_SYMBOL_COMDAT) == 0) {
            return true;
        } else if ((b->flags & TB_LINKER_SYMBOL_COMDAT) == 0) {
            return false;
        } else {
            // COMDAT, we need to decide which of these lives but for now we don't care.
            /* uint64_t old_order = a->tag == TB_LINKER_SYMBOL_NORMAL ? a->normal.order : 0;
            uint64_t sym_order = b->tag == TB_LINKER_SYMBOL_NORMAL ? b->normal.order : 0;
            return old_order > sym_order;*/
            return false;
        }
    } else {
        // symbol collision if we're overriding something that's
        // not a forward ref.
        if (a->tag == TB_LINKER_SYMBOL_NORMAL && b->tag == TB_LINKER_SYMBOL_NORMAL) {
            mtx_lock(&l->lock);
            printf("\x1b[31merror\x1b[0m: symbol collision: %.*s\n", (int) b->name.length, b->name.data);
            printf("  old: "); print_name(a->normal.piece->obj->name); if (a->normal.piece->obj->parent) {
                printf("(");
                print_name(a->normal.piece->obj->parent->name);
                printf(")");
            } printf("\n");
            printf("  new: "); print_name(b->normal.piece->obj->name); if (b->normal.piece->obj->parent) {
                printf("(");
                print_name(b->normal.piece->obj->parent->name);
                printf(")");
            } printf("\n");
            mtx_unlock(&l->lock);
        }
        return false;
    }
}

void tb_linker_lazy_resolve(TB_Linker* l, TB_LinkerSymbol* sym) {
    cuikperf_region_start("lazy resolve", NULL);
    TB_LinkerArchive* lib = sym->lazy.lib;
    uint32_t offset = sym->lazy.offset;

    // create object file, we don't know the name
    // of it just yet but that'll be filled in soon.
    TB_LinkerObject* obj = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
    *obj = (TB_LinkerObject){
        .linker = l,
        .time   = lib->header.time + offset,
        .parent = &lib->header,
        .file = lib->header.file,
        .offset = offset,
    };

    TB_LinkerObject* k = objhs_intern(&l->objects, obj);
    if (k != obj) {
        tb_arena_free(&linker_perm_arena, k, sizeof(TB_LinkerObject));
        cuikperf_region_end();
        return;
    }

    #if 0
    log_info("Load object file at %d:%zu (for %.*s)", lib->header.file->fd, offset, (int) sym->name.length, sym->name.data);
    l->jobs.count += 1;
    tb_linker_issue_prefetch(obj->linker, obj);
    #endif

    cuikperf_region_end();
}

// if owned is true, the sym can be deleted if it's not inserted
TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* new_sym, bool owned) {
    TB_LinkerSymbol* sym = new_sym;
    sym->flags |= TB_LINKER_SYMBOL_GLOBAL;

    // printf("%.*s    %"PRIx32"\n", (int) sym->name.length, sym->name.data, tb__murmur3_32(sym->name.data, sym->name.length) & 65535);

    #if 0 // For debugging
    static const char sss[] = "$stateUnwindMap$?catch$0@?0???2@YAPEAX_KW4align_val_t@std@@AEBUnothrow_t@1@@Z@4HA";
    if (sym->name.length == sizeof(sss)-1 && memcmp((const char*) sym->name.data, sss, sizeof(sss)-1) == 0) {
        mtx_lock(&l->lock);
        printf("INSERT %.*s (%s)", (int) sym->name.length, sym->name.data, tag_name(sym->tag));

        TB_LinkerObject* obj = NULL;
        if (sym->tag == TB_LINKER_SYMBOL_NORMAL) {
            obj = sym->normal.piece->obj;
        } else if (sym->tag == TB_LINKER_SYMBOL_LAZY) {
            obj = &sym->lazy.lib->header;
        } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            printf(" I%d", sym->import.ordinal);
        }

        if (obj) {
            printf(" (");
            print_name(obj->name);
            if (obj->parent) {
                printf(" : ");
                print_name(obj->parent->name);
            }
            printf(")");
        }
        printf("\n");
        mtx_unlock(&l->lock);
    }
    #endif

    // cuikperf_region_start("TX", NULL);
    TB_LinkerSymbol* old;
    NBHM_Tx tx = symhs_tx_begin(&l->symbols, new_sym, false);
    do {
        // if the merge is false, meaning our new symbol is
        // ordered before the current one, we don't need to
        // commit anything.
        old = nbhm_tx_val(&tx);
        assert(old != new_sym);

        if (old != NULL && !tb_linker_symbol_merge(l, old, new_sym)) {
            sym = old;
            break;
        }

        if (old != NULL) {
            // Concurrent union-find to speed up later lookups, now we
            // know that old < new_sym.
            TB_LinkerSymbol* old_root = old->root;
            if (old_root == NULL) {
                atomic_compare_exchange_strong(&old->root, &old_root, new_sym);
            }
        }
    } while (!symhs_tx_commit(&tx, new_sym));
    symhs_tx_end(&l->symbols);
    // cuikperf_region_end();

    // I think all calls have the "owned" as true? check that out later and maybe
    // decide we don't need it
    if (owned && tx.k != sym && old == sym) {
        tb_linker_free_mem(new_sym, sizeof(TB_LinkerSymbol));
    }
    return sym;
}

#ifdef CONFIG_HAS_TB
void tb_linker_append_module_symbols(TB_Linker* l, TB_Module* m) {
    DynArray(TB_ModuleSection) sections = m->sections;

    CUIK_TIMED_BLOCK("apply symbols") {
        dyn_array_for(i, sections) {
            DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
            DynArray(TB_Global*) globals = sections[i].globals;
            TB_LinkerSectionPiece* piece = sections[i].piece;

            dyn_array_for(i, funcs) {
                const char* name = funcs[i]->parent->super.name;
                TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = { (const uint8_t*) name, strlen(name) },
                    .tag  = TB_LINKER_SYMBOL_TB,
                    .tb   = { piece, &funcs[i]->parent->super }
                };

                if (funcs[i]->linkage != TB_LINKAGE_PRIVATE) {
                    s = tb_linker_symbol_insert(l, s, true);
                }

                funcs[i]->parent->super.address = s;
            }

            dyn_array_for(i, globals) {
                const char* name  = globals[i]->super.name;
                TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = { (const uint8_t*) name, strlen(name) },
                    .tag  = TB_LINKER_SYMBOL_TB,
                    .tb   = { piece, &globals[i]->super }
                };

                if (globals[i]->super.linkage != TB_LINKAGE_PRIVATE) {
                    s = tb_linker_symbol_insert(l, s, true);
                }
                globals[i]->super.address = s;
            }
        }
    }
}
#endif

static TB_Slice as_filename(TB_Slice s) {
    size_t last = 0;
    FOR_N(i, 0, s.length) {
        if (s.data[i] == '/' || s.data[i] == '\\') {
            last = i+1;
        }
    }

    return (TB_Slice){ s.data + last, s.length - last };
}

static int compare_linker_sections(const void* a, const void* b) {
    const TB_LinkerSection* sec_a = *(const TB_LinkerSection**) a;
    const TB_LinkerSection* sec_b = *(const TB_LinkerSection**) b;

    size_t len = TB_MIN(sec_a->name.length, sec_b->name.length);
    FOR_N(i, 0, len) {
        int c = sec_a->name.data[i] - sec_b->name.data[i];
        if (c != 0) { return c; }
    }

    if (sec_a->name.length < sec_b->name.length) { return -1; }
    if (sec_a->name.length > sec_b->name.length) { return  1; }
    return 0;
}

static int compare_linker_pieces(const void* a, const void* b) {
    const TB_LinkerSectionPiece* sec_a = *(const TB_LinkerSectionPiece**) a;
    const TB_LinkerSectionPiece* sec_b = *(const TB_LinkerSectionPiece**) b;

    if (sec_a->order < sec_b->order) return -1;
    if (sec_a->order > sec_b->order) return  1;
    return 0;
}

static void tb_linker_sort_section(TPool* pool, void** args) {
    TB_Linker* l = args[0];
    TB_LinkerSection* s = args[1];
    cuikperf_region_start2("sort", s->name.length, (const char*) s->name.data);

    size_t piece_count = s->piece_count;

    ////////////////////////////////
    // Sort sections
    ////////////////////////////////
    // convert into array
    DynArray(TB_LinkerSectionPiece*) array_form = dyn_array_create(TB_LinkerSectionPiece*, piece_count);
    CUIK_TIMED_BLOCK("convert to array") {
        TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if (p->size != 0 && (p->flags & TB_LINKER_PIECE_LIVE)) {
                dyn_array_put(array_form, p);
            }
        }

        // printf("%.*s: %zu -> %zu\n", (int) s->name.length, s->name.data, piece_count, dyn_array_length(array_form));
    }

    if (dyn_array_length(array_form) == 0) {
        dyn_array_destroy(array_form);
        s->generic_flags |= TB_LINKER_SECTION_DISCARD;
        goto done;
    }

    // sort
    CUIK_TIMED_BLOCK("sort section") {
        qsort(array_form, dyn_array_length(array_form), sizeof(TB_LinkerSectionPiece*), compare_linker_pieces);
    }

    // convert back into linked list
    CUIK_TIMED_BLOCK("convert into list") {
        // bool log = s->name.length > 3 && s->name.data[1] == 'C' && s->name.data[2] == 'R';
        // if (log) printf("\nSECTION %.*s\n", (int) s->name.length, s->name.data);

        size_t offset = 0;
        TB_LinkerSectionPiece* prev = NULL;
        dyn_array_for(j, array_form) {
            size_t mask = (1u << array_form[j]->align_log2) - 1;
            size_t next = (offset + mask) & ~mask;
            if (j > 0) {
                array_form[j - 1]->size += next - offset;
            }
            offset = next;

            // if (log) printf("  PIECE %06zx %06zx (align=%06x)\n", offset, array_form[j]->size, (1u << array_form[j]->align_log2));

            array_form[j]->offset = offset;
            offset += array_form[j]->size;
            prev = array_form[j];
        }

        s->size = offset;
        s->list = NULL;
        s->piece_count = 0xCAFEBABE;
        s->pieces = array_form;

        // log_debug("Section %.*s: %zu pieces with %zu bytes", (int) s->name.length, s->name.data, piece_count, offset);
    }

    done:
    cuikperf_region_end();
    tb_linker_job_done(l);
}

// static _Atomic int enqueued, dealt;
bool tb_linker_layout(TB_Linker* l) {
    // printf("JOB %d %d\n", enqueued, dealt);

    namehs_resize_barrier(&l->unresolved_symbols);
    if (nbhs_count(&l->unresolved_symbols) > 0) {
        NBHS_FOR(e, &l->unresolved_symbols) {
            TB_Slice* sym_name = e.k;
            printf("\x1b[31merror\x1b[0m: unresolved external: %.*s\n", (int) sym_name->length, sym_name->data);

            #if 0
            size_t i = 0;
            for (; u && i < 5; u = u->next, i++) {
                // walk input stack
                TB_LinkerInputHandle curr = u->reloc;
                printf("  in ");

                int depth = 0;
                while (curr != 0) {
                    TB_LinkerInput* input = &l->inputs[curr];

                    depth++;
                    if (depth) {
                        printf("(");
                    }

                    if (input->tag == TB_LINKER_INPUT_MODULE) {
                        printf("<tb-module %p>\n", input->module);
                    } else {
                        TB_Slice obj_name = as_filename(input->name);
                        printf("%.*s", (int) obj_name.length, obj_name.data);
                    }

                    curr = input->parent;
                }

                while (depth--) printf(")");
                printf("\n");
            }

            if (u) {
                // count the rest
                while (u) u = u->next, i++;

                printf("  ...and %zu more...\n", i - 5);
            }
            printf("\n");
            #endif
        }

        return false;
    }

    DynArray(TB_LinkerSection*) sections = NULL;
    CUIK_TIMED_BLOCK("sort section pieces") {
        size_t num = 0;
        NBHS_FOR(e, &l->sections) {
            TB_LinkerSection* s = e.k;
            if ((s->generic_flags & TB_LINKER_SECTION_DISCARD) || s->piece_count == 0) {
                continue;
            }

            void* args[2] = { l, s };
            l->jobs.count += 1;
            if (l->jobs.pool != NULL && tpool_num_threads(l->jobs.pool) > 1) {
                tpool_add_task2(l->jobs.pool, false, tb_linker_sort_section, 2, args);
            } else {
                tb_linker_sort_section(l->jobs.pool, args);
            }
            dyn_array_put(sections, s);
        }

        for (size_t i = 0; i < dyn_array_length(sections);) {
            if (sections[i]->generic_flags & TB_LINKER_SECTION_DISCARD) {
                dyn_array_remove(sections, i);
            } else {
                i += 1;
            }
        }
    }

    cuikperf_region_start("sort sections", NULL);
    qsort(sections, dyn_array_length(sections), sizeof(TB_LinkerSection*), compare_linker_sections);
    l->sections_arr = sections;
    cuikperf_region_end();

    cuikperf_region_start("wait for sort", NULL);
    tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
    cuikperf_region_end();

    // get or add linker segment
    DynArray(TB_LinkerSegment*) segments = NULL;
    dyn_array_for(i, sections) {
        TB_Slice name = sections[i]->name;

        // sections with the same name before the dollar sign will be combined.
        int dollar = find_char(name, '$');
        if (dollar < name.length) {
            name.length = dollar;
        }

        size_t j = 0, segment_count = dyn_array_length(segments);
        for (; j < segment_count; j++) {
            if (segments[j]->name.length == name.length && memcmp(segments[j]->name.data, name.data, name.length) == 0) {
                break;
            }
        }

        TB_LinkerSegment* segment;
        if (j == segment_count) {
            segment = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSegment));
            *segment = (TB_LinkerSegment){
                .name   = name,
                .number = segment_count,
                .flags  = sections[i]->flags,
            };
            dyn_array_put(segments, segment);
        } else {
            segment = segments[j];
            segment->flags  = sections[i]->flags;
        }

        sections[i]->segment = segment;
        dyn_array_put(segment->sections, sections[i]);
    }

    // sort sections inside the segment
    dyn_array_for(i, segments) {
        qsort(segments[i]->sections, dyn_array_length(segments[i]->sections), sizeof(TB_LinkerSection*), compare_linker_sections);

        // layout offsets next to each other in the segment
        size_t offset = 0;
        dyn_array_for(j, segments[i]->sections) {
            TB_LinkerSection* sec = segments[i]->sections[j];

            /*TB_ASSERT(dyn_array_length(sec->pieces) > 0);
            size_t first_align_log2 = sec->pieces[0]->align_log2;

            size_t mask = (1u << first_align_log2) - 1;
            size_t next = (offset + mask) & ~mask;
            if (j > 0 && offset != next) {
            // add padding to the end of the section
            TB_LinkerSection* prev = segments[i]->sections[j - 1];
            TB_ASSERT(prev->offset + prev->size == offset);

            size_t pad = next - offset;

            prev->pieces[dyn_array_length(prev->pieces) - 1]->size += pad;
            prev->size += pad;
            }*/

            sec->offset = offset;
            offset += sec->size;
        }
        segments[i]->size = offset;
    }
    l->segments = segments;

    return true;
}

////////////////////////////////
// Mark phase
////////////////////////////////
enum { MARK_WORKLIST_SIZE = 128 };

typedef struct {
    int top;
    TB_LinkerSectionPiece* elems[MARK_WORKLIST_SIZE];
} MarkJob;

bool tb_linker_mark_piece(TB_Linker* l, TB_LinkerSectionPiece* p) {
    if (p->size == 0 || (p->parent->generic_flags & TB_LINKER_SECTION_DISCARD)) {
        return false;
    }

    TB_LinkerPieceFlags flags = atomic_load_explicit(&p->flags, memory_order_acquire);
    if ((flags & TB_LINKER_PIECE_LIVE) || !atomic_compare_exchange_strong(&p->flags, &flags, flags | TB_LINKER_PIECE_LIVE)) {
        return false;
    }

    return true;
}

// returns true if we marked it
bool tb_linker_mark_symbol(TB_Linker* l, TB_LinkerSymbol* sym) {
    if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
        TB_LinkerSymbol* alt = tb_linker_root_symbol(l, atomic_load_explicit(&sym->weak_alt, memory_order_relaxed));
        if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN && alt->tag != TB_LINKER_SYMBOL_LAZY) {
            // we could make this the leader to path compress
            sym = alt;
        } else {
            namehs_intern(&l->unresolved_symbols, &sym->name);
            return false;
        }
    }

    // add import to table
    if (sym->tag == TB_LINKER_SYMBOL_IMPORT || sym->tag == TB_LINKER_SYMBOL_THUNK) {
        TB_LinkerSymbol* imp_sym = sym;
        if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
            imp_sym = sym->thunk;
        }

        // gotta mark the import sym to avoid duplicates
        TB_LinkerSymbolFlags flags = atomic_load_explicit(&imp_sym->flags, memory_order_acquire);
        if ((flags & TB_LINKER_SYMBOL_USED) || !atomic_compare_exchange_strong(&imp_sym->flags, &flags, flags | TB_LINKER_SYMBOL_USED)) {
            // If we lose, someone else must've marked it
            assert(flags & TB_LINKER_SYMBOL_USED);
            return false;
        }

        // TODO(NeGate): consider a concurrent data structure here to avoid
        // slamming this lock...
        cuikperf_region_start2("import", imp_sym->name.length, (const char*) imp_sym->name.data);
        ImportTable* imp_table = imp_sym->import.table;
        mtx_lock(&imp_table->lock);
        if (imp_table == NULL) {
            imp_table->thunks = dyn_array_create(TB_LinkerSymbol*, 256);
        }
        dyn_array_put(imp_table->thunks, imp_sym);
        mtx_unlock(&imp_table->lock);
        cuikperf_region_end();
        return false;
    }

    if (sym->tag != TB_LINKER_SYMBOL_NORMAL && sym->tag != TB_LINKER_SYMBOL_TB) {
        return false;
    }

    return true;
}

static void tb_linker_mark_job(TPool* pool, void** args);
void bcache_mark_response(TB_Linker* l, void* arg, TB_Slice content, bool io_thread) {
    l->jobs.count += 1;
    void* args[2] = { l, arg };
    if (io_thread) {
        tpool_io_forward(l->jobs.pool, true, tb_linker_mark_job, 2, args);
    } else {
        tpool_add_task2(l->jobs.pool, true, tb_linker_mark_job, 2, args);
    }
}

enum {
    RELOC_ALIGNMENT = 128*1024,
};

// doesn't change the mark stuff, you need to check that earlier. this
// only handles offloading a mark job to another worker.
static void tb_linker_enqueue_piece(TB_Linker* l, TB_LinkerSectionPiece* p) {
    size_t base = p->reloc_pos & -RELOC_ALIGNMENT;
    size_t limit = (p->reloc_pos + p->reloc_size + RELOC_ALIGNMENT - 1) & -RELOC_ALIGNMENT;
    if (limit > p->obj->file->size) {
        limit = p->obj->file->size;
    }

    // tb_linker_read_req(l, p->obj->file, base, limit - base, NULL, p, bcache_mark_response);
    /* if (!tpool_is_high_io_load(l->jobs.pool)) {
    tb_linker_prefetch(l, p->obj->file, base, limit - base);
    } */

    l->jobs.count += 1;
    void* args[2] = { l, p };
    tpool_add_task2(l->jobs.pool, true, tb_linker_mark_job, 2, args);
}

static bool tb_linker_is_piece_leaf(TB_LinkerSectionPiece* p) {
    return p->reloc_count == 0 && dyn_array_length(p->assoc) == 0 && p->obj->module == NULL;
}

static void tb_linker_mark_push(MarkJob* job, TB_Linker* l, TB_LinkerSectionPiece* p) {
    if (p != NULL && tb_linker_mark_piece(l, p) && !tb_linker_is_piece_leaf(p)) {
        size_t base = p->reloc_pos & -RELOC_ALIGNMENT;
        size_t limit = (p->reloc_pos + p->reloc_size + RELOC_ALIGNMENT - 1) & -RELOC_ALIGNMENT;
        if (limit > p->obj->file->size) {
            limit = p->obj->file->size;
        }

        __builtin_debugtrap(); // TODO
        /* if (job->top < MARK_WORKLIST_SIZE) {
        // tb_linker_prefetch(l, p->obj->file, base, limit - base);
        job->elems[job->top++] = p;
        } else {
        tb_linker_read_req(l, p->obj->file, base, limit - base, NULL, p, bcache_mark_response);
        } */

        #if 0
        // we can only put tasks with cached relocations into the private worklists
        if (tb_linker_read_req_FAST(l, p->obj->file, base, limit - base, NULL, p, bcache_mark_response)) {
            if (job->top < MARK_WORKLIST_SIZE) {
                job->elems[job->top++] = p;
            } else {
                l->jobs.count += 1;
                void* args[2] = { l, p };
                tpool_add_task2(l->jobs.pool, tb_linker_mark_job, 2, args);
            }
        }
        #endif
    }
}

static void tb_linker_mark_push2(MarkJob* job, TB_Linker* l, TB_LinkerSymbol* sym) {
    if (tb_linker_mark_symbol(l, sym)) {
        // cuikperf_region_start("push", NULL);
        TB_LinkerSectionPiece* p = tb_linker_get_piece(l, sym);
        tb_linker_mark_push(job, l, p);
        // cuikperf_region_end();
    }
}

static void tb_linker_mark_job(TPool* pool, void** args) {
    cuikperf_region_start("visit", NULL);
    TB_Linker* l = args[0];
    tb_linker_worker_init(l);

    TB_LinkerSectionPiece* p = args[1];
    assert(p->obj != NULL);
    assert(p->relocs != NULL || p->reloc_size == 0);

    #ifdef CONFIG_HAS_TB
    RelocParser parse_reloc = p->obj && p->obj->module ? tb__linker_module_parse_reloc : l->vtbl.parse_reloc;
    #else
    RelocParser parse_reloc = l->vtbl.parse_reloc;
    #endif

    // To avoid placing too many tiny jobs on the workpool, we'll try
    // to use a private worklist for a tiny amount of the pieces and
    // if we overflow it then we'll push things out.
    MarkJob ctx;

    // root piece
    ctx.elems[0] = p;
    ctx.top = 1;

    while (ctx.top > 0) {
        TB_LinkerSectionPiece* p = ctx.elems[--ctx.top];

        __builtin_debugtrap(); // TODO
        /* // Grab the data, if it's
        if (!tb_linker_read_req_FAST(l, p->obj->file, p->reloc_pos, p->reloc_size, NULL, p, bcache_mark_response)) {
        continue;
        } */

        cuikperf_region_start("v", NULL);
        if (!p->obj->live) {
            atomic_store_explicit(&p->obj->live, true, memory_order_relaxed);
        }

        // mark module content
        if (p->obj->module && !p->obj->module->visited) {
            p->obj->module->visited = true;

            #ifdef CONFIG_HAS_TB
            TB_Module* m = p->obj->module;
            dyn_array_for(i, m->sections) {
                tb_linker_mark_push(&ctx, l, m->sections[i].piece);
            }
            #endif

            // associate TB externals with linker symbols
            FOR_N(i, 0, m->exports.count) {
                if (&m->exports.data[i]->super == m->chkstk_extern && m->uses_chkstk == 0) {
                    continue;
                }

                TB_External* ext = m->exports.data[i];
                TB_LinkerSymbol* sym = tb_linker_find_symbol2(l, ext->super.name);

                // HACK(NeGate): this isn't currently atomic... it'll work by
                // shear bs
                if (ext->super.address == NULL) {
                    ext->super.address = sym;
                }
                tb_linker_mark_push2(&ctx, l, sym);
            }
        }

        // mark the symbols attached to the relocations
        FOR_N(i, 0, p->reloc_count) {
            TB_LinkerReloc rel;
            parse_reloc(l, p, i, &rel);

            TB_LinkerSymbol* sym = tb_linker_root_symbol(l, rel.target);
            TB_LinkerSectionPiece* sym_piece = tb_linker_get_piece(l, sym);
            if (sym_piece == p) {
                // the piece is already marked, might as well skip all that code
                tb_linker_mark_symbol(l, sym);
            } else {
                tb_linker_mark_push2(&ctx, l, sym);
            }
        }

        // associated section
        if (dyn_array_length(p->assoc)) {
            dyn_array_for(i, p->assoc) {
                tb_linker_mark_push(&ctx, l, p->assoc[i]);
            }
        }
        cuikperf_region_end();
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

// bool tb_linker_push_piece(TB_Linker* l, TB_LinkerSectionPiece* p, int depth);

void tb_linker_push_named(TB_Linker* l, const char* name) {
    TB_LinkerSymbol* sym = tb_linker_find_symbol2(l, name);
    TB_LinkerSectionPiece* p = tb_linker_get_piece(l, sym);
    if (tb_linker_mark_symbol(l, sym) && !tb_linker_is_piece_leaf(p)) {
        tb_linker_enqueue_piece(l, p);
    }
}

void tb_linker_mark_live(TB_Linker* l) {
    tb_linker_push_named(l, l->entrypoint);

    // mark all non-COMDAT pieces as live
    cuikperf_region_start("root scan", NULL);

    #define MATCH(str) (s->name.length == sizeof(str)-1 && memcmp(s->name.data, str, s->name.length) == 0)

    #if 0
    NBHS_FOR(e, &l->sections) {
        TB_LinkerSection* s = e.k;
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) { continue; }

        // we don't consider .debug as roots because codeview is compiled into the PDB
        if (MATCH(".debug") || MATCH(".gfids$y") || MATCH(".gehcont$y")) {
            continue;
        }

        TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if ((p->flags & TB_LINKER_PIECE_COMDAT) == 0 && !tb_linker_is_piece_leaf(p)) {
                printf("A %p %u\n", p, p->reloc_count);
            }
        }
    }
    return;
    #endif

    NBHS_FOR(e, &l->sections) {
        TB_LinkerSection* s = e.k;
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) { continue; }

        // we don't consider .debug as roots because codeview is compiled into the PDB
        if (MATCH(".debug") || MATCH(".gfids$y") || MATCH(".gehcont$y")) {
            continue;
        }

        TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if ((p->flags & TB_LINKER_PIECE_COMDAT) == 0 &&
                tb_linker_mark_piece(l, p) &&
                !tb_linker_is_piece_leaf(p)) {
                tb_linker_enqueue_piece(l, p);
            }
        }
    }
    #undef MATCH
    cuikperf_region_end();

    if (l->jobs.pool) {
        #if CUIK_ALLOW_THREADS
        cuikperf_region_start("wait for scan", NULL);
        tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
        cuikperf_region_end();
        #else
        abort();
        #endif
    } else {
        cuikperf_region_start("mark", NULL);
        while (dyn_array_length(l->worklist)) {
            TB_LinkerSectionPiece* p = dyn_array_pop(l->worklist);
            void* args[2] = { l, p };
            tb_linker_mark_job(NULL, args);
        }
        cuikperf_region_end();
    }
}

////////////////////////////////
// Exporting
////////////////////////////////
enum {
    EXPORT_BROADCAST_BATCH = 64
};

static void tb_linker_export_job(TPool* pool, void** args);
void bcache_export_response(TB_Linker* l, void* arg, TB_Slice content, bool io_thread) {
    l->jobs.count += 1;
    void* args[2] = { l, arg };
    if (io_thread) {
        tpool_io_forward(l->jobs.pool, true, tb_linker_export_job, 2, args);
    } else {
        tpool_add_task2(l->jobs.pool, true, tb_linker_export_job, 2, args);
    }
}

// just run whatever reloc function from the spec
static int32_t resolve_reloc(TB_LinkerSymbol* sym, TB_ObjectRelocType type, uint64_t source_pos, uint64_t target_rva, int addend) {
    switch (type) {
        case TB_OBJECT_RELOC_ADDR32NB:
        return target_rva;

        case TB_OBJECT_RELOC_SECTION:
        if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE || sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
            return 0;
        } else {
            return sym->normal.piece->parent->segment->number;
        }

        case TB_OBJECT_RELOC_SECREL:
        if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE || sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
            return 0;
        } else {
            // return sym->normal.piece->parent->offset + sym->normal.piece->offset + sym->normal.secrel;
            return sym->normal.piece->offset + sym->normal.secrel;
        }

        case TB_OBJECT_RELOC_REL32:
        return (target_rva + addend) - source_pos;

        default:
        tb_todo();
    }
}

void tb__linker_module_parse_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, size_t reloc_i, TB_LinkerReloc* out_reloc) {
    const TB_LinkerReloc* relocs = p->relocs;
    *out_reloc = relocs[reloc_i];
}

size_t tb_linker_apply_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, uint32_t section_rva, uint32_t trampoline_rva, size_t reloc_i, size_t head, size_t tail) {
    #ifdef CONFIG_HAS_TB
    RelocParser parse_reloc = p->obj && p->obj->module ? tb__linker_module_parse_reloc : l->vtbl.parse_reloc;
    #else
    RelocParser parse_reloc = l->vtbl.parse_reloc;
    #endif

    size_t reloc_len = p->reloc_count;
    while (reloc_i < reloc_len) {
        TB_LinkerReloc rel;
        parse_reloc(l, p, reloc_i, &rel);

        int rel_size = rel.type == TB_OBJECT_RELOC_ADDR64 ? 8 : 4;

        // we only apply if it's not hanging off the right edge, if that's
        // the case we've fully loaded the memory we're overlaying.
        int dst_pos = rel.src_offset - head;
        if (rel.src_offset + rel_size > tail) {
            break;
        }

        // by this point, we've fully resolved the relocation
        TB_LinkerSymbol* sym = tb_linker_root_symbol(l, rel.target);
        if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
            TB_LinkerSymbol* alt = tb_linker_root_symbol(l, atomic_load_explicit(&sym->weak_alt, memory_order_relaxed));
            if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN && alt->tag != TB_LINKER_SYMBOL_LAZY) {
                sym = alt;
            }
        }
        TB_ASSERT(sym && sym->tag != TB_LINKER_SYMBOL_UNKNOWN && sym->tag != TB_LINKER_SYMBOL_LAZY);
        uint64_t src_rva = section_rva + p->offset + rel.src_offset;

        // resolve source location
        uint64_t target_rva = 0;
        if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE) {
            target_rva = 0;
        } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            target_rva = l->iat_pos + (sym->import.thunk_id * 8);
        } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
            TB_LinkerSymbol* import_sym = sym->thunk;
            target_rva = trampoline_rva + import_sym->import.ds_address;
        } else {
            target_rva = tb__get_symbol_rva(sym);
        }

        // printf("RELOC S=%#zx T=%#zx, A=%d (%.*s)\n", src_rva, target_rva, rel.addend, (int) sym->name.length, sym->name.data);
        if (rel.type == TB_OBJECT_RELOC_ADDR64) {
            if (sym->tag != TB_LINKER_SYMBOL_ABSOLUTE) {
                target_rva += 0x140000000;
            }

            // we write out the fake VA and the base relocs will fix it up
            int64_t* dst = (int64_t*) &out[dst_pos];
            *dst += target_rva;
        } else {
            int32_t* dst = (int32_t*) &out[dst_pos];
            *dst += resolve_reloc(sym, rel.type, src_rva, target_rva, rel.addend);
        }
        reloc_i += 1;
    }
    return reloc_i;
}

// has_zero is when we know the output buffer is already zeroed
static void tb_linker_export_piece(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, bool has_zero) {
    size_t section_rva = tb_linker_section_rva(p->parent);
    size_t buffer_size = p->buffer_size;
    if (p->kind == PIECE_BUFFER) {
        memcpy(out, p->buffer, buffer_size);
    } else if (p->kind == PIECE_FILE) {
        // grab from cache
        TB_LinkerObject* obj = p->obj;
        memcpy(out, &obj->file->raw_map[p->file_offset], buffer_size);
    }

    // zero the remaining space (or CC if it's code)
    int b = (p->flags & TB_LINKER_PIECE_CODE) ? 0xCC : 0;
    if (p->size > buffer_size && (!has_zero || b != 0)) {
        memset(&out[buffer_size], b, p->size - buffer_size);
    }

    if (p->reloc_count > 0) CUIK_TIMED_BLOCK("reloc") {
        tb_linker_apply_reloc(l, p, out, section_rva, l->trampoline_rva, 0, 0, p->size);
    }
}

void tb_linker_export_job(TPool* pool, void** args) {
    cuikperf_region_start("export", NULL);

    TB_Linker* l = args[0];
    TB_LinkerSectionPiece* p = args[1];
    tb_linker_worker_init(l);

    size_t section_file_offset = tb_linker_section_file_pos(p->parent);
    uint8_t* out = &l->output[section_file_offset + p->offset];
    tb_linker_export_piece(l, p, out, true);

    cuikperf_region_end();
    tb_linker_job_done(l);
}

static void tb_linker_broadcast_pieces(TPool* pool, void** args) {
    TB_Linker* l = args[0];
    TB_LinkerSection* section = args[1];
    size_t base = (size_t) args[2];

    cuikperf_region_start("broadcast", (const char*) section->name.data);
    size_t limit = base + EXPORT_BROADCAST_BATCH;
    if (limit > dyn_array_length(section->pieces)) {
        limit = dyn_array_length(section->pieces);
    }

    TB_LinkerSection* text  = tb_linker_find_section(l, ".text");
    uint32_t trampoline_rva = text->segment->address + l->trampoline_pos;

    // issue cached read requests, if they're ready now we handle them now
    // if not we'll handle them when the read comes back.
    int io_batch_size = 0;
    FOR_N(j, base, limit) {
        TB_LinkerSectionPiece* p = section->pieces[j];
        if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind == PIECE_FILE) {
            __builtin_debugtrap(); // TODO
            /* if (tb_linker_read_req_FAST(l, p->obj->file, p->file_offset, p->buffer_size, NULL, p, bcache_export_response)) {
            size_t section_file_offset = tb_linker_section_file_pos(p->parent);
            uint8_t* out = &l->output[section_file_offset + p->offset];
            tb_linker_export_piece(l, p, out, true);
            } */
            io_batch_size++;
        }
    }

    if (io_batch_size < EXPORT_BROADCAST_BATCH) {
        // for buffer copies we can apply the relocations now
        FOR_N(j, base, limit) {
            TB_LinkerSectionPiece* p = section->pieces[j];
            if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind == PIECE_BUFFER) {
                size_t section_file_offset = tb_linker_section_file_pos(p->parent);
                uint8_t* out = &l->output[section_file_offset + p->offset];
                tb_linker_export_piece(l, p, out, true);
            }
        }
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

void tb_linker_export_pieces(TB_Linker* l) {
    DynArray(TB_LinkerSection*) sections = l->sections_arr;

    uint8_t* output = l->output;
    if (l->jobs.pool != NULL) {
        #if CUIK_ALLOW_THREADS
        l->jobs.done = 0;
        l->jobs.count = 0;

        // each of the pieces can be exported in parallel
        cuikperf_region_start("submitting", NULL);
        dyn_array_for(i, sections) {
            size_t count = dyn_array_length(sections[i]->pieces);
            for (size_t j = 0; j < count; j += EXPORT_BROADCAST_BATCH) {
                cuikperf_region_start("submit", NULL);
                l->jobs.count += 1;
                void* args[3] = { l, sections[i], (void*) j };
                tpool_add_task2(l->jobs.pool, false, tb_linker_broadcast_pieces, 3, args);
                cuikperf_region_end();
            }
        }
        cuikperf_region_end();
        tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
        #else
        abort();
        #endif
    } else {
        assert(0 && "TODO");

        cuikperf_region_start("export pieces", NULL);
        dyn_array_for(i, sections) {
            dyn_array_for(j, sections[i]->pieces) {
                TB_LinkerSectionPiece* p = sections[i]->pieces[j];
                if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
                    void* args[2] = { l, p };
                    tb_linker_export_job(NULL, args);
                }
            }
        }
        cuikperf_region_end();
    }
}

#define XXH_STATIC_LINKING_ONLY
#define XXH_IMPLEMENTATION
#include "xxhash.h"