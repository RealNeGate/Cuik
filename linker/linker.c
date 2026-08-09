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
    h = tb__murmur3_mix(h, obj->fd);
    h = tb__murmur3_mix(h, obj->offset);
    return tb__murmur3_finalize(h, 8);
}

static bool objhs_cmp(const void* a, const void* b) {
    const TB_LinkerObject* aa = a;
    const TB_LinkerObject* bb = b;
    return aa->fd == bb->fd && aa->offset == bb->offset;
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
void tb_linker_read_imm(int fd, size_t offset, size_t size, void* data) {
    cuikperf_region_start("pread", NULL);
    pread(fd, data, size, offset);
    cuikperf_region_end();
}

static thread_local char* linker_bump_base;
static thread_local char* linker_bump_mark;

void* tb_linker_moar_mem(size_t size) {
    cuikperf_region_start("alloc", NULL);
    if (linker_bump_base == NULL) {
        linker_bump_base = cuik__valloc(1ull << 30ull);
        linker_bump_mark = linker_bump_base;
    }

    assert(linker_bump_mark - linker_bump_base < (1ull << 30ull));
    size = (size + 63) & ~63ull;

    char* ptr = linker_bump_mark;
    linker_bump_mark += size;
    cuikperf_region_end();

    return ptr;
}

void tb_linker_read_req(TB_Linker* l, size_t offset, size_t size, void* buffer, TB_LinkerObject* obj) {
    log_trace("read_req(%zu, %zu, %p)", offset, size, buffer);

    cuikperf_region_start("read_req", NULL);
    tpool_io_read(l->jobs.pool, obj->fd, offset, size, buffer, l->vtbl.add_input, obj, NULL, NULL, &obj->io_rem);
    cuikperf_region_end();
}

void tb_linker_read_req2(TB_Linker* l, int fd, size_t offset, size_t size, void* buffer, tpool_task_proc* fn) {
    assert(fn != NULL);
    log_trace("read_req(%zu, %zu, %p)", offset, size, buffer);

    cuikperf_region_start("read_req", NULL);
    tpool_io_read(l->jobs.pool, fd, offset, size, buffer, fn, l, buffer, ((char*) buffer) + size, NULL);
    cuikperf_region_end();
}

void tb_linker_read_req3(TB_Linker* l, int fd, size_t offset, size_t size, void* buffer, tpool_task_proc* fn, void* arg) {
    assert(fn != NULL);
    log_trace("read_req(%zu, %zu, %p)", offset, size, buffer);

    cuikperf_region_start("read_req", NULL);
    tpool_io_read(l->jobs.pool, fd, offset, size, buffer, fn, l, arg, NULL, NULL);
    cuikperf_region_end();
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
        tpool_add_task(l->jobs.pool, fn, arg);
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
        tpool_add_task2(l->jobs.pool, fn, count, args);
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

    l->symbols  = nbhm_alloc(16384);
    l->sections = nbhs_alloc(16);
    l->imports  = nbhs_alloc(256);
    l->libs     = nbhs_alloc(32);
    l->objects  = nbhs_alloc(256);
    l->unresolved_symbols = nbhs_alloc(16);

    CUIK_TIMED_BLOCK("I/O thread prep") {
        tpool_io_prep_all(l->jobs.pool);
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
        if ((sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_IMPORT) && (sym->flags & TB_LINKER_SYMBOL_USED)) {
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

void tb_linker_forward_input(TPool* pool, void** args) {
    cuikperf_region_start("forward", NULL);
    TB_LinkerObject* obj = args[0];
    TB_Linker* l = obj->linker;
    const char* file_name = (const char*) obj->name.data;
    tb_linker_worker_init(obj->linker);

    size_t size;
    int fd = open_file(file_name, &size);
    if (fd < 0) {
        printf("tblink: could not find file: %s\n", file_name);
        goto done;
    }

    obj->fd = fd;
    obj->size = size;
    obj->io_rem = 1;
    obj->prefetch_page = tb_linker_moar_mem(PREFETCH_BLOCK_SIZE);
    tb_linker_read_req(l, 0, PREFETCH_BLOCK_SIZE, obj->prefetch_page, obj);

    done:
    cuikperf_region_end();
}

void tb_linker_append_object(TB_Linker* l, const char* file_name) {
    CUIK_TIMED_BLOCK("append_obj") {
        TB_LinkerObject* obj = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
        *obj = (TB_LinkerObject){
            .name   = { (const uint8_t*) file_name, strlen(file_name) },
            .linker = l,
            .time   = atomic_fetch_add(&l->time, 0x100000000),
        };
        // Push to a pool thread so it's work can be distributed
        tb_linker_job_submit_1(l, tb_linker_forward_input, obj);
    }
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

    log_info("Loading library: %s", resolved_path);
    size_t newlen = strlen(resolved_path);
    char* newstr = linker_newstr(newlen, resolved_path);

    TB_LinkerArchive* lib_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerArchive));
    *lib_file = (TB_LinkerArchive){ {
            .name   = { (const uint8_t*) newstr, newlen },
            .linker = l,
            .time   = t,
            .fd     = fd,
            .size   = size,
        } };

    lib_file->header.io_rem = 1;
    lib_file->header.prefetch_page = tb_linker_moar_mem(PREFETCH_BLOCK_SIZE);
    tb_linker_read_req(l, 0, PREFETCH_BLOCK_SIZE, lib_file->header.prefetch_page, &lib_file->header);

    done:
    cuikperf_region_end();
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

        l->jobs.count += dyn_array_length(l->worklist);
        dyn_array_for(i, l->worklist) {
            TB_LinkerObject* obj = l->worklist[i];
            tpool_add_task(l->jobs.pool, l->vtbl.add_input, obj);
        }
        dyn_array_clear(l->worklist);

        tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
        #else
        abort(); // Unreachable
        #endif
    } else {
        // process all items on the worklist
        while (dyn_array_length(l->worklist)) {
            TB_LinkerObject* obj = dyn_array_pop(l->worklist);
            l->vtbl.add_input(NULL, (void**) &obj);
        }
    }
    cuikperf_region_end();
}

void tb_linker_complete_appends(TB_Linker* l) {
    cuikperf_region_start("complete", NULL);
    bool repeat;
    do {
        tb_linker_barrier(l);

        l->defer_jobs = true;

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

        l->defer_jobs = false;
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
    b->comdat_parent = a;
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
    if (sym == NULL || (sym->flags & TB_LINKER_SYMBOL_GLOBAL) == 0) {
        return sym;
    }
    return symhs_get(&l->symbols, &sym->name);
}

TB_LinkerSection* tb_linker_find_section(TB_Linker* l, const char* name) {
    TB_Slice str = { (const uint8_t*) name, strlen(name) };
    return namehs_get(&l->sections, &str);
}

TB_LinkerSection* tb_linker_find_or_create_section(TB_Linker* l, size_t name_len, const char* name, uint32_t flags) {
    TB_LinkerSection* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSection));
    *s = (TB_LinkerSection){ .name = { (const uint8_t*) name, name_len }, .flags = flags };

    TB_LinkerSection* k = namehs_intern(&l->sections, s);
    if (s != k) {
        tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
        return k;
    }
    return s;
}

TB_LinkerSectionPiece* tb_linker_append_piece(TB_LinkerSection* section, int kind, size_t size, TB_LinkerObject* obj) {
    TB_LinkerSectionPiece* piece = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSectionPiece));
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

void tb__set_weak_sym(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerSymbol* alt) {
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

void tb_linker_symbol_weak(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerSymbol* alt) {
    log_debug("WEAK %.*s", (int) sym->name.length, sym->name.data);

    TB_LinkerSymbol* next = sym;
    do {
        sym = next;
        tb__set_weak_sym(l, sym, alt);
        next = symhs_get(&l->symbols, &sym->name);
    } while (sym != next);
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
        // If the symbol is already defined, don't load the lazy one.
        if (is_symbol_defined(a)) {
            return false;
        } else if (is_symbol_defined(b)) {
            return true;
        } else {
            // Whichever symbol is lazy gets resolved
            if (b->tag == TB_LINKER_SYMBOL_LAZY) {
                tb_linker_lazy_resolve(l, b);
                return true;
            } else {
                assert(a->tag == TB_LINKER_SYMBOL_LAZY);
                tb_linker_lazy_resolve(l, a);
                return false;
            }
        }
    } else if (a->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        // if we're both unresolved then we don't need to do shit yet
        return b->tag != TB_LINKER_SYMBOL_UNKNOWN;
    } else if (b->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        return false;
    } else if (a->comdat != TB_LINKER_COMDAT_NONE || b->comdat != TB_LINKER_COMDAT_NONE) {
        // if only one is COMDAT, we always pick the COMDAT one
        if (a->comdat == TB_LINKER_COMDAT_NONE) {
            return true;
        } else if (b->comdat == TB_LINKER_COMDAT_NONE) {
            return false;
        } else {
            // COMDAT, we need to decide which of these lives but for now we don't care.
            uint64_t old_order = a->tag == TB_LINKER_SYMBOL_NORMAL ? a->normal.piece->order : 0;
            uint64_t sym_order = b->tag == TB_LINKER_SYMBOL_NORMAL ? b->normal.piece->order : 0;
            return old_order > sym_order;
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
    TB_LinkerArchive* lib = sym->lazy.lib;
    uint32_t offset = sym->lazy.offset;

    // create object file, we don't know the name
    // of it just yet but that'll be filled in soon.
    TB_LinkerObject* obj = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
    *obj = (TB_LinkerObject){
        .linker = l,
        .time   = lib->header.time + offset,
        .parent = &lib->header,
        .fd = lib->header.fd, .offset = offset,
    };

    TB_LinkerObject* k = objhs_intern(&l->objects, obj);
    if (k != obj) {
        tb_arena_free(&linker_perm_arena, k, sizeof(TB_LinkerObject));
        return;
    }

    log_debug("Load object file at %d:%zu (for %.*s)", lib->header.fd, offset, (int) sym->name.length, sym->name.data);

    l->jobs.count += 1;
    obj->io_rem = 1;
    obj->prefetch_page = tb_linker_moar_mem(PREFETCH_BLOCK_SIZE);
    tb_linker_read_req(obj->linker, offset, PREFETCH_BLOCK_SIZE, obj->prefetch_page, obj);

    #if 0
    size_t slash = 0;
    FOR_REV_N(i, 0, obj->name.length) {
        if (obj->name.data[i] == '/' || obj->name.data[i] == '\\') {
            slash = i + 1;
            break;
        }
    }
    log_debug("Loaded %.*s for %.*s", (int) (obj->name.length - slash), obj->name.data + slash, (int) sym->name.length, sym->name.data);
    #endif
}

// if owned is true, the sym can be deleted if it's not inserted
TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* new_sym, bool owned) {
    TB_LinkerSymbol* sym = new_sym;
    sym->flags |= TB_LINKER_SYMBOL_GLOBAL;

    // printf("%.*s    %"PRIx32"\n", (int) sym->name.length, sym->name.data, tb__murmur3_32(sym->name.data, sym->name.length) & 65535);

    #if 0 // For debugging
    static const char sss[] = "__scrt_exe_initialize_mta";
    if (sym->name.length == sizeof(sss)-1 && memcmp((const char*) sym->name.data, sss, sizeof(sss)-1) == 0) {
        mtx_lock(&l->lock);
        printf("INSERT %.*s %d (%s)", (int) sym->name.length, sym->name.data, sym->comdat, tag_name(sym->tag));

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

    cuikperf_region_start("TX", NULL);
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

        // migrate the weak alternative up
        if (old != NULL) {
            TB_LinkerSymbol* weak_alt = atomic_load_explicit(&old->weak_alt, memory_order_acquire);
            if (weak_alt != NULL) {
                tb__set_weak_sym(l, new_sym, weak_alt);
            }
        }
    } while (!symhs_tx_commit(&tx, new_sym));
    symhs_tx_end(&l->symbols);
    cuikperf_region_end();

    // I think all calls have the "owned" as true? check that out later and maybe
    // decide we don't need it
    if (owned && tx.k != sym && old == sym) {
        tb_arena_free(&linker_perm_arena, new_sym, sizeof(TB_LinkerSymbol));
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

enum { EXPORT_BUFFER_SIZE = 64*1024 };
void tb_linker_export_piece(TPool* pool, void** args) {
    cuikperf_region_start("export", NULL);

    TB_Linker* l = args[0];
    tb_linker_worker_init(l);

    uint8_t* file = l->output;
    TB_LinkerSectionPiece* p = args[1];

    assert((p->flags & TB_LINKER_PIECE_LIVE) || p->kind == PIECE_BSS);

    TB_LinkerSection* text  = tb_linker_find_section(l, ".text");
    uint32_t trampoline_rva = text->segment->address + l->trampoline_pos;

    TB_ASSERT(p->kind == PIECE_BUFFER);
    size_t section_rva = tb_linker_section_rva(p->parent);
    size_t section_file_offset = tb_linker_section_file_pos(p->parent);

    size_t head = 0, reloc_i = 0;
    while (head < p->size) {
        uint8_t* out = &file[section_file_offset + p->offset + head];

        // copy from input stream
        size_t tail;
        CUIK_TIMED_BLOCK("read") {
            tail = head + EXPORT_BUFFER_SIZE;
            if (tail > p->size) { tail = p->size; }

            size_t rem = 0;
            if (p->buffer) {
                size_t buffer_tail = tail > p->buffer_size ? p->buffer_size : tail;
                memcpy(out, &p->buffer[head], buffer_tail - head);
                rem = buffer_tail - head;
            }

            // zero the remaining space (or CC if it's code)
            if (rem < (tail - head)) {
                int b = (p->flags & TB_LINKER_PIECE_CODE) ? 0xCC : 0;
                memset(&out[rem], b, (tail - head) - rem);
            }
        }

        // apply relocations
        CUIK_TIMED_BLOCK("relocs") {
            reloc_i = tb_linker_apply_reloc(l, p, out, section_rva, trampoline_rva, reloc_i, head, tail);
        }
        head = tail;
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

static void tb_linker_broadcast_pieces(TPool* pool, void** args) {
    TB_Linker* l = args[0];
    TB_LinkerSection* section = args[1];

    cuikperf_region_start("broadcast", (const char*) section->name.data);

    dyn_array_for(j, section->pieces) {
        TB_LinkerSectionPiece* p = section->pieces[j];
        if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
            // Must increment before the job is dispatched
            l->jobs.count += 1;

            cuikperf_region_start("submit", NULL);
            void* args[2] = { l, p };
            tpool_add_task2(l->jobs.pool, tb_linker_export_piece, 2, args);
            cuikperf_region_end();
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
            #if 1
            l->jobs.count += 1;

            cuikperf_region_start("submit", NULL);
            void* args[2] = { l, sections[i] };
            tpool_add_task2(l->jobs.pool, tb_linker_broadcast_pieces, 2, args);
            cuikperf_region_end();
            #else
            int c = 0;
            dyn_array_for(j, sections[i]->pieces) {
                TB_LinkerSectionPiece* p = sections[i]->pieces[j];
                if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
                    cuikperf_region_start("submit", NULL);
                    void* args[2] = { l, p };
                    tpool_add_task2(l->jobs.pool, tb_linker_export_piece, 2, args);
                    c++;
                    cuikperf_region_end();
                }
            }
            l->jobs.count += c;
            #endif
        }
        cuikperf_region_end();
        tpool_wait_for_jobs(l->jobs.pool, &l->jobs.done, &l->jobs.count);
        #else
        abort();
        #endif
    } else {
        cuikperf_region_start("export pieces", NULL);
        dyn_array_for(i, sections) {
            dyn_array_for(j, sections[i]->pieces) {
                TB_LinkerSectionPiece* p = sections[i]->pieces[j];
                if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
                    void* args[2] = { l, p };
                    tb_linker_export_piece(NULL, args);
                }
            }
        }
        cuikperf_region_end();
    }
}

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

bool tb_linker_layout(TB_Linker* l) {
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
    CUIK_TIMED_BLOCK("sort sections") {
        size_t num = 0;
        NBHS_FOR(e, &l->sections) {
            TB_LinkerSection* s = e.k;
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) {
                continue;
            }

            size_t piece_count = s->piece_count;
            TB_ASSERT(piece_count != 0);

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
                continue;
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

            dyn_array_put(sections, s);
        }
    }
    qsort(sections, dyn_array_length(sections), sizeof(TB_LinkerSection*), compare_linker_sections);
    l->sections_arr = sections;

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

static TB_LinkerSymbol* resolve_external(TB_Linker* l, TB_External* ext) {
    TB_LinkerSymbol* sym = tb_linker_find_symbol2(l, ext->super.name);
    if (sym == NULL || sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        namehs_intern(&l->unresolved_symbols, &sym->name);
    } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
        sym->thunk->flags |= TB_LINKER_SYMBOL_USED;
    }

    ext->super.address = sym;
    sym->flags |= TB_LINKER_SYMBOL_USED;
    return sym;
}

static void tb_linker_mark_piece(TPool* pool, void** args) {
    cuikperf_region_start("visit", NULL);

    TB_Linker* l = args[0];
    tb_linker_worker_init(l);

    TB_LinkerSectionPiece* p = args[1];
    if (p->relocs == NULL && p->reloc_size > 0) {
        cuikperf_region_start("alloc", NULL);
        uint8_t* buf = tb_linker_moar_mem(p->reloc_size);
        p->relocs = buf;
        cuikperf_region_end();

        tb_linker_read_req3(l, p->obj->fd, p->reloc_pos, p->reloc_size, buf, tb_linker_mark_piece, p);
        cuikperf_region_end();
        return;
    }

    if (p->obj) {
        atomic_store_explicit(&p->obj->live, true, memory_order_relaxed);
    }
    // printf("Walk: %#llx (%zu, %.*s)\n", p->order, p->reloc_count, (int) p->obj->name.length, p->obj->name.data);

    // associated section
    dyn_array_for(i, p->assoc) {
        tb_linker_push_piece(l, p->assoc[i]);
    }

    // mark module content
    if (p->obj->module && !p->obj->module->visited) {
        p->obj->module->visited = true;

        #ifdef CONFIG_HAS_TB
        TB_Module* m = p->obj->module;
        dyn_array_for(i, m->sections) {
            if (m->sections[i].piece) {
                tb_linker_push_piece(l, m->sections[i].piece);
            }
        }
        #endif

        // associate TB externals with linker symbols
        FOR_N(i, 0, m->exports.count) {
            if (&m->exports.data[i]->super == m->chkstk_extern && m->uses_chkstk == 0) {
                continue;
            }

            TB_LinkerSymbol* sym = resolve_external(l, m->exports.data[i]);
            TB_LinkerSectionPiece* piece = tb_linker_get_piece(l, sym);
            if (piece) {
                tb_linker_push_piece(l, piece);
            }
        }
    }

    #ifdef CONFIG_HAS_TB
    RelocParser parse_reloc = p->obj && p->obj->module ? tb__linker_module_parse_reloc : l->vtbl.parse_reloc;
    #else
    RelocParser parse_reloc = l->vtbl.parse_reloc;
    #endif

    // mark any relocations:
    //   by this point, the symbols aren't being fought for so we really should
    // use relaxed loads when possible (might matter for ARM but not x86)
    FOR_N(i, 0, p->reloc_count) {
        TB_LinkerReloc rel;
        parse_reloc(l, p, i, &rel);

        TB_LinkerSymbol* sym = tb_linker_root_symbol(l, rel.target);
        tb_linker_push_symbol(l, sym);
    }

    cuikperf_region_end();
    tb_linker_job_done(l);
}

bool tb_linker_push_piece(TB_Linker* l, TB_LinkerSectionPiece* p) {
    if (p->size == 0 || (p->parent->generic_flags & TB_LINKER_SECTION_DISCARD)) {
        return false;
    }

    TB_LinkerPieceFlags flags = atomic_load_explicit(&p->flags, memory_order_acquire);
    if ((flags & TB_LINKER_PIECE_LIVE) || !atomic_compare_exchange_strong(&p->flags, &flags, flags | TB_LINKER_PIECE_LIVE)) {
        return false;
    }

    if (l->jobs.pool) {
        l->jobs.count += 1;
        void* args[2] = { l, p };
        tpool_add_task2(l->jobs.pool, tb_linker_mark_piece, 2, args);
    } else {
        dyn_array_put(l->worklist, p);
    }
    return true;
}

void tb_linker_push_symbol(TB_Linker* l, TB_LinkerSymbol* sym) {
    if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
        TB_LinkerSymbol* alt = tb_linker_root_symbol(l, atomic_load_explicit(&sym->weak_alt, memory_order_relaxed));
        if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN && alt->tag != TB_LINKER_SYMBOL_LAZY) {
            // we could make this the leader to path compress
            sym = alt;
        } else {
            namehs_intern(&l->unresolved_symbols, &sym->name);
            return;
        }
    }

    TB_LinkerSymbolFlags flags = atomic_load_explicit(&sym->flags, memory_order_acquire);
    if (!atomic_compare_exchange_strong(&sym->flags, &flags, flags | TB_LINKER_SYMBOL_USED)) {
        // If we lose, someone else must've marked it
        assert(flags & TB_LINKER_SYMBOL_USED);
        return;
    }

    if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
        sym->thunk->flags |= TB_LINKER_SYMBOL_USED;
    }

    if (sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB) {
        tb_linker_push_piece(l, tb_linker_get_piece(l, sym));
    }
}

void tb_linker_push_named(TB_Linker* l, const char* name) {
    TB_LinkerSymbol* sym = tb_linker_find_symbol2(l, name);
    tb_linker_push_symbol(l, sym);
}

void tb_linker_mark_live(TB_Linker* l) {
    tb_linker_push_named(l, l->entrypoint);

    // mark all non-COMDAT pieces as live
    cuikperf_region_start("root scan", NULL);
    NBHS_FOR(e, &l->sections) {
        TB_LinkerSection* s = e.k;
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) { continue; }
        // we don't consider .debug as roots because codeview is compiled into the PDB
        if (s->name.length == sizeof(".debug")-1 && memcmp(s->name.data, ".debug", s->name.length) == 0) {
            continue;
        }

        TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if ((p->flags & TB_LINKER_PIECE_COMDAT) == 0) {
                tb_linker_push_piece(l, p);
            }
        }
    }
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
            tb_linker_mark_piece(NULL, args);
        }
        cuikperf_region_end();
    }
}

