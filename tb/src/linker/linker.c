#include "linker.h"
#include <file_map.h>

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

#define NBHS_FN(n) strhs_ ## n
#include <nbhs.h>

#define NBHS_FN(n) namehs_ ## n
#include <nbhs.h>

thread_local bool linker_thread_init;
thread_local TB_Arena linker_tmp_arena;
thread_local TB_Arena linker_perm_arena;

extern TB_LinkerVtbl tb__linker_pe, tb__linker_elf;
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
static void* muh_hs_alloc(size_t size) {
    void* ptr = tb_platform_heap_alloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void muh_hs_free(void* ptr, size_t size) {
    tb_platform_heap_free(ptr);
}

TB_Slice tb_linker_read_entire_file(FILE* file) {
    cuikperf_region_start("file", NULL);
    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        return (TB_Slice){ 0 };
    }

    size_t length = file_stats.st_size;
    void* data = tb_platform_heap_alloc(length);

    fseek(file, 0, SEEK_SET);
    fread(data, 1, length, file);
    fclose(file);
    cuikperf_region_end();

    return (TB_Slice){ data, length };
}

TB_Linker* tb_linker_create(TB_ExecutableType exe, TB_Arch arch, TPool* tp) {
    TB_Linker* l = tb_platform_heap_alloc(sizeof(TB_Linker));
    memset(l, 0, sizeof(TB_Linker));
    l->target_arch = arch;
    l->jobs.pool = tp;

    l->symbols  = nbhs_alloc(256, muh_hs_alloc, muh_hs_free);
    l->sections = nbhs_alloc(16,  muh_hs_alloc, muh_hs_free);
    l->imports  = nbhs_alloc(256, muh_hs_alloc, muh_hs_free);
    l->libs     = nbhs_alloc(32,  muh_hs_alloc, muh_hs_free);
    l->objects  = nbhs_alloc(256, muh_hs_alloc, muh_hs_free);
    l->unresolved_symbols = nbhs_alloc(16, muh_hs_alloc, muh_hs_free);

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
    char* newstr = tb_platform_heap_alloc(len + 1);
    memcpy(newstr, path, len);
    newstr[len] = 0;
    return newstr;
}

void tb_linker_add_libpath(TB_Linker* l, const char* path) {
    dyn_array_put(l->libpaths, linker_newstr(strlen(path), path));
}

void tb_linker_append_object(TB_Linker* l, const char* file_name) {
    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    CUIK_TIMED_BLOCK("append_obj") {
        FileMap fm = open_file_map_read(file_name);
        TB_LinkerObject* obj_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
        *obj_file = (TB_LinkerObject){
            { (const uint8_t*) file_name, strlen(file_name) },
            l,
            { fm.data, fm.size },
            atomic_fetch_add(&l->time, 0x100000000),
        };

        if (l->jobs.pool != NULL) {
            l->jobs.count += 1;
            tpool_add_task(l->jobs.pool, (tpool_task_proc*) l->vtbl.append_object, obj_file);
        } else {
            l->vtbl.append_object(NULL, obj_file);
        }
    }
}

void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    CUIK_TIMED_BLOCK("append_module") {
        __debugbreak();
    }
}

void tb_linker_append_library(TB_Linker* l, const char* file_name) {
    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    size_t len = strlen(file_name);
    char* str = tb_arena_alloc(&linker_perm_arena, len + 1);
    memcpy(str, file_name, len);
    str[len] = 0;

    if (strhs_intern(&l->libs, str) != str) {
        tb_arena_free(&linker_perm_arena, str, len + 1);
        return;
    }

    cuikperf_region_start("find lib", NULL);
    char test_path[FILENAME_MAX];
    FileMap fm;
    dyn_array_for(j, l->libpaths) {
        snprintf(test_path, FILENAME_MAX, "%s/%s", l->libpaths[j], file_name);
        fm = open_file_map_read(test_path);
        if (fm.data != NULL) {
            break;
        }
        test_path[0] = 0;
    }
    cuikperf_region_end();

    if (test_path[0] == 0) {
        fprintf(stderr, "cuiklink: could not find library: %s\n", file_name);
        dyn_array_for(j, l->libpaths) {
            snprintf(test_path, FILENAME_MAX, "%s/%s", l->libpaths[j], file_name);
            fprintf(stderr, "  searched at %s\n", test_path);
        }
        return;
    }

    /* CUIK_TIMED_BLOCK("load") {
        volatile const uint8_t* buf = fm.data;
        for (size_t i = 0; i < fm.size; i += 4096) {
            buf[i];
        }
    } */

    size_t newlen = strlen(test_path);
    char* newstr = linker_newstr(newlen, test_path);

    TB_LinkerObject* lib_file = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerObject));
    *lib_file = (TB_LinkerObject){
        { (const uint8_t*) newstr, newlen },
        l,
        { fm.data, fm.size },
        atomic_fetch_add(&l->time, 0x100000000),
    };

    if (l->jobs.pool != NULL) {
        l->jobs.count += 1;
        tpool_add_task(l->jobs.pool, (tpool_task_proc*) l->vtbl.append_library, lib_file);
    } else {
        l->vtbl.append_library(NULL, lib_file);
    }
}

void tb_linker_append_module_section(TB_Linker* l, TB_LinkerObject* mod, TB_ModuleSection* section, uint32_t flags) {
    assert(mod->module != NULL && "not a TB_Module's section?");
    if (section->total_size > 0) {
        TB_LinkerSection* ls = tb_linker_find_or_create_section(l, strlen(section->name), section->name, flags);

        section->piece = tb_linker_append_piece(ls, PIECE_MODULE_SECTION, section->total_size, mod);
        section->piece->obj = mod;
        section->piece->ir_section = section;
    }
}

bool tb_linker_export(TB_Linker* l, const char* file_name) {
    return l->vtbl.export(l, file_name);
}

void tb_linker_destroy(TB_Linker* l) {
    tb_platform_heap_free(l);
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

uint64_t tb__get_symbol_rva(TB_Linker* l, TB_LinkerSymbol* sym) {
    if (sym->tag == TB_LINKER_SYMBOL_ABSOLUTE) {
        return 0;
    } else if (sym->tag == TB_LINKER_SYMBOL_IMAGEBASE) {
        return sym->imagebase;
    }

    // normal or TB
    assert(sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB);
    TB_LinkerSectionPiece* piece = sym->normal.piece;

    uint32_t rva = piece->parent->address + piece->offset;
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

void tb_linker_associate(TB_Linker* l, TB_LinkerSectionPiece* a, TB_LinkerSectionPiece* b) {
    assert(a->assoc == NULL);
    a->assoc = b;
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
    return namehs_get(&l->symbols, &name);
}

TB_LinkerSymbol* tb_linker_find_symbol2(TB_Linker* l, const char* name) {
    TB_Slice str = { (const uint8_t*) name, strlen(name) };
    return namehs_get(&l->symbols, &str);
}

TB_LinkerSymbol* tb_linker_import_symbol(TB_Linker* l, TB_Slice name) {
    TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
    *s = (TB_LinkerSymbol){ .name = name, .tag = TB_LINKER_SYMBOL_UNKNOWN };

    TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
    if (new_s != s) {
        tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
        s = new_s;
    }
    return s;
}

TB_LinkerSymbol* tb_linker_new_symbol(TB_Linker* l, size_t len, const char* name) {
    TB_LinkerSymbol* s = tb_arena_alloc(&linker_perm_arena, sizeof(TB_LinkerSymbol));
    *s = (TB_LinkerSymbol){ .name = { (const uint8_t*) name, len }, .tag = TB_LINKER_SYMBOL_NORMAL };

    TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
    if (new_s != s) {
        tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
        s = new_s;
    }
    return s;
}

void tb_linker_symbol_weak(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerSymbol* alt) {
    // complete a CAS on the leader where no one writes to weak_sym, that would mean
    // there's only one alternate name attached to the symbol (as intended).
    for (;;) {
        TB_LinkerSymbol* expected = NULL;
        if (!atomic_compare_exchange_strong(&sym->weak_alt, &expected, alt)) {
            // TODO(NeGate): write a good error plz
            // abort();
        }

        TB_LinkerSymbol* parent = atomic_load_explicit(&sym->parent, memory_order_acquire);
        if (parent == NULL) {
            break;
        }
        sym = parent;
    }
}

// leader can't be in the symbol table btw
void tb_linker_symbol_union(TB_Linker* l, TB_LinkerSymbol* leader, TB_LinkerSymbol* other_guy) {
    // insert link above the "other_guy"
    TB_LinkerSymbol* old = NULL;
    do {
        old = atomic_load_explicit(&other_guy->parent, memory_order_relaxed);
        atomic_store_explicit(&leader->parent, old, memory_order_release);
    } while (!atomic_compare_exchange_strong(&other_guy->parent, &old, leader));

    // migrate the weak alternative up
    TB_LinkerSymbol* weak_alt = atomic_load_explicit(&other_guy->weak_alt, memory_order_acquire);
    if (weak_alt != NULL) {
        tb_linker_symbol_weak(l, leader, weak_alt);
    }
}

TB_LinkerSymbol* tb_linker_symbol_find(TB_LinkerSymbol* sym) {
    if (sym == NULL) {
        return NULL;
    }

    TB_LinkerSymbol* parent;
    while (parent = atomic_load(&sym->parent), parent != NULL) {
        sym = parent;
    }
    return sym;
}

void tb_linker_lazy_resolve(TB_Linker* l, TB_LinkerSymbol* sym, TB_LinkerObject* obj) {
    bool expected = false;
    if (atomic_compare_exchange_strong(&obj->loaded, &expected, true)) {
        size_t slash = 0;
        FOR_REV_N(i, 0, obj->name.length) {
            if (obj->name.data[i] == '/' || obj->name.data[i] == '\\') {
                slash = i + 1;
                break;
            }
        }

        // printf("tb-link: Loaded %.*s for %.*s\n", (int) (obj->name.length - slash), obj->name.data + slash, (int) sym->name.length, sym->name.data);

        if (l->jobs.pool != NULL) {
            l->jobs.count += 1;
            tpool_add_task(l->jobs.pool, (tpool_task_proc*) l->vtbl.append_object, obj);
        } else {
            l->vtbl.append_object(NULL, obj);
        }
    }
}

TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* sym) {
    // printf("%.*s    %"PRIx32"\n", (int) sym->name.length, sym->name.data, tb__murmur3_32(sym->name.data, sym->name.length) & 65535);

    // insert into global symbol table
    TB_LinkerSymbol* old2 = namehs_intern(&l->symbols, sym);
    TB_LinkerSymbol* old = tb_linker_symbol_find(old2);
    if (sym != old) {
        if (old->tag == TB_LINKER_SYMBOL_LAZY && sym->tag == TB_LINKER_SYMBOL_LAZY) {
            // this doesn't force a resolution
            sym = old;
        } else if (old->tag == TB_LINKER_SYMBOL_LAZY || sym->tag == TB_LINKER_SYMBOL_LAZY) {
            if (old->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                tb_linker_lazy_resolve(l, sym, sym->lazy.obj);
            } else if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                tb_linker_lazy_resolve(l, old, old->lazy.obj);
            }

            // if we're requesting this symbol and it's lazy, load it :p
            if (sym->tag != TB_LINKER_SYMBOL_LAZY) {
                tb_linker_symbol_union(l, sym, old);
            } else {
                sym = old;
            }
        } else if (old->tag == TB_LINKER_SYMBOL_UNKNOWN) {
            if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                // if we're both unresolved then we don't need to do shit yet
                return old;
            }

            tb_linker_symbol_union(l, sym, old);
        } else if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
            sym = old;
        } else if ((old->flags & TB_LINKER_SYMBOL_COMDAT)) {
            // COMDAT, we need to decide which of these lives but for now we don't care.
            sym = old;
        } else {
            // symbol collision if we're overriding something that's
            // not a forward ref.
            // log_debug("Collision at %.*s", (int) sym->name.length, sym->name.data);
            sym = old;
        }
    }

    return sym;
}

void tb_linker_append_module_symbols(TB_Linker* l, TB_Module* m) {
    DynArray(TB_ModuleSection) sections = m->sections;

    CUIK_TIMED_BLOCK("apply symbols") {
        dyn_array_for(i, sections) {
            DynArray(TB_FunctionOutput*) funcs = sections[i].funcs;
            DynArray(TB_Global*) globals = sections[i].globals;
            TB_LinkerSectionPiece* piece = sections[i].piece;

            dyn_array_for(i, funcs) {
                const char* name = funcs[i]->parent->super.name;
                TB_LinkerSymbol* s = tb_platform_heap_alloc(sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = { (const uint8_t*) name, strlen(name) },
                    .tag  = TB_LINKER_SYMBOL_TB,
                    .tb   = { piece, &funcs[i]->parent->super }
                };

                if (funcs[i]->linkage != TB_LINKAGE_PRIVATE) {
                    TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
                    if (new_s != s) {
                        tb_platform_heap_free(s);
                        s = new_s;
                    }
                }

                funcs[i]->parent->super.address = s;
            }

            dyn_array_for(i, globals) {
                const char* name  = globals[i]->super.name;
                TB_LinkerSymbol* s = tb_platform_heap_alloc(sizeof(TB_LinkerSymbol));
                *s = (TB_LinkerSymbol){
                    .name = { (const uint8_t*) name, strlen(name) },
                    .tag  = TB_LINKER_SYMBOL_TB,
                    .tb   = { piece, &globals[i]->super }
                };

                if (globals[i]->linkage != TB_LINKAGE_PRIVATE) {
                    TB_LinkerSymbol* new_s = tb_linker_symbol_insert(l, s);
                    if (new_s != s) {
                        tb_platform_heap_free(s);
                        s = new_s;
                    }
                }
                globals[i]->super.address = s;
            }
        }
    }
}

enum { EXPORT_BUFFER_SIZE = 64*1024 };
void tb_linker_export_piece(TPool* pool, ExportTask* task) {
    TB_Linker* l = task->linker;
    cuikperf_region_start("export", NULL);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_LinkerSectionPiece* end = task->end;
    for (TB_LinkerSectionPiece* p = task->start; p != end; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
        if ((p->flags & TB_LINKER_PIECE_LIVE) == 0 || p->kind == PIECE_BSS) {
            continue;
        }

        cuikperf_region_start("piece", NULL);

        TB_LinkerSection* text  = tb_linker_find_section(l, ".text");
        uint32_t trampoline_rva = text->address + l->trampoline_pos;

        TB_ASSERT(p->kind == PIECE_BUFFER);
        size_t section_file_offset = p->parent->offset;
        size_t section_rva = p->parent->address;

        size_t head = 0, reloc_i = 0;
        while (head < p->size) {
            uint8_t* out = &task->file[section_file_offset + p->offset + head];

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
    }

    if (l->jobs.pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }
    cuikperf_region_end();
}

void tb_linker_export_pieces(TB_Linker* l, DynArray(TB_LinkerSection*) sections, uint8_t* output) {
    if (l->jobs.pool != NULL) {
        l->jobs.done = 0;
        l->jobs.count = 0;

        // each of the pieces can be exported in parallel
        cuikperf_region_start("submitting", NULL);
        dyn_array_for(i, sections) {
            TB_LinkerSectionPiece* p = atomic_load_explicit(&sections[i]->list, memory_order_relaxed);

            // we wanna dispatch tasks as a nice batch so if the accumulated
            // size goes past like 8k we'll dispatch it
            TB_LinkerSectionPiece* accum_start = p;
            size_t accum_size = 0;

            int c = 0;
            while (p != NULL) {
                if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
                    accum_size += p->size;
                }

                if (accum_size >= 8192) {
                    ExportTask* task = tb_arena_alloc(&linker_perm_arena, sizeof(ExportTask));
                    *task = (ExportTask){ l, output, accum_start, p };
                    tpool_add_task(l->jobs.pool, (tpool_task_proc*) tb_linker_export_piece, task);
                    c += 1;

                    accum_size = p->size;
                    accum_start = p;
                }

                p = atomic_load_explicit(&p->next, memory_order_relaxed);
            }

            // push remaining bits
            if (accum_start != NULL) {
                ExportTask* task = tb_arena_alloc(&linker_perm_arena, sizeof(ExportTask));
                *task = (ExportTask){ l, output, accum_start, NULL };
                tpool_add_task(l->jobs.pool, (tpool_task_proc*) tb_linker_export_piece, task);
                c += 1;
            }

            l->jobs.count += c;
        }
        cuikperf_region_end();

        // finish up exporting
        futex_wait_eq(&l->jobs.done, l->jobs.count);
    } else {
        dyn_array_for(i, sections) {
            size_t section_file_offset = sections[i]->offset;
            size_t section_rva = sections[i]->address;

            TB_LinkerSectionPiece* p = atomic_load_explicit(&sections[i]->list, memory_order_relaxed);
            while (p != NULL) {
                TB_LinkerSectionPiece* next = atomic_load_explicit(&p->next, memory_order_relaxed);
                if ((p->flags & TB_LINKER_PIECE_LIVE) && p->kind != PIECE_BSS) {
                    ExportTask task = { l, output, p, next };
                    tb_linker_export_piece(NULL, &task);
                }
                p = next;
            }
        }
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
    const TB_LinkerSectionPiece* sec_a = *(const TB_LinkerSectionPiece**) a;
    const TB_LinkerSectionPiece* sec_b = *(const TB_LinkerSectionPiece**) b;

    size_t len = TB_MIN(sec_a->coff_order.length, sec_b->coff_order.length);
    FOR_N(i, 0, len) {
        int c = sec_a->coff_order.data[i] - sec_b->coff_order.data[i];
        if (c != 0) { return c; }
    }

    if (sec_a->coff_order.length < sec_b->coff_order.length) { return -1; }
    if (sec_a->coff_order.length > sec_b->coff_order.length) { return  1; }

    if (sec_a->order < sec_b->order) return -1;
    if (sec_a->order > sec_b->order) return  1;
    return 0;
}

DynArray(TB_LinkerSection*) tb__finalize_sections(TB_Linker* l) {
    namehs_resize_barrier(&l->unresolved_symbols);
    if (nbhs_count(&l->unresolved_symbols) > 0) {
        nbhs_for(e, &l->unresolved_symbols) {
            TB_Slice* sym_name = *e;
            fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved external: %.*s\n", (int) sym_name->length, sym_name->data);

            #if 0
            size_t i = 0;
            for (; u && i < 5; u = u->next, i++) {
                // walk input stack
                TB_LinkerInputHandle curr = u->reloc;
                fprintf(stderr, "  in ");

                int depth = 0;
                while (curr != 0) {
                    TB_LinkerInput* input = &l->inputs[curr];

                    depth++;
                    if (depth) {
                        fprintf(stderr, "(");
                    }

                    if (input->tag == TB_LINKER_INPUT_MODULE) {
                        fprintf(stderr, "<tb-module %p>\n", input->module);
                    } else {
                        TB_Slice obj_name = as_filename(input->name);
                        fprintf(stderr, "%.*s", (int) obj_name.length, obj_name.data);
                    }

                    curr = input->parent;
                }

                while (depth--) fprintf(stderr, ")");
                fprintf(stderr, "\n");
            }

            if (u) {
                // count the rest
                while (u) u = u->next, i++;

                fprintf(stderr, "  ...and %zu more...\n", i - 5);
            }
            fprintf(stderr, "\n");
            #endif
        }

        return NULL;
    }

    DynArray(TB_LinkerSection*) sections = NULL;
    CUIK_TIMED_BLOCK("sort sections") {
        TB_LinkerSectionPiece** array_form = NULL;
        size_t num = 0;

        nbhs_for(e, &l->sections) {
            TB_LinkerSection* s = *e;
            if (s->generic_flags & TB_LINKER_SECTION_DISCARD) {
                continue;
            }

            size_t piece_count = s->piece_count;

            ////////////////////////////////
            // Sort sections
            ////////////////////////////////
            // convert into array
            size_t j = 0;
            CUIK_TIMED_BLOCK("convert to array") {
                assert(s->piece_count != 0);
                array_form = tb_platform_heap_realloc(array_form, piece_count * sizeof(TB_LinkerSectionPiece*));

                TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
                for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
                    if (p->size != 0 && (p->flags & TB_LINKER_PIECE_LIVE)) {
                        array_form[j++] = p;
                    }
                }

                // fprintf(stderr, "%.*s: %zu -> %zu\n", (int) s->name.length, s->name.data, piece_count, j);
                piece_count = j;
            }

            if (piece_count == 0) {
                s->generic_flags |= TB_LINKER_SECTION_DISCARD;
                continue;
            }

            // sort
            CUIK_TIMED_BLOCK("sort section") {
                qsort(array_form, piece_count, sizeof(TB_LinkerSectionPiece*), compare_linker_sections);
            }

            // convert back into linked list
            CUIK_TIMED_BLOCK("convert into list") {
                // printf("\nSECTION %.*s\n", (int) s->name.length, s->name.data);

                size_t offset = 0;
                TB_LinkerSectionPiece* prev = NULL;
                FOR_N(j, 0, piece_count) {
                    size_t mask = (1u << array_form[j]->align_log2) - 1;
                    size_t next = (offset + mask) & ~mask;
                    if (prev != NULL) {
                        prev->size += next - offset;
                    }
                    offset = next;

                    /* if (array_form[j]->coff_order.length > 0) {
                        TB_Slice n = array_form[j]->coff_order;
                        printf("  PIECE %06zx (align=%06x): %.*s", offset, (1u << array_form[j]->align_log2), (int) n.length, n.data);
                    } else {
                        printf("  PIECE %06zx (align=%06x)", offset, (1u << array_form[j]->align_log2));
                    }
                    printf("\n"); */

                    array_form[j]->offset = offset;
                    offset += array_form[j]->size;

                    if (prev) {
                        atomic_store_explicit(&prev->next, array_form[j], memory_order_relaxed);
                    }
                    prev = array_form[j];
                }
                atomic_store_explicit(&prev->next, NULL, memory_order_relaxed);

                s->size = offset;
                s->list = array_form[0];
                s->piece_count = piece_count;

                log_debug("Section %.*s: %zu pieces with %zu bytes", (int) s->name.length, s->name.data, piece_count, offset);
            }

            dyn_array_put(sections, s);
            s->number = num++;
        }
        tb_platform_heap_free(array_form);
    }

    return sections;
}

// just run whatever reloc function from the spec
static int32_t resolve_reloc(TB_LinkerSymbol* sym, TB_ObjectRelocType type, uint32_t source_pos, uint32_t target_rva, int addend) {
    switch (type) {
        case TB_OBJECT_RELOC_ADDR32NB:
        return target_rva;

        case TB_OBJECT_RELOC_SECTION:
        return sym->normal.piece->parent->number;

        case TB_OBJECT_RELOC_SECREL:
        return sym->normal.piece->offset + sym->normal.secrel;

        case TB_OBJECT_RELOC_REL32:
        return (target_rva + addend) - source_pos;

        default:
        tb_todo();
    }
}

size_t tb_linker_apply_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, uint32_t section_rva, uint32_t trampoline_rva, size_t reloc_i, size_t head, size_t tail) {
    size_t reloc_len = p->reloc_count;
    while (reloc_i < reloc_len) {
        TB_LinkerReloc rel;
        l->vtbl.parse_reloc(l, p, reloc_i, &rel);

        int rel_size = rel.type == TB_OBJECT_RELOC_ADDR64 ? 8 : 4;

        // we only apply if it's not hanging off the right edge, if that's
        // the case we've fully loaded the memory we're overlaying.
        int dst_pos = rel.src_offset - head;
        if (dst_pos + rel_size > tail) { break; }

        // by this point, we've fully resolved the relocation
        TB_LinkerSymbol* sym = tb_linker_symbol_find(rel.target);
        if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
            TB_LinkerSymbol* alt = tb_linker_symbol_find(atomic_load_explicit(&sym->weak_alt, memory_order_relaxed));
            if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN && alt->tag != TB_LINKER_SYMBOL_LAZY) {
                sym = alt;
            }
        }
        TB_ASSERT(sym && sym->tag != TB_LINKER_SYMBOL_UNKNOWN && sym->tag != TB_LINKER_SYMBOL_LAZY);

        // resolve source location
        uint32_t target_rva = 0;
        if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
            target_rva = l->iat_pos + (sym->import.thunk_id * 8);
        } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
            TB_LinkerSymbol* import_sym = sym->thunk;
            target_rva = trampoline_rva + (import_sym->import.thunk_id * 6);
        } else {
            target_rva = tb__get_symbol_rva(l, sym);
        }

        // skip because it's irrelevant
        if (rel.type == TB_OBJECT_RELOC_ADDR64) {
            // we write out the fake VA and the base relocs will fix it up
            int64_t* dst = (int64_t*) &out[dst_pos];
            *dst += 0x140000000 + target_rva;
        } else {
            uint32_t src_rva = section_rva + p->offset + rel.src_offset;
            int32_t* dst = (int32_t*) &out[dst_pos];
            *dst += resolve_reloc(sym, rel.type, src_rva, target_rva, rel.addend);
        }
        reloc_i += 1;
    }
    return reloc_i;
}

void tb_linker_push_named(TB_Linker* l, const char* name) {
    TB_LinkerSymbol* sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, name));
    if (sym->tag != TB_LINKER_SYMBOL_UNKNOWN && sym->tag != TB_LINKER_SYMBOL_LAZY) {
        sym->flags |= TB_LINKER_SYMBOL_USED;
        tb_linker_push_piece(l, tb_linker_get_piece(l, sym));
    }
}

bool tb_linker_push_piece(TB_Linker* l, TB_LinkerSectionPiece* p) {
    if (p->size == 0 || (p->flags & TB_LINKER_PIECE_LIVE) || (p->parent->generic_flags & TB_LINKER_SECTION_DISCARD)) {
        return false;
    }

    p->flags |= TB_LINKER_PIECE_LIVE;
    dyn_array_put(l->worklist, p);
    return true;
}

static TB_LinkerSymbol* resolve_external(TB_Linker* l, TB_External* ext) {
    TB_LinkerSymbol* sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, ext->super.name));
    if (sym == NULL || sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
        namehs_intern(&l->unresolved_symbols, &sym->name);
    } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
        sym->thunk->flags |= TB_LINKER_SYMBOL_USED;
    }

    ext->super.address = sym;
    sym->flags |= TB_LINKER_SYMBOL_USED;
    return sym;
}

void tb_linker_mark_live(TB_Linker* l) {
    tb_linker_push_named(l, l->entrypoint);

    // mark all non-COMDAT pieces as live
    cuikperf_region_start("root scan", NULL);
    nbhs_for(e, &l->sections) {
        TB_LinkerSection* s = *e;
        if (s->generic_flags & TB_LINKER_SECTION_DISCARD) { continue; }
        // we don't consider .debug as roots because codeview is compiled into the PDB
        if (s->name.length == sizeof(".debug")-1 && memcmp(s->name.data, ".debug", s->name.length) == 0) { continue; }

        TB_LinkerSectionPiece* p = atomic_load_explicit(&s->list, memory_order_relaxed);
        for (; p != NULL; p = atomic_load_explicit(&p->next, memory_order_relaxed)) {
            if ((p->flags & TB_LINKER_PIECE_COMDAT) == 0) {
                tb_linker_push_piece(l, p);
            }
        }
    }
    cuikperf_region_end();

    cuikperf_region_start("mark", NULL);
    while (dyn_array_length(l->worklist)) {
        TB_LinkerSectionPiece* p = dyn_array_pop(l->worklist);
        // printf("Walk: %p\n", p);

        // associated section
        if (p->assoc) {
            tb_linker_push_piece(l, p->assoc);
        }

        // mark module content
        if (p->obj->module && !p->obj->module->visited) {
            p->obj->module->visited = true;

            TB_Module* m = p->obj->module;
            dyn_array_for(i, m->sections) {
                if (m->sections[i].piece) {
                    tb_linker_push_piece(l, m->sections[i].piece);
                }
            }

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

        // mark any relocations:
        //   by this point, the symbols aren't being fought for so we really should
        // use relaxed loads when possible (might matter for ARM but not x86)
        FOR_N(i, 0, p->reloc_count) {
            TB_LinkerReloc rel;
            l->vtbl.parse_reloc(l, p, i, &rel);

            TB_LinkerSymbol* sym = tb_linker_symbol_find(rel.target);
            if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN || sym->tag == TB_LINKER_SYMBOL_LAZY) {
                TB_LinkerSymbol* alt = tb_linker_symbol_find(atomic_load_explicit(&sym->weak_alt, memory_order_relaxed));
                if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN && alt->tag != TB_LINKER_SYMBOL_LAZY) {
                    // we could make this the leader to path compress
                    sym = alt;
                } else {
                    namehs_intern(&l->unresolved_symbols, &sym->name);
                }
            }

            sym->flags |= TB_LINKER_SYMBOL_USED;
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB) {
                tb_linker_push_piece(l, sym->normal.piece);
                // printf("  Mark: %.*s (%p)\n", (int) sym->name.length, sym->name.data, sym->normal.piece);
            }
        }
    }
    cuikperf_region_end();
}

