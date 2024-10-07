#include "linker.h"
#include <file_map.h>

#if __STDC_VERSION__ < 201112L || defined(__STDC_NO_ATOMICS__)
#error "Missing C11 support for stdatomic.h"
#endif

#include <stdatomic.h>

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
static uint32_t name_hash(const void* a) {
    const TB_Slice* sym = a;
    uint32_t h = 0x811C9DC5;
    FOR_N(i, 0, sym->length) {
        h = ((uint8_t) sym->data[i] ^ h) * 0x01000193;
    }
    return h;
}

static bool name_cmp(const void* a, const void* b) {
    const TB_Slice* aa = a;
    const TB_Slice* bb = b;
    return aa->length == bb->length && memcmp(aa->data, bb->data, aa->length) == 0;
}

static void* muh_hs_alloc(size_t size) {
    void* ptr = tb_platform_heap_alloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void muh_hs_free(void* ptr, size_t size) {
    tb_platform_heap_free(ptr);
}

TB_Linker* tb_linker_create(TB_ExecutableType exe, TB_Arch arch, Cuik_IThreadpool* tp) {
    TB_Linker* l = tb_platform_heap_alloc(sizeof(TB_Linker));
    memset(l, 0, sizeof(TB_Linker));
    l->target_arch = arch;
    l->thread_pool = tp;

    l->symbols  = nbhs_alloc(256, muh_hs_alloc, muh_hs_free, name_cmp, name_hash);
    l->sections = nbhs_alloc(16,  muh_hs_alloc, muh_hs_free, name_cmp, name_hash);
    l->imports  = nbhs_alloc(256, muh_hs_alloc, muh_hs_free, name_cmp, name_hash);
    l->unresolved_symbols = nbhs_alloc(16,  muh_hs_alloc, muh_hs_free, name_cmp, name_hash);

    switch (exe) {
        case TB_EXECUTABLE_PE:  l->vtbl = tb__linker_pe; break;
        // case TB_EXECUTABLE_ELF: l->vtbl = tb__linker_elf; break;
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

void tb_linker_append_object(TB_Linker* l, const char* file_name) {
    FileMap fm = open_file_map(file_name);
    ImportObjTask task = {
        l,
        { (const uint8_t*) file_name, strlen(file_name) },
        { fm.data, fm.size }
    };

    if (l->thread_pool != NULL) {
        CUIK_CALL(l->thread_pool, submit, (Cuik_TaskFn) l->vtbl.append_object, sizeof(task), &task);
        l->jobs.count += 1;
    } else {
        l->vtbl.append_object(&task);
    }

    // we close the file map once we're done exporting
    // close_file_map(&fm);
}

void tb_linker_append_module(TB_Linker* l, TB_Module* m) {
    CUIK_TIMED_BLOCK("append_module") {
        __debugbreak();
    }
}

void tb_linker_append_library(TB_Linker* l, const char* file_name) {
    FileMap fm = open_file_map(file_name);
    ImportObjTask task = {
        l,
        { (const uint8_t*) file_name, strlen(file_name) },
        { fm.data, fm.size }
    };

    if (l->thread_pool != NULL) {
        CUIK_CALL(l->thread_pool, submit, (Cuik_TaskFn) l->vtbl.append_library, sizeof(task), &task);
        l->jobs.count += 1;
    } else {
        l->vtbl.append_library(&task);
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

uint64_t tb__compute_rva(TB_Linker* l, TB_Module* m, const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_FUNCTION) {
        TB_Function* f = (TB_Function*) s;
        assert(f->output != NULL);

        TB_LinkerSectionPiece* piece = m->sections[f->section].piece;
        return piece->parent->address + piece->offset + f->output->code_pos;
    } else if (s->tag == TB_SYMBOL_GLOBAL) {
        TB_Global* g = (TB_Global*) s;
        TB_LinkerSectionPiece* piece = m->sections[g->parent].piece;
        return piece->parent->address + piece->offset + g->pos;
    } else {
        tb_todo();
        return 0;
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
    return nbhs_get(&l->sections, &str);
}

TB_LinkerSection* tb_linker_find_or_create_section(TB_Linker* l, size_t name_len, const char* name, uint32_t flags) {
    // use an arena
    TB_LinkerSection* s = tb_platform_heap_alloc(sizeof(TB_LinkerSection));
    *s = (TB_LinkerSection){ .name = { (const uint8_t*) name, name_len }, .flags = flags };

    TB_LinkerSection* k = nbhs_intern(&l->sections, s);
    if (s != k) {
        tb_arena_free(&linker_perm_arena, s, sizeof(TB_LinkerSymbol));
        return k;
    }
    return s;
}

TB_LinkerSectionPiece* tb_linker_append_piece(TB_LinkerSection* section, int kind, size_t size, TB_LinkerObject* obj) {
    // use an arena
    TB_LinkerSectionPiece* piece = tb_platform_heap_alloc(sizeof(TB_LinkerSectionPiece));
    *piece = (TB_LinkerSectionPiece){
        .kind   = kind,
        .parent = section,
        .obj    = obj,
        .size   = size,
    };
    atomic_fetch_add(&section->piece_count, 1);

    TB_LinkerSectionPiece* old_top;
    do {
        old_top = atomic_load(&section->list);
        piece->next = old_top;
    } while (!atomic_compare_exchange_strong(&section->list, &old_top, piece));

    return piece;
}

TB_LinkerSymbol* tb_linker_find_symbol(TB_Linker* l, TB_Slice name) {
    return nbhs_get(&l->symbols, &name);
}

TB_LinkerSymbol* tb_linker_find_symbol2(TB_Linker* l, const char* name) {
    TB_Slice str = { (const uint8_t*) name, strlen(name) };
    return nbhs_get(&l->symbols, &str);
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

void tb_linker_symbol_union(TB_Linker* l, TB_LinkerSymbol* leader, TB_LinkerSymbol* other_guy) {
    TB_LinkerSymbol* old = NULL;
    while (!atomic_compare_exchange_strong(&other_guy->parent, &old, leader)) {
        // we failed to make leader the real leader... ok let's walk up the chain and try again
        other_guy = old, old = NULL;
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

TB_LinkerSymbol* tb_linker_symbol_insert(TB_Linker* l, TB_LinkerSymbol* sym) {
    // insert into global symbol table
    TB_LinkerSymbol* old2 = nbhs_intern(&l->symbols, sym);
    TB_LinkerSymbol* old = tb_linker_symbol_find(old2);
    if (sym != old) {
        if (old->tag == TB_LINKER_SYMBOL_UNKNOWN) {
            if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                // if we're both unresolved then we don't need to do shit yet
                return old;
            }

            tb_linker_symbol_union(l, sym, old);
        } else if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
            tb_linker_symbol_union(l, old, sym);
        } else if ((old->flags & TB_LINKER_SYMBOL_COMDAT)) {
            // COMDAT, we need to decide which of these lives but for now we don't care.
            tb_linker_symbol_union(l, old, sym);
        } else {
            // symbol collision if we're overriding something that's
            // not a forward ref.
            log_debug("Collision at %.*s", (int) sym->name.length, sym->name.data);
            old = sym;
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

void tb_linker_apply_module_relocs(TB_Linker* l, TB_Module* m, TB_LinkerSection* text, uint8_t* output) {
    uint64_t trampoline_rva = text->address + l->trampoline_pos;

    dyn_array_for(i, m->sections) {
        DynArray(TB_FunctionOutput*) funcs = m->sections[i].funcs;
        TB_LinkerSectionPiece* piece = m->sections[i].piece;
        if (piece == NULL) {
            continue;
        }

        uint64_t text_piece_rva = piece->parent->address + piece->offset;
        uint64_t text_piece_file = piece->parent->offset + piece->offset;

        dyn_array_for(j, funcs) {
            TB_FunctionOutput* out_f = funcs[j];
            for (TB_SymbolPatch* patch = out_f->first_patch; patch; patch = patch->next) {
                int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + patch->pos];
                size_t actual_pos = text_piece_rva + out_f->code_pos + patch->pos + 4;

                int32_t p = 0;
                if (patch->target->tag == TB_SYMBOL_EXTERNAL) {
                    TB_LinkerSymbol* sym = patch->target->address;
                    if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                        // TODO(NeGate): error for unresolved symbol
                        tb_todo();
                    } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
                        p = trampoline_rva + (sym->thunk->import.thunk_id * 6);
                    } else if (sym->tag == TB_LINKER_SYMBOL_IMPORT) {
                        p = trampoline_rva + (sym->import.thunk_id * 6);
                    } else {
                        p = tb__get_symbol_rva(l, sym);
                    }

                    p -= actual_pos;
                } else if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                    // internal patching has already handled this
                } else if (patch->target->tag == TB_SYMBOL_GLOBAL) {
                    TB_Global* global = (TB_Global*) patch->target;
                    assert(global->super.tag == TB_SYMBOL_GLOBAL);

                    uint32_t flags = m->sections[global->parent].flags;
                    TB_LinkerSectionPiece* piece = m->sections[global->parent].piece;
                    uint32_t piece_rva = piece->parent->address + piece->offset;

                    int32_t* dst = (int32_t*) &output[text_piece_file + out_f->code_pos + patch->pos];
                    if (flags & TB_MODULE_SECTION_TLS) {
                        // section relative for TLS
                        p = piece_rva + global->pos;
                    } else {
                        p = (piece_rva + global->pos) - actual_pos;
                    }
                } else {
                    tb_todo();
                }

                *dst += p;
            }
        }
    }
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
        return target_rva - (source_pos + addend);

        default:
        tb_todo();
    }
}

size_t tb_linker_apply_reloc(TB_Linker* l, TB_LinkerSectionPiece* p, uint8_t* out, uint32_t section_rva, uint32_t trampoline_rva, size_t reloc_i, size_t head, size_t tail) {
    size_t reloc_len = dyn_array_length(p->relocs);
    while (reloc_i < reloc_len) {
        TB_LinkerReloc* restrict rel = &p->relocs[reloc_i];
        int rel_size = rel->type == TB_OBJECT_RELOC_ADDR64 ? 8 : 4;

        // we only apply if it's not hanging off the right edge, if that's
        // the case we've fully loaded the memory we're overlaying.
        int dst_pos = rel->src_offset - head;
        if (dst_pos + rel_size > tail) { break; }

        // by this point, we've fully resolved the relocation
        TB_LinkerSymbol* sym = rel->target;
        TB_ASSERT(sym && sym->tag != TB_LINKER_SYMBOL_UNKNOWN);

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
        if (rel->type == TB_OBJECT_RELOC_ADDR64) {
            // we write out the fake VA and the base relocs will fix it up
            int64_t* dst = (int64_t*) &out[dst_pos];
            *dst += 0x140000000 + target_rva;
        } else {
            uint32_t src_rva = section_rva + p->offset + rel->src_offset;
            int32_t* dst = (int32_t*) &out[dst_pos];
            *dst += resolve_reloc(sym, rel->type, src_rva, target_rva, rel->addend);
        }
        reloc_i += 1;
    }
    return reloc_i;
}

enum { EXPORT_BUFFER_SIZE = 64*1024 };
void tb_linker_export_piece(ExportTask* task) {
    TB_Linker* l = task->linker;
    TB_LinkerSectionPiece* p = task->piece;

    cuikperf_region_start("export", NULL);

    if (!linker_thread_init) {
        linker_thread_init = true;
        tb_arena_create(&linker_perm_arena, "LinkerPerm");
        tb_arena_create(&linker_tmp_arena, "LinkerTmp");
    }

    TB_LinkerSection* text  = tb_linker_find_section(l, ".text");
    uint32_t trampoline_rva = text->address + l->trampoline_pos;

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

            assert(p->kind == PIECE_BUFFER);
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

    if (l->thread_pool != NULL) {
        l->jobs.done += 1;
        futex_signal(&l->jobs.done);
    }
    cuikperf_region_end();
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

    if (sec_a->order < sec_b->order) return -1;
    if (sec_a->order > sec_b->order) return 1;
    return 0;
}

DynArray(TB_LinkerSection*) tb__finalize_sections(TB_Linker* l) {
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

        __debugbreak();
        return NULL;
    }

    DynArray(TB_LinkerSection*) sections = NULL;
    CUIK_TIMED_BLOCK("sort sections") {
        TB_LinkerSectionPiece** array_form = NULL;
        size_t num = 0;

        nbhs_for(e, &l->sections) {
            TB_LinkerSection* s = *e;
        }

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
                int piece_alignment = 16;
                if (s->name.length == 6 && memcmp(s->name.data, ".pdata", 6) == 0) {
                    piece_alignment = 1;
                }

                size_t offset = 0;
                TB_LinkerSectionPiece* prev = NULL;
                FOR_N(j, 0, piece_count) {
                    array_form[j]->offset = offset;
                    array_form[j]->size   = align_up(array_form[j]->size, piece_alignment);
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

void tb_linker_push_named(TB_Linker* l, const char* name) {
    TB_LinkerSymbol* sym = tb_linker_symbol_find(tb_linker_find_symbol2(l, name));
    if (sym != NULL) {
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
        nbhs_intern(&l->unresolved_symbols, &sym->name);
    } else if (sym->tag == TB_LINKER_SYMBOL_THUNK) {
        sym->thunk->flags |= TB_LINKER_SYMBOL_USED;
    }

    ext->super.address = sym;
    sym->flags |= TB_LINKER_SYMBOL_USED;
    return sym;
}

void tb_linker_mark_live(TB_Linker* l) {
    tb_linker_push_named(l, l->entrypoint);

    // mark all non-COMDAT symbols as live
    nbhs_for(e, &l->symbols) {
        TB_LinkerSymbol* sym = *e;
        if (sym->tag != TB_LINKER_SYMBOL_NORMAL && sym->tag != TB_LINKER_SYMBOL_TB) {
            continue;
        }

        TB_LinkerSectionPiece* p = sym->normal.piece;
        TB_LinkerSection* s = p->parent;

        // we don't consider .debug as roots because codeview is compiled into the PDB
        if (s->name.length == sizeof(".debug")-1 && memcmp(s->name.data, ".debug", s->name.length) == 0) {
            continue;
        }

        if ((p->flags & TB_LINKER_PIECE_COMDAT) == 0) {
            tb_linker_push_piece(l, p);
        }
    }

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

        // mark any relocations
        dyn_array_for(i, p->relocs) {
            TB_LinkerSymbol* sym = tb_linker_symbol_find(p->relocs[i].target);
            if (sym->tag == TB_LINKER_SYMBOL_UNKNOWN) {
                TB_LinkerSymbol* alt = tb_linker_symbol_find(p->relocs[i].alt);
                if (alt && alt->tag != TB_LINKER_SYMBOL_UNKNOWN) {
                    sym = alt;
                } else {
                    nbhs_intern(&l->unresolved_symbols, &sym->name);
                }
            }

            if (sym->name.length == sizeof("__scrt_exe_initialize_mta") - 1 && memcmp(sym->name.data, "__scrt_exe_initialize_mta", sym->name.length) == 0) {
                __debugbreak();
            }

            sym->flags |= TB_LINKER_SYMBOL_USED;
            if (sym->tag == TB_LINKER_SYMBOL_NORMAL || sym->tag == TB_LINKER_SYMBOL_TB) {
                tb_linker_push_piece(l, sym->normal.piece);
                // printf("  Mark: %.*s (%p)\n", (int) sym->name.length, sym->name.data, sym->normal.piece);
            }
            p->relocs[i].target = sym;
        }
    }
}

