#include "tb_internal.h"
#include "host.h"
#include <setjmp.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

enum {
    ALLOC_COOKIE = 0xBAADF00D,
    ALLOC_GRANULARITY = 16,
    STACK_SIZE = 2*1024*1024
};

typedef struct {
    uint8_t* pos;

    // when we insert a breakpoint, we're
    // replacing some byte with an INT3 (at
    // least on x86), we need to restore it
    // before continuing execution.
    uint8_t prev_byte;
} TB_Breakpoint;

typedef struct {
    uint32_t cookie;
    uint32_t size; // if low bit is set, we're in use.
    char data[];
} FreeList;

// addr -> symbol
typedef struct {
    uint32_t k;
    void* v;
} Tag;

struct TB_JIT {
    size_t capacity;
    mtx_t lock;
    NL_Strmap(void*) loaded_funcs;
    DynArray(Tag) tags;

    FreeList heap;
};

static const char* prot_names[] = {
    "RO", "RW", "RX", "RXW",
};

static void tb_jit_insert_sym(TB_JIT* jit, void* ptr, void* tag) {
    assert(tag);
    uint32_t offset = (char*) ptr - (char*) jit;

    size_t i = 0, count = dyn_array_length(jit->tags);
    for (; i < count; i++) {
        if (offset < jit->tags[i].k) break;
    }

    // we know where to insert
    dyn_array_put_uninit(jit->tags, 1);
    memmove(&jit->tags[i + 1], &jit->tags[i], (count - i) * sizeof(Tag));
    jit->tags[i] = (Tag){ offset, tag };
}

TB_ResolvedAddr tb_jit_addr2sym(TB_JIT* jit, void* ptr) {
    mtx_lock(&jit->lock);
    uint32_t offset = (char*) ptr - (char*) jit;

    size_t left = 0;
    size_t right = dyn_array_length(jit->tags);
    if (right == 0) goto bad;

    Tag* tags = jit->tags;
    while (left < right) {
        size_t middle = (left + right) / 2;
        if (tags[middle].k > offset) {
            right = middle;
        } else {
            left = middle + 1;
        }
    }

    size_t i = right - 1;
    TB_Symbol* s = tags[i].v;
    if (s->tag == TB_SYMBOL_FUNCTION) {
        // check if we're in bounds for the leftmost option
        TB_Function* f = (TB_Function*) s;
        uint32_t end = tags[i].k + f->output->code_size;
        if (offset >= end) goto bad;

        mtx_unlock(&jit->lock);
        return (TB_ResolvedAddr){ s, offset - tags[i].k };
    }

    bad:
    mtx_unlock(&jit->lock);
    return (TB_ResolvedAddr){ 0 };
}

static TB_ResolvedLine jit__addr2line(TB_JIT* jit, TB_ResolvedAddr addr) {
    TB_Function* f = (TB_Function*) addr.base;
    DynArray(TB_Location) locs = f->output->locations;
    if (dyn_array_length(locs) == 0) {
        return (TB_ResolvedLine){ 0 };
    }

    // find cool line
    size_t left = 0;
    size_t right = dyn_array_length(locs);
    while (left < right) {
        size_t middle = (left + right) / 2;
        if (locs[middle].pos > addr.offset) {
            right = middle;
        } else {
            left = middle + 1;
        }
    }

    uint32_t start = locs[right - 1].pos;
    uint32_t end = f->output->code_size;
    if (right < dyn_array_length(locs)) {
        end = locs[right].pos;
    }

    return (TB_ResolvedLine){ f, &locs[right - 1], start, end };
}

TB_ResolvedLine tb_jit_addr2line(TB_JIT* jit, void* ptr) {
    TB_ResolvedAddr addr = tb_jit_addr2sym(jit, ptr);
    if (addr.base == NULL || addr.base->tag != TB_SYMBOL_FUNCTION) {
        return (TB_ResolvedLine){ 0 };
    }

    return jit__addr2line(jit, addr);
}

static void* tb_jit_alloc_obj(TB_JIT* jit, void* tag, size_t size, size_t align) {
    mtx_lock(&jit->lock);
    size = (size + ALLOC_GRANULARITY - 1) & ~(ALLOC_GRANULARITY - 1);

    FreeList* l = &jit->heap;
    FreeList* end = (FreeList*) ((char*) jit + jit->capacity);
    for (;;) {
        assert(l->cookie == ALLOC_COOKIE);

        // free slot, let's see if it's big enough
        if ((l->size & 1) == 0 && (l->size >> 1) >= size) {
            // if the node is bigger than one alloc, we need to split it
            size_t whole_size = l->size >> 1;
            size_t split_size = sizeof(FreeList) + size;
            if (whole_size > split_size) {
                // split
                l->size = (size << 1) | 1;

                // free leftovers
                FreeList* rest = (FreeList*) &l->data[l->size >> 1];
                rest->cookie = ALLOC_COOKIE;
                rest->size = (whole_size - split_size) << 1;
            } else {
                // take entire piece
                l->size |= 1;
            }

            if (tag) {
                tb_jit_insert_sym(jit, l->data, tag);
            }

            mtx_unlock(&jit->lock);
            return l->data;
        }

        FreeList* next = (FreeList*) &l->data[l->size >> 1];
        if (next == end) {
            break;
        }

        // coalesce free regions when we see them here, it'll
        // make later walks faster
        while ((l->size & 1) == 0 && (next->size & 1) == 0) {
            size_t new_size = (l->size >> 1) + sizeof(FreeList) + (next->size >> 1);
            l->size = (new_size << 1);

            next = (FreeList*) &l->data[new_size];
        }

        l = next;
    }

    mtx_unlock(&jit->lock);
    return NULL;
}

void tb_jit_free_obj(TB_JIT* jit, void* ptr) {
    FreeList* obj = ((FreeList*) ptr) - 1;
    assert(obj->cookie == ALLOC_COOKIE);
    obj->size &= ~1;
}

void tb_jit_dump_heap(TB_JIT* jit) {
    mtx_lock(&jit->lock);

    FreeList* l = &jit->heap;
    FreeList* next;
    FreeList* end = (FreeList*) ((char*) jit + jit->capacity);

    printf("HEAP:\n");
    size_t tag = 0, tag_count = dyn_array_length(jit->tags);
    char* base = (char*) jit;
    for (;;) {
        if (l->size & 1) {
            printf("* ALLOC [%p %u", l->data, l->size >> 1);

            // found the next tag here
            TB_Symbol* s = NULL;
            if (tag < tag_count && l->data == &base[jit->tags[tag].k]) {
                s = jit->tags[tag].v;
                printf(" TAG=%s", s->name);
                tag += 1;
            }

            printf("]\n");
        }

        next = (FreeList*) &l->data[l->size >> 1];
        if (next == end) {
            break;
        }
        l = next;
    }
    mtx_unlock(&jit->lock);
}

static void* get_proc(TB_JIT* jit, const char* name) {
    #ifdef _WIN32
    static HMODULE kernel32, user32, gdi32, opengl32, msvcrt;
    if (user32 == NULL) {
        kernel32 = LoadLibrary("kernel32.dll");
        user32   = LoadLibrary("user32.dll");
        gdi32    = LoadLibrary("gdi32.dll");
        opengl32 = LoadLibrary("opengl32.dll");
        msvcrt   = LoadLibrary("msvcrt.dll");
    }

    // check cache first
    ptrdiff_t search = nl_map_get_cstr(jit->loaded_funcs, name);
    if (search >= 0) return jit->loaded_funcs[search].v;

    void* addr = GetProcAddress(NULL, name);
    if (addr == NULL) addr = GetProcAddress(kernel32, name);
    if (addr == NULL) addr = GetProcAddress(user32, name);
    if (addr == NULL) addr = GetProcAddress(gdi32, name);
    if (addr == NULL) addr = GetProcAddress(opengl32, name);
    if (addr == NULL) addr = GetProcAddress(msvcrt, name);

    // printf("JIT: loaded %s (%p)\n", name, addr);
    nl_map_put_cstr(jit->loaded_funcs, name, addr);
    return addr;
    #else
    return NULL;
    #endif
}

static void* get_symbol_address(const TB_Symbol* s) {
    if (s->tag == TB_SYMBOL_GLOBAL) {
        return ((TB_Global*) s)->address;
    } else if (s->tag == TB_SYMBOL_FUNCTION) {
        return ((TB_Function*) s)->compiled_pos;
    } else {
        tb_todo();
    }
}

void* tb_jit_place_function(TB_JIT* jit, TB_Function* f) {
    TB_FunctionOutput* func_out = f->output;
    if (f->compiled_pos != NULL) {
        return f->compiled_pos;
    }

    // copy machine code
    char* dst = tb_jit_alloc_obj(jit, f, func_out->code_size, 16);
    memcpy(dst, func_out->code, func_out->code_size);
    f->compiled_pos = dst;

    log_debug("jit: apply function %s (%p)", f->super.name, dst);

    // apply relocations, any leftovers are mapped to thunks
    for (TB_SymbolPatch* p = func_out->first_patch; p; p = p->next) {
        size_t actual_pos = p->pos;
        TB_SymbolTag tag = p->target->tag;

        int32_t* patch = (int32_t*) &dst[actual_pos];
        if (tag == TB_SYMBOL_FUNCTION) {
            TB_Function* f = (TB_Function*) p->target;
            void* addr = tb_jit_place_function(jit, f);

            int32_t rel32 = (intptr_t)addr - ((intptr_t)patch + 4);
            *patch += rel32;
        } else if (tag == TB_SYMBOL_EXTERNAL) {
            TB_External* e = (TB_External*) p->target;

            void* addr = e->thunk ? e->thunk : p->target->address;
            if (addr == NULL) {
                addr = get_proc(jit, p->target->name);
                if (addr == NULL) {
                    tb_panic("Could not find procedure: %s", p->target->name);
                }
            }

            ptrdiff_t rel = (intptr_t)addr - ((intptr_t)patch + 4);
            int32_t rel32 = rel;
            if (rel == rel32) {
                memcpy(dst + actual_pos, &rel32, sizeof(int32_t));
            } else {
                // generate thunk to make far call
                char* thunk = tb_jit_alloc_obj(jit, NULL, 6 + sizeof(void*), 1);
                thunk[0] = 0xFF; // jmp qword [rip]
                thunk[1] = 0x25;
                thunk[2] = 0x00;
                thunk[3] = 0x00;
                thunk[4] = 0x00;
                thunk[5] = 0x00;

                // write final address into the thunk
                memcpy(thunk + 6, &addr, sizeof(void*));
                e->thunk = thunk;

                int32_t rel32 = (intptr_t)thunk - ((intptr_t)patch + 4);
                *patch += rel32;
            }
        } else if (tag == TB_SYMBOL_GLOBAL) {
            TB_Global* g = (TB_Global*) p->target;
            void* addr = tb_jit_place_global(jit, g);

            int32_t* patch = (int32_t*) &dst[actual_pos];
            int32_t rel32 = (intptr_t)addr - ((intptr_t)patch + 4);
            *patch += rel32;
        } else {
            tb_todo();
        }
    }

    return dst;
}

void* tb_jit_place_global(TB_JIT* jit, TB_Global* g) {
    if (g->address != NULL) {
        return g->address;
    }

    char* data = tb_jit_alloc_obj(jit, g, g->size, g->align);
    g->address = data;

    log_debug("jit: apply global %s (%p)", g->super.name ? g->super.name : "<unnamed>", data);

    memset(data, 0, g->size);
    FOREACH_N(k, 0, g->obj_count) {
        if (g->objects[k].type == TB_INIT_OBJ_REGION) {
            memcpy(&data[g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
        }
    }

    FOREACH_N(k, 0, g->obj_count) {
        if (g->objects[k].type == TB_INIT_OBJ_RELOC) {
            uintptr_t addr = (uintptr_t) get_symbol_address(g->objects[k].reloc);

            uintptr_t* dst = (uintptr_t*) &data[g->objects[k].offset];
            *dst += addr;
        }
    }

    return data;
}

TB_JIT* tb_jit_begin(TB_Module* m, size_t jit_heap_capacity) {
    if (jit_heap_capacity == 0) {
        jit_heap_capacity = 2*1024*1024;
    }

    TB_JIT* jit = tb_platform_valloc(jit_heap_capacity);
    mtx_init(&jit->lock, mtx_plain);
    jit->capacity = jit_heap_capacity;
    jit->heap.cookie = ALLOC_COOKIE;
    jit->heap.size = (jit_heap_capacity - sizeof(TB_JIT)) << 1;

    // a lil unsafe... im sorry momma
    tb_platform_vprotect(jit, jit_heap_capacity, TB_PAGE_RXW);
    return jit;
}

void tb_jit_end(TB_JIT* jit) {
    mtx_destroy(&jit->lock);
    tb_platform_vfree(jit, jit->capacity);
}

void* tb_jit_get_code_ptr(TB_Function* f) {
    return f->compiled_pos;
}

////////////////////////////////
// Debugger
////////////////////////////////
#if defined(CUIK__IS_X64) && defined(_WIN32)
struct TB_CPUContext {
    // used for stack crawling
    void* pc;
    void* sp;

    void* old_sp;

    TB_JIT* jit;
    size_t ud_size;

    volatile bool done;
    volatile bool running;

    CONTEXT cont;

    // safepoint page
    _Alignas(4096) char poll_site[4096];

    char user_data[];
};

typedef struct {
    // rcx, rdx, r8, r9
    void* gprs[4];
} X64Params;

// jump_in(Ctx, PC, X64Params)
__declspec(allocate(".text")) __declspec(align(16))
static const uint8_t tb_jit__trampoline[] = {
    // # save old SP, we'll come back for it later
    0x48, 0x89, 0x61, 0x10,                   // mov [rcx + 0x10], rsp
    // # place SP at the top of the region (minus 40 for shadow stack + RPC)
    0x48, 0x89, 0xCC,                         // mov rsp, rcx
    0x48, 0x81, 0xC4, 0xD8, 0xFF, 0x1F, 0x00, // add rsp, (2*1024*1024) - 40
    // # shuffle some params into win64 volatile regs
    0x48, 0x89, 0xD0,                         // mov rax, rdx
    0x4D, 0x89, 0xC2,                         // mov r10, r8
    // # fill GPR params
    0x49, 0x8B, 0x0A,                         // mov rcx, [r10 + 0x00]
    0x49, 0x8B, 0x52, 0x08,                   // mov rdx, [r10 + 0x08]
    0x4D, 0x8B, 0x42, 0x10,                   // mov r8,  [r10 + 0x10]
    0x4D, 0x8B, 0x4A, 0x18,                   // mov r9,  [r10 + 0x18]
    0xFF, 0xD0,                               // call rax
    // # restore stack & return normally
    0x48, 0x81, 0xE4, 0x00, 0x00, 0xE0, 0xFF, // and rsp, -0x200000
    0x48, 0x8B, 0x64, 0x24, 0x10,             // mov rsp, [rsp + 0x10]
    0xC3,                                     // ret
};

TB_CPUContext* tb_jit_thread_create(TB_JIT* jit, size_t ud_size) {
    TB_CPUContext* cpu = tb_jit_stack_create();
    cpu->ud_size = ud_size;
    cpu->jit = jit;
    memset(cpu->user_data, 0, ud_size);
    return cpu;
}

void* tb_jit_thread_get_userdata(TB_CPUContext* cpu) {
    return cpu->user_data;
}

static LONG except_handler(EXCEPTION_POINTERS* e) {
    TB_CPUContext* cpu = (TB_CPUContext*) (e->ContextRecord->Rsp & -STACK_SIZE);
    if (e->ExceptionRecord->ExceptionCode == EXCEPTION_GUARD_PAGE &&
        e->ExceptionRecord->ExceptionInformation[1] == (uintptr_t) &cpu->poll_site[0]) {
        // we hit a safepoint poll
        void* addr = 0;

        // TODO(NeGate): copy values out of the regs/stk
        printf("Yikes!\n");

        // TB_* sp = nl_table_get(cpu->safepoints, (void*) e->ContextRecord->Rip);

        // necessary for stack crawling later
        cpu->pc = (void*) e->ContextRecord->Rip;
        cpu->sp = (void*) e->ContextRecord->Rsp;
        cpu->running = false;

        // jump out of the JIT
        *e->ContextRecord = cpu->cont;
        return EXCEPTION_CONTINUE_EXECUTION;
    } else if (e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        // TODO(NeGate): NULLchk segv
        // void* accessed = (void*) e->ExceptionRecord->ExceptionInformation[1];
    }

    return EXCEPTION_CONTINUE_SEARCH;
}

bool tb_jit_thread_resume(TB_CPUContext* cpu, void* pc, uint64_t* ret, size_t arg_count, void** args) {
    // install exception handler, then we can run code
    void* handle = AddVectoredExceptionHandler(1, except_handler);

    // save our precious restore point
    cpu->running = true;
    RtlCaptureContext(&cpu->cont);

    if (cpu->running) {
        // continue in JITted code
        X64Params params;

        // pass int reg params (there's 4 on win64, 6 on sysv)
        assert(arg_count <= 4);
        for (size_t i = 0; i < arg_count && i < 4; i++) {
            params.gprs[i] = args[i];
        }

        typedef uint64_t (*Trampoline)(TB_CPUContext* cpu, void* pc, X64Params* params);
        uint64_t r = ((Trampoline)tb_jit__trampoline)(cpu, pc, &params);
        if (ret) { *ret = r; }

        cpu->running = false;
    }

    RemoveVectoredExceptionHandler(handle);
    return !cpu->done;
}
#endif
