#include <setjmp.h>

////////////////////////////////
// Interpreter
////////////////////////////////
typedef struct {
    Dictionary* dict;
    union {
        uint64_t i;
        Obj* o;
    };
} Value;

#define VALUE_IS_INT(v)  ((v).dict == NULL)
#define VALUE_IS_OBJ(v) ((v).dict != NULL)

enum {
    VM_STACK_SIZE = 16384,
    VM_COOKIE = 0xDAADF00D,
};

typedef enum {
    // no errors
    VM_STATUS_OK = 0,

    // no errors except the function didn't have JITting ready
    VM_STATUS_NO_JIT,

    // not enough elements on the stack
    VM_STATUS_UNDERFLOW,

    // expected to pop correct type
    VM_STATUS_BAD_TYPE,

    // hit some breakpoint, even single stepping
    VM_STATUS_BREAK,
} VM_Status;

typedef struct VM_Thread VM_Thread;
struct VM_Thread {
    uint32_t cookie;

    // locked when running
    mtx_t lock;

    // scheduling
    VM_Thread* next;

    // debugging
    VM_Status status;
    bool single_step;

    // interpreter state
    size_t head;
    size_t control_head;

    Value stack[64];
    uint64_t control[32];

    // used by pause exception to safely leave
    jmp_buf early_exit;

    // thread-local poll site is used to manage the pause state.
    // it may be converted into a guard page to force a segfault.
    _Alignas(4096) volatile char poll_site[4096];

    // this struct is just the stack base
    char jit_stack[];
};

static void vm_interp(VM_Thread* env, Word* w);

// allocates thread state but doesn't run
static VM_Thread* vm_thread_new(void) {
    VM_Thread* t = VirtualAlloc(NULL, VM_STACK_SIZE, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    t->cookie = VM_COOKIE;
    mtx_init(&t->lock, mtx_plain);
    t->next = NULL;
    t->single_step = false;
    return t;
}

static LONG its_so_over(EXCEPTION_POINTERS* e) {
    // read from PAUSE_ADDR means we hit a safepoint during a pause
    if (e->ExceptionRecord->ExceptionCode == EXCEPTION_GUARD_PAGE) {
        // get thread from stack pointer
        VM_Thread* t = (VM_Thread*) (e->ContextRecord->Rsp & -VM_STACK_SIZE);
        if (t->cookie == VM_COOKIE) {
            // crawl stack, we need to convert any stack frames into VM frames.
            __debugbreak();

            // early out from resume site
            longjmp(t->early_exit, 1);
        }
    }

    return EXCEPTION_CONTINUE_SEARCH;
}

// takes a paused thread (potentially just initialized) and
// allows it to run.
//
// if w is non-NULL, we're forcing this to intercept the call stack
static void vm_thread_resume(VM_Thread* t, Word* w, int count, ...) {
    // we're now able to modify the interpreter state,
    // this is unlocked either by the end of this call
    // or safepoint.
    mtx_lock(&t->lock);
    if (setjmp(t->early_exit) == 0) {
        assert(t->head + count < 64);

        // extract varargs
        va_list ap;
        va_start(ap, count);
        for (size_t i = 0; i < count; i++) {
            int64_t x = va_arg(ap, int64_t);
            t->stack[t->head++] = (Value){ .i = x };
        }
        va_end(ap);

        // run
        vm_interp(t, w);
    }
    mtx_unlock(&t->lock);
}

static void vm_thread_pause(VM_Thread* t, Word* w) {
    // forces the JIT code to stop at the nearest safepoint
    DWORD old;
    if (!VirtualProtect((void*) &t->poll_site[0], 4096, PAGE_GUARD | PAGE_READONLY, &old)) {
        fprintf(stderr, "error: could not reset guard page!\n");
        abort();
    }

    // steal the lock
    mtx_lock(&t->lock);
    mtx_unlock(&t->lock);
}

#define PEEK()  (env->stack[env->head - 1])
#define POP()   (env->stack[--env->head])
#define PUSH(x) (env->stack[env->head++] = (x))

static int jit_try_call(VM_Thread* env, Word* w);

static void pusho(VM_Thread* env, Dictionary* dict, Obj* o) {
    assert(env->head < 64);
    env->stack[env->head++] = (Value){ dict, .o = o };
}

static void push(VM_Thread* env, int64_t x) {
    assert(env->head < 64);
    env->stack[env->head++] = (Value){ .i = x };
}

static uint64_t pop(VM_Thread* env) {
    assert(env->head > 0 && env->stack[env->head - 1].dict == NULL);
    return env->stack[--env->head].i;
}

static Obj* popo(VM_Thread* env) {
    assert(env->head > 0 && env->stack[env->head - 1].dict != NULL);
    return env->stack[--env->head].o;
}

static uint64_t peek(VM_Thread* env) {
    assert(env->head > 0 && env->stack[env->head - 1].dict == NULL);
    return env->stack[env->head - 1].i;
}

static void dump(VM_Thread* env) {
    printf("[ ");
    for (size_t i = env->head; i--;) {
        if (env->stack[i].dict) {
            printf("%p ", env->stack[i].o);
        } else {
            printf("%llu ", env->stack[i].i);
        }
    }
    printf("]\n");
}

// written as a non-recursive style to accomodate pausing and resuming
// because the JIT will do those sorts of things.
static void vm_interp(VM_Thread* env, Word* w) {
    // this is used to initialize the control flow easily
    if (w != NULL) {
        // if the JIT is ready, we'll just use this, ideally
        // we switch to directly calling the JIT soon
        int r = jit_try_call(env, w);
        if (r != VM_STATUS_NO_JIT) {
            env->status = r;
            return;
        } else {
            env->control[env->control_head++] = (w - words.entries) << 32ull;
        }
    }

    cuikperf_region_start("interp", NULL);
    while (env->control_head > 0) recover: {
        // peek the top control
        uint64_t ip = env->control[env->control_head - 1];

        // process next instruction
        w = &words.entries[ip >> 32ull];
        size_t i = ip & 0xFFFFFFFF, len = dyn_array_length(w->ops);
        while (i < len) {
            int64_t x = w->ops[i];

            if (x >= 0) { // trivial literals
                push(env, w->ops[i]), i += 1;
            } else if (x == OP_TAIL) {
                int r = env->single_step ? VM_STATUS_NO_JIT : jit_try_call(env, w);
                if (r != VM_STATUS_NO_JIT) {
                    // once the JIT finishes with a tail call, we just return
                    break;
                } else {
                    i = 0;
                }
            } else if (x <= -1000) {
                Word* new_w = &words.entries[-x - 1000];

                int r = env->single_step ? VM_STATUS_NO_JIT : jit_try_call(env, new_w);
                if (r != VM_STATUS_NO_JIT) {
                    i += 1;
                } else {
                    // save return continuation
                    env->control[env->control_head - 1] = ((w - words.entries) << 32ull) | (i + 1);

                    // push new continuation
                    env->control[env->control_head++] = (new_w - words.entries) << 32ull;
                    goto recover;
                }
            } else {
                uint64_t tmp;
                switch (x) {
                    case OP_ADD:  tmp = pop(env), push(env, pop(env) + tmp); break;
                    case OP_SUB:  tmp = pop(env), push(env, pop(env) - tmp); break;
                    case OP_MUL:  tmp = pop(env), push(env, pop(env) * tmp); break;
                    case OP_DIV:  tmp = pop(env), push(env, pop(env) / tmp); break;
                    case OP_DUP:  tmp = peek(env), push(env, tmp); break;
                    case OP_DROP: env->head -= 1; break;
                    case OP_SWAP: {
                        Value t = env->stack[env->head - 1];
                        env->stack[env->head - 1] = env->stack[env->head - 2];
                        env->stack[env->head - 2] = t;
                        break;
                    }
                    case OP_READ: {
                        uint64_t* addr = (uint64_t*) pop(env);
                        push(env, *addr);
                        break;
                    }
                    case OP_WRITE: {
                        uint64_t* addr = (uint64_t*) pop(env);
                        uint64_t val = pop(env);
                        *addr = val;
                        break;
                    }
                    case OP_KEY:  push(env, getchar()); break;
                    case OP_EMIT: printf("%c", (char) pop(env)); break;
                    case OP_PRINT: printf("%lld", pop(env)); break;
                    case OP_RECT: {
                        int h = pop(env);
                        int w = pop(env);
                        int y = pop(env);
                        int x = pop(env);
                        draw_rect2(0xFFE6E1E5, x, y, w, h);
                        break;
                    }
                    case OP_DUMP: dump(env); break;
                    case OP_IF: {
                        // find matching }else{
                        int depth = 0, brace = i;
                        while (brace < len) {
                            brace += 1;

                            if (w->ops[brace] == OP_IF) depth++;
                            if (w->ops[brace] == OP_CLOSE) depth--;
                            if (w->ops[brace] == OP_ELSE && depth == 0) break;
                        }
                        assert(brace != len && "missing }else{");

                        int x = pop(env);
                        if (x == 0) {
                            i = brace; // jump to else
                        }
                        break;
                    }
                    case OP_ELSE: {
                        // skip to matching closing brace
                        int depth = 1, brace = i;
                        while (brace < len && depth != 0) {
                            brace += 1;

                            if (w->ops[brace] == OP_IF) depth++;
                            if (w->ops[brace] == OP_CLOSE) depth--;
                        }
                        assert(brace != len && "missing }");

                        i = brace;
                        break;
                    }
                    case OP_CLOSE: break; // doesn't really do anything, it's more of a marker
                    default: assert(0 && "TODO"); break;
                }
                i += 1;
            }

            if (env->single_step) {
                // interpreter savepoint
                env->control[env->control_head - 1] = ((w - words.entries) << 32ull) | i;
                env->status = VM_STATUS_BREAK;
                goto done;
            }
        }

        // construct object here
        if (w->tag == WORD_TYPE) {
            assert(0);
            /*Dictionary* dict = w->as_type;
            assert(dict != NULL);

            Obj* o = tb_arena_alloc(&young_gen, sizeof(Obj) + dict->data_size);
            o->dict = dict;
            memset(o->data, 0, dict->data_size);

            // pop + copy fields
            for (Field* f = dict->fields; f; f = f->next) {
                int64_t v = pop(env);
                memcpy(o->data + f->offset, &v, sizeof(v));
            }

            push(env, (uintptr_t) o);*/
        }

        // exit word
        env->control_head -= 1;
        if (env->single_step) {
            env->status = VM_STATUS_BREAK;
            goto done;
        }
    }

    env->status = VM_STATUS_OK;

    done:
    cuikperf_region_end();
}
