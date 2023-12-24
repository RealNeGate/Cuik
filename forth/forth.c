#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <threads.h>
#include <inttypes.h>
#include <dyn_array.h>
#include <hash_map.h>
#include <mimalloc.h>
#include <tb.h>
#include <arena.h>
#include <chunked_array.h>

typedef uint64_t Val;

typedef struct Env Env;
typedef struct Words Words;
typedef struct StackFrame StackFrame;

typedef void Word(Env* env, uintptr_t arg);

// set of words in a stream, each is a code pointer except if
// the bottom bit is set you've got a argument following the ptr.
struct Words {
    // atomics (should be separated from the rest of crap to avoid false-sharing)
    _Alignas(64) struct {
        // compile queue
        _Atomic(Words*) next_compile;

        // stats
        int trips;
    };

    // compiler
    _Atomic(void*) jitted;

    // dbg info
    size_t name_len;
    const char* name;

    // code
    int cap, len;
    uintptr_t arr[];
};

// two kinds of stack frames: JIT and interpreted, we
// don't really wanna unpack all JIT frame details until
// queried so just track the PC+SP (we can reconstruct
// the rest later).
struct StackFrame {
    enum { STK_JIT_FRAME, STK_INTERP_FRAME } tag;
    struct StackFrame* prev;
    union {
        struct { void *pc, *sp; };    // JIT
        struct { Words* fn; int i; }; // Interp
    };
};

struct Env {
    // control stack
    StackFrame* ctrl;

    // data stack
    size_t top;
    Val stk[1023];
};

static void env_exit(int status) {
    __debugbreak();
    exit(status);
}

static void env_dump_stack(Env* env) {
    printf("Stack trace: (v=interpreted, V=compiled)\n");
    for (StackFrame* frame = env->ctrl; frame; frame = frame->prev) {
        printf("  %c  ", frame->tag["vV"]);
        if (frame->tag == STK_INTERP_FRAME) {
            Words* w = frame->fn;
            printf("%.*s+%d\n", (int) w->name_len, w->name, frame->i);
        } else {
            printf("\n");
        }
    }
}

static Val env_pop(Env* env) {
    if (env->top == 0) {
        printf("\x1b[31mERROR:\x1b[0m no value on the stack to use in operation.\n");
        env_dump_stack(env);
        env_exit(1);
    }

    return env->stk[--env->top];
}

static void env_push(Env* env, Val v) {
    if (env->top >= 1023) {
        printf("\x1b[31mERROR:\x1b[0m cannot push any more items to the data stack (%zu).\n", env->top);
        env_dump_stack(env);
        env_exit(1);
    }

    env->stk[env->top++] = v;
}

static Words* words_create(void) {
    Words* w = mi_aligned_alloc(64, sizeof(Words) + 64*sizeof(uintptr_t));
    *w = (Words) { .cap = 64 };
    return w;
}

static Words* word_add(Words* w, Word* fn) {
    if (w->cap == w->len) {
        w->cap *= 2;
        w = cuik_realloc(w, sizeof(Words) + w->cap*sizeof(uintptr_t));
    }
    w->arr[w->len++] = (uintptr_t) fn;
    return w;
}

static Words* word_add2(Words* w, Word* fn, void* arg) {
    if (w->cap+1 >= w->len) {
        w->cap *= 2;
        w = cuik_realloc(w, sizeof(Words) + w->cap*sizeof(uintptr_t));
    }

    uintptr_t raw = (uintptr_t) fn;
    w->arr[w->len++] = (raw << 1) | 1;
    w->arr[w->len++] = (uintptr_t) arg;
    return w;
}

static TB_Arena crap;
static NL_Strmap(Words*) dict;

////////////////////////////////
// Compile thread
////////////////////////////////
static struct {
    _Atomic bool running;

    TB_Arena arena;
    TB_Module* mod;
    TB_JIT* jit;

    // this is how we call from the interpreter
    TB_FunctionPrototype* proto;
} jit;

// dummy word such that the queue is never empty
static Words dummy_words = { 0 };

// queue for compilation tasks
static _Atomic(Words*) compile_writer = &dummy_words;
static _Atomic(Words*) compile_reader = &dummy_words;

static void push_to_compile(Words* w) {
    Words* old_writer;
    for (;;) {
        old_writer = atomic_load_explicit(&compile_writer, memory_order_relaxed);

        // try to steal the writer by setting NULL -> w (once it's non-NULL everyone else just sorta
        // waits until the head is moved forward which should happen... *promptly*)
        Words* null = NULL;
        if (atomic_compare_exchange_strong(&compile_writer->next_compile, &null, w)) {
            compile_writer = w;
            break;
        }
    }
}

static Words* pop_to_compile(void) {
    // if we can move the read head, we can take the item
    Words *old_reader, *next;
    do {
        old_reader = atomic_load_explicit(&compile_reader, memory_order_relaxed);
        next = old_reader->next_compile;
        if (next == NULL) {
            // queue is empty
            return NULL;
        }
    } while (!atomic_compare_exchange_strong(&compile_reader, &old_reader, next));

    // we can take our sweet time resetting next_compile, it's not on the queue
    // rn so it's not visible to other threads.
    next->next_compile = NULL;
    return next;
}

static int jit_thread_routine(void* arg) {
    tb_arena_create(&jit.arena, TB_ARENA_LARGE_CHUNK_SIZE);

    jit.running = true;
    jit.mod = tb_module_create_for_host(true);
    jit.jit = tb_jit_begin(jit.mod, 0);

    // all words share a prototype
    TB_PrototypeParam params[] = { { TB_TYPE_PTR }, { TB_TYPE_I64 } };
    jit.proto = tb_prototype_create(jit.mod, TB_STDCALL, 2, params, 0, NULL, false);

    while (atomic_load_explicit(&jit.running, memory_order_relaxed)) {
        Words* w = pop_to_compile();
        if (w == NULL) {
            // might wanna place a semaphore or futex to wake it up
            thrd_yield();
            continue;
        }

        printf("compile %.*s\n", (int) w->name_len, w->name);
    }

    tb_module_destroy(jit.mod);
    return 0;
}

////////////////////////////////
// Interpreter
////////////////////////////////
static void interp(Env* env, Words* restrict words) {
    // don't interpret the word if we've got a JIT alternative (it's never stale
    // if it were it would've been unlinked).
    Word* code_ptr = atomic_load_explicit(&words->jitted, memory_order_relaxed);
    if (code_ptr) {
        code_ptr(env, (uintptr_t) words);
        words->trips += 1;
        return;
    }

    // push stack frame
    TB_ArenaSavepoint sp = tb_arena_save(&crap);
    StackFrame* frame = tb_arena_alloc(&crap, sizeof(StackFrame));
    frame->tag  = STK_INTERP_FRAME;
    frame->prev = env->ctrl;
    frame->fn   = words;
    frame->i    = 0;
    env->ctrl   = frame;

    // TODO(NeGate): do cooler stats
    if (++words->trips == 100) {
        // enqueue
        push_to_compile(words);
    }

    size_t i = 0, len = words->len;
    while (i < len) {
        // decode phase
        uintptr_t ptr = words->arr[i++];
        uintptr_t aux = 0;
        if (ptr & 1) {
            ptr >>= 1;
            aux = words->arr[i++];
        }
        frame->i = i;

        // execute phase
        Word* fn = (Word*) ptr;
        fn(env, aux);

        assert(env->ctrl == frame && "we corrupted the stack i think?");
        i = frame->i;
    }

    env->ctrl = frame->prev;
    tb_arena_restore(&crap, sp);
}

static void push_num(Env* env, uintptr_t arg) {
    env_push(env, arg);
}

static void self_invoke(Env* env, uintptr_t arg) {
    env->ctrl->i = 0;
}

static void invoke(Env* env, uintptr_t arg) {
    interp(env, (Words*) arg);
}

static void add_num(Env* env, uintptr_t arg) {
    uint64_t tmp = env_pop(env);
    env_push(env, env_pop(env) + tmp);
}

static void print_num(Env* env, uintptr_t arg) {
    uint64_t a = env_pop(env);
    printf("%"PRIu64, a);
}

static void emit_char(Env* env, uintptr_t arg) {
    uint64_t a = env_pop(env);
    printf("%c", (char) a);
}

////////////////////////////////
// Parser
////////////////////////////////
static bool is_num(char ch)   { return ch >= '0' && ch <= '9'; }
static bool is_ident(char ch) { return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'); }

static const char* skip_ws(const char* str) {
    while (*str == ' ' ||*str == '\n' || *str == '\r') str++;
    return str;
}

static Words* parse(const char* str) {
    Words* words = words_create();
    words->name_len = 6;
    words->name     = "<root>";

    Words* top_level = NULL;
    NL_Slice word_name = { 0 };

    int errors = 0;
    while (*str) {
        str = skip_ws(str);

        const char* start = str;
        if (*str == 0) {
            break;
        } else if (is_num(*str)) {
            int num = 0;
            do { num *= 10, num += (*str - '0') % 10, str++; } while (is_num(*str));

            words = word_add2(words, push_num, (void*) (uintptr_t) num);
        } else if (is_ident(*str)) {
            do { str++; } while (is_ident(*str));

            // tail recursive symbols use a special word 'self_invoke'
            NL_Slice key = { str - start, (const uint8_t*) start };
            if (word_name.length == key.length && !memcmp(word_name.data, key.data, key.length)) {
                words = word_add(words, self_invoke);
            } else {
                ptrdiff_t search = nl_map_get(dict, key);
                if (search < 0) {
                    printf("\x1b[31mERROR:\x1b[0m missing word '%.*s'.\n", (int)(str - start), start);
                    errors++;
                    continue;
                }

                words = word_add2(words, invoke, dict[search].v);
            }
        } else if (*str == ':') {
            if (word_name.length != 0) {
                assert(0 && "can't define word inside word");
            }

            top_level = words;
            words = words_create();

            str = skip_ws(str + 1);
            if (!is_ident(*str)) {
                assert(0 && "parse error");
            }

            const char* start_name = str;
            do { str++; } while (is_ident(*str));

            word_name = (NL_Slice){ str - start_name, (const uint8_t*) start_name };
            words->name_len = str - start_name;
            words->name     = start_name;
        } else if (*str == ';') {
            nl_map_put(dict, word_name, words);
            words = top_level;
            word_name.length = 0;
            str++;
        } else {
            // single chars
            Word* w = NULL;
            switch (*str) {
                case '+': w = add_num;   break;
                case '.': w = print_num; break;
                default: assert(0 && "fuck");
            }
            words = word_add(words, w);
            str++;
        }
    }

    return words;
}

int main() {
    thrd_t jit_thread;
    if (thrd_create(&jit_thread, jit_thread_routine, NULL) != thrd_success) {
        fprintf(stderr, "error: could not create JIT thread");
        return EXIT_FAILURE;
    }

    tb_arena_create(&crap, TB_ARENA_LARGE_CHUNK_SIZE);

    printf("Hello, World!\n");
    Words* words = parse(
        "1 4 +\n"
        ": x 1 ;\n"
        ": loop 6 x + . loop ;\n"
        "loop\n"
    );

    Env* env = tb_arena_alloc(&crap, sizeof(Env));
    interp(env, words);

    jit.running = false;
    thrd_join(&jit_thread, NULL);
    env_exit(0);
    return 0;
}
