#include <stdio.h>
#include <dyn_array.h>
#include <hash_map.h>
#include <perf.h>

#define CUIK_FS_IMPL
#include <cuik_fs.h>

#include <tb.h>
#include <arena.h>

// compile threads are separate from interpreter threads
#include <threads.h>
#include <stdatomic.h>

typedef struct Env Env;
typedef struct Word Word;

enum {
    // arithmatic
    OP_ADD = -1,
    OP_SUB = -2,
    OP_MUL = -3,
    OP_DIV = -4,

    // stack
    OP_DUP  = -5,
    OP_DROP = -6,

    // control flow
    OP_IF    = -7,  // if{
    OP_ELSE  = -8,  // }else{
    OP_CLOSE = -9, // }

    OP_TAIL  = -10, // no name, generated automatically

    // debug
    OP_DUMP  = -11,

    // console
    OP_EMIT  = -12,
    OP_PRINT = -13,
    OP_KEY   = -14,
};

struct Word {
    NL_Slice name;
    DynArray(int64_t) ops;

    // type
    int arity, outputs;
    int tails;

    // for now there's no tiers or recompilation.
    //
    // so once 'trip_count' == JIT_THRESHOLD, we submit
    // to the JIT thread. once it's ready later we can
    // check 'jitted'
    _Atomic int trip_count;
    _Atomic(void*) jitted;
};

static void infer(Word* w);

////////////////////////////////
// Parser
////////////////////////////////
static DynArray(Word) words;
static NL_Strmap(int) dict;

typedef struct {
    const char* source;
} Parser;

static NL_Slice dup_slice(NL_Slice s) {
    uint8_t* new_mem = cuik_malloc(s.length);
    memcpy(new_mem, s.data, s.length);
    return (NL_Slice){ s.length, new_mem };
}

static NL_Slice lex(Parser* p) {
    // skip spaces & comments
    const char* line = p->source;
    for (;;) {
        while (*line == '\n' || *line == '\t' || *line == ' ') line++;

        if (*line == 0) {
            return (NL_Slice){ 0 };
        } else if (*line == '\\') {
            while (*line && *line != '\n') line++;
            continue;
        }

        break;
    }

    // read word
    const char* end = line;
    while (*end && *end != '\n' && *end != '\t' && *end != ' ') end++;

    // advance
    p->source = *end ? end + 1 : end;

    return (NL_Slice){ end - line, (const uint8_t*) line };
}

// parse builds up a word
static void parse(Parser* p, Word* w) {
    for (;;) {
        NL_Slice token = lex(p);

        if (token.length == 0) {
            break;
        } else if (token.length == 1 && token.data[0] == ':') { // define word
            NL_Slice name = dup_slice(lex(p));

            size_t id = dyn_array_length(words);
            dyn_array_put(words, (Word){ 0 });
            nl_map_put(dict, name, -1000 - id);

            // build up word inside root word
            Word kid = { 0 };
            parse(p, &kid);

            // split from the parse function to avoid resize problems
            kid.name = name;
            words[id] = kid;

            infer(&words[id]);
        } else if (token.length == 1 && token.data[0] == ';') {
            return;
        } else if (token.data[0] == '\'') {
            // char literals
            dyn_array_put(w->ops, token.data[1]);
            assert(token.data[2] == '\'');
        } else if (token.data[0] >= '0' && token.data[0] <= '9') {
            uint64_t num = 0;
            for (size_t i = 0; i < token.length; i++) {
                if (token.data[i] < '0' || token.data[i] > '9') {
                    break;
                }

                num *= 10;
                num += token.data[i] - '0';
            }

            dyn_array_put(w->ops, num);
        } else {
            ptrdiff_t search = nl_map_get(dict, token);
            if (search < 0) {
                printf("error: undefined word %.*s\n", (int) token.length, token.data);
                abort();
            }

            dyn_array_put(w->ops, dict[search].v);
        }
    }
}

////////////////////////////////
// Type checker
////////////////////////////////
// represents control flow structures like if{ }else{ }
typedef struct {
    // the outs of a Control must match
    int arity, pos, out_count;

    // TB specific
    TB_Node *entry, *exit;

    // didn't want to write true and false
    TB_Node *on, *off;

    // "phis"
    TB_Node** outs;
} Control;

typedef struct {
    int head;
    Control stack[64];
} ControlStack;

static Control* control_push(TB_Function* f, ControlStack* restrict cs, int pos, int arity) {
    Control* c = &cs->stack[cs->head++];
    c->arity = arity;
    c->pos = pos;

    if (f != NULL) {
        c->entry = tb_inst_get_control(f);
        c->on = tb_inst_region(f);
        c->off = tb_inst_region(f);
        c->exit = tb_inst_region(f);
    }

    return c;
}

static Control* control_peek(ControlStack* restrict cs) {
    return cs->head ? &cs->stack[cs->head - 1] : NULL;
}

static Control* control_pop(ControlStack* restrict cs) {
    return &cs->stack[--cs->head];
}

static int prim_arity(int64_t x) {
    switch (x) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        return 2;

        case OP_DUP:
        case OP_DROP:
        case OP_IF:
        case OP_PRINT:
        case OP_EMIT:
        return 1;

        case OP_DUMP:
        case OP_CLOSE:
        case OP_ELSE:
        case OP_KEY:
        return 0;

        default:
        assert(0 && "TODO");
        return -1;
    }
}

static void infer(Word* w) {
    // we'll be building this up as we go
    w->arity = 0;
    w->outputs = 0;

    ControlStack cs;
    cs.head = 0;

    bool has_tail = false;
    size_t head = 0, len = dyn_array_length(w->ops);
    for (size_t i = 0; i < len; i++) {
        int64_t x = w->ops[i];

        if (x >= 0) { // push literal
            head++;
        } else if (x <= -1000) {
            Word* new_w = &words[-x - 1000];

            // pluck parameter from outside the word
            if (new_w->arity > head) {
                w->arity += new_w->arity - head;
                head = 0;
            } else {
                head -= new_w->arity;
            }

            // check for tail recursion
            if (w == new_w) {
                // we're literally at the end
                size_t j = i + 1;
                if (j < len && w->ops[j] == OP_ELSE) {
                    // if it's an }else{ we might be able to reach the end
                    int depth = 1;
                    while (j < len && depth != 0) {
                        j += 1;

                        if (w->ops[j] == OP_IF) depth++;
                        if (w->ops[j] == OP_CLOSE) depth--;
                    }
                    assert(j != len && "missing }");
                }

                if (j == len - 1) {
                    w->ops[i] = OP_TAIL;
                    w->tails++;
                }
            }

            head += new_w->outputs;
        } else if (x == OP_TAIL) {
            // tails can't pluck parameter from outside the word
            assert(w->arity <= head);
            head -= w->arity;

            w->tails++;
        } else {
            int arity = prim_arity(x);

            // pluck parameter from outside the word
            if (arity > head) {
                w->arity += arity - head;
                head = 0;
            } else {
                head -= arity;
            }

            switch (x) {
                // arithmatic
                case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: head += 1; break;

                // stack
                case OP_DUP: head += 2; break;
                case OP_DROP: break;

                // console
                case OP_KEY: head += 1; break;
                case OP_EMIT: break;
                case OP_PRINT: break;

                // control flow
                case OP_IF: control_push(NULL, &cs, i, head); break;
                case OP_ELSE: {
                    Control* c = control_peek(&cs);
                    assert(c && "missing if{");

                    c->out_count = head;
                    head = c->arity;
                    break;
                }
                case OP_CLOSE: {
                    Control* c = control_pop(&cs);
                    assert(head == c->out_count && "stinky dynamic effects");
                    break;
                }

                default: assert(0 && "TODO");
            }
        }
    }

    assert(cs.head == 0 && "you're missing closing braces?");
    if (w->tails > 0) {
        assert(w->outputs == 0 && "tail recursive functions can't return things... yet?");
    } else {
        w->outputs = head;
    }

    // dump resulting type
    if (1) {
        printf(": %.*s ( ", (int) w->name.length, w->name.data);
        for (int i = 0; i < w->arity; i++) {
            printf("%c ", 'a'+i);
        }
        printf("-- ");
        for (int i = 0; i < head; i++) {
            printf("%c ", 'A'+i);
        }
        printf(") ... ;\n");
    }
}

////////////////////////////////
// Interpreter
////////////////////////////////
struct Env {
    size_t head;
    uint64_t stack[64];

    // control stack
    size_t control_head;
    uint64_t control[32];

    // debugging
    bool single_step;
};

#define PEEK()  (env->stack[env->head - 1])
#define POP()   (env->stack[--env->head])
#define PUSH(x) (env->stack[env->head++] = (x))

static void compile_word(Word* w);

static void push(Env* env, uint64_t x) {
    assert(env->head < 64);
    env->stack[env->head++] = x;
}

static uint64_t pop(Env* env) {
    assert(env->head > 0);
    return env->stack[--env->head];
}

static uint64_t peek(Env* env) {
    assert(env->head > 0);
    return env->stack[env->head - 1];
}

static void dump(Env* env) {
    printf("[ ");
    for (size_t i = env->head; i--;) {
        printf("%llu ", env->stack[i]);
    }
    printf("]\n");
}

// this is the interp() result
enum {
    // no errors
    INTERP_OK = 0,

    // not enough elements on the stack
    INTERP_UNDERFLOW,

    // the function
    INTERP_NO_JIT,

    // if env->single_step is on this is what we return
    INTERP_STEP,
};

#include "forth_jit.h"

// written as a non-recursive style to accomodate pausing and resuming
// because the JIT will do those sorts of things.
static int interp(Env* env, Word* w) {
    // this is used to initialize the control flow easily
    if (w != NULL) {
        // if the JIT is ready, we'll just use this, ideally
        // we switch to directly calling the JIT soon
        int r = jit_try_call(env, w);
        if (r != INTERP_NO_JIT) {
            return r;
        } else {
            env->control_head = 1;
            env->control[0] = 0;
        }
    }

    while (env->control_head > 0) recover: {
        // peek the top control
        uint64_t ip = env->control[env->control_head - 1];

        // process next instruction
        Word* w = &words[ip >> 32ull];
        size_t i = ip & 0xFFFFFFFF, len = dyn_array_length(w->ops);
        while (i < len) {
            int64_t x = w->ops[i];

            if (x >= 0) { // trivial literals
                push(env, w->ops[i]), i += 1;
            } else if (x == OP_TAIL) {
                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, w);
                if (r != INTERP_NO_JIT) {
                    // once the JIT finishes with a tail call, we just return
                    break;
                } else {
                    i = 0;
                }
            } else if (x <= -1000) {
                Word* new_w = &words[-x - 1000];

                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, w);
                if (r != INTERP_NO_JIT) {
                    i += 1;
                } else {
                    // save return continuation
                    env->control[env->control_head - 1] = ((w - words) << 32ull) | (i + 1);

                    // push new continuation
                    env->control[env->control_head++] = (new_w - words) << 32ull;
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
                    case OP_KEY:  push(env, getchar()); break;
                    case OP_EMIT: printf("%c", (char) pop(env)); break;
                    case OP_PRINT:printf("%lld", pop(env)); break;
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
                env->control[env->control_head - 1] = ((w - words) << 32ull) | i;
                return INTERP_STEP;
            }
        }

        // pop
        env->control_head -= 1;
        if (env->single_step) {
            return INTERP_STEP;
        }
    }

    return INTERP_OK;
}

int main(int argc, const char** argv) {
    cuik_init_terminal();

    thrd_t jit_thread;
    if (thrd_create(&jit_thread, jit_thread_routine, NULL) != thrd_success) {
        fprintf(stderr, "error: could not create JIT thread");
        return EXIT_FAILURE;
    }

    nl_map_create(dict, 256);
    nl_map_put_cstr(dict, "+",      OP_ADD);
    nl_map_put_cstr(dict, "-",      OP_SUB);
    nl_map_put_cstr(dict, "*",      OP_MUL);
    nl_map_put_cstr(dict, "/",      OP_DIV);
    nl_map_put_cstr(dict, ".",      OP_PRINT);
    nl_map_put_cstr(dict, "dup",    OP_DUP);
    nl_map_put_cstr(dict, "drop",   OP_DROP);
    nl_map_put_cstr(dict, "emit",   OP_EMIT);
    nl_map_put_cstr(dict, "dump",   OP_DUMP);
    nl_map_put_cstr(dict, "key",    OP_KEY);
    nl_map_put_cstr(dict, "if{",    OP_IF);
    nl_map_put_cstr(dict, "}else{", OP_ELSE);
    nl_map_put_cstr(dict, "}",      OP_CLOSE);
    nl_map_put_cstr(dict, "tail",   OP_TAIL);

    Env env;
    env.head = 0;
    env.single_step = false;

    Cuik_File* f = cuikfs_open("rect.forth", false);

    size_t len;
    if (!cuikfs_get_length(f, &len)) {
        printf("error: bad file!\n");
        return 1;
    }

    char* buffer = cuik_malloc(len + 1);
    cuikfs_read(f, buffer, len);
    buffer[len] = 0;

    cuikfs_close(f);

    // parse
    Word root = { 0 };
    dyn_array_put_uninit(words, 1);
    parse(&(Parser){ .source = buffer }, &root);
    words[0] = root;

    // run interpreter+JIT
    interp(&env, &root);

    // wait for JIT thread to stop
    jit.running = false;
    thrd_join(&jit_thread, NULL);

    return 0;
}
