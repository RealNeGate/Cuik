////////////////////////////////
// Interpreter
////////////////////////////////
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

typedef struct Env {
    // data stack
    size_t head;
    uint64_t stack[64];

    // control stack
    size_t control_head;
    uint64_t control[32];

    // debugging
    bool single_step;
} Env;

#define PEEK()  (env->stack[env->head - 1])
#define POP()   (env->stack[--env->head])
#define PUSH(x) (env->stack[env->head++] = (x))

static int jit_try_call(Env* env, Word* w);

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
            env->control[0] = (w - words.entries) << 32ull;
        }
    }

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
                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, w);
                if (r != INTERP_NO_JIT) {
                    // once the JIT finishes with a tail call, we just return
                    break;
                } else {
                    i = 0;
                }
            } else if (x <= -1000) {
                Word* new_w = &words.entries[-x - 1000];

                int r = env->single_step ? INTERP_NO_JIT : jit_try_call(env, new_w);
                if (r != INTERP_NO_JIT) {
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
                        tmp = env->stack[env->head - 1];
                        env->stack[env->head - 1] = env->stack[env->head - 2];
                        env->stack[env->head - 2] = tmp;
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

