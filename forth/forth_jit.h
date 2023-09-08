#define JIT_THRESHOLD 50

#define PP_NARG(...)  PP_NARG_(__VA_ARGS__,PP_RSEQ_N())
#define PP_NARG_(...) PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N( \
    _1, _2, _3, _4, _5, _6, _7, _8, _9,_10, \
    _11,_12,_13,_14,_15,_16,_17,_18,_19,_20, \
    _21,_22,_23,_24,_25,_26,_27,_28,_29,_30, \
    _31,_32,_33,_34,_35,_36,_37,_38,_39,_40, \
    _41,_42,_43,_44,_45,_46,_47,_48,_49,_50, \
    _51,_52,_53,_54,_55,_56,_57,_58,_59,_60, \
    _61,_62,_63,N,...) N
#define PP_RSEQ_N() \
63,62,61,60,59,58,57,56,55,54, \
53,52,51,50,49,48,47,46,45,44, \
43,42,41,40,39,38,37,36,35,34, \
33,32,31,30,29,28,27,26,25,24, \
23,22,21,20,19,18,17,16,15,14, \
13,12,11,10,9,8,7,6,5,4,3,2,1,0

// depends on the forth interpreter
static int interp(Env* env, Word* w);

enum {
    FOREIGN_INTERP,
    FOREIGN_GETCHAR,
    FOREIGN_PUTCHAR,
    FOREIGN_PUTNUM,
    FOREIGN_RECT,

    FOREIGN_MAX,
};

typedef struct {
    TB_FunctionPrototype *proto;
    TB_Symbol *sym;
} JIT_ForeignFunc;

typedef int (*JIT_Caller)(Env* env, Value* args, void* fn);

// for now it's 1 jit thread, N forths
static struct {
    _Atomic bool running;

    TB_Arena arena;
    TB_Module* mod;
    TB_JIT* jit;

    // this is how we call from the interpreter
    TB_FunctionPrototype* proto;

    // the JIT will load these on startup for runtime support
    JIT_ForeignFunc builtins[FOREIGN_MAX];

    // words are pushed to a queue and then compiled async
    _Atomic uint32_t queue;
    void* queue_elems[256];

    // the JIT will use functions where the arguments are proper arguments
    // as opposed to in-memory stack elements. each needs a matching JITted
    // caller function, we do these lazily.
    _Atomic(TB_FunctionPrototype*) internal_protos[16];

    // these are used to call JITted words from interpreters
    _Atomic(uintptr_t) callers[16];
} jit;

// have the sign bit set to make them look distinct from pointers.
static uintptr_t jit__thread_key(void) {
    #ifdef _WIN32
    extern unsigned long GetCurrentThreadId();
    uintptr_t key = GetCurrentThreadId();
    #else
    #error "Unsupported"
    #endif

    // set sign bit
    key |= ~(UINTPTR_MAX >> 1);

    return key;
}

static bool should_early_inline(Word* w) {
    return w->tag != WORD_TYPE && dyn_array_length(w->ops) < 10;
}

////////////////////////////////
// Compilation queue
////////////////////////////////
// i love forward decls gon...
static JIT_Caller jit__get_trampoline(size_t arity);
static void* jit__compile_trampoline(TB_FunctionPrototype* proto, int arity);

static void jit__queue_push(uint32_t exp, _Atomic uint32_t* queue, void** elems, void* val);
static void* jit__queue_pop(uint32_t exp, _Atomic uint32_t* queue, void** elems);

// we don't need to waste cycles JITting this lmao
static int jit__caller_0ary(Env* env, Value* stack, void* jitted) {
    return ((int (*)(Env*)) jitted)(env);
}

int jit_try_call(Env* env, Word* w) {
    // never jit:
    if (w->tag == WORD_TYPE) {
        return INTERP_NO_JIT;
    }

    void* fn = atomic_load(&w->jitted);
    if (fn == NULL) {
        if (atomic_fetch_add(&w->trip_count, 1) == JIT_THRESHOLD) {
            // check if a parent is more suited
            if (env->control_head >= 1) {
                Word* best = w;
                for (size_t i = env->control_head; i--;) {
                    if (!should_early_inline(best)) break;

                    best = &words.entries[env->control[i] >> 32ull];
                }

                if (best != w) {
                    // put word into queue
                    if (atomic_compare_exchange_strong(&best->jit_mark, &(bool){ false }, true)) {
                        log_debug("jit: picked a better compile site above %.*s (%.*s)", (int) w->name.length, w->name.data, (int) best->name.length, best->name.data, JIT_THRESHOLD);
                        jit__queue_push(8, &jit.queue, jit.queue_elems, best);
                    }

                    // if it's so good to inline, then we should expect it has
                    // been even if it's "best" can't be compiled :)
                    return INTERP_NO_JIT;
                }
            }

            // we don't wanna send it in twice in case it got found via a call stack walk
            if (atomic_compare_exchange_strong(&w->jit_mark, &(bool){ false }, true)) {
                log_debug("jit: trip count exceeded for %.*s", (int) w->name.length, w->name.data, JIT_THRESHOLD);
                // put word into queue
                jit__queue_push(8, &jit.queue, jit.queue_elems, w);
            }
        }

        return INTERP_NO_JIT;
    }

    size_t arity = w->type.in_count;
    if (env->head < arity) {
        return INTERP_UNDERFLOW;
    }

    // type check trampoline
    JIT_Caller c = jit__get_trampoline(arity);
    return c(env, &env->stack[env->head - arity], w->jitted);
}

static JIT_Caller jit__get_trampoline(size_t arity) {
    if (arity == 0) {
        return jit__caller_0ary;
    }

    // if the caller is not ready, make sure to initialize it now.
    // since it can be called on multiple threads, once one of the
    // threads grabs hold, it'll compile while the others wait, this
    // is a really tiny compilation so it's ok.
    cuikperf_region_start("get trampoline", NULL);
    uintptr_t sign_bit = ~(UINTPTR_MAX >> 1);
    uintptr_t key = jit__thread_key();

    _Atomic(uintptr_t)* caller = &jit.callers[arity];
    uintptr_t p;
    retry: {
        p = atomic_load(caller);

        // check if someone is already working on it
        if (p & sign_bit) {
            assert(p != key && "somehow we've already started compiling the caller on this thread?");
            goto retry;
        }

        if (p == 0) {
            // try lock?
            if (!atomic_compare_exchange_strong(caller, &(uintptr_t){ 0 }, key)) {
                goto retry;
            }

            // make prototype
            TB_PrototypeParam params[17];
            params[0] = (TB_PrototypeParam){ TB_TYPE_PTR }; // Env*
            for (size_t i = 0; i < arity; i++) {
                params[i + 1] = (TB_PrototypeParam){ TB_TYPE_I64 };
            }

            TB_PrototypeParam ret = { TB_TYPE_I32 };
            TB_FunctionPrototype* proto = tb_prototype_create(jit.mod, TB_STDCALL, arity + 1, params, 1, &ret, false);
            jit.internal_protos[arity] = proto;

            // compile caller
            p = (uintptr_t) jit__compile_trampoline(proto, arity);

            // unlock
            atomic_store(caller, p);
        }
    }

    // it's ready
    cuikperf_region_end();
    return (JIT_Caller) p;
}

static void* jit__compile_trampoline(TB_FunctionPrototype* proto, int arity) {
    char name[32];
    snprintf(name, 32, "jit_caller_%dary", arity);

    // generate caller
    TB_Function* f = tb_function_create(jit.mod, -1, name, TB_LINKAGE_PUBLIC, TB_COMDAT_NONE);
    tb_function_set_prototype(f, jit.proto, NULL);

    // top of the stack is gonna be loaded into params
    TB_Node* tos = tb_inst_param(f, 1);

    TB_Node* args[17];
    args[0] = tb_inst_param(f, 0);
    for (size_t i = 0; i < arity; i++) {
        size_t offset = i*sizeof(Value) + offsetof(Value, i);

        TB_Node* ptr = tb_inst_member_access(f, tos, offset);
        args[i + 1] = tb_inst_load(f, TB_TYPE_I64, ptr, _Alignof(size_t), false);
    }

    TB_MultiOutput o = tb_inst_call(f, proto, tb_inst_param(f, 2), arity + 1, args);
    tb_inst_ret(f, 1, &o.single);

    // JIT & export
    TB_Passes* p = tb_pass_enter(f, tb_function_get_arena(f));
    tb_pass_codegen(p, false);
    tb_pass_exit(p);

    // we can throw away the function IR now
    tb_arena_clear(tb_function_get_arena(f));

    void* fn;
    CUIK_TIMED_BLOCK("apply") {
        fn = tb_jit_place_function(jit.jit, f);
    }
    return fn;
}

////////////////////////////////
// Compilation queue
////////////////////////////////
static void jit__queue_push(uint32_t exp, _Atomic uint32_t* queue, void** elems, void* val) {
    uint32_t mask = (1u << exp) - 1;

    for (;;) {
        // might wanna change the memory order on this atomic op
        uint32_t r = *queue;

        uint32_t head = r & mask;
        uint32_t tail = (r >> 16) & mask;
        uint32_t next = (head + 1u) & mask;
        if (r & 0x8000) { // avoid overflow on commit
            *queue &= ~0x8000;
        }

        // it don't fit...
        if (next != tail) {
            elems[head] = val;

            // commit
            *queue += 1;
            return;
        }

        log_warn("the queue is choked up!");
    }
}

static void* jit__queue_pop(uint32_t exp, _Atomic uint32_t* queue, void** elems) {
    uint32_t mask = (1u << exp) - 1;

    uint32_t r;
    ptrdiff_t elem = -1;
    void* tmp;

    do {
        r = *queue;
        uint32_t head = r & mask;
        uint32_t tail = (r >> 16) & mask;

        if (head == tail) {
            // take a nap if we ain't find shit
            return NULL;
        }

        // copy out before we commit
        tmp = elems[tail];
    } while (!atomic_compare_exchange_strong(queue, &r, r + 0x10000));

    return tmp;
}

////////////////////////////////
// Compiler thread
////////////////////////////////
static void jit__compile_blob(Env* env, Word* w, const char* name);

// used by jitted code, needs to be bound on startup
static void jit__putnum(int64_t num) { printf("%lld", num); }
static void jit__rect(int x, int y, int w, int h) { draw_rect2(0xFFE6E1E5, x, y, w, h); }

#define JIT__BIND(i, n, r, ...) jit__bind_builtin_foreign(FOREIGN_ ## i, #n, &n, r, PP_NARG(__VA_ARGS__), ## __VA_ARGS__)
static void jit__bind_builtin_foreign(size_t index, const char* name, void* ptr, TB_DataType ret, size_t param_count, ...) {
    assert(param_count < 16);
    TB_PrototypeParam proto_params[16];

    va_list ap;
    va_start(ap, param_count);
    for (size_t i = 0; i < param_count; i++) {
        proto_params[i] = (TB_PrototypeParam){ va_arg(ap, TB_DataType) };
    }
    va_end(ap);

    bool has_ret = !(ret.type == TB_INT && ret.data == 0);
    TB_PrototypeParam proto_ret = { ret };

    assert(index < FOREIGN_MAX);
    jit.builtins[index].proto = tb_prototype_create(jit.mod, TB_STDCALL, param_count, proto_params, has_ret, &proto_ret, false);
    jit.builtins[index].sym = (TB_Symbol*) tb_extern_create(jit.mod, -1, name, TB_EXTERNAL_SO_EXPORT);
    tb_symbol_bind_ptr(jit.builtins[index].sym, ptr);
}

static int jit_thread_routine(void* arg) {
    ////////////////////////////////
    // Initializer compiler
    ////////////////////////////////
    spallperf__start_thread();

    Env* env = arg;
    CUIK_TIMED_BLOCK("init") {
        tb_arena_create(&jit.arena, TB_ARENA_LARGE_CHUNK_SIZE);

        TB_FeatureSet features = { 0 };
        jit.mod = tb_module_create_for_host(&features, true);
        jit.jit = tb_jit_begin(jit.mod, 0);

        // all JIT regions use the same prototype
        TB_PrototypeParam params[] = { { TB_TYPE_PTR }, { TB_TYPE_PTR }, { TB_TYPE_PTR } };
        TB_PrototypeParam ret = { TB_TYPE_I32 };
        jit.proto = tb_prototype_create(jit.mod, TB_STDCALL, 3, params, 1, &ret, false);

        jit.internal_protos[0] = tb_prototype_create(
            jit.mod, TB_STDCALL,
            1, &(TB_PrototypeParam){ TB_TYPE_PTR },
            1, &(TB_PrototypeParam){ TB_TYPE_I32 },
            false
        );

        JIT__BIND(INTERP,  interp,      TB_TYPE_I32,  TB_TYPE_PTR, TB_TYPE_PTR);
        JIT__BIND(PUTCHAR, putchar,     TB_TYPE_I32,  TB_TYPE_I32);
        JIT__BIND(PUTNUM,  jit__putnum, TB_TYPE_VOID, TB_TYPE_I64);
        JIT__BIND(GETCHAR, getchar,     TB_TYPE_I32);
        JIT__BIND(RECT,    jit__rect,   TB_TYPE_VOID, TB_TYPE_I32, TB_TYPE_I32, TB_TYPE_I32, TB_TYPE_I32);

        jit.running = true;
    }

    // Compile words which were submitted
    while (jit.running) {
        // pop word
        Word* w = jit__queue_pop(8, &jit.queue, jit.queue_elems);
        if (w == NULL) {
            // might wanna place a semaphore or futex to wake it up
            thrd_yield();
            continue;
        }

        char name[32];
        snprintf(name, 32, "%.*s", (int) w->name.length, w->name.data);

        CUIK_TIMED_BLOCK_ARGS("blob", name) {
            jit__compile_blob(env, w, name);
        }
        // log_debug("jit: compiled %s", name);
    }

    tb_module_destroy(jit.mod);
    spallperf__stop_thread();
    return 0;
}

////////////////////////////////
// IR generation
////////////////////////////////
typedef struct {
    TB_Function* f;

    // we cache the symbols
    TB_Node* sym_addr_cache[FOREIGN_MAX];

    TB_Node* env;
    ControlStack cs;

    // how many words in the blob, used for early-inlining
    size_t accum_words;

    size_t head;
    TB_Node* stack[64];
} JIT_Builder;

static TB_Node* jit__get_builtin(JIT_Builder* ctx, size_t id) {
    assert(id < FOREIGN_MAX);
    if (ctx->sym_addr_cache[id] == NULL) {
        ctx->sym_addr_cache[id] = tb_inst_get_symbol_address(ctx->f, jit.builtins[id].sym);
    }

    return ctx->sym_addr_cache[id];
}

static TB_Node* jit__call_builtin(JIT_Builder* ctx, size_t id, size_t count, TB_Node** args) {
    assert(id < FOREIGN_MAX);
    return tb_inst_call(ctx->f, jit.builtins[id].proto, jit__get_builtin(ctx, id), count, args).single;
}

static TB_Node* jit__shift_head(TB_Function* f, TB_Node* env, int delta, bool post) {
    TB_Node* head_ptr = tb_inst_member_access(f, env, offsetof(Env, head));
    TB_Node* ld_head = tb_inst_load(f, TB_TYPE_I64, head_ptr, _Alignof(size_t), false);

    // writeback new head
    if (delta != 0) {
        // write out new head
        TB_Node* add_head = NULL;
        if (delta < 0) {
            add_head = tb_inst_sub(f, ld_head, tb_inst_sint(f, TB_TYPE_I64, -delta), 0);
        } else {
            add_head = tb_inst_add(f, ld_head, tb_inst_sint(f, TB_TYPE_I64, delta), 0);
        }
        tb_inst_store(f, TB_TYPE_I64, head_ptr, add_head, _Alignof(size_t), false);
        return post ? add_head : ld_head;
    }

    return ld_head;
}

static TB_Node* jit__spill_stack(TB_Function* f, TB_Node* env, TB_Node** stack, TB_Node* ld_head, int count) {
    // dump stack (args is the top of the stack)
    TB_Node* tos = tb_inst_member_access(f, env, offsetof(Env, stack));
    tos = tb_inst_array_access(f, tos, ld_head, sizeof(Value));

    for (size_t i = 0; i < count; i++) {
        TB_Node* ptr = tb_inst_member_access(f, tos, i * sizeof(Value));
        tb_inst_store(f, TB_TYPE_I64, ptr, stack[i], _Alignof(size_t), false);
    }

    return tos;
}

static void jit__compile_word(JIT_Builder* ctx, Word* w) {
    TB_Function* f = ctx->f;
    TB_Node* env = ctx->env;

    // accumulate more words
    ctx->accum_words += dyn_array_length(w->ops);

    TB_Node** phis = NULL;
    TB_Node* loop_body = NULL;
    if (w->type.tails > 0) {
        phis = cuik_malloc(w->type.in_count * sizeof(TB_Node*));

        loop_body = tb_inst_region(f);
        tb_inst_goto(f, loop_body);
        tb_inst_set_control(f, loop_body);

        // convert parameters into placeholder PHIs
        TB_Node** params = ctx->stack + ctx->head - w->type.in_count;
        for (size_t i = 0; i < w->type.in_count; i++) {
            phis[i] = tb_inst_incomplete_phi(f, TB_TYPE_I64, loop_body, 1 + w->type.tails);
            phis[i]->inputs[1] = params[i];

            params[i] = phis[i];
        }
    }

    // functions we might import
    size_t head = ctx->head;
    TB_Node** stack = ctx->stack;

    size_t len = dyn_array_length(w->ops);
    for (size_t i = 0; i < len; i++) {
        if (tb_inst_get_control(f) == NULL) {
            tb_inst_set_control(f, tb_inst_region(f));
        }

        int64_t x = w->ops[i];

        if (x >= 0) { // push literal
            assert(head < 64);
            stack[head++] = tb_inst_sint(f, TB_TYPE_I64, x);
        } else if (x == OP_TAIL) {
            int arity = w->type.in_count;
            head -= arity;
            TB_Node** args = &stack[head];

            TB_Node* region = tb_get_parent_region(tb_inst_get_control(f));
            tb_inst_goto(f, loop_body);

            for (size_t i = 0; i < arity; i++) {
                if (!tb_inst_add_phi_operand(f, phis[i], region, args[i])) {
                    assert(0 && "we should've reserved enough phi operands?");
                }
            }
        } else if (x <= -1000) {
            Word* new_w = &words.entries[-x - 1000];
            assert(new_w != w && "recursive calls should've gone through OP_TAIL");
            ref_word(new_w - words.entries);

            if (should_early_inline(new_w)) {
                // always inline very tiny words
                CUIK_TIMED_BLOCK_ARGS("inlined", (const char*) new_w->name.data) {
                    ctx->head = head;
                    jit__compile_word(ctx, new_w);
                    head = ctx->head;
                }
            } else {
                head -= new_w->type.in_count;

                TB_Node** src_args = &stack[head];
                TB_Node* ld_head = jit__shift_head(f, env, new_w->type.in_count, false);

                // if a JITted form already exists, use that
                _Atomic(void*) jitted = atomic_load(&new_w->jitted);
                if (jitted == NULL) {
                    // we have to spill to the stack
                    if (new_w->type.in_count > 0) {
                        jit__spill_stack(f, env, stack, ld_head, new_w->type.in_count);
                    }

                    // this is kinda hacky but we're JITting it's fine
                    TB_Node* new_w_node = tb_inst_uint(f, TB_TYPE_PTR, (uintptr_t) new_w);

                    TB_Node* args[2] = { env, new_w_node };
                    jit__call_builtin(ctx, FOREIGN_INTERP, 2, args);
                } else {
                    // generate argument list
                    TB_Node* args[32];
                    args[0] = env;
                    for (size_t i = 0; i < new_w->type.in_count; i++) {
                        args[1 + i] = src_args[i];
                    }

                    // log_debug("jit %s: calling separate jit function %s (%p)", w->name.data, new_w->name.data, jitted);

                    // make sure the caller & proto are ready
                    jit__get_trampoline(new_w->type.in_count);

                    TB_Node* target = tb_inst_uint(f, TB_TYPE_PTR, (uintptr_t) jitted);
                    tb_inst_call(f, jit.internal_protos[new_w->type.in_count], target, 1 + new_w->type.in_count, args);
                }

                // pop returns off the stack
                int outputs = new_w->type.out_count;
                if (outputs > 0) {
                    TB_Node* ld_head = jit__shift_head(f, env, -outputs, true);

                    // dump stack (args is the top of the stack)
                    TB_Node* tos = tb_inst_member_access(f, env, offsetof(Env, stack));
                    tos = tb_inst_array_access(f, tos, ld_head, sizeof(size_t));

                    for (size_t i = 0; i < outputs; i++) {
                        TB_Node* ptr = tb_inst_member_access(f, tos, i * sizeof(uint64_t));

                        assert(new_w->type.outputs[i] == &int_dict);
                        src_args[i] = tb_inst_load(f, TB_TYPE_I64, ptr,  _Alignof(size_t), false);
                    }
                    head += outputs;
                }
            }
            unref_word(new_w - words.entries);
        } else {
            // all primitives have static effects, let's make sure all args
            // are available before we continue
            int arity = prim_arity(x);
            assert(head >= arity);
            head -= arity;
            TB_Node** args = &stack[head];

            TB_Node* tmp = NULL;
            switch (x) {
                // arithmatic
                case OP_ADD: stack[head++] = tb_inst_add(f, args[0], args[1], 0); break;
                case OP_SUB: stack[head++] = tb_inst_sub(f, args[0], args[1], 0); break;
                case OP_MUL: stack[head++] = tb_inst_mul(f, args[0], args[1], 0); break;

                // stack
                case OP_DUP: stack[head] = args[0], stack[head+1] = args[0], head += 2; break;
                case OP_DROP: /* no op, just throws away argument */ break;
                case OP_SWAP: tmp = args[0], args[0] = args[1], args[1] = tmp, head += 2; break;

                // memory
                case OP_READ: {
                    stack[head++] = tb_inst_load(f, TB_TYPE_I64, args[0], 8, false);
                    break;
                }
                case OP_WRITE: {
                    tb_inst_store(f, TB_TYPE_I64, args[1], args[0], 8, false);
                    break;
                }

                // console
                case OP_KEY: {
                    stack[head++] = tb_inst_sxt(f, jit__call_builtin(ctx, FOREIGN_GETCHAR, 0, NULL), TB_TYPE_I64);
                    break;
                }
                case OP_EMIT: {
                    TB_Node* t = tb_inst_trunc(f, args[0], TB_TYPE_I32);
                    jit__call_builtin(ctx, FOREIGN_PUTCHAR, 1, &t);
                    break;
                }
                case OP_PRINT: {
                    jit__call_builtin(ctx, FOREIGN_PUTNUM, 1, &args[0]);
                    break;
                }
                case OP_RECT: {
                    jit__call_builtin(ctx, FOREIGN_RECT, 4, &args[0]);
                    break;
                }

                // control flow
                case OP_IF: {
                    Control* c = control_push(f, &ctx->cs, i, head);
                    tb_inst_if(f, args[0], c->on, c->off);
                    tb_inst_set_control(f, c->on);
                    break;
                }
                case OP_ELSE: {
                    Control* c = control_peek(&ctx->cs);
                    assert(c && "missing if{");

                    // mark the "out_count" known at this point,
                    // if the else case doesn't match we'll cry
                    c->out_count = head;
                    c->outs = cuik_malloc(c->out_count * sizeof(TB_Node*));

                    // mark outputs
                    TB_Node** top = &stack[head - c->out_count];
                    size_t j = 0;
                    while (j < c->out_count) {
                        c->outs[j] = top[j], j += 1;
                    }

                    // reset arity to what the if said
                    head = c->arity;

                    tb_inst_goto(f, c->exit);
                    tb_inst_set_control(f, c->off);
                    break;
                }
                case OP_CLOSE: {
                    Control* c = control_pop(&ctx->cs);
                    assert(head == c->out_count && "stinky dynamic effects");

                    // fallthrough
                    tb_inst_goto(f, c->exit);
                    tb_inst_set_control(f, c->exit);

                    // insert phis
                    TB_Node** top = &stack[head - c->out_count];
                    size_t j = 0;
                    while (j < c->out_count) {
                        top[j] = tb_inst_phi2(f, c->exit, c->outs[j], top[j]);
                        j += 1;
                    }
                    break;
                }

                default: assert(0 && "TODO");
            }
        }
    }

    // writeback
    ctx->head = head;
}

// blob refers to a set of words, we start with the root but
// we will end up inlining early if things are small.
static void jit__compile_blob(Env* env, Word* w, const char* name) {
    JIT_Builder builder;
    builder.accum_words = 0;
    builder.cs.head = 0;

    for (size_t i = 0; i < FOREIGN_MAX; i++) {
        builder.sym_addr_cache[i] = NULL;
    }

    // we should be able to safely read the internal_protos now that we've guarenteed it's
    // created... no weird threading issues i think
    jit__get_trampoline(w->type.in_count);
    TB_FunctionPrototype* proto = atomic_load_explicit(&jit.internal_protos[w->type.in_count], memory_order_relaxed);

    TB_Function* f = builder.f = tb_function_create(jit.mod, -1, name, TB_LINKAGE_PUBLIC, TB_COMDAT_NONE);
    tb_function_set_prototype(f, proto, &jit.arena);

    // FLOAT INT 1 + +
    CUIK_TIMED_BLOCK("IR") {
        // fill parameter into the stack
        builder.head = w->type.in_count;
        builder.env = tb_inst_param(f, 0);
        for (size_t i = builder.head; i--;) {
            builder.stack[i] = tb_inst_param(f, i + 1);

            // cast parameter to pointer
            if (w->type.inputs[i] != &int_dict) {
                __debugbreak();
            }
        }

        // do word
        CUIK_TIMED_BLOCK_ARGS("word", (const char*) w->name.data) {
            jit__compile_word(&builder, w);
        }

        // finalize the blob, spill leftover stack elements
        if (tb_inst_get_control(f) != NULL) {
            assert(builder.head == w->type.out_count);
            if (builder.head != w->type.in_count) {
                TB_Node* ld_head = jit__shift_head(f, builder.env, builder.head - w->type.in_count, false);

                if (builder.head > 0) {
                    jit__spill_stack(f, builder.env, builder.stack, ld_head, builder.head);
                }
            }

            TB_Node* ret = tb_inst_uint(f, TB_TYPE_I32, INTERP_OK);
            tb_inst_ret(f, 1, &ret);
        }
    }
    assert(builder.cs.head == 0 && "you're missing closing braces?");

    TB_Passes* p = tb_pass_enter(f, &jit.arena);
    {
        // optimizer
        tb_pass_peephole(p, TB_PEEPHOLE_ALL);

        // tb_pass_print(p);
        // tb_function_print(f, tb_default_print_callback, stdout);

        // compile
        TB_FunctionOutput* out = tb_pass_codegen(p, false);
        // tb_output_print_asm(out, stdout);
    }
    tb_pass_exit(p);

    // write out results
    CUIK_TIMED_BLOCK("apply") {
        w->jitted = tb_jit_place_function(jit.jit, f);
    }
}
