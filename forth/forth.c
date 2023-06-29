#include <stdio.h>
#include <dyn_array.h>
#include <hash_map.h>

#define CUIK_FS_IMPL
#include <cuik_fs.h>

#include <tb.h>

#define JIT_THRESHOLD 1

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
    OP_LOOP  = -9,  // loop{
    OP_CLOSE = -10, // }

    // debug
    OP_DUMP  = -11,

    // console
    OP_EMIT  = -12,
    OP_PRINT = -13,
};

typedef int (*WordJIT)(Env* env, uint64_t* args);

struct Word {
    NL_Slice name;
    DynArray(int64_t) ops;

    // JIT
    int trip_count, arity;
    WordJIT jit;
};

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
    p->source = end + 1;

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
        } else if (token.length == 1 && token.data[0] == ';') {
            return;
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
                break;
            }

            dyn_array_put(w->ops, dict[search].v);
        }
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

    // if env->single_step is on this is what we return
    INTERP_STEP,
};

// written as a non-recursive style to accomodate pausing and resuming
// because the JIT will do those sorts of things.
static int interp(Env* env, Word* w) {
    while (env->control_head > 0) {
        // this is how non-return continuations happen
        recover:

        // peek the top control
        uint64_t ip = env->control[env->control_head - 1];

        // process next instruction
        Word* w = &words[ip >> 32ull];
        size_t i = ip & 0xFFFFFFFF, len = dyn_array_length(w->ops);
        while (i < len) {
            int64_t x = w->ops[i];

            if (x >= 0) { // trivial literals
                push(env, w->ops[i]), i += 1;
            } else if (x <= -1000) {
                Word* new_w = &words[-x - 1000];

                // this is where the JIT happens. we
                // hit enough trips and we'll attempt
                // to compile the word.
                if (new_w->jit != NULL) {
                    assert(env->head >= new_w->arity && "JIT entered without enough arguments");
                    new_w->jit(env, &env->stack[env->head - new_w->arity]);

                    i += 1;
                    continue;
                } else {
                    if (new_w->trip_count++ >= JIT_THRESHOLD) {
                        // it's async in theory so it won't be ready
                        // until next time
                        compile_word(new_w);
                    }
                }

                if (i != len - 1) {
                    // save return continuation
                    env->control[env->control_head - 1] = ((w - words) << 32ull) | (i + 1);

                    // push new continuation
                    env->control[env->control_head++] = (new_w - words) << 32ull;
                    goto recover;
                }

                // tail call
                i = 0, w = new_w, len = dyn_array_length(w->ops);
            } else {
                uint64_t tmp;
                switch (x) {
                    case OP_ADD:  tmp = pop(env), push(env, pop(env) + tmp); break;
                    case OP_SUB:  tmp = pop(env), push(env, pop(env) - tmp); break;
                    case OP_MUL:  tmp = pop(env), push(env, pop(env) * tmp); break;
                    case OP_DIV:  tmp = pop(env), push(env, pop(env) / tmp); break;
                    case OP_DUP:  tmp = peek(env), push(env, tmp); break;
                    case OP_DROP: env->head -= 1; break;
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
                    default: break;
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
    }

    return INTERP_OK;
}

////////////////////////////////
// Compiler
////////////////////////////////
static TB_Module* ir_module;
static TB_JITContext* jit;
static TB_FunctionPrototype* proto;

// imported functions
static TB_FunctionPrototype* putchar_proto;
static TB_Symbol* putchar_sym;

static int prim_arity(int64_t x) {
    switch (x) {
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        return 2;

        case OP_DUP:
        case OP_DROP:
        case OP_EMIT:
        case OP_IF:
        case OP_PRINT:
        return 1;

        case OP_DUMP:
        case OP_CLOSE:
        case OP_ELSE:
        return 0;

        default:
        assert(0 && "TODO");
        return -1;
    }
}

// represents control flow structures like if{ }else{ }
typedef struct {
    // the outs of a Control must match
    int arity, pos, out_count;
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
    c->entry = tb_inst_get_control(f);
    c->on = tb_inst_region(f);
    c->off = tb_inst_region(f);
    c->exit = tb_inst_region(f);
    return c;
}

static Control* control_peek(ControlStack* restrict cs) {
    return cs->head ? &cs->stack[cs->head - 1] : NULL;
}

static Control* control_pop(ControlStack* restrict cs) {
    return &cs->stack[--cs->head];
}

static void jit_helper_emit(int64_t x) {
    printf("%c", (char) x);
}

static void compile_word(Word* w) {
    char name[16];
    int name_len = w->name.length > 15 ? 15 : w->name.length;
    memcpy(name, w->name.data, name_len);
    name[name_len] = 0;

    // log_debug("jit: compiling %s after %d trips", name, w->trip_count);

    TB_Function* f = tb_function_create(ir_module, name, TB_LINKAGE_PUBLIC, TB_COMDAT_NONE);
    tb_function_set_prototype(f, proto);

    TB_Node* args = tb_inst_param(f, 1);

    // ir-based value stack
    size_t head = 0;
    TB_Node* stack[64];

    // we'll be building this up as we go
    w->arity = 0;

    ControlStack cs;
    cs.head = 0;

    // functions we might import
    TB_Node* putchar_n = NULL;

    size_t len = dyn_array_length(w->ops);
    for (size_t i = 0; i < len; i++) {
        int64_t x = w->ops[i];

        if (x >= 0) { // push literal
            assert(head < 64);
            stack[head++] = tb_inst_sint(f, TB_TYPE_I64, x);
        } else if (x <= -1000) {
            assert(0 && "TODO");
        } else {
            // all primitives have static effects, let's make sure all args
            // are available before we continue
            int arity = prim_arity(x);
            while (head < arity) {
                // infer that the leftovers are arguments
                // so we pop them.
                assert(head < 64);

                // args[arity++]
                int i = w->arity++;
                TB_Node* ptr = tb_inst_member_access(f, args, i * sizeof(uint64_t));
                stack[head++] = tb_inst_load(f, TB_TYPE_I64, ptr, _Alignof(size_t), false);
            }

            head -= arity;
            TB_Node** args = &stack[head];

            TB_Node* new_head = NULL;
            switch (x) {
                // arithmatic
                case OP_ADD: stack[head++] = tb_inst_add(f, args[0], args[1], 0); break;

                // stack
                case OP_DUP: stack[head] = args[0], stack[head+1] = args[0], head += 2; break;

                // console
                case OP_EMIT: {
                    if (putchar_n == NULL) {
                        putchar_n = tb_inst_get_symbol_address(f, putchar_sym);
                    }

                    TB_Node* t = tb_inst_trunc(f, args[0], TB_TYPE_I32);
                    tb_inst_call(f, putchar_proto, putchar_n, 1, &t);
                    break;
                }

                // control flow
                case OP_IF: {
                    Control* c = control_push(f, &cs, i, head);
                    tb_inst_if(f, args[0], c->on, c->off);
                    tb_inst_set_control(f, c->on);
                    break;
                }
                case OP_ELSE: {
                    Control* c = control_peek(&cs);
                    assert(c && "missing if{");

                    // mark the "out_count" known at this point,
                    // if the else case doesn't match we'll cry
                    c->out_count = head - c->arity;
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
                    Control* c = control_pop(&cs);
                    assert(head - c->arity == c->out_count && "stinky dynamic effects");

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

    assert(cs.head == 0 && "you're missing closing braces?");

    // writeback new head
    int delta = head - w->arity;
    if (delta != 0) {
        TB_Node* env = tb_inst_param(f, 0);
        TB_Node* head_ptr = tb_inst_member_access(f, env, offsetof(Env, head));

        // write out new head
        TB_Node* ld_head = tb_inst_load(f, TB_TYPE_I64, head_ptr, _Alignof(size_t), false);
        TB_Node* add_head = NULL;
        if (delta < 0) {
            add_head = tb_inst_sub(f, ld_head, tb_inst_sint(f, TB_TYPE_I64, -delta), 0);
        } else {
            add_head = tb_inst_add(f, ld_head, tb_inst_sint(f, TB_TYPE_I64, delta), 0);
        }
        tb_inst_store(f, TB_TYPE_I64, head_ptr, add_head, _Alignof(size_t), false);
    }

    // dump stack (args is the top of the stack)
    for (size_t i = 0; i < head; i++) {
        TB_Node* ptr = tb_inst_member_access(f, args, i * sizeof(uint64_t));
        tb_inst_store(f, TB_TYPE_I64, ptr, stack[i], _Alignof(size_t), false);
    }

    TB_Node* ret = tb_inst_uint(f, TB_TYPE_I32, INTERP_OK);
    tb_inst_ret(f, 1, &ret);

    tb_function_print(f, tb_default_print_callback, stdout);

    // compile & apply
    tb_module_compile_function(ir_module, f, TB_ISEL_FAST);

    w->jit = tb_module_apply_function(jit, f);
    tb_module_ready_jit(jit);
}

int main(int argc, const char** argv) {
    cuik_init_terminal();

    TB_FeatureSet features = { 0 };
    ir_module = tb_module_create_for_host(&features, true);
    jit = tb_module_begin_jit(ir_module, 0);

    // all JIT regions use the same prototype
    TB_PrototypeParam params[] = { { TB_TYPE_PTR }, { TB_TYPE_PTR } };
    TB_PrototypeParam ret = { TB_TYPE_I32 };
    proto = tb_prototype_create(ir_module, TB_STDCALL, 2, params, 1, &ret, false);

    {
        TB_PrototypeParam params[] = { { TB_TYPE_I32 } };
        TB_PrototypeParam ret = { TB_TYPE_I32 };
        putchar_proto = tb_prototype_create(ir_module, TB_STDCALL, 1, params, 1, &ret, false);

        putchar_sym = (TB_Symbol*) tb_extern_create(ir_module, "putchar", TB_EXTERNAL_SO_EXPORT);
        tb_symbol_bind_ptr(putchar_sym, &putchar);
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
    nl_map_put_cstr(dict, "if{",    OP_IF);
    nl_map_put_cstr(dict, "}else{", OP_ELSE);
    nl_map_put_cstr(dict, "loop{",  OP_LOOP);
    nl_map_put_cstr(dict, "}",      OP_CLOSE);

    Env env;
    env.head = 0;
    env.single_step = false;
    env.control_head = 1;
    env.control[0] = 0;

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
    tb_module_destroy(ir_module);
    return 0;
}
