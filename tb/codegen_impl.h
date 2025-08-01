// See codegen.h for more details, this is the implementation file for it, each target
// will include this to define their own copy of the codegen.
#include "codegen.h"

#define COMMENT(...) (e->has_comments ? tb_emit_comment(e, &ctx->f->tmp_arena, __VA_ARGS__) : (void)0)

// ASM-style settings
#define ASM_STYLE_PRINT_POS 0
#define ASM_STYLE_PRINT_NOP 0

static void node_add_tmps(Ctx* restrict ctx, TB_Node* n);

// RA constraints:
//   TODO
static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins);
//   TODO
static int node_constraint_kill(Ctx* restrict ctx, TB_Node* n, RegMask** kills);
//   when we represent 2addr ops in the SSA, we define which edge is potentially
//   shared such that lifetimes don't freak out about the fact that there's a
//   hypothetical move before the op (which means that it'll interfere with its
//   own inputs).
//
//   return -1 if it's not a 2addr and anything else to represent which edge
//   doesn't alias the dst.
static int node_2addr(TB_Node* n);
//   returns true if it's possible to rematerialize rather than spill this node.
static bool node_remat(TB_Node* n);

// Code emit:
//   finally write bytes, this is done post-RA so you're expected to use the VReg data
//   to know which regs you were assigned.
static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle);
//   this is where I recommend emitting prologue bytes.
static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n);
//   this is called AFTER the emitting of epilogues, those are emitted as normal
//   nodes. An example use of mine is NOP padding, idk do whatever, emitting is done.
static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e);
//   called at the start of each BB, it's mostly for bookkeeping about where labels
//   are placed.
static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb);
//   these two sequential nodes might fit into the same bundle (given they're input-independent)
static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b);

// Scheduling bits:
//   simple latency until the results of a node are useful (list scheduler will
//   generally prioritize dispatching higher latency ops first).
static int node_latency(TB_Function* f, TB_Node* n, int i);
//   on VLIWs it's important that we keep track of which functional units a specific
//   node can even run on, use the bits to represent that (at most you can make 64
//   functional units in the current design but i don't think i need more than 10 rn)
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n);

static void init_ctx(Ctx* restrict ctx, TB_ABI abi);

// Dissassembly:
static bool sym_handler(TB_Disasm* disasm, int inst_length, uint64_t field, int field_pos, int field_len, bool is_offset);
static void dump_stack_layout(Ctx* restrict ctx, TB_CGEmitter* e);

// just a pretty asm-like printer
static void print_pretty(Ctx* restrict ctx, TB_Node* n);

TB_OPTDEBUG(STATS)(int stats_miss, stats_hit);

typedef struct {
    TB_Node* n;
    uint32_t ip;
} NodeIPPair;

typedef struct {
    TB_Node* n;
    int index;
} NodeCursor;

// Helpers
static void dump_sched(Ctx* restrict ctx, OutStream* s) {
    if (s == NULL) {
        s = &OUT_STREAM_DEFAULT;
    }

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        s_writef(s, "BB %zu:\n", i);
        aarray_for(j, bb->items) {
            s_writef(s, "  ");
            tb_print_dumb_node_raw(NULL, bb->items[j], s);
            s_writef(s, "\n");
        }
    }
}

static void dump_pretty_sched(Ctx* restrict ctx) {
    printf("=== DUMP %s ===\n", ctx->f->super.name);
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf(".BB%zu:\n", i);
        aarray_for(j, bb->items) {
            print_pretty(ctx, bb->items[j]);
            printf("\n");
        }
    }
    printf("\n");
}

/*static void dbg_submit_sched_event(Ctx* restrict ctx, const char* desc, ...) {
    #if TB_OPTDEBUG_SERVER
    if (dbg_server == NULL) {
        return;
    }

    // if we're acting as a debug server, submit the latest copy of the
    // IR to the list of events. The viewer will organize the timeline
    // on it's end
    int t = f->dbg_server_t++;

    BufferOutStream s = bos_make();
    s.header.quoted = true;
    s_writef(&s.header, "{ \"type\":\"OPT\", \"name\":\"%s\", \"time\":%d, \"desc\":\"", f->super.name, t);

    va_list ap;
    va_start(ap, desc);
    s.header.writef(&s.header, desc, ap);
    va_end(ap);

    s_writef(&s.header, "\", \"content\":\"");
    tb_print_to_stream(f, &s.header);
    s_writef(&s.header, "\" }");

    // printf("%.*s\n", (int) s.cnt, s.data);

    write_bytes(dbg_client, s.data, s.cnt);
    sb_poll_server(dbg_server, 0);
    cuik_free(s.data);
    #endif
}*/

static void flush_bundle(Ctx* restrict ctx, TB_CGEmitter* restrict e, Bundle* b) {
    #if TB_OPTDEBUG_EMIT
    FOR_N(i, 0, b->count) {
        if (i) {
            printf("\n");
        }

        TB_Node* n = b->arr[i];
        print_pretty(ctx, n);
    }

    #if BUNDLE_INST_MAX > 1
    printf(" ;;\n");
    #else
    printf("\n");
    #endif
    #endif

    bundle_emit(ctx, e, b);
    b->count = 0;
}

static TB_Node* node_add_tmp(Ctx* restrict ctx, TB_Node* n, RegMask* mask) {
    TB_Function* f = ctx->f;
    TB_Node* tmp = tb_alloc_node(f, TB_MACH_TEMP, TB_TYPE_VOID, 1, sizeof(TB_NodeMachTemp));
    set_input(f, tmp, f->root_node, 0);
    add_input_late(f, n, tmp);
    TB_NODE_SET_EXTRA(tmp, TB_NodeMachTemp, .def = mask);
    return tmp;
}

static int try_create_vreg(Ctx* restrict ctx, TB_Node* n, RegMask* def_mask) {
    int vreg_id = ctx->vreg_map[n->gvn];
    if (vreg_id > 0) {
        TB_ASSERT(def_mask != &TB_REG_EMPTY);
        ctx->vregs[vreg_id] = (VReg){ .n = n, .mask = def_mask, .assigned = -1, .spill_cost = NAN, .uses = 1 };
        ctx->vregs[vreg_id].reg_width = tb__reg_width_from_dt(def_mask->class, n->dt);

        if (def_mask->class == REG_CLASS_STK) {
            ctx->vregs[vreg_id].spill_cost = INFINITY;

            int fixed = fixed_reg_mask(def_mask);
            if (fixed >= 0) {
                ctx->vregs[vreg_id].class = REG_CLASS_STK;
                ctx->vregs[vreg_id].assigned = fixed;
            }
        }
    }
    return vreg_id;
}

static void construct_prologue_epilogue(Ctx* restrict ctx, TB_Function* f) {
    TB_Node* n = f->root_node;

    // convert params into machine projections
    int used[8];
    FOR_N(i, 0, ctx->num_classes) {
        used[i] = 0;
    }
    used[REG_CLASS_STK] = 1;

    // find all projections
    TB_Node** projs = tb_arena_alloc(&f->tmp_arena, (1 + ctx->param_count) * sizeof(TB_Node*));
    FOR_USERS(u, n) if (is_proj(USERN(u))) {
        TB_Node* un = USERN(u);
        int index = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
        if (index >= 2) {
            projs[index - 2] = un;
        }
    }

    // replace the return address projection
    CallingConv* cc = ctx->calling_conv;
    {
        RegMask* rm = intern_regmask2(ctx, cc->rpc_class, false, cc->rpc_reg);
        TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, projs[0]->dt, 1, sizeof(TB_NodeMachProj));
        TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = 2, .def = rm);
        subsume_node(f, projs[0], proj);
        set_input(f, proj, n, 0);
    }

    // pointer math around stack slots will refer to this
    ctx->frame_ptr = tb_alloc_node(f, TB_MACH_FRAME_PTR, TB_TYPE_PTR, 1, 0);
    set_input(f, ctx->frame_ptr, f->root_node, 0);
    ctx->frame_ptr = tb_opt_gvn_node(f, ctx->frame_ptr);

    // HACK(NeGate): this code kinda just assumes that float regs go into Class2 (int regs into Class1)
    FOR_N(i, 0, ctx->param_count) {
        // on win64 we always have the XMMs and GPRs used match the param_num
        // so if XMM2 is used, it's always the 3rd parameter.
        if (!cc->flexible_param_alloc) {
            used[1] = used[2] = i;
            used[REG_CLASS_STK] = 1 + ctx->param_count + i;
        }

        TB_Node* in = projs[1 + i];
        int reg_class = TB_IS_FLOAT_TYPE(in->dt) ? 2 : 1;
        int reg_num = used[reg_class];

        RegMask* rm = NULL;
        if (reg_num >= cc->param_count[reg_class]) {
            rm = intern_regmask2(ctx, REG_CLASS_STK, false, used[REG_CLASS_STK]);
            used[REG_CLASS_STK] += 1;
        } else {
            TB_ASSERT(TB_IS_INT_OR_PTR(in->dt) || TB_IS_FLOAT_TYPE(in->dt));
            rm = intern_regmask2(ctx, reg_class, false, cc->params[reg_class][reg_num]);
            used[reg_class] += 1;
        }

        TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, projs[1 + i]->dt, 1, sizeof(TB_NodeMachProj));
        TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = 3 + i, .def = rm);
        subsume_node(f, projs[1 + i], proj);
        set_input(f, proj, n, 0);
    }
    tb_arena_free(&f->tmp_arena, projs, ctx->param_count * sizeof(TB_Node*));

    TB_Node* ret = n->inputs[1];
    if (ret->type == TB_RETURN) {
        add_input_late(f, ret, ctx->frame_ptr);

        // insert callee saves
        int proj_count = 3 + f->prototype->param_count;

        CallingConv* cc = ctx->calling_conv;
        bool use_frame_ptr = ctx->f->features.gen & TB_FEATURE_FRAME_PTR;
        FOR_N(i, 1, ctx->num_classes) {
            const char* saves = cc->reg_saves[i];
            if (saves == NULL) { continue; }

            // HACK(NeGate): we really should be describing the "reg class" => "ideal data type"
            // conversion somewhere, rather than hard coding in "XMM is floats"
            TB_DataType dt = TB_TYPE_I64;
            if (i == 2) {
                dt.type = TB_TAG_V128;
                dt.elem_or_addrspace = TB_TAG_F32;
            }

            FOR_N(j, 0, ctx->num_regs[i]) {
                if (saves[j] == 'c' &&
                    // if we're using the frame ptr, it should be treated as "no save"
                    (!use_frame_ptr || cc->fp_class != i || cc->fp_reg != j)
                ) {
                    RegMask* rm = intern_regmask(ctx, i, false, 1ull << j);
                    TB_Node* proj = tb_alloc_node(f, TB_MACH_PROJ, dt, 1, sizeof(TB_NodeMachProj));
                    TB_NODE_SET_EXTRA(proj, TB_NodeMachProj, .index = proj_count++, .def = rm);

                    set_input(f, proj, n, 0);
                    add_input_late(f, ret, proj);
                }
            }
        }
    }
}

// Autogenerated by DSL
static uint32_t mach_grammar_exp;
static uint32_t* mach_grammar;

static bool mach_is_operand[512];
static bool mach_is_subpat[512];
static TB_Node* mach_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state);
static TB_Node* mach_dfa_bare_memory(Ctx* ctx, TB_Function* f, TB_Node* n);

static void node_grammar_alloc(size_t cap) {
    cap = (cap * 4) / 3;
    if (cap < 4) cap = 4;

    // next power of two
    #if defined(_MSC_VER) && !defined(__clang__)
    size_t exp = 64 - _lzcnt_u64(cap - 1);
    #else
    size_t exp = 64 - __builtin_clzll(cap - 1);
    #endif

    cap = (cap == 1 ? 1 : 1ull << exp);

    mach_grammar_exp = exp;
    mach_grammar = calloc(2 * cap, sizeof(uint32_t));
}

static void node_grammar_put(uint32_t k, uint32_t v) {
    // TODO(NeGate): real hash
    uint32_t i = (k * 11400714819323198485llu) >> (64 - mach_grammar_exp);
    uint32_t first = i;
    uint32_t mask = (1u << mach_grammar_exp) - 1;
    do {
        if (mach_grammar[2*i + 0] == 0) {
            mach_grammar[2*i + 0] = k;
            mach_grammar[2*i + 1] = v;
            return;
        } else if (mach_grammar[2*i] == k) {
            tb_panic("wtf? why are you overriding entries?");
        }
        i = (i + 1) & mask;
    } while (i != first);

    tb_panic("wtf?");
}

static uint32_t node_grammar_get(uint32_t k) {
    // TODO(NeGate): real hash
    uint32_t i = (k * 11400714819323198485llu) >> (64 - mach_grammar_exp);
    uint32_t first = i;
    uint32_t mask = (1u << mach_grammar_exp) - 1;
    do {
        if (mach_grammar[2*i + 0] == k) {
            return mach_grammar[2*i + 1];
        } else if (mach_grammar[2*i + 0] == 0) {
            return 0;
        }
        i = (i + 1) & mask;
    } while (i != first);

    return 0;
}

static void indent(int depth) {
    FOR_N(i, 0, depth) {
        printf("  ");
    }
}

// root is at depth=1
static TB_Node* node_isel_raw(Ctx* restrict ctx, TB_Function* f, TB_Node* n, TB_Worklist* walker_ws, int depth) {
    TB_ASSERT(n->type != TB_PHI);
    if (depth == 1 && mach_is_subpat[n->type]) {
        return NULL;
    }

    NodeCursor stk[16];
    int head = 1, state = 0;
    stk[0] = (NodeCursor){ 0 };

    TB_OPTDEBUG(ISEL2)(indent(depth), printf("Matching %%%u (%d uses)...\n", n->gvn, n->user_count));

    TB_Node* curr = NULL;
    int index = 0;
    do {
        // fetch current edge
        TB_Node* in = curr ? (index < curr->input_count ? curr->inputs[index] : NULL) : n;
        TB_NodeTypeEnum in_type = in ? in->type : TB_NULL;
        if (in) {
            TB_OPTDEBUG(ISEL2)(indent(depth), printf("  step(%%%-3u: %-16s, %3d): ", in->gvn, tb_node_get_name(in_type), state));

            // either matches against COND or MEMORY
            int wants_operand = 0;
            if (node_grammar_get(state << 16 | (256+1)) > 0) {
                wants_operand = 1; // MEMORY
            } else if (node_grammar_get(state << 16 | (257+1)) > 0) {
                wants_operand = 2; // COND
            }

            if (in->type != n->type && wants_operand) {
                if (mach_is_operand[in->type]) {
                    bool pushed_already = worklist_test_n_set(walker_ws, in);

                    TB_OPTDEBUG(ISEL2)(printf("\n"));
                    TB_Node* new_in = node_isel_raw(ctx, f, in, walker_ws, depth + 1);
                    if (new_in && new_in != in) {
                        // we can GVN machine nodes :)
                        if (!mach_is_subpat[in->type]) {
                            subsume_node(f, in, new_in);
                        } else {
                            // tb__gvn_remove(f, curr);
                            set_input(f, curr, new_in, index);
                        }

                        // don't walk the replacement
                        worklist_push(walker_ws, new_in);

                        in = new_in;
                        in_type = in ? in->type : TB_NULL;
                    } else if (!pushed_already) {
                        // node failed but hasn't been processed, treat it as a node of its own
                        dyn_array_put(walker_ws->items, in);
                    }

                    #if TB_OPTDEBUG_ISEL2
                    indent(depth);
                    printf("    => ");
                    tb_print_dumb_node(NULL, in);
                    printf("\n");
                    #endif
                } else {
                    uint32_t next = node_grammar_get(state << 16 | (in_type+1));
                    if (next == 0) { next = node_grammar_get(state<<16); }

                    // if there's no non-memory match, we'll take it as a last resort
                    if (wants_operand == 1 && next == 0) {
                        TB_Node* new_in = mach_dfa_bare_memory(ctx, f, in);
                        set_input(f, curr, new_in, index);

                        in = new_in;
                        in_type = in ? in->type : TB_NULL;
                        TB_ASSERT(in_type == 256);

                        #if TB_OPTDEBUG_ISEL2
                        printf("\n    %%%u treated as MEMORY node\n    ", in->gvn);
                        tb_print_dumb_node(NULL, new_in);
                        printf("\n");
                        #endif

                        TB_OPTDEBUG(ISEL2)(indent(depth), printf("  step(%%%-3u: %-16s, %3d): ", in->gvn, tb_node_get_name(in_type), state));
                    }
                }
            }
        } else {
            TB_OPTDEBUG(ISEL2)(indent(depth), printf("  step(      %-16s, %3d): ", "___", state));
        }
        uint32_t next = node_grammar_get(state<<16 | (in_type+1));
        // if there's no specific match, try general
        if (next == 0) { next = node_grammar_get(state<<16); }
        // if there's no match, try to resolve the input then go
        TB_OPTDEBUG(ISEL2)(indent(depth));
        if (next == 0) {
            TB_OPTDEBUG(ISEL2)(printf("failed!\n"));
            return NULL;
        }
        // advance
        index += 1;
        if ((next >> 16u) == 0) {
            TB_OPTDEBUG(ISEL2)(printf("next\n"));
        } else if ((next >> 16u) == 1) {
            stk[head - 1].index = index, stk[head++] = (NodeCursor){ in, 0 }, curr = in, index = 0;

            #if TB_OPTDEBUG_ISEL2
            printf("push ");
            tb_print_dumb_node(NULL, curr);
            printf("\n");
            #endif
        } else {
            head -= (next >> 16u) - 1;
            index = stk[head - 1].index, curr = stk[head - 1].n;

            #if TB_OPTDEBUG_ISEL2
            printf("pop  ");
            if (curr) {
                tb_print_dumb_node(NULL, curr);
            }
            printf("\n");
            #endif
        }
        state = next & 0xFFFF;
    } while (head > 1);

    // we've found a match, jump to the relevant C match
    size_t old_node_count = f->node_count;
    TB_Node* k = mach_dfa_accept(ctx, f, n, state);

    if (k && k->gvn >= old_node_count) {
        // we can GVN machine nodes :)
        k = tb_opt_gvn_node(f, k);
    }
    return k;
}

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void disassemble(TB_Arch arch, TB_CGEmitter* e, Disasm* restrict d, int bb, size_t start, size_t end) {
    char str[500];
    TB_Disasm disasm = {
        .ctx = d,
        // input
        .in = e->data, .in_curr = start, .in_len = end,
        // output
        .out = str, .out_len = sizeof(str),
        // symbols
        .symbol_handler = sym_handler,
    };

    while (disasm.in_curr < disasm.in_len) {
        disasm.out_curr = 0;

        size_t pos = disasm.in_curr;
        while (d->loc != d->end && d->loc->pos == pos) {
            TB_OPTDEBUG(ANSI)(E("\x1b[32m"));
            E("// %s : line %d\n", d->loc->file->path, d->loc->line);
            TB_OPTDEBUG(ANSI)(E("\x1b[0m"));
            d->loc++;
        }

        if (tb_disasm_print(arch, &disasm, true) < 0) {
            E("ERROR %#02x\n", disasm.in[disasm.in_curr++]);
            continue;
        }

        uint64_t line_start = e->total_asm;
        #if ASM_STYLE_PRINT_POS
        E("%-4x  %.*s", pos, (int) disasm.out_curr, disasm.out);
        #else
        E("  %.*s", (int) disasm.out_curr, disasm.out);
        #endif

        int offset = e->total_asm - line_start;
        if (d->comment && d->comment->pos == pos) {
            TB_OPTDEBUG(ANSI)(E("\x1b[32m"));
            E("%*s// ", 50 - offset, "");
            bool out_of_line = false;
            do {
                if (out_of_line) {
                    // tack on a newline
                    E("%*s// ", 50, "");
                }

                E("%.*s\n", d->comment->line_len, d->comment->line);
                d->comment = d->comment->next;
                out_of_line = true;
            } while (d->comment && d->comment->pos == pos);
            TB_OPTDEBUG(ANSI)(E("\x1b[0m"));
        } else {
            E("\n");
        }
    }
}
#undef E

static void log_phase_end(TB_Function* f, size_t og_size, const char* label) {
    log_debug("%s: tmp_arena=%.1f KiB, ir_arena=%.1f KiB (post %s)", f->super.name, tb_arena_current_size(&f->tmp_arena) / 1024.0f, (tb_arena_current_size(&f->arena) - og_size) / 1024.0f, label);
}

static bool is_vreg_match(Ctx* ctx, TB_Node* a, TB_Node* b) {
    VReg* aa = &ctx->vregs[ctx->vreg_map[a->gvn]];
    VReg* bb = &ctx->vregs[ctx->vreg_map[b->gvn]];
    if (aa == bb) {
        return true;
    }

    return aa->class == bb->class && aa->assigned == bb->assigned;
}

static void compile_function(TB_Function* restrict f, TB_CodegenRA ra, TB_FunctionOutput* restrict func_out, TB_Arena* code_arena, bool emit_asm) {
    #if TB_OPTDEBUG_ISEL
    tb_print_dumb(f);
    #endif

    if (0) {
        static float dst[256];
        uint64_t args[] = { (uintptr_t) &dst, 0, 0 };
        uint64_t ret = tb_interpret(f, f->worklist, args);

        // printf("AAA = %"PRId64"\n", ret);
        printf("Dst = \n");
        FOR_N(j, 0, 16) {
            FOR_N(i, 0, 16) {
                printf("%.3f ", dst[j*16 + i]);
            }
            printf("\n");
        }

        __debugbreak();
    }

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    #ifndef NDEBUG
    f->arena.allocs = f->arena.alloc_bytes = 0;
    f->tmp_arena.allocs = f->tmp_arena.alloc_bytes = 0;
    #endif

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .constraint  = node_constraint,
        .constraint_kill = node_constraint_kill,
        .node_2addr  = node_2addr,
        .remat       = node_remat,
        .print_pretty= print_pretty,
        .num_classes = REG_CLASS_COUNT,
        .features = f->features,
        .emit = {
            .output = func_out,
            .arena = code_arena,
            .has_comments = true,
        }
    };

    init_ctx(&ctx, f->super.module->target_abi);
    TB_Worklist* restrict ws = f->worklist;

    size_t og_size = tb_arena_current_size(&f->arena);

    ctx.mask_intern = nl_hashset_alloc(200);
    nl_hashset_put2(&ctx.mask_intern, &TB_REG_EMPTY, rm_hash, rm_compare);

    FOR_N(i, 1, REG_CLASS_COUNT) {
        nl_hashset_put2(&ctx.mask_intern, ctx.normie_mask[i], rm_hash, rm_compare);
        if (ctx.mayspill_mask[i]) {
            nl_hashset_put2(&ctx.mask_intern, ctx.mayspill_mask[i], rm_hash, rm_compare);
        }
    }

    CUIK_TIMED_BLOCK("isel") {
        STATS_ENTER(MACH_ISEL);
        log_debug("%s: tmp_arena=%.1f KiB (pre-isel)", f->super.name, tb_arena_current_size(&f->tmp_arena) / 1024.0f);

        // rewrite the root node
        construct_prologue_epilogue(&ctx, f);

        TB_ArenaSavepoint pins_sp = tb_arena_save(&f->tmp_arena);
        ArenaArray(TB_Node*) pins = aarray_create(&f->tmp_arena, TB_Node*, (f->node_count / 32) + 16);

        TB_Worklist walker_ws = { 0 };
        worklist_alloc(&walker_ws, f->node_count);

        ctx.walker_ws = &walker_ws;

        // find all nodes
        worklist_clear(ws);
        worklist_push(ws, f->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            if (tb_node_is_pinned(n) && !is_proj(n)) {
                aarray_push(pins, n);
            }

            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }

        // dead node elim
        CUIK_TIMED_BLOCK("dead node elim") {
            for (TB_Node* n; n = worklist_pop(ws), n;) {
                if (n->user_count == 0 && !is_proj(n) && n != ctx.frame_ptr) {
                    TB_OPTDEBUG(ISEL3)(printf("  ISEL t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n), printf(" => \x1b[31mKILL\x1b[0m\n"));
                    tb_kill_node(f, n);
                }
            }
        }

        // greedy instruction selector does bottom-up rewrites from the pinned nodes
        aarray_for(i, pins) {
            TB_Node* pin_n = pins[i];

            TB_OPTDEBUG(ISEL3)(printf("PIN    t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, pin_n), printf("\n"));
            worklist_push(&walker_ws, pin_n);

            while (dyn_array_length(walker_ws.items) > 0) {
                TB_Node* n = dyn_array_pop(walker_ws.items);
                TB_OPTDEBUG(ISEL3)(printf("ISEL t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

                if (!is_proj(n) && n->user_count == 0) {
                    TB_OPTDEBUG(ISEL3)(printf(" => \x1b[31mKILL\x1b[0m\n"));
                    worklist_push(ws, n);
                    continue;
                }

                // memory out nodes are *notorious* for having unused loads, we wanna prune these
                if (n->dt.type == TB_TAG_MEMORY) {
                    FOR_USERS(u, n) {
                        if (USERN(u)->user_count == 0) {
                            worklist_push(ws, USERN(u));
                        }
                    }
                }

                // replace with machine op
                TB_OPTDEBUG(ISEL3)(printf("\n"));
                if (n->type == TB_PHI) {
                    worklist_test_n_set(&walker_ws, n);
                } else {
                    tb__gvn_remove(f, n);

                    bool progress;
                    do {
                        TB_Node* k = node_isel_raw(&ctx, f, n, &walker_ws, 1);
                        progress = k != NULL;

                        if (k) {
                            TB_OPTDEBUG(ISEL3)(printf("  => \x1b[32m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m\n"));

                            if (k != n) {
                                subsume_node(f, n, k);
                            }

                            // don't walk the replacement
                            worklist_test_n_set(&walker_ws, k);
                            n = k;
                        }
                    } while (progress);
                }
                TB_OPTDEBUG(ISEL2)(printf("\n"));

                node_add_tmps(&ctx, n);

                FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
                    worklist_push(&walker_ws, n->inputs[i]);
                }
            }
        }
        worklist_free(&walker_ws);

        if (ctx.frame_ptr->user_count == 0) {
            tb_kill_node(f, ctx.frame_ptr);
            ctx.frame_ptr = NULL;
        }

        // dead node elim
        CUIK_TIMED_BLOCK("dead node elim") {
            for (TB_Node* n; n = worklist_pop(ws), n;) {
                if (n->user_count == 0 && !is_proj(n)) {
                    TB_OPTDEBUG(ISEL3)(printf("  ISEL t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n), printf(" => \x1b[31mKILL\x1b[0m\n"));
                    tb_kill_node(f, n);
                }
            }
        }

        tb_arena_restore(&f->tmp_arena, pins_sp);
        log_phase_end(f, og_size, "isel");

        TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "ISel"));
        STATS_EXIT(MACH_ISEL);
    }

    TB_CFG cfg;
    CUIK_TIMED_BLOCK("global sched") {
        STATS_ENTER(MACH_GCM);
        // we're gonna build a bunch of compact tables... they're only
        // compact if we didn't spend like 40% of our value numbers on dead shit.
        #if !TB_OPTDEBUG_ISEL && !TB_OPTDEBUG_ISEL2 && !TB_OPTDEBUG_ISEL3
        tb_renumber_nodes(f, ws);
        #endif

        TB_OPTDEBUG(ISEL)(tb_print_dumb(f));

        ctx.cfg = cfg = tb_compute_cfg(f, ws, &f->tmp_arena, true);
        tb_compute_synthetic_loop_freq(f, &cfg);
        tb_global_schedule(f, ws, cfg, false, node_latency);

        // live ins & outs will outlive this function so we wanna alloc before the savepoint
        aarray_for(i, cfg.blocks) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            bb->live_in  = set_create_in_arena(&f->tmp_arena, f->node_count);
            bb->live_out = set_create_in_arena(&f->tmp_arena, f->node_count);
        }
        tb_dataflow(f, &f->tmp_arena, cfg);
        log_phase_end(f, og_size, "GCM");
        STATS_EXIT(MACH_GCM);
    }

    size_t bb_count = aarray_length(cfg.blocks);
    Set has_safepoints = set_create_in_arena(&f->arena, f->node_count);

    size_t node_count = f->node_count;
    size_t vreg_cap = 0;
    CUIK_TIMED_BLOCK("local schedule") {
        STATS_ENTER(MACH_LCM);
        ctx.vreg_map = aarray_create(&f->arena, int, tb_next_pow2(f->node_count + 16));
        aarray_set_length(ctx.vreg_map, f->node_count);

        FOR_N(i, 0, f->node_count) {
            ctx.vreg_map[i] = 0;
        }

        TB_OPTDEBUG(SCHED2)(printf("=== SCHED %s ===\n", ctx.f->super.name));

        int max_ins = 0;
        size_t vreg_count = 1; // 0 is reserved as the NULL vreg
        TB_ASSERT(dyn_array_length(ws->items) == 0);
        FOR_N(i, 0, bb_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            // compute local schedule, this schedule doesn't care about the exact pipeline details
            CUIK_TIMED_BLOCK("local sched") {
                tb_list_scheduler(f, &cfg, ws, bb, node_latency);
            }

            // a bit of slack for spills
            size_t item_count = dyn_array_length(ws->items);
            ArenaArray(TB_Node*) items = aarray_create(&f->arena, TB_Node*, item_count + 16);

            TB_OPTDEBUG(SCHED2)(printf(".bb%zu:\n", i));

            // copy out sched
            for (size_t j = 0; j < item_count; j++) {
                TB_Node* n = ws->items[j];

                // projections cannot emit code
                #if TB_OPTDEBUG_SCHED2
                if (!cfg_is_region(n)) {
                    print_pretty(&ctx, n);
                    printf("\n");
                }
                #endif

                if (n->type == TB_SAFEPOINT) {
                    set_put(&has_safepoints, n->inputs[2]->gvn);
                }

                // temps are added as extras so they don't
                // increase the "input_count"
                if (n->input_count > max_ins) {
                    max_ins = n->input_count;
                }
                aarray_push(items, n);

                // these ops are guarenteed to fit since new nodes aren't being added and the
                // size was fine when we started.
                TB_ASSERT(n->gvn < aarray_length(ctx.vreg_map));

                int vreg_id = 0;
                RegMask* def_mask = node_constraint(&ctx, n, NULL);
                if (def_mask != &TB_REG_EMPTY) {
                    ctx.vreg_map[n->gvn] = vreg_count++;
                }
            }
            dyn_array_clear(ws->items);
            bb->items = items;
        }
        ctx.bb_count = bb_count;

        // needs to be big enough for the node_constraint_kill queries too
        if (max_ins < REG_CLASS_COUNT) {
            max_ins = REG_CLASS_COUNT;
        }
        ctx.ins = tb_arena_alloc(&f->tmp_arena, max_ins * sizeof(RegMask*));

        log_phase_end(f, og_size, "local-sched & ra constraints");

        // setup for the next phase
        ctx.vregs = aarray_create(&f->arena, VReg, tb_next_pow2(vreg_count + 16));
        aarray_set_length(ctx.vregs, vreg_count);
        STATS_EXIT(MACH_LCM);
    }

    CUIK_TIMED_BLOCK("gather RA constraints") {
        #if TB_OPTDEBUG_REGALLOC
        printf("====== PRE-RA %-20s ======\n", ctx.f->super.name);
        #endif

        FOR_N(i, 0, bb_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            #if TB_OPTDEBUG_REGALLOC
            printf("BB %zu (freq=%.4f)\n", i, bb->freq);
            #endif

            aarray_for(j, bb->items) {
                TB_Node* n = bb->items[j];

                #if TB_OPTDEBUG_REGALLOC
                printf("  "), tb_print_dumb_node(NULL, n), printf("\n");
                #endif

                RegMask* def_mask = node_constraint(&ctx, n, ctx.ins);
                int vreg_id = try_create_vreg(&ctx, n, def_mask);

                #if TB_OPTDEBUG_REGALLOC
                if (vreg_id > 0) {
                    printf("    OUT    = "), tb__print_regmask(def_mask), printf(" \x1b[32m# VREG=%d\x1b[0m\n", vreg_id);
                }

                FOR_N(k, 1, n->input_count) {
                    if (n->inputs[k]) {
                        if (n->inputs[k]->type == TB_MACH_TEMP) {
                            printf("    TMP[%zu] = ", k), tb__print_regmask(ctx.ins[k]), printf(" %%%d\n", n->inputs[k]->gvn);
                        } else if (ctx.ins[k] != &TB_REG_EMPTY) {
                            printf("    IN[%zu]  = ", k), tb__print_regmask(ctx.ins[k]), printf(" %%%d\n", n->inputs[k]->gvn);
                        }
                    }
                }

                int kill_count = node_constraint_kill(&ctx, n, ctx.ins);
                FOR_N(k, 0, kill_count) {
                    printf("    KILL[%zu] = ", k), tb__print_regmask(ctx.ins[k]), printf("\n");
                }
                #endif
            }
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("=======================================\n");
        #endif
    }

    CUIK_TIMED_BLOCK("regalloc") {
        STATS_ENTER(MACH_RA);
        switch (ra) {
            case TB_RA_ROGERS: tb__rogers(&ctx, &f->tmp_arena); break;
            case TB_RA_BRIGGS: tb__briggs(&ctx, &f->tmp_arena); break;
        }

        worklist_clear(ws);
        nl_hashset_free(ctx.mask_intern);
        STATS_EXIT(MACH_RA);

        log_phase_end(f, og_size, "RA");
    }

    int* final_order = ctx.emit.final_order = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(int));
    size_t final_order_count = 0;

    CUIK_TIMED_BLOCK("BB scheduling") {
        STATS_ENTER(MACH_BB_SCHED);
        if (bb_count == 1) {
            // none of this shit is necessary for single BB functions
            final_order_count = 1;
            final_order[0] = 0;
        } else {
            // any empty projection blocks will mark which block they want you to jump to, if the
            // block "forwards" to itself then it's not actually forwarding.
            FOR_N(i, 0, bb_count) {
                TB_BasicBlock* bb = &cfg.blocks[i];
                bool empty = true;

                size_t item_count = aarray_length(bb->items);
                if (cfg_is_region(bb->items[0])) {
                    // if it's all phis then we've got an empty enough block for forwarding
                    FOR_N(j, 1, item_count) {
                        if (bb->items[j]->type != TB_PHI) {
                            empty = false;
                            break;
                        }
                    }
                } else if (item_count > 1 || !cfg_is_cproj(bb->start)) {
                    // if there's just empty copies we can consider it empty
                    FOR_N(j, 0, item_count) {
                        TB_Node* n = bb->items[j];
                        if (bb->start == n) {
                            continue;
                        } else if (n->type == TB_MACH_COPY) {
                            // if both dst & src match, it's not gonna emit anything
                            if (is_vreg_match(&ctx, n, n->inputs[1])) {
                                continue;
                            }
                        }

                        empty = false;
                        break;
                    }
                }

                if (empty) {
                    TB_Node* next = cfg_next_control(bb->start);
                    TB_BasicBlock* succ = nl_map_get_checked(cfg.node_to_block, next);
                    bb->fwd = succ - cfg.blocks;
                } else {
                    bb->fwd = i;
                }
            }

            // sometimes we forward twice (wacky cases around "empty" regions)
            FOR_N(i, 0, bb_count) {
                TB_BasicBlock* bb = &cfg.blocks[i];

                int fwd = bb->fwd;
                while (fwd != cfg.blocks[fwd].fwd) {
                    fwd = cfg.blocks[fwd].fwd;
                }

                bb->fwd = fwd;
            }

            // final_order_count = bb_placement_rpo(&f->tmp_arena, &cfg, final_order);
            final_order_count = bb_placement_trace(&f->tmp_arena, &cfg, final_order);

            // insert fallthrus
            FOR_N(i, 0, final_order_count) {
                TB_BasicBlock* bb = &cfg.blocks[final_order[i]];

                int fallthru = -1;
                if (i + 1 < final_order_count) {
                    fallthru = final_order[i + 1];
                }

                // insert jump for the fallthru case, this matters most for the platforms which can
                // bundle jumps (branch delay slots and VLIWs)
                if (!cfg_is_terminator(bb->end)) {
                    TB_Node* succ_n = cfg_next_control(bb->end);
                    TB_BasicBlock* succ_bb = nl_map_get_checked(ctx.cfg.node_to_block, succ_n);
                    if (succ_bb->fwd != fallthru) {
                        TB_Node* jmp = tb_alloc_node(f, TB_MACH_JUMP, TB_TYPE_CONTROL, 1, 0);
                        TB_User* succ = cfg_next_user(bb->end);
                        set_input(f, USERN(succ), jmp, USERI(succ));
                        set_input(f, jmp, bb->end, 0);
                        bb->end = jmp;

                        aarray_push(bb->items, jmp);
                        tb__insert(&ctx, f, bb, jmp);
                    }
                }
            }
        }

        ctx.emit.final_order_count = final_order_count;

        #if TB_OPTDEBUG_PLACEMENT
        printf("Final Schedule:\n");
        FOR_N(i, 0, final_order_count) {
            int id = final_order[i];
            TB_ASSERT(id == cfg.blocks[id].fwd);
            printf("  BB%-3d (freq=%f)\n", id, cfg.blocks[id].freq);
        }
        #endif
        STATS_EXIT(MACH_BB_SCHED);
    }

    CUIK_TIMED_BLOCK("emit") {
        STATS_ENTER(MACH_EMIT);

        // most functions are probably decently small, it's ok tho if it needs to
        // resize it can do that pretty quickly
        ctx.emit.capacity = 1024;
        ctx.emit.data = tb_arena_alloc(code_arena, ctx.emit.capacity);

        // allocate more stuff now that we've run stats on the IR
        ctx.emit.label_count = bb_count;
        ctx.emit.labels = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(uint32_t));
        memset(ctx.emit.labels, 0, bb_count * sizeof(uint32_t));

        TB_CGEmitter* e = &ctx.emit;
        pre_emit(&ctx, e, f->root_node);

        Bundle bundle;
        bundle.count = 0;
        bundle.arr = tb_arena_alloc(&f->tmp_arena, BUNDLE_INST_MAX * sizeof(Bundle));

        TB_OPTDEBUG(EMIT)(printf("====== EMIT %-20s ======\n", ctx.f->super.name));

        ArenaArray(NodeIPPair) sfpt_nodes = aarray_create(&f->tmp_arena, NodeIPPair, 16);
        FOR_N(i, 0, final_order_count) {
            int id = final_order[i];
            TB_BasicBlock* bb = &cfg.blocks[id];

            on_basic_block(&ctx, e, id);

            ctx.fallthrough = INT_MAX;
            if (i + 1 < final_order_count) {
                ctx.fallthrough = final_order[i + 1];
            }

            ctx.current_emit_bb = bb;
            ctx.current_emit_bb_pos = GET_CODE_POS(e);

            // mark label
            TB_OPTDEBUG(EMIT)(printf(".bb%d:\n", id));

            TB_Node* prev_n = NULL;
            aarray_for(i, bb->items) {
                TB_Node* n = bb->items[i];

                // projections cannot emit code
                if (is_proj(n) || cfg_is_region(n)) {
                    continue;
                }

                // TODO(NeGate): we should be checking if the bundle can support the resources we're asking for.
                // an example is that one bundle might only be able to do 2 memory operations so "fits_as_bundle"
                // would return true for two stores but trying to stretch things to 3 would force a split.
                bool legal = false;
                if (bundle.count > 0 && bundle.count < BUNDLE_INST_MAX) {
                    if (i > 1 && fits_as_bundle(&ctx, bundle.arr[bundle.count - 1], n)) {
                        legal = true;

                        // if n refers to prev then we're dependent, if not then we can't
                        // be, just because there's nothing between them that could make
                        // the connection more indirect.
                        TB_Node* prev = bundle.arr[bundle.count - 1];
                        FOR_N(i, 0, n->input_count) {
                            if (n->inputs[i] == prev) {
                                legal = false;
                                break;
                            }
                        }
                    }
                }

                // can't have two safepoints in the same bundle
                if (bundle.has_safepoint && set_get(&has_safepoints, n->gvn)) {
                    legal = false;
                }

                // flush bundle
                if (!legal && bundle.count > 0) {
                    flush_bundle(&ctx, e, &bundle);
                }

                if (set_get(&has_safepoints, n->gvn)) {
                    TB_Node* sfpt = NULL;
                    FOR_USERS(u, n) {
                        if (USERN(u)->type == TB_SAFEPOINT) {
                            sfpt = USERN(u);
                            break;
                        }
                    }
                    TB_ASSERT(sfpt != NULL);

                    NodeIPPair pair = { sfpt, GET_CODE_POS(e) };
                    aarray_push(sfpt_nodes, pair);
                }

                bundle.arr[bundle.count++] = n;
            }

            if (bundle.count > 0) {
                flush_bundle(&ctx, e, &bundle);
            }
        }
        TB_OPTDEBUG(EMIT)(printf("=======================================\n"));

        post_emit(&ctx, e);
        log_phase_end(f, og_size, "emit");

        // trim code arena
        tb_arena_free(code_arena, ctx.emit.data + ctx.emit.count, ctx.emit.capacity - ctx.emit.count);
        tb_arena_realign(code_arena);

        if (aarray_length(sfpt_nodes) > 0) {
            CUIK_TIMED_BLOCK("build safepoint table") {
                ArenaArray(TB_Safepoint*) safepoints = aarray_create(code_arena, TB_Safepoint*, aarray_length(sfpt_nodes));

                // it's built sorted by PC to make binsearching for it easy... why? i'll probably
                // care about it for random stack sample crap
                aarray_for(i, sfpt_nodes) {
                    TB_Node* n  = sfpt_nodes[i].n;
                    uint32_t ip = sfpt_nodes[i].ip;

                    TB_Node* proj1 = USERN(proj_with_index(n, 1));
                    TB_BasicBlock* succ_bb = nl_map_get_checked(ctx.cfg.node_to_block, proj1);

                    uint32_t target = ctx.emit.labels[succ_bb->fwd];
                    TB_ASSERT((target & 0x80000000) && "target label wasn't resolved... what?");

                    TB_NodeSafepoint* n_sfpt = TB_NODE_GET_EXTRA(n);
                    TB_Safepoint* sfpt = tb_arena_alloc(code_arena, sizeof(TB_Safepoint) + n_sfpt->saved_val_count*sizeof(int32_t));
                    sfpt->func = ctx.f;
                    sfpt->node = n;
                    sfpt->userdata = n_sfpt->userdata;
                    sfpt->ip = ip;
                    sfpt->target = target;
                    FOR_N(i, 0, n_sfpt->saved_val_count) {
                        TB_Node* in = n->inputs[3 + i];
                        VReg* vreg = &ctx.vregs[ctx.vreg_map[in->gvn]];
                        TB_ASSERT(vreg->assigned >= 0);

                        sfpt->values[i] = (vreg->class << 24u) | vreg->assigned;
                    }
                    aarray_push(safepoints, sfpt);
                }

                func_out->safepoints = safepoints;
            }
        }

        // fill jump table entries
        CUIK_TIMED_BLOCK("jump tables") {
            dyn_array_for(i, ctx.jump_table_patches) {
                uint32_t target = ctx.emit.labels[ctx.jump_table_patches[i].target];
                TB_ASSERT((target & 0x80000000) && "target label wasn't resolved... what?");
                *ctx.jump_table_patches[i].pos = target & ~0x80000000;
            }
        }

        STATS_EXIT(MACH_EMIT);
    }

    if (ctx.locations) {
        ctx.locations[0].pos = 0;
    }

    // TODO(NeGate): move the assembly output to code arena
    if (emit_asm) CUIK_TIMED_BLOCK("dissassembly") {
        STATS_ENTER(MACH_ASM_PRINT);

        dump_stack_layout(&ctx, &ctx.emit);
        dyn_array_for(i, ctx.debug_stack_slots) {
            TB_StackSlot* s = &ctx.debug_stack_slots[i];

            TB_OPTDEBUG(ANSI)(EMITA(&ctx.emit, "\x1b[32m"));
            EMITA(&ctx.emit, "// %s = [rsp + %d]\n", s->name, (ctx.stack_usage - ctx.stack_header) + s->storage.offset);
            TB_OPTDEBUG(ANSI)(EMITA(&ctx.emit, "\x1b[0m"));
        }
        EMITA(&ctx.emit, "%s:\n", f->super.name);

        Disasm d = {
            &ctx.emit,
            func_out->first_patch,
            ctx.locations,
            &ctx.locations[dyn_array_length(ctx.locations)],
            ctx.emit.comment_head,
            func_out->safepoints
        };

        TB_Arch arch = f->super.module->target_arch;
        if (ctx.prologue_length) {
            disassemble(arch, &ctx.emit, &d, -1, 0, ctx.prologue_length);
        }

        FOR_N(i, 0, final_order_count) {
            int id = final_order[i];

            uint32_t start = ctx.emit.labels[id] & ~0x80000000;
            uint32_t end   = ctx.emit.count;

            #if !ASM_STYLE_PRINT_NOP
            end -= ctx.nop_pads;
            #endif

            if (i + 1 < final_order_count) {
                end = ctx.emit.labels[final_order[i + 1]] & ~0x80000000;
            }

            TB_CGEmitter* e = &ctx.emit;
            {
                #if ASM_STYLE_PRINT_POS
                int len = tb_asm_print(e, "%-4x  BB%d: ", start, id);

                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[32m"));
                tb_asm_print(e, "// Freq: %.4f, (", cfg.blocks[id].freq);
                TB_Node* r = cfg.blocks[id].start;
                if (id != 0) {
                    FOR_N(i, 0, cfg_is_region(r) ? r->input_count : 1) {
                        TB_Node* pred = r->inputs[i];
                        TB_BasicBlock* pred_bb = f->scheduled[pred->gvn];

                        // if it's forwarded we should walk "before" this block
                        while (pred_bb && pred_bb->fwd != (pred_bb - cfg.blocks)) {
                            pred    = pred->inputs[0];
                            pred_bb = f->scheduled[pred->gvn];
                        }

                        if (pred_bb) {
                            tb_asm_print(e, " BB%d", pred_bb->fwd);
                        }
                    }
                }
                tb_asm_print(e, " ->");
                FOR_SUCC(it, cfg.blocks[id].end) {
                    TB_BasicBlock* succ_bb = nl_map_get_checked(cfg.node_to_block, it.succ);
                    tb_asm_print(e, " BB%d", succ_bb->fwd);
                }
                tb_asm_print(e, " )");
                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[0m"));
                tb_asm_print(e, "\n");
                #else
                int len = tb_asm_print(e, ".bb%d: ", id);
                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[32m"));
                tb_asm_print(e, "// Freq: %.4f", cfg.blocks[id].freq);
                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[0m"));
                tb_asm_print(e, "\n");
                #endif
            }
            disassemble(arch, e, &d, id, start, end);
        }

        STATS_EXIT(MACH_ASM_PRINT);
    }

    // cleanup memory
    tb_free_cfg(&cfg);

    #ifndef NDEBUG
    log_debug("%s: total allocs on ir_arena=%.1f KiB", f->super.name, f->arena.alloc_bytes / 1024.0f);
    log_debug("%s: total allocs on tmp_arena=%.1f KiB", f->super.name, f->tmp_arena.alloc_bytes / 1024.0f);
    log_debug("%s: code_arena=%.1f KiB", f->super.name, tb_arena_current_size(code_arena) / 1024.0f);
    #endif

    TB_OPTDEBUG(STATS)(printf("%f miss rate (%d misses, %d hits)\n", stats_miss / (float) (stats_miss + stats_hit), stats_miss, stats_hit));

    tb_arena_clear(&f->tmp_arena);

    f->scheduled_n = 0;
    f->scheduled = NULL;

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->locations = ctx.locations;
    func_out->base_locals = ctx.call_usage*8;
    func_out->stack_slots = ctx.debug_stack_slots;
    func_out->stack_header = ctx.stack_header;
    func_out->stack_usage = ctx.stack_usage;
    func_out->prologue_length = ctx.prologue_length;
    func_out->epilogue_length = ctx.epilogue_length;
    func_out->nop_pads = ctx.nop_pads;
}
