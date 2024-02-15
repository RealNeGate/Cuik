#ifdef TB_HAS_WASM
#include "../tb_internal.h"
#include "../emitter.h"
#include "../opt/passes.h"

typedef struct {
    int id; // where in the locals it went, -1 for unallocated
    int uses;
} ValueDesc;

typedef struct DomTree DomTree;
struct DomTree {
    int id, depth;
    TB_Node *start, *end;
    DynArray(DomTree*) kids;
};

typedef struct Ctx {
    TB_Passes* p;
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_FeatureSet features;

    TB_CFG cfg;
    size_t block_count;

    DomTree* doms;
    DynArray(DomTree*) filtered;

    int stack_n;
    TB_Node* stack[64];

    int local_count;
    ValueDesc* locals;
} Ctx;

static void indent(int depth) { while (depth--) printf("  "); }

// uleb128 encode
static void emit_uint(Ctx* ctx, uint64_t x) {
    do {
        uint32_t lo = x & 0x7F;
        x >>= 7;
        if (x) {
            lo |= 0x80;
        }
        EMIT1(&ctx->emit, lo);
    } while (x);
}

static int use_count(TB_Node* n) {
    int c = 0;
    FOR_USERS(u, n) { c += 1; }
    return c;
}

static int spill_tos(Ctx* ctx, TB_Node* n) {
    // if there was something on top, we probably wanna spill it later
    ValueDesc* spilled = &ctx->locals[n->gvn];
    if (spilled->id < 0) {
        spilled->id = ctx->local_count++;
    }
    return spilled->id;
}

static void push_val(Ctx* ctx, TB_Node* n) {
    spill_tos(ctx, n);
    ctx->stack[ctx->stack_n++] = n;
}

static void compile_bb(Ctx* ctx, TB_Node* bb_start, int depth) {
    TB_BasicBlock* bb = ctx->p->scheduled[bb_start->gvn];
    Worklist* ws = &ctx->p->worklist;

    #ifndef NDEBUG
    TB_BasicBlock* expected = &nl_map_get_checked(ctx->cfg.node_to_block, bb_start);
    assert(expected == bb);
    #endif

    greedy_scheduler(ctx->p, &ctx->cfg, ws, NULL, bb, bb->end);

    FOREACH_N(i, ctx->cfg.block_count, dyn_array_length(ws->items)) {
        TB_Node* n = ws->items[i];
        if (n->type == TB_PROJ || n->type == TB_REGION || n->type == TB_PHI) continue;

        // begin building up a tree (right now they'll all be tiny)
        ctx->stack_n  = 1;
        ctx->stack[0] = n;

        switch (n->type) {
            case TB_INTEGER_CONST:
            break;

            // integer ops
            case TB_AND:
            case TB_OR:
            case TB_XOR:
            case TB_ADD:
            case TB_SUB:
            case TB_MUL:
            case TB_CMP_EQ:
            case TB_CMP_NE:
            case TB_CMP_SLT:
            case TB_CMP_SLE:
            case TB_CMP_ULT:
            case TB_CMP_ULE:
            case TB_CMP_FLT:
            case TB_CMP_FLE:
            {
                push_val(ctx, n->inputs[2]);
                push_val(ctx, n->inputs[1]);
                break;
            }

            case TB_RETURN: {
                push_val(ctx, n->inputs[3]);
                break;
            }

            default: tb_todo();
        }

        // ok emit ops now
        FOREACH_REVERSE_N(i, 0, ctx->stack_n) {
            TB_Node* val_n = ctx->stack[i];
            ValueDesc* val = &ctx->locals[val_n->gvn];

            indent(depth);
            if (i != 0 && val->id >= 0) {
                printf("get_local %d\n", val->id);
                continue;
            }

            switch (val_n->type) {
                case TB_INTEGER_CONST: {
                    TB_NodeInt* i = TB_NODE_GET_EXTRA(val_n);
                    EMIT1(&ctx->emit, 0x41);
                    emit_uint(ctx, i->value);
                    break;
                }

                // integer ops
                case TB_AND:
                case TB_OR:
                case TB_XOR:
                case TB_ADD:
                case TB_SUB:
                case TB_MUL: {
                    static const uint8_t ops[] = { 0x71, 0x72, 0x73, 0x6A, 0x6B, 0x6C };

                    assert(n->dt.type == TB_INT);
                    int base = n->dt.data > 32 ? 0x7C - 0x6A : 0;
                    EMIT1(&ctx->emit, base + ops[n->type - TB_AND]);
                    break;
                }

                case TB_CMP_EQ:
                case TB_CMP_NE:
                case TB_CMP_SLT:
                case TB_CMP_SLE:
                case TB_CMP_ULT:
                case TB_CMP_ULE:
                {
                    // position of the 32bit ops, we'll relocate for 64bit ones (same offsets)
                    static const uint8_t ops[] = { 0x46, 0x47, 0x48, 0x4C, 0x49, 0x4D };

                    if (n->dt.type == TB_INT || n->dt.type == TB_PTR) {
                        int bits = n->dt.type == TB_INT ? n->dt.data : 32;
                        int base = bits > 32 ? 0x51 - 0x46 : 0;

                        EMIT1(&ctx->emit, base + ops[n->type - TB_CMP_EQ]);
                    } else {
                        tb_todo();
                    }
                    break;
                }

                case TB_RETURN: {
                    break;
                }

                default: tb_todo();
            }
            printf("\n");
        }

        if (n->dt.type == TB_INT || n->dt.type == TB_PTR || n->dt.type == TB_FLOAT) {
            int dst = spill_tos(ctx, n);
            indent(depth);
            printf("set_local %d\n", dst);
        }
    }
}

static TB_Node** successors(Ctx* ctx, Worklist* ws, TB_Node* end, size_t* out_succ_count) {
    size_t succ_count = !cfg_is_endpoint(end);
    if (end->type == TB_BRANCH) {
        succ_count = TB_NODE_GET_EXTRA_T(end, TB_NodeBranch)->succ_count;
    }
    *out_succ_count = succ_count;

    size_t base = dyn_array_length(ws->items);
    dyn_array_put_uninit(ws->items, succ_count);
    TB_Node** succ_blocks = &ws->items[base];

    if (end->type == TB_BRANCH) {
        FOR_USERS(u, end) {
            if (u->n->type == TB_PROJ) {
                TB_Node* succ = cfg_next_bb_after_cproj(u->n);
                int index = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
                succ_blocks[index] = succ;
            }
        }
    } else if (!cfg_is_endpoint(end)) {
        TB_Node* succ = cfg_next_control(end);
        if (succ) { succ_blocks[0] = succ; }
    }

    return succ_blocks;
}

static bool is_natural_loop(Ctx* ctx, TB_Node* header) {
    if (header->type == TB_REGION) {
        FOREACH_N(i, 0, header->input_count) {
            TB_Node* pred = cfg_get_pred(&ctx->cfg, header, i);
            if (slow_dommy(&ctx->cfg, header, pred)) {
                return true;
            }
        }
    }

    return false;
}

static bool has_merge_root(Ctx* ctx, TB_Node* n, int id) {
    if (n->type != TB_REGION) {
        return false;
    }

    FOREACH_N(i, 0, n->input_count) {
        TB_Node* pred = cfg_get_pred(&ctx->cfg, n, i);
        TB_BasicBlock* pred_bb = ctx->p->scheduled[pred->gvn];

        if (pred_bb->id >= id) {
            return false;
        }
    }

    return true;
}

static void do_dom_tree(Ctx* ctx, DomTree* node, int depth);

static void do_branch(Ctx* ctx, DomTree* src, TB_Node* bb_start, int depth) {
    TB_BasicBlock* bb = ctx->p->scheduled[bb_start->gvn];
    DomTree* dst = &ctx->doms[bb->id];

    if (dst->id < src->id || has_merge_root(ctx, bb_start, dst->id)) {
        // forward or backwards edge (continue/break)
        indent(depth);
        printf("  br %d (BB%d)\n", depth - dst->depth, dst->id);
    } else {
        do_dom_tree(ctx, dst, depth + 1);
    }
}

static void do_dom_tree(Ctx* ctx, DomTree* node, int depth) {
    bool loop = is_natural_loop(ctx, node->start);
    indent(depth);
    if (loop) {
        printf("loop 64 (depth=%d)\n", depth);
    } else {
        printf("block 64 (depth=%d)\n", depth);
    }

    // compile code for node
    indent(depth + 1);
    printf("BB%d\n", node->id);

    node->depth = depth;
    compile_bb(ctx, node->start, depth + 1);

    // find merges
    size_t base = dyn_array_length(ctx->filtered);
    FOREACH_N(i, 0, dyn_array_length(node->kids)) {
        TB_Node* start = node->kids[i]->start;
        TB_BasicBlock* start_bb = ctx->p->scheduled[start->gvn];

        if (has_merge_root(ctx, start, start_bb->id)) {
            dyn_array_put(ctx->filtered, node->kids[i]);
            node->kids[i]->depth = depth;
        }
    }

    // exit path of node
    size_t succ_count;
    TB_Node** succ_blocks = successors(ctx, &ctx->p->worklist, node->end, &succ_count);
    if (succ_count == 1) {
        do_branch(ctx, node, succ_blocks[0], depth);
    } else if (succ_count == 2) {
        indent(depth + 1);
        printf("if 64 (depth=%d)\n", depth + 1);

        do_branch(ctx, node, succ_blocks[0], depth + 1);

        indent(depth + 1);
        printf("else\n");

        do_branch(ctx, node, succ_blocks[1], depth + 1);

        indent(depth + 1);
        printf("end\n");
    } else if (succ_count != 0) {
        tb_todo();
    }

    // compile merges
    FOREACH_N(i, base, dyn_array_length(ctx->filtered)) {
        do_dom_tree(ctx, ctx->filtered[i], depth + 1);
    }
    dyn_array_set_length(ctx->filtered, base);

    indent(depth);
    printf("end\n");
}

static int dom_sort_cmp(const void* a, const void* b) {
    DomTree* const* aa = a;
    DomTree* const* bb = b;
    return aa[0]->id - bb[0]->id;
}

static void compile_function(TB_Passes* restrict p, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, TB_Arena* code_arena, bool emit_asm) {
    verify_tmp_arena(p);

    TB_Arena* arena = tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    TB_Function* restrict f = p->f;
    TB_OPTDEBUG(CODEGEN)(tb_pass_print(p));

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .p = p,
        .emit = {
            .output = func_out,
            .arena = arena,
            .has_comments = false,
        }
    };

    // allocate entire top of the code arena (we'll trim it later if possible)
    ctx.emit.capacity = code_arena->high_point - code_arena->watermark;
    ctx.emit.data = tb_arena_alloc(code_arena, ctx.emit.capacity);

    ctx.locals = tb_arena_alloc(arena, f->node_count * sizeof(ValueDesc));
    FOREACH_N(i, 0, f->node_count) {
        ctx.locals[i].id = -1;
    }

    FOR_USERS(u, f->root_node) if (u->n->type == TB_PROJ) {
        int i = TB_NODE_GET_EXTRA_T(u->n, TB_NodeProj)->index;
        if (i >= 3) {
            // params fit into the first few locals
            ctx.locals[u->n->gvn].id = i - 3;
            ctx.locals[u->n->gvn].uses = use_count(u->n);
            ctx.local_count++;
        }
    }

    if (features == NULL) {
        ctx.features = (TB_FeatureSet){ 0 };
    } else {
        ctx.features = *features;
    }

    Worklist* restrict ws = &p->worklist;
    worklist_clear(ws);

    CUIK_TIMED_BLOCK("global sched") {
        // We need to generate a CFG
        ctx.cfg = tb_compute_rpo(f, p);
        ctx.block_count = dyn_array_length(ws->items);
        // And perform global scheduling
        tb_pass_schedule(p, ctx.cfg, false);
    }

    DomTree* doms = ctx.doms = tb_arena_alloc(tmp_arena, ctx.cfg.block_count * sizeof(DomTree));
    FOREACH_N(i, 0, ctx.cfg.block_count) {
        doms[i].id    = i;
        doms[i].depth = -1;
        doms[i].start = NULL;
        doms[i].end   = NULL;
        doms[i].kids  = NULL;
    }

    FOREACH_N(i, 0, ctx.cfg.block_count) {
        TB_BasicBlock* bb = ctx.p->scheduled[ws->items[i]->gvn];
        doms[i].start = ws->items[i];
        doms[i].end   = bb->end;

        dyn_array_put(doms[bb->dom->id].kids, &doms[bb->id]);
    }

    FOREACH_N(i, 0, ctx.cfg.block_count) {
        DomTree* node = &ctx.doms[i];
        qsort(node->kids, dyn_array_length(node->kids), sizeof(DomTree*), dom_sort_cmp);
    }

    CUIK_TIMED_BLOCK("emit") {
        worklist_clear_visited(ws);
        do_dom_tree(&ctx, &doms[0], 0);
    }

    tb_free_cfg(&ctx.cfg);

    // trim code arena (it fits in a single chunk so just arena free the top)
    code_arena->watermark = (char*) &ctx.emit.data[ctx.emit.count];
    tb_arena_realign(code_arena);

    // TODO(NeGate): move the assembly output to code arena
    if (emit_asm) CUIK_TIMED_BLOCK("dissassembly") {
        __debugbreak();
    }

    log_debug("%s: code_arena=%.1f KiB", f->super.name, tb_arena_current_size(code_arena) / 1024.0f);
    tb_arena_restore(arena, sp);
    p->scheduled = NULL;

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    // func_out->locations = ctx.locations;
    // func_out->stack_slots = ctx.debug_stack_slots;
    // func_out->stack_usage = ctx.stack_usage;
    // func_out->prologue_length = ctx.prologue_length;
    // func_out->epilogue_length = ctx.epilogue_length;
}

static size_t emit_call_patches(TB_Module* restrict m, TB_FunctionOutput* out_f) {
    return 0;
}

static void get_data_type_size(TB_DataType dt, size_t* out_size, size_t* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // above 64bits we really dont care that much about natural alignment
            bool is_big_int = dt.data > 64;

            // round up bits to a byte
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data - 1);

            *out_size  = ((bits+7) / 8);
            *out_align = is_big_int ? 8 : ((dt.data + 7) / 8);
            break;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            *out_size = s;
            *out_align = s;
            break;
        }
        case TB_PTR: {
            *out_size = 8;
            *out_align = 8;
            break;
        }
        default: tb_unreachable();
    }
}

ICodeGen tb__wasm32_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 32,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
#endif
