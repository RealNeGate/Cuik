#include "codegen.h"

// used by codegen.h & it's friends but some of those get compiled multiple
// TUs and i want a consistent address.
RegMask TB_REG_EMPTY = { 1, 0, 1, { 0 } };

VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n) {
    int i = aarray_length(ctx->vregs);
    aarray_insert(ctx->vreg_map, n->gvn, i);
    aarray_push(ctx->vregs, (VReg){ .n = n, .assigned = -1, .spill_cost = NAN, .uses = 1 });
    return &ctx->vregs[i];
}

void tb__insert(Ctx* ctx, TB_Function* f, TB_BasicBlock* bb, TB_Node* n) {
    if (f->node_count >= f->scheduled_n) {
        TB_BasicBlock** new_sched = tb_arena_alloc(&f->arena, 2 * f->scheduled_n * sizeof(TB_BasicBlock*));
        memcpy(new_sched, f->scheduled, f->scheduled_n * sizeof(TB_BasicBlock*));
        FOR_N(i, f->scheduled_n, 2 * f->scheduled_n) {
            new_sched[i] = NULL;
        }
        f->scheduled = new_sched;
        f->scheduled_n *= 2;
    }

    TB_ASSERT(bb);
    f->scheduled[n->gvn] = bb;
}

void tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n) {
    TB_BasicBlock* bb = f->scheduled[before_n->gvn];
    tb__insert(ctx, f, bb, n);

    size_t i = 0, cnt = aarray_length(bb->items);
    while (i < cnt && bb->items[i] != before_n) { i++; }

    aarray_push(bb->items, 0);
    memmove(&bb->items[i + 1], &bb->items[i], (cnt - i) * sizeof(TB_Node*));
    bb->items[i] = n;
}

void tb__remove_node(Ctx* ctx, TB_Function* f, TB_Node* n) {
    TB_BasicBlock* bb = f->scheduled[n->gvn];

    size_t i = 0, cnt = aarray_length(bb->items);
    while (i < cnt && bb->items[i] != n) { i++; }

    TB_ASSERT(bb->items[i] == n);
    memmove(&bb->items[i], &bb->items[i + 1], (cnt - (i + 1)) * sizeof(TB_Node*));
    aarray_pop(bb->items);
    f->scheduled[n->gvn] = NULL;
}

void tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* after_n) {
    TB_BasicBlock* bb = f->scheduled[after_n->gvn];
    tb__insert(ctx, f, bb, n);

    size_t i = 0, cnt = aarray_length(bb->items);
    while (i < cnt && bb->items[i] != after_n) { i++; }

    TB_ASSERT(i != cnt);
    i += 1;

    aarray_push(bb->items, NULL);
    memmove(&bb->items[i + 1], &bb->items[i], (cnt - i) * sizeof(TB_Node*));
    bb->items[i] = n;
}

RegMask* tb__reg_mask_meet(Ctx* ctx, RegMask* a, RegMask* b) {
    // a /\ a = a
    if (a == b) { return a; }
    // a /\ TOP = a
    if (a == NULL) { return b; }
    if (b == NULL) { return a; }
    // if they both may spill, we can intersect on the stack
    bool may_spill = a->may_spill && b->may_spill;
    // a /\ b = BOT if their masks disagree
    if (!may_spill && a->class != b->class) { return &TB_REG_EMPTY; }
    // if it's stack and both don't ask for a slot... we're good
    // a /\ b = intersect masks
    assert(a->count == b->count);
    assert(a->count == 1);
    uint64_t i = a->mask[0] & b->mask[0];
    return intern_regmask(ctx, i == 0 ? 1 : a->class, may_spill, i);
}

void tb__print_regmask(RegMask* mask) {
    if (!reg_mask_is_not_empty(mask)) {
        printf("[SPILL]");
        return;
    }

    TB_ASSERT(mask->count == 1 && "TODO");

    int i = 0;
    bool comma = false;
    uint64_t bits = mask->mask[0];

    printf("[%s:", reg_class_name(mask->class));
    while (bits) {
        // skip zeros
        int skip = __builtin_ffs(bits) - 1;
        i += skip, bits >>= skip;

        if (!comma) {
            comma = true;
        } else {
            printf(", ");
        }

        // find sequence of ones
        int len = __builtin_ffs(~bits) - 1;
        printf("R%d", i);
        if (len > 1) {
            printf(" .. R%d", i+len-1);
        }

        // skip ones
        bits >>= len, i += len;
    }

    if (mask->may_spill) {
        printf(" | SPILL");
    }
    printf("]");
}

static bool reg_mask_may_intersect(RegMask* a, RegMask* b) {
    if (a == b) {
        return true;
    } else if (a->class != b->class) {
        return false;
    }

    if (a->may_spill && b->may_spill) {
        return true;
    }

    TB_ASSERT(a->count == b->count);
    FOR_N(i, 0, a->count) {
        if ((a->mask[i] & b->mask[i]) != 0) {
            return true;
        }
    }

    return false;
}

static void redo_dataflow(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;

    aarray_for(i, ctx->cfg.blocks) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        bb->live_in  = set_create_in_arena(arena, f->node_count);
        bb->live_out = set_create_in_arena(arena, f->node_count);
    }

    tb_dataflow(f, arena, ctx->cfg);
}

static RegMask* constraint_in(Ctx* ctx, TB_Node* n, int i) {
    ctx->constraint(ctx, n, ctx->ins);
    return ctx->ins[i];
}

// static const char* GPR_NAMES[] = { "X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",  "X9", "X10", "X11", "X12", "X13", "X14", "X15" };
static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static void print_reg_name(int rg, int num) {
    if (rg == 1) {
        printf("R%d", num);
        // printf("%s", GPR_NAMES[num]);
    } else if (rg == 2) {
        printf("XMM%d", num);
    } else if (rg == REG_CLASS_STK) {
        printf("STACK%d", num);
    } else {
        tb_todo();
    }
}

static int reg_assign(Ctx* ctx, VReg* vreg, uint64_t* mask, size_t num_regs) {
    int def_class = vreg->mask->class;
    size_t mask_word_count = (num_regs + 63) / 64;

    // find unset bit
    int reg = -1;
    FOR_N(j, 0, mask_word_count) {
        if (mask[j] == UINT64_MAX) { continue; }

        int index = mask[j] != 0 ? tb_ffs64(~mask[j]) - 1 : 0;
        if (j*64 + index < num_regs) {
            reg = j*64 + index;
        }
        break;
    }

    if (reg < 0) {
        if (def_class != REG_CLASS_STK) {
            TB_OPTDEBUG(REGALLOC)(printf("#   assigned UNCOLORED\n"));
        }

        // failed to color.. sadge
        vreg->class    = 0;
        vreg->assigned = -1;
        return false;
    } else {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(def_class, reg), printf("\n"));

        vreg->class    = def_class;
        vreg->assigned = reg;
        return true;
    }
}

static void rematerialize(Ctx* ctx, int* fixed_vregs, TB_Node* n) {
    // TB_ASSERT(n->input_count == 1 && "for now remat only happens for simple stuff like constants");

    size_t extra = extra_bytes(n);
    TB_Function* f = ctx->f;
    TB_Node* root = f->root_node;
    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    // don't want weird pointer invalidation crap
    size_t user_count = n->user_count;
    TB_User* users = tb_arena_alloc(&f->tmp_arena, n->user_count * sizeof(TB_User));
    memcpy(users, n->users, n->user_count * sizeof(TB_User));

    // aggressive reload
    for (size_t i = 0; i < user_count; i++) {
        TB_Node* use_n = USERN(&users[i]);
        int use_i      = USERI(&users[i]);

        // it's never in[0] lmao
        assert(use_i != 0);
        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        // remat per use site
        TB_Node* remat = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
        memcpy(remat->extra, n->extra, extra);
        FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
            remat->inputs[j] = n->inputs[j];
            add_user(f, remat, n->inputs[j], j);
        }

        set_input(f, use_n, remat, use_i);

        // schedule the split right before use
        tb__insert_before(ctx, ctx->f, remat, use_n);
        VReg* reload_vreg = tb__set_node_vreg(ctx, remat);

        // reloads are unlikely to spill... but not impossible
        reload_vreg->spill_bias = 1e9;

        RegMask* remat_mask = ctx->constraint(ctx, remat, NULL);
        reload_vreg->mask = tb__reg_mask_meet(ctx, in_mask, remat_mask);
        assert(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        // if it's remat'ing a copy, we should edit the def mask to match the use
        if (remat->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(remat);

            // slightly harder to rematerialize than a normal remat because we tightened it
            reload_vreg->spill_bias = 1e10;
            cpy->def = reload_vreg->mask;
        }

        // if we're going into a fixed-dst copy, we should hint towards that vreg
        if (fixed_vregs && use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            int fixed = fixed_reg_mask(cpy->def);
            if (fixed >= 0) {
                reload_vreg->hint_vreg = fixed_vregs[cpy->def->class] + fixed;
            }
        }

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: remat  (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(&f->tmp_arena, sp);

    // delete the original def
    ctx->vregs[ctx->vreg_map[n->gvn]].uses -= 1;
    ctx->vreg_map[n->gvn] = 0;
    tb__remove_node(ctx, f, n);
    tb_kill_node(f, n);
}

static void dump_sched(Ctx* restrict ctx) {
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf("BB %zu (freq=%f):\n", i, bb->freq);
        aarray_for(i, bb->items) {
            printf("  ");
            tb_print_dumb_node(NULL, bb->items[i]);
            printf("\n");
        }
    }
}

static void spill_entire_lifetime(Ctx* ctx, VReg* to_spill, RegMask* spill_mask, bool conflict) {
    TB_Function* f = ctx->f;
    TB_Node* n = to_spill->n;
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: spill  (%%%u)\x1b[0m\n", to_spill - ctx->vregs, n->gvn));

    to_spill->mask = spill_mask;

    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);

    // don't want weird pointer invalidation crap
    size_t user_count = n->user_count;
    TB_User* users = tb_arena_alloc(&f->tmp_arena, n->user_count * sizeof(TB_User));
    memcpy(users, n->users, n->user_count * sizeof(TB_User));

    // aggressive reload
    for (size_t i = 0; i < user_count; i++) {
        TB_Node* use_n = USERN(&users[i]);
        int use_i      = USERI(&users[i]);

        // it's never in[0] or the extra deps
        if (use_i == 0 || use_i >= use_n->input_count) {
            continue;
        }

        RegMask* in_mask = constraint_in(ctx, use_n, use_i);
        if (conflict) {
            RegMask* intersect = tb__reg_mask_meet(ctx, in_mask, spill_mask);
            if (intersect == spill_mask) { continue; }
        }

        // if it's already a machine copy, inserting an extra one is useless
        if (use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            if (!reg_mask_is_stack(in_mask) || !reg_mask_is_stack(spill_mask)) {
                TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%d: folded reload (%%%u)\x1b[0m\n", ctx->vreg_map[use_n->gvn], use_n->gvn));
                cpy->use = spill_mask;
                continue;
            }
        }

        TB_ASSERT(in_mask != NULL);
        if (reg_mask_is_stack(in_mask) && reg_mask_is_stack(spill_mask)) {
            // stack-stack moves require an intermediate copy to a register
            //   whichever register we'll use to transfer, for now we'll assume any normie[1] is fine
            RegMask* xfer_mask = ctx->normie_mask[1];
            //   src stk -> reg
            TB_Node* to_reg = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, to_reg, n, 1);
            TB_NODE_SET_EXTRA(to_reg, TB_NodeMachCopy, .def = xfer_mask, .use = spill_mask);
            //   reg -> dst stk
            TB_Node* to_stk = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, use_n, to_stk, use_i);
            set_input(f, to_stk, to_reg, 1);
            TB_NODE_SET_EXTRA(to_stk, TB_NodeMachCopy, .def = in_mask, .use = xfer_mask);

            // schedule the split right before use
            tb__insert_before(ctx, ctx->f, to_reg, use_n);
            tb__insert_before(ctx, ctx->f, to_stk, use_n);

            VReg* to_reg_vreg = tb__set_node_vreg(ctx, to_reg);
            to_reg_vreg->mask = xfer_mask;

            VReg* to_stk_vreg = tb__set_node_vreg(ctx, to_stk);
            to_stk_vreg->mask = in_mask;

            // TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: stack-stack reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n->gvn));
        } else {
            // reload per use site
            TB_Node* reload_n = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, use_n, reload_n, use_i);
            set_input(f, reload_n, n, 1);
            TB_NODE_SET_EXTRA(reload_n, TB_NodeMachCopy, .def = in_mask, .use = spill_mask);

            // schedule the split right before use
            tb__insert_before(ctx, ctx->f, reload_n, use_n);
            VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n);
            reload_vreg->mask = in_mask;

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   V%zu: reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n->gvn));
        }
    }
    tb_arena_restore(&f->tmp_arena, sp);
}

