#include "codegen.h"

// used by codegen.h & it's friends but some of those get compiled multiple
// TUs and i want a consistent address.
RegMask TB_REG_EMPTY = { 1, 0, 1, { 0 } };

VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n) {
    int i = aarray_length(ctx->vregs);
    aarray_insert(ctx->vreg_map, n->gvn, i);
    aarray_push(ctx->vregs, (VReg){ .n = n, .assigned = -1, .spill_cost = NAN });
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

    assert(bb);
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

static bool reg_mask_may_stack(RegMask* a) {
    return a->class == REG_CLASS_STK || a->may_spill;
}

