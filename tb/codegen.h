// This is the general codegen framework, it combines together the various components in
// the target dependent part, if you see any of these TODOs and wanna contribute, those
// are good places to go.
//
// This can be broken down into a few steps:
//
//   Init => Selection => RA constraints => GCM + Dataflow => Local sched => Regalloc => Emit
//
// Here's where you can find more info on these:
//
//   * Init: fill in the ctx.num_regs and ctx.normie_mask i guess
//
//   * Selection: handled by node_isel(), it'll just do a bottom-up rewrite of the generic nodes into
//     machine-friendly forms, this is where you might introduce things like
//
//   * RA constraints: handled by node_constraint(), this is how we define the node's constraints.
//
//   * GCM: we've kept all the nice SoN dependencies so we can flatten the graph after isel (making
//     the most of memory aliasing for instance)
//
//   * Dataflow: because we're in SSA form, we don't need a local schedule to derive the
//     live-ins & outs, see gcm.c for more info.
//
//   * Local sched: just toposorts the nodes in a block, for now we've got a dumb latency-based
//     list scheduler (list_scheduler) & an RPO walker (greedy_scheduler).
//
//   * Reg alloc: solves the constraints, inserting nodes into an already scheduled graph if
//     necessary (and it is necessary since spilling is basically inevitable)
//
//   * Emit: handled by node_emit, writes out the bytes from the flattened SoN + RA results.
//
#pragma once

#include "opt/passes.h"
#include "emitter.h"
#include <log.h>
#include <math.h>
#include <arena_array.h>

enum {
    // every platform has a stack regclass, the mask is actually an offset
    // on the stack (useful for parameter passing).
    REG_CLASS_STK = 0,

    // all we can fit into 3bits, but also... 8 classes is a lot.
    //
    // * x86 has 3 currently: Stack, GPR, Vector.
    MAX_REG_CLASSES = 8,
};

enum {
    // any assigned stack vregs past this point refer to spill slots
    // not the stack base (where params usually go).
    STACK_BASE_REG_NAMES = 0x4000,
};

// represents a set of registers, usually for register constraints.
// we can also say that a value might fit into the stack with may_spill.
//
// we can model these regmasks in a lattice, why? because I made a
// meet function (mfw my shit gets ordered).
struct RegMask {
    uint64_t class     : 3;
    uint64_t may_spill : 1;
    uint64_t count     : 60;
    uint64_t mask[];
};

typedef struct {
    TB_SymbolPatch* patch;
    TB_Location* loc;
    TB_Location* end;
    Comment* comment;
    ArenaArray(TB_Safepoint*) safepoints;
    int safepoint_i;
} Disasm;

typedef struct VReg VReg;
struct VReg {
    TB_Node* n;

    int16_t class;
    int16_t assigned;

    RegMask* mask;

    // spill cost (sum of block_freq * uses_in_block)
    //   NaN if not computed yet
    float spill_cost;
    // certain events make us more likely to bias spilling, mostly
    // if we've already spilled.
    float spill_bias;
    int hint_vreg;

    bool marked_spilled;

    // only matters for chaitin
    struct {
        // debug purposes only
        int coalesced;
    };
};

typedef struct Ctx Ctx;
typedef int (*TmpCount)(Ctx* restrict ctx, TB_Node* n);

// ins can be NULL
typedef RegMask* (*NodeConstraint)(Ctx* restrict ctx, TB_Node* n, RegMask** ins);

typedef bool (*NodeRemat)(TB_Node* n);

// if we're doing 2addr ops like x86 the real operations are mutating:
//
//   v4 = x86_add v5, v6 # would require a move
//
// to avoid unnecessary moves and stretched lifetimes we'll tell RA which
// each is the "shared" edge (holds input but could act as the dst), in this
// case that's whichever holds v5 "in[1]".
//
// returns -1 if there's no edge btw.
typedef int (*TB_2Addr)(TB_Node* n);

typedef struct {
    uint32_t* pos;
    uint32_t target;
} JumpTablePatch;

typedef struct {
    int count;
    int elems[]; // vregs
} Tmps;

// like a VLIW bundle, except it can be used to model delay slots.
// there's also the degenerate case where the bundle is always 1
// node (most superscalars are in this camp)
typedef struct {
    int count;
    TB_Node** arr;
} Bundle;

struct Ctx {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_FeatureSet features;
    TB_Node* frame_ptr;
    TB_CFG cfg;
    TB_Worklist* walker_ws;

    // user callbacks
    TmpCount tmp_count;
    NodeConstraint constraint;
    TB_2Addr node_2addr;
    NodeRemat remat;

    // target-dependent index
    int abi_index;
    int fallthrough;

    int param_count;
    uint8_t prologue_length;
    uint8_t epilogue_length;
    uint8_t nop_pads;

    // Basic blocks
    int bb_count;
    TB_Node** rpo_nodes;

    // used when calling node_constraint since it needs an array, we
    // figure out the max input count of all nodes before allocating it.
    RegMask** ins;

    // Values
    ArenaArray(VReg) vregs;   // [vid]
    ArenaArray(int) vreg_map; // [gvn] -> vid
    NL_Table tmps_map;        // TB_Node* -> Tmps*

    // Regalloc
    int num_spills;
    int stack_slot_size;
    int stack_header;
    int stack_usage;
    int call_usage;
    int num_classes;
    int num_regs[MAX_REG_CLASSES];

    NL_HashSet mask_intern;
    RegMask* normie_mask[MAX_REG_CLASSES];
    RegMask* mayspill_mask[MAX_REG_CLASSES];

    DynArray(TB_StackSlot) debug_stack_slots;
    DynArray(JumpTablePatch) jump_table_patches;

    // Line info
    TB_BasicBlock* current_emit_bb;
    int current_emit_bb_pos;

    DynArray(TB_Location) locations;
};

extern RegMask TB_REG_EMPTY;

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena);
void tb__chaitin(Ctx* restrict ctx, TB_Arena* arena);

// RA helpers
RegMask* tb__reg_mask_meet(Ctx* ctx, RegMask* a, RegMask* b);
void tb__insert(Ctx* ctx, TB_Function* f, TB_BasicBlock* bb, TB_Node* n);
void tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
void tb__remove_node(Ctx* ctx, TB_Function* f, TB_Node* n);
void tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n);

static bool tb__reg_mask_less(Ctx* ctx, RegMask* a, RegMask* b) {
    return a == b ? false : tb__reg_mask_meet(ctx, a, b) != a;
}

static VReg* vreg_at(Ctx* ctx, int id)       { return id > 0 ? &ctx->vregs[id] : NULL; }
static VReg* node_vreg(Ctx* ctx, TB_Node* n) { return n && ctx->vreg_map[n->gvn] > 0 ? &ctx->vregs[ctx->vreg_map[n->gvn]] : NULL; }

static bool can_remat(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        // these can rematerialize
        case TB_POISON:
        case TB_ICONST:
        case TB_F32CONST:
        case TB_F64CONST:
        case TB_MACH_COPY:
        return true;

        // user-defined rematerializing
        default:
        return ctx->remat(n);
    }
}

static float get_spill_cost(Ctx* restrict ctx, VReg* vreg) {
    if (!isnan(vreg->spill_cost)) {
        return vreg->spill_cost;
    } else if (can_remat(ctx, vreg->n)) {
        return (vreg->spill_cost = -1.0f + vreg->spill_bias);
    }

    float c = 0.0f;

    // sum of (block_freq * uses_in_block)
    FOR_USERS(u, vreg->n) {
        TB_Node* un = USERN(u);
        if (ctx->f->scheduled[un->gvn] == NULL) { continue; }
        c += ctx->f->scheduled[un->gvn]->freq;
    }

    return (vreg->spill_cost = c + vreg->spill_bias);
}

static int op_reg_at(Ctx* ctx, TB_Node* n, int class) {
    assert(ctx->vreg_map[n->gvn] > 0);
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    assert(vreg->assigned >= 0);
    assert(vreg->class == class);
    return vreg->assigned;
}

static const char* reg_class_name(int class) {
    switch (class) {
        case 0: return "STK";
        case 1: return "GPR";
        case 2: return "XMM";
        default: return NULL;
    }
}

static bool reg_mask_eq(RegMask* a, RegMask* b) {
    if (a->count != b->count) { return false; }
    FOR_N(i, 0, a->count) {
        if (a->mask[i] != b->mask[i]) return false;
    }

    return true;
}

static bool reg_mask_is_not_empty(RegMask* mask) {
    if (mask == NULL) return false;

    assert(mask->count == 1);
    FOR_N(i, 0, mask->count) {
        if (mask->mask[0] != 0) return true;
    }

    return false;
}

static bool reg_mask_is_stack(RegMask* mask) {
    return mask->class == REG_CLASS_STK || (!reg_mask_is_not_empty(mask) && mask->may_spill);
}

static bool reg_mask_is_spill(RegMask* mask) {
    return mask->class != REG_CLASS_STK && (!reg_mask_is_not_empty(mask) && mask->may_spill);
}

static int fixed_reg_mask(RegMask* mask) {
    if (mask->class == REG_CLASS_STK) {
        assert(mask->count == 1);
        return mask->mask[0];
    } else {
        int set = -1;
        FOR_N(i, 0, mask->count) {
            int found = 63 - tb_clz64(mask->mask[i]);
            if (mask->mask[0] == (1ull << found)) {
                if (set >= 0) return -1;
                set = i*64 + found;
            }
        }

        return set;
    }
}

static RegMask* new_regmask(TB_Function* f, int reg_class, bool may_spill, uint64_t mask) {
    RegMask* rm = tb_arena_alloc(&f->arena, sizeof(RegMask) + sizeof(uint64_t));
    rm->may_spill = may_spill;
    rm->class = reg_class;
    rm->count = 1;
    rm->mask[0] = mask;
    return rm;
}

static uint32_t rm_hash(void* a) {
    RegMask* x = a;
    uint32_t sum = 0;
    FOR_N(i, 0, x->count) {
        sum += x->mask[i];
    }

    return x->count + x->class + x->may_spill + sum;
}

static bool rm_compare(void* a, void* b) {
    RegMask *x = a, *y = b;
    if (x->count != y->count || x->class != y->class || x->count != y->count) {
        return false;
    }

    FOR_N(i, 0, x->count) {
        if (x->mask[i] != y->mask[i]) { return false; }
    }
    return true;
}

static RegMask* intern_regmask(Ctx* ctx, int reg_class, bool may_spill, uint64_t mask) {
    RegMask* new_rm = new_regmask(ctx->f, reg_class, may_spill, mask);
    RegMask* old_rm = nl_hashset_put2(&ctx->mask_intern, new_rm, rm_hash, rm_compare);
    if (old_rm != NULL) {
        tb_arena_free(&ctx->f->arena, new_rm, sizeof(RegMask));
        return old_rm;
    }
    return new_rm;
}
