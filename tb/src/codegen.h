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
#include <arena_array.h>

enum {
    // every platform has a stack regclass, the mask is actually an offset
    // on the stack (useful for parameter passing).
    REG_CLASS_STK = 0,

    // all we can fit into 3bits, but also... 8 classes is a lot.
    //
    // * x86 has 3 currently: Stack, GPR, Vector, and FLAGS.
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
    int id;

    // times
    int start_t, end_t;

    TB_Node* n;
    TB_Node* end_n;

    // local schedule
    ArenaArray(TB_Node*) items;
} MachineBB;

typedef struct {
    TB_SymbolPatch* patch;
    TB_Location* loc;
    TB_Location* end;
    Comment* comment;
} Disasm;

// Static-sized hash map
typedef struct {
    TB_Node* k;
    MachineBB* v;
} NodeToBB;

typedef struct Range {
    struct Range* next;
    int start, end;
} Range;

typedef struct VReg VReg;
struct VReg {
    TB_Node* n;

    int16_t class;
    int16_t assigned;

    RegMask* mask;

    // only matters for linear-scan
    struct {
        int end_time, hint_vreg;
        Range* active_range;
        Range* saved_range;
    };
};

typedef struct Ctx Ctx;
typedef int (*TmpCount)(Ctx* restrict ctx, TB_Node* n);

// ins can be NULL
typedef RegMask* (*NodeConstraint)(Ctx* restrict ctx, TB_Node* n, RegMask** ins);
typedef bool (*NodeFlags)(Ctx* restrict ctx, TB_Node* n);

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

struct Ctx {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_FeatureSet features;
    TB_Node* frame_ptr;

    // user callbacks
    TmpCount tmp_count;
    NodeConstraint constraint;
    NodeFlags flags;
    TB_2Addr node_2addr;

    // target-dependent index
    int abi_index;
    int fallthrough;

    uint8_t prologue_length;
    uint8_t epilogue_length;
    uint8_t nop_pads;

    // TB_Node* -> MachineBB*
    struct {
        size_t exp;
        NodeToBB* entries;
    } node_to_bb;

    // Basic blocks
    int bb_count;
    MachineBB* machine_bbs;

    // used when calling node_constraint since it needs an array, we
    // figure out the max input count of all nodes before allocating it.
    RegMask** ins;

    // Values
    ArenaArray(VReg) vregs;   // [vid]
    ArenaArray(int) vreg_map; // [gvn] -> vid
    NL_Table tmps_map;        // TB_Node* -> Tmps*

    // Regalloc
    bool has_flags; // if true, the reg class 1 is reserved for it

    int num_spills;
    int stack_slot_size;
    int stack_header;
    int stack_usage;
    int call_usage;
    int num_classes;
    int num_regs[MAX_REG_CLASSES];

    NL_HashSet mask_intern;
    RegMask* normie_mask[MAX_REG_CLASSES];

    DynArray(TB_StackSlot) debug_stack_slots;
    DynArray(JumpTablePatch) jump_table_patches;

    // Line info
    MachineBB* current_emit_bb;
    int current_emit_bb_pos;

    DynArray(TB_Location) locations;
};

extern RegMask TB_REG_EMPTY;

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena);
void tb__chaitin(Ctx* restrict ctx, TB_Arena* arena);

void tb__print_regmask(RegMask* mask);

// RA helpers
RegMask* tb__reg_mask_meet(Ctx* ctx, RegMask* a, RegMask* b);
MachineBB* tb__insert(Ctx* ctx, TB_Function* f, TB_BasicBlock* bb, TB_Node* n);
void tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
void tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n);

static bool tb__reg_mask_less(Ctx* ctx, RegMask* a, RegMask* b) {
    return a == b ? false : tb__reg_mask_meet(ctx, a, b) != a;
}

static VReg* vreg_at(Ctx* ctx, int id)       { return id > 0 ? &ctx->vregs[id] : NULL; }
static VReg* node_vreg(Ctx* ctx, TB_Node* n) { return n && ctx->vreg_map[n->gvn] > 0 ? &ctx->vregs[ctx->vreg_map[n->gvn]] : NULL; }

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
    RegMask* rm = tb_arena_alloc(f->tmp_arena, sizeof(RegMask) + sizeof(uint64_t));
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
        tb_arena_free(ctx->f->tmp_arena, new_rm, sizeof(RegMask));
        return old_rm;
    }
    return new_rm;
}

static uint32_t node_to_bb_hash(void* ptr) { return (((uintptr_t) ptr) * 11400714819323198485ull) >> 32ull; }
static MachineBB* node_to_bb(Ctx* restrict ctx, TB_Node* n) {
    uint32_t h = node_to_bb_hash(n);

    size_t mask = (1 << ctx->node_to_bb.exp) - 1;
    size_t first = h & mask, i = first;
    do {
        if (ctx->node_to_bb.entries[i].k == n) {
            return ctx->node_to_bb.entries[i].v;
        }

        i = (i + 1) & mask;
    } while (i != first);

    abort();
}

static void node_to_bb_put(Ctx* restrict ctx, TB_Node* n, MachineBB* bb) {
    uint32_t h = node_to_bb_hash(n);

    size_t mask = (1 << ctx->node_to_bb.exp) - 1;
    size_t first = h & mask, i = first;
    do {
        if (ctx->node_to_bb.entries[i].k == NULL) {
            ctx->node_to_bb.entries[i].k = n;
            ctx->node_to_bb.entries[i].v = bb;
            return;
        }

        i = (i + 1) & mask;
    } while (i != first);

    abort();
}

