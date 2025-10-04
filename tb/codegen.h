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
    // every platform has a stack regclass.
    REG_CLASS_STK = 0,

    // all we can fit into 3bits, but also... 8 classes is a lot.
    //
    // * x86 has 3 currently: Stack, GPR, Vector.
    MAX_REG_CLASSES = 8,
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
    TB_CGEmitter* emit;
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

    // always aligned, always power-of-two
    int reg_width;

    // how many program points has this vreg been alive for
    uint64_t area;
    // spill cost (sum of block_freq * uses_in_block)
    //   NaN if not computed yet
    double spill_cost;
    // certain events make us more likely to bias spilling, mostly
    // if we've already spilled.
    double spill_bias;
    int hint_vreg;

    // BRIGGS: when coalesced this number will go up
    int uses;

    bool was_spilled : 1;
    bool was_reload  : 1;
};

typedef struct Ctx Ctx;
typedef int (*TmpCount)(Ctx* restrict ctx, TB_Node* n);

// ins can be NULL
typedef RegMask* (*NodeConstraint)(Ctx* restrict ctx, TB_Node* n, RegMask** ins);
typedef int (*NodeConstraintKill)(Ctx* restrict ctx, TB_Node* n, RegMask** kills);

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
    uint64_t* pos;
    uint32_t target;
} JumpTablePatch;

// like a VLIW bundle, except it can be used to model delay slots.
// there's also the degenerate case where the bundle is always 1
// node (most superscalars are in this camp)
typedef struct {
    bool has_safepoint;
    int count;
    TB_Node** arr;
} Bundle;

typedef struct {
    // where is the stack pointer
    uint8_t sp_class, sp_reg;

    // where is the frame pointer defined (when enabled)
    uint8_t fp_class, fp_reg;

    // where is the return address
    uint8_t rpc_class, rpc_reg;

    // 'C'  caller save (volatile)
    // 'c'  callee save (non volatile)
    // '\0' no save
    const char* reg_saves[8];

    // when it's true we'll allocate the next
    // available param in the class rather than matching 1-to-1 to
    // the param slot.
    bool flexible_param_alloc;

    // param passing
    uint8_t param_count[8];
    uint8_t* params[8];

    // return vals
    uint8_t ret_count[8];
    uint8_t rets[8][2];
} CallingConv;

// Relevant to the DSL
typedef int MatchRuleID;
typedef TB_Node* (*MatchRule)(Ctx* ctx, TB_Function* f, TB_Node* n);

typedef struct {
    MatchRuleID id;
    TB_Node* n;
    int index;
} TB_SubMatch;

// inserted at the bottom of the function to handle whatever little runtime support
typedef struct TB_CodeStub {
    struct TB_CodeStub* prev;
    uint32_t tag;
    uint32_t pos;
} TB_CodeStub;

struct Ctx {
    TB_CGEmitter emit;
    TB_FeatureSet features;

    TB_Module* module;
    TB_Function* f;
    TB_Node* frame_ptr;
    TB_CFG cfg;
    TB_Worklist* walker_ws;

    size_t old_node_count;

    // user callbacks
    NodeConstraint constraint;
    NodeConstraintKill constraint_kill;

    TB_2Addr node_2addr;
    NodeRemat remat;

    void (*print_pretty)(Ctx* restrict ctx, TB_Node* n);

    // target-dependent index
    CallingConv* calling_conv;
    int fallthrough;

    int param_count;
    uint8_t prologue_length;
    uint8_t epilogue_length;
    uint8_t nop_pads;

    struct {
        int top;
        TB_SubMatch accept[16];
    } dsl;

    // Basic blocks
    int bb_count;

    // used when calling node_constraint since it needs an array, we
    // figure out the max input count of all nodes before allocating it.
    RegMask** ins;

    // cached set of mask for all argument passing slots, these are generally
    // clobbered by function calls so i figured keeping a copy of them around
    // would be nice.
    RegMask* cached_arg_list_mask;
    TB_CodeStub* stubs;

    // Values
    ArenaArray(VReg) vregs;   // [vid]
    ArenaArray(int) vreg_map; // [gvn] -> vid

    // Regalloc
    int num_spills;
    int stack_slot_size;
    int stack_header;
    int stack_usage;
    int call_usage;
    int num_classes;
    int num_regs[MAX_REG_CLASSES];

    NL_HashSet mask_intern;
    RegMask* all_mask[MAX_REG_CLASSES];
    RegMask* normie_mask[MAX_REG_CLASSES];
    RegMask* mayspill_mask[MAX_REG_CLASSES];

    DynArray(TB_StackSlot) debug_stack_slots;
    DynArray(JumpTablePatch) jump_table_patches;

    // Line info
    TB_BasicBlock* current_emit_bb;
    int current_emit_bb_pos;

    DynArray(TB_Location) locations;
};

// Rogers RA stats collection crap
TB_OPTDEBUG(STATS)(extern int stats_miss, stats_hit);

extern RegMask TB_REG_EMPTY;

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena);
void tb__briggs(Ctx* restrict ctx, TB_Arena* arena);

typedef struct {
    int count;
    int* stack;
    uint64_t* visited;
} IFG_Worklist;

static IFG_Worklist ifg_ws_alloc(Ctx* restrict ctx, TB_Arena* arena, int len);
static void ifg_ws_remove(IFG_Worklist* ws, int vreg_id);
static bool ifg_ws_push(IFG_Worklist* ws, int vreg_id);
static int ifg_ws_pop(IFG_Worklist* ws);

// RA helpers
RegMask* tb__reg_mask_meet(Ctx* ctx, RegMask* a, RegMask* b);
void tb__insert(Ctx* ctx, TB_Function* f, TB_BasicBlock* bb, TB_Node* n);
size_t tb__remove_node(Ctx* ctx, TB_Function* f, TB_Node* n);
size_t tb__insert_before(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
size_t tb__insert_after(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node* before_n);
VReg* tb__set_node_vreg(Ctx* ctx, TB_Node* n);
int tb__reg_width_from_dt(int reg_class, TB_DataType dt);

static bool tb__reg_mask_less(Ctx* ctx, RegMask* a, RegMask* b) {
    return a == b ? false : tb__reg_mask_meet(ctx, a, b) != a;
}

static VReg* vreg_at(Ctx* ctx, int id)       { return id > 0 ? &ctx->vregs[id] : NULL; }
static VReg* node_vreg(Ctx* ctx, TB_Node* n) { return n && ctx->vreg_map[n->gvn] > 0 ? &ctx->vregs[ctx->vreg_map[n->gvn]] : NULL; }

static void* add_code_stub(Ctx* restrict ctx, uint32_t tag, size_t size) {
    TB_CodeStub* stub = tb_arena_alloc(&ctx->f->arena, size);
    stub->tag = tag;
    stub->pos = 0;
    stub->prev = ctx->stubs;
    ctx->stubs = stub;
    return stub;
}

static bool can_remat(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        // these can rematerialize
        case TB_POISON:
        case TB_ICONST:
        case TB_F32CONST:
        case TB_F64CONST:
        case TB_MACH_COPY:
        case TB_MACH_TEMP:
        return true;

        // user-defined rematerializing
        default:
        return ctx->remat(n);
    }
}

static double get_node_spill_cost(Ctx* restrict ctx, TB_Node* n) {
    if (n->type == TB_MACH_TEMP) {
        return 1.0;
    } else if (n->type == TB_MACH_FRAME_PTR || (n->type != TB_PHI && n->user_count == 0)) {
        // no users? probably a projection that can't be spilled
        return INFINITY;
    } else {
        // remats grow slower
        double scale = can_remat(ctx, n) ? 0.5 : 1.0;

        // sum of (block_freq * uses_in_block)
        double c = 0.0f;
        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            if (ctx->f->scheduled[un->gvn] == NULL) { continue; }
            c += ctx->f->scheduled[un->gvn]->freq * scale;
        }

        return c;
    }
}

static double get_spill_cost(Ctx* restrict ctx, VReg* vreg) {
    if (isnan(vreg->spill_cost)) {
        vreg->spill_cost = get_node_spill_cost(ctx, vreg->n) + vreg->spill_bias;
    }

    // no area? this means it's used right after def
    if (vreg->area <= 1) {
        return INFINITY;
    }

    return vreg->spill_cost / vreg->area;
}

static int op_reg_at(Ctx* ctx, TB_Node* n, int class) {
    TB_ASSERT(ctx->vreg_map[n->gvn] > 0);
    VReg* vreg = &ctx->vregs[ctx->vreg_map[n->gvn]];
    TB_ASSERT(vreg->assigned >= 0);
    TB_ASSERT(vreg->class == class);
    return vreg->assigned;
}

static const char* reg_class_name(int class) {
    switch (class) {
        case 0: return "STK";
        case 1: return "GPR";
        case 2: return "XMM";
        case 3: return "FLAGS";
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

    FOR_N(i, 0, mask->count) {
        if (mask->mask[0] != 0) return true;
    }

    return false;
}

static bool reg_mask_is_stack(RegMask* mask) {
    return mask->class == REG_CLASS_STK || mask->may_spill;
}

static bool reg_mask_is_spill(RegMask* mask) {
    if (mask->class != REG_CLASS_STK || !mask->may_spill) {
        return false;
    }

    // if any bits are set it's not a spill slot
    FOR_N(i, 0, mask->count) {
        if (mask->mask[0] != 0) { return false; }
    }

    return true;
}

static int popcnt_reg_mask(RegMask* mask) {
    int sum = 0;
    FOR_N(i, 0, mask->count) {
        sum += tb_popcount64(mask->mask[i]);
    }
    return sum;
}

static bool within_reg_mask(RegMask* mask, uint64_t i) {
    return i/64 < mask->count ? mask->mask[i/64] & (1ull << (i%64)) : false;
}

static int fixed_reg_mask(RegMask* mask) {
    int set = -1;
    FOR_N(i, 0, mask->count) {
        if (mask->mask[i] == 0) { continue; }
        int found = tb_ffs64(mask->mask[i]) - 1;
        if (mask->mask[i] == (1ull << found)) {
            if (set >= 0) return -1;
            set = i*64 + found;
        }
    }

    return set;
}

static RegMask* new_regmask(TB_Function* f, int reg_class, bool may_spill, uint64_t mask) {
    RegMask* rm = tb_arena_alloc(&f->arena, sizeof(RegMask) + sizeof(uint64_t));
    rm->may_spill = may_spill;
    rm->class = reg_class;
    rm->count = 1;
    rm->mask[0] = mask;
    return rm;
}

static RegMask* new_regmask_range(TB_Function* f, int reg_class, bool may_spill, int start, int count) {
    TB_ASSERT(count != 0);
    int end = start + count;
    int words = (end + 63) / 64;

    RegMask* rm = tb_arena_alloc(&f->arena, sizeof(RegMask) + words*sizeof(uint64_t));
    rm->may_spill = may_spill;
    rm->class = reg_class;
    rm->count = words;

    // leading
    FOR_N(j, 0, (start + 63) / 64) { rm->mask[j] = 0; }
    if (start % 64) {
        rm->mask[start / 64] |= UINT64_MAX << (start % 64);
    }

    // trailing
    FOR_N(j, (start + 63) / 64, (end + 63) / 64) { rm->mask[j] = UINT64_MAX; }
    if (end % 64) {
        rm->mask[end / 64] &= UINT64_MAX >> (64ull - (end % 64));
    }
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
    if (x->may_spill != y->may_spill || x->count != y->count || x->class != y->class || x->count != y->count) {
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

static RegMask* intern_regmask2(Ctx* ctx, int reg_class, bool may_spill, int reg) {
    RegMask* new_rm = tb_arena_alloc(&ctx->f->arena, sizeof(RegMask) + sizeof(uint64_t));
    new_rm->may_spill = may_spill;
    new_rm->class = reg_class;
    new_rm->count = reg ? (reg + 63) / 64 : 1;
    FOR_N(i, 0, new_rm->count) {
        new_rm->mask[i] = 0;
    }
    new_rm->mask[reg / 64ull] |= (1ull << (reg % 64ull));

    RegMask* old_rm = nl_hashset_put2(&ctx->mask_intern, new_rm, rm_hash, rm_compare);
    if (old_rm != NULL) {
        tb_arena_free(&ctx->f->arena, new_rm, sizeof(RegMask));
        return old_rm;
    }
    return new_rm;
}

#define BITS64_FOR(it, set, cap) for (int it = bits64_first(set, cap); it >= 0; it = bits64_next(set, cap, it))
#define BITS64_FOR_ANDN(it, A, B, cap) for (int it = bits64_first_andn(A, B, cap); it >= 0; it = bits64_next_andn(A, B, cap, it))

static int bits64_next(uint64_t* arr, size_t cnt, int x) {
    // skip one ahead
    x += 1;

    // unpack coords
    size_t i = x / 64, j = x % 64;

    uint64_t word;
    for (;;) {
        // we're done
        if (i*64 >= cnt) { return -1; }
        // chop off the bottom bits which have been processed
        uint64_t mask = UINT64_MAX << j;
        word = arr[i] & mask;
        if (word != 0) {
            return i*64 + tb_ffs64(word) - 1;
        }
        i += 1, j = 0;
    }
}

static int bits64_first(uint64_t* arr, size_t cnt) {
    TB_ASSERT(cnt > 0);
    return arr[0] & 1 ? 0 : bits64_next(arr, cnt, 0);
}

static int bits64_next_andn(uint64_t* A, uint64_t* B, size_t cnt, int x) {
    // skip one ahead
    x += 1;

    // unpack coords
    size_t i = x / 64, j = x % 64;

    uint64_t word;
    for (;;) {
        // we're done
        if (i*64 >= cnt) { return -1; }
        // chop off the bottom bits which have been processed
        uint64_t mask = UINT64_MAX << j;
        word = (A[i] & ~B[i]) & mask;
        if (word != 0) {
            return i*64 + tb_ffs64(word) - 1;
        }
        i += 1, j = 0;
    }
}

static int bits64_first_andn(uint64_t* A, uint64_t* B, size_t cnt) {
    TB_ASSERT(cnt > 0);
    return (A[0] & ~B[0]) & 1 ? 0 : bits64_next_andn(A, B, cnt, 0);
}

static bool bits64_member(uint64_t* arr, size_t x) {
    return arr[x / 64] & (1ull << (x % 64));
}

