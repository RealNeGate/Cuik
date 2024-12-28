#pragma once
#include "../tb_internal.h"
#include <arena_array.h>
#include <limits.h>

enum {
    INT_WIDEN_LIMIT = 3,
    FAST_IDOM_LIMIT = 20
};

#if TB_PACKED_USERS
#define USERN(u) ((TB_Node*) ((u)->_n)) // node
#define USERI(u) ((int) ((u)->_slot))   // index
#else
#define USERN(u) ((u)->_n)    // node
#define USERI(u) ((u)->_slot) // index
#endif

#define FOR_USERS(u, n) for (TB_User *u = (n)->users, *_end_ = &u[(n)->user_count]; u != _end_; u++)

////////////////////////////////
// Constant prop
////////////////////////////////
typedef struct {
    int64_t min, max;
    // for known bit analysis
    uint64_t known_zeros, known_ones;
    // we really don't wanna widen 18 quintillion times, it's never worth it
    uint64_t widen;
} LatticeInt;

// Represents the fancier type system within the optimizer, it's
// all backed by my shitty understanding of lattice theory
struct Lattice {
    enum {
        LATTICE_BOT, // bot ^ x = bot
        LATTICE_TOP, // top ^ x = x

        LATTICE_INT,
        LATTICE_TUPLE,

        // float (each float type has it's own separate set of these btw):
        //
        //        top
        //       /   \
        //      /     \
        //     /       \
        //    /         \
        //   /|\       /|\
        //  / | \     / | \
        // N  N  N 0.0 1.5 ... # fltcon
        //  \ | /     \ | /
        //   \|/       \|/
        //   nan      ~nan
        //     \       /
        //      \     /
        //       \   /
        //        \ /
        //        flt
        //
        // N means NaN it's just too long to write in the diagram
        LATTICE_FLT32,    LATTICE_FLT64,    // bottom types for floats
        LATTICE_NAN32,    LATTICE_NAN64,
        LATTICE_XNAN32,   LATTICE_XNAN64,
        LATTICE_FLTCON32, LATTICE_FLTCON64, // _f32 and _f64

        // pointers:
        //    anyptr
        //     /   \
        //    /     \
        //   /     /|\
        //   |    / | \
        // null  a  b  ... # ptrcon
        //   |    \ | /
        //   \    ~null
        //    \   /
        //    allptr
        LATTICE_ALLPTR,
        LATTICE_ANYPTR,
        LATTICE_NULL,
        LATTICE_XNULL,
        LATTICE_PTRCON,

        // memory types
        LATTICE_MEMORY,

        // control tokens:
        //    top
        //     |
        //   dead
        //     |
        //   live
        //     |
        //    bot
        LATTICE_LIVE,
        LATTICE_DEAD,
    } tag;
    union {
        size_t _elem_count; // LATTICE_TUPLE
        LatticeInt _int;    // LATTICE_INT
        TB_Symbol* _ptr;    // LATTICE_PTRCON
        float  _f32;        // LATTICE_FLTCON32
        double _f64;        // LATTICE_FLTCON64
    };
    union {
        Lattice* elems[0];
    };
};

////////////////////////////////
// Cool properties
////////////////////////////////
uint32_t cfg_flags(TB_Node* n);
bool cfg_is_region(TB_Node* n);
bool cfg_is_natural_loop(TB_Node* n);
bool cfg_is_branch(TB_Node* n);
bool cfg_is_fork(TB_Node* n);
bool cfg_is_terminator(TB_Node* n);
bool cfg_is_endpoint(TB_Node* n);

bool tb_node_is_safepoint(TB_Node* n);
bool tb_node_has_mem_out(TB_Node* n);
TB_Node* tb_node_mem_in(TB_Node* n);

////////////////////////////////
// CFG
////////////////////////////////
typedef struct {
    TB_Node *phi, *n;
    int dst, src;
} PhiVal;

////////////////////////////////
// Core optimizer
////////////////////////////////
typedef struct {
    TB_Module* mod;
    NL_HashSet visited;

    size_t ws_cap;
    size_t ws_cnt;
    TB_Function** ws;
} IPOSolver;

static bool cant_signed_overflow(TB_Node* n) {
    return TB_NODE_GET_EXTRA_T(n, TB_NodeBinopInt)->ab & TB_ARITHMATIC_NSW;
}

static bool is_proj(TB_Node* n) {
    return n->type == TB_PROJ || n->type == TB_MACH_PROJ || n->type == TB_BRANCH_PROJ;
}

static uint64_t tb__mask(uint64_t bits) {
    return ~UINT64_C(0) >> (64 - bits);
}

static bool cfg_is_cproj(TB_Node* n) {
    return is_proj(n) && n->dt.type == TB_TAG_CONTROL;
}

static bool cfg_is_mproj(TB_Node* n) {
    return n->type == TB_PROJ && n->dt.type == TB_TAG_MEMORY;
}

// includes tuples which have control flow
static bool cfg_is_control(TB_Node* n) {
    if (n->dt.type == TB_TAG_CONTROL) { return true; }
    if (n->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, n) {
            if (cfg_is_cproj(USERN(u))) { return true; }
        }
    }
    return false;
}

static bool cfg_is_bb_entry(TB_Node* n) {
    if (cfg_is_region(n)) {
        return true;
    } else if (cfg_is_cproj(n) && (n->inputs[0]->type == TB_ROOT || cfg_is_fork(n->inputs[0]))) {
        // Start's control proj or a branch target
        return true;
    } else {
        return false;
    }
}

// returns a BranchProj's falsey proj, if it's an if-like TB_BRANCH
static TB_NodeBranchProj* cfg_if_branch(TB_Node* n) {
    size_t succ_count = 0;
    if (n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH) {
        TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
        succ_count = br->succ_count;
    } else if (cfg_is_branch(n)) {
        FOR_USERS(u, n) {
            if (USERN(u)->type == TB_BRANCH_PROJ) { succ_count++; }
        }
    } else {
        tb_todo();
    }

    if (succ_count != 2) { return NULL; }
    FOR_USERS(u, n) {
        if (USERN(u)->type == TB_BRANCH_PROJ) {
            TB_NodeBranchProj* proj = TB_NODE_GET_EXTRA(USERN(u));
            if (proj->index == 1) { return proj; }
        }
    }

    // shouldn't be reached wtf?
    return NULL;
}

static bool is_mem_out_op(TB_Node* n) {
    return n->dt.type == TB_TAG_MEMORY || (n->type >= TB_STORE && n->type <= TB_ATOMIC_CAS) || (n->type >= TB_CALL && n->type <= TB_TAILCALL) || n->type == TB_SPLITMEM || n->type == TB_MERGEMEM || n->type == TB_DEBUG_LOCATION;
}

static bool is_mem_end_op(TB_Node* n) {
    return n->type == TB_RETURN || n->type == TB_TRAP || n->type == TB_UNREACHABLE;
}

static bool is_mem_in_op(TB_Node* n) {
    return is_mem_out_op(n) || n->type == TB_SAFEPOINT || n->type == TB_LOAD;
}

static bool is_mem_only_in_op(TB_Node* n) {
    return n->type == TB_SAFEPOINT || n->type == TB_LOAD;
}

static bool single_use(TB_Node* n) {
    return n->user_count == 1;
}

static TB_User* get_single_use(TB_Node* n) {
    return n->user_count == 1 ? &n->users[0] : NULL;
}

static bool tb_node_is_pinned(TB_Node* n) {
    if ((n->type >= TB_ROOT && n->type <= TB_SAFEPOINT) || is_proj(n) || cfg_is_control(n)) {
        return true;
    }

    return cfg_flags(n) & NODE_PINNED;
}

////////////////////////////////
// CFG analysis
////////////////////////////////
// if we see a branch projection, it may either be a BB itself
// or if it enters a REGION directly, then that region is the BB.
static TB_Node* cfg_next_bb_after_cproj(TB_Node* proj) {
    return proj;
}

static TB_User* proj_with_index(TB_Node* n, int i) {
    FOR_USERS(u, n) if (is_proj(USERN(u))) {
        TB_NodeProj* p = TB_NODE_GET_EXTRA(USERN(u));
        if (p->index == i) { return u; }
    }

    return NULL;
}

static TB_User* cfg_next_user(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_control(USERN(u))) { return u; }
    }

    return NULL;
}

static bool cfg_has_phis(TB_Node* n) {
    if (!cfg_is_region(n)) { return false; }
    FOR_USERS(u, n) {
        if (USERN(u)->type == TB_PHI) { return true; }
    }
    return false;
}

static bool cfg_is_unreachable(TB_Node* n) {
    FOR_USERS(u, n) {
        if (USERN(u)->type == TB_UNREACHABLE) { return true; }
    }

    return false;
}

static TB_Node* cfg_next_control(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_control(USERN(u))) { return USERN(u); }
    }

    return NULL;
}

static TB_Node* cfg_get_pred(TB_CFG* cfg, TB_Node* n, int i) {
    n = n->inputs[i];
    for (;;) {
        ptrdiff_t search = nl_map_get(cfg->node_to_block, n);
        if (search >= 0 || n->type == TB_DEAD || cfg_is_region(n)) {
            return n;
        }

        n = n->inputs[0];
    }
}

static TB_BasicBlock* cfg_get_pred_bb(TB_CFG* cfg, TB_Node* n, int i) {
    n = n->inputs[i];
    for (;;) {
        ptrdiff_t search = nl_map_get(cfg->node_to_block, n);
        if (search >= 0) {
            return cfg->node_to_block[search].v;
        } else if (n->type == TB_DEAD || cfg_is_region(n)) {
            return NULL;
        }

        n = n->inputs[0];
    }
}

// shorthand because we use it a lot
static TB_Node* idom(TB_CFG* cfg, TB_Node* n) {
    TB_ASSERT(cfg->node_to_block == NULL);
    ptrdiff_t search = nl_map_get(cfg->node_to_block, n);
    if (search < 0) {
        return NULL;
    }

    TB_BasicBlock* dom = cfg->node_to_block[search].v->dom;
    return dom ? dom->start : NULL;
}

static int dom_depth(TB_CFG* cfg, TB_Node* n) {
    return nl_map_get_checked(cfg->node_to_block, n)->dom_depth;
}

static bool slow_dommy2(TB_BasicBlock* expected_dom, TB_BasicBlock* bb) {
    while (bb->dom_depth > expected_dom->dom_depth) {
        bb = bb->dom;
    }
    return bb == expected_dom;
}

static bool slow_dommy(TB_CFG* cfg, TB_Node* expected_dom, TB_Node* bb) {
    TB_BasicBlock* a = nl_map_get_checked(cfg->node_to_block, expected_dom);
    TB_BasicBlock* b = nl_map_get_checked(cfg->node_to_block, bb);
    return slow_dommy2(a, b);
}


////////////////////////////////
// Unordered SoN successor iterator
////////////////////////////////
#define FOR_SUCC(it, n) for (SuccIter it = succ_iter(n); succ_iter_next(&it);)

typedef struct {
    TB_Node* n;
    TB_Node* succ;
    int index; // -1 if we're not walking CProjs
} SuccIter;

static SuccIter succ_iter(TB_Node* n) {
    if (n->dt.type == TB_TAG_TUPLE) {
        return (SuccIter){ n, NULL, 0 };
    } else if (!cfg_is_endpoint(n)) {
        return (SuccIter){ n, NULL, -1 };
    } else {
        return (SuccIter){ n, NULL, n->user_count };
    }
}

static bool succ_iter_next(SuccIter* restrict it) {
    TB_Node* n = it->n;

    // not branching? ok pick single next control
    if (it->index == -1) {
        it->index = n->user_count; // terminate
        it->succ = cfg_next_control(n);
        return true;
    }

    // if we're in this loop, we know we're scanning for CProjs
    while (it->index < n->user_count) {
        TB_Node* un = USERN(&n->users[it->index++]);
        if (cfg_is_cproj(un)) {
            it->succ = un;
            return true;
        }
    }

    return false;
}

// lovely properties
bool cfg_is_region(TB_Node* n);
bool cfg_is_natural_loop(TB_Node* n);
bool cfg_is_terminator(TB_Node* n);
bool cfg_is_endpoint(TB_Node* n);

// internal debugging mostly
void tb_print_dumb_node(Lattice** types, TB_Node* n);

// computes basic blocks but also dominators and loop nests if necessary.
TB_CFG tb_compute_cfg(TB_Function* f, TB_Worklist* ws, TB_Arena* tmp_arena, bool dominators);
void tb_free_cfg(TB_CFG* cfg);

// TB_Worklist API
void worklist_alloc(TB_Worklist* restrict ws, size_t initial_cap);
void worklist_free(TB_Worklist* restrict ws);
void worklist_clear(TB_Worklist* restrict ws);
void worklist_clear_visited(TB_Worklist* restrict ws);
bool worklist_test(TB_Worklist* restrict ws, TB_Node* n);
bool worklist_test_n_set(TB_Worklist* restrict ws, TB_Node* n);
void worklist_push(TB_Worklist* restrict ws, TB_Node* restrict n);
int worklist_count(TB_Worklist* ws);
TB_Node* worklist_pop(TB_Worklist* ws);

void subsume_node(TB_Function* f, TB_Node* n, TB_Node* new_n);
void subsume_node2(TB_Function* f, TB_Node* n, TB_Node* new_n);
void subsume_node_without_phis(TB_Function* f, TB_Node* n, TB_Node* new_n);
void tb__gvn_remove(TB_Function* f, TB_Node* n);

// Scheduler's cost model crap (talk about these in codegen_impl.h)
typedef int (*TB_GetLatency)(TB_Function* f, TB_Node* n, TB_Node* end);
typedef uint64_t (*TB_GetUnitMask)(TB_Function* f, TB_Node* n);

// Local scheduler
void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count);
void tb_greedy_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb);
void tb_dataflow(TB_Function* f, TB_Arena* arena, TB_CFG cfg);

// Global scheduler
void tb_clear_anti_deps(TB_Function* f, TB_Worklist* ws);
void tb_renumber_nodes(TB_Function* f, TB_Worklist* ws);
void tb_compact_nodes(TB_Function* f, TB_Worklist* ws);
void tb_global_schedule(TB_Function* f, TB_Worklist* ws, TB_CFG cfg, bool early_only, bool dataflow, TB_GetLatency get_lat);
void tb_compute_synthetic_loop_freq(TB_Function* f, TB_CFG* cfg);

// BB placement
int bb_placement_rpo(TB_Arena* arena, TB_CFG* cfg, int* dst_order);
int bb_placement_trace(TB_Arena* arena, TB_CFG* cfg, int* dst_order);

// makes arch-friendly IR
void tb_opt_legalize(TB_Function* f, TB_Arch arch);
int tb_opt_peeps(TB_Function* f);
int tb_opt_locals(TB_Function* f);

// Integrated IR debugger
void tb_integrated_dbg(TB_Function* f, TB_Node* n);

Lattice* latuni_get(TB_Function* f, TB_Node* n);

void tb__print_regmask(RegMask* mask);

