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
        //      top
        //     /   \
        //    /     \
        //   /     /|\
        //   |    / | \
        // null  a  b  ... # ptrcon
        //   |    \ | /
        //   \    ~null
        //    \   /
        //     ptr
        LATTICE_ALLPTR,
        LATTICE_ANYPTR,
        LATTICE_NULL,
        LATTICE_XNULL,
        LATTICE_PTRCON,

        // memory types
        LATTICE_ALLMEM,    // bottom type for memory
        LATTICE_ANYMEM,    // top type for memory
        LATTICE_MEM_SLICE, // some set of bits where 1 means aliased.

        // control tokens:
        //    top
        //     |
        //   ctrl
        //     |
        //    bot
        LATTICE_CTRL,
    } tag;
    union {
        size_t _alias_n;    // LATTICE_MEM_SLICE
        size_t _elem_count; // LATTICE_TUPLE
        LatticeInt _int;    // LATTICE_INT
        TB_Symbol* _ptr;    // LATTICE_PTRCON
        float  _f32;        // LATTICE_FLTCON32
        double _f64;        // LATTICE_FLTCON64
    };
    union {
        uint64_t alias[0];
        Lattice* elems[0];
    };
};

////////////////////////////////
// Cool properties
////////////////////////////////
bool cfg_is_region(TB_Node* n);
bool cfg_is_natural_loop(TB_Node* n);
bool cfg_is_fork(TB_Node* n);
bool cfg_is_terminator(TB_Node* n);
bool cfg_is_endpoint(TB_Node* n);

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
    if (n->type == TB_REGION) {
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
    assert(n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH);
    TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
    if (br->succ_count != 2) { return NULL; }

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
    return n->dt.type == TB_TAG_MEMORY || (n->type >= TB_STORE && n->type <= TB_ATOMIC_CAS) || (n->type >= TB_CALL && n->type <= TB_TAILCALL) || n->type == TB_SPLITMEM || n->type == TB_MERGEMEM;
}

static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_ROOT && n->type <= TB_SAFEPOINT_POLL) || is_proj(n);
}

static bool is_mem_end_op(TB_Node* n) {
    return n->type == TB_RETURN || n->type == TB_TRAP || n->type == TB_UNREACHABLE;
}

static bool is_mem_in_op(TB_Node* n) {
    return is_mem_out_op(n) || n->type == TB_SAFEPOINT_POLL || n->type == TB_LOAD;
}

static bool is_mem_only_in_op(TB_Node* n) {
    return n->type == TB_SAFEPOINT_POLL || n->type == TB_LOAD;
}

static bool single_use(TB_Node* n) {
    return n->user_count == 1;
}

////////////////////////////////
// CFG analysis
////////////////////////////////
static bool cfg_has_non_mem_phis(TB_Node* n) {
    if (!cfg_is_region(n)) { return false; }
    FOR_USERS(u, n) {
        if (USERN(u)->type == TB_PHI && USERN(u)->dt.type != TB_TAG_MEMORY) {
            return true;
        }
    }
    return false;
}

// if we see a branch projection, it may either be a BB itself
// or if it enters a REGION directly, then that region is the BB.
static TB_Node* cfg_next_bb_after_cproj(TB_Node* proj) {
    assert(cfg_is_cproj(proj) && cfg_is_fork(proj->inputs[0]));
    TB_Node* n = proj->inputs[0];

    assert(proj->user_count >= 1 && "missing successor after cproj");
    TB_Node* r = USERN(proj->users);
    if (!single_use(proj) || !cfg_is_region(r)) {
        // multi-user proj, this means it's basically a BB
        return proj;
    }

    int blocks_with_phis = 0;
    FOR_USERS(u, n) {
        TB_Node* path = USERN(u);
        if (cfg_is_cproj(path) && single_use(path)) {
            TB_Node* next = USERN(path->users);
            if (cfg_has_non_mem_phis(next)) {
                blocks_with_phis++;
            }
        }
    }

    if (blocks_with_phis > 1) {
        FOR_USERS(u, r) {
            if (USERN(u)->type == TB_PHI && USERN(u)->dt.type != TB_TAG_MEMORY) {
                return proj;
            }
        }
    }

    return r;
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

// shorthand because we use it a lot
static TB_Node* idom(TB_CFG* cfg, TB_Node* n) {
    if (cfg->node_to_block == NULL) return NULL;

    ptrdiff_t search = nl_map_get(cfg->node_to_block, n);
    if (search < 0) {
        return NULL;
    }

    TB_BasicBlock* dom = cfg->node_to_block[search].v.dom;
    return dom ? dom->start : NULL;
}

static int dom_depth(TB_CFG* cfg, TB_Node* n) {
    return nl_map_get_checked(cfg->node_to_block, n).dom_depth;
}

static bool slow_dommy2(TB_BasicBlock* expected_dom, TB_BasicBlock* bb) {
    while (bb != NULL && expected_dom != bb) {
        TB_BasicBlock* new_bb = bb->dom;
        if (new_bb == NULL || new_bb == bb) {
            return false;
        }
        bb = new_bb;
    }

    return true;
}

static bool slow_dommy(TB_CFG* cfg, TB_Node* expected_dom, TB_Node* bb) {
    while (bb != NULL && expected_dom != bb) {
        TB_Node* new_bb = idom(cfg, bb);
        if (new_bb == NULL || new_bb == bb) {
            return false;
        }
        bb = new_bb;
    }

    return true;
}

// lovely properties
bool cfg_is_region(TB_Node* n);
bool cfg_is_natural_loop(TB_Node* n);
bool cfg_is_terminator(TB_Node* n);
bool cfg_is_endpoint(TB_Node* n);

// internal debugging mostly
void tb_print_dumb_node(Lattice** types, TB_Node* n);

// pushes RPO walk into worklist items, also modifies the visited set.
TB_CFG tb_compute_rpo(TB_Function* f, TB_Worklist* ws);
void tb_free_cfg(TB_CFG* cfg);
//   postorder walk -> dominators
void tb_compute_dominators(TB_Function* f, TB_Worklist* ws, TB_CFG cfg);

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
void tb__gvn_remove(TB_Function* f, TB_Node* n);

// Scheduler's cost model crap (talk about these in codegen_impl.h)
typedef int (*TB_GetLatency)(TB_Function* f, TB_Node* n, TB_Node* end);
typedef uint64_t (*TB_GetUnitMask)(TB_Function* f, TB_Node* n);

// Local scheduler
void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat, TB_GetUnitMask get_unit_mask, int unit_count);
void tb_greedy_scheduler(TB_Function* f, TB_CFG* cfg, TB_Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb);
void tb_dataflow(TB_Function* f, TB_Arena* arena, TB_CFG cfg, TB_Node** rpo_nodes);

// Global scheduler
void tb_renumber_nodes(TB_Function* f, TB_Worklist* ws);
void tb_global_schedule(TB_Function* f, TB_Worklist* ws, TB_CFG cfg, bool loop_nests, bool dataflow, TB_GetLatency get_lat);

// makes arch-friendly IR
void tb_opt_legalize(TB_Function* f, TB_Arch arch);
void tb_opt_build_loop_tree(TB_Function* f);
void tb_opt_loops(TB_Function* f);

int tb_opt_peeps(TB_Function* f);
int tb_opt_locals(TB_Function* f);

Lattice* latuni_get(TB_Function* f, TB_Node* n);

