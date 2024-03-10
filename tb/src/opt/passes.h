#pragma once
#include "../tb_internal.h"
#include "properties.h"

enum {
    FAST_IDOM_LIMIT = 20
};

#define TB_OPTDEBUG_STATS    0
#define TB_OPTDEBUG_PEEP     0
#define TB_OPTDEBUG_SCCP     0
#define TB_OPTDEBUG_LOOP     0
#define TB_OPTDEBUG_SROA     0
#define TB_OPTDEBUG_GCM      0
#define TB_OPTDEBUG_MEM2REG  0
#define TB_OPTDEBUG_CODEGEN  0
#define TB_OPTDEBUG_DATAFLOW 0
#define TB_OPTDEBUG_INLINE   0
#define TB_OPTDEBUG_REGALLOC 0
#define TB_OPTDEBUG_GVN      0
#define TB_OPTDEBUG_SCHEDULE 0

// for toggling ANSI colors
#define TB_OPTDEBUG_ANSI     1

#define TB_OPTDEBUG(cond) CONCAT(DO_IF_, CONCAT(TB_OPTDEBUG_, cond))

#define DO_IF(cond) CONCAT(DO_IF_, cond)
#define DO_IF_0(...)
#define DO_IF_1(...) __VA_ARGS__

#define BB_LOW_FREQ 1e-4

#define FOR_USERS(u, n) for (User* u = n->users; u; u = u->next)

////////////////////////////////
// Constant prop
////////////////////////////////
// TODO(NeGate): implement dual? from there i can do join with
// dual(dual(x) ^ dual(y)) = join(x, y)
typedef struct {
    uint64_t min, max;

    // for known bit analysis
    uint64_t known_zeros;
    uint64_t known_ones;
} LatticeInt;

// a simplification of the set of all pointers (or floats)
typedef struct {
    TB_Symbol* sym;
} LatticePtrConst;

typedef struct {
    size_t count;
} LatticeTuple;

typedef struct {
    int alias_idx;
} LatticeMem;

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
        // N  N  N 0.0 1.0 ... # fltcon
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
        //   |   a  b  ... # ptrcon
        //   |    \ | /
        // null   ~null
        //     \  /
        //    botptr
        LATTICE_BOTPTR,
        LATTICE_NULL,
        LATTICE_XNULL,
        LATTICE_PTRCON,

        // memory:
        //    top
        //   / | \
        //  a  b  ...
        //   \ | /
        //    bot
        //
        // alias idx on each memory type.
        LATTICE_MEM,

        // control tokens
        LATTICE_CTRL,
        LATTICE_XCTRL,
    } tag;
    uint32_t pad;
    union {
        LatticeInt _int;
        LatticePtrConst _ptr;
        LatticeTuple _tuple;
        LatticeMem _mem;
        float _f32;
        double _f64;
    };
    Lattice* elems[];
};

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

static uint64_t tb__mask(uint64_t bits) {
    return ~UINT64_C(0) >> (64 - bits);
}

static bool ctrl_out_as_cproj_but_not_branch(TB_Node* n) {
    return n->type == TB_CALL || n->type == TB_TAILCALL || n->type == TB_SYSCALL || n->type == TB_READ || n->type == TB_WRITE;
}

static bool cfg_is_mproj(TB_Node* n) {
    return n->type == TB_PROJ && n->dt.type == TB_MEMORY;
}

// includes tuples which have control flow
static bool cfg_is_control(TB_Node* n) {
    // easy case
    if (n->dt.type == TB_CONTROL) return true;

    if (n->dt.type == TB_TUPLE) {
        FOR_USERS(u, n) if (u->n->type == TB_PROJ && u->n->dt.type == TB_CONTROL) {
            return true;
        }
    }

    return false;
}

static bool cfg_is_bb_entry(TB_Node* n) {
    if (n->type == TB_REGION) {
        return true;
    } else if (n->type == TB_PROJ && (n->inputs[0]->type == TB_ROOT || n->inputs[0]->type == TB_BRANCH)) {
        // Start's control proj or a branch target
        return true;
    } else {
        return false;
    }
}

static bool cfg_underneath(TB_CFG* cfg, TB_Node* a, TB_BasicBlock* bb) {
    // follow until we hit a terminator
    for (;;) {
        a = a->inputs[0];

        ptrdiff_t search = nl_map_get(cfg->node_to_block, a);
        if (search >= 0) {
            return &cfg->node_to_block[search].v == bb;
        }
    }
}

static bool is_mem_out_op(TB_Node* n) {
    return n->dt.type == TB_MEMORY || (n->type >= TB_STORE && n->type <= TB_ATOMIC_CAS) || (n->type >= TB_CALL && n->type <= TB_TAILCALL) || n->type == TB_SPLITMEM || n->type == TB_MERGEMEM;
}

static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_ROOT && n->type <= TB_SAFEPOINT_POLL) || n->type == TB_PROJ;
}

static bool is_mem_in_op(TB_Node* n) {
    return is_mem_out_op(n) || n->type == TB_SAFEPOINT_POLL || n->type == TB_LOAD;
}

static bool is_mem_only_in_op(TB_Node* n) {
    return n->type == TB_SAFEPOINT_POLL || n->type == TB_LOAD;
}

static bool cfg_critical_edge(TB_Node* proj, TB_Node* n) {
    assert(proj->type == TB_PROJ);

    // multi-user proj, this means it's basically a BB
    if (proj->users->next != NULL || proj->users->n->type != TB_REGION) {
        return true;
    }

    assert(n->type == TB_BRANCH);
    TB_Node* r = proj->users->n;
    if (r->type == TB_REGION) {
        FOR_USERS(u, r) {
            if (u->n->type == TB_PHI && u->n->dt.type != TB_MEMORY) {
                return true;
            }
        }
    }

    return false;
}

////////////////////////////////
// CFG analysis
////////////////////////////////
// if we see a branch projection, it may either be a BB itself
// or if it enters a REGION directly, then that region is the BB.
static TB_Node* cfg_next_bb_after_cproj(TB_Node* n) {
    assert(n->type == TB_PROJ && n->inputs[0]->type == TB_BRANCH);
    if (!cfg_critical_edge(n, n->inputs[0])) {
        return n->users->n;
    } else {
        return n;
    }
}

static TB_Node* cfg_next_region_control(TB_Node* n) {
    if (n->type != TB_REGION) {
        FOR_USERS(u, n) {
            if (u->n->type == TB_REGION && u->n->input_count == 1) {
                return u->n;
            }
        }
    }

    return n;
}

static User* proj_with_index(TB_Node* n, int i) {
    FOR_USERS(u, n) {
        TB_NodeProj* p = TB_NODE_GET_EXTRA(u->n);
        if (p->index == i) {
            return u;
        }
    }

    return NULL;
}

static User* cfg_next_user(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_control(u->n)) {
            return u;
        }
    }

    return NULL;
}

static bool cfg_basically_empty_only_mem_phis(TB_Node* n) {
    if (n->type == TB_PROJ && n->users->next == NULL && n->users->n->type == TB_REGION) {
        FOR_USERS(u, n) {
            if (u->n->type == TB_PHI && u->n->dt.type != TB_MEMORY) {
                return false;
            }
        }

        return true;
    }

    return false;
}

static bool cfg_has_phis(TB_Node* n) {
    if (n->type != TB_REGION) {
        return false;
    }

    FOR_USERS(u, n) {
        if (u->n->type == TB_PHI) {
            return true;
        }
    }

    return false;
}

static bool cfg_is_unreachable(TB_Node* n) {
    FOR_USERS(u, n) {
        if (u->n->type == TB_UNREACHABLE) {
            return true;
        }
    }

    return false;
}

static TB_Node* cfg_next_control0(TB_Node* n) {
    FOR_USERS(u, n) {
        if (u->slot == 0 && cfg_is_control(u->n)) {
            return u->n;
        }
    }

    return NULL;
}

static TB_Node* cfg_next_control(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_control(u->n)) {
            return u->n;
        }
    }

    return NULL;
}

static TB_Node* get_pred(TB_Node* n, int i) {
    TB_Node* base = n;
    n = n->inputs[i];

    if (base->type == TB_REGION && n->type == TB_PROJ) {
        TB_Node* parent = n->inputs[0];

        // start or cprojs with multiple users (it's a BB) will just exit
        if (parent->type == TB_ROOT || (!ctrl_out_as_cproj_but_not_branch(parent) && n->users->next != NULL)) {
            return n;
        }
        n = parent;
    }

    while (!cfg_is_bb_entry(n)) {
        n = n->inputs[0];
    }

    return n;
}

static TB_Node* cfg_get_pred(TB_CFG* cfg, TB_Node* n, int i) {
    n = n->inputs[i];
    for (;;) {
        ptrdiff_t search = nl_map_get(cfg->node_to_block, n);
        if (search >= 0 || n->type == TB_REGION) {
            return n;
        }

        n = n->inputs[0];
    }
}

static TB_Node* next_control(TB_Node* n) {
    // unless it's a branch (aka a terminator), it'll have one successor
    TB_Node* next = NULL;
    FOR_USERS(u, n) {
        TB_Node* succ = u->n;

        // we can't treat regions in the chain
        if (succ->type == TB_REGION) break;

        // we've found the next step in control flow
        if (cfg_is_control(succ)) {
            return succ;
        }
    }

    return NULL;
}

static TB_Node* get_block_begin(TB_Node* n) {
    while (!cfg_is_bb_entry(n)) {
        n = n->inputs[0];
    }
    return n;
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

// internal debugging mostly
void tb_print_dumb_node(Lattice** types, TB_Node* n);

// pushes RPO walk into worklist items, also modifies the visited set.
TB_CFG tb_compute_rpo(TB_Function* f, Worklist* ws);
void tb_free_cfg(TB_CFG* cfg);
//   postorder walk -> dominators
void tb_compute_dominators(TB_Function* f, Worklist* ws, TB_CFG cfg);

// Worklist API
void worklist_alloc(Worklist* restrict ws, size_t initial_cap);
void worklist_free(Worklist* restrict ws);
void worklist_clear(Worklist* restrict ws);
void worklist_clear_visited(Worklist* restrict ws);
bool worklist_test(Worklist* restrict ws, TB_Node* n);
bool worklist_test_n_set(Worklist* restrict ws, TB_Node* n);
void worklist_push(Worklist* restrict ws, TB_Node* restrict n);
int worklist_count(Worklist* ws);
TB_Node* worklist_pop(Worklist* ws);

void subsume_node(TB_Function* f, TB_Node* n, TB_Node* new_n);
void subsume_node2(TB_Function* f, TB_Node* n, TB_Node* new_n);
void tb__gvn_remove(TB_Function* f, TB_Node* n);

// node latency
typedef int (*TB_GetLatency)(TB_Function* f, TB_Node* n);

// Local scheduler
void tb_list_scheduler(TB_Function* f, TB_CFG* cfg, Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb, TB_GetLatency get_lat);
void tb_greedy_scheduler(TB_Function* f, TB_CFG* cfg, Worklist* ws, DynArray(PhiVal*) phi_vals, TB_BasicBlock* bb);

// Global scheduler
void tb_global_schedule(TB_Function* f, TB_CFG cfg, bool renumber, bool dataflow, TB_GetLatency get_lat);

// makes arch-friendly IR
void tb_opt_legalize(TB_Function* f, TB_Arch arch);
void tb_opt_peeps(TB_Function* f);
void tb_opt_loops(TB_Function* f);
void tb_opt_locals(TB_Function* f);

Lattice* latuni_get(TB_Function* f, TB_Node* n);
