// Let's just explain the architecture of the optimizer here.
//
// # Peephole optimizations
//   These are the kind which work locally like 2+2=4 and in TB's design they're
//   performed incrementally which means that certain mutations must go through
//   functions to guarentee they update correctly. Let's go over those:
//
//   set_input(f, n, in, slot)
//     basically `n->inputs[slot] = in` except it correctly updates the user set
//
// # How to implement peepholes
//     TODO
//
#include "passes.h"
#include <log.h>
#include <math.h>

#include "../sandbird/sandbird.h"

// helps us do some matching later
static void remove_user(TB_Function* f, TB_Node* n, int slot);
static void remove_input(TB_Function* f, TB_Node* n, size_t i);
static bool alloc_types(TB_Function* f);

static bool can_gvn(TB_Node* n);
static void print_lattice(Lattice* l);
static bool is_dead_ctrl(TB_Function* f, TB_Node* n);

static Lattice* value_of(TB_Function* f, TB_Node* n);
static TB_Node* try_as_const(TB_Function* f, TB_Node* n, Lattice* l);
static Lattice* affine_iv(TB_Function* f, Lattice* init, int64_t trips_min, int64_t trips_max, int64_t step, int bits);

// node creation helpers
TB_Node* make_poison(TB_Function* f, TB_DataType dt);
TB_Node* dead_node(TB_Function* f);
TB_Node* make_f64_node(TB_Function* f, double x);
TB_Node* make_proj_node(TB_Function* f, TB_DataType dt, TB_Node* src, int i);
TB_Node* make_int_binop(TB_Function* f, TB_NodeTypeEnum type, TB_Node* lhs, TB_Node* rhs);
TB_Node* make_ptr_offset(TB_Function* f, TB_Node* lhs, TB_Node* rhs);
TB_Node* make_int_unary(TB_Function* f, TB_DataType dt, TB_NodeTypeEnum type, TB_Node* src);

// for random debugging stuff
static mtx_t aaa;
_Atomic(uint64_t) tb_stats[STATS_MAX];

static const char* stats_get_name(int type) {
    switch (type) {
        #define X(name, desc) case STATS_ ## name: return desc;
        #include "stats.h"

        default: return "???";
    }
}

static void node_resize_inputs(TB_Function* f, TB_Node* n, size_t cnt) {
    if (cnt >= n->input_cap) {
        size_t new_cap = tb_next_pow2(cnt + 1);
        if (new_cap >= UINT16_MAX) {
            tb_panic("Too many inputs for one node");
        }

        TB_Node** new_inputs = tb_arena_alloc(&f->arena, new_cap * sizeof(TB_Node*));
        if (n->inputs != NULL) {
            memcpy(new_inputs, n->inputs, cnt * sizeof(TB_Node*));
        }

        // because extra edges go here, we need to make sure it's actually clear
        FOR_N(i, cnt, new_cap) {
            new_inputs[i] = NULL;
        }

        n->inputs = new_inputs;
        n->input_cap = new_cap;
    }
}

// this has to move things which is not nice...
void add_input_late(TB_Function* f, TB_Node* n, TB_Node* in) {
    // if there's an extra dep, we gotta shuffle it away. since
    // the extra deps are unique i'll just move it wherever
    TB_Node* old = n->input_count < n->input_cap ? n->inputs[n->input_count] : NULL;
    if (old != NULL) {
        remove_user(f, n, n->input_count);
        n->inputs[n->input_count] = in;

        tb_node_add_extra(f, n, old);
    } else {
        node_resize_inputs(f, n, n->input_count);
        n->inputs[n->input_count] = in;
    }

    if (in) {
        add_user(f, n, in, n->input_count);
    }
    n->input_count += 1;
}

void tb_node_add_extra(TB_Function* f, TB_Node* n, TB_Node* in) {
    FOR_N(i, n->input_count, n->input_cap) {
        if (n->inputs[i] == NULL) {
            n->inputs[i] = in;
            add_user(f, n, in, i);
            return;
        } else if (n->inputs[i] == in) {
            return;
        }
    }

    size_t i = n->input_cap;
    node_resize_inputs(f, n, n->input_cap);

    TB_ASSERT(i < n->input_cap);
    n->inputs[i] = in;
    add_user(f, n, in, i);
}

void tb_node_clear_extras(TB_Function* f, TB_Node* n) {
    FOR_N(i, n->input_count, n->input_cap) {
        remove_user(f, n, i);
        n->inputs[i] = NULL;
    }
}

static bool is_empty_bb(TB_Function* f, TB_Node* end) {
    TB_ASSERT(cfg_is_terminator(end));
    if (!cfg_is_bb_entry(end->inputs[0])) {
        return false;
    }

    TB_Node* bb = end->inputs[0];
    FOR_USERS(u, bb) {
        if (USERN(u) != end) { return false; }
    }

    return true;
}

static TB_Node* next_mem_user(TB_Node* n) {
    FOR_USERS(u, n) {
        if (cfg_is_mproj(USERN(u)) || tb_node_has_mem_out(USERN(u)) || (USERN(u)->type == TB_PHI && USERN(u)->dt.type == TB_TAG_MEMORY)) {
            return USERN(u);
        }
    }

    return NULL;
}

float tb_edge_prob(TB_Node* n) {
    TB_ASSERT(!cfg_is_endpoint(n));

    // not even a branch? then it's getting 100%
    if (!cfg_is_cproj(n)) {
        return 1.0f;
    }

    TB_Node* tup = n->inputs[0];
    int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;

    if (cfg_is_if(tup)) {
        TB_NodeIf* br = TB_NODE_GET_EXTRA(tup);
        return index ? 1.0 - br->prob : br->prob;
    } else if (cfg_is_branch(tup)) {
        uint64_t total = TB_NODE_GET_EXTRA_T(tup, TB_NodeBranch)->total_hits;
        uint64_t hits  = TB_NODE_GET_EXTRA_T(n, TB_NodeBranchProj)->taken;
        return (float)hits / (float)total;
    } else if (n->type >= 0x100) {
        int family = n->type / 0x100;
        TB_ASSERT(family >= 1 && family < TB_ARCH_MAX);
        return tb_codegen_families[family].edge_prob(n);
    } else {
        int succ_count = 0;
        FOR_USERS(u, tup) {
            succ_count += cfg_is_cproj(USERN(u));
        }
        return 1.0f / succ_count;
    }
}

// incremental dominators, plays nice with peepholes and has
// a limited walk of 20 steps.
static TB_Node* fast_idom(TB_Node* bb) {
    int steps = 0;

    // note that "subtypes" of region like TB_NATURAL_LOOP and TB_AFFINE_LOOP are
    // valid for fast doms since they guarentee they're dominated by inputs[0]
    while (steps < FAST_IDOM_LIMIT && bb->type != TB_REGION && bb->type != TB_ROOT) {
        bb = bb->inputs[0];
        steps++;
    }

    return bb;
}

static bool fast_dommy(TB_Node* expected_dom, TB_Node* bb) {
    int steps = 0;

    // note that "subtypes" of region like TB_NATURAL_LOOP and TB_AFFINE_LOOP are
    // valid for fast doms since they guarentee they're dominated by inputs[0]
    while (steps < FAST_IDOM_LIMIT && bb != expected_dom && bb->type != TB_REGION && bb->type != TB_ROOT) {
        TB_ASSERT(bb->inputs[0]);
        bb = bb->inputs[0];
        steps++;
    }

    return bb == expected_dom;
}

static void mark_node(TB_Function* f, TB_Node* n) {
    worklist_push(f->worklist, n);
}

static void mark_users_raw(TB_Function* f, TB_Node* n) {
    FOR_USERS(u, n) { mark_node(f, USERN(u)); }
}

static void mark_users(TB_Function* f, TB_Node* n) {
    FOR_USERS(u, n) {
        TB_NodeTypeEnum type = USERN(u)->type;
        if (type == TB_CALLGRAPH) {
            continue;
        }

        worklist_push(f->worklist, USERN(u));

        // tuples changing means their projections did too.
        if (type == TB_PROJ || type == TB_PTR_OFFSET) {
            mark_users_raw(f, USERN(u));
        }

        // (br (cmp a b)) => ...
        // (or (shl a 24) (shr a 40)) => ...
        // (trunc (mul a b)) => ...
        // (phi ...) => ... (usually converting into branchless ops)
        if ((type >= TB_CMP_EQ && type <= TB_CMP_FLE) ||
            type == TB_SHL || type == TB_SHR || type == TB_ADD || type == TB_MUL ||
            type == TB_STORE || type == TB_PHI) {
            mark_users_raw(f, USERN(u));
        }
    }
}

static void mark_node_n_users(TB_Function* f, TB_Node* n) {
    worklist_push(f->worklist, n);
    mark_users(f, n);
}

// unity build with all the passes
#include "worklist.h"
#include "properties.h"
#include "lattice.h"
#include "cfg.h"
#include "gvn.h"
#include "fold.h"

// Standard local rewrites
#include "peep_int.h"
#include "peep_cmp.h"
#include "peep_mem.h"
#include "peep_float.h"

#include "mem_opt.h"
#include "sroa.h"
#include "print.h"
#include "print_dumb.h"
#include "serialize.h"
#include "loop.h"
#include "slp.h"
#include "branches.h"
#include "verify.h"
#include "print_svg.h"
#include "compact.h"
#include "gcm.h"
#include "libcalls.h"
#include "mem.h"
#include "rpo_sched.h"
#include "list_sched.h"
#include "bb_placement.h"
#include "interp.h"
#include "dbg_server.h"
#include "dbg.h"

static bool is_dead_ctrl(TB_Function* f, TB_Node* n) {
    Lattice* l = latuni_get(f, n);
    return l == &TOP_IN_THE_SKY || l == &DEAD_IN_THE_SKY;
}

void tb__gvn_remove(TB_Function* f, TB_Node* n) {
    if (can_gvn(n)) {
        nl_hashset_remove2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
    }
}

void tb_violent_kill(TB_Function* f, TB_Node* n) {
    // remove from GVN if we're murdering it
    size_t extra = extra_bytes(n);
    nl_hashset_remove2(&f->gvn_nodes, n, gvn_hash, gvn_compare);

    FOR_N(i, 0, n->input_count) {
        remove_user(f, n, i);
        n->inputs[i] = NULL;
    }

    TB_Arena* arena = &f->arena;
    tb_arena_free(arena, n->users, n->user_cap * sizeof(TB_User));
    tb_arena_free(arena, n->inputs, n->input_cap * sizeof(TB_Node*));
    tb_arena_free(arena, n, sizeof(TB_Node) + extra);

    TB_ASSERT(n->user_count == 0);
    n->user_cap = n->user_count = 0;
    n->users = NULL;
    n->input_count = 0;
    n->type = TB_NULL;
}

static Lattice* value_f32(TB_Function* f, TB_Node* n) {
    TB_ASSERT(n->type == TB_F32CONST);
    TB_NodeFloat32* num = TB_NODE_GET_EXTRA(n);
    return lattice_intern(f, (Lattice){ LATTICE_FLTCON32, ._f32 = num->value });
}

static Lattice* value_f64(TB_Function* f, TB_Node* n) {
    TB_ASSERT(n->type == TB_F64CONST);
    TB_NodeFloat64* num = TB_NODE_GET_EXTRA(n);
    return lattice_intern(f, (Lattice){ LATTICE_FLTCON64, ._f64 = num->value });
}

static Lattice* value_int(TB_Function* f, TB_Node* n) {
    TB_ASSERT(n->type == TB_ICONST);
    TB_NodeInt* num = TB_NODE_GET_EXTRA(n);
    if (n->dt.type == TB_TAG_PTR) {
        return num->value ? &XNULL_IN_THE_SKY : &NULL_IN_THE_SKY;
    } else {
        int64_t x = tb__sxt(num->value, tb_data_type_bit_size(f->super.module, n->dt.type), 64);
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { x, x, ~x, x } });
    }
}

static Lattice* value_root(TB_Function* f, TB_Node* n) {
    // if we reach this point, it means we're somehow processing a function which has no
    /* if (f->super.module->during_ipsccp && f->ipsccp_args == NULL) {
        return &TOP_IN_THE_SKY;
    } */

    return f->ipsccp_args;
}

static Lattice* value_proj(TB_Function* f, TB_Node* n) {
    TB_ASSERT(is_proj(n));
    Lattice* l = latuni_get(f, n->inputs[0]);
    if (l == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    } else if (l == &BOT_IN_THE_SKY) {
        return lattice_from_dt(f, n->dt);
    } else {
        TB_ASSERT(l->tag == LATTICE_TUPLE);
        int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
        return l->elems[index];
    }
}

static Lattice* value_dead(TB_Function* f, TB_Node* n) {
    return &DEAD_IN_THE_SKY;
}

static Lattice* value_ctrl(TB_Function* f, TB_Node* n) {
    return latuni_get(f, n->inputs[0]);
}

static Lattice* value_ptr_vals(TB_Function* f, TB_Node* n) {
    if (n->type == TB_LOCAL) {
        return &XNULL_IN_THE_SKY;
    } else {
        TB_ASSERT(n->type == TB_SYMBOL);
        return lattice_intern(f, (Lattice){ LATTICE_PTRCON, ._ptr = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym });
    }
}

/* static Lattice* value_lookup(TB_Function* f, TB_Node* n) {
    TB_NodeLookup* l = TB_NODE_GET_EXTRA(n);
    TB_DataType dt = n->dt;
    TB_ASSERT(TB_IS_INTEGER_TYPE(dt));

    LatticeInt a = { l->entries[0].val, l->entries[0].val, l->entries[0].val, ~l->entries[0].val };
    FOR_N(i, 1, n->input_count) {
        TB_LookupEntry* e = &l->entries[i];
        a.min = TB_MIN(a.min, l->entries[i].val);
        a.max = TB_MAX(a.max, l->entries[i].val);
        a.known_zeros &=  l->entries[i].val;
        a.known_ones  &= ~l->entries[i].val;
    }

    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = a });
} */

static Lattice* value_region(TB_Function* f, TB_Node* n) {
    TB_ASSERT(cfg_is_region(n));

    FOR_N(i, 0, n->input_count) {
        Lattice* edge = latuni_get(f, n->inputs[i]);
        if (edge == &LIVE_IN_THE_SKY) { return &LIVE_IN_THE_SKY; }
    }

    return &DEAD_IN_THE_SKY;
}

static Lattice* affine_iv(TB_Function* f, Lattice* init, int64_t trips_min, int64_t trips_max, int64_t step, int bits) {
    int64_t max;
    if (mul_overflow(trips_max, step, bits, &max)) { return NULL; }
    if (add_overflow(max, init->_int.max, bits, &max)) { return NULL; }

    // min can't overflow if max overflows since min < max
    int64_t min = (uint64_t)init->_int.min + (uint64_t) (((uint64_t) trips_min-1)*step);
    if (min >= lattice_int_min(bits) && max <= lattice_int_max(bits)) {
        if (step > 0) {
            if (min <= max) { return lattice_gimme_int(f, min, max, bits); }
        } else if (step > 0) {
            if (min >= max) { return lattice_gimme_int(f, max, min, bits); }
        }
    }
    return NULL;
}

static TB_Node* ideal_location(TB_Function* f, TB_Node* n) {
    TB_ASSERT(n->type == TB_DEBUG_LOCATION);
    if (n->user_count == 0) { return NULL; }

    if (n->inputs[0]->type == TB_PROJ && n->inputs[0]->inputs[0]->type == TB_DEBUG_LOCATION) {
        TB_Node* cproj = USERN(proj_with_index(n, 0));
        TB_Node* mproj = USERN(proj_with_index(n, 1));

        set_input(f, cproj, NULL, 0);
        set_input(f, mproj, NULL, 0);

        mark_users(f, cproj);
        mark_users(f, mproj);

        subsume_node(f, cproj, n->inputs[0]);
        subsume_node(f, mproj, n->inputs[1]);
        return n;
    }
    return NULL;
}

static Lattice* value_phi(TB_Function* f, TB_Node* n) {
    // wait for region to check first
    TB_Node* r = n->inputs[0];
    if (latuni_get(f, r) != &LIVE_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    Lattice* old = latuni_get(f, n);
    if (r->type == TB_AFFINE_LOOP) {
        TB_Node* latch = affine_loop_latch(r);
        if (latch && (TB_IS_INTEGER_TYPE(n->dt) || TB_IS_FLOAT_TYPE(n->dt))) {
            // we wanna know loop bounds
            uint64_t trips_min = 1, trips_max = UINT64_MAX;
            uint64_t* step_ptr = find_affine_indvar(n, r);
            Lattice* end = NULL;
            if (step_ptr) {
                int bits = tb_data_type_bit_size(f->super.module, n->dt.type);
                int64_t step = tb__sxt(*step_ptr, bits, 64);

                TB_InductionVar var;
                if (find_latch_indvar(r, latch, &var)) {
                    Lattice* init = latuni_get(f, var.phi->inputs[1]);
                    end = var.end_cond ? latuni_get(f, var.end_cond) : lattice_int_const(f, var.end_const);

                    if (init->tag == LATTICE_INT && end->tag == LATTICE_INT) {
                        Lattice* range = value_arith_raw(f, TB_SUB, n->dt, end, init, false, false);
                        if (var.step > 0 && range->_int.max > 0) {
                            int64_t pad = var.step - (var.pred == IND_SLT || var.pred == IND_ULT ? 1 : 0);
                            int64_t trips = (range->_int.max + pad) / var.step;
                            int64_t rem   = range->_int.max % var.step;
                            if (rem == 0 || var.pred != IND_NE) {
                                trips_max = trips;
                            }
                        }
                    }

                    if (var.phi != n) { end = NULL; }
                } else {
                    // affine loop missing latch? ok then it's a single trip
                    trips_min = trips_max = 1;
                }
                TB_ASSERT(trips_min <= trips_max);

                if (TB_IS_FLOAT_TYPE(n->dt)) {
                    // is the float representing integer values?
                    //   IEEE-754 singles can fit 24bit ints, doubles can be 53bit ints.
                    // __debugbreak();
                } else {
                    Lattice* init = latuni_get(f, n->inputs[1]);
                    if (init == &TOP_IN_THE_SKY) {
                        return &TOP_IN_THE_SKY;
                    }

                    if (init->tag == LATTICE_INT && trips_max <= INT64_MAX) {
                        Lattice* range = affine_iv(f, init, trips_min, trips_max, step, bits);
                        if (range) { return lattice_int_widen(f, range, old); }
                    }

                    if (step > 0 && cant_signed_overflow(n->inputs[2])) {
                        // pretty common that iterators won't overflow, thus never goes below init
                        int64_t min = init->_int.min;
                        int64_t max = end ? end->_int.max : lattice_int_max(bits);

                        // JOIN would achieve this effect too btw
                        if (old == &TOP_IN_THE_SKY) {
                            return lattice_int_widen(f, lattice_gimme_int(f, min, max, bits), old);
                        } else {
                            min = TB_MAX(min, old->_int.min);
                            max = TB_MIN(max, old->_int.max);

                            #if 0
                            if (init->_int.known_zeros) {
                                tb_print(f);
                                __debugbreak();
                            }

                            // if the bottom bits are all zeros in the init & step, we
                            // want to make sure the phi knows that.
                            uint64_t step_zeros = *step_ptr;
                            uint64_t diff = ~(init->_int.known_zeros ^ step_zeros);
                            int lsb_diff = __builtin_ffsll(diff) - 1;
                            uint64_t zeros = lsb_diff ? UINT64_MAX >> (64 - lsb_diff) : 0;
                            #endif

                            return lattice_int_widen(f, lattice_gimme_int2(f, min, max, 0, 0, bits), old);
                        }
                    }
                }
            }
        }
    } else if (r->type == TB_NATURAL_LOOP) {
        /* if (TB_IS_FLOAT_TYPE(n->dt) && (n->inputs[2]->type == TB_FADD || n->inputs[2]->type == TB_FSUB)) {
            Lattice* init = latuni_get(f, n->inputs[1]);
            Lattice* step = latuni_get(f, n->inputs[2]->inputs[2]);

            __debugbreak();
        } */
    }

    Lattice* l = &TOP_IN_THE_SKY;
    FOR_N(i, 1, n->input_count) {
        Lattice* ctrl = latuni_get(f, r->inputs[i - 1]);
        if (ctrl == &LIVE_IN_THE_SKY) {
            Lattice* edge = latuni_get(f, n->inputs[i]);
            l = lattice_meet(f, l, edge);
        }
    }

    // we wanna preserve widening count regardless (on pessimistic stuff
    // it would never widen so it'll stay as 0, on optimistic stuff old values
    // might have a widening that the meet of the phi doesn't include, we account
    // for that here)
    if (l->tag == LATTICE_INT && old->tag == LATTICE_INT) {
        if (l->_int.widen < old->_int.widen) {
            Lattice new_l = *l;
            new_l._int.widen = old->_int.widen;
            l = lattice_intern(f, new_l);
        }
    }

    // downward progress will widen, if we hit the limit we stop growing
    if (old->tag == LATTICE_INT && old != l) {
        Lattice* glb = lattice_meet(f, old, l);
        if (glb == l && l->tag == LATTICE_INT) {
            Lattice new_l = *l;
            new_l._int.widen = TB_MAX(old->_int.widen, l->_int.widen) + 1;
            if (new_l._int.widen >= INT_WIDEN_LIMIT) {
                int bits = tb_data_type_bit_size(f->super.module, n->dt.type);
                return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = {
                            .min         =  lattice_int_min(bits),
                            .max         =  lattice_int_max(bits),
                            .widen       =  INT_WIDEN_LIMIT
                        } });
            }

            return lattice_intern(f, new_l);
        }
    }

    return l;
}

static TB_Node* identity_select(TB_Function* f, TB_Node* n) {
    Lattice* pred = lattice_truthy(latuni_get(f, n->inputs[1]));
    if (pred == &TRUE_IN_THE_SKY) {
        return n->inputs[2];
    } else if (pred == &FALSE_IN_THE_SKY) {
        return n->inputs[3];
    } else {
        return n;
    }
}

static Lattice* value_poison(TB_Function* f, TB_Node* n) {
    return &TOP_IN_THE_SKY;
}

static Lattice* value_select(TB_Function* f, TB_Node* n) {
    Lattice* pred = lattice_truthy(latuni_get(f, n->inputs[1]));
    Lattice* a = latuni_get(f, n->inputs[2]);
    Lattice* b = latuni_get(f, n->inputs[3]);
    if (pred == &TOP_IN_THE_SKY || a == &TOP_IN_THE_SKY || b == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    }

    if (pred == &TRUE_IN_THE_SKY) {
        return a;
    } else if (pred == &FALSE_IN_THE_SKY) {
        return b;
    } else {
        return lattice_meet(f, a, b);
    }
}

// this is where the vtable goes for all peepholes
#include "peeps.h"

// Returns NULL or a modified node (could be the same node, we can stitch it back into place)
static TB_Node* idealize(TB_Function* f, TB_Node* n) {
    NodeIdealize ideal = node_vtables[n->type].idealize;
    return ideal ? ideal(f, n) : NULL;
}

static TB_Node* identity(TB_Function* f, TB_Node* n) {
    NodeIdentity identity = node_vtables[n->type].identity;
    return identity ? identity(f, n) : n;
}

static Lattice* value_of(TB_Function* f, TB_Node* n) {
    NodeValueOf value = node_vtables[n->type].value;
    Lattice* type = value ? value(f, n) : NULL;

    // no type provided? just make a not-so-form fitting bottom type
    if (type == NULL) {
        return n->dt.type == TB_TAG_TUPLE ? lattice_tuple_from_node(f, n) : lattice_from_dt(f, n->dt);
    } else {
        return type;
    }
}

#if TB_OPTDEBUG_STATS
static void inc_nums(int* arr, int i) { if (i < TB_NODE_TYPE_MAX) { arr[i]++; } }
#endif

#include "cprop.h"
#include "ipo.h"

static bool can_gvn(TB_Node* n) {
    switch (n->type) {
        case TB_LOCAL:
        case TB_MACH_TEMP:
        return false;

        // control producing nodes can't win from GVN (they all gonna be unique so the rules
        // are met, they'd just bloat the table tho).
        case TB_ROOT:
        case TB_CALL:
        case TB_HARD_BARRIER:
        case TB_REGION:
        case TB_RETURN:
        case TB_BRANCH:
        case TB_AFFINE_LATCH:
        case TB_SYSCALL:
        case TB_TAILCALL:
        case TB_CALLGRAPH:
        case TB_NATURAL_LOOP:
        case TB_AFFINE_LOOP:
        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_PTROFF:
        case TB_ATOMIC_CAS:
        case TB_SAFEPOINT:
        return false;

        default: {
            int family = n->type / 0x100;
            if (family == 0) {
                return true;
            } else {
                TB_ASSERT(family >= 0 && family < TB_ARCH_MAX);
                return tb_codegen_families[family].extra_bytes(n);
            }
        }
    }
}

TB_Node* tb_opt_gvn_node(TB_Function* f, TB_Node* n) {
    return tb__gvn(f, n, extra_bytes(n));
}

TB_Node* tb__gvn(TB_Function* f, TB_Node* n, size_t extra) {
    if (!can_gvn(n)) { return n; }

    // try GVN, if we succeed, just delete the node and use the old copy
    TB_Node* k = nl_hashset_put2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
    if (k && k != n) {
        // remove users
        FOR_N(i, 0, n->input_count) {
            remove_user(f, n, i);
            n->inputs[i] = NULL;
        }

        tb_arena_free(&f->arena, n->users, n->user_cap * sizeof(TB_User));
        tb_arena_free(&f->arena, n->inputs, n->input_cap * sizeof(TB_Node*));
        tb_arena_free(&f->arena, n, sizeof(TB_Node) + extra);
        n->type = TB_NULL;
        return k;
    } else {
        return n;
    }
}

TB_Node* make_poison(TB_Function* f, TB_DataType dt) {
    TB_Node* n = tb_alloc_node(f, TB_POISON, dt, 1, 0);
    set_input(f, n, f->root_node, 0);
    latuni_set(f, n, &TOP_IN_THE_SKY);
    return tb__gvn(f, n, 0);
}

TB_Node* make_int_node(TB_Function* f, TB_DataType dt, uint64_t x) {
    int bits = tb_data_type_bit_size(f->super.module, dt.type);
    x &= tb__mask(bits);

    TB_Node* n = tb_alloc_node(f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->value = x;

    set_input(f, n, f->root_node, 0);
    if (f->types) {
        latuni_set(f, n, value_int(f, n));
    }
    return tb__gvn(f, n, sizeof(TB_NodeInt));
}

TB_Node* make_f64_node(TB_Function* f, double x) {
    TB_Node* n = tb_alloc_node(f, TB_F64CONST, TB_TYPE_F64, 1, sizeof(TB_NodeFloat64));
    TB_NodeFloat64* i = TB_NODE_GET_EXTRA(n);
    i->value = x;

    set_input(f, n, f->root_node, 0);
    if (f->types) {
        latuni_set(f, n, value_f64(f, n));
    }
    return tb__gvn(f, n, sizeof(TB_NodeFloat64));
}

TB_Node* dead_node(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_DEAD, TB_TYPE_VOID, 1, 0);
    set_input(f, n, f->root_node, 0);
    latuni_set(f, n, &DEAD_IN_THE_SKY);
    return tb__gvn(f, n, 0);
}

TB_Node* make_ptr_offset(TB_Function* f, TB_Node* lhs, TB_Node* rhs) {
    TB_Node* n = tb_alloc_node(f, TB_PTR_OFFSET, TB_TYPE_PTR, 3, 0);
    set_input(f, n, lhs, 1);
    set_input(f, n, rhs, 2);

    TB_Node* k = tb_opt_peep_node(f, n);
    if (k != n && n->user_count == 0) {
        tb_violent_kill(f, n);
        n = k;
    }

    return n;
}

TB_Node* make_int_unary(TB_Function* f, TB_DataType dt, TB_NodeTypeEnum type, TB_Node* src) {
    TB_Node* n = tb_alloc_node(f, type, dt, 2, 0);
    set_input(f, n, src, 1);

    TB_Node* k = tb_opt_peep_node(f, n);
    if (k != n && n->user_count == 0) {
        tb_violent_kill(f, n);
        n = k;
    }

    return n;
}

TB_Node* make_int_binop(TB_Function* f, TB_NodeTypeEnum type, TB_Node* lhs, TB_Node* rhs) {
    TB_Node* n = tb_alloc_node(f, type, lhs->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, lhs, 1);
    set_input(f, n, rhs, 2);

    TB_Node* k = tb_opt_peep_node(f, n);
    if (k != n && n->user_count == 0) {
        tb_violent_kill(f, n);
        n = k;
    }
    return n;
}

TB_Node* make_proj_node(TB_Function* f, TB_DataType dt, TB_Node* src, int i) {
    TB_Node* n = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(f, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = i);
    return n;
}

TB_Node* make_branch_proj_node(TB_Function* f, TB_Node* src, int i, uint64_t key) {
    TB_Node* n = tb_alloc_node(f, TB_BRANCH_PROJ, TB_TYPE_CONTROL, 1, sizeof(TB_NodeBranchProj));
    set_input(f, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeBranchProj, .index = i, .key = key);
    return n;
}

static void remove_input(TB_Function* f, TB_Node* n, size_t i) {
    // remove swap
    size_t last = n->input_count - 1;
    if (i != last) {
        set_input(f, n, n->inputs[last], i);
    }
    set_input(f, n, NULL, last);
    n->input_count = last;
}

void tb_kill_node(TB_Function* f, TB_Node* n) {
    f->dead_node_bytes += sizeof(TB_Node);
    f->dead_node_bytes += n->input_cap*sizeof(TB_Node*);
    f->dead_node_bytes += n->user_cap*sizeof(TB_User);
    f->dead_node_bytes += extra_bytes(n);

    // remove from GVN if we're murdering it
    if (can_gvn(n)) {
        nl_hashset_remove2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
    }

    FOR_N(i, 0, n->input_count) {
        remove_user(f, n, i);
        n->inputs[i] = NULL;
    }

    // TB_ASSERT(n->users == NULL && "we can't kill nodes with users, that's fucking rude");
    n->input_count = 0;
    n->type = TB_NULL;
}

static void remove_user(TB_Function* f, TB_Node* n, int slot) {
    // early out: there was no previous input
    if (n->inputs[slot] == NULL) { return; }

    TB_Node* old = n->inputs[slot];
    TB_User* old_use = old->users;

    // remove-swap
    FOR_N(i, 0, old->user_count) {
        if (USERN(&old_use[i]) == n && USERI(&old_use[i]) == slot) {
            old->user_count -= 1;
            old_use[i] = old_use[old->user_count];

            // push to worklist, we've got a dead node
            if (old->user_count == 0 && f->worklist) {
                worklist_push(f->worklist, old);
            }
            return;
        }
    }

    tb_panic("Failed to remove non-existent user %p from %p (slot %d)", old, n, slot);
}

void set_input(TB_Function* f, TB_Node* n, TB_Node* in, int slot) {
    // try to recycle the user
    TB_ASSERT(slot < n->input_count);
    remove_user(f, n, slot);
    n->inputs[slot] = in;
    if (in != NULL) { add_user(f, n, in, slot); }
}

// we sometimes get the choice to recycle users because we just deleted something
void add_user(TB_Function* f, TB_Node* n, TB_Node* in, int slot) {
    if (in->user_count >= in->user_cap) {
        size_t new_cap = ((size_t) in->user_cap) * 2;
        if (new_cap >= UINT16_MAX) {
            tb_panic("Too many users to one node");
        }

        // resize
        TB_User* users = tb_arena_alloc(&f->arena, new_cap * sizeof(TB_User));
        memcpy(users, in->users, in->user_count * sizeof(TB_User));

        // in debug builds we'll fill the old array if easily detectable garbage to notice
        // pointer invalidation issues
        #ifndef NDEBUG
        memset(in->users, 0xF0, in->user_cap * sizeof(TB_User));
        memset(users + in->user_count, 0xF0, (new_cap - in->user_count) * sizeof(TB_User));
        #endif

        in->user_cap = new_cap;
        in->users = users;
    }

    #if TB_PACKED_USERS
    in->users[in->user_count]._n    = (uintptr_t) n;
    in->users[in->user_count]._slot = slot;
    #else
    in->users[in->user_count]._n    = n;
    in->users[in->user_count]._slot = slot;
    #endif
    in->user_count += 1;
}

void node_ensure_users_cap(TB_Function* f, TB_Node* n, size_t extra) {
    if (n->user_count + extra >= n->user_cap) {
        size_t new_cap = tb_next_pow2(n->user_count + extra + 4);
        TB_ASSERT(new_cap < UINT16_MAX);

        // resize
        TB_User* users = tb_arena_alloc(&f->arena, new_cap * sizeof(TB_User));
        memcpy(users, n->users, n->user_count * sizeof(TB_User));

        // in debug builds we'll fill the old array if easily detectable garbage to notice
        // pointer invalidation issues
        #ifndef NDEBUG
        memset(n->users, 0xF0, n->user_cap * sizeof(TB_User));
        memset(users + n->user_count, 0xF0, (new_cap - n->user_count) * sizeof(TB_User));
        #endif

        n->user_cap = new_cap;
        n->users = users;
    }
}

void subsume_node_without_phis(TB_Function* f, TB_Node* n, TB_Node* new_n) {
    TB_ASSERT(new_n != n);
    node_ensure_users_cap(f, new_n, n->user_count);

    FOR_N(i, 0, n->user_count) {
        TB_User u = n->users[i];
        TB_Node* un = USERN(&u);
        int ui      = USERI(&u);
        if (un->type == TB_PHI) { continue; }

        TB_ASSERT_MSG(un->inputs[ui] == n, "Mismatch between def-use and use-def data");
        TB_ASSERT(ui < un->input_cap);

        remove_user(f, un, ui);
        un->inputs[ui] = new_n;

        // we've resized in bulk, so no need to check in this loop
        new_n->users[new_n->user_count++] = u;
    }
}

void subsume_node2(TB_Function* f, TB_Node* n, TB_Node* new_n) {
    TB_ASSERT(new_n != n);
    node_ensure_users_cap(f, new_n, n->user_count);

    // these nodes can subsume themselves
    bool allow_cycle = cfg_is_region(new_n) || new_n->type == TB_PHI;
    FOR_N(i, 0, n->user_count) {
        TB_User u = n->users[i];
        TB_Node* un = USERN(&u);
        int ui      = USERI(&u);
        if (!allow_cycle && un == new_n) { continue; }

        TB_ASSERT_MSG(un->inputs[ui] == n, "Mismatch between def-use and use-def data");
        TB_ASSERT(ui < un->input_cap);

        remove_user(f, un, ui);
        un->inputs[ui] = new_n;

        // we've resized in bulk, so no need to check in this loop
        new_n->users[new_n->user_count++] = u;
    }
}

void subsume_node(TB_Function* f, TB_Node* n, TB_Node* new_n) {
    subsume_node2(f, n, new_n);
    if (n->user_count == 0) {
        tb_kill_node(f, n);
    }
}

// converts constant Lattice into constant node
static TB_Node* try_as_const(TB_Function* f, TB_Node* n, Lattice* l) {
    // already a constant?
    if (n->type == TB_SYMBOL || n->type == TB_ICONST || n->type == TB_F32CONST || n->type == TB_F64CONST) {
        return NULL;
    }

    if (cfg_is_region(n)) {
        // regions just should prune their dead edges
        return prune_region(f, n);
    } else if (n->type != TB_ROOT && n->type != TB_UNREACHABLE && n->inputs[0] && is_dead_ctrl(f, n->inputs[0])) {
        // control-dependent nodes which become considered dead will also
        // have to be dead.
        if (n->dt.type == TB_TAG_TUPLE) {
            TB_Node* dead = dead_node(f);
            while (n->user_count > 0) {
                TB_Node* use_n = USERN(&n->users[n->user_count - 1]);
                int use_i      = USERI(&n->users[n->user_count - 1]);

                if (use_n->type == TB_CALLGRAPH) {
                    TB_Node* last = use_n->inputs[use_n->input_count - 1];
                    set_input(f, use_n, NULL, use_n->input_count - 1);
                    if (use_i != use_n->input_count - 1) {
                        set_input(f, use_n, last, use_i);
                    }
                    use_n->input_count--;
                    mark_node(f, use_n);
                } else if (is_proj(use_n) || use_n->type == TB_SYMBOL_TABLE) {
                    TB_Node* replacement = use_n->dt.type == TB_TAG_CONTROL
                        ? dead
                        : make_poison(f, use_n->dt);

                    subsume_node(f, use_n, replacement);
                    mark_node(f, replacement);
                } else {
                    tb_todo();
                }
            }

            return dead;
        } else if (n->dt.type == TB_TAG_CONTROL) {
            return dead_node(f);
        } else {
            return make_poison(f, n->dt);
        }
    } else {
        // fold simple constants, calls, and branches
        switch (l->tag) {
            case LATTICE_INT: {
                // degenerate range
                if (l->_int.min == l->_int.max) {
                    return make_int_node(f, n->dt, l->_int.max);
                }

                // all bits are known
                int bits = tb_data_type_bit_size(f->super.module, n->dt.type);
                uint64_t mask = tb__mask(bits);
                if ((l->_int.known_zeros | l->_int.known_ones) == UINT64_MAX) {
                    return make_int_node(f, n->dt, l->_int.known_ones);
                }

                return NULL;
            }

            case LATTICE_FLTCON32: {
                TB_Node* k = tb_alloc_node(f, TB_F32CONST, n->dt, 1, sizeof(TB_NodeFloat32));
                set_input(f, k, f->root_node, 0);
                TB_NODE_SET_EXTRA(k, TB_NodeFloat32, .value = l->_f32);
                latuni_set(f, k, l);
                return tb__gvn(f, k, sizeof(TB_NodeFloat32));
            }

            case LATTICE_FLTCON64: {
                TB_Node* k = tb_alloc_node(f, TB_F64CONST, n->dt, 1, sizeof(TB_NodeFloat64));
                set_input(f, k, f->root_node, 0);
                TB_NODE_SET_EXTRA(k, TB_NodeFloat64, .value = l->_f64);
                latuni_set(f, k, l);
                return tb__gvn(f, k, sizeof(TB_NodeFloat64));
            }

            case LATTICE_NULL:
            return make_int_node(f, n->dt, 0);

            case LATTICE_TUPLE: {
                if (n->type == TB_CALL && l->tag == LATTICE_TUPLE && l->elems[0] != &LIVE_IN_THE_SKY) {
                    TB_Node* ctrl_out = USERN(proj_with_index(n, 0));
                    TB_User* succ_u = cfg_next_user(ctrl_out);
                    if (succ_u && USERN(succ_u)->type != TB_UNREACHABLE) {
                        TB_Node* mem_out = USERN(proj_with_index(n, 1));

                        TB_Node* dead = dead_node(f);
                        subsume_node2(f, ctrl_out, dead);

                        // either we find a new unreachable to hook it to
                        TB_Node* end = tb_alloc_node(f, TB_UNREACHABLE, TB_TYPE_CONTROL, 2, 0);
                        set_input(f, end, ctrl_out, 0);
                        set_input(f, end, mem_out,  1);
                        add_input_late(f, f->root_node, end);

                        mark_users(f, n);
                        return n;
                    }
                }

                if (n->type != TB_IF && n->type != TB_AFFINE_LATCH) {
                    return NULL;
                }

                // check if tuple is constant path
                int trues = 0;
                FOR_N(i, 0, l->_elem_count) {
                    if (l->elems[i] == &LIVE_IN_THE_SKY) {
                        trues++;
                    }
                }

                if (trues == 1) {
                    TB_Node* dead = dead_node(f);
                    TB_Node* ctrl = n->inputs[0];

                    for (size_t i = 0; i < n->user_count;) {
                        TB_Node* un = USERN(&n->users[i]);
                        if (is_proj(un)) {
                            TB_ASSERT(USERI(&n->users[i]) == 0);
                            int index   = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
                            TB_Node* in = l->elems[index] == &LIVE_IN_THE_SKY ? ctrl : dead;

                            set_input(f, un, NULL, 0);
                            subsume_node(f, un, in);
                        } else {
                            i += 1;
                        }
                    }

                    // no more projections, kill the branch
                    tb_kill_node(f, n);
                    mark_users(f, dead);
                    return ctrl;
                } else {
                    return NULL;
                }
            }

            default: return NULL;
        }
    }
}

static void print_lattice(Lattice* l) {
    switch (l->tag) {
        case LATTICE_BOT:      printf("bot");                       break;
        case LATTICE_TOP:      printf("top");                       break;
        case LATTICE_LIVE:     printf("live");                      break;
        case LATTICE_DEAD:     printf("dead");                      break;
        case LATTICE_FLT32:    printf("f32");                       break;
        case LATTICE_FLT64:    printf("f64");                       break;
        case LATTICE_FLTCON32: printf("[f32: %f]", l->_f32);        break;
        case LATTICE_FLTCON64: printf("[f64: %f]", l->_f64);        break;
        case LATTICE_NULL:     printf("null");                      break;
        case LATTICE_XNULL:    printf("~null");                     break;
        case LATTICE_NAN32:    printf("NaN32");                     break;
        case LATTICE_XNAN32:   printf("~NaN32");                    break;
        case LATTICE_NAN64:    printf("NaN64");                     break;
        case LATTICE_XNAN64:   printf("~NaN64");                    break;
        case LATTICE_ALLPTR:   printf("allptr");                    break;
        case LATTICE_ANYPTR:   printf("anyptr");                    break;
        case LATTICE_MEMORY:   printf("memory");                    break;
        case LATTICE_PTRCON:
        if (l->_ptr->name[0]) {
            printf("%s", l->_ptr->name);
        } else {
            printf("sym%p", l->_ptr);
        }
        break;

        case LATTICE_TUPLE: {
            printf("[");
            FOR_N(i, 0, l->_elem_count) {
                if (i) printf(", ");
                print_lattice(l->elems[i]);
            }
            printf("]");
            break;
        }

        case LATTICE_INT: {
            printf("[");
            if (l->_int.min == l->_int.max) {
                if (llabs(l->_int.min) > 10000) {
                    printf("%#"PRIx64, l->_int.min);
                } else {
                    printf("%"PRId64, l->_int.min);
                }
            } else if (l->_int.min == -1 && l->_int.max == 0) {
                printf("bool");
            } else if (l->_int.min == INT8_MIN && l->_int.max == INT8_MAX) {
                printf("i8");
            } else if (l->_int.min == INT16_MIN && l->_int.max == INT16_MAX) {
                printf("i16");
            } else if (l->_int.min == INT32_MIN && l->_int.max == INT32_MAX) {
                printf("i32");
            } else if (l->_int.min == INT64_MIN && l->_int.max == INT64_MAX) {
                printf("i64");
            } else if (l->_int.min > l->_int.max) {
                printf("%#"PRIx64",%#"PRIx64, l->_int.min, l->_int.max);
            } else {
                printf("%"PRId64",%"PRId64, l->_int.min, l->_int.max);
            }

            if (l->_int.known_zeros || l->_int.known_ones) {
                printf("; zeros=%#"PRIx64", ones=%#"PRIx64, l->_int.known_zeros, l->_int.known_ones);
            }
            if (l->_int.widen) {
                printf(", widen=%"PRIu64, l->_int.widen);
            }
            printf("]");
            break;
        }

        default:
        break;
    }
}

static int node_sort_cmp(const void* a, const void* b) {
    TB_Node* const* aa = a;
    TB_Node* const* bb = b;
    return aa[0]->gvn - bb[0]->gvn;
}

static void migrate_type(TB_Function* f, TB_Node* n, TB_Node* k) {
    // if both nodes are the same datatype, we should join the elements to avoid
    // weird backtracking when dealing with the pessimistic solver.
    if (k->dt.raw == n->dt.raw) {
        Lattice* new_t = latuni_get(f, k);
        Lattice* old_t = latuni_get(f, n);
        Lattice* merged = lattice_join(f, old_t, new_t);
        latuni_set(f, k, merged);
    }
}

// because certain optimizations apply when things are the same
// we mark ALL users including the ones who didn't get changed
// when subsuming.
static TB_Node* peephole(TB_Function* f, TB_Node* n, bool see_users) {
    TB_OPTLOG(PEEP, printf("PEEP t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

    bool progress = false;

    // idealize can modify the node, make sure it's not in the GVN pool at the time
    if (can_gvn(n)) {
        nl_hashset_remove2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
    }

    // idealize node (this can technically run an arbitrary number of times
    // but in practice we should only hit a node like once or twice)
    TB_NodeTypeEnum old_n_type = n->type;
    TB_Node* k = idealize(f, n);
    DO_IF(TB_OPTDEBUG_PEEP)(int loop_count=0);
    while (k != NULL) {
        DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.rewrites, old_n_type));
        TB_OPTLOG(PEEP, printf(" => \x1b[32m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m"));

        // transfer users from n -> k
        if (n != k) {
            migrate_type(f, n, k);
            subsume_node(f, n, k);
            n = k;
        }

        // mark post subsume since previous users of n might have
        // name equality based opts.
        progress = true;
        mark_users(f, n);

        // try again, maybe we get another transformation
        old_n_type = n->type;
        k = idealize(f, n);
        DO_IF(TB_OPTDEBUG_PEEP)(if (++loop_count > 5) { log_warn("%p: we looping a lil too much dawg...", n); });
    }

    // idealize ops could kill a node
    if (see_users && n->user_count == 0) {
        TB_OPTLOG(PEEP, printf(" => \x1b[93mKILL\x1b[0m"));
        return NULL;
    }

    // pessimistic constant prop
    {
        #ifndef NDEBUG
        Lattice* old_type = latuni_get(f, n);
        Lattice* new_type = value_of(f, n);

        // monotonic moving up
        Lattice* glb = lattice_meet(f, old_type, new_type);
        if (glb != old_type) {
            // HACK(NeGate): forward progress when making a range into a constant can sometimes get fucky with
            // the known bits so i'll just hack around that for now.
            // if (lattice_is_const(old_type) || !lattice_is_const(new_type)) {
            TB_OPTLOG(PEEP, printf("\n\nFORWARD PROGRESS ASSERT!\n"));
            TB_OPTLOG(PEEP, printf("  "), print_lattice(old_type), printf("  is not higher than  "), print_lattice(new_type), printf(", MEET: "), print_lattice(glb), printf("\n\n"));
            TB_ASSERT_MSG(0, "forward progress assert!");
            // }
        }
        #else
        Lattice* new_type = value_of(f, n);
        #endif

        // print fancy type
        TB_OPTLOG(PEEP, printf(" => \x1b[93m"), print_lattice(new_type), printf("\x1b[0m"));

        // validate int
        if (new_type->tag == LATTICE_INT) {
            TB_ASSERT_MSG((new_type->_int.known_ones & new_type->_int.known_zeros) == 0, "overlapping known bits?");

            if (new_type->_int.min == new_type->_int.max) {
                TB_ASSERT_MSG(new_type->_int.min == ~new_type->_int.known_zeros, "why is the known zeros so evil?");
                TB_ASSERT_MSG(new_type->_int.max ==  new_type->_int.known_ones,  "why is the known ones so evil?");
            }
        }

        if (latuni_set_progress(f, n, new_type)) {
            mark_users(f, n);

            old_n_type = n->type;
            TB_Node* k = try_as_const(f, n, new_type);
            if (k && k != n) {
                DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.constants, old_n_type));
                TB_OPTLOG(PEEP, printf(" => \x1b[96m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m\n"));

                migrate_type(f, n, k);
                subsume_node(f, n, k);

                if (k->dt.type == TB_TAG_TUPLE) {
                    TB_ASSERT(new_type->tag == LATTICE_TUPLE);
                    FOR_USERS(u, n) {
                        if (is_proj(USERN(u))) {
                            int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                            TB_ASSERT(index < new_type->_elem_count);

                            // just assign the type and mark users, projections
                            // have no real rules
                            if (latuni_set_progress(f, USERN(u), new_type->elems[index])) {
                                mark_users(f, USERN(u));
                            }
                        } else if (USERN(u)->type != TB_CALLGRAPH) {
                            worklist_push(f->worklist, USERN(u));
                        }
                    }
                } else {
                    mark_users(f, k);
                }

                progress = true;
                n = k;
            }
        }
    }

    // convert into matching identity
    old_n_type = n->type;
    k = identity(f, n);
    if (n != k) {
        DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.identities, old_n_type));
        TB_OPTLOG(PEEP, printf(" => \x1b[33m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m\n"));

        if (n->type == TB_PHI) {
            // notify region and neighbor phis
            mark_node_n_users(f, n->inputs[0]);
        }

        migrate_type(f, n, k);
        subsume_node(f, n, k);
        mark_users(f, k);
        return k;
    }

    // global value numbering
    #if TB_OPTDEBUG_GVN
    DynArray(TB_Node*) arr = dyn_array_create(TB_Node*, 64);
    nl_hashset_for(p, &f->gvn_nodes) {
        dyn_array_put(arr, *p);
    }
    qsort(arr, dyn_array_length(arr), sizeof(TB_Node*), node_sort_cmp);
    dyn_array_for(i, arr) {
        printf("  * ");
        tb_print_dumb_node(NULL, arr[i]);
        if (gvn_compare(arr[i], n)) {
            printf(" <-- HERE");
        }
        printf(" (hash=%#x)\n", gvn_hash(arr[i]));
    }
    #endif

    if (can_gvn(n)) {
        DO_IF(TB_OPTDEBUG_STATS)(f->stats.gvn_tries++);
        k = nl_hashset_put2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
        if (k && (k != n)) {
            DO_IF(TB_OPTDEBUG_STATS)(f->stats.gvn_hit++);
            TB_OPTLOG(PEEP, printf(" => \x1b[95mGVN %%%u\x1b[0m\n", k->gvn));

            migrate_type(f, n, k);
            subsume_node(f, n, k);
            mark_users(f, k);
            return k;
        }
    }

    TB_OPTLOG(PEEP, printf("\n"));
    return progress ? n : NULL;
}

TB_Node* tb_opt_peep_node(TB_Function* f, TB_Node* n) {
    TB_Node* k = peephole(f, n, false);
    return k ? k : n;
}

int tb_opt_peeps(TB_Function* f) {
    if (alloc_types(f)) {
        FOR_N(i, 0, dyn_array_length(f->worklist->items)) {
            TB_Node* n = f->worklist->items[i];
            f->types[n->gvn] = lattice_from_dt(f, n->dt);
        }
        f->types[f->root_node->gvn] = lattice_tuple_from_node(f, f->root_node);
    }

    int changes = 0;

    #if TB_OPTDEBUG_STATS
    uint64_t start = cuik_time_in_nanos();
    f->stats.solver_n = f->node_count;
    f->stats.solver_big_o = f->node_count;
    #endif

    CUIK_TIMED_BLOCK("peephole") {
        int i = 0;

        TB_Worklist* ws = f->worklist;
        while (worklist_count(ws)) {
            TB_ASSERT(i < dyn_array_length(ws->items));
            TB_Node* n = ws->items[i];

            // remove item
            worklist_remove(ws, n);
            dyn_array_remove(ws->items, i);

            // advance to literally any other place
            i += 1;
            if (i >= dyn_array_length(ws->items)) {
                i = 0;
            }

            // must've dead sometime between getting scheduled and getting here.
            if (n->type == TB_NULL) {
                continue;
            }

            if (!is_proj(n) && n->user_count == 0) {
                DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.killed, n->type));
                TB_OPTLOG(PEEP, printf("PEEP t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));
                TB_OPTLOG(PEEP, printf(" => \x1b[196mKILL\x1b[0m\n"));
                if (n->type == TB_SYMBOL_TABLE) {
                    mark_node_n_users(f, n->inputs[0]);
                }
                tb_kill_node(f, n);
            } else if (peephole(f, n, true)) {
                changes += 1;
            }
        }
    }

    #if TB_OPTDEBUG_STATS
    f->stats.solver_time = (cuik_time_in_nanos() - start);
    #endif

    return changes;
}

static void* zalloc(size_t s) {
    void* ptr = cuik_malloc(s);
    memset(ptr, 0, s);
    return ptr;
}

bool tb_opt(TB_Function* f, TB_Worklist* ws, bool preserve_types) {
    if (f->super.tag != TB_SYMBOL_FUNCTION) {
        return false;
    }

    #if 0
    __debugbreak();
    Lattice* a = lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { 0, 240, .known_zeros = 0xffffffffffffff0f } });
    Lattice* b = lattice_gimme_int(f, 0, lattice_uint_max(32), 32);
    Lattice* c = lattice_join(f, a, b);
    __debugbreak();
    #endif

    TB_ASSERT_MSG(f->root_node, "missing root node");
    f->worklist = ws;

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    #if TB_OPT_LOG_ENABLED
    if (1 || strcmp(f->super.name, "pack_table") == 0) {
        f->enable_log = true;
    }
    #endif

    #if TB_OPTDEBUG_STATS
    f->stats.peeps = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.identities = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.rewrites = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.constants = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.opto_constants = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.killed = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.cprop_t = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    #endif

    TB_ASSERT(worklist_count(ws) == 0);
    CUIK_TIMED_BLOCK("push_all_nodes") {
        // generate work list (put everything)
        worklist_push(ws, f->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }

        // just leads to getting to the important bits first in practice (RPO would be better but
        // more work to perform)
        /*CUIK_TIMED_BLOCK("reversing") {
            size_t last = dyn_array_length(ws->items) - 1;
            FOR_N(i, 0, dyn_array_length(ws->items) / 2) {
                SWAP(TB_Node*, ws->items[i], ws->items[last - i]);
            }
        }*/

        TB_OPTDEBUG(STATS)(f->stats.initial = worklist_count(ws));
    }
    TB_OPTLOG(PEEP, log_debug("%s: pushed %d nodes (out of %d)", f->super.name, worklist_count(f->worklist), f->node_count));

    TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Initial"));
    TB_OPTDEBUG(PASSES)(printf("FUNCTION %s:\n", f->super.name));
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));

    // uint64_t total = 0;
    enum {
        // "major" passes don't need to re-run
        // if they're not dirtied.
        CPROP_DIRTY = 1,
        LOOP_DIRTY  = 2,
        ALL_DIRTY   = LOOP_DIRTY | CPROP_DIRTY,
    };

    int k;
    int rounds = 0;

    int dirty = ALL_DIRTY;
    do {
        rounds++;
        TB_OPTDEBUG(PASSES)(printf("  * ROUND %d:\n", rounds));
        TB_OPTDEBUG(PASSES)(printf("    * Minor rewrites\n"));

        // minor opts
        cuikperf_region_start("minor opts", NULL);
        while (worklist_count(f->worklist) > 0) {
            TB_OPTDEBUG(PASSES)(printf("      * Peeps (%d nodes)\n", worklist_count(f->worklist)));

            // combined pessimistic solver
            STATS_ENTER(PEEPHOLES);
            if (k = tb_opt_peeps(f), k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Rewrote %d times\n", k));
                dirty |= ALL_DIRTY;
            }
            STATS_EXIT(PEEPHOLES);
            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Peepholes"));

            #if TB_OPTDEBUG_STATS
            if (f->stats.solver_n > 0) {
                double rate = f->stats.solver_time / f->stats.solver_n;
                TB_OPTDEBUG(PASSES)(printf("        * Rate: %f ns / node, O(%"PRId64")\n", rate, f->stats.solver_n));
            }
            #endif

            // locals scans the TB_LOCAL nodes, it might introduce peephole
            // work when it returns true.
            TB_OPTDEBUG(PASSES)(printf("      * Locals\n"));
            TB_OPTLOG(PEEP, printf("=== LOCALS ===\n"));
            STATS_ENTER(MEMORY);
            if (k = tb_opt_locals(f), k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Folded %d locals into SSA\n", k));
                dirty |= ALL_DIRTY;
            }
            STATS_EXIT(MEMORY);

            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Memory"));
        }
        cuikperf_region_end();

        // avoids bloating up my arenas with freed nodes
        float dead_factor = (float)f->dead_node_bytes / (float)tb_arena_current_size(&f->arena);
        if (dead_factor > 0.2f) {
            TB_OPTLOG(PEEP, printf("=== COMPACT ===\n"));
            STATS_ENTER(COMPACT);
            size_t old = tb_arena_current_size(&f->arena);
            tb_compact_nodes(f, ws);
            size_t new = tb_arena_current_size(&f->arena);
            STATS_EXIT(COMPACT);
            TB_OPTDEBUG(PASSES)(printf("    * Node GC: %.f KiB => %.f KiB\n", old / 1024.0, new / 1024.0));

            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Compact"));
        }

        if (dirty & LOOP_DIRTY) {
            dirty &= ~LOOP_DIRTY;

            TB_OPTLOG(PEEP, printf("=== LOOPS OPTS ===\n"));
            TB_OPTDEBUG(PASSES)(printf("    * Loops\n"));
            cuikperf_region_start("loops", NULL);

            TB_Worklist tmp_ws = { 0 };
            worklist_alloc(&tmp_ws, f->node_count);

            TB_OPTLOG(PEEP, tb_print(f));

            ////////////////////////////////
            // 1. Loop finding
            ////////////////////////////////
            bool progress = false;
            STATS_ENTER(LOOP_FIND);
            LoopOpt ctx = loop_opt_begin(f);
            STATS_EXIT(LOOP_FIND);
            ////////////////////////////////
            // 2. Remove safepoints
            ////////////////////////////////
            progress |= loop_opt_remove_safepoints(f, &ctx);
            ////////////////////////////////
            // 3. Rotate loops + Simplify backedges
            ////////////////////////////////
            STATS_ENTER(LOOP_ROTATE);
            progress |= loop_opt_canonicalize(f, &ctx, &tmp_ws);
            STATS_EXIT(LOOP_ROTATE);
            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Loop rotation"));
            ////////////////////////////////
            // 4. SLP vectorizer
            ////////////////////////////////
            // Run SLP on each loop (+ the main body)
            TB_OPTDEBUG(PASSES)(printf("    * Vectorize\n"));
            CUIK_TIMED_BLOCK("SLP") {
                STATS_ENTER(SUPERWORD);
                aarray_for(i, ctx.cfg.loops) {
                    TB_LoopTree* loop = ctx.cfg.loops[i];
                    if (loop->header->type != TB_NATURAL_LOOP && loop->header->type != TB_AFFINE_LOOP) {
                        continue;
                    }

                    if (slp_transform(f, &ctx, &tmp_ws, loop)) {
                        TB_OPTDEBUG(PASSES)(printf("      * Vectorized Loop%zu!\n", i));
                        progress = true;
                    }
                }

                if (slp_transform(f, &ctx, &tmp_ws, NULL)) {
                    TB_OPTDEBUG(PASSES)(printf("      * Vectorized Body!\n"));
                    progress = true;
                }
                STATS_EXIT(SUPERWORD);
            }
            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "SLP"));
            ////////////////////////////////
            // 5. Peepholes
            ////////////////////////////////
            STATS_ENTER(PEEPHOLES);
            CUIK_TIMED_BLOCK("loop peeps") {
                tb_opt_peeps(f);
            }
            STATS_EXIT(PEEPHOLES);
            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Peepholes"));
            ////////////////////////////////
            // 6. Loop strength reduction
            ////////////////////////////////
            TB_OPTDEBUG(LOOP)(tb_print(f));
            STATS_ENTER(STRENGTH_REDUCE);
            CUIK_TIMED_BLOCK("induction vars") {
                aarray_for(i, ctx.cfg.loops) {
                    TB_LoopTree* loop = ctx.cfg.loops[i];
                    if (loop->header->type == TB_AFFINE_LOOP) {
                        loop_strength_reduce(f, loop->header);
                    }
                }
            }
            STATS_EXIT(STRENGTH_REDUCE);
            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Loop IVs"));

            worklist_free(&tmp_ws);
            nl_table_free(ctx.loop_map);
            tb_free_cfg(&ctx.cfg);

            f->doms = NULL;
            f->doms_n = 0;

            tb_arena_clear(&f->tmp_arena);
            cuikperf_region_end();

            if (progress) {
                dirty |= CPROP_DIRTY;
            }
        }
        // don't worry, we'll scan all the nodes regardless
        worklist_clear(f->worklist);

        if (dirty & CPROP_DIRTY) {
            dirty &= ~CPROP_DIRTY;

            TB_OPTLOG(PEEP, printf("=== OPTIMISTIC ===\n"));
            TB_OPTDEBUG(PASSES)(printf("    * Optimistic solver\n"));

            // loop optimizer will bully the fuck out of the lattice types, so we might
            // as well reconstruct them using the optimistic crap
            cuikperf_region_start("optimistic", NULL);
            STATS_ENTER(OPTIMISTIC);
            CProp cprop = tb_opt_cprop_init(f);
            tb_opt_cprop_analyze(f, &cprop, false);
            k = tb_opt_cprop_rewrite(f);
            tb_opt_cprop_deinit(f, &cprop);
            STATS_EXIT(OPTIMISTIC);
            cuikperf_region_end();

            TB_OPTDEBUG(SERVER)(dbg_submit_event(f, "Optimistic"));

            if (k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Rewrote %d times\n", k));
                dirty |= LOOP_DIRTY;
            }

            #if TB_OPTDEBUG_STATS
            if (f->stats.solver_n > 0) {
                double rate = f->stats.solver_time / f->stats.solver_n;
                TB_OPTDEBUG(PASSES)(printf("        * Rate: %f ns / log node, O(%"PRId64" * log(%"PRId64")) = %"PRId64"\n", rate, f->stats.solver_n, f->stats.solver_n, f->stats.solver_big_o));
            }
            #endif
        }
    } while (dirty);
    TB_ASSERT(worklist_count(ws) == 0);
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));
    // if we're doing IPO then it's helpful to keep these
    if (!preserve_types) {
        tb_opt_free_types(f);
    }
    tb_arena_destroy(&f->tmp_arena);

    #if TB_OPTDEBUG_STATS
    tb_opt_dump_stats(f);

    cuik_free(f->stats.identities);
    cuik_free(f->stats.rewrites);
    cuik_free(f->stats.constants);
    cuik_free(f->stats.opto_constants);
    cuik_free(f->stats.killed);
    cuik_free(f->stats.cprop_t);
    #endif

    // printf("%f", total / 1000000.0);
    f->worklist = NULL;
    return dirty;
}

#if TB_OPTDEBUG_STATS
static void dump_nums(int* arr) {
    FOR_N(i, 0, TB_NODE_TYPE_MAX) if (arr[i]) {
        printf("%s: %d\n", tb_node_get_name(i), arr[i]);
    }
}
void tb_opt_dump_stats(TB_Function* f) {
    int final_count = f->node_count;
    double factor = ((double) final_count / (double) f->stats.initial) * 100.0;

    printf("%s: stats:\n", f->super.name);
    printf("  %4d   -> %4d nodes (%.2f%%)\n", f->stats.initial, final_count, factor);
    printf("  %4d GVN hit    %4d GVN attempts\n", f->stats.gvn_hit, f->stats.gvn_tries);
    printf("                  Identity Rewrite  (Pes)CCP (Opt)CCP Kill     CPropT\n");

    int d[6] = { 0 };
    FOR_N(i, 1, TB_NODE_TYPE_MAX) {
        int c[6];
        c[0] = f->stats.identities[i];
        c[1] = f->stats.rewrites[i];
        c[2] = f->stats.constants[i];
        c[3] = f->stats.opto_constants[i];
        c[4] = f->stats.killed[i];
        c[5] = f->stats.cprop_t[i];

        bool any = false;
        FOR_N(j, 0, 6) {
            any |= c[j] > 0;
            d[j] += c[j];
        }

        if (any) {
            printf("%-16s: ", tb_node_get_name(i));
            FOR_N(j, 0, 6) {
                if (c[j]) {
                    printf("\x1b[32m*%-6d\x1b[0m  ", c[j]);
                } else {
                    printf(" %-6d  ", 0);
                }
            }
            printf("\n");
        }
    }

    printf("  %4d rewrites    %4d identities\n", d[0], d[1]);
    printf("  %4d constants  %4d of which were optimistic\n", d[2] + d[3], d[3]);
    printf("  %d nanos spent on value_of() calls\n", d[5]);

    #if TB_OPTDEBUG_PEEP || TB_OPTDEBUG_SCCP
    printf("  %4d ticks\n", f->stats.time);
    #endif
}
#endif

TB_API TB_Worklist* tb_worklist_alloc(void) {
    TB_Worklist* ws = cuik_malloc(sizeof(TB_Worklist));
    worklist_alloc(ws, 500);
    return ws;
}

TB_API void tb_worklist_free(TB_Worklist* ws) {
    worklist_free(ws);
    cuik_free(ws);
}

static bool alloc_types(TB_Function* f) {
    if (f->types != NULL) { return false; }

    CUIK_TIMED_BLOCK("allocate type array") {
        size_t count = (f->node_count + 63ull) & ~63ull;
        f->type_cap = count;
        f->types = cuik_malloc(count * sizeof(Lattice*));
        // when latuni_get sees a NULL, it'll replace it with the correct bottom type
        FOR_N(i, 0, count) { f->types[i] = NULL; }
    }
    return true;
}

TB_API void tb_opt_free_types(TB_Function* f) {
    if (f->types != NULL) {
        cuik_free(f->types);
        f->types = NULL;
    }
}
