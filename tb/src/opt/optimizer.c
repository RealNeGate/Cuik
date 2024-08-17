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

// helps us do some matching later
static void remove_user(TB_Function* f, TB_Node* n, int slot);
static void remove_input(TB_Function* f, TB_Node* n, size_t i);
static void violent_kill(TB_Function* f, TB_Node* n);
static bool alloc_types(TB_Function* f);

static bool can_gvn(TB_Node* n);
static void print_lattice(Lattice* l);
static bool is_dead_ctrl(TB_Function* f, TB_Node* n);

static Lattice* value_of(TB_Function* f, TB_Node* n);

// node creation helpers
TB_Node* make_poison(TB_Function* f, TB_DataType dt);
TB_Node* dead_node(TB_Function* f);
TB_Node* make_int_node(TB_Function* f, TB_DataType dt, uint64_t x);
TB_Node* make_proj_node(TB_Function* f, TB_DataType dt, TB_Node* src, int i);

static void node_resize_inputs(TB_Function* f, TB_Node* n, size_t cnt) {
    if (cnt >= n->input_cap) {
        size_t new_cap = tb_next_pow2(cnt + 1);
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
    #ifndef NDEBUG
    // if it's possible to add late inputs, we can't have extra deps.
    FOR_N(i, n->input_count, n->input_cap) {
        TB_ASSERT(n->inputs[i] == NULL);
    }
    #endif

    node_resize_inputs(f, n, n->input_count);

    n->inputs[n->input_count] = in;
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

static TB_Node* mem_user(TB_Function* f, TB_Node* n, int slot) {
    FOR_USERS(u, n) {
        if ((USERN(u)->type == TB_PROJ && USERN(u)->dt.type == TB_TAG_MEMORY) ||
            (USERI(u) == slot && is_mem_out_op(USERN(u)))) {
            return USERN(u);
        }
    }

    return NULL;
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

static bool same_sorta_branch(TB_Node* n, TB_Node* n2) {
    TB_NodeBranchProj* br  = cfg_if_branch(n);
    TB_NodeBranchProj* br2 = cfg_if_branch(n2);
    return br && br2 && br->key == br2->key;
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
        worklist_push(f->worklist, USERN(u));
        TB_NodeTypeEnum type = USERN(u)->type;

        // tuples changing means their projections did too.
        if (type == TB_PROJ || type == TB_PTR_OFFSET) {
            mark_users(f, USERN(u));
        }

        // (br (cmp a b)) => ...
        // (or (shl a 24) (shr a 40)) => ...
        // (trunc (mul a b)) => ...
        // (phi ...) => ... (usually converting into branchless ops)
        if ((type >= TB_CMP_EQ && type <= TB_CMP_FLE) ||
            type == TB_SHL || type == TB_SHR || type == TB_MUL ||
            type == TB_STORE || type == TB_PHI) {
            mark_users_raw(f, USERN(u));
        }
    }
}

static void mark_node_n_users(TB_Function* f, TB_Node* n) {
    worklist_push(f->worklist, n);
    mark_node(f, n);
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
#include "peep_mem.h"
#include "peep_float.h"

// Module-level opts
#include "ipo.h"

#include "mem_opt.h"
#include "sroa.h"
#include "loop.h"
#include "branches.h"
#include "print.h"
#include "verify.h"
#include "print_dumb.h"
#include "print_svg.h"
#include "compact.h"
#include "gcm.h"
#include "slp.h"
#include "libcalls.h"
#include "mem2reg.h"
#include "rpo_sched.h"
#include "list_sched.h"
#include "bb_placement.h"

static bool is_dead_ctrl(TB_Function* f, TB_Node* n) {
    Lattice* l = latuni_get(f, n);
    return l == &TOP_IN_THE_SKY || l == &DEAD_IN_THE_SKY;
}

void tb__gvn_remove(TB_Function* f, TB_Node* n) {
    if (can_gvn(n)) {
        nl_hashset_remove2(&f->gvn_nodes, n, gvn_hash, gvn_compare);
    }
}

static void violent_kill(TB_Function* f, TB_Node* n) {
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

    assert(n->user_count == 0);
    n->user_cap = n->user_count = 0;
    n->users = NULL;
    n->input_count = 0;
    n->type = TB_NULL;
}

static Lattice* value_f32(TB_Function* f, TB_Node* n) {
    assert(n->type == TB_F32CONST);
    TB_NodeFloat32* num = TB_NODE_GET_EXTRA(n);
    return lattice_intern(f, (Lattice){ LATTICE_FLTCON32, ._f32 = num->value });
}

static Lattice* value_f64(TB_Function* f, TB_Node* n) {
    assert(n->type == TB_F64CONST);
    TB_NodeFloat64* num = TB_NODE_GET_EXTRA(n);
    return lattice_intern(f, (Lattice){ LATTICE_FLTCON64, ._f64 = num->value });
}

static Lattice* value_int(TB_Function* f, TB_Node* n) {
    assert(n->type == TB_ICONST);
    TB_NodeInt* num = TB_NODE_GET_EXTRA(n);
    if (n->dt.type == TB_TAG_PTR) {
        return num->value ? &XNULL_IN_THE_SKY : &NULL_IN_THE_SKY;
    } else {
        int64_t x = tb__sxt(num->value, tb_data_type_bit_size(f->super.module, n->dt.type), 64);
        return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { x, x, ~x, x } });
    }
}

static Lattice* value_root(TB_Function* f, TB_Node* n) {
    return NULL;
}

static Lattice* value_proj(TB_Function* f, TB_Node* n) {
    assert(is_proj(n));
    Lattice* l = latuni_get(f, n->inputs[0]);
    if (l == &TOP_IN_THE_SKY) {
        return &TOP_IN_THE_SKY;
    } else if (l == &BOT_IN_THE_SKY) {
        return lattice_from_dt(f, n->dt);
    } else {
        assert(l->tag == LATTICE_TUPLE);
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

static Lattice* value_lookup(TB_Function* f, TB_Node* n) {
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
}

static Lattice* value_region(TB_Function* f, TB_Node* n) {
    assert(cfg_is_region(n));

    FOR_N(i, 0, n->input_count) {
        Lattice* edge = latuni_get(f, n->inputs[i]);
        if (edge == &LIVE_IN_THE_SKY) { return &LIVE_IN_THE_SKY; }
    }

    return &DEAD_IN_THE_SKY;
}

static Lattice* affine_iv(TB_Function* f, Lattice* init, int64_t trips_min, int64_t trips_max, int64_t step, int bits) {
    int64_t max;
    if (__builtin_mul_overflow(trips_max, step, &max)) { return NULL; }
    if (__builtin_add_overflow(max, init->_int.min, &max)) { return NULL; }

    int64_t min = (uint64_t)init->_int.min + (uint64_t) (((uint64_t) trips_min-1)*step);
    if (step > 0) {
        if (min <= max) { return lattice_gimme_int(f, min, max, bits); }
    } else if (step > 0) {
        if (min >= max) { return lattice_gimme_int(f, max, min, bits); }
    }
    return NULL;
}

static TB_Node* ideal_location(TB_Function* f, TB_Node* n) {
    assert(n->type == TB_DEBUG_LOCATION);
    if (n->user_count == 0) { return NULL; }

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

static Lattice* value_phi(TB_Function* f, TB_Node* n) {
    // wait for region to check first
    TB_Node* r = n->inputs[0];
    if (latuni_get(f, r) == &TOP_IN_THE_SKY) return &TOP_IN_THE_SKY;

    Lattice* old = latuni_get(f, n);
    if (r->type == TB_AFFINE_LOOP) {
        TB_Node* latch = affine_loop_latch(r);
        if (latch && TB_IS_INTEGER_TYPE(n->dt)) {
            // we wanna know loop bounds
            uint64_t trips_min = 1, trips_max = UINT64_MAX;
            uint64_t* step_ptr = find_affine_indvar(n, r);
            Lattice* end = NULL;
            if (step_ptr) {
                int bits = tb_data_type_bit_size(f->super.module, n->dt.type);

                TB_InductionVar var;
                if (find_latch_indvar(r, latch, &var)) {
                    Lattice* init = latuni_get(f, var.phi->inputs[1]);
                    end = var.end_cond ? latuni_get(f, var.end_cond) : lattice_int_const(f, var.end_const);

                    if (lattice_is_const(init) && lattice_is_const(end)) {
                        int64_t trips = (end->_int.min - init->_int.min) / var.step;
                        int64_t rem   = (end->_int.min - init->_int.min) % var.step;
                        if (rem == 0 || var.pred != IND_NE) {
                            trips_max = trips;
                        }
                    } else {
                        // TODO(NeGate): we vaguely know the range, this is the common case
                        // so let's handle it. main things we wanna know are whether or not
                        // the number is ever negative.
                    }

                    if (var.phi != n) { end = NULL; }
                } else {
                    // affine loop missing latch? ok then it's 1 trips
                    trips_min = trips_max = 1;
                }
                assert(trips_min <= trips_max);

                Lattice* init = latuni_get(f, n->inputs[1]);
                if (lattice_is_const(init) && trips_max <= INT64_MAX) {
                    Lattice* range = affine_iv(f, init, trips_min, trips_max, *step_ptr, bits);
                    if (range) { return range; }
                }

                if (*step_ptr > 0 && cant_signed_overflow(n->inputs[2])) {
                    // pretty common that iterators won't overflow, thus never goes below init
                    int64_t min = init->_int.min;
                    int64_t max = end ? end->_int.max : lattice_int_max(bits);

                    // JOIN would achieve this effect too btw
                    if (old == &TOP_IN_THE_SKY) {
                        return lattice_gimme_int(f, min, max, bits);
                    } else {
                        return lattice_gimme_int(f, TB_MAX(min, old->_int.min), TB_MIN(max, old->_int.max), bits);
                    }
                }
            }
        }
    }

    Lattice* l = &TOP_IN_THE_SKY;
    FOR_N(i, 1, n->input_count) {
        Lattice* ctrl = latuni_get(f, r->inputs[i - 1]);
        if (ctrl == &LIVE_IN_THE_SKY) {
            Lattice* edge = latuni_get(f, n->inputs[i]);
            l = lattice_meet(f, l, edge);
        }
    }

    if (old->tag == LATTICE_INT) {
        // we wanna preserve widening count regardless (on pessimistic stuff
        // it would never widen so it'll stay as 0, on optimistic stuff old values
        // might have a widening that the meet of the phi doesn't include, we account
        // for that here)
        if (l->tag == LATTICE_INT && l->_int.widen < old->_int.widen) {
            Lattice new_l = *l;
            new_l._int.widen = old->_int.widen;
            l = lattice_intern(f, new_l);
        }

        // downward progress will widen...
        if (old != l) {
            Lattice* glb = lattice_meet(f, old, l);
            if (glb == l && l->tag == LATTICE_INT) {
                // we've hit the widening limit, since MAFs scale with the lattice height we limit how
                // many steps our ints can take since the lattice itself has a height of 18 quintillion...
                if (l->_int.widen >= INT_WIDEN_LIMIT) {
                    int bits = tb_data_type_bit_size(f->super.module, n->dt.type);
                    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = {
                                .min         =  lattice_int_min(bits),
                                .max         =  lattice_int_max(bits),
                                .widen       =  INT_WIDEN_LIMIT
                            } });
                }

                Lattice new_l = *l;
                new_l._int.widen = TB_MAX(old->_int.widen, l->_int.widen) + 1;
                return lattice_intern(f, new_l);
            }
        }
    }

    return l;
}

static Lattice* value_select(TB_Function* f, TB_Node* n) {
    Lattice* a = latuni_get(f, n->inputs[2]);
    Lattice* b = latuni_get(f, n->inputs[3]);
    return lattice_meet(f, a, b);
}

// this is where the vtable goes for all peepholes
#include "peeps.h"

static bool can_gvn(TB_Node* n) {
    switch (n->type) {
        case TB_LOCAL:
        case TB_MACH_MOVE:
        return false;

        // control producing nodes can't really GVN, it doesn't make sense if
        // they're constructed from a CFG.
        case TB_ROOT:
        case TB_CALL:
        case TB_READ:
        case TB_REGION:
        case TB_WRITE:
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
                assert(family >= 0 && family < TB_ARCH_MAX);
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
    return tb__gvn(f, n, 0);
}

TB_Node* make_int_node(TB_Function* f, TB_DataType dt, uint64_t x) {
    int bits = tb_data_type_bit_size(f->super.module, dt.type);
    x &= tb__mask(bits);

    TB_Node* n = tb_alloc_node(f, TB_ICONST, dt, 1, sizeof(TB_NodeInt));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->value = x;

    set_input(f, n, f->root_node, 0);

    latuni_set(f, n, value_int(f, n));
    return tb__gvn(f, n, sizeof(TB_NodeInt));
}

TB_Node* dead_node(TB_Function* f) {
    TB_Node* n = tb_alloc_node(f, TB_DEAD, TB_TYPE_VOID, 1, 0);
    set_input(f, n, f->root_node, 0);
    latuni_set(f, n, &DEAD_IN_THE_SKY);
    return tb__gvn(f, n, 0);
}

TB_Node* make_proj_node(TB_Function* f, TB_DataType dt, TB_Node* src, int i) {
    TB_Node* n = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(f, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = i);
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

    // assert(n->users == NULL && "we can't kill nodes with users, that's fucking rude");
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
    assert(slot < n->input_count);
    remove_user(f, n, slot);
    n->inputs[slot] = in;
    if (in != NULL) { add_user(f, n, in, slot); }
}

// we sometimes get the choice to recycle users because we just deleted something
void add_user(TB_Function* f, TB_Node* n, TB_Node* in, int slot) {
    if (in->user_count >= in->user_cap) {
        size_t new_cap = ((size_t) in->user_cap) * 2;
        assert(new_cap < UINT16_MAX);

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

    bool allow_cycle = cfg_is_region(n) || n->type == TB_PHI;
    // it's also possible to have a legal cycle where we subsume away the entire body of a loop
    if (cfg_is_region(new_n)) { allow_cycle = true; }

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

// converts constant Lattice into constant node
static TB_Node* try_as_const(TB_Function* f, TB_Node* n, Lattice* l) {
    // already a constant?
    if (n->type == TB_SYMBOL || n->type == TB_ICONST || n->type == TB_F32CONST || n->type == TB_F64CONST) {
        return NULL;
    }

    if (n->type != TB_ROOT && !cfg_is_region(n) && n->inputs[0] && is_dead_ctrl(f, n->inputs[0])) {
        if (n->type == TB_BRANCH || n->type == TB_AFFINE_LATCH) {
            f->invalidated_loops = true;
        }

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
                } else if (is_proj(use_n)) {
                    TB_Node* replacement = use_n->dt.type == TB_TAG_CONTROL
                        ? dead
                        : make_poison(f, use_n->dt);

                    subsume_node(f, use_n, replacement);
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
    }

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
            TB_NODE_SET_EXTRA(n, TB_NodeFloat64, .value = l->_f64);
            latuni_set(f, k, l);
            return tb__gvn(f, k, sizeof(TB_NodeFloat64));
        }

        case LATTICE_NULL:
        return make_int_node(f, n->dt, 0);

        case LATTICE_TUPLE: {
            if (n->type != TB_BRANCH && n->type != TB_AFFINE_LATCH) {
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
        case LATTICE_PTRCON:   printf("%s", l->_ptr->name);         break;
        case LATTICE_MEMORY:   printf("memory");                    break;

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
            } else if (l->_int.min == INT16_MIN && l->_int.max == INT16_MAX) {
                printf("i8");
            } else if (l->_int.min == INT16_MIN && l->_int.max == INT16_MAX) {
                printf("i16");
            } else if (l->_int.min == INT32_MIN && l->_int.max == INT32_MAX) {
                printf("i32");
            } else if (l->_int.min == INT64_MIN && l->_int.max == INT64_MAX) {
                printf("i64");
            } else if (l->_int.min > l->_int.max || llabs(l->_int.min) > 10000 || llabs(l->_int.max) > 10000) {
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

#if TB_OPTDEBUG_STATS
static void inc_nums(int* arr, int i) { if (i < TB_NODE_TYPE_MAX) { arr[i]++; } }
#endif

// because certain optimizations apply when things are the same
// we mark ALL users including the ones who didn't get changed
// when subsuming.
static TB_Node* peephole(TB_Function* f, TB_Node* n) {
    DO_IF(TB_OPTDEBUG_PEEP)(printf("PEEP t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

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
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[32m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m"));

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

    // pessimistic constant prop
    {
        #ifndef NDEBUG
        Lattice* old_type = latuni_get(f, n);
        Lattice* new_type = value_of(f, n);

        // monotonic moving up
        Lattice* glb = lattice_meet(f, old_type, new_type);
        if (glb != old_type) {
            TB_OPTDEBUG(PEEP)(printf("\n\nFORWARD PROGRESS ASSERT!\n"));
            TB_OPTDEBUG(PEEP)(printf("  "), print_lattice(old_type), printf("  =//=>  "), print_lattice(new_type), printf(", MEET: "), print_lattice(glb), printf("\n\n"));
            TB_ASSERT_MSG(0, "forward progress assert!");
        }
        #else
        Lattice* new_type = value_of(f, n);
        #endif

        // print fancy type
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[93m"), print_lattice(new_type), printf("\x1b[0m"));

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
                DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[96m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m\n"));

                migrate_type(f, n, k);
                subsume_node(f, n, k);
                mark_users(f, k);
                n = k;
            }
        }
    }

    // convert into matching identity
    old_n_type = n->type;
    k = identity(f, n);
    if (n != k) {
        DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.identities, old_n_type));
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[33m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m\n"));

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
            DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[95mGVN v%u\x1b[0m\n", k->gvn));

            migrate_type(f, n, k);
            subsume_node(f, n, k);
            mark_users(f, k);
            return k;
        }
    }

    DO_IF(TB_OPTDEBUG_PEEP)(printf("\n"));
    return progress ? n : NULL;
}

TB_Node* tb_opt_peep_node(TB_Function* f, TB_Node* n) {
    TB_Node* k = peephole(f, n);
    return k ? k : n;
}

void tb_pass_sroa(TB_Function* f) {
    CUIK_TIMED_BLOCK("sroa") {
        TB_Worklist* ws = f->worklist;
        int pointer_size = f->super.module->codegen->pointer_size;
        TB_Node* root = f->root_node;

        // write initial locals
        FOR_USERS(u, root) {
            if (USERN(u)->type == TB_LOCAL) { worklist_push(ws, USERN(u)); }
        }

        // i think the SROA'd pieces can't themselves split more? that should something we check
        size_t local_count = dyn_array_length(ws->items);
        for (size_t i = 0; i < local_count; i++) {
            TB_ASSERT(ws->items[i]->type == TB_LOCAL);
            sroa_rewrite(f, root, ws->items[i]);
        }
    }
}

static void push_non_bottoms(TB_Function* f, TB_Node* n) {
    // if it's a bottom there's no more steps it can take, don't recompute it
    Lattice* l = latuni_get(f, n);
    if (l != lattice_from_dt(f, n->dt)) { mark_node(f, n); }
}

static void tb_opt_cprop_node(TB_Function* f, TB_Node* n) {
    DO_IF(TB_OPTDEBUG_SCCP)(printf("TYPE t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

    Lattice* old_type = latuni_get(f, n);
    Lattice* new_type = value_of(f, n);

    DO_IF(TB_OPTDEBUG_SCCP)(printf(" => \x1b[93m"), print_lattice(new_type), printf("\x1b[0m\n"));
    if (old_type != new_type) {
        #ifndef NDEBUG
        // validate int
        if (new_type->tag == LATTICE_INT) {
            TB_ASSERT_MSG((new_type->_int.known_ones & new_type->_int.known_zeros) == 0, "overlapping known bits?");
        }

        Lattice* glb = lattice_meet(f, old_type, new_type);
        if (glb != new_type) {
            TB_OPTDEBUG(PEEP)(printf("\n\nFORWARD PROGRESS ASSERT!\n"));
            TB_OPTDEBUG(PEEP)(printf("  "), print_lattice(old_type), printf("  =//=>  "), print_lattice(new_type), printf(", MEET: "), print_lattice(glb), printf("\n\n"));
            TB_ASSERT_MSG(0, "forward progress assert!");
        }
        #endif

        latuni_set(f, n, new_type);

        // push affected users (handling one-input shit immediately)
        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            if (un->input_count == 1) {
                tb_opt_cprop_node(f, un);
            } else {
                push_non_bottoms(f, un);
                if (cfg_is_region(un)) {
                    FOR_USERS(phi, un) if (USERN(phi)->type == TB_PHI) {
                        push_non_bottoms(f, USERN(phi));
                    }
                }
            }
        }
    }
}

bool tb_opt_cprop(TB_Function* f) {
    TB_ASSERT(worklist_count(f->worklist) == 0);

    alloc_types(f);
    //   reset all types into TOP
    FOR_N(i, 0, f->node_count) { f->types[i] = &TOP_IN_THE_SKY; }
    //   anything unallocated should stay as NULL tho
    FOR_N(i, f->node_count, f->type_cap) { f->types[i] = NULL; }
    // except for ROOT
    worklist_push(f->worklist, f->root_node);

    // Pass 1: find constants.
    CUIK_TIMED_BLOCK("sccp") {
        TB_Node* n;
        while (n = worklist_pop(f->worklist), n) {
            tb_opt_cprop_node(f, n);
        }
    }

    // Pass 2: ok replace with constants now
    //   fills up the entire worklist again
    bool progress = false;
    worklist_push(f->worklist, f->root_node);
    for (size_t i = 0; i < dyn_array_length(f->worklist->items); i++) {
        TB_Node* n = f->worklist->items[i];
        TB_Node* k = try_as_const(f, n, latuni_get(f, n));
        DO_IF(TB_OPTDEBUG_SCCP)(printf("CONST t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));
        if (k != NULL) {
            DO_IF(TB_OPTDEBUG_STATS)(f->stats.opto_constants++);
            DO_IF(TB_OPTDEBUG_SCCP)(printf(" => \x1b[96m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m"));

            mark_node_n_users(f, k);
            if (n != k) {
                subsume_node(f, n, k);
            }
            progress = true;
            n = k;
        }
        DO_IF(TB_OPTDEBUG_SCCP)(printf("\n"));
        FOR_USERS(u, n) { mark_node(f, USERN(u)); }
    }

    return progress;
}

static void* zalloc(size_t s) {
    void* ptr = tb_platform_heap_alloc(s);
    memset(ptr, 0, s);
    return ptr;
}

bool tb_opt(TB_Function* f, TB_Worklist* ws, bool preserve_types) {
    TB_ASSERT_MSG(f->root_node, "missing root node");
    f->worklist = ws;

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    #if TB_OPTDEBUG_STATS
    f->stats.peeps = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.identities = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.rewrites = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.constants = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.opto_constants = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
    f->stats.killed = zalloc(TB_NODE_TYPE_MAX * sizeof(int));
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
        CUIK_TIMED_BLOCK("reversing") {
            size_t last = dyn_array_length(ws->items) - 1;
            FOR_N(i, 0, dyn_array_length(ws->items) / 2) {
                SWAP(TB_Node*, ws->items[i], ws->items[last - i]);
            }
        }

        TB_OPTDEBUG(STATS)(f->stats.initial = worklist_count(ws));
    }
    TB_OPTDEBUG(PEEP)(log_debug("%s: pushed %d nodes (out of %d)", f->super.name, worklist_count(f->worklist), f->node_count));
    f->invalidated_loops = true;

    TB_OPTDEBUG(PASSES)(printf("FUNCTION %s:\n", f->super.name));
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));

    int k;
    int rounds = 0;

    bool progress = false;
    bool major_progress;
    do {
        major_progress = false;

        TB_OPTDEBUG(PASSES)(printf("  * ROUND %d:\n", ++rounds));
        TB_OPTDEBUG(PASSES)(printf("    * Minor rewrites\n"));

        // minor opts
        while (worklist_count(f->worklist) > 0) {
            TB_OPTDEBUG(PASSES)(printf("      * Peeps (%d nodes)\n", worklist_count(f->worklist)));
            // combined pessimistic solver
            if (k = tb_opt_peeps(f), k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Rewrote %d times\n", k));
                progress = true;
            }

            // locals scans the TB_LOCAL nodes, it might introduce peephole
            // work when it returns true.
            TB_OPTDEBUG(PASSES)(printf("      * Locals\n"));
            DO_IF(TB_OPTDEBUG_PEEP)(printf("=== LOCALS ===\n"));
            if (k = tb_opt_locals(f), k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Folded %d locals into SSA\n", k));
            }
        }

        // const prop leaves work for the peephole optimizer and
        // sometimes might invalidate the loop tree so we should
        // track when it makes CFG changes.
        TB_OPTDEBUG(PASSES)(printf("    * Optimistic solver\n"));
        major_progress |= tb_opt_cprop(f);

        TB_OPTDEBUG(PASSES)(printf("      * Peeps (%d nodes)\n", worklist_count(f->worklist)));
        if (k = tb_opt_peeps(f), k > 0) {
            TB_OPTDEBUG(PASSES)(printf("        * Rewrote %d times\n", k));
        }

        // currently only rotating loops
        TB_OPTDEBUG(PASSES)(printf("    * Loops\n"));
        DO_IF(TB_OPTDEBUG_PEEP)(printf("=== LOOPS OPTS ===\n"));
        if (tb_opt_loops(f)) {
            major_progress = true;

            TB_OPTDEBUG(PASSES)(printf("      * Peeps (%d nodes)\n", worklist_count(f->worklist)));
            if (k = tb_opt_peeps(f), k > 0) {
                TB_OPTDEBUG(PASSES)(printf("        * Rewrote %d times\n", k));
            }
        }

        // TODO(NeGate): doesn't do anything yet
        // progress |= tb_opt_vectorize(f);

        progress |= major_progress;
    } while (major_progress);
    TB_ASSERT(tb_arena_is_empty(&f->tmp_arena));
    // if we're doing IPO then it's helpful to keep these
    if (!preserve_types) {
        tb_opt_free_types(f);
    }
    // avoids bloating up my arenas with freed nodes
    float dead_factor = (float)f->dead_node_bytes / (float)tb_arena_current_size(&f->arena);
    if (dead_factor > 0.2f) {
        tb_compact_nodes(f, ws);
    }
    tb_arena_destroy(&f->tmp_arena);

    #if TB_OPTDEBUG_STATS
    tb_opt_dump_stats(f);

    tb_platform_heap_free(f->stats.identities);
    tb_platform_heap_free(f->stats.rewrites);
    tb_platform_heap_free(f->stats.constants);
    tb_platform_heap_free(f->stats.opto_constants);
    tb_platform_heap_free(f->stats.killed);
    #endif

    f->worklist = NULL;
    return progress;
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

    printf("                 Identity  Rewrite      CCP     SCCP     Kill\n");
    FOR_N(i, 1, TB_NODE_TYPE_MAX) {
        int c[5];
        c[0] = f->stats.identities[i];
        c[1] = f->stats.rewrites[i];
        c[2] = f->stats.constants[i];
        c[3] = f->stats.opto_constants[i];
        c[4] = f->stats.killed[i];

        printf("%-16s: ", tb_node_get_name(i));
        FOR_N(j, 0, 5) {
            if (c[j]) {
                printf("\x1b[32m%6d*\x1b[0m  ", c[j]);
            } else {
                printf("%6d   ", 0);
            }
        }
        printf("\n");
    }

    // printf("  %4d peepholes  %4d rewrites    %4d identities\n", f->stats.peeps, f->stats.rewrites, f->stats.identities);
    // printf("  %4d constants  %4d of which were optimistic\n", total_constants, f->stats.opto_constants);

    #if TB_OPTDEBUG_PEEP || TB_OPTDEBUG_SCCP
    printf("  %4d ticks\n", f->stats.time);
    #endif
}
#endif

TB_API TB_Worklist* tb_worklist_alloc(void) {
    TB_Worklist* ws = tb_platform_heap_alloc(sizeof(TB_Worklist));
    worklist_alloc(ws, 500);
    return ws;
}

TB_API void tb_worklist_free(TB_Worklist* ws) {
    worklist_free(ws);
    tb_platform_heap_free(ws);
}

static bool alloc_types(TB_Function* f) {
    if (f->types != NULL) { return false; }

    CUIK_TIMED_BLOCK("allocate type array") {
        size_t count = (f->node_count + 63ull) & ~63ull;
        f->type_cap = count;
        f->types = tb_platform_heap_alloc(count * sizeof(Lattice*));
        // when latuni_get sees a NULL, it'll replace it with the correct bottom type
        FOR_N(i, 0, count) { f->types[i] = NULL; }
    }
    return true;
}

TB_API void tb_opt_free_types(TB_Function* f) {
    if (f->types != NULL) {
        tb_platform_heap_free(f->types);
        f->types = NULL;
    }
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
    CUIK_TIMED_BLOCK("peephole") {
        TB_Node* n;
        while ((n = worklist_pop(f->worklist))) {
            // must've dead sometime between getting scheduled and getting here.
            if (n->type == TB_NULL) { continue; }

            if (!is_proj(n) && n->user_count == 0) {
                DO_IF(TB_OPTDEBUG_STATS)(inc_nums(f->stats.killed, n->type));
                DO_IF(TB_OPTDEBUG_PEEP)(printf("PEEP t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));
                DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[196mKILL\x1b[0m\n"));
                tb_kill_node(f, n);
            } else if (peephole(f, n)) {
                changes += 1;
            }
        }
    }

    return changes;
}


