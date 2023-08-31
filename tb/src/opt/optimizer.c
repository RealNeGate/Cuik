// Let's just explain the architecture of the optimizer here.
//
// # Peephole optimizations
//   These are the kind which work locally like 2+2=4 and in TB's design they're
//   performed incrementally which means that certain mutations must go through
//   functions to guarentee they update correctly. Let's go over those:
//
//   set_input(opt, n, in, slot)
//     basically `n->inputs[slot] = in` except it correctly updates the user set
//
// # Implement peepholes
//     TODO
//
#include "../passes.h"
#include <log.h>

thread_local TB_Arena* tmp_arena;

// helps us do some matching later
static TB_Node* unsafe_get_region(TB_Node* n);
static void add_user(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot, User* recycled);
static User* remove_user(TB_Passes* restrict p, TB_Node* n, int slot);

// transmutations let us generate new nodes from old ones
void tb_transmute_to_poison(TB_Passes* restrict p, TB_Node* n);
TB_Node* tb_transmute_to_int(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, int num_words);

static void subsume_node(TB_Passes* restrict p, TB_Function* f, TB_Node* n, TB_Node* new_n);

// *new_node is set to true if we make a new node, it won't set it false for you
static TB_Node* clone_node(TB_Passes* restrict p, TB_Function* f, TB_Node* region, TB_Node* n, bool* new_node);

// node creation helpers
TB_Node* make_poison(TB_Function* f, TB_Passes* restrict p, TB_DataType dt);
TB_Node* make_int_node(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, uint64_t x);
TB_Node* make_proj_node(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, TB_Node* src, int i);

static void remove_pred(TB_Passes* restrict p, TB_Function* f, TB_Node* src, TB_Node* dst);

void verify_tmp_arena(TB_Passes* p) {
    // once passes are run on a thread, they're pinned to it.
    TB_Module* m = p->f->super.module;
    TB_ThreadInfo* info = tb_thread_info(m);

    if (p->pinned_thread == NULL) {
        p->pinned_thread = info;
        tb_arena_clear(&p->pinned_thread->tmp_arena);
    } else if (p->pinned_thread != info) {
        tb_panic(
            "TB_Passes are bound to a thread, you can't switch which threads they're run on\n\n"
            "NOTE: if you really need to run across threads you'll need to exit the passes and\n"
            "start anew... though you pay a performance hit everytime you start one"
        );
    }

    tmp_arena = &p->pinned_thread->tmp_arena;
}

static int bits_in_data_type(int pointer_size, TB_DataType dt) {
    switch (dt.type) {
        case TB_INT: return dt.data;
        case TB_PTR: return pointer_size;
        case TB_FLOAT:
        if (dt.data == TB_FLT_32) return 32;
        if (dt.data == TB_FLT_64) return 64;
        return 0;
        default: return 0;
    }
}

static char* lil_name(TB_Function* f, const char* fmt, ...) {
    char* buf = TB_ARENA_ALLOC(tmp_arena, 30);

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, 30, fmt, ap);
    va_end(ap);
    return buf;
}

static bool is_same_align(TB_Node* a, TB_Node* b) {
    TB_NodeMemAccess* aa = TB_NODE_GET_EXTRA(a);
    TB_NodeMemAccess* bb = TB_NODE_GET_EXTRA(b);
    return aa->align == bb->align;
}

static bool is_empty_bb(TB_Passes* restrict p, TB_Node* end) {
    assert(end->type == TB_BRANCH || end->type == TB_UNREACHABLE);
    if (end->inputs[0]->type != TB_START && end->inputs[0]->type != TB_REGION) {
        return false;
    }

    TB_Node* bb = end->inputs[0];
    for (User* use = find_users(p, bb); use; use = use->next) {
        TB_Node* n = use->n;
        if (use->n != end) return false;
    }

    return true;
}

static bool is_if_branch(TB_Node* n, uint64_t* falsey) {
    if (n->type == TB_BRANCH && n->input_count == 2 && TB_NODE_GET_EXTRA_T(n, TB_NodeBranch)->succ_count == 2) {
        *falsey = TB_NODE_GET_EXTRA_T(n, TB_NodeBranch)->keys[0];
        return true;
    }

    return false;
}

// unity build with all the passes
#include "lattice.h"
#include "cse.h"
#include "dce.h"
#include "fold.h"
#include "mem_opt.h"
#include "loop.h"
#include "branches.h"
#include "print.h"
#include "mem2reg.h"
#include "gcm.h"
#include "libcalls.h"

static void recompute_cfg(TB_Function* f, TB_Passes* restrict p) {
    CUIK_TIMED_BLOCK("recompute order") {
        tb_function_free_postorder(&p->order);
        p->order = tb_function_get_postorder(f);
    }

    CUIK_TIMED_BLOCK("doms") {
        FOREACH_N(i, 0, p->order.count) {
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(p->order.traversal[i]);
            r->dom_depth = -1; // unresolved
            r->dom = NULL;
        }

        // entry dominates itself
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(f->start_node);
        r->dom_depth = 0;
        r->dom = f->start_node;

        tb_compute_dominators(f, p->order);
    }
}

TB_Node* make_poison(TB_Function* f, TB_Passes* restrict p, TB_DataType dt) {
    TB_Node* n = tb_alloc_node(f, TB_POISON, dt, 1, 0);

    // try CSE, if we succeed, just delete the node and use the old copy
    TB_Node* k = nl_hashset_put2(&p->cse_nodes, n, cse_hash, cse_compare);
    if (k != NULL) {
        // try free?
        log_debug("%s: early CSE on poison", f->super.name);
        tb_arena_free(f->arena, n->inputs, sizeof(TB_Node*));
        tb_arena_free(f->arena, n, sizeof(TB_Node) + n->extra_count);
        return k;
    } else {
        return n;
    }
}

TB_Node* make_int_node(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, uint64_t x) {
    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 1, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = x;

    // try CSE, if we succeed, just delete the node and use the old copy
    TB_Node* k = nl_hashset_put2(&p->cse_nodes, n, cse_hash, cse_compare);
    if (k != NULL) {
        // try free?
        log_debug("%s: early CSE on integer %lld", f->super.name, x);
        tb_arena_free(f->arena, n->inputs, sizeof(TB_Node*));
        tb_arena_free(f->arena, n, sizeof(TB_Node) + n->extra_count);
        return k;
    } else {
        return n;
    }
}

TB_Node* tb_transmute_to_int(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, int num_words) {
    TB_Node* new_n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 1, sizeof(TB_NodeInt) + (num_words * sizeof(uint64_t)));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(new_n);
    i->num_words = num_words;
    return new_n;
}

TB_Node* make_proj_node(TB_Function* f, TB_Passes* restrict p, TB_DataType dt, TB_Node* src, int i) {
    TB_Node* n = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(p, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = i);
    return n;
}

static TB_Node* clone_node(TB_Passes* restrict p, TB_Function* f, TB_Node* region, TB_Node* n, bool* new_node) {
    assert(0 && "TODO");
    return NULL;
}

static void remove_input(TB_Passes* restrict p, TB_Function* f, TB_Node* n, size_t i) {
    // remove swap
    n->input_count--;
    if (n->input_count > 0) {
        if (n->input_count != i) {
            set_input(p, n, n->inputs[n->input_count], i);
        }
        set_input(p, n, NULL, n->input_count);
    }
}

// src -//-> dst
static void remove_pred(TB_Passes* restrict p, TB_Function* f, TB_Node* src, TB_Node* dst) {
    FOREACH_N(i, 0, dst->input_count) {
        if (tb_get_parent_region(dst->inputs[i]) == src) {
            remove_input(p, f, dst, i);

            // update PHIs
            for (User* use = find_users(p, dst); use; use = use->next) {
                if (use->n->type == TB_PHI && use->slot == 0) {
                    remove_input(p, f, use->n, i + 1);
                }
            }
            return;
        }
    }
}

void tb_transmute_to_poison(TB_Passes* restrict p, TB_Node* n) {
    // remove old users
    FOREACH_N(i, 0, n->input_count) {
        remove_user(p, n, i);
    }

    n->type = TB_POISON;
    n->input_count = 0;
    n->extra_count = 0;
}

void tb_pass_kill_node(TB_Passes* restrict p, TB_Node* n) {
    if (n->type == TB_LOCAL) {
        // remove from local list
        dyn_array_for(i, p->locals) if (p->locals[i] == n) {
            dyn_array_remove(p->locals, i);
            break;
        }
    }

    FOREACH_N(i, 0, n->input_count) {
        remove_user(p, n, i);
        n->inputs[i] = NULL;
    }

    nl_map_remove(p->users, n);

    n->input_count = 0;
    n->type = TB_NULL;
}

static User* remove_user(TB_Passes* restrict p, TB_Node* n, int slot) {
    // early out: there was no previous input
    if (n->inputs[slot] == NULL) return NULL;

    TB_Node* old = n->inputs[slot];
    User* old_use = find_users(p, old);
    if (old_use == NULL) return NULL;

    // remove old user (this must pass unless our users go desync'd)
    for (User* prev = NULL; old_use; prev = old_use, old_use = old_use->next) {
        if (old_use->slot == slot && old_use->n == n) {
            // remove
            if (prev) prev->next = old_use->next;
            else if (old_use->next == NULL) {
                nl_map_remove(p->users, old);
            } else {
                nl_map_put(p->users, old, old_use->next);
            }

            return old_use;
        }
    }

    log_error("Failed to remove non-existent user %p from %p (slot %d)", old, n, slot);
    log_error("Users:");
    nl_map_for(i, p->users) {
        log_error("  %p %s", p->users[i].k, tb_node_get_name(p->users[i].k));
        for (User* u = p->users[i].v; u; u = u->next) {
            log_error("    %p %d", u->n, u->slot);
        }
    }

    assert(0 && "we tried to remove something which didn't exist? (user list has desync'd)");
    return NULL;
}

void set_input(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot) {
    // recycle the user
    User* old_use = remove_user(p, n, slot);

    n->inputs[slot] = in;
    if (in != NULL) {
        add_user(p, n, in, slot, old_use);
    }
}

static void add_user(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot, User* recycled) {
    // just generate a new user list (if the slots don't match)
    ptrdiff_t search = nl_map_get(p->users, in);
    if (search < 0) {
        User* use = recycled ? recycled : TB_ARENA_ALLOC(tmp_arena, User);
        use->next = NULL;
        use->n = n;
        use->slot = slot;

        nl_map_put(p->users, in, use);
    } else {
        User* old = p->users[search].v;

        User* use = recycled ? recycled : TB_ARENA_ALLOC(tmp_arena, User);
        use->next = old;
        use->n = n;
        use->slot = slot;
        p->users[search].v = use;
    }
}

User* find_users(TB_Passes* restrict p, TB_Node* n) {
    ptrdiff_t search = nl_map_get(p->users, n);
    return search >= 0 ? p->users[search].v : NULL;
}

static void tb_pass_mark_users_raw(TB_Passes* restrict p, TB_Node* n) {
    for (User* use = find_users(p, n); use; use = use->next) {
        tb_pass_mark(p, use->n);
    }
}

void tb_pass_ensure_empty(TB_Passes* restrict p) {
    if (dyn_array_length(p->worklist) == 0) {
        dyn_array_clear(p->worklist);
    }

    CUIK_TIMED_BLOCK("clear visited") {
        nl_hashset_clear(&p->visited);
    }
}

void tb_pass_mark_users(TB_Passes* restrict p, TB_Node* n) {
    for (User* use = find_users(p, n); use; use = use->next) {
        tb_pass_mark(p, use->n);
        TB_NodeTypeEnum type = use->n->type;

        // tuples changing means their projections did too.
        if (use->n->dt.type == TB_TUPLE || type == TB_PROJ) {
            tb_pass_mark_users(p, use->n);
        }

        // if the store is changed, the users (potential loads) should be notified.
        // (br (cmp ...))
        if (type == TB_CMP_NE || type == TB_CMP_EQ || type == TB_STORE) {
            tb_pass_mark_users_raw(p, use->n);
        }

        if (type == TB_REGION) {
            tb_pass_mark_users_raw(p, use->n);

            TB_NodeRegion* r = TB_NODE_GET_EXTRA(use->n);
            TB_Node* end = r->end;
            if (end->type == TB_BRANCH) {
                tb_pass_mark(p, end);

                // mark direct successors
                TB_NodeBranch* br_info = TB_NODE_GET_EXTRA(end);
                FOREACH_N(i, 0, br_info->succ_count) {
                    tb_pass_mark(p, br_info->succ[i]);
                    tb_pass_mark_users_raw(p, br_info->succ[i]);
                }
            }
        }
    }
}

bool tb_pass_mark(TB_Passes* restrict p, TB_Node* n) {
    if (!nl_hashset_put(&p->visited, n)) {
        return false;
    }

    // log_debug("  %p: push %s", n, tb_node_get_name(n));

    dyn_array_put(p->worklist, n);
    nl_hashset_put(&p->visited, n);
    return true;
}

static void fill_all(TB_Passes* restrict p, TB_Node* n) {
    if (!nl_hashset_put(&p->visited, n)) {
        return;
    }
    dyn_array_put(p->worklist, n);

    FOREACH_REVERSE_N(i, 0, n->input_count) if (n->inputs[i]) {
        tb_assert(n->inputs[i], "empty input... in this economy?");
        fill_all(p, n->inputs[i]);
    }

    // walk successors for regions
    if (n->type == TB_START || n->type == TB_REGION) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);

        tb_assert(r->end, "missing terminator");
        fill_all(p, r->end);

        if (r->end->type == TB_BRANCH) {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
            FOREACH_N(i, 0, br->succ_count) {
                fill_all(p, br->succ[i]);
            }
        }
    }
}

static void cool_print_type(TB_Node* n) {
    TB_DataType dt = n->dt;
    if (n->type != TB_START && n->type != TB_REGION && !(n->type == TB_BRANCH && n->input_count == 1)) {
        if (n->type == TB_STORE) {
            dt = n->inputs[2]->dt;
        } else if (n->type == TB_BRANCH) {
            dt = n->inputs[1]->dt;
        } else if (n->type == TB_STOP) {
            dt = n->input_count > 1 ? n->inputs[1]->dt : TB_TYPE_VOID;
        } else if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
            dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
        }
        printf(".");
        print_type(dt);
    }
}

void print_node_sexpr(TB_Function* f, TB_Node* n, int depth) {
    if (n->type == TB_INTEGER_CONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);
        printf("%"PRId64, num->words[0]);
    } else if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            printf("%s", sym->name);
        } else {
            printf("sym%p", sym);
        }
    } else if (depth >= 1) {
        printf("(%s", tb_node_get_name(n));
        cool_print_type(n);
        printf(" ...)");
    } else {
        depth -= (n->type == TB_PROJ);

        printf("(%s", tb_node_get_name(n));
        cool_print_type(n);
        FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
            if (i == 0) printf(" @");
            else printf(" ");

            print_node_sexpr(f, n->inputs[i], depth + 1);
        }

        switch (n->type) {
            case TB_ARRAY_ACCESS:
            printf(" %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride);
            break;

            case TB_MEMBER_ACCESS:
            printf(" %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset);
            break;

            case TB_PROJ:
            printf(" %d", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
            break;
        }
        printf(")");
    }
}

// Returns NULL or a modified node (could be the same node, we can stitch it back into
// place)
static TB_Node* idealize(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    switch (n->type) {
        case TB_NOT:
        case TB_NEG:
        return ideal_int_unary(p, f, n);

        // integer ops
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        return ideal_int_binop(p, f, n);

        // pointer
        case TB_ARRAY_ACCESS:
        return ideal_array_ptr(p, f, n);

        // memory
        case TB_LOAD:
        return ideal_load(p, f, n);

        case TB_STORE:
        return ideal_store(p, f, n);

        case TB_MEMCPY:
        return ideal_memcpy(p, f, n);

        // division
        case TB_SDIV:
        case TB_UDIV:
        return ideal_int_div(p, f, n);

        // casting
        case TB_SIGN_EXT:
        case TB_ZERO_EXT:
        return ideal_extension(p, f, n);

        case TB_INT2PTR:
        return ideal_int2ptr(p, f, n);

        // truncate
        case TB_TRUNCATE:
        return ideal_truncate(p, f, n);

        case TB_CALL:
        return ideal_libcall(p, f, n);

        case TB_SELECT:
        return ideal_select(p, f, n);

        // control flow
        case TB_PHI:
        return ideal_phi(p, f, n);

        case TB_BRANCH:
        return ideal_branch(p, f, n);

        default:
        return NULL;
    }
}

// May return one of the inputs, this is used
static TB_Node* identity(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    switch (n->type) {
        // integer ops
        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        return identity_int_binop(p, f, n);

        case TB_SIGN_EXT:
        case TB_ZERO_EXT:
        return identity_extension(p, f, n);

        case TB_MEMBER_ACCESS:
        if (TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset == 0) {
            return n->inputs[1];
        }
        return n;

        case TB_LOAD:
        return identity_load(p, f, n);

        // dumb phis
        case TB_PHI: {
            TB_Node* same = n->inputs[1];
            FOREACH_N(i, 2, n->input_count) {
                if (same != n->inputs[i]) return n;
            }

            return same;
        }

        default:
        return n;
    }
}

static bool is_terminator(TB_Node* n) {
    return n->type == TB_BRANCH || n->type == TB_STOP || n->type == TB_TRAP || n->type == TB_UNREACHABLE;
}

static TB_Node* unsafe_get_region(TB_Node* n) {
    do {
        n = n->inputs[0];
    } while (n->type != TB_REGION && n->type != TB_START);
    return n;
}

static TB_Node* ideal_region(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // if there's one predecessor and it's to an unconditional branch, merge them.
    if (n->input_count == 1) {
        if (
            n->inputs[0]->type == TB_PROJ &&
            n->inputs[0]->inputs[0]->type == TB_BRANCH &&
            n->inputs[0]->inputs[0]->input_count == 1
        ) {
            TB_Node* top_node = unsafe_get_region(n->inputs[0]);
            TB_NodeRegion* top_region = TB_NODE_GET_EXTRA(top_node);
            TB_NodeRegion* bot_region = TB_NODE_GET_EXTRA(n);

            // bottom's region should be joined to whatever is above top's terminator
            subsume_node(p, f, n, top_region->end->inputs[0]);

            // set new terminator
            subsume_node(p, f, top_region->end, bot_region->end);
            top_region->end = bot_region->end;

            if (top_region->tag == NULL) {
                top_region->tag = bot_region->tag;
            }

            // re-evaluate traversal
            recompute_cfg(f, p);

            tb_pass_mark(p, top_node);
            return top_node;
        }
    }

    return NULL;
}

static bool peephole(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // must've dead sometime between getting scheduled and getting
    // here.
    if (find_users(p, n) == NULL) {
        return false;
    }

    // special case is peepholes on regions
    if (n->type == TB_REGION) {
        // check if it's a dead region
        if (n->input_count == 0) {
            DO_IF(TB_OPTDEBUG_PEEP)(printf("kill! "), print_node_sexpr(f, n, 0));

            // remove predecessor from other branches
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
            if (r->end->type == TB_BRANCH) {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
                FOREACH_N(i, 0, br->succ_count) if (br->succ[i]->input_count > 0) {
                    remove_pred(p, f, n, br->succ[i]);

                    tb_pass_mark(p, br->succ[i]);
                    tb_pass_mark_users(p, br->succ[i]);
                }
            }

            return true;
        }

        return ideal_region(p, f, n);
    }

    DO_IF(TB_OPTDEBUG_PEEP)(printf("peep? "), print_node_sexpr(f, n, 0));

    // idealize node (in a loop of course)
    TB_Node* k = idealize(p, f, n);
    DO_IF(TB_OPTDEBUG_PEEP)(int loop_count=0);
    while (k != NULL) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[32m"), print_node_sexpr(f, k, 0), printf("\x1b[0m"));

        tb_pass_mark_users(p, k);

        // transfer users from n -> k
        if (n != k) {
            tb_assert(!is_terminator(n), "can't peephole a branch into a new branch");
            subsume_node(p, f, n, k);

            n = k;
        }

        // try again, maybe we get another transformation
        k = idealize(p, f, n);

        DO_IF(TB_OPTDEBUG_PEEP)(if (++loop_count > 10) { log_warn("%p: we looping a lil too much dawg...", n); });
    }
    // DO_IF(TB_OPTDEBUG_PEEP)(printf(loop_count ? "\n" : "\x1b[2K\x1b[0G"));

    // convert into matching identity
    k = identity(p, f, n);
    if (n != k) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[33m"), print_node_sexpr(f, k, 0), printf("\x1b[0m"));
        subsume_node(p, f, n, k);
        return k;
    }

    // common subexpression elim
    k = nl_hashset_put2(&p->cse_nodes, n, cse_hash, cse_compare);
    if (k && (k != n)) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[31mCSE\x1b[0m"));
        subsume_node(p, f, n, k);
        return k;
    }

    return n;
}

static void subsume_node(TB_Passes* restrict p, TB_Function* f, TB_Node* n, TB_Node* new_n) {
    User* use = find_users(p, n);
    while (use != NULL) {
        tb_assert(use->n->inputs[use->slot] == n, "Mismatch between def-use and use-def data");

        // set_input will delete 'use' so we can't use it afterwards
        TB_Node* use_n = use->n;
        User* next = use->next;

        set_input(p, use->n, new_n, use->slot);
        use = next;
    }

    tb_pass_mark_users(p, new_n);
    tb_pass_kill_node(p, n);
}

static void generate_use_lists(TB_Passes* restrict p, TB_Function* f) {
    dyn_array_for(i, p->worklist) {
        TB_Node* n = p->worklist[i];
        nl_hashset_put2(&p->cse_nodes, n, cse_hash, cse_compare);

        if (n->type == TB_LOCAL) {
            // we don't need to check for duplicates here, the worklist is uniques
            dyn_array_put(p->locals, n);
        }

        FOREACH_N(i, 0, n->input_count) if (n->inputs[i]) {
            add_user(p, n, n->inputs[i], i, NULL);
        }
    }
}

TB_Passes* tb_pass_enter(TB_Function* f, TB_Arena* arena) {
    assert(f->stop_node && "missing return");
    TB_Passes* p = tb_platform_heap_alloc(sizeof(TB_Passes));
    *p = (TB_Passes){ .f = f };

    f->line_attrib.loc.file = NULL;
    f->arena = arena;

    verify_tmp_arena(p);

    p->cse_nodes = nl_hashset_alloc(f->node_count);

    // generate dominators
    p->order = tb_function_get_postorder(f);
    tb_compute_dominators(f, p->order);
    p->visited = nl_hashset_alloc(f->node_count);

    // generate work list (put everything)
    CUIK_TIMED_BLOCK("gen worklist") {
        fill_all(p, f->start_node);
    }

    f->node_count = dyn_array_length(p->worklist);
    DO_IF(TB_OPTDEBUG_PEEP)(log_debug("%s: starting passes with %d nodes", f->super.name, f->node_count));

    // find all outgoing edges
    CUIK_TIMED_BLOCK("gen users") {
        generate_use_lists(p, f);
    }

    return p;
}

bool tb_pass_peephole(TB_Passes* p) {
    verify_tmp_arena(p);

    TB_Function* f = p->f;
    bool changes = false;
    CUIK_TIMED_BLOCK("peephole") {
        while (dyn_array_length(p->worklist) > 0) CUIK_TIMED_BLOCK("iter") {
            // pull from worklist
            TB_Node* n = dyn_array_pop(p->worklist);
            nl_hashset_remove(&p->visited, n);

            if (peephole(p, f, n)) {
                changes = true;
                DO_IF(TB_OPTDEBUG_PEEP)(printf("\n"));
            }
        }
    }

    return changes;
}

void tb_pass_exit(TB_Passes* p) {
    verify_tmp_arena(p);
    TB_Function* f = p->f;

    nl_hashset_free(p->cse_nodes);
    nl_hashset_free(p->visited);

    tb_function_free_postorder(&p->order);
    tb_arena_clear(tmp_arena);
    nl_map_free(p->users);
    dyn_array_destroy(p->worklist);
}
