// Let's just explain the architecture of the optimizer here.
//
// # Peephole optimizations
//   These are the kind which work locally like 2+2=4 and in TB's design they're
//   performed incrementally which means that certain mutations must go through
//   functions to guarentee they update correctly. Let's go over those:
//
//   set_input(queue, n, in, slot)
//     basically `n->inputs[slot] = in` except it correctly updates the user set
//
// # Implement peepholes
//     TODO
//
#include "../tb_internal.h"
#include <log.h>

#define TB_OPTDEBUG_PEEP 0
#define TB_OPTDEBUG_LOOP 1

#define DO_IF(cond) CONCAT(DO_IF_, cond)
#define DO_IF_0(...)
#define DO_IF_1(...) __VA_ARGS__

typedef struct User User;
struct User {
    User* next;
    TB_Node* n;
    int slot;
};

struct TB_FuncOpt {
    TB_Function* f;

    // some function state is changed for the duration of the
    // optimizer run, we need to track that to reset it
    TB_Attrib* old_line_attrib;
    TB_Arena* old_arena;

    DynArray(TB_Node*) queue;
    NL_Map(TB_Node*, int) lookup;

    // we wanna track locals because it's nice and easy
    DynArray(TB_Node*) locals;

    // this is used to do CSE
    NL_HashSet cse_nodes;

    // outgoing edges are incrementally updated every time we
    // run a rewrite rule
    NL_Map(TB_Node*, User*) users;
};

static User* find_users(TB_FuncOpt* restrict opt, TB_Node* n);
static void add_user(TB_FuncOpt* restrict opt, TB_Node* n, TB_Node* in, int slot, User* recycled);
static User* remove_user(TB_FuncOpt* restrict opt, TB_Node* n, int slot);

static void set_input(TB_FuncOpt* restrict opt, TB_Node* n, TB_Node* in, int slot);

// transmutations let us generate new nodes from old ones
void tb_transmute_to_poison(TB_FuncOpt* restrict opt, TB_Node* n);
TB_Node* tb_transmute_to_int(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, int num_words);

static void subsume_node(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n, TB_Node* new_n);

// *new_node is set to true if we make a new node, it won't set it false for you
static TB_Node* clone_node(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* region, TB_Node* n, bool* new_node);

// node creation helpers
TB_Node* make_int_node(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, uint64_t x);
TB_Node* make_proj_node(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, TB_Node* src, int i);

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
    char* buf = alloc_from_node_arena(f, 30);

    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, 30, fmt, ap);
    va_end(ap);
    return buf;
}

// unity build with all the passes
#include "cse.h"
#include "dce.h"
#include "fold.h"
#include "load_opt.h"
#include "loop.h"
#include "print.h"
#include "mem2reg.h"
#include "libcalls.h"

TB_Node* make_int_node(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, uint64_t x) {
    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = x;
    return n;
}

TB_Node* tb_transmute_to_int(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, int num_words) {
    TB_Node* new_n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + (num_words * sizeof(uint64_t)));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(new_n);
    i->num_words = num_words;
    return new_n;
}

TB_Node* make_proj_node(TB_Function* f, TB_FuncOpt* restrict opt, TB_DataType dt, TB_Node* src, int i) {
    TB_Node* n = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(opt, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = i);
    return n;
}

static TB_Node* clone_node(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* region, TB_Node* n, bool* new_node) {
    assert(0 && "TODO");
    return NULL;
}

void tb_transmute_to_poison(TB_FuncOpt* restrict opt, TB_Node* n) {
    // remove old users
    FOREACH_N(i, 0, n->input_count) {
        remove_user(opt, n, i);
    }

    n->type = TB_POISON;
    n->input_count = 0;
    n->extra_count = 0;
}

void tb_funcopt_kill(TB_FuncOpt* restrict opt, TB_Node* n) {
    if (n->type == TB_LOCAL) {
        // remove from local list
        dyn_array_for(i, opt->locals) if (opt->locals[i] == n) {
            dyn_array_remove(opt->locals, i);
            break;
        }
    }

    FOREACH_N(i, 0, n->input_count) {
        remove_user(opt, n, i);
    }

    nl_map_remove(opt->users, n);
    TB_KILL_NODE(n);
}

static User* remove_user(TB_FuncOpt* restrict opt, TB_Node* n, int slot) {
    // early out: there was no previous input
    if (n->inputs[slot] == NULL) return NULL;

    TB_Node* old = n->inputs[slot];
    User* old_use = find_users(opt, old);
    if (old_use == NULL) return NULL;

    // remove old user (this must pass unless our users go desync'd)
    for (User* prev = NULL; old_use; prev = old_use, old_use = old_use->next) {
        if (old_use->slot == slot && old_use->n == n) {
            // remove
            if (prev) prev->next = old_use->next;
            else if (old_use->next == NULL) {
                nl_map_remove(opt->users, old);
            } else {
                nl_map_put(opt->users, old, old_use->next);
            }

            return old_use;
        }
    }

    log_error("Failed to remove non-existent user %p from %p (slot %d)", old, n, slot);
    log_error("Users:");
    nl_map_for(i, opt->users) {
        log_error("  %p %s", opt->users[i].k, tb_node_get_name(opt->users[i].k));
        for (User* u = opt->users[i].v; u; u = u->next) {
            log_error("    %p %d", u->n, u->slot);
        }
    }

    assert(0 && "we tried to remove something which didn't exist? (user list has desync'd)");
    return NULL;
}

static void set_input(TB_FuncOpt* restrict opt, TB_Node* n, TB_Node* in, int slot) {
    // recycle the user
    User* old_use = remove_user(opt, n, slot);

    n->inputs[slot] = in;
    if (in != NULL) {
        add_user(opt, n, in, slot, old_use);
    }
}

static void add_user(TB_FuncOpt* restrict opt, TB_Node* n, TB_Node* in, int slot, User* recycled) {
    // just generate a new user list (if the slots don't match)
    User* use = find_users(opt, in);
    if (use == NULL) {
        use = recycled ? recycled : ARENA_ALLOC(&tb__arena, User);
        use->next = NULL;
        use->n = n;
        use->slot = slot;

        nl_map_put(opt->users, in, use);
    } else {
        // the slot might've already existed, let's check
        User* prev = NULL;
        for (; use != NULL; prev = use, use = use->next) {
            if (use->n == n && use->slot == slot) return;
        }

        use = recycled ? recycled : ARENA_ALLOC(&tb__arena, User);
        use->next = NULL;
        use->n = n;
        use->slot = slot;

        assert(prev);
        prev->next = use;
    }
}

static User* find_users(TB_FuncOpt* restrict opt, TB_Node* n) {
    ptrdiff_t search = nl_map_get(opt->users, n);
    return search >= 0 ? opt->users[search].v : NULL;
}

void tb_funcopt_mark_users(TB_FuncOpt* restrict opt, TB_Node* n) {
    for (User* use = find_users(opt, n); use; use = use->next) {
        tb_funcopt_mark(opt, use->n);

        // if the store is changed, the users (potential loads) should
        // be notified.
        if (use->n->type == TB_STORE || use->n->type == TB_REGION || use->n->type == TB_PROJ) {
            tb_funcopt_mark_users(opt, use->n);
        }
    }
}

bool tb_funcopt_mark(TB_FuncOpt* restrict opt, TB_Node* n) {
    ptrdiff_t search = nl_map_get(opt->lookup, n);
    if (search >= 0) {
        return false;
    }

    // log_debug("  %p: push %s", n, tb_node_get_name(n));

    size_t i = dyn_array_length(opt->queue);
    dyn_array_put(opt->queue, n);
    nl_map_put(opt->lookup, n, i);
    return true;
}

static void fill_all(TB_FuncOpt* restrict opt, TB_Node* n) {
    ptrdiff_t search = nl_map_get(opt->lookup, n);
    if (search >= 0) {
        return;
    }

    size_t index = dyn_array_length(opt->queue);
    dyn_array_put(opt->queue, n);
    nl_map_put(opt->lookup, n, index);

    FOREACH_REVERSE_N(i, 0, n->input_count) {
        fill_all(opt, n->inputs[i]);
    }

    // walk successors for regions
    if (n->type == TB_START || n->type == TB_REGION) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        FOREACH_N(i, 0, r->succ_count) {
            fill_all(opt, r->succ[i]);
        }

        fill_all(opt, r->end);
    }
}

/*static TB_Node* peephole_node(TB_FuncOpt* restrict queue, TB_Function* f, TB_Node* n) {
    switch (n->type) {
        case TB_NOT:
        case TB_NEG:
        return try_unary_fold(f, queue, n);

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
        return try_int_binop_fold(f, queue, n);

        // division
        case TB_SDIV:
        case TB_UDIV:
        return try_idiv_fold(f, queue, n);

        case TB_SIGN_EXT:
        case TB_ZERO_EXT:
        return try_extension_fold(f, queue, n);

        // array access
        case TB_ARRAY_ACCESS: {
            return NULL;
        }

        // memory
        case TB_LOAD:
        return try_load_opts(f, queue, n);

        // dumb phis
        case TB_PHI: {
            TB_Node* same = n->inputs[1];
            FOREACH_N(i, 2, n->input_count) {
                if (same != n->inputs[i]) return NULL;
            }

            return same;
        }

        // special functions
        case TB_CALL:
        if (n->inputs[1]->type == TB_GET_SYMBOL_ADDRESS) {
            return try_libcalls_fold(f, queue, n);
        }
        return NULL;

        // no changes
        default: return NULL;
    }
}*/

static void print_node_sexpr(TB_Function* f, TB_Node* n, int depth) {
    if (n->type == TB_INTEGER_CONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);
        printf("%lld", num->words[0]);
    } else if (depth > 0) {
        printf("(%s ...)", tb_node_get_name(n));
    } else {
        printf("(%s", tb_node_get_name(n));
        FOREACH_N(i, 0, n->input_count) {
            printf(" ");
            print_node_sexpr(f, n->inputs[i], depth + 1);
        }

        switch (n->type) {
            case TB_ARRAY_ACCESS:
            printf(" %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride);
            break;

            case TB_MEMBER_ACCESS:
            printf(" %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset);
            break;
        }
        printf(")");
    }
}

// Returns NULL or a modified node (could be the same node, we can stitch it back into
// place)
static TB_Node* idealize(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    switch (n->type) {
        case TB_NOT:
        case TB_NEG:
        return ideal_int_unary(opt, f, n);

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
        return ideal_int_binop(opt, f, n);

        // pointer
        case TB_ARRAY_ACCESS:
        return ideal_array_ptr(opt, f, n);

        // memory
        case TB_LOAD:
        return ideal_load(opt, f, n);

        // division
        case TB_SDIV:
        case TB_UDIV:
        return ideal_int_div(opt, f, n);

        // casting
        case TB_SIGN_EXT:
        case TB_ZERO_EXT:
        return ideal_extension(opt, f, n);

        // truncate
        case TB_TRUNCATE:
        return ideal_truncate(opt, f, n);

        // control flow
        case TB_BRANCH:
        // "this is what graph rewriting looks like, you may not like it but this is peak optimizer"
        // "CPU caches hate this trick"
        // "billions must stall"
        if (n->input_count == 1 &&
            n->inputs[0]->type == TB_REGION &&
            n->inputs[0]->input_count == 1 &&
            n->inputs[0]->inputs[0]->type == TB_PROJ &&
            n->inputs[0]->inputs[0]->inputs[0]->type == TB_BRANCH &&
            n->inputs[0]->inputs[0]->inputs[0]->input_count == 1) {
            return n->inputs[0]->inputs[0]->inputs[0];
        }
        return NULL;

        default:
        return NULL;
    }
}

// May return one of the inputs, this is used
static TB_Node* identity(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
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
        return identity_int_binop(opt, f, n);

        default:
        return n;
    }
}

static bool is_terminator(TB_Node* n) {
    return n->type == TB_BRANCH || n->type == TB_RET || n->type == TB_UNREACHABLE;
}

static TB_Node* unsafe_get_region(TB_Node* n) {
    do {
        n = n->inputs[0];
    } while (n->type != TB_REGION && n->type != TB_START);
    return n;
}

static TB_Node* peephole(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n) {
    // must've dead sometime between getting scheduled and getting
    // here.
    if (find_users(opt, n) == NULL) {
        return NULL;
    }

    #if TB_OPTDEBUG_PEEP
    printf("peep? ");
    print_node_sexpr(f, n, 0);
    #endif

    // if we fold branches, we need to know who to update
    bool terminator = is_terminator(n);

    // idealize node (in a loop of course)
    TB_Node* k = idealize(opt, f, n);
    DO_IF(TB_OPTDEBUG_PEEP)(int loop_count=0);
    while (k != NULL) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[%dm", 36+loop_count), print_node_sexpr(f, k, 0), printf("\x1b[0m"));

        tb_funcopt_mark_users(opt, k);

        // transfer users from n -> k
        if (n != k) {
            // if we're modifying a branch, we'll need to update Region data
            if (UNLIKELY(terminator)) {
                tb_assert(is_terminator(k), "k must be either a terminator for this peephole to be legal");
                TB_NodeRegion* new_region = TB_NODE_GET_EXTRA(unsafe_get_region(k));

                // update k's sucessors to what n says
                size_t succ_count = 0;
                for (User* use = find_users(opt, n); use != NULL; use = use->next) succ_count++;

                // resize new successors
                if (new_region->succ_count != succ_count) {
                    new_region->succ_count = succ_count;
                    new_region->succ = alloc_from_node_arena(f, succ_count * sizeof(TB_Node*));
                }

                User* use = find_users(opt, n);
                while (use != NULL) {
                    tb_assert(use->n->inputs[use->slot] == n, "Mismatch between def-use and use-def data");

                    // set_input will delete 'use' so we can't use it afterwards
                    TB_Node* use_n = use->n;
                    User* next = use->next;

                    set_input(opt, use->n, k, use->slot);

                    TB_Node* succ_region = find_users(opt, use->n)->n;
                    assert(succ_region->type == TB_REGION);
                    new_region->succ[use->slot] = succ_region;

                    use = next;
                }

                tb_funcopt_mark_users(opt, k);
            } else {
                subsume_node(opt, f, n, k);
            }

            n = k;
        }

        // try again, maybe we get another transformation
        k = idealize(opt, f, n);

        DO_IF(TB_OPTDEBUG_PEEP)(if (++loop_count > 10) { log_warn("%p: we looping a lil too much dawg...", n); });
    }
    // TB_DEBUG_CODE(printf(loop_count ? "\n" : "\x1b[2K\x1b[0G"));

    // convert into matching identity
    k = identity(opt, f, n);
    if (n != k) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[%dm", 37+loop_count), print_node_sexpr(f, k, 0), printf("\x1b[0m"));
        subsume_node(opt, f, n, k);
        return k;
    }

    // common subexpression elim
    k = nl_hashset_put2(&opt->cse_nodes, n, cse_hash, cse_compare);
    if (k && (k != n)) {
        DO_IF(TB_OPTDEBUG_PEEP)(printf(" => \x1b[31mCSE\x1b[0m"));
        subsume_node(opt, f, n, k);
        return k;
    }

    return n;
}

static void subsume_node(TB_FuncOpt* restrict opt, TB_Function* f, TB_Node* n, TB_Node* new_n) {
    User* use = find_users(opt, n);
    while (use != NULL) {
        tb_assert(use->n->inputs[use->slot] == n, "Mismatch between def-use and use-def data");

        // set_input will delete 'use' so we can't use it afterwards
        TB_Node* use_n = use->n;
        User* next = use->next;

        set_input(opt, use->n, new_n, use->slot);
        use = next;
    }

    tb_funcopt_mark_users(opt, new_n);
    tb_funcopt_kill(opt, n);
}

static void generate_use_lists(TB_FuncOpt* restrict queue, TB_Function* f) {
    dyn_array_for(i, queue->queue) {
        TB_Node* n = queue->queue[i];
        nl_hashset_put2(&queue->cse_nodes, n, cse_hash, cse_compare);

        if (n->type == TB_LOCAL) {
            // we don't need to check for duplicates here, the queue is uniques
            dyn_array_put(queue->locals, n);
        }

        FOREACH_N(i, 0, n->input_count) {
            add_user(queue, n, n->inputs[i], i, NULL);
        }
    }
}

TB_FuncOpt* tb_funcopt_enter(TB_Function* f, TB_Arena* arena) {
    TB_FuncOpt* opt = tb_platform_heap_alloc(sizeof(TB_FuncOpt));
    *opt = (TB_FuncOpt){ .f = f, .old_line_attrib = f->line_attrib, .old_arena = f->arena };

    f->line_attrib = NULL; // don't add line info automatically to optimizer-generated nodes
    f->arena = arena;

    opt->cse_nodes = nl_hashset_alloc(f->node_count);

    CUIK_TIMED_BLOCK("nl_map_create") {
        nl_map_create(opt->lookup, f->node_count);
    }

    // generate work list (put everything)
    CUIK_TIMED_BLOCK("tb_optqueue_fill_all") {
        fill_all(opt, f->start_node);
    }

    // find all outgoing edges
    CUIK_TIMED_BLOCK("generate_use_lists") {
        generate_use_lists(opt, f);
    }

    return opt;
}

bool tb_funcopt_peephole(TB_FuncOpt* opt) {
    bool changes = false;
    CUIK_TIMED_BLOCK("peephole") {
        TB_Function* f = opt->f;

        while (dyn_array_length(opt->queue) > 0) {
            // pull from worklist
            TB_Node* n = dyn_array_pop(opt->queue);
            nl_map_remove(opt->lookup, n);

            if (peephole(opt, f, n)) {
                changes = true;

                DO_IF(TB_OPTDEBUG_PEEP)(printf("\n"));
            }
        }
    }

    return changes;
}

void tb_funcopt_exit(TB_FuncOpt* opt) {
    TB_Function* f = opt->f;
    f->line_attrib = opt->old_line_attrib;
    f->arena = opt->old_arena;

    nl_hashset_free(opt->cse_nodes);

    arena_clear(&tb__arena);
    nl_map_free(opt->users);
    nl_map_free(opt->lookup);
    dyn_array_destroy(opt->queue);
}
