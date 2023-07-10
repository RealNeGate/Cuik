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
#include "../tb_internal.h"
#include <log.h>

typedef struct User User;
struct User {
    User* next;
    TB_Node* n;
    int slot;
};

// inspired by the Hotspot's iter
struct TB_OptQueue {
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

static User* find_users(TB_OptQueue* restrict queue, TB_Node* n);
static void add_user(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* in, int slot, User* recycled);
static User* remove_user(TB_OptQueue* restrict queue, TB_Node* n, int slot);

static void set_input(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* in, int slot);

// transmutations let us generate new nodes from old ones
void tb_transmute_to_pass(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* point_to);
void tb_transmute_to_poison(TB_OptQueue* restrict queue, TB_Node* n);
TB_Node* tb_transmute_to_int(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, int num_words);

// node creation helpers
TB_Node* make_int_node(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, uint64_t x);
TB_Node* make_proj_node(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, TB_Node* src, int i);

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

// unity build with all the passes
#include "cse.h"
#include "dce.h"
#include "fold.h"
#include "load_opt.h"
#include "mem2reg.h"
#include "libcalls.h"
#include "identity.h"

TB_Node* make_int_node(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, uint64_t x) {
    TB_Node* n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + sizeof(uint64_t));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
    i->num_words = 1;
    i->words[0] = x;
    return n;
}

TB_Node* tb_transmute_to_int(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, int num_words) {
    TB_Node* new_n = tb_alloc_node(f, TB_INTEGER_CONST, dt, 0, sizeof(TB_NodeInt) + (num_words * sizeof(uint64_t)));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(new_n);
    i->num_words = num_words;
    return new_n;
}

TB_Node* make_proj_node(TB_Function* f, TB_OptQueue* restrict queue, TB_DataType dt, TB_Node* src, int i) {
    TB_Node* n = tb_alloc_node(f, TB_PROJ, dt, 1, sizeof(TB_NodeProj));
    set_input(queue, n, src, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeProj, .index = i);
    return n;
}

void tb_transmute_to_poison(TB_OptQueue* restrict queue, TB_Node* n) {
    // remove old users
    FOREACH_N(i, 0, n->input_count) {
        remove_user(queue, n, i);
    }

    n->type = TB_POISON;
    n->input_count = 0;
    n->extra_count = 0;
}

void tb_transmute_to_pass(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* point_to) {
    assert(n->input_count >= 1 && "TODO: Too small to transmute");

    // remove old users
    User* recycled = NULL;
    FOREACH_N(i, 0, n->input_count) {
        User* r = remove_user(queue, n, i);
        if (r) recycled = r;
    }

    n->type = TB_PASS;
    n->input_count = 1;
    n->extra_count = 0;
    n->dt = point_to->dt;
    n->inputs[0] = point_to;
    add_user(queue, n, point_to, 0, recycled);
}

void tb_optqueue_local_spawn(TB_OptQueue* restrict queue, TB_Node* n) {
    dyn_array_for(i, queue->locals) if (queue->locals[i] == n) {
        return;
    }

    dyn_array_put(queue->locals, n);
}

void tb_optqueue_kill(TB_OptQueue* restrict queue, TB_Node* n) {
    if (n->type == TB_LOCAL && queue->locals != NULL) {
        // remove from local list
        dyn_array_for(i, queue->locals) if (queue->locals[i] == n) {
            dyn_array_remove(queue->locals, i);
            break;
        }
    }

    FOREACH_N(i, 0, n->input_count) {
        remove_user(queue, n, i);
    }

    nl_map_remove(queue->users, n);
    TB_KILL_NODE(n);
}

static User* remove_user(TB_OptQueue* restrict queue, TB_Node* n, int slot) {
    // early out: there was no previous input
    if (n->inputs[slot] == NULL) return NULL;

    TB_Node* old = n->inputs[slot];
    User* old_use = find_users(queue, old);
    if (old_use == NULL) return NULL;

    // remove old user (this must pass unless our users go desync'd)
    for (User* prev = NULL; old_use; prev = old_use, old_use = old_use->next) {
        if (old_use->slot == slot && old_use->n == n) {
            // remove
            if (prev) prev->next = old_use->next;
            else if (old_use->next == NULL) {
                nl_map_remove(queue->users, old);
            } else {
                nl_map_put(queue->users, old, old_use->next);
            }

            return old_use;
        }
    }

    log_error("Failed to remove non-existent user %p from %p (slot %d)", old, n, slot);
    log_error("Users:");
    nl_map_for(i, queue->users) {
        log_error("  %p %s", queue->users[i].k, tb_node_get_name(queue->users[i].k));
        for (User* u = queue->users[i].v; u; u = u->next) {
            log_error("    %p %d", u->n, u->slot);
        }
    }

    assert(0 && "we tried to remove something which didn't exist? (user list has desync'd)");
    return NULL;
}

static void set_input(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* in, int slot) {
    // recycle the user
    User* old_use = remove_user(queue, n, slot);

    n->inputs[slot] = in;
    add_user(queue, n, in, slot, old_use);
}

static void add_user(TB_OptQueue* restrict queue, TB_Node* n, TB_Node* in, int slot, User* recycled) {
    // just generate a new user list (if the slots don't match)
    User* use = find_users(queue, in);
    if (use == NULL) {
        use = recycled ? recycled : ARENA_ALLOC(&tb__arena, User);
        use->next = NULL;
        use->n = n;
        use->slot = slot;

        nl_map_put(queue->users, in, use);
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

static User* find_users(TB_OptQueue* restrict queue, TB_Node* n) {
    ptrdiff_t search = nl_map_get(queue->users, n);
    return search >= 0 ? queue->users[search].v : NULL;
}

bool tb_optqueue_mark(TB_OptQueue* restrict queue, TB_Node* n, bool mark_kids) {
    ptrdiff_t search = nl_map_get(queue->lookup, n);
    if (search >= 0) {
        return false;
    }

    log_debug("  %p: push %s", n, tb_node_get_name(n));

    size_t i = dyn_array_length(queue->queue);
    dyn_array_put(queue->queue, n);
    nl_map_put(queue->lookup, n, i);

    // mark all users
    if (mark_kids) {
        for (User* use = find_users(queue, n); use; use = use->next) {
            tb_optqueue_mark(queue, use->n, false);
        }
    }

    return true;
}

void tb_optqueue_fill_all(TB_OptQueue* restrict queue, TB_Node* n) {
    ptrdiff_t search = nl_map_get(queue->lookup, n);
    if (search >= 0) {
        return;
    }

    size_t index = dyn_array_length(queue->queue);
    dyn_array_put(queue->queue, n);
    nl_map_put(queue->lookup, n, index);

    FOREACH_REVERSE_N(i, 0, n->input_count) {
        tb_optqueue_fill_all(queue, n->inputs[i]);
    }

    // walk successors for regions
    if (n->type == TB_START || n->type == TB_REGION) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        FOREACH_N(i, 0, r->succ_count) {
            tb_optqueue_fill_all(queue, r->succ[i]);
        }

        tb_optqueue_fill_all(queue, r->end);
    }
}

static TB_Node* peephole_node(TB_OptQueue* restrict queue, TB_Function* f, TB_Node* n) {
    bool passes = false;
    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]->type == TB_PASS) {
        set_input(queue, n, n->inputs[i]->inputs[0], i);
        passes = true;
    }

    if (passes) {
        return n;
    }

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
            if (n->inputs[1]->type == TB_INTEGER_CONST) {
                TB_NodeInt* src_i = TB_NODE_GET_EXTRA(n->inputs[1]);
                if (src_i->num_words == 1) {
                    int64_t offset = src_i->words[0] * TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride;

                    TB_Node* new_n = tb_alloc_node(f, TB_MEMBER_ACCESS, n->dt, 1, sizeof(TB_NodeMember));
                    set_input(queue, new_n, n->inputs[0], 0);
                    TB_NODE_SET_EXTRA(new_n, TB_NodeMember, .offset = offset);
                    return new_n;
                }
            }
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
}

static void peephole(TB_OptQueue* restrict queue, TB_Function* f) {
    CUIK_TIMED_BLOCK_ARGS("peephole", f->super.name) {
        // time complexity assertion lmao
        #ifndef NDEBUG
        int m = 0, n = dyn_array_length(queue->queue);
        #endif

        while (dyn_array_length(queue->queue) > 0) {
            // pull from worklist
            TB_Node* n = dyn_array_pop(queue->queue);
            nl_map_remove(queue->lookup, n);

            log_debug("  %p: pop %s", n, tb_node_get_name(n));

            #ifndef NDEBUG
            m += 1;
            #endif

            // try peephole
            TB_Node* progress = peephole_node(queue, f, n);
            if (progress == NULL) {
                // try CSE
                progress = nl_hashset_put2(&queue->cse_nodes, n, cse_hash, cse_compare);

                if (progress == NULL || progress == n) {
                    // no changes
                    continue;
                }
            } else {
                // try CSE
                TB_Node* potential_reuse = nl_hashset_put2(&queue->cse_nodes, progress, cse_hash, cse_compare);
                if (potential_reuse != NULL) {
                    progress = potential_reuse;
                }
            }

            if (progress != n) {
                // transmute the node into pass automatically
                User* use = find_users(queue, n);
                while (use != NULL) {
                    assert(use->n->inputs[use->slot] == n);

                    // set_input will delete 'use' so we can't use it afterwards
                    TB_Node* use_n = use->n;
                    User* next = use->next;

                    set_input(queue, use->n, progress, use->slot);
                    tb_optqueue_mark(queue, use_n, false);
                    use = next;
                }
            } else {
                tb_optqueue_mark(queue, n, true);
            }
        }

        #ifndef NDEBUG
        if (m+n > m*2) {
            log_warn("peephole %s: O(%d + %d) <= O(%d*2)", f->super.name, m, n, m);
        }
        #endif
    }
}

bool tb_passes_iter(TB_PassManager* manager, TB_Module* m, TB_Passes* passes) {
    // scan for function level
    size_t start = passes->end, sync = start;
    for (; sync < manager->count; sync++) {
        if (manager->passes[sync].is_module) break;
    }

    if (start != sync) {
        *passes = (TB_Passes){ false, start, sync };
        return true;
    }

    // handle module level pass
    if (sync < manager->count) {
        *passes = (TB_Passes){ true, sync, sync + 1 };
        return true;
    }

    // complete
    return false;
}

static void generate_use_lists(TB_OptQueue* restrict queue, TB_Function* f) {
    dyn_array_for(i, queue->queue) {
        TB_Node* n = queue->queue[i];
        nl_hashset_put2(&queue->cse_nodes, n, cse_hash, cse_compare);

        if (n->type == TB_LOCAL) {
            // we don't need to check for duplicates here, the queue is uniques
            dyn_array_put(queue->locals, n);
        }

        FOREACH_N(i, 0, n->input_count) {
            add_user(queue, n, n->inputs[i], i, NULL);
            tb_optqueue_mark(queue, n->inputs[i], false);
        }
    }
}

void tb_function_apply_passes(TB_PassManager* manager, TB_Passes passes, TB_Function* f, TB_Arena* arena) {
    CUIK_TIMED_BLOCK("optimize function") {
        assert(!passes.module_level);
        f->line_attrib = NULL; // don't add line info automatically to optimizer-generated nodes
        f->arena = arena;

        const TB_Pass* restrict arr = manager->passes;
        log_debug("run %d passes for %s", passes.end - passes.start, f->super.name);

        // generate work list (put everything)
        TB_OptQueue queue = { 0 };
        queue.cse_nodes = nl_hashset_alloc(f->node_count);

        CUIK_TIMED_BLOCK("nl_map_create") {
            nl_map_create(queue.lookup, f->node_count);
        }

        CUIK_TIMED_BLOCK("tb_optqueue_fill_all") {
            tb_optqueue_fill_all(&queue, f->start_node);
        }

        // find all outgoing edges
        CUIK_TIMED_BLOCK("generate_use_lists") {
            generate_use_lists(&queue, f);
        }

        #ifndef NDEBUG
        // printf("\n\nORIGINAL:\n");
        // tb_function_print(f, tb_default_print_callback, stdout);
        #endif

        // run passes
        FOREACH_N(i, passes.start, passes.end) {
            peephole(&queue, f);

            #ifndef NDEBUG
            // printf("\n\nAFTER PEEP:\n");
            // tb_function_print(f, tb_default_print_callback, stdout);
            #endif

            CUIK_TIMED_BLOCK_ARGS(arr[i].name, f->super.name) {
                log_debug("run %s on %s", arr[i].name, f->super.name);
                arr[i].func_run(f, &queue);
            }
        }

        peephole(&queue, f);
        nl_hashset_free(queue.cse_nodes);

        arena_clear(&tb__arena);
        nl_map_free(queue.users);
        nl_map_free(queue.lookup);
        dyn_array_destroy(queue.queue);
    }
}

void tb_module_apply_passes(TB_PassManager* manager, TB_Passes passes, TB_Module* m, TB_Arena* arena) {
    log_debug("run %d passes for entire module %p", passes.end - passes.start, m);

    const TB_Pass* restrict arr = manager->passes;
    FOREACH_N(i, passes.start, passes.end) {
        arr[i].mod_run(m);
    }

    arena_clear(&tb__arena);
}
