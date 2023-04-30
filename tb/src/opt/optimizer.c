#include "../tb_internal.h"
#include <stdarg.h>

typedef struct TB_Pass {
    // it's either a module-level pass or function-level
    bool is_module;
    const char* name;

    union {
        bool(*func_run)(TB_Function* f, TB_TemporaryStorage* tls);
        bool(*mod_run)(TB_Module* m);
    };
} TB_Pass;

// unity build with all the passes
#include "dce.h"
#include "fold.h"
#include "canonical.h"
// #include "mem2reg.h"
// #include "libcalls.h"

static void canonicalize(TB_Function* f) {
    #if 0
    // tb_function_print(f, tb_default_print_callback, stdout, false);
    bool changes;
    do {
        changes = false;

        CUIK_TIMED_BLOCK_ARGS("Canonical", f->super.name) {
            TB_FOR_BASIC_BLOCK(bb, f) {
                TB_FOR_NODE(n, f, bb) {
                    // reassoc(f, n);
                    const_fold(f, bb, n);
                    simplify_cmp(f, n);
                    simplify_pointers(f, bb, n);

                    if (n->type == TB_BRANCH && n->inputs[0] && n->inputs[0]->type == TB_INTEGER_CONST) {
                        // check for dead paths
                        TB_Node* key_n = n->inputs[0];
                        TB_NodeInt* key = TB_NODE_GET_EXTRA(key_n);
                        TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);

                        if (key->num_words == 1) {
                            FOREACH_N(i, 0, br->count) {
                                if (key->words[0] == br->targets[i].key) {
                                    key_n->extra_count = sizeof(TB_NodeBranch);
                                    n->inputs[0] = NULL;
                                    br->count = 0;
                                    br->default_label = br->targets[i].value;
                                    goto done;
                                }
                            }

                            // keep only default label
                            n->inputs[0] = NULL;
                            key_n->extra_count = sizeof(TB_NodeBranch);
                            br->count = 0;
                        }

                        done:;
                    } else if (n->type == TB_PHI) {
                        // check if all paths are identical
                        bool success = true;
                        FOREACH_N(i, 1, n->input_count) {
                            if (n->inputs[0] != n->inputs[i]) {
                                success = false;
                                break;
                            }
                        }

                        if (success) {
                            tb_transmute_to_pass(n, n->inputs[0]);
                        }
                    }
                }
            }
        }

        CUIK_TIMED_BLOCK_ARGS("PassRemove", f->super.name) {
            PassCtx ctx = { 0 };
            TB_FOR_BASIC_BLOCK(bb, f) {
                TB_FOR_NODE(r, f, bb) {
                    changes |= handle_pass(f, &ctx, bb, r);
                }
            }
            nl_map_free(ctx.def_table);
        }
    } while (changes);
    #endif
}

static void schedule_function_level_opts(TB_Module* m, TB_Function* f, size_t pass_count, const TB_Pass* passes[]) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // run passes
    FOREACH_N(i, 0, pass_count) {
        canonicalize(f);

        CUIK_TIMED_BLOCK_ARGS(passes[i]->name, f->super.name) {
            passes[i]->func_run(f, tls);
        }

        arena_clear(&tb__arena);
    }

    // just in case
    canonicalize(f);
    arena_clear(&tb__arena);
}

TB_API void tb_module_optimize(TB_Module* m, size_t pass_count, const TB_Pass* passes[]) {
    size_t i = 0;
    while (i < pass_count) {
        size_t sync = i;
        for (; sync < pass_count; sync++) {
            if (passes[sync]->is_module) break;
        }

        // TODO(NeGate): convert this into a trivial parallel dispatch
        if (sync != i) {
            TB_FOR_FUNCTIONS(f, m) {
                schedule_function_level_opts(m, f, sync - i, &passes[i]);
            }
            i = sync;
        }

        // synchronize and handle the module level stuff
        // TODO(NeGate): this requires special scheduling to be threaded
        // but it's completely possible
        for (; i < pass_count; i++) {
            if (!passes[i]->is_module) break;

            // run module level
            passes[i]->mod_run(m);
        }
    }
}
