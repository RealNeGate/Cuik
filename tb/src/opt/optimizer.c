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
#include "merge_ret.h"
#include "mem2reg.h"

typedef struct {
    NL_Map(TB_Node*, char) marked;
} DCE;

static void dce_mark(TB_Function* f, DCE* dce, TB_Node* n) {
    ptrdiff_t search = nl_map_get(dce->marked, n);
    if (search >= 0) return;

    nl_map_put(dce->marked, n, 1);

    TB_FOR_INPUT_IN_NODE(in, n) if (*in) {
        dce_mark(f, dce, *in);
    }
}

static void dce(TB_Function* f) {
    // mark roots
    DCE dce = { 0 };
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (!tb_is_expr_like(r)) dce_mark(f, &dce, r);
        }
    }

    // sweep
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_Node* n = f->bbs[bb].start;
        for (; n != NULL; n = n->next) {
            ptrdiff_t search = nl_map_get(dce.marked, n);

            if (search < 0 && tb_is_expr_like(n)) {
                TB_KILL_NODE(n);
            }
        }
    }
    nl_map_free(dce.marked);
}

static void canonicalize(TB_Function* f) {
    CUIK_TIMED_BLOCK_ARGS("Canonical", f->super.name) {
        TB_FOR_BASIC_BLOCK(bb, f) {
            TB_FOR_NODE(n, f, bb) {
                const_fold(f, bb, n);
                simplify_cmp(f, n);
                // reassoc(f, n);
                simplify_pointers(f, n);

                // check if all paths are identical
                if (n->type == TB_PHI) {
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
                handle_pass(f, &ctx, bb, r);
            }
        }
        nl_map_free(ctx.def_table);
    }

    // kill any unused regs
    CUIK_TIMED_BLOCK_ARGS("DCE", f->super.name) {
        dce(f);
    }
}

static void schedule_function_level_opts(TB_Module* m, TB_Function* f, size_t pass_count, const TB_Pass* passes[]) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // run passes
    FOREACH_N(i, 0, pass_count) {
        canonicalize(f);

        CUIK_TIMED_BLOCK_ARGS(passes[i]->name, f->super.name) {
            passes[i]->func_run(f, tls);
        }
    }

    // just in case
    canonicalize(f);
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
