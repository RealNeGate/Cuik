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

static void dce_mark(TB_Function* f, Set* live, TB_Reg r) {
    set_put(live, r);

    TB_FOR_INPUT_IN_REG(it, f, r) {
        if (!set_get(live, it.r)) dce_mark(f, live, it.r);
    }
}

static void dce(TB_Function* f) {
    // mark roots
    Set live = set_create(f->node_count);
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (!is_expr_like(f, r)) dce_mark(f, &live, r);
        }
    }

    // sweep
    FOREACH_N(r, 0, f->node_count) {
        if (set_get(&live, r)) continue;

        f->nodes[r].type = TB_NULL;
    }
    set_free(&live);
}

static void canonicalize(TB_Function* f) {
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            const_fold(f, &f->nodes[r]);
            simplify_cmp(f, &f->nodes[r]);
            reassoc(f, &f->nodes[r]);
            simplify_pointers(f, &f->nodes[r]);

            // check if all paths are identical
            if (tb_node_is_phi_node(f, r)) {
                int count = tb_node_get_phi_width(f, r);
                TB_PhiInput* inputs = tb_node_get_phi_inputs(f, r);

                bool success = true;
                FOREACH_N(i, 1, count) {
                    if (inputs[i].val != inputs[0].val) {
                        success = false;
                        break;
                    }
                }

                if (success) {
                    f->nodes[r].type = TB_PASS;
                    f->nodes[r].unary.src = inputs[0].val;
                }
            }
        }
    }

    PassCtx ctx = { 0 };
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            handle_pass(f, &ctx, bb, r);
        }
    }
    nl_map_free(ctx.def_table);

    // kill any unused regs
    dce(f);
}

static void schedule_function_level_opts(TB_Module* m, TB_Function* f, size_t pass_count, const TB_Pass* passes[]) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // run passes
    FOREACH_N(i, 0, pass_count) {
        canonicalize(f);

        passes[i]->func_run(f, tls);
        tb_function_print(f, tb_default_print_callback, stdout, false);
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
