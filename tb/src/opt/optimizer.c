#include "../tb_internal.h"
#include <stdarg.h>

typedef struct Use Use;
struct Use {
    Use* next;
    TB_Reg r;
};

typedef struct TB_OptimizerCtx {
    // we have to mark any instructions which are
    // changed per pass.
    bool is_changed_ir_on_heap;
    size_t changed_ir_cap;
    size_t changed_ir_count;
    TB_Reg* changed_ir;

    // users[reg] is an unordered list
    Use** users;
} TB_OptimizerCtx;

typedef struct TB_Pass {
    // it's either a module-level pass or function-level
    bool is_module;
    const char* name;

    union {
        bool(*func_run)(TB_Function* f, TB_OptimizerCtx* ctx, TB_TemporaryStorage* tls);
        bool(*mod_run)(TB_Module* m);
    };
} TB_Pass;

// optimizer specific macros
#include "opt_words.h"

// unity build with all the passes
#include "dce.h"
#include "fold.h"
#include "canonical.h"
#include "hoist_locals.h"

static Use** generate_use_list(TB_Function* f, TB_TemporaryStorage* tls) {
    Use** users = tb_tls_push(tls, f->node_count * sizeof(Use*));

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            TB_FOR_INPUT_IN_NODE(it, f, n) {
                Use* new_edge = tb_tls_push(tls, sizeof(Use));
                new_edge->next = users[it.r];
                new_edge->r = r;
                users[it.r] = new_edge;
            }
        }
    }

    return users;
}

static bool canonicalize(TB_Function* f, TB_OptimizerCtx* restrict ctx) {
    // fold, ld/st ops
    //     only try canonicalization on changed instructions.
    bool changes = ctx->changed_ir_count;
    while (ctx->changed_ir_count > 0) {
        TB_Reg r = ctx->changed_ir[--ctx->changed_ir_count];
        canonical_opt(f, ctx, r);
    }
    return changes;
}

static void schedule_function_level_opts(TB_Module* m, TB_Function* f, size_t pass_count, const TB_Pass* passes[]) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    // __debugbreak();

    size_t c = f->node_count < 100 ? 100 : f->node_count;
    TB_OptimizerCtx ctx = {
        .changed_ir_cap = c,
        .changed_ir = tb_tls_push(tls, c * sizeof(TB_Reg))
    };

    // generate user-list (we only do this once, we incrementally rebuild it while doing passes)
    ctx.users = generate_use_list(f, tls);

    // canonicalize all instructions first
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            canonical_opt(f, &ctx, r);
        }
    }

    // run passes
    FOREACH_N(i, 0, pass_count) {
        canonicalize(f, &ctx);

        passes[i]->func_run(f, &ctx, tls);
    }

    // final simplication, just in case
    canonicalize(f, &ctx);
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
