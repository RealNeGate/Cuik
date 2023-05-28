#include "../tb_internal.h"
#include <stdarg.h>

// NULL means no change, non-NULL will allow the neighbors to re-evaluate the folding engine.
typedef TB_Node* (*FoldOp)(TB_Function* f, TB_Node* n);

// unity build with all the passes
#include "dce.h"
#include "fold.h"
#include "canonical.h"
// #include "mem2reg.h"
#include "libcalls.h"

// inspired by the Hotspot's iter
typedef struct {
    DynArray(TB_Node*) queue;
    NL_Map(TB_Node*, int) lookup;
} Folder;

static bool folder_enqueue(Folder* restrict fold, TB_Node* n) {
    ptrdiff_t search = nl_map_get(fold->lookup, n);
    if (search >= 0) {
        return false;
    }

    size_t i = dyn_array_length(fold->queue);
    dyn_array_put(fold->queue, n);
    nl_map_put(fold->lookup, n, i);
    return true;
}

static void fill_queue(Folder* restrict fold, TB_Node* n) {
    if (!folder_enqueue(fold, n)) {
        return;
    }

    FOREACH_N(i, 0, n->input_count) {
        fill_queue(fold, n->inputs[i]);
    }

    // walk successors for regions
    if (n->type == TB_START || n->type == TB_REGION) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        FOREACH_N(i, 0, r->succ_count) {
            fill_queue(fold, r->succ[i]);
        }

        fill_queue(fold, r->end);
    }
}

static TB_Node* do_fold_node(TB_Function* f, TB_Node* n) {
    bool redirect = false;
    FOREACH_N(i, 0, n->input_count) if (n->inputs[i]->type == TB_PASS) {
        n->inputs[i] = n->inputs[i]->inputs[0];
        redirect = true;
    }

    if (redirect) {
        return n;
    }

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
        return do_int_fold(f, n);

        // no changes
        default: return NULL;
    }
}

static void do_fold(Folder* restrict fold, TB_Function* f) {
    if (fold->lookup == NULL) {
        nl_map_create(fold->lookup, f->node_count);
    }

    // generate work list (put everything)
    fill_queue(fold, f->start_node);

    // printf("canonical:\n");
    while (dyn_array_length(fold->queue) > 0) {
        // pull from worklist
        TB_Node* n = dyn_array_pop(fold->queue);
        nl_map_remove(fold->lookup, n);

        // try peephole
        // printf("  - work: %p (%s)\n", n, tb_node_get_name(n));
        TB_Node* progress = do_fold_node(f, n);
        if (progress == NULL) {
            // no changes
            continue;
        }

        // printf("    - success!\n");

        // push new value
        folder_enqueue(fold, progress);

        // push inputs to worklist
        FOREACH_N(i, 0, n->input_count) {
            folder_enqueue(fold, n->inputs[i]);
        }

        // TODO(NeGate): push outputs to worklist
    }

    // tb_function_print(f, tb_default_print_callback, stdout);
}

static void schedule_function_level_opts(TB_Module* m, TB_Function* f, size_t pass_count, const TB_FunctionPass passes[]) {
    // run passes
    Folder fold = { 0 };
    FOREACH_N(i, 0, pass_count) {
        CUIK_TIMED_BLOCK("fold") do_fold(&fold, f);

        CUIK_TIMED_BLOCK_ARGS("Pass", f->super.name) {
            passes[i](f);
        }
    }

    CUIK_TIMED_BLOCK("fold") do_fold(&fold, f);

    arena_clear(&tb__arena);
    nl_map_free(fold.lookup);
    dyn_array_destroy(fold.queue);
}

void tb_module_optimize(TB_Module* m, size_t pass_count, const TB_FunctionPass passes[]) {
    TB_FOR_FUNCTIONS(f, m) {
        schedule_function_level_opts(m, f, pass_count, passes);
    }
}
