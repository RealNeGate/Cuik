#include "../tb_internal.h"
#include <log.h>
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
    log_debug("do_fold on %s", f->super.name);

    if (fold->lookup == NULL) {
        nl_map_create(fold->lookup, f->node_count);
    }

    // generate work list (put everything)
    fill_queue(fold, f->start_node);

    while (dyn_array_length(fold->queue) > 0) {
        // pull from worklist
        TB_Node* n = dyn_array_pop(fold->queue);
        nl_map_remove(fold->lookup, n);

        // try peephole
        TB_Node* progress = do_fold_node(f, n);
        if (progress == NULL) {
            // no changes
            continue;
        }

        // push new value
        folder_enqueue(fold, progress);

        // push inputs to worklist
        FOREACH_N(i, 0, n->input_count) {
            folder_enqueue(fold, n->inputs[i]);
        }
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

void tb_function_apply_passes(TB_PassManager* manager, TB_Passes passes, TB_Function* f) {
    assert(!passes.module_level);
    log_debug("run %d passes for %s", manager->count, f->super.name);

    const TB_Pass* restrict arr = manager->passes;

    // run passes
    Folder fold = { 0 };
    FOREACH_N(i, passes.start, passes.end) {
        do_fold(&fold, f);

        CUIK_TIMED_BLOCK_ARGS(arr[i].name, f->super.name) {
            arr[i].func_run(f);
        }
    }

    do_fold(&fold, f);

    arena_clear(&tb__arena);
    nl_map_free(fold.lookup);
    dyn_array_destroy(fold.queue);
}

void tb_module_apply_passes(TB_PassManager* manager, TB_Passes passes, TB_Module* m) {
    log_debug("run %d passes for entire module %p", passes.end - passes.start, m);

    const TB_Pass* restrict arr = manager->passes;
    FOREACH_N(i, passes.start, passes.end) {
        arr[i].mod_run(m);
    }

    arena_clear(&tb__arena);
}
