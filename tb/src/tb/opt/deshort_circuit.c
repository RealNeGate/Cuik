#include "../tb_internal.h"

bool tb_opt_deshort_circuit(TB_Function* f) {
    return false;

    #if 0
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // Calculate all the immediate predecessors
    // First BB has no predecessors
    int* pred_count; // [label_count]
    TB_Label** preds; // [label_count][pred_count[i]]
    {
        pred_count = tb_tls_push(tls, f->label_count * sizeof(int));
        preds = tb_tls_push(tls, f->label_count * sizeof(TB_Label*));

        // entry label has no predecessors
        pred_count[0] = 0;
        preds[0] = NULL;

        loop_range(j, 1, f->label_count) {
            preds[j] = (TB_Label*)tb_tls_push(tls, 0);
            tb_calculate_immediate_predeccessors(f, tls, j, &pred_count[j]);
        }
    }

    // We're trying to combine conditions which were short circuit eval
    // into unconditional:
    // (a && b)  =>  (a & b)
    FOREACH_N(i, 1, f->label_count) if (pred_count[i] == 1) {
        // TODO(NeGate): We're paying a hefty performance cost by calling
        // tb_find_reg_from_label so much... ish
        TB_Reg self = tb_find_reg_from_label(f, i);
        TB_Reg self_terminator = f->nodes.payload[self].label.terminator;

        TB_Reg parent = tb_find_reg_from_label(f, preds[i][0]);
        TB_Reg parent_terminator = f->nodes.payload[parent].label.terminator;

        // we want simple IFs
        if (f->nodes.type[self_terminator] != TB_IF) continue;
        if (f->nodes.type[parent_terminator] != TB_IF) continue;

        // If we are going to merge their conditions we need
        // to guarentee that there's no side effects that might
        // be affected.
        bool has_side_effects = false;
        loop_range(j, self+1, self_terminator-1) {
            if (TB_IS_NODE_SIDE_EFFECT(f->nodes.type[j])) {
                has_side_effects = true;
                break;
            }
        }

        if (has_side_effects) continue;

        // We're ready to join the conditions
        TB_Reg cond1 = f->nodes.payload[self_terminator].if_.cond;
        TB_Reg cond2 = f->nodes.payload[parent_terminator].if_.cond;

        // NOTE(NeGate): These actually don't change i still store them
        // because the terminator might be shifted around.
        TB_Label true_path = f->nodes.payload[self_terminator].if_.if_true;
        TB_Label false_path = f->nodes.payload[self_terminator].if_.if_false;

        // convert if into a GOTO
        f->nodes.type[parent_terminator] = TB_GOTO;
        f->nodes.dt[parent_terminator] = TB_TYPE_PTR;
        f->nodes.payload[parent_terminator].goto_.label = i;

        // insert register for new AND
        TB_Reg join_reg = self_terminator;
        tb_insert_op(f, join_reg);

        // We update the register references we're still using
        self_terminator++;

        // create boolean AND joiner
        f->nodes.type[join_reg] = TB_AND;
        f->nodes.dt[join_reg] = TB_TYPE_BOOL;
        f->nodes.payload[join_reg].i_arith.arith_behavior = TB_ASSUME_NUW;
        f->nodes.payload[join_reg].i_arith.a = cond1;
        f->nodes.payload[join_reg].i_arith.b = cond2;

        f->nodes.type[self_terminator] = TB_IF;
        f->nodes.dt[self_terminator] = TB_TYPE_PTR;
        f->nodes.payload[self_terminator].if_.cond = join_reg;
        f->nodes.payload[self_terminator].if_.if_true = true_path;
        f->nodes.payload[self_terminator].if_.if_false = false_path;

        // We only modify one of these per pass because
        // we need to recalculate a lot of info once it's
        // done.
        return 1;
    }

    return 0;
    #endif
}
