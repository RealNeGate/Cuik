#if 0
#include "../tb_internal.h"

static bool is_outside_of_loop(TB_Function* f, const TB_Loop* l, TB_Label bb) {
    TB_Node* bb_start = &f->nodes[tb_find_reg_from_label(f, bb)];
    TB_Node* bb_end = &f->nodes[bb_start->label.terminator];

    if (bb_end->type == TB_GOTO) return false;

    TB_Label potential_exit = bb_end->goto_.label;
    loop(i, l->body_count) {
        if (l->body[i] == potential_exit) return false;
    }

    return true;
}

bool tb_opt_refinement(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // TODO(NeGate): we might wanna make a variant that does all 3 of these just to
    // hand us the results of all of them easily
    TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);

    // find dominators
    TB_Label* doms = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    tb_get_dominators(f, preds, doms);

    // we'll be using the loops to find out the ranges of different things
    TB_LoopInfo loops = tb_get_loop_info(f, preds, doms);
    tb_function_print(f, tb_default_print_callback, stdout, false);

    FOREACH_N(i, 0, loops.count) {
        const TB_Loop* l = &loops.loops[i];

        TB_Reg loop_header = tb_find_reg_from_label(f, l->header);
        TB_Label loop_header_id = f->nodes[loop_header].label.id;

        // check for which predeccessors actually exist outside of the loop, these
        TB_Label loop_entry_id = 0;
        loop(j, preds.count[loop_header_id]) {
            if (preds.preds[loop_header_id][j] != l->backedge) {
                if (loop_entry_id != 0) {
                    loop_entry_id = 0;
                    break;
                }

                loop_entry_id = preds.preds[loop_header_id][j];
            }
        }

        if (loop_entry_id == 0) continue;

        // we have one clean predecessor which we can use to track initial loop state


        // we're just gonna work with the assumption that this is always gonna
        // be an IF with one of the edges pointing out, there's different kinds
        // of loops but we'll figure that out later.
        TB_Node* loop_gate = &f->nodes[f->nodes[loop_header].label.terminator];
        if (loop_gate->type != TB_IF) continue;

        bool exits_on_true = is_outside_of_loop(f, l, loop_gate->if_.if_true);
        bool exits_on_false = is_outside_of_loop(f, l, loop_gate->if_.if_false);

        // constant loop?
        if (exits_on_true == exits_on_false) continue;

        TB_Node* loop_cond = &f->nodes[loop_gate->if_.cond];
        switch (loop_cond->type) {
            case TB_CMP_ULT:
            break;

            default: break;
        }
    }

    tb_free_loop_info(loops);
    return false;
}
#endif