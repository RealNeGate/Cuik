#include "../tb_internal.h"

// We just move them up because it's slightly easier to think about them
static bool hoist_locals(TB_Function* f) {
    size_t locals_to_move = 0;

    for (TB_Label bb = 1; bb < f->bb_count; bb++) {
        TB_FOR_NODE(r, f, bb) {
            locals_to_move += (f->nodes[r].type == TB_LOCAL);
        }
    }

    if (locals_to_move == 0) {
        return false;
    }

    // check where in the entry label we should place the locals
    //
    // place to start putting all the locals
    // must go after the parameters
    TB_Reg local_basepoint = 1, prev = 1;
    TB_FOR_NODE(r, f, 0) {
        TB_Node* n = &f->nodes[r];

        if (n->type != TB_PARAM && n->type != TB_PARAM_ADDR && !TB_IS_NODE_TERMINATOR(n->type)) {
            local_basepoint = (n - f->nodes);
        }
        prev = r;
    }

    if (local_basepoint == 1) local_basepoint = prev;

    // hoist all locals which aren't in the entry label
    for (TB_Label bb = 1; bb < f->bb_count; bb++) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_LOCAL) {
                // move to the entry block
                TB_Reg new_reg = tb_function_insert_after(f, 0, local_basepoint);
                TB_Node* new_node = &f->nodes[new_reg];
                n = &f->nodes[r];

                TB_Reg new_reg_next = new_node->next;
                memcpy(new_node, n, sizeof(TB_Node));
                new_node->next = new_reg_next;

                n->type = TB_NULL;
                locals_to_move--;

                OPTIMIZER_LOG(n - f->nodes, "hoisted local");
                tb_function_find_replace_reg(f, n - f->nodes, new_reg);

                if (locals_to_move == 0) {
                    // ran out of stuff to do, early exit
                    return true;
                }
            }
        }
    }

    return true;
}

TB_API TB_Pass tb_opt_hoist_locals(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "HoistLocals",
        .func_run = hoist_locals,
    };
}
