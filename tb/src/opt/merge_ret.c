#include "../tb_internal.h"

static bool merge_rets(TB_Function* f) {
    int count = 0;
    TB_PhiInput* inputs = NULL;

    TB_Label endpoint = f->bb_count;
    TB_DataType dt = TB_TYPE_VOID;
    TB_Node* the_goto_we_might_convert_back_if_we_fail = f->nodes;

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_RET) {
                int index = count++;
                inputs = tb_platform_heap_realloc(inputs, count * sizeof(TB_PhiInput));
                inputs[index] = (TB_PhiInput){ bb, n->ret.value };

                dt = n->dt;

                n->type = TB_GOTO;
                n->dt = TB_TYPE_VOID;
                n->goto_.label = endpoint;
                the_goto_we_might_convert_back_if_we_fail = n;
            }
        }
    }

    if (count > 1) {
        TB_Label bb = tb_basic_block_create(f);

        // reg_base + 0  a = phi(...)
        // reg_base + 1  ret a
        TB_Reg reg_base = f->node_count;
        tb_function_reserve_nodes(f, 2);
        f->node_count += 2;

        OPTIMIZER_LOG(reg_base, "Insert new PHI node");

        f->bbs[bb].start = reg_base;
        f->bbs[bb].end = reg_base + 1;

        f->nodes[reg_base] = (TB_Node){
            .type = TB_PHIN,
            .next = reg_base + 1,
            .dt = dt,
            .phi = (struct TB_NodePhi){ .count = count, .inputs = inputs },
        };

        f->nodes[reg_base + 1] = (TB_Node){
            .type = TB_RET,
            .dt = dt,
            .ret = (struct TB_NodeReturn){ reg_base },
        };

        return true;
    } else {
        if (inputs != NULL) {
            TB_Node* n = the_goto_we_might_convert_back_if_we_fail;
            n->type = TB_RET;
            n->dt = dt;
            n->ret.value = inputs[0].val;

            tb_platform_heap_free(inputs);
        }

        return false;
    }
}

TB_API TB_Pass tb_opt_merge_rets(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "MergeReturns",
        .func_run = merge_rets,
    };
}
