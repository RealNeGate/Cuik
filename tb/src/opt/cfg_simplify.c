#include "../tb_internal.h"

static TB_Label find_threadable_target(TB_Function* f, TB_Label target) {
    // last safe bet as a threadable target
    TB_Label result = -1;
    for (;;) {
        TB_Reg target_end = f->bbs[target].end;
        if (f->nodes[target_end].type != TB_GOTO) goto done;

        TB_FOR_NODE(r, f, target) {
            if (f->nodes[r].type == TB_LINE_INFO) continue;
            if (f->nodes[r].type == TB_NULL) continue;
            if (f->nodes[r].type == TB_GOTO) continue;

            goto done;
        }

        target = result = f->nodes[f->bbs[target].end].goto_.label;
    }

    done:
    return result;
}

static bool cfg_simplify(TB_Function* f) {
    int changes = 0;
    FOREACH_REVERSE_N(bb, 0, f->bb_count) {
        retry:
        TB_Reg end = f->bbs[bb].end;

        if (f->nodes[end].type == TB_GOTO) {
            TB_Label target = find_threadable_target(f, f->nodes[end].goto_.label);
            if (target >= 0) {
                f->nodes[end].goto_.label = target;
                changes++;
            }
        } else if (f->nodes[end].type == TB_IF) {
            TB_Label target_t = find_threadable_target(f, f->nodes[end].if_.if_true);
            TB_Label target_f = find_threadable_target(f, f->nodes[end].if_.if_false);

            if (target_t >= 0 && target_t == target_f) {
                f->nodes[end].type = TB_GOTO;
                f->nodes[end].goto_.label = target_t;
                changes++;
                goto retry;
            } else if (target_t >= 0) {
                f->nodes[end].if_.if_true = target_t;
                changes++;
            } else if (target_f >= 0) {
                f->nodes[end].if_.if_false = target_f;
                changes++;
            }
        }
    }

    return changes;
}

TB_API TB_Pass tb_opt_cfg_simplify(void) {
    return (TB_Pass){
        .name = "SimplifyCFG",
        .func_run = cfg_simplify,
    };
}