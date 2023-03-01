#include "../tb_internal.h"

static bool dead_block_elim(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    // TODO(NeGate): clean this up for speed purposes :(
    bool changes = false;
    bool local_changes;
    do {
        local_changes = false;

        TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);
        int kill_count = 0;
        TB_Reg* mark_to_kill = tb_tls_push(tls, 0);

        TB_FOR_BASIC_BLOCK(bb, f) {
            if (bb > 0 && f->bbs[bb].end != 0 && preds.count[bb] == 0) {
                tb_tls_push(tls, sizeof(int));
                mark_to_kill[kill_count++] = bb;
            }
        }

        // actually delete them
        FOREACH_N(i, 0, kill_count) {
            f->bbs[mark_to_kill[i]] = (TB_BasicBlock){ 0 };
        }

        local_changes = (kill_count > 0);
        changes |= local_changes;
        tb_free_temp_predeccesors(tls, preds);
    } while (local_changes);

    return changes;
}

TB_API TB_Pass tb_opt_dead_block_elim(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "DeadBlockElimination",
        .func_run = dead_block_elim,
    };
}
