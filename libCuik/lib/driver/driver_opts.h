static const TB_OptFn passes_O1[] = {
    tb_opt_hoist_locals,
    tb_opt_merge_rets,

    tb_opt_instcombine,
    tb_opt_dead_expr_elim,
    tb_opt_dead_block_elim,
    tb_opt_subexpr_elim,

    tb_opt_mem2reg,
    tb_opt_remove_pass_nodes,
    tb_opt_instcombine,
    tb_opt_remove_pass_nodes,
    tb_opt_dead_expr_elim,
    tb_opt_dead_block_elim,
    tb_opt_subexpr_elim,
    tb_opt_remove_pass_nodes,
    tb_opt_compact_dead_regs,

    tb_opt_remove_pass_nodes,
    tb_opt_compact_dead_regs,
};
