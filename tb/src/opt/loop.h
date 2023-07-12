
bool tb_funcopt_loop(TB_FuncOpt* opt) {
    TB_Function* f = opt->f;

    TB_PostorderWalk order = tb_function_get_postorder(f);
    TB_Dominators doms = tb_get_dominators(f, order);
    TB_LoopInfo loops = tb_get_loop_info(f, order, doms);

    return false;
}
