
static bool identity(TB_Function* f, TB_OptQueue* queue) {
    return false;
}

TB_Pass tb_opt_identity(void) {
    return (TB_Pass){ .name = "Identity", .func_run = identity };
}
