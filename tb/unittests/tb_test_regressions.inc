#include "util.inc"

static int test_regression_module_arena(void) {
  tb_module_destroy(tb_module_create(tb_test_arch, tb_test_system,
                                     &tb_test_feature_set, 0));
  tb_module_destroy(tb_module_create(tb_test_arch, tb_test_system,
                                     &tb_test_feature_set, 0));

  // We're testing for segfault.
  return 1;
}

static int test_regression_link_global(void) {
  //  FIXME
  //  Make sure this test is correct.
  return 1;

  TB_Module *module = tb_module_create(tb_test_arch, tb_test_system,
                                       &tb_test_feature_set, 0);
  TB_Global *global = tb_global_create(module, -1, "global", NULL,
                                       TB_LINKAGE_PRIVATE);
  TB_Linker *linker = tb_linker_create(tb_test_exe_type,
                                       tb_test_arch);
  tb_linker_append_module(linker, module);
  tb_module_destroy(module);
  tb_linker_destroy(linker);
  return 1;
}
