#include <stdio.h>
#include <tb.h>

int main() {
    TB_Module* m = tb_module_create_for_host(&(TB_FeatureSet){ 0 }, false);

    TB_Function* f = tb_function_create(m, "main", TB_LINKAGE_PUBLIC, TB_COMDAT_NONE);

    // set prototype
    static TB_PrototypeParam params[] = {
        { TB_TYPE_I64 }
    };
    tb_function_set_prototype(f, tb_prototype_create(m, TB_STDCALL, 1, params, 0, NULL, false));

    TB_Node* region = tb_inst_get_control(f);
    TB_Node* t_case = tb_inst_region(f);
    TB_Node* f_case = tb_inst_region(f);
    tb_inst_if(f, tb_inst_param(f, 0), t_case, f_case);

    // if (x) return 2
    {
        tb_inst_set_control(f, t_case);
        TB_Node* number1 = tb_inst_uint(f, TB_TYPE_I64, 2);
        tb_inst_ret(f, 1, &number1);
    }
    // else return 9
    {
        tb_inst_set_control(f, f_case);
        TB_Node* number2 = tb_inst_uint(f, TB_TYPE_I64, 4);
        tb_inst_ret(f, 1, &number2);
    }

    tb_function_print(f, tb_default_print_callback, stdout);
    return 0;
}
