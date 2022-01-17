// This file is responsible for desugaring all intrinsics
#pragma once

typedef struct IntrinsicResult {
	bool success;
	TB_Register reg;
} IntrinsicResult;

static IntrinsicResult resolve_generic_intrinsic_call(TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	if (strcmp(name, "__va_start") == 0) {
		TB_Register dst = as_rvalue(func, args[0]);
		IRVal src = gen_expr(func, args[1]);
		assert(src.value_type == LVALUE);
		
		tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_member_access(func, src.reg, 8), 8);
		return (IntrinsicResult){ true };
	} else {
		return (IntrinsicResult){};
	}
}

static IntrinsicResult resolve_x86_intrinsic_call(TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	return (IntrinsicResult){};
}
