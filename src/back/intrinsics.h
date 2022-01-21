// This file is responsible for desugaring all intrinsics
#pragma once

typedef struct IntrinsicResult {
	bool success;
	TB_Register reg;
} IntrinsicResult;

static IntrinsicResult resolve_generic_intrinsic_call(TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	if (strcmp(name, "__va_start") == 0) {
		// TODO(NeGate): Remove this later because it will emotionally damage our optimizer.
		// the issue is that it blatantly accesses out of bounds and we should probably just
		// have a node for va_start in the backend instead.
		TB_Register dst = as_rvalue(func, args[0]);
		IRVal src = gen_expr(func, args[1]);
		assert(src.value_type == LVALUE);
		
		tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_member_access(func, src.reg, 8), 8);
		return (IntrinsicResult){ true };
	} else {
		return (IntrinsicResult){};
	}
}

// TODO(NeGate): this is so shit but it mostly works for the stuff 
// i've got supported. the thing is that all x86 intrinsics are forward
// declared by the intrin header files and they are all within reserved
// identifier space because they're extern and start with underscore so
// we can assume that it's an x86 intrinsic by the time it reaches this
// code.
static IntrinsicResult resolve_x86_intrinsic_call(TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	const char* op_start = name + 4;
	const char* type_start = name + 4;
	do { type_start++; } while (*type_start && *type_start != '_');
	
	/*TB_DataType dt;
	int align = 0;
	if (memcmp(type_start, "_ps", 4) == 0) {
		dt = tb_vector_type(TB_F32, 4);
		align = 16;
	} else if (memcmp(type_start, "_pd", 4) == 0) {
		dt = tb_vector_type(TB_F64, 2);
		align = 16;
	} else if (memcmp(type_start, "_ss", 4) == 0) {
		dt = tb_vector_type(TB_F32, 1);
		align = 4;
	} else if (memcmp(type_start, "_sd", 4) == 0) {
		dt = tb_vector_type(TB_F64, 1);
		align = 8;
	} else {
		printf("Unknown intrinsic: %s\n", name);
		abort();
	}*/
	
	// unary
	TB_Register left = as_rvalue(func, args[0]);
	
	if (memcmp(op_start, "sqrt", 4) == 0) {
		return (IntrinsicResult){ true, tb_inst_x86_sqrt(func, left) };
	} else if (memcmp(op_start, "rsqrt", 5) == 0) {
		return (IntrinsicResult){ true, tb_inst_x86_rsqrt(func, left) };
	}
	
	// binary
	TB_Register right = as_rvalue(func, args[1]);
	TB_Register result = 0;
	if (memcmp(op_start, "add_", 4) == 0) {
		result = tb_inst_fadd(func, left, right);
	} else if (memcmp(op_start, "sub_", 4) == 0) {
		result = tb_inst_fsub(func, left, right);
	} else if (memcmp(op_start, "mul_", 4) == 0) {
		result = tb_inst_fmul(func, left, right);
	} else if (memcmp(op_start, "div_", 4) == 0) {
		result = tb_inst_fdiv(func, left, right);
	} else {
		printf("Unknown intrinsic: %s\n", name);
		abort();
	}
	
	return (IntrinsicResult){ true, result };
}
