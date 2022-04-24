#include "targets.h"
#include <front/sema.h>

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

static void set_defines(CPP_Context* cpp) {
	cpp_define_empty(cpp, "_CUIK_TARGET_64BIT_");
	
	//cpp_define_empty(cpp, "_X86_");
	cpp_define_empty(cpp, "_M_X64");
	cpp_define_empty(cpp, "_AMD64_");
	cpp_define_empty(cpp, "_M_AMD64");

	cpp_define_empty(cpp, "_WIN32");
	cpp_define_empty(cpp, "_WIN64");

	// stdatomic.h lock free
	cpp_define(cpp, "__CUIK_ATOMIC_BOOL_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_CHAR_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_CHAR16_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_CHAR32_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_WCHAR_T_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_SHORT_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_INT_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_LONG_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_LLONG_LOCK_FREE", "1");
	cpp_define(cpp, "__CUIK_ATOMIC_POINTER_LOCK_FREE", "1");
}

// on Win64 all structs that have a size of 1,2,4,8
// or any scalars are passed via registers
static bool win64_should_pass_via_reg(TranslationUnit* tu, TypeIndex type_index) {
	const Type* type = &tu->types[type_index];

	if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
		switch (type->size) {
		case 1: case 2: case 4: case 8: return true;
		default: return false;
		}
	} else {
		return true;
	}
}

static TB_FunctionPrototype* create_prototype(TranslationUnit* tu, TypeIndex type_index) {
	Type* restrict type = &tu->types[type_index];

	// decide if return value is aggregate
	bool is_aggregate_return = !win64_should_pass_via_reg(tu, type->func.return_type);

	// parameters
	ParamIndex param_list = type->func.param_list;
	ParamIndex param_count = type->func.param_count;
	
	// estimate parameter count		
	size_t real_param_count = (is_aggregate_return ? 1 : 0) + param_count;

	TB_DataType return_dt = TB_TYPE_PTR;
	if (!is_aggregate_return) return_dt = ctype_to_tbtype(&tu->types[type->func.return_type]);

	TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, return_dt, real_param_count, type->func.has_varargs);

	if (is_aggregate_return) {
		tb_prototype_add_param(proto, TB_TYPE_PTR);
	}

	for (size_t i = 0; i < param_count; i++) {
		Param* p = &tu->params[param_list + i];

		if (win64_should_pass_via_reg(tu, p->type)) {
			Type* param_type = &tu->types[p->type];
			TB_DataType dt = ctype_to_tbtype(param_type);
			
			assert(dt.width < 8);
			tb_prototype_add_param(proto, dt);
		} else {
			tb_prototype_add_param(proto, TB_TYPE_PTR);
		}
	}

	return proto;
}

static bool pass_return(TranslationUnit* tu, TypeIndex type_index) {
	Type* type = &tu->types[type_index];
	return (type->kind == KIND_STRUCT || type->kind == KIND_UNION);
}

static int deduce_parameter_usage(TranslationUnit* tu, TypeIndex type_index) {
	return 1;
}

static int pass_parameter(TranslationUnit* tu, TB_Function* func, ExprIndex e, bool is_vararg, TB_Reg* out_param) {
	TypeIndex arg_type_index = tu->exprs[e].type;
	Type* arg_type = &tu->types[arg_type_index];

	if (!win64_should_pass_via_reg(tu, arg_type_index)) {
		// const pass-by-value is considered as a const ref
		// since it doesn't mutate
		IRVal arg = irgen_expr(tu, func, e);
		TB_Reg arg_addr = TB_NULL_REG;
		switch (arg.value_type) {
			case LVALUE: arg_addr = arg.reg; break;
			case LVALUE_FUNC: arg_addr = tb_inst_get_func_address(func, arg.func); break;
			case LVALUE_EFUNC: arg_addr = tb_inst_get_extern_address(func, arg.ext); break;
			default: break;
		}
		assert(arg_addr);
		
		// TODO(NeGate): we might wanna define some TB instruction
		// for killing locals since some have really limited lifetimes
		TB_CharUnits size = arg_type->size;
		TB_CharUnits align = arg_type->align;
	
		if (arg_type->is_const) {
			out_param[0] = arg_addr;
		} else {
			TB_Reg temp_slot = tb_inst_local(func, size, align);
			TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
	
			tb_inst_memcpy(func, temp_slot, arg_addr, size_reg, align);	
			
			out_param[0] = temp_slot;
		}

		return 1;
	} else {
		if (arg_type->kind == KIND_STRUCT ||
			arg_type->kind == KIND_UNION) {
			// Convert aggregate into TB scalar
			IRVal arg = irgen_expr(tu, func, e);
			TB_Reg arg_addr = TB_NULL_REG;
			switch (arg.value_type) {
			case LVALUE: arg_addr = arg.reg; break;
			case LVALUE_FUNC: arg_addr = tb_inst_get_func_address(func, arg.func); break;
			case LVALUE_EFUNC: arg_addr = tb_inst_get_extern_address(func, arg.ext); break;
			default: break;
			}
			assert(arg_addr);
			
			TB_DataType dt = TB_TYPE_VOID;
			switch (arg_type->size) {
			case 1: dt = TB_TYPE_I8; break;
			case 2: dt = TB_TYPE_I16; break;
			case 4: dt = TB_TYPE_I32; break;
			case 8: dt = TB_TYPE_I64; break;
			default: break;
			}

			out_param[0] = tb_inst_load(func, dt, arg_addr, arg_type->align);
			return 1;
		} else {
			TB_Reg arg = irgen_as_rvalue(tu, func, e);
			TB_DataType dt = tb_function_get_node(func, arg)->dt;
		
			if (is_vararg && dt.type == TB_F64 && dt.width == 0) {
				// convert any float variadic arguments into integers
				arg = tb_inst_bitcast(func, arg, TB_TYPE_I64);
			}
		
			out_param[0] = arg;
			return 1;
		}
	}
}

// TODO(NeGate): Add some type checking utilities to match against a list of types since that's kinda important :p
static TypeIndex type_check_builtin(TranslationUnit* tu, SourceLocIndex loc, const char* name, int arg_count, ExprIndex* args) {
	if (strcmp(name, "__c11_atomic_compare_exchange_strong") == 0) {
		// type check arguments
		return TYPE_BOOL;
	} else if (strcmp(name, "__builtin_mul_overflow") == 0) {
		if (arg_count != 3) {
			sema_error(loc, "%s requires 3 arguments", name);
			return 0;
		}

		TypeIndex type = sema_expr(tu, args[0]);
		if (tu->types[type].kind < KIND_CHAR || tu->types[type].kind > KIND_LONG) {
			sema_error(loc, "%s can only be applied onto integers");
			goto failure;
		}
		
		for (size_t i = 1; i < arg_count; i++) {
			TypeIndex arg_type = sema_expr(tu, args[i]);

			if (i == 2) {
				if (tu->types[arg_type].kind != KIND_PTR) {
					type_as_string(tu, sizeof(temp_string0), temp_string0, type);
					sema_error(tu->exprs[args[i]].loc, "Expected pointer to '%s' for the 3rd argument", temp_string0);
					goto failure;
				}

				arg_type = tu->types[arg_type].ptr_to;
			}

			if (!type_compatible(tu, arg_type, type, args[i])) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
				type_as_string(tu, sizeof(temp_string1), temp_string1, type);
				
				SourceLocIndex loc = tu->exprs[args[i]].loc;
				sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
				goto failure;
			}
		
			TypeIndex cast_type = (i == 2) ? new_pointer(tu, type) : type;
			tu->exprs[args[i]].cast_type = cast_type;
		}

		failure:
		return TYPE_BOOL;
	}

	return 0;
}

static TB_Register compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	if (strcmp(name, "__c11_atomic_thread_fence") == 0) {
		printf("TODO __c11_atomic_thread_fence!");
		abort();
	} else if (strcmp(name, "__c11_atomic_signal_fence") == 0) {
		printf("TODO __c11_atomic_signal_fence!");
		abort();
	} else if (strcmp(name, "__builtin_unreachable") == 0) {
		printf("TODO __builtin_unreachable!");
		abort();
	} else if (strcmp(name, "__builtin_expect") == 0) {
		printf("TODO __builtin_expect!");
		abort();
	} else if (strcmp(name, "__debugbreak") == 0) {
		tb_inst_debugbreak(func);
		return 0;
	} else if (strcmp(name, "__va_start") == 0) {
		// TODO(NeGate): Remove this later because it will emotionally damage our optimizer.
		// the issue is that it blatantly accesses out of bounds and we should probably just
		// have a node for va_start in the backend instead.
		TB_Register dst = irgen_as_rvalue(tu, func, args[0]);
		IRVal src = irgen_expr(tu, func, args[1]);
		assert(src.value_type == LVALUE);

		tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_va_start(func, src.reg), 8);
		return 0;
	} else if (strcmp(name, "_mul128") == 0) {
		return tb_inst_uint(func, TB_TYPE_I64, 0);
	} else if (strcmp(name, "_umul128") == 0) {
		return tb_inst_uint(func, TB_TYPE_I64, 0);
	} else {
		return 0;
	}
}

TargetDescriptor get_x64_target_descriptor() {
	BuiltinBinding* builtins = NULL;

	// gcc/clang
	shput(builtins, "__builtin_expect", 1);
	shput(builtins, "__builtin_unreachable", 1);
	shput(builtins, "__builtin_mul_overflow", 1);
	shput(builtins, "__c11_atomic_compare_exchange_strong", 1);
	shput(builtins, "__c11_atomic_thread_fence", 1);
	shput(builtins, "__c11_atomic_signal_fence", 1);
	shput(builtins, "__c11_atomic_is_lock_free", 1);
	shput(builtins, "__c11_atomic_load", 1);
	shput(builtins, "__c11_atomic_store", 1);
	shput(builtins, "__c11_atomic_exchange", 1);
	shput(builtins, "__c11_atomic_compare_exchange_strong", 1);
	shput(builtins, "__c11_atomic_compare_exchange_weak", 1);
	shput(builtins, "__c11_atomic_fetch_add", 1);
	shput(builtins, "__c11_atomic_fetch_sub", 1);
	shput(builtins, "__c11_atomic_fetch_or", 1);
	shput(builtins, "__c11_atomic_fetch_xor", 1);
	shput(builtins, "__c11_atomic_fetch_and", 1);

	// msvc intrinsics
	shput(builtins, "__debugbreak", 1);
	shput(builtins, "__va_start", 1);
	shput(builtins, "_umul128", 1);
	shput(builtins, "_mul128", 1);

	return (TargetDescriptor) {
		.builtin_func_map = builtins,
		.set_defines = set_defines,
		.create_prototype = create_prototype,
		.pass_return = pass_return,
		.deduce_parameter_usage = deduce_parameter_usage,
		.pass_parameter = pass_parameter,
		.type_check_builtin = type_check_builtin,
		.compile_builtin = compile_builtin
	};
}
