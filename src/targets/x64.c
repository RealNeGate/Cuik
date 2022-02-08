#include "targets.h"

static void set_defines(CPP_Context* cpp) {
	//cpp_define_empty(cpp, "_X86_");
	cpp_define_empty(cpp, "_M_X64");
	cpp_define_empty(cpp, "_AMD64_");
	cpp_define_empty(cpp, "_M_AMD64");
	
	cpp_define_empty(cpp, "_WIN32");
	cpp_define_empty(cpp, "_WIN64");
}

TB_Register compile_builtin(TranslationUnit* tu, TB_Function* func, const char* name, int arg_count, ExprIndex* args) {
	if (strcmp(name, "__debugbreak") == 0) {
		tb_inst_debugbreak(func);
		return 0;
	} else if (strcmp(name, "__va_start") == 0) {
		// TODO(NeGate): Remove this later because it will emotionally damage our optimizer.
		// the issue is that it blatantly accesses out of bounds and we should probably just
		// have a node for va_start in the backend instead.
		TB_Register dst = irgen_as_rvalue(tu, func, args[0]);
		IRVal src = irgen_expr(tu, func, args[1]);
		assert(src.value_type == LVALUE);
		
		tb_inst_store(func, TB_TYPE_PTR, dst, tb_inst_member_access(func, src.reg, 8), 8);
		return 0;
	} else if (strcmp(name, "_mul128") == 0) {
		return tb_inst_uint(func, TB_TYPE_I64, 0);
	} else if (strcmp(name, "_umul128") == 0) {
		return tb_inst_uint(func, TB_TYPE_I64, 0);
	}
	
	if (memcmp(name, "_mm_", 4) != 0) {
		assert(0);
	}
	
	// x86 intrinsics are prefixed with _mm_ and suffix that represents
	// the type:
	// 
	// SUFFIX     DESCRIPTION                     BACKEND TYPE
	// =====================================================
	// _ss        scalar single                   f32 x  1
	// _sd        scalar double                   f64 x  1
	// _ps        packed single                   f32 x  4
	// _pd        packed double                   f64 x  2
	// _epu64     extended packed unsigned long   i64 x  2
	// _epi64     extended packed signed long     i64 x  2
	// _epu32     extended packed unsigned int    i32 x  4
	// _epi32     extended packed signed int      i32 x  4
	// _epu16     extended packed unsigned short  i16 x  8
	// _epi16     extended packed signed short    i16 x  8
	// _epu16     extended packed unsigned byte   i8  x  16
	// _epi16     extended packed signed byte     i8  x  16
	// _si32      32-bit integer                  i32 x  1
	// _si128     128-bit integer                 i32 x  4
	const char* op_start = name + 4;
	const char* type_start = name + 4;
	do { type_start++; } while (*type_start && *type_start != '_');
	
	TB_DataType dt;
	bool is_integer = false;
	bool is_signed = false;
	if (memcmp(type_start, "_ps", 4) == 0) {
		dt = tb_vector_type(TB_F32, 4);
	} else if (memcmp(type_start, "_pd", 4) == 0) {
		dt = tb_vector_type(TB_F64, 2);
	} else if (memcmp(type_start, "_ss", 4) == 0) {
		dt = tb_vector_type(TB_F32, 1);
	} else if (memcmp(type_start, "_sd", 4) == 0) {
		dt = tb_vector_type(TB_F64, 1);
	} else if (memcmp(type_start, "_epi32", 6) == 0 || memcmp(type_start, "_epu32", 6) == 0) {
		dt = tb_vector_type(TB_I32, 4);
		is_integer = true;
		is_signed = (type_start[3] == 'i');
	} else if (memcmp(type_start, "_epi16", 6) == 0 || memcmp(type_start, "_epu16", 6) == 0) {
		dt = tb_vector_type(TB_I16, 8);
		is_integer = true;
		is_signed = (type_start[3] == 'i');
	} else if (memcmp(type_start, "_epi8", 6) == 0 || memcmp(type_start, "_epu8", 6) == 0) {
		dt = tb_vector_type(TB_I8, 16);
		is_integer = true;
		is_signed = (type_start[3] == 'i');
	} else if (memcmp(type_start, "_si32", 6) == 0) {
		dt = tb_vector_type(TB_I32, 1);
		is_integer = true;
		is_signed = true;
	} else {
		printf("Unknown intrinsic: %s\n", name);
		abort();
	}
	
	((void)is_signed);
	
	// unary
	TB_Register left = irgen_as_rvalue(tu, func, args[0]);
	TB_DataType left_dt = tb_function_get_node(func, left)->dt;
	if (left_dt.type != dt.type || left_dt.width != dt.width) {
		left = tb_inst_bitcast(func, left, dt);
	}
	
	if (memcmp(op_start, "sqrt", 4) == 0) {
		return tb_inst_x86_sqrt(func, left);
	} else if (memcmp(op_start, "rsqrt", 5) == 0) {
		return tb_inst_x86_rsqrt(func, left);
	}
	
	// binary
	TB_Register right = irgen_as_rvalue(tu, func, args[1]);
	TB_DataType right_dt = tb_function_get_node(func, right)->dt;
	if (right_dt.type != dt.type || right_dt.width != dt.width) {
		right = tb_inst_bitcast(func, right, dt);
	}
	
	TB_Register result = 0;
	if (memcmp(op_start, "add_", 4) == 0) {
		result = is_integer ? tb_inst_add(func, left, right, TB_ASSUME_NUW) : tb_inst_fadd(func, left, right);
	} else if (memcmp(op_start, "sub_", 4) == 0) {
		result = is_integer ? tb_inst_sub(func, left, right, TB_ASSUME_NUW) : tb_inst_fsub(func, left, right);
	} else if (memcmp(op_start, "mul_", 4) == 0) {
		result = is_integer ? tb_inst_mul(func, left, right, TB_ASSUME_NUW) : tb_inst_fmul(func, left, right);
	} else if (memcmp(op_start, "div_", 4) == 0) {
		result = is_integer ? 0 : tb_inst_fdiv(func, left, right);
	} else {
		printf("Unknown intrinsic: %s\n", name);
		abort();
	}
	
	assert(result != TB_NULL_REG);
	return result;
}

TargetDescriptor get_x64_target_descriptor() {
	BuiltinBinding* builtins = NULL;
	
	// msvc intrinsics
	shput(builtins, "__debugbreak", 1);
	shput(builtins, "__va_start", 1);
	shput(builtins, "_umul128", 1);
	shput(builtins, "_mul128", 1);
	
	// x86 intrinsics
	shput(builtins, "_mm_add_ss", 1);
	shput(builtins, "_mm_add_ps", 1);
	shput(builtins, "_mm_sub_ss", 1);
	shput(builtins, "_mm_sub_ps", 1);
	shput(builtins, "_mm_mul_ss", 1);
	shput(builtins, "_mm_mul_ps", 1);
	shput(builtins, "_mm_div_ss", 1);
	shput(builtins, "_mm_div_ps", 1);
	shput(builtins, "_mm_sqrt_ss", 1);
	shput(builtins, "_mm_sqrt_ps", 1);
	shput(builtins, "_mm_rcp_ss", 1);
	shput(builtins, "_mm_rcp_ps", 1);
	shput(builtins, "_mm_rsqrt_ss", 1);
	shput(builtins, "_mm_rsqrt_ps", 1);
	shput(builtins, "_mm_min_ss", 1);
	shput(builtins, "_mm_min_ps", 1);
	shput(builtins, "_mm_max_ss", 1);
	shput(builtins, "_mm_max_ps", 1);
	shput(builtins, "_mm_and_ps", 1);
	shput(builtins, "_mm_andnot_ps", 1);
	shput(builtins, "_mm_or_ps", 1);
	shput(builtins, "_mm_xor_ps", 1);
	
	shput(builtins, "_mm_add_sd", 1);
	shput(builtins, "_mm_add_pd", 1);
	shput(builtins, "_mm_sub_sd", 1);
	shput(builtins, "_mm_sub_pd", 1);
	shput(builtins, "_mm_mul_sd", 1);
	shput(builtins, "_mm_mul_pd", 1);
	shput(builtins, "_mm_sqrt_sd", 1);
	shput(builtins, "_mm_sqrt_pd", 1);
	shput(builtins, "_mm_div_sd", 1);
	shput(builtins, "_mm_div_pd", 1);
	shput(builtins, "_mm_min_sd", 1);
	shput(builtins, "_mm_min_pd", 1);
	shput(builtins, "_mm_max_sd", 1);
	shput(builtins, "_mm_max_pd", 1);
	shput(builtins, "_mm_and_pd", 1);
	shput(builtins, "_mm_andnot_pd", 1);
	shput(builtins, "_mm_or_pd", 1);
	shput(builtins, "_mm_xor_pd", 1);
	shput(builtins, "_mm_cmpeq_sd", 1);
	shput(builtins, "_mm_cmpeq_pd", 1);
	shput(builtins, "_mm_cmplt_sd", 1);
	shput(builtins, "_mm_cmplt_pd", 1);
	shput(builtins, "_mm_cmple_sd", 1);
	shput(builtins, "_mm_cmple_pd", 1);
	shput(builtins, "_mm_cmpgt_sd", 1);
	shput(builtins, "_mm_cmpgt_pd", 1);
	shput(builtins, "_mm_cmpge_sd", 1);
	shput(builtins, "_mm_cmpge_pd", 1);
	shput(builtins, "_mm_cmpneq_sd", 1);
	shput(builtins, "_mm_cmpneq_pd", 1);
	shput(builtins, "_mm_cmpnlt_sd", 1);
	shput(builtins, "_mm_cmpnlt_pd", 1);
	shput(builtins, "_mm_cmpnle_sd", 1);
	shput(builtins, "_mm_cmpnle_pd", 1);
	shput(builtins, "_mm_cmpngt_sd", 1);
	shput(builtins, "_mm_cmpngt_pd", 1);
	shput(builtins, "_mm_cmpnge_sd", 1);
	shput(builtins, "_mm_cmpnge_pd", 1);
	shput(builtins, "_mm_cmpord_pd", 1);
	shput(builtins, "_mm_cmpord_sd", 1);
	shput(builtins, "_mm_cmpunord_pd", 1);
	shput(builtins, "_mm_cmpunord_sd", 1);
	shput(builtins, "_mm_comieq_sd", 1);
	shput(builtins, "_mm_comilt_sd", 1);
	shput(builtins, "_mm_comile_sd", 1);
	shput(builtins, "_mm_comigt_sd", 1);
	shput(builtins, "_mm_comige_sd", 1);
	shput(builtins, "_mm_comineq_sd", 1);
	shput(builtins, "_mm_ucomieq_sd", 1);
	shput(builtins, "_mm_ucomilt_sd", 1);
	shput(builtins, "_mm_ucomile_sd", 1);
	shput(builtins, "_mm_ucomigt_sd", 1);
	shput(builtins, "_mm_ucomige_sd", 1);
	shput(builtins, "_mm_ucomineq_sd", 1);
	shput(builtins, "_mm_cvtepi32_pd", 1);
	shput(builtins, "_mm_cvtpd_epi32", 1);
	shput(builtins, "_mm_cvttpd_epi32", 1);
	shput(builtins, "_mm_cvtepi32_ps", 1);
	shput(builtins, "_mm_cvtps_epi32", 1);
	shput(builtins, "_mm_cvttps_epi32", 1);
	shput(builtins, "_mm_cvtpd_ps", 1);
	shput(builtins, "_mm_cvtps_pd", 1);
	shput(builtins, "_mm_cvtsd_ss", 1);
	shput(builtins, "_mm_cvtss_sd", 1);
	shput(builtins, "_mm_cvtsd_si32", 1);
	shput(builtins, "_mm_cvttsd_si32", 1);
	shput(builtins, "_mm_cvtsi32_sd", 1);
	shput(builtins, "_mm_unpackhi_pd", 1);
	shput(builtins, "_mm_unpacklo_pd", 1);
	shput(builtins, "_mm_movemask_pd", 1);
	shput(builtins, "_mm_shuffle_pd", 1);
	shput(builtins, "_mm_load_pd", 1);
	shput(builtins, "_mm_load1_pd", 1);
	shput(builtins, "_mm_loadr_pd", 1);
	shput(builtins, "_mm_loadu_pd", 1);
	shput(builtins, "_mm_load_sd", 1);
	shput(builtins, "_mm_loadh_pd", 1);
	shput(builtins, "_mm_loadl_pd", 1);
	shput(builtins, "_mm_set_sd", 1);
	shput(builtins, "_mm_set1_pd", 1);
	shput(builtins, "_mm_set_pd", 1);
	shput(builtins, "_mm_setr_pd", 1);
	shput(builtins, "_mm_setzero_pd", 1);
	shput(builtins, "_mm_move_sd", 1);
	shput(builtins, "_mm_store_sd", 1);
	shput(builtins, "_mm_store1_pd", 1);
	shput(builtins, "_mm_store_pd", 1);
	shput(builtins, "_mm_storeu_pd", 1);
	shput(builtins, "_mm_storer_pd", 1);
	shput(builtins, "_mm_storeh_pd", 1);
	shput(builtins, "_mm_storel_pd", 1);
	shput(builtins, "_mm_add_epi8", 1);
	shput(builtins, "_mm_add_epi16", 1);
	shput(builtins, "_mm_add_epi32", 1);
	shput(builtins, "_mm_add_epi64", 1);
	shput(builtins, "_mm_adds_epi8", 1);
	shput(builtins, "_mm_adds_epi16", 1);
	shput(builtins, "_mm_adds_epu8", 1);
	shput(builtins, "_mm_adds_epu16", 1);
	shput(builtins, "_mm_avg_epu8", 1);
	shput(builtins, "_mm_avg_epu16", 1);
	shput(builtins, "_mm_madd_epi16", 1);
	shput(builtins, "_mm_max_epi16", 1);
	shput(builtins, "_mm_max_epu8", 1);
	shput(builtins, "_mm_min_epi16", 1);
	shput(builtins, "_mm_min_epu8", 1);
	shput(builtins, "_mm_mulhi_epi16", 1);
	shput(builtins, "_mm_mulhi_epu16", 1);
	shput(builtins, "_mm_mullo_epi16", 1);
	shput(builtins, "_mm_mul_epu32", 1);
	shput(builtins, "_mm_sad_epu8", 1);
	shput(builtins, "_mm_sub_epi8", 1);
	shput(builtins, "_mm_sub_epi16", 1);
	shput(builtins, "_mm_sub_epi32", 1);
	shput(builtins, "_mm_sub_epi64", 1);
	shput(builtins, "_mm_subs_epi8", 1);
	shput(builtins, "_mm_subs_epi16", 1);
	shput(builtins, "_mm_subs_epu8", 1);
	shput(builtins, "_mm_subs_epu16", 1);
	shput(builtins, "_mm_and_si128", 1);
	shput(builtins, "_mm_andnot_si128", 1);
	shput(builtins, "_mm_or_si128", 1);
	shput(builtins, "_mm_xor_si128", 1);
	shput(builtins, "_mm_slli_si128", 1);
	shput(builtins, "_mm_slli_epi16", 1);
	shput(builtins, "_mm_sll_epi16", 1);
	shput(builtins, "_mm_slli_epi32", 1);
	shput(builtins, "_mm_sll_epi32", 1);
	shput(builtins, "_mm_slli_epi64", 1);
	shput(builtins, "_mm_sll_epi64", 1);
	shput(builtins, "_mm_srai_epi16", 1);
	shput(builtins, "_mm_sra_epi16", 1);
	shput(builtins, "_mm_srai_epi32", 1);
	shput(builtins, "_mm_sra_epi32 ", 1);
	shput(builtins, "_mm_srli_si128", 1);
	shput(builtins, "_mm_srli_epi16", 1);
	shput(builtins, "_mm_srl_epi16", 1);
	shput(builtins, "_mm_srli_epi32", 1);
	shput(builtins, "_mm_srl_epi32", 1);
	shput(builtins, "_mm_srli_epi64", 1);
	shput(builtins, "_mm_srl_epi64", 1);
	shput(builtins, "_mm_cmpeq_epi8", 1);
	shput(builtins, "_mm_cmpeq_epi16", 1);
	shput(builtins, "_mm_cmpeq_epi32", 1);
	shput(builtins, "_mm_cmpgt_epi8", 1);
	shput(builtins, "_mm_cmpgt_epi16", 1);
	shput(builtins, "_mm_cmpgt_epi32", 1);
	shput(builtins, "_mm_cmplt_epi8", 1);
	shput(builtins, "_mm_cmplt_epi16", 1);
	shput(builtins, "_mm_cmplt_epi32", 1);
	shput(builtins, "_mm_cvtsi32_si128", 1);
	shput(builtins, "_mm_cvtsi128_si32", 1);
	shput(builtins, "_mm_packs_epi16", 1);
	shput(builtins, "_mm_packs_epi32", 1);
	shput(builtins, "_mm_packus_epi16", 1);
	shput(builtins, "_mm_extract_epi16", 1);
	shput(builtins, "_mm_insert_epi16", 1);
	shput(builtins, "_mm_movemask_epi8", 1);
	shput(builtins, "_mm_shuffle_epi32", 1);
	shput(builtins, "_mm_shufflehi_epi16", 1);
	shput(builtins, "_mm_shufflelo_epi16", 1);
	shput(builtins, "_mm_unpackhi_epi8", 1);
	shput(builtins, "_mm_unpackhi_epi16", 1);
	shput(builtins, "_mm_unpackhi_epi32", 1);
	shput(builtins, "_mm_unpackhi_epi64", 1);
	shput(builtins, "_mm_unpacklo_epi8", 1);
	shput(builtins, "_mm_unpacklo_epi16", 1);
	shput(builtins, "_mm_unpacklo_epi32", 1);
	shput(builtins, "_mm_unpacklo_epi64", 1);
	shput(builtins, "_mm_load_si128", 1);
	shput(builtins, "_mm_loadu_si128", 1);
	shput(builtins, "_mm_loadl_epi64", 1);
	shput(builtins, "_mm_set_epi64x", 1);
	shput(builtins, "_mm_set_epi32", 1);
	shput(builtins, "_mm_set_epi16", 1);
	shput(builtins, "_mm_set_epi8", 1);
	shput(builtins, "_mm_set1_epi64x", 1);
	shput(builtins, "_mm_set1_epi32", 1);
	shput(builtins, "_mm_set1_epi16", 1);
	shput(builtins, "_mm_set1_epi8", 1);
	shput(builtins, "_mm_setl_epi64", 1);
	shput(builtins, "_mm_setr_epi32", 1);
	shput(builtins, "_mm_setr_epi16", 1);
	shput(builtins, "_mm_setr_epi8", 1);
	shput(builtins, "_mm_setzero_si128", 1);
	shput(builtins, "_mm_store_si128", 1);
	shput(builtins, "_mm_storeu_si128", 1);
	shput(builtins, "_mm_storel_epi64", 1);
	shput(builtins, "_mm_maskmoveu_si128", 1);
	shput(builtins, "_mm_move_epi64", 1);
	shput(builtins, "_mm_stream_pd", 1);
	shput(builtins, "_mm_stream_si128", 1);
	shput(builtins, "_mm_clflush", 1);
	shput(builtins, "_mm_lfence", 1);
	shput(builtins, "_mm_mfence", 1);
	shput(builtins, "_mm_stream_si32", 1);
	shput(builtins, "_mm_pause", 1);
	shput(builtins, "_mm_cvtsd_f64", 1);
	shput(builtins, "_mm_castpd_ps", 1);
	shput(builtins, "_mm_castpd_si128", 1);
	shput(builtins, "_mm_castps_pd", 1);
	shput(builtins, "_mm_castps_si128", 1);
	shput(builtins, "_mm_castsi128_ps", 1);
	shput(builtins, "_mm_castsi128_pd", 1);
	shput(builtins, "_mm_cvtsd_si64", 1);
	shput(builtins, "_mm_cvttsd_si64", 1);
	shput(builtins, "_mm_cvtsi64_sd", 1);
	shput(builtins, "_mm_cvtsi64_si128", 1);
	shput(builtins, "_mm_cvtsi128_si64", 1);
	
	return (TargetDescriptor) {
		.builtin_func_map = builtins,
		.set_defines = set_defines,
		.compile_builtin = compile_builtin
	};
}
