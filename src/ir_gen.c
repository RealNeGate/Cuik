#include "ir_gen.h"

enum { LVALUE, RVALUE };

typedef struct IRVal {
	int value_type;
	TB_Register reg;
	TypeIndex type;
} IRVal;

TB_Module* mod;

// Maps param_num -> TB_Register
static _Thread_local TB_Register* parameter_map;

static TB_DataType ctype_to_tbtype(const Type* t) {
	switch (t->kind) {
		case KIND_VOID: return TB_TYPE_VOID;
		case KIND_BOOL: return TB_TYPE_BOOL;
		case KIND_CHAR: return TB_TYPE_I8;
		case KIND_SHORT: return TB_TYPE_I16;
		case KIND_INT: return TB_TYPE_I32;
		case KIND_LONG: return TB_TYPE_I64;
		case KIND_FLOAT: return TB_TYPE_F32;
		case KIND_DOUBLE: return TB_TYPE_F64;
		case KIND_ENUM: return TB_TYPE_I32;
		case KIND_PTR: return TB_TYPE_PTR;
		case KIND_FUNC: return TB_TYPE_PTR;
		default: abort(); // TODO
	}
}

static void cvt_l2r(TB_Function* func, IRVal* restrict v, TypeIndex dst_type) {
	if (v->value_type == LVALUE) {
		v->value_type = RVALUE;
		
		Type* src = &type_arena.data[v->type];
		v->reg = tb_inst_load(func, ctype_to_tbtype(src), v->reg, src->align);
		
		if (v->type != dst_type) {
			Type* dst = &type_arena.data[dst_type];
			
			if (src->kind >= KIND_CHAR &&
				src->kind <= KIND_LONG &&
				dst->kind >= KIND_CHAR && 
				dst->kind <= KIND_LONG) {
				// if it's an integer, handle some implicit casts
				if (dst->kind > src->kind) {
					// up-casts
					if (dst->is_unsigned) v->reg = tb_inst_zxt(func, v->reg, ctype_to_tbtype(dst));
					else v->reg = tb_inst_sxt(func, v->reg, ctype_to_tbtype(dst));
				} else if (dst->kind < src->kind) {
					// down-casts
					v->reg = tb_inst_trunc(func, v->reg, ctype_to_tbtype(dst));
				}
			}
		}
	}
}

static IRVal gen_expr(TB_Function* func, ExprIndex e);

// direction is either 1 for addition or -1 for subtraction
// pointer arithmatic can only have one pointer
static IRVal gen_ptr_arithmatic(TB_Function* func, TypeIndex elem_type, ExprIndex a, ExprIndex b, int direction) {
	Type* a_type = &type_arena.data[expr_arena.data[a].type];
	Type* b_type = &type_arena.data[expr_arena.data[b].type];
	
	// Decide who's the pointer
	if (b_type->kind == KIND_PTR) {
		swap(a, b);
		
		if (a_type->kind == KIND_PTR) {
			// TODO(NeGate): Make a better error for
			// (ptr + ptr) arithmatic
			abort();
		}
	}
	
	// resolve them a:ptr, b:ulong
	IRVal base = gen_expr(func, a);
	IRVal index = gen_expr(func, b);
	
	assert(base.value_type == LVALUE);
	
	// the index type is always long... or int maybe
	// if we support 32bit platforms at some point
	cvt_l2r(func, &index, TYPE_ULONG);
	
	int stride = type_arena.data[elem_type].size;
	return (IRVal) {
		.value_type = LVALUE,
		.type = elem_type,
		.reg = tb_inst_array_access(func, base.reg, index.reg, direction * stride)
	};
}

static IRVal gen_expr(TB_Function* func, ExprIndex e) {
	TypeIndex type_index = expr_arena.data[e].type;
	Type* restrict type = &type_arena.data[type_index];
	
	switch (expr_arena.data[e].op) {
		case EXPR_NUM: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = type_index,
				.reg = tb_inst_iconst(func, ctype_to_tbtype(type), expr_arena.data[e].num)
			};
		}
		case EXPR_VAR: {
			StmtIndex stmt = expr_arena.data[e].var;
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_index,
				.reg = stmt_arena.data[stmt].backing.r
			};
		}
		case EXPR_PARAM: {
			int param_num = expr_arena.data[e].param_num;
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_index,
				.reg = parameter_map[param_num]
			};
		}
		case EXPR_ADDR: {
			IRVal src = gen_expr(func, expr_arena.data[e].unary_op.src);
			assert(src.value_type == LVALUE);
			src.value_type = RVALUE;
			
			return src;
		}
		case EXPR_DEREF: {
			IRVal src = gen_expr(func, expr_arena.data[e].unary_op.src);
			cvt_l2r(func, &src, type_index);
			
			assert(type_arena.data[src.type].kind == KIND_PTR);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_index,
				.reg = src.reg
			};
		}
		case EXPR_CALL: {
			// TODO(NeGate): Optimize this!
			ExprIndex target = expr_arena.data[e].call.target;
			
			Type* func_type = &type_arena.data[expr_arena.data[target].type];
			ArgIndex arg_start = func_type->func.arg_start;
			ArgIndex arg_end = func_type->func.arg_end;
			ArgIndex arg_count = arg_end - arg_start;
			
			ExprIndexIndex param_start = expr_arena.data[e].call.param_start;
			ExprIndexIndex param_end = expr_arena.data[e].call.param_end;
			ExprIndexIndex param_count = param_end - param_start;
			
			if (param_count != arg_count) {
				abort();
			}
			
			TB_Register* params = tls_push(param_count * sizeof(TB_Register));
			
			// Resolve parameters
			for (size_t i = 0; i < param_count; i++) {
				ExprIndex p = expr_ref_arena.data[param_start + i];
				Arg* a = &arg_arena.data[arg_start + i];
				
				IRVal src = gen_expr(func, p);
				cvt_l2r(func, &src, a->type);
				
				params[i] = src.reg;
			}
			
			// Call function
			Expr* target_expr = &expr_arena.data[target];
			TB_DataType dt = ctype_to_tbtype(type);
			
			TB_Register r = TB_NULL_REG;
			if (target_expr->op == EXPR_VAR) {
				StmtIndex var = target_expr->var;
				StmtOp stmt_op = stmt_arena.data[var].op;
				
				if (stmt_op == STMT_FUNC_DECL) {
					r = tb_inst_call(func, dt, stmt_arena.data[var].backing.f, param_count, params);
				} else if (stmt_op == STMT_DECL) {
					r = tb_inst_ecall(func, dt, stmt_arena.data[var].backing.e, param_count, params);
				} else {
					abort();
				}
			} else {
				IRVal ptr = gen_expr(func, target);
				cvt_l2r(func, &ptr, type_index);
				r = tb_inst_vcall(func, dt, ptr.reg, param_count, params);
			}
			
			tls_restore(params);
			return (IRVal) {
				.value_type = RVALUE,
				.type = type_index,
				.reg = r
			};
		}
		case EXPR_SUBSCRIPT: {
			ExprIndex a = expr_arena.data[e].subscript.base;
			ExprIndex b = expr_arena.data[e].subscript.index;
			
			return gen_ptr_arithmatic(func, type_index, a, b, 1);
		}
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			bool is_inc = (expr_arena.data[e].op == EXPR_POST_INC);
			
			IRVal src = gen_expr(func, expr_arena.data[e].unary_op.src);
			assert(src.value_type == LVALUE);
			
			IRVal loaded = src;
			cvt_l2r(func, &loaded, type_index);
			
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_iconst(func, TB_TYPE_PTR, type_arena.data[type->ptr_to].size);
				TB_ArithmaticBehavior ab = TB_CAN_WRAP;
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, TB_TYPE_PTR, loaded.reg, stride, ab);
				else operation = tb_inst_sub(func, TB_TYPE_PTR, loaded.reg, stride, ab);
				
				tb_inst_store(func, TB_TYPE_PTR, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = type_index,
					.reg = loaded.reg
				};
			} else {
				TB_DataType dt = ctype_to_tbtype(type);
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				TB_Register one = tb_inst_iconst(func, dt, 1);
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, dt, loaded.reg, one, ab);
				else operation = tb_inst_sub(func, dt, loaded.reg, one, ab);
				
				tb_inst_store(func, dt, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = type_index,
					.reg = loaded.reg
				};
			}
		}
		case EXPR_PLUS:
		case EXPR_MINUS:
		if (type->kind == KIND_PTR) {
			// pointer arithmatic
			ExprIndex a = expr_arena.data[e].bin_op.left;
			ExprIndex b = expr_arena.data[e].bin_op.right;
			int dir = expr_arena.data[e].op == EXPR_PLUS ? 1 : -1;
			
			return gen_ptr_arithmatic(func, type->ptr_to, a, b, dir);
		}
		// fallthrough
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR: {
			IRVal l = gen_expr(func, expr_arena.data[e].bin_op.left);
			IRVal r = gen_expr(func, expr_arena.data[e].bin_op.right);
			cvt_l2r(func, &l, type_index);
			cvt_l2r(func, &r, type_index);
			
			TB_DataType dt = ctype_to_tbtype(type);
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (expr_arena.data[e].op) {
					case EXPR_PLUS: data = tb_inst_fadd(func, dt, l.reg, r.reg); break;
					case EXPR_MINUS: data = tb_inst_fsub(func, dt, l.reg, r.reg); break;
					case EXPR_TIMES: data = tb_inst_fmul(func, dt, l.reg, r.reg); break;
					case EXPR_SLASH: data = tb_inst_fdiv(func, dt, l.reg, r.reg); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (expr_arena.data[e].op) {
					case EXPR_PLUS: data = tb_inst_add(func, dt, l.reg, r.reg, ab); break;
					case EXPR_MINUS: data = tb_inst_sub(func, dt, l.reg, r.reg, ab); break;
					case EXPR_TIMES: data = tb_inst_mul(func, dt, l.reg, r.reg, ab); break;
					case EXPR_SLASH: data = tb_inst_div(func, dt, l.reg, r.reg, !type->is_unsigned); break;
					case EXPR_AND: data = tb_inst_and(func, dt, l.reg, r.reg); break;
					case EXPR_OR: data = tb_inst_or(func, dt, l.reg, r.reg); break;
					case EXPR_XOR: data = tb_inst_xor(func, dt, l.reg, r.reg); break;
					case EXPR_SHL: data = tb_inst_shl(func, dt, l.reg, r.reg, ab); break;
					case EXPR_SHR: data = type->is_unsigned ? tb_inst_shr(func, dt, l.reg, r.reg) : tb_inst_sar(func, dt, l.reg, r.reg); break;
					default: abort();
				}
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = type_index,
				.reg = data
			};
		}
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			ExprIndex lhs = expr_arena.data[e].bin_op.left;
			ExprIndex rhs = expr_arena.data[e].bin_op.right;
			IRVal l = gen_expr(func, lhs);
			IRVal r = gen_expr(func, rhs);
			
			type_index = get_common_type(expr_arena.data[lhs].type,
										 expr_arena.data[rhs].type);
			
			cvt_l2r(func, &l, type_index);
			cvt_l2r(func, &r, type_index);
			
			TB_DataType dt = ctype_to_tbtype(&type_arena.data[type_index]);
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (expr_arena.data[e].op) {
					case EXPR_CMPGT: data = tb_inst_cmp_fgt(func, dt, l.reg, r.reg); break;
					case EXPR_CMPGE: data = tb_inst_cmp_fge(func, dt, l.reg, r.reg); break;
					case EXPR_CMPLT: data = tb_inst_cmp_flt(func, dt, l.reg, r.reg); break;
					case EXPR_CMPLE: data = tb_inst_cmp_fle(func, dt, l.reg, r.reg); break;
					default: abort();
				}
			} else {
				switch (expr_arena.data[e].op) {
					case EXPR_CMPGT: data = tb_inst_cmp_igt(func, dt, l.reg, r.reg, !type->is_unsigned); break;
					case EXPR_CMPGE: data = tb_inst_cmp_ige(func, dt, l.reg, r.reg, !type->is_unsigned); break;
					case EXPR_CMPLT: data = tb_inst_cmp_ilt(func, dt, l.reg, r.reg, !type->is_unsigned); break;
					case EXPR_CMPLE: data = tb_inst_cmp_ile(func, dt, l.reg, r.reg, !type->is_unsigned); break;
					default: abort();
				}
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_BOOL,
				.reg = data
			};
		}
		case EXPR_PLUS_ASSIGN:
		case EXPR_MINUS_ASSIGN:
		if (type->kind == KIND_PTR) {
			int dir = expr_arena.data[e].op == EXPR_PLUS_ASSIGN ? 1 : -1;
			int stride = type_arena.data[type->ptr_to].size;
			
			// pointer arithmatic
			IRVal l = gen_expr(func, expr_arena.data[e].bin_op.left);
			IRVal r = gen_expr(func, expr_arena.data[e].bin_op.right);
			cvt_l2r(func, &r, type_index);
			
			assert(l.value_type == LVALUE);
			
			// TODO(NeGate): Maybe some type checking errors?
			IRVal loaded = l;
			cvt_l2r(func, &loaded, TYPE_ULONG);
			
			TB_Register arith = tb_inst_array_access(func, loaded.reg, r.reg, dir * stride);
			tb_inst_store(func, TB_TYPE_PTR, l.reg, arith, type->align);
			return l;
		}
		// fallthrough
		case EXPR_ASSIGN:
		case EXPR_TIMES_ASSIGN:
		case EXPR_SLASH_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN: {
			IRVal l = gen_expr(func, expr_arena.data[e].bin_op.left);
			IRVal r = gen_expr(func, expr_arena.data[e].bin_op.right);
			cvt_l2r(func, &r, type_index);
			
			assert(l.value_type == LVALUE);
			
			IRVal ld_l;
			if (expr_arena.data[e].op != EXPR_ASSIGN) {
				// don't do this conversion for ASSIGN, since it won't
				// be needing it
				ld_l = l;
				cvt_l2r(func, &ld_l, type_index);
			}
			
			TB_Register data;
			TB_DataType dt = ctype_to_tbtype(type);
			
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (expr_arena.data[e].op) {
					case EXPR_ASSIGN: data = r.reg; break;
					case EXPR_PLUS_ASSIGN: data = tb_inst_fadd(func, dt, ld_l.reg, r.reg); break;
					case EXPR_MINUS_ASSIGN: data = tb_inst_fsub(func, dt, ld_l.reg, r.reg); break;
					case EXPR_TIMES_ASSIGN: data = tb_inst_fmul(func, dt, ld_l.reg, r.reg); break;
					case EXPR_SLASH_ASSIGN: data = tb_inst_fdiv(func, dt, ld_l.reg, r.reg); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (expr_arena.data[e].op) {
					case EXPR_ASSIGN: data = r.reg; break;
					case EXPR_PLUS_ASSIGN: data = tb_inst_add(func, dt, ld_l.reg, r.reg, ab); break;
					case EXPR_MINUS_ASSIGN: data = tb_inst_sub(func, dt, ld_l.reg, r.reg, ab); break;
					case EXPR_TIMES_ASSIGN: data = tb_inst_mul(func, dt, ld_l.reg, r.reg, ab); break;
					case EXPR_SLASH_ASSIGN: data = tb_inst_div(func, dt, ld_l.reg, r.reg, !type->is_unsigned); break;
					case EXPR_AND_ASSIGN: data = tb_inst_and(func, dt, ld_l.reg, r.reg); break;
					case EXPR_OR_ASSIGN: data = tb_inst_or(func, dt, ld_l.reg, r.reg); break;
					case EXPR_XOR_ASSIGN: data = tb_inst_xor(func, dt, ld_l.reg, r.reg); break;
					case EXPR_SHL_ASSIGN: data = tb_inst_shl(func, dt, ld_l.reg, r.reg, ab); break;
					case EXPR_SHR_ASSIGN: data = type->is_unsigned ? tb_inst_shr(func, dt, ld_l.reg, r.reg) : tb_inst_sar(func, dt, ld_l.reg, r.reg); break;
					default: abort();
				}
			}
			
			tb_inst_store(func, dt, l.reg, data, type->align);
			return l;
		}
		default: abort();
	}
}

static void gen_stmt(TB_Function* func, StmtIndex s) {
	switch (stmt_arena.data[s].op) {
		case STMT_NONE: {
			break;
		}
		case STMT_COMPOUND: {
			StmtIndexIndex start = stmt_arena.data[s].kids_start;
			StmtIndexIndex end = stmt_arena.data[s].kids_end;
			
			for (StmtIndexIndex i = start; i != end; i++) {
				StmtIndex s = stmt_ref_arena.data[i];
				
				gen_stmt(func, s);
			}
			break;
		}
		case STMT_FUNC_DECL: {
			// TODO(NeGate): No nested functions
			abort();
		}
		case STMT_DECL: {
			//Attribs attrs = stmt_arena.data[s].attrs;
			TypeIndex type_index = stmt_arena.data[s].decl_type;
			Type* restrict type = &type_arena.data[type_index];
			
			TB_Register addr = tb_inst_local(func, type->size, type->align);
			stmt_arena.data[s].backing.r = addr;
			
			if (stmt_arena.data[s].expr) {
				IRVal v = gen_expr(func, stmt_arena.data[s].expr);
				
				if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
					TB_Register size_reg = tb_inst_iconst(func, TB_TYPE_I32, type->size);
					
					tb_inst_memcpy(func, addr, v.reg, size_reg, type->align);
				} else {
					cvt_l2r(func, &v, type_index);
					tb_inst_store(func, ctype_to_tbtype(type), addr, v.reg, type->align);
				}
			}
			break;
		}
		case STMT_EXPR: {
			gen_expr(func, stmt_arena.data[s].expr);
			break;
		}
		case STMT_RETURN: {
			ExprIndex e = stmt_arena.data[s].expr;
			
			TypeIndex type_index = expr_arena.data[e].type;
			Type* restrict type = &type_arena.data[type_index];
			
			IRVal v = gen_expr(func, e);
			cvt_l2r(func, &v, type_index);
			
			tb_inst_ret(func, ctype_to_tbtype(type), v.reg);
			break;
		}
		case STMT_IF: {
			TB_Label if_true = tb_inst_new_label_id(func);
			TB_Label if_false = tb_inst_new_label_id(func);
			
			IRVal cond = gen_expr(func, stmt_arena.data[s].expr);
			cvt_l2r(func, &cond, TYPE_BOOL);
			
			tb_inst_if(func, cond.reg, if_true, if_false);
			tb_inst_label(func, if_true);
			gen_stmt(func, stmt_arena.data[s].body);
			
			if (stmt_arena.data[s].body2) {
				TB_Label exit = tb_inst_new_label_id(func);
				tb_inst_goto(func, exit);
				
				tb_inst_label(func, if_false);
				gen_stmt(func, stmt_arena.data[s].body2);
				
				// fallthrough
				tb_inst_label(func, exit);
			} else {
				tb_inst_label(func, if_false);
			}
			break;
		}
		case STMT_WHILE: {
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			tb_inst_label(func, header);
			
			IRVal cond = gen_expr(func, stmt_arena.data[s].expr);
			cvt_l2r(func, &cond, TYPE_BOOL);
			
			tb_inst_if(func, cond.reg, body, exit);
			
			tb_inst_label(func, body);
			gen_stmt(func, stmt_arena.data[s].body);
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		case STMT_DO_WHILE: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			tb_inst_label(func, body);
			
			gen_stmt(func, stmt_arena.data[s].body);
			
			IRVal cond = gen_expr(func, stmt_arena.data[s].expr);
			cvt_l2r(func, &cond, TYPE_BOOL);
			tb_inst_if(func, cond.reg, body, exit);
			
			tb_inst_label(func, exit);
			break;
		}
		case STMT_FOR: {
			
			break;
		}
		case STMT_FOR2:
		__builtin_unreachable();
	}
}

static void gen_func(TypeIndex type, StmtIndex s) {
	// Clear TLS
	tls_init();
	
	TB_Function* func = tb_function_create(mod, stmt_arena.data[s].decl_name, TB_TYPE_I64);
	stmt_arena.data[s].backing.f = func;
	
	// Parameters
	ArgIndex arg_start = type_arena.data[type].func.arg_start;
	ArgIndex arg_end = type_arena.data[type].func.arg_end;
	ArgIndex arg_count = arg_end - arg_start;
	
	TB_Register* params = parameter_map = tls_push(arg_count * sizeof(TB_Register));
	for (int i = 0; i < arg_count; i++) {
		Arg* a = &arg_arena.data[arg_start + i];
		
		// Decide on the data type
		TB_DataType dt;
		Type* arg_type = &type_arena.data[a->type];
		
		if (arg_type->kind == KIND_STRUCT) {
			dt = TB_TYPE_PTR;
		} else {
			dt = ctype_to_tbtype(arg_type);
		}
		
		params[i] = tb_inst_param(func, dt);
	}
	
	// give them stack slots
	for (int i = 0; i < arg_count; i++) {
		params[i] = tb_inst_param_addr(func, params[i]);
	}
	
	// Body
	// NOTE(NeGate): STMT_FUNC_DECL is always followed by a compound block
	gen_stmt(func, s + 1);
	
	tb_function_print(func, stdout);
	printf("\n\n\n");
	
	tb_module_compile_func(mod, func);
}

void gen_ir(TopLevel tl) {
	for (StmtIndexIndex i = tl.start; i != tl.end; i++) {
		StmtIndex s = stmt_ref_arena.data[i];
		
		if (stmt_arena.data[s].op == STMT_FUNC_DECL) {
			TypeIndex type = stmt_arena.data[s].decl_type;
			assert(type_arena.data[type].kind == KIND_FUNC);
			
			gen_func(type, s);
		} else if (stmt_arena.data[s].op == STMT_DECL) {
			TypeIndex type = stmt_arena.data[s].decl_type;
			
			// TODO(NeGate): Implement other global forward decls
			if (type_arena.data[type].kind != KIND_FUNC) {
				abort();
			}
			
			stmt_arena.data[s].backing.e = tb_module_extern(mod, stmt_arena.data[s].decl_name);
		}
	}
}
