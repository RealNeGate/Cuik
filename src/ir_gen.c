#include "ir_gen.h"

enum { 
	RVALUE,
	
	LVALUE,
	LVALUE_LABEL,
	LVALUE_FUNC,
	LVALUE_EFUNC,
	
	// if value is a special kind of rvalue
	// it essentially represents a phi node
	// where it's true on one path and false
	// on the other.
	RVALUE_PHI
};

typedef struct IRVal {
	int value_type;
	TypeIndex type;
	
	union {
		TB_Register reg;
		TB_Function* func;
		TB_ExternalID ext;
		struct {
			TB_Label if_true;
			TB_Label if_false;
		} phi;
		TB_Label label;
	};
} IRVal;

TB_Module* mod;

// Maps param_num -> TB_Register
static _Thread_local TB_Register* parameter_map;
static _Thread_local TypeIndex function_type;

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
		case KIND_ARRAY: return TB_TYPE_PTR;
		case KIND_FUNC: return TB_TYPE_PTR;
		default: abort(); // TODO
	}
}

static void cvt_l2r(TB_Function* func, IRVal* restrict v, TypeIndex dst_type) {
	Type* src = &type_arena.data[v->type];
	if (v->value_type == RVALUE_PHI) {
		TB_Label merger = tb_inst_new_label_id(func);
		
		tb_inst_label(func, v->phi.if_true);
		TB_Register one = tb_inst_iconst(func, TB_TYPE_BOOL, 1);
		
		tb_inst_label(func, v->phi.if_false);
		TB_Register zero = tb_inst_iconst(func, TB_TYPE_BOOL, 0); 
		
		tb_inst_label(func, merger);
		v->value_type = RVALUE;
		v->reg = tb_inst_phi2(func, ctype_to_tbtype(src), v->phi.if_true, one, v->phi.if_false, zero);
	} else if (v->value_type == LVALUE_LABEL) {
		// TODO(NeGate)
		abort();
	} else if (v->value_type == LVALUE) {
		// Implicit array to pointer
		if (src->kind == KIND_ARRAY) {
			// just pass the address don't load
			v->type = new_pointer_locked(src->array_of);
			v->value_type = RVALUE;
			
			src = &type_arena.data[v->type];
		} else {
			v->value_type = RVALUE;
			v->reg = tb_inst_load(func, ctype_to_tbtype(src), v->reg, src->align);
		}
	} else if (v->value_type == LVALUE_FUNC) {
		v->value_type = RVALUE;
		v->reg = tb_inst_get_func_address(func, v->func);
	} else if (v->value_type == LVALUE_EFUNC) {
		v->value_type = RVALUE;
		v->reg = tb_inst_get_extern_address(func, v->ext);
	}
	
	// Implicit function to pointer
	if (src->kind == KIND_FUNC) {
		v->type = new_pointer_locked(v->type);
		src = &type_arena.data[v->type];
	}
	
	// Cast into correct type
	// TODO(NeGate): Implement float casts
	if (v->type != dst_type) {
		Type* dst = &type_arena.data[dst_type];
		
		if (src->kind >= KIND_BOOL &&
			src->kind <= KIND_LONG &&
			dst->kind >= KIND_BOOL && 
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
		} else if (src->kind == KIND_PTR &&
				   dst->kind == KIND_PTR) {
			// void* -> T* is fine
			// T* -> void* is also fine
			// A* -> B*    is not, unless they actually do match
			if (src->ptr_to != TYPE_VOID && dst->ptr_to != TYPE_VOID) {
				if (!type_equal(src->ptr_to, dst->ptr_to)) {
					printf("No implicit cast between these types.");
					abort();
				}
			}
		} else {
			printf("No implicit cast between these types.");
			abort();
		}
	}
}

static IRVal gen_expr(TB_Function* func, ExprIndex e) {
	const Expr* restrict ep = &expr_arena.data[e];
	
	switch (ep->op) {
		case EXPR_NUM: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_INT,
				.reg = tb_inst_iconst(func, TB_TYPE_I32, ep->num)
			};
		}
		case EXPR_STR: {
			const char* start = (const char*)(ep->str.start + 1);
			const char* end = (const char*)(ep->str.end - 1);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = new_array(TYPE_CHAR, end-start),
				.reg = tb_inst_const_string(func, start, end-start)
			};
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			StmtOp stmt_op = stmt_arena.data[stmt].op;
			assert(stmt_op == STMT_DECL || stmt_op == STMT_LABEL || stmt_op == STMT_FUNC_DECL);
			
			TypeIndex type_index = stmt_arena.data[stmt].decl.type;
			Type* type = &type_arena.data[type_index];
			
			if (stmt_op == STMT_LABEL) {
				return (IRVal) {
					.value_type = LVALUE_LABEL,
					.type = TYPE_NONE,
					.label = stmt_arena.data[stmt].backing.l
				};
			} else if (type->kind == KIND_FUNC) {
				if (stmt_op == STMT_FUNC_DECL) {
					return (IRVal) {
						.value_type = LVALUE_FUNC,
						.type = type_index,
						.func = tb_get_function_by_id(mod, stmt_arena.data[stmt].backing.f)
					};
				} else if (stmt_op == STMT_DECL) {
					return (IRVal) {
						.value_type = LVALUE_EFUNC,
						.type = type_index,
						.ext = stmt_arena.data[stmt].backing.e
					};
				}
			}
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_index,
				.reg = stmt_arena.data[stmt].backing.r
			};
		}
		case EXPR_PARAM: {
			int param_num = ep->param_num;
			TB_Register reg = parameter_map[param_num];
			
			ArgIndex arg = type_arena.data[function_type].func.arg_start + param_num;
			TypeIndex arg_type = arg_arena.data[arg].type;
			assert(arg_type);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = arg_type,
				.reg = reg
			};
		}
		case EXPR_ADDR: {
			IRVal src = gen_expr(func, ep->unary_op.src);
			assert(src.value_type == LVALUE);
			
			src.type = new_pointer_locked(src.type);
			src.value_type = RVALUE;
			return src;
		}
		case EXPR_CAST: {
			IRVal src = gen_expr(func, ep->cast.src);
			if (src.type == ep->cast.type) return src;
			
			Type* restrict src_type = &type_arena.data[src.type];
			Type* restrict dst_type = &type_arena.data[ep->cast.type];
			
			if (src_type->kind >= KIND_CHAR &&
				src_type->kind <= KIND_LONG &&
				dst_type->kind == KIND_PTR) {
				src.reg = tb_inst_int2ptr(func, src.reg);
				src.type = ep->cast.type;
			} else if (dst_type->kind >= KIND_CHAR &&
					   dst_type->kind <= KIND_LONG &&
					   src_type->kind == KIND_PTR) {
				src.reg = tb_inst_ptr2int(func, src.reg, ctype_to_tbtype(dst_type));
				src.type = ep->cast.type;
			}
			
			cvt_l2r(func, &src, ep->cast.type);
			return src;
		}
		case EXPR_DEREF: {
			IRVal src = gen_expr(func, ep->unary_op.src);
			cvt_l2r(func, &src, src.type);
			
			assert(type_arena.data[src.type].kind == KIND_PTR);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_arena.data[src.type].ptr_to,
				.reg = src.reg
			};
		}
		case EXPR_CALL: {
			// TODO(NeGate): It's ugly, fix it
			ExprIndex target = ep->call.target;
			
			// Call function
			IRVal func_ptr = gen_expr(func, target);
			
			TypeIndex func_type_index = func_ptr.type;
			Type* func_type = &type_arena.data[func_type_index];
			
			ArgIndex arg_start = func_type->func.arg_start;
			//ArgIndex arg_end = func_type->func.arg_end;
			//ArgIndex arg_count = arg_end - arg_start;
			
			ExprIndexIndex param_start = ep->call.param_start;
			ExprIndexIndex param_end = ep->call.param_end;
			ExprIndexIndex param_count = param_end - param_start;
			
			//if (param_count != arg_count) abort();
			
			// Resolve parameters
			TB_Register* params = tls_push(param_count * sizeof(TB_Register));
			for (size_t i = 0; i < param_count; i++) {
				ExprIndex p = expr_ref_arena.data[param_start + i];
				Arg* a = &arg_arena.data[arg_start + i];
				
				IRVal src = gen_expr(func, p);
				cvt_l2r(func, &src, a->type);
				
				params[i] = src.reg;
			}
			
			// Resolve call target
			// NOTE(NeGate): Could have been resized in the parameter's gen_expr
			func_type = &type_arena.data[func_type_index];
			TypeIndex return_type = func_type->func.return_type;
			TB_DataType dt = ctype_to_tbtype(&type_arena.data[return_type]);
			
			TB_Register r;
			if (func_ptr.value_type == LVALUE_FUNC) {
				r = tb_inst_call(func, dt, func_ptr.func, param_count, params);
			} else if (func_ptr.value_type == LVALUE_EFUNC) {
				r = tb_inst_ecall(func, dt, func_ptr.ext, param_count, params);
			} else {
				cvt_l2r(func, &func_ptr, func_type_index);
				r = tb_inst_vcall(func, dt, func_ptr.reg, param_count, params);
			}
			
			tls_restore(params);
			return (IRVal) {
				.value_type = RVALUE,
				.type = return_type,
				.reg = r
			};
		}
		case EXPR_SUBSCRIPT: {
			IRVal base = gen_expr(func, ep->subscript.base);
			IRVal index = gen_expr(func, ep->subscript.index);
			
			if (type_arena.data[index.type].kind == KIND_PTR ||
				type_arena.data[index.type].kind == KIND_ARRAY) swap(base, index);
			
			if (type_arena.data[base.type].kind == KIND_ARRAY) {
				base.type = new_pointer_locked(type_arena.data[base.type].array_of);
			}
			
			cvt_l2r(func, &base, base.type);
			cvt_l2r(func, &index, TYPE_ULONG);
			
			TypeIndex element_type = type_arena.data[base.type].ptr_to;
			int stride = type_arena.data[element_type].size;
			return (IRVal) {
				.value_type = LVALUE,
				.type = element_type,
				.reg = tb_inst_array_access(func, base.reg, index.reg, stride)
			};
		}
		case EXPR_DOT: {
			IRVal src = gen_expr(func, ep->dot.base);
			assert(src.value_type == LVALUE);
			
			Atom name = ep->dot.name;
			Type* restrict record_type = &type_arena.data[src.type];
			
			MemberIndex start = record_type->record.kids_start;
			MemberIndex end = record_type->record.kids_end;
			for (MemberIndex m = start; m < end; m++) {
				Member* member = &member_arena.data[m];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(name, member->name)) {
					assert(!member->is_bitfield);
					
					return (IRVal) {
						.value_type = LVALUE,
						.type = member->type,
						.reg = tb_inst_member_access(func, src.reg, member->offset)
					};
				}
			}
			
			abort();
		}
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			bool is_inc = (ep->op == EXPR_POST_INC);
			
			IRVal src = gen_expr(func, ep->unary_op.src);
			TypeIndex type_index = src.type;
			assert(src.value_type == LVALUE);
			
			IRVal loaded = src;
			cvt_l2r(func, &loaded, type_index);
			
			Type* type = &type_arena.data[type_index];
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
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR: {
			// a && b
			//
			//          if (a) { goto try_rhs } else { goto false }
			// try_rhs: if (b) { goto true    } else { goto false }
			//
			//
			// a || b
			//
			//          if (a) { goto true    } else { goto try_rhs }
			// try_rhs: if (b) { goto true    } else { goto false }
			bool is_and = (ep->op == EXPR_LOGICAL_AND);
			TB_Label try_rhs_lbl = tb_inst_new_label_id(func);
			
			// Eval first operand
			IRVal a = gen_expr(func, ep->bin_op.left);
			
			TB_Label true_lbl, false_lbl;
			if (a.value_type == RVALUE_PHI) {
				// chain the previous phi.
				// for logical OR, it's the false label.
				// for logical AND, it's the true label.
				if (is_and) {
					tb_inst_label(func, a.phi.if_true);
					tb_inst_goto(func, try_rhs_lbl);
					
					true_lbl = tb_inst_new_label_id(func);
					false_lbl = a.phi.if_false;
				} else {
					tb_inst_label(func, a.phi.if_false);
					tb_inst_goto(func, try_rhs_lbl);
					
					true_lbl = a.phi.if_true;
					false_lbl = tb_inst_new_label_id(func);
				}
			} else {
				true_lbl = tb_inst_new_label_id(func);
				false_lbl = tb_inst_new_label_id(func);
				
				cvt_l2r(func, &a, TYPE_BOOL);
				tb_inst_if(func, a.reg, true_lbl, try_rhs_lbl);
			}
			
			// Eval second operand
			tb_inst_label(func, try_rhs_lbl);
			
			IRVal b = gen_expr(func, ep->bin_op.right);
			cvt_l2r(func, &b, TYPE_BOOL);
			tb_inst_if(func, b.reg, true_lbl, false_lbl);
			
			// we delay placement of the labels so that we can
			// fold multiple shortcircuits together
			return (IRVal) {
				.value_type = RVALUE_PHI,
				.type = TYPE_BOOL,
				.phi = { true_lbl, false_lbl }
			};
		}
		case EXPR_COMMA: {
			gen_expr(func, ep->bin_op.left);
			return gen_expr(func, ep->bin_op.right);
		}
		case EXPR_PLUS:
		case EXPR_MINUS:
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR: {
			IRVal l = gen_expr(func, ep->bin_op.left);
			IRVal r = gen_expr(func, ep->bin_op.right);
			
			TypeIndex type_index = get_common_type(l.type, r.type);
			Type* restrict type = &type_arena.data[type_index];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				int dir = ep->op == EXPR_PLUS ? 1 : -1;
				int stride = type_arena.data[type->ptr_to].size;
				
				assert(l.value_type == LVALUE);
				cvt_l2r(func, &l, type_index);
				cvt_l2r(func, &r, TYPE_ULONG);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = type_index,
					.reg = tb_inst_array_access(func, l.reg, r.reg, dir * stride)
				};
			}
			
			cvt_l2r(func, &l, type_index);
			cvt_l2r(func, &r, type_index);
			
			TB_DataType dt = ctype_to_tbtype(type);
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (ep->op) {
					case EXPR_PLUS: data = tb_inst_fadd(func, dt, l.reg, r.reg); break;
					case EXPR_MINUS: data = tb_inst_fsub(func, dt, l.reg, r.reg); break;
					case EXPR_TIMES: data = tb_inst_fmul(func, dt, l.reg, r.reg); break;
					case EXPR_SLASH: data = tb_inst_fdiv(func, dt, l.reg, r.reg); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (ep->op) {
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
		case EXPR_CMPEQ:
		case EXPR_CMPNE: {
			IRVal l = gen_expr(func, ep->bin_op.left);
			IRVal r = gen_expr(func, ep->bin_op.right);
			
			TypeIndex type_index = get_common_type(l.type, r.type);
			Type* restrict type = &type_arena.data[type_index];
			TB_DataType dt = ctype_to_tbtype(type);
			
			cvt_l2r(func, &l, type_index);
			cvt_l2r(func, &r, type_index);
			
			TB_Register result;
			if (ep->op == EXPR_CMPEQ) result = tb_inst_cmp_eq(func, dt, l.reg, r.reg);
			else result = tb_inst_cmp_ne(func, dt, l.reg, r.reg);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_BOOL,
				.reg = result
			};
		}
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			IRVal l = gen_expr(func, ep->bin_op.left);
			IRVal r = gen_expr(func, ep->bin_op.right);
			
			TypeIndex type_index = get_common_type(l.type, r.type);
			Type* restrict type = &type_arena.data[type_index];
			TB_DataType dt = ctype_to_tbtype(type);
			
			cvt_l2r(func, &l, type_index);
			cvt_l2r(func, &r, type_index);
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (ep->op) {
					case EXPR_CMPGT: data = tb_inst_cmp_fgt(func, dt, l.reg, r.reg); break;
					case EXPR_CMPGE: data = tb_inst_cmp_fge(func, dt, l.reg, r.reg); break;
					case EXPR_CMPLT: data = tb_inst_cmp_flt(func, dt, l.reg, r.reg); break;
					case EXPR_CMPLE: data = tb_inst_cmp_fle(func, dt, l.reg, r.reg); break;
					default: abort();
				}
			} else {
				switch (ep->op) {
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
		case EXPR_ASSIGN:
		case EXPR_TIMES_ASSIGN:
		case EXPR_SLASH_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN: {
			IRVal l = gen_expr(func, ep->bin_op.left);
			IRVal r = gen_expr(func, ep->bin_op.right);
			
			assert(l.value_type == LVALUE);
			Type* restrict type = &type_arena.data[l.type];
			
			// Load inputs
			IRVal ld_l;
			if (ep->op != EXPR_ASSIGN) {
				// don't do this conversion for ASSIGN, since it won't
				// be needing it
				ld_l = l;
				cvt_l2r(func, &ld_l, l.type);
			}
			
			// Try pointer arithmatic
			if ((ep->op == EXPR_PLUS_ASSIGN || ep->op == EXPR_MINUS_ASSIGN) &&
				type->kind == KIND_PTR) {
				int dir = ep->op == EXPR_PLUS_ASSIGN ? 1 : -1;
				int stride = type_arena.data[type->ptr_to].size;
				
				assert(l.value_type == LVALUE);
				cvt_l2r(func, &r, TYPE_ULONG);
				
				TB_Register arith = tb_inst_array_access(func, ld_l.reg, r.reg, dir * stride);
				tb_inst_store(func, TB_TYPE_PTR, l.reg, arith, type->align);
				return l;
			}
			
			cvt_l2r(func, &r, l.type);
			
			TB_Register data;
			TB_DataType dt = ctype_to_tbtype(type);
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (ep->op) {
					case EXPR_ASSIGN: data = r.reg; break;
					case EXPR_PLUS_ASSIGN: data = tb_inst_fadd(func, dt, ld_l.reg, r.reg); break;
					case EXPR_MINUS_ASSIGN: data = tb_inst_fsub(func, dt, ld_l.reg, r.reg); break;
					case EXPR_TIMES_ASSIGN: data = tb_inst_fmul(func, dt, ld_l.reg, r.reg); break;
					case EXPR_SLASH_ASSIGN: data = tb_inst_fdiv(func, dt, ld_l.reg, r.reg); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (ep->op) {
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
	Stmt* restrict sp = &stmt_arena.data[s];
	
	switch (sp->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_LABEL: {
			tb_inst_label(func, sp->backing.l);
			break;
		}
		case STMT_GOTO: {
			IRVal target = gen_expr(func, sp->goto_.target);
			
			if (target.value_type == LVALUE_LABEL) {
				tb_inst_goto(func, target.label);
			} else {
				// TODO(NeGate): Handle computed goto case
				abort();
			}
			
			// spawn a fallthrough just in case
			tb_inst_label(func, tb_inst_new_label_id(func));
			break;
		}
		case STMT_COMPOUND: {
			StmtIndexIndex start = sp->compound.kids_start;
			StmtIndexIndex end = sp->compound.kids_end;
			
			for (StmtIndexIndex i = start; i != end; i++) {
				StmtIndex s = stmt_ref_arena.data[i];
				
				gen_stmt(func, s);
			}
			break;
		}
		case STMT_FUNC_DECL: {
			// TODO(NeGate): No nested functions... maybe?
			abort();
		}
		case STMT_DECL: {
			//Attribs attrs = stmt_arena.data[s].attrs;
			TypeIndex type_index = sp->decl.type;
			int kind = type_arena.data[type_index].kind;
			int size = type_arena.data[type_index].size;
			int align = type_arena.data[type_index].align;
			
			TB_Register addr = tb_inst_local(func, size, align);
			sp->backing.r = addr;
			
			if (sp->decl.initial) {
				IRVal v = gen_expr(func, sp->decl.initial);
				
				if (kind == KIND_STRUCT || kind == KIND_UNION) {
					TB_Register size_reg = tb_inst_iconst(func, TB_TYPE_I32, size);
					
					tb_inst_memcpy(func, addr, v.reg, size_reg, align);
				} else {
					cvt_l2r(func, &v, type_index);
					tb_inst_store(func, ctype_to_tbtype(&type_arena.data[type_index]), addr, v.reg, align);
				}
			} else {
				// uninitialized
				if (kind == KIND_STRUCT) {
					// TODO(NeGate): Remove this!!
					TB_Register size_reg = tb_inst_iconst(func, TB_TYPE_I32, size);
					TB_Register zero = tb_inst_iconst(func, TB_TYPE_I8, 0);
					
					tb_inst_memset(func, addr, zero, size_reg, align);
				}
			}
			break;
		}
		case STMT_EXPR: {
			gen_expr(func, sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			ExprIndex e = sp->return_.expr;
			
			if (e) {
				IRVal v = gen_expr(func, e);
				
				cvt_l2r(func, &v, v.type);
				tb_inst_ret(func, v.reg);
			} else {
				tb_inst_ret(func, TB_NULL_REG);
			}
			break;
		}
		case STMT_IF: {
			IRVal cond = gen_expr(func, sp->if_.cond);
			
			TB_Label if_true, if_false;
			if (cond.value_type == RVALUE_PHI) {
				if_true = cond.phi.if_true;
				if_false = cond.phi.if_false;
			} else {
				if_true = tb_inst_new_label_id(func);
				if_false = tb_inst_new_label_id(func);
				
				cvt_l2r(func, &cond, TYPE_BOOL);
				tb_inst_if(func, cond.reg, if_true, if_false);
			}
			
			tb_inst_label(func, if_true);
			gen_stmt(func, sp->if_.body);
			
			if (sp->if_.next) {
				TB_Label exit = tb_inst_new_label_id(func);
				tb_inst_goto(func, exit);
				
				tb_inst_label(func, if_false);
				gen_stmt(func, sp->if_.next);
				
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
			
			IRVal cond = gen_expr(func, sp->while_.cond);
			cvt_l2r(func, &cond, TYPE_BOOL);
			
			tb_inst_if(func, cond.reg, body, exit);
			
			tb_inst_label(func, body);
			gen_stmt(func, sp->while_.body);
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		case STMT_DO_WHILE: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			tb_inst_label(func, body);
			
			gen_stmt(func, sp->while_.body);
			
			IRVal cond = gen_expr(func, sp->while_.cond);
			cvt_l2r(func, &cond, TYPE_BOOL);
			tb_inst_if(func, cond.reg, body, exit);
			
			tb_inst_label(func, exit);
			break;
		}
		case STMT_FOR: {
<<<<<<< HEAD
=======
			// TODO(NeGate)
>>>>>>> 34dbd0f94d4326337ecd4f139cb493ebd4a1e42d
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			gen_stmt(func, sp->for_.first);
			
			tb_inst_label(func, header);
			
			IRVal cond = gen_expr(func, sp->for_.cond);
			cvt_l2r(func, &cond, TYPE_BOOL);
			tb_inst_if(func, cond.reg, body, exit);
			
			tb_inst_label(func, body);
			
			gen_stmt(func, sp->for_.body);
			gen_expr(func, sp->for_.next);
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		default:
		__builtin_unreachable();
	}
}

static void gen_func_header(TypeIndex type, StmtIndex s) {
	const Type* return_type = &type_arena.data[type_arena.data[type].func.return_type];
	
	// Parameters
	ArgIndex arg_start = type_arena.data[type].func.arg_start;
	ArgIndex arg_end = type_arena.data[type].func.arg_end;
	
	TB_FunctionPrototype* proto = tb_prototype_create(mod,
													  TB_STDCALL,
													  ctype_to_tbtype(return_type),
													  arg_end - arg_start, false);
	
	for (ArgIndex i = arg_start; i < arg_end; i++) {
		Arg* a = &arg_arena.data[i];
		
		// Decide on the data type
		TB_DataType dt;
		Type* arg_type = &type_arena.data[a->type];
		
		if (arg_type->kind == KIND_STRUCT) {
			dt = TB_TYPE_PTR;
		} else {
			dt = ctype_to_tbtype(arg_type);
		}
		
		tb_prototype_add_param(proto, dt);
	}
	
	TB_Function* func = tb_prototype_build(mod, proto, (char*) stmt_arena.data[s].decl.name);
	stmt_arena.data[s].backing.f = tb_function_get_id(mod, func);
}

static void gen_func_body(TypeIndex type, StmtIndex s, StmtIndex end) {
	// Clear TLS
	tls_init();
	assert(type);
	
	TB_Function* func = tb_get_function_by_id(mod, stmt_arena.data[s].backing.f);
	
	// Parameters
	ArgIndex arg_start = type_arena.data[type].func.arg_start;
	ArgIndex arg_end = type_arena.data[type].func.arg_end;
	ArgIndex arg_count = arg_end - arg_start;
	
	TB_Register* params = parameter_map = tls_push(arg_count * sizeof(TB_Register));
	
	// gimme stack slots
	for (int i = 0; i < arg_count; i++) {
		params[i] = tb_inst_param_addr(func, i);
	}
	
	// TODO(NeGate): Ok fix this up later but essentially we need to prepass
	// over the nodes to find the label statements, then forward declare the labels
	// in TB.
	for (StmtIndex i = s+1; i < end; i++) {
		if (stmt_arena.data[i].op == STMT_LABEL) {
			stmt_arena.data[i].backing.l = tb_inst_new_label_id(func);
		}
	}
	
	// Body
	// NOTE(NeGate): STMT_FUNC_DECL is always followed by a compound block
	function_type = type;
	gen_stmt(func, s + 1);
	function_type = 0;
	
	Type* return_type = &type_arena.data[type_arena.data[type].func.return_type];
	TB_Register last = tb_node_get_last_register(func);
	if (tb_node_is_label(func, last) || !tb_node_is_terminator(func, last)) {
		if (return_type->kind != KIND_VOID) {
			// Needs return value
			abort();
		}
		
		tb_inst_ret(func, TB_NULL_REG);
	}
	
	//tb_function_print(func, stdout);
	//printf("\n\n\n");
	
	tb_module_compile_func(mod, func);
	tb_function_free(func);
}

void gen_ir_stage1(TopLevel tl, size_t i) {
	StmtIndex s = tl.arr[i];
	
	if (stmt_arena.data[s].op == STMT_FUNC_DECL) {
		TypeIndex type = stmt_arena.data[s].decl.type;
		assert(type_arena.data[type].kind == KIND_FUNC);
		
		gen_func_header(type, s);
	} else if (stmt_arena.data[s].op == STMT_DECL) {
		TypeIndex type = stmt_arena.data[s].decl.type;
		
		// TODO(NeGate): Implement other global forward decls
		if (type_arena.data[type].kind != KIND_FUNC) {
			abort();
		}
		
		if (!stmt_arena.data[s].decl.attrs.is_used) {
			return;
		}
		
		stmt_arena.data[s].backing.e = tb_module_extern(mod, (char*) stmt_arena.data[s].decl.name);
	}
}

void gen_ir_stage2(TopLevel tl, size_t i) {
	StmtIndex s = tl.arr[i];
	StmtIndex end = i + 1 < arrlen(tl.arr) ? tl.arr[i+1] : stmt_arena.count;
	
	if (stmt_arena.data[s].op == STMT_FUNC_DECL) {
		TypeIndex type = stmt_arena.data[s].decl.type;
		assert(type_arena.data[type].kind == KIND_FUNC);
		
		gen_func_body(type, s, end);
	}
}
