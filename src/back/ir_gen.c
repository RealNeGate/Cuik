#include "ir_gen.h"
#include "settings.h"
#include <stdarg.h>

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

TokenStream ir_gen_tokens;
TB_Module* mod;

// Maps param_num -> TB_Register
static _Thread_local TB_Register* parameter_map;
static _Thread_local TypeIndex function_type;

// For aggregate returns
static _Thread_local TB_Register return_value_address;

static IRVal gen_expr(TB_Function* func, ExprIndex e);

static _Noreturn void sema_fatal(SourceLocIndex loc, const char* fmt, ...) {
	SourceLoc* l = &ir_gen_tokens.line_arena[loc];
	printf("%s:%d: error: ", l->file, l->line);
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	abort();
}

static TB_Register cast_reg(TB_Function* func, TB_Register reg, const Type* src, const Type* dst) {
	// Cast into correct type
	if (src->kind >= KIND_BOOL &&
		src->kind <= KIND_LONG &&
		dst->kind >= KIND_BOOL && 
		dst->kind <= KIND_LONG) {
		if (dst->kind > src->kind) {
			// up-casts
			if (dst->is_unsigned) reg = tb_inst_zxt(func, reg, ctype_to_tbtype(dst));
			else reg = tb_inst_sxt(func, reg, ctype_to_tbtype(dst));
		} else if (dst->kind < src->kind) {
			// down-casts
			reg = tb_inst_trunc(func, reg, ctype_to_tbtype(dst));
		}
	} else if (src->kind >= KIND_CHAR &&
			   src->kind <= KIND_LONG &&
			   dst->kind == KIND_PTR) {
		reg = tb_inst_int2ptr(func, reg);
	} else if (src->kind == KIND_PTR &&
			   dst->kind >= KIND_CHAR &&
			   dst->kind <= KIND_LONG) {
		reg = tb_inst_ptr2int(func, reg, ctype_to_tbtype(dst));
	} else if (src->kind == KIND_PTR &&
			   dst->kind == KIND_PTR) {
		/* TB has opaque pointers, nothing needs to be done. */
	} else if (src->kind == KIND_FLOAT &&
			   dst->kind == KIND_DOUBLE) {
		reg = tb_inst_fpxt(func, reg, TB_TYPE_F64);
	} else if (src->kind == KIND_DOUBLE &&
			   dst->kind == KIND_FLOAT) {
		reg = tb_inst_trunc(func, reg, TB_TYPE_F32);
	}
	
	return reg;
}

static TB_Register cvt2rval(TB_Function* func, const IRVal v, ExprIndex e) {
	const Expr* restrict ep = &expr_arena.data[e];
	const Type* src = &type_arena.data[ep->type];
	
	TB_Register reg = 0;
	switch (v.value_type) {
		case RVALUE: {
			reg = v.reg; 
			break;
		}
		case RVALUE_PHI: {
			TB_Label merger = tb_inst_new_label_id(func);
			
			tb_inst_label(func, v.phi.if_true);
			TB_Register one = tb_inst_bool(func, true);
			tb_inst_goto(func, merger);
			
			tb_inst_label(func, v.phi.if_false);
			TB_Register zero = tb_inst_bool(func, false);
			
			tb_inst_label(func, merger);
			
			reg = tb_inst_phi2(func, v.phi.if_true, one, v.phi.if_false, zero);
			break;
		}
		case LVALUE: {
			// Implicit array to pointer
			if (src->kind == KIND_ARRAY) {
				// just pass the address don't load
				TypeIndex type = new_pointer_locked(src->array_of);
				
				src = &type_arena.data[type];
				reg = v.reg;
			} else {
				reg = tb_inst_load(func, ctype_to_tbtype(src), v.reg, src->align);
			}
			break;
		}
		case LVALUE_FUNC: {
			reg = tb_inst_get_func_address(func, v.func);
			break;
		}
		case LVALUE_EFUNC: {
			reg = tb_inst_get_extern_address(func, v.ext);
			break;
		}
		default: abort();
	}
	
	const Type* dst = &type_arena.data[ep->cast_type];
	return src != dst ? cast_reg(func, reg, src, dst) : reg;
}

static TB_Register as_rvalue(TB_Function* func, ExprIndex e) {
	return cvt2rval(func, gen_expr(func, e), e);
}

// Does pointer math scare you? this is like mostly just addition but yea
static InitNode* count_max_tb_init_objects(int node_count, InitNode* node, int* out_count) {
	for (int i = 0; i < node_count; i++) {
		if (node->kids_count == 0) {
			*out_count += 1;
		} else {
			node = count_max_tb_init_objects(node->kids_count, node, out_count);
		}
		
		node += 1;
	}
	
	return node;
}

// TODO(NeGate): Revisit this code as a smarter man...
// if the addr is 0 then we only apply constant initializers.
// func doesn't need to be non-NULL if it's addr is NULL.
static InitNode* eval_initializer_objects(TB_Function* func, TB_InitializerID init, TB_Register addr, TypeIndex t, int node_count, InitNode* node, int* offset) {
	// TODO(NeGate): Implement line info for this code for error handling.
	SourceLocIndex loc = 0;
	
	// Identify boundaries:
	//   Scalar types are 1
	//   Records depend on the member count
	//   Arrays are based on array count
	int bounds;
	{
		Type* restrict type = &type_arena.data[t];
		switch (type->kind) {
			case KIND_ARRAY:
			bounds = type->array_count; 
			break;
			
			case KIND_UNION: case KIND_STRUCT:
			bounds = type->record.kids_end - type->record.kids_start;
			break;
			
			default: 
			bounds = 1;
			break;
		}
	}
	
	// Starts at the first node and just keep traversing through any nodes with children.
	int cursor = 0;
	for (int i = 0; i < node_count; i++) {
		// NOTE(NeGate): We reload this value because it's possible that
		// expression IR generation may clobber the type_arena.
		Type* restrict type = &type_arena.data[t];
		
		// if there's no designator then the cursor
		// just keeps incrementing
		int pos, pos_end;
		if (node->mode == INIT_MEMBER) {
			if (type->kind != KIND_STRUCT && type->kind != KIND_UNION) {
				sema_fatal(loc, "Cannot get the member of a non-record type.");
			}
			
			pos = pos_end = -1;
			
			MemberIndex start = type->record.kids_start;
			MemberIndex end = type->record.kids_end;
			for (MemberIndex m = start; m < end; m++) {
				Member* member = &member_arena.data[m];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(node->member_name, member->name)) {
					pos = cursor;
					pos_end = cursor + 1;
					cursor = pos_end;
					break;
				}
			}
			
			if (pos < 0) {
				sema_fatal(loc, "Could not find member under that name.");
			}
		} else if (node->mode == INIT_ARRAY) {
			if (type->kind != KIND_ARRAY) {
				sema_fatal(loc, "Cannot apply array initializer to non-array type.");
			}
			
			pos = node->start;
			pos_end = node->start + node->count;
			cursor = pos_end;
		} else {
			if (type->kind != KIND_STRUCT && type->kind != KIND_UNION && type->kind != KIND_ARRAY) {
				sema_fatal(loc, "Compound literal with multiple elements must be a struct, union or array.");
			}
			
			pos = cursor;
			pos_end = cursor + 1;
			cursor++;
		}
		
		// Validate indices
		if (pos < 0 || pos >= bounds) {
			sema_fatal(loc, "Initializer out of range, TODO error ugly");
		} else if (pos_end <= 0 && pos_end > bounds) {
			sema_fatal(loc, "Initializer out of range, TODO error ugly");
		}
		
		// TODO(NeGate): Implement array range initializer
		if (pos + 1 != pos_end) {
			sema_fatal(loc, "TODO");
		}
		
		// Identify entry type
		TypeIndex child_type;
		if (type->kind == KIND_ARRAY) {
			child_type = type->array_of;
		} else if (type->kind == KIND_UNION || type->kind == KIND_STRUCT) {
			child_type = member_arena.data[type->record.kids_start + pos].type;
		} else {
			child_type = t;
		}
		
		// traverse children
		int relative_offset = 0;
		if (type->kind == KIND_ARRAY) {
			relative_offset += type_arena.data[type->array_of].size * pos;
		} else if (type->kind == KIND_UNION || type->kind == KIND_STRUCT) {
			relative_offset += member_arena.data[type->record.kids_start + pos].offset;
		}
		
		if (node->kids_count > 0) {
			*offset += relative_offset;
			node = eval_initializer_objects(func, init, addr, child_type, node->kids_count, node, offset);
		} else {
			// initialize a value
			assert(func);
			assert(node->expr);
			
			TB_Register effective_addr;
			if (addr) {
				effective_addr = tb_inst_member_access(func, addr, *offset + relative_offset);
			} else {
				effective_addr = addr;
			}
			
			switch (expr_arena.data[node->expr].op) {
				// TODO(NeGate): Implement constants for literals
				// to allow for more stuff to be precomputed.
				// case EXPR_INT: 
				// ...
				// break;
				
				// dynamic expressions
				default:
				if (addr) {
					TypeKind kind = type_arena.data[child_type].kind;
					int size = type_arena.data[child_type].size;
					int align = type_arena.data[child_type].align;
					
					if (kind == KIND_STRUCT || kind == KIND_UNION || kind == KIND_ARRAY) {
						IRVal v = gen_expr(func, node->expr);
						
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						tb_inst_memcpy(func, effective_addr, v.reg, size_reg, align);
					} else {
						TB_Register v = as_rvalue(func, node->expr);
						
						tb_inst_store(func, ctype_to_tbtype(&type_arena.data[child_type]), effective_addr, v, align);
					}
				}
				break;
			}
		}
		
		node += 1;
	}
	
	return node;
}

static TB_Register gen_local_initializer(TB_Function* func, TypeIndex t, int node_count, InitNode* nodes) {
	// Walk initializer for max constant expression initializers.
	int max_tb_objects;
	count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
	
	TB_InitializerID init = tb_initializer_create(mod, 
												  type_arena.data[t].size,
												  type_arena.data[t].align, 
												  max_tb_objects);
	
	TB_Register addr = tb_inst_local(func, type_arena.data[t].size, type_arena.data[t].align);
	
	// Initialize all const expressions
	eval_initializer_objects(func, init, TB_NULL_REG, t, node_count, nodes, &(int) { 0 });
	tb_inst_initialize_mem(func, addr, init);
	
	// Initialize all dynamic expressions
	eval_initializer_objects(func, init, addr, t, node_count, nodes, &(int) { 0 });
	return addr;
}

static IRVal gen_expr(TB_Function* func, ExprIndex e) {
	Expr* restrict ep = &expr_arena.data[e];
	
	switch (ep->op) {
		case EXPR_INT: {
			TB_DataType dt = ctype_to_tbtype(&type_arena.data[ep->type]);
			
			if (type_arena.data[ep->type].is_unsigned) {
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = tb_inst_uint(func, dt, ep->int_num.num)
				};
			} else {
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = tb_inst_sint(func, dt, ep->int_num.num)
				};
			}
		}
		case EXPR_FLOAT: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_DOUBLE,
				.reg = tb_inst_float(func, TB_TYPE_F64, ep->float_num)
			};
		}
		case EXPR_STR: {
			const char* start = (const char*)(ep->str.start + 1);
			const char* end = (const char*)(ep->str.end - 1);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = new_array(TYPE_CHAR, end-start),
				.reg = tb_inst_string(func, end-start, start)
			};
		}
		case EXPR_INITIALIZER: {
			TB_Register r = gen_local_initializer(func, ep->init.type, ep->init.count, ep->init.nodes);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->init.type,
				.reg = r
			};
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			StmtOp stmt_op = stmt_arena.data[stmt].op;
			assert(stmt_op == STMT_DECL || stmt_op == STMT_LABEL || stmt_op == STMT_GLOBAL_DECL || stmt_op == STMT_FUNC_DECL);
			
			TypeIndex type_index = stmt_arena.data[stmt].decl.type;
			Type* type = &type_arena.data[type_index];
			
			if (stmt_op == STMT_GLOBAL_DECL) {
				return (IRVal) {
					.value_type = LVALUE,
					.type = type_index,
					.reg = tb_inst_get_global_address(func, stmt_arena.data[stmt].backing.g)
				};
			} else if (stmt_op == STMT_LABEL) {
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
						.func = tb_function_from_id(mod, stmt_arena.data[stmt].backing.f)
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
			
			ParamIndex param = type_arena.data[function_type].func.param_list + param_num;
			TypeIndex arg_type = param_arena.data[param].type;
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
			
			src.type = ep->type;
			src.value_type = RVALUE;
			return src;
		}
		case EXPR_NOT: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_not(func, as_rvalue(func, ep->unary_op.src))
			};
		}
		case EXPR_NEGATE: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_neg(func, as_rvalue(func, ep->unary_op.src))
			};
		}
		case EXPR_CAST: {
			IRVal src = gen_expr(func, ep->cast.src);
			if (src.type == ep->cast.type) return src;
			
			// stuff like ((void) x)
			if (type_arena.data[src.type].kind == KIND_VOID) {
				return (IRVal) { .value_type = RVALUE, .type = TYPE_VOID, .reg = 0 };
			}
			
			return (IRVal) { 
				.value_type = RVALUE,
				.type = ep->cast.type,
				.reg = as_rvalue(func, ep->cast.src)
			};
		}
		case EXPR_DEREF: {
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->type,
				.reg = as_rvalue(func, ep->unary_op.src)
			};
		}
		case EXPR_CALL: {
			// Call function
			IRVal func_ptr = gen_expr(func, ep->call.target);
			
			TypeIndex func_type_index = func_ptr.type;
			Type* func_type = &type_arena.data[func_type_index];
			
			ExprIndex* args = ep->call.param_start;
			int arg_count = ep->call.param_count;
			
			// NOTE(NeGate): returning aggregates requires us
			// to allocate our own space and pass it to the 
			// callee.
			bool is_aggregate_return = false;
			TypeIndex return_type = func_type->func.return_type;
			if (type_arena.data[return_type].kind == KIND_STRUCT ||
				type_arena.data[return_type].kind == KIND_UNION) {
				is_aggregate_return = true;
				return_type = TYPE_VOID;
			}
			
			// Resolve parameters
			size_t real_arg_count = arg_count + is_aggregate_return;
			TB_Register* ir_args = tls_push(real_arg_count * sizeof(TB_Register));
			
			if (is_aggregate_return) {
				ir_args[0] = tb_inst_local(func, type_arena.data[return_type].size, type_arena.data[return_type].align);
			}
			
			for (size_t i = 0; i < arg_count; i++) {
				ir_args[is_aggregate_return+i] = as_rvalue(func, args[i]);
			}
			
			// Resolve call target
			//
			// NOTE(NeGate): Could have been resized in the parameter's gen_expr
			// so we reload the pointer.
			func_type = &type_arena.data[func_type_index];
			TB_DataType dt = ctype_to_tbtype(&type_arena.data[return_type]);
			
			TB_Register r;
			if (func_ptr.value_type == LVALUE_FUNC) {
				r = tb_inst_call(func, dt, func_ptr.func, real_arg_count, ir_args);
			} else if (func_ptr.value_type == LVALUE_EFUNC) {
				r = tb_inst_ecall(func, dt, func_ptr.ext, real_arg_count, ir_args);
			} else {
				TB_Register target_reg = cvt2rval(func, func_ptr, ep->call.target);
				
				r = tb_inst_vcall(func, dt, target_reg, real_arg_count, ir_args);
			}
			
			tls_restore(ir_args);
			return (IRVal) {
				.value_type = RVALUE,
				.type = return_type,
				.reg = r
			};
		}
		case EXPR_SUBSCRIPT: {
			TB_Register base = as_rvalue(func, ep->subscript.base);
			TB_Register index = as_rvalue(func, ep->subscript.index);
			
			int stride = type_arena.data[ep->type].size;
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->type,
				.reg = tb_inst_array_access(func, base, index, stride)
			};
		}
		case EXPR_DOT: {
			IRVal src = gen_expr(func, ep->dot.base);
			assert(src.value_type == LVALUE);
			
			Member* member = &member_arena.data[ep->dot.member];
			
			// TODO(NeGate): Implement bitfields
			assert(!member->is_bitfield);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = member->type,
				.reg = tb_inst_member_access(func, src.reg, member->offset)
			};
		}
		case EXPR_ARROW: {
			TB_Register src = as_rvalue(func, ep->arrow.base);
			
			// TODO(NeGate): Setup a pointer size constant thingy
			// so i dont hardcode these everywhere.
			src = tb_inst_load(func, TB_TYPE_PTR, src, 8);
			
			Member* member = &member_arena.data[ep->arrow.member];
			
			// TODO(NeGate): Implement bitfields
			assert(!member->is_bitfield);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = member->type,
				.reg = tb_inst_member_access(func, src, member->offset)
			};
		}
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC: {
			bool is_inc = (ep->op == EXPR_PRE_INC);
			
			IRVal src = gen_expr(func, ep->unary_op.src);
			assert(src.value_type == LVALUE);
			
			TB_Register loaded = cvt2rval(func, src, ep->unary_op.src);
			Type* type = &type_arena.data[ep->type];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_sint(func, TB_TYPE_PTR, type_arena.data[type->ptr_to].size);
				TB_ArithmaticBehavior ab = TB_CAN_WRAP;
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, stride, ab);
				else operation = tb_inst_sub(func, loaded, stride, ab);
				
				tb_inst_store(func, TB_TYPE_PTR, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = operation
				};
			} else {
				TB_DataType dt = ctype_to_tbtype(type);
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				TB_Register one = type->is_unsigned ? tb_inst_uint(func, dt, 1) : tb_inst_sint(func, dt, 1);
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, one, ab);
				else operation = tb_inst_sub(func, loaded, one, ab);
				
				tb_inst_store(func, dt, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = operation
				};
			}
		}
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			bool is_inc = (ep->op == EXPR_POST_INC);
			
			IRVal src = gen_expr(func, ep->unary_op.src);
			assert(src.value_type == LVALUE);
			
			TB_Register loaded = cvt2rval(func, src, ep->unary_op.src);
			Type* type = &type_arena.data[ep->type];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_sint(func, TB_TYPE_PTR, type_arena.data[type->ptr_to].size);
				TB_ArithmaticBehavior ab = TB_CAN_WRAP;
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, stride, ab);
				else operation = tb_inst_sub(func, loaded, stride, ab);
				
				tb_inst_store(func, TB_TYPE_PTR, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = loaded
				};
			} else {
				TB_DataType dt = ctype_to_tbtype(type);
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				TB_Register one = type->is_unsigned ? tb_inst_uint(func, dt, 1) : tb_inst_sint(func, dt, 1);
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, one, ab);
				else operation = tb_inst_sub(func, loaded, one, ab);
				
				tb_inst_store(func, dt, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = loaded
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
			
			// TODO(NeGate): Remove this later because it kills
			// the codegen a bit in certain cases.
			TB_Label entry_lbl = tb_inst_new_label_id(func);
			tb_inst_label(func, entry_lbl);
			
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
				
				TB_Register a_reg = cvt2rval(func, a, ep->bin_op.left);
				tb_inst_if(func, a_reg, true_lbl, try_rhs_lbl);
			}
			
			// Eval second operand
			tb_inst_label(func, try_rhs_lbl);
			
			TB_Register b = as_rvalue(func, ep->bin_op.right);
			tb_inst_if(func, b, true_lbl, false_lbl);
			
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
			TB_Register l = as_rvalue(func, ep->bin_op.left);
			TB_Register r = as_rvalue(func, ep->bin_op.right);
			
			Type* restrict type = &type_arena.data[ep->type];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				int dir = ep->op == EXPR_PLUS ? 1 : -1;
				int stride = type_arena.data[type->ptr_to].size;
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = tb_inst_array_access(func, l, r, dir * stride)
				};
			}
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (ep->op) {
					case EXPR_PLUS: data = tb_inst_fadd(func, l, r); break;
					case EXPR_MINUS: data = tb_inst_fsub(func, l, r); break;
					case EXPR_TIMES: data = tb_inst_fmul(func, l, r); break;
					case EXPR_SLASH: data = tb_inst_fdiv(func, l, r); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (ep->op) {
					case EXPR_PLUS: data = tb_inst_add(func, l, r, ab); break;
					case EXPR_MINUS: data = tb_inst_sub(func, l, r, ab); break;
					case EXPR_TIMES: data = tb_inst_mul(func, l, r, ab); break;
					case EXPR_SLASH: data = tb_inst_div(func, l, r, !type->is_unsigned); break;
					case EXPR_AND: data = tb_inst_and(func, l, r); break;
					case EXPR_OR: data = tb_inst_or(func, l, r); break;
					case EXPR_XOR: data = tb_inst_xor(func, l, r); break;
					case EXPR_SHL: data = tb_inst_shl(func, l, r, ab); break;
					case EXPR_SHR: data = type->is_unsigned ? tb_inst_shr(func, l, r) : tb_inst_sar(func, l, r); break;
					default: abort();
				}
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = data
			};
		}
		case EXPR_CMPEQ:
		case EXPR_CMPNE: {
			TB_Register l = as_rvalue(func, ep->bin_op.left);
			TB_Register r = as_rvalue(func, ep->bin_op.right);
			
			TB_Register result;
			if (ep->op == EXPR_CMPEQ) {
				result = tb_inst_cmp_eq(func, l, r);
			} else {
				result = tb_inst_cmp_ne(func, l, r);
			}
			
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
			TB_Register l = as_rvalue(func, ep->bin_op.left);
			TB_Register r = as_rvalue(func, ep->bin_op.right);
			
			Type* type = &type_arena.data[ep->type];
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (ep->op) {
					case EXPR_CMPGT: data = tb_inst_cmp_fgt(func, l, r); break;
					case EXPR_CMPGE: data = tb_inst_cmp_fge(func, l, r); break;
					case EXPR_CMPLT: data = tb_inst_cmp_flt(func, l, r); break;
					case EXPR_CMPLE: data = tb_inst_cmp_fle(func, l, r); break;
					default: abort();
				}
			} else {
				switch (ep->op) {
					case EXPR_CMPGT: data = tb_inst_cmp_igt(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPGE: data = tb_inst_cmp_ige(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPLT: data = tb_inst_cmp_ilt(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPLE: data = tb_inst_cmp_ile(func, l, r, !type->is_unsigned); break;
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
			Type* restrict type = &type_arena.data[ep->type];
			
			// Load inputs
			IRVal lhs = gen_expr(func, ep->bin_op.left);
			
			TB_Register l;
			if (ep->op != EXPR_ASSIGN) {
				// don't do this conversion for ASSIGN, since it won't
				// be needing it
				l = cvt2rval(func, lhs, ep->bin_op.left);
			}
			
			IRVal rhs = gen_expr(func, ep->bin_op.right);
			
			// Try pointer arithmatic
			if ((ep->op == EXPR_PLUS_ASSIGN || ep->op == EXPR_MINUS_ASSIGN) && type->kind == KIND_PTR) {
				int dir = ep->op == EXPR_PLUS_ASSIGN ? 1 : -1;
				int stride = type_arena.data[type->ptr_to].size;
				
				TB_Register r = cvt2rval(func, rhs, ep->bin_op.right);
				TB_Register arith = tb_inst_array_access(func, l, r, dir * stride);
				
				assert(lhs.value_type == LVALUE);
				tb_inst_store(func, TB_TYPE_PTR, lhs.reg, arith, type->align);
				return lhs;
			}
			
			TB_DataType dt = ctype_to_tbtype(type);
			
			TB_Register data;
			if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
				if (ep->op != EXPR_ASSIGN) abort();
				
				TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, type->size);
				tb_inst_memcpy(func, lhs.reg, rhs.reg, size_reg, type->align);
			} else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				TB_Register r = cvt2rval(func, rhs, ep->bin_op.right);
				
				switch (ep->op) {
					case EXPR_ASSIGN: data = r; break;
					case EXPR_PLUS_ASSIGN: data = tb_inst_fadd(func, l, r); break;
					case EXPR_MINUS_ASSIGN: data = tb_inst_fsub(func, l, r); break;
					case EXPR_TIMES_ASSIGN: data = tb_inst_fmul(func, l, r); break;
					case EXPR_SLASH_ASSIGN: data = tb_inst_fdiv(func, l, r); break;
					default: abort();
				}
				
				assert(lhs.value_type == LVALUE);
				tb_inst_store(func, dt, lhs.reg, data, type->align);
			} else {
				TB_Register r = cvt2rval(func, rhs, ep->bin_op.right);
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (ep->op) {
					case EXPR_ASSIGN: data = r; break;
					case EXPR_PLUS_ASSIGN: data = tb_inst_add(func, l, r, ab); break;
					case EXPR_MINUS_ASSIGN: data = tb_inst_sub(func, l, r, ab); break;
					case EXPR_TIMES_ASSIGN: data = tb_inst_mul(func, l, r, ab); break;
					case EXPR_SLASH_ASSIGN: data = tb_inst_div(func, l, r, !type->is_unsigned); break;
					case EXPR_AND_ASSIGN: data = tb_inst_and(func, l, r); break;
					case EXPR_OR_ASSIGN: data = tb_inst_or(func, l, r); break;
					case EXPR_XOR_ASSIGN: data = tb_inst_xor(func, l, r); break;
					case EXPR_SHL_ASSIGN: data = tb_inst_shl(func, l, r, ab); break;
					case EXPR_SHR_ASSIGN: data = type->is_unsigned ? tb_inst_shr(func, l, r) : tb_inst_sar(func, l, r); break;
					default: abort();
				}
				
				assert(lhs.value_type == LVALUE);
				tb_inst_store(func, dt, lhs.reg, data, type->align);
			}
			
			return lhs;
		}
		case EXPR_TERNARY: {
			IRVal cond = gen_expr(func, ep->ternary_op.left);
			
			TB_Label if_true, if_false;
			TB_Label exit = tb_inst_new_label_id(func);
			if (cond.value_type == RVALUE_PHI) {
				if_true = cond.phi.if_true;
				if_false = cond.phi.if_false;
			} else {
				if_true = tb_inst_new_label_id(func);
				if_false = tb_inst_new_label_id(func);
				
				TB_Register reg = cvt2rval(func, cond, ep->ternary_op.left);
				tb_inst_if(func, reg, if_true, if_false);
			}
			
			TB_Register true_val;
			{
				tb_inst_label(func, if_true);
				
				true_val = as_rvalue(func, ep->ternary_op.middle);
				tb_inst_goto(func, exit);
			}
			
			TB_Register false_val;
			{
				tb_inst_label(func, if_false);
				
				false_val = as_rvalue(func, ep->ternary_op.right);
				// fallthrough to exit
			}
			tb_inst_label(func, exit);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_phi2(func, if_true, true_val, if_false, false_val)
			};
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
			StmtIndex* kids = sp->compound.kids;
			size_t count = sp->compound.kids_count;
			
			for (size_t i = 0; i < count; i++) {
				gen_stmt(func, kids[i]);
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
			
			TB_Register addr = 0;
			if (sp->decl.initial) {
				Expr* restrict ep = &expr_arena.data[sp->decl.initial];
				
				if (ep->op == EXPR_INITIALIZER) {
					addr = gen_local_initializer(func, type_index, ep->init.count, ep->init.nodes);
				} else {
					addr = tb_inst_local(func, size, align);
					
					if (kind == KIND_STRUCT || kind == KIND_UNION) {
						IRVal v = gen_expr(func, sp->decl.initial);
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						
						tb_inst_memcpy(func, addr, v.reg, size_reg, align);
					} else {
						TB_Register v = as_rvalue(func, sp->decl.initial);
						
						tb_inst_store(func, ctype_to_tbtype(&type_arena.data[type_index]), addr, v, align);
					}
				}
			} else {
				/* uninitialized */
			}
			
			if (addr == TB_NULL_REG) {
				addr = tb_inst_local(func, size, align);
			}
			
			sp->backing.r = addr;
			break;
		}
		case STMT_EXPR: {
			gen_expr(func, sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			ExprIndex e = sp->return_.expr;
			
			if (e) {
				TypeIndex type = expr_arena.data[e].cast_type;
				
				if (type_arena.data[type].kind == KIND_STRUCT ||
					type_arena.data[type].kind == KIND_UNION) {
					IRVal v = gen_expr(func, e);
					
					// returning aggregates just copies into the first parameter
					// which is agreed to be a caller owned buffer.
					int size = type_arena.data[type].size;
					int align = type_arena.data[type].align;
					
					TB_Register dst_address = tb_inst_load(func, TB_TYPE_PTR, return_value_address, 8);
					TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
					
					tb_inst_memcpy(func, dst_address, v.reg, size_reg, align);
					tb_inst_ret(func, TB_NULL_REG);
				} else {
					tb_inst_ret(func, as_rvalue(func, e));
				}
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
				
				// Cast to bool
				TB_Register reg = cvt2rval(func, cond, sp->if_.cond);
				tb_inst_if(func, reg, if_true, if_false);
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
			
			TB_Register cond = as_rvalue(func, sp->while_.cond);
			tb_inst_if(func, cond, body, exit);
			
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
			
			TB_Register cond = as_rvalue(func, sp->while_.cond);
			tb_inst_if(func, cond, body, exit);
			
			tb_inst_label(func, exit);
			break;
		}
		case STMT_FOR: {
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			gen_stmt(func, sp->for_.first);
			
			tb_inst_label(func, header);
			
			TB_Register cond = as_rvalue(func, sp->for_.cond);
			tb_inst_if(func, cond, body, exit);
			
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

static void gen_func_body(TypeIndex type, StmtIndex s, StmtIndex end) {
	// Clear TLS
	tls_init();
	assert(type);
	
	TB_Function* func = tb_function_from_id(mod, stmt_arena.data[s].backing.f);
	
	// Parameters
	ParamIndex param_count = type_arena.data[type].func.param_count;
	
	TB_Register* params = parameter_map = tls_push(param_count * sizeof(TB_Register));
	Type* restrict return_type = &type_arena.data[type_arena.data[type].func.return_type];
	
	// mark return value address (if it applies)
	// and get stack slots for parameters
	if (return_type->kind == KIND_STRUCT ||
		return_type->kind == KIND_UNION) {
		return_value_address = tb_inst_param_addr(func, 0);
		
		// gimme stack slots
		for (int i = 0; i < param_count; i++) {
			params[i] = tb_inst_param_addr(func, 1+i);
		}
	} else {
		return_value_address = TB_NULL_REG;
		
		// gimme stack slots
		for (int i = 0; i < param_count; i++) {
			params[i] = tb_inst_param_addr(func, i);
		}
	}
	
	// TODO(NeGate): Ok fix this up later but essentially we need to prepass
	// over the nodes to find the label statements, then forward declare the labels
	// in TB.
	StmtIndex body = (StmtIndex)stmt_arena.data[s].decl.initial;
	
	for (StmtIndex i = body; i < end; i++) {
		if (stmt_arena.data[i].op == STMT_LABEL) {
			stmt_arena.data[i].backing.l = tb_inst_new_label_id(func);
		}
	}
	
	// Body
	function_type = type;
	gen_stmt(func, body);
	function_type = 0;
	
	{
		Type* restrict return_type = &type_arena.data[type_arena.data[type].func.return_type];
		TB_Register last = tb_node_get_last_register(func);
		if (tb_node_is_label(func, last) || !tb_node_is_terminator(func, last)) {
			if (return_type->kind != KIND_VOID &&
				return_type->kind != KIND_STRUCT &&
				return_type->kind != KIND_UNION) {
				// Needs return value
				sema_fatal(stmt_arena.data[s].loc, "Expected return with value.");
			}
			
			tb_inst_ret(func, TB_NULL_REG);
		}
	}
	
	if (settings.print_tb_ir) {
		tb_function_print(func, stdout);
		printf("\n\n\n");
	}
	
	if (settings.optimization_level != TB_OPT_O0) {
		tb_function_optimize(func, settings.optimization_level);
	}
	
	tb_module_compile_func(mod, func);
	tb_function_free(func);
}

void gen_ir(TopLevel tl, size_t i) {
	StmtIndex s = tl.arr[i];
	
	if (stmt_arena.data[s].op == STMT_FUNC_DECL) {
		TypeIndex type = stmt_arena.data[s].decl.type;
		assert(type_arena.data[type].kind == KIND_FUNC);
		
		StmtIndex end = i + 1 < arrlen(tl.arr) ? tl.arr[i+1] : stmt_arena.count;
		gen_func_body(type, s, end);
	}
}
