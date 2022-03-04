#include "ir_gen.h"
#include "settings.h"
#include "../timer.h"
#include <stdarg.h>
#include <targets/targets.h>

TB_Module* mod;
atomic_size_t function_count;

// Maps param_num -> TB_Register
static thread_local TB_Register* parameter_map;
static thread_local TypeIndex function_type;
static thread_local const char* function_name;

// For aggregate returns
static thread_local TB_Register return_value_address;

TB_Function* static_init_func;
static mtx_t static_init_mutex;

// it's a single file so we wanna lock when writing to data races
static mtx_t emit_ir_mutex;

_Noreturn void internal_error(const char* fmt, ...) {
	printf("internal compiler error: ");
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	abort();
}

static TB_Register cast_reg(TB_Function* func, TB_Register reg, const Type* src, const Type* dst) {
	// Cast into correct type
	if (src->kind >= KIND_CHAR &&
		src->kind <= KIND_LONG &&
		dst->kind >= KIND_CHAR && 
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
			   dst->kind == KIND_BOOL) {
		reg = tb_inst_cmp_ne(func, reg, tb_inst_uint(func, ctype_to_tbtype(src), 0));
	} else if (src->kind == KIND_BOOL &&
			   dst->kind >= KIND_CHAR &&
			   dst->kind <= KIND_LONG) {
		reg = tb_inst_zxt(func, reg, ctype_to_tbtype(dst));
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
		TB_DataType dt = tb_function_get_node(func, reg)->dt;
		
		if (!(dt.type == TB_F64 && dt.width == 0)) {
			reg = tb_inst_fpxt(func, reg, TB_TYPE_F64);
		}
	} else if (src->kind == KIND_DOUBLE &&
			   dst->kind == KIND_FLOAT) {
		TB_DataType dt = tb_function_get_node(func, reg)->dt;
		
		if (!(dt.type == TB_F32 && dt.width == 0)) {
			reg = tb_inst_trunc(func, reg, TB_TYPE_F32);
		}
	} else if (src->kind >= KIND_FLOAT &&
			   src->kind <= KIND_DOUBLE &&
			   dst->kind >= KIND_CHAR &&
			   dst->kind <= KIND_LONG) {
		reg = tb_inst_float2int(func, reg, ctype_to_tbtype(dst));
	} else if (src->kind >= KIND_CHAR &&
			   src->kind <= KIND_LONG &&
			   dst->kind >= KIND_FLOAT &&
			   dst->kind <= KIND_DOUBLE) {
		reg = tb_inst_int2float(func, reg, ctype_to_tbtype(dst));
	}
	
	assert(reg);
	return reg;
}

static TB_Register cvt2rval(TranslationUnit* tu, TB_Function* func, const IRVal v, ExprIndex e) {
	const Expr* restrict ep = &tu->exprs[e];
	const Type* src = &tu->types[ep->type];
	
	TB_Register reg = 0;
	switch (v.value_type) {
		case RVALUE: {
			reg = v.reg; 
			break;
		}
		case LVALUE: {
			// Implicit array to pointer
			if (src->kind == KIND_ARRAY) {
				// just pass the address don't load
				const Type* cast_type = &tu->types[ep->cast_type];
				//assert(cast_type->kind == KIND_PTR && cast_type->ptr_to == src->array_of);
				
				src = cast_type;
				reg = v.reg;
			} else {
				reg = tb_inst_load(func, ctype_to_tbtype(src), v.reg, src->align);
			}
			break;
		}
		case LVALUE_BITS: {
			uint64_t mask = (UINT64_MAX >> (64ull - v.bits.width));
			TB_DataType dt = ctype_to_tbtype(src);
			
			reg = tb_inst_load(func, dt, v.reg, src->align);
			if (v.bits.width != (src->size*8)) {
				reg = tb_inst_and(func, reg, tb_inst_uint(func, dt, mask));
			}
			
			if (v.bits.offset) {
				reg = tb_inst_shr(func, reg, tb_inst_uint(func, dt, v.bits.offset));
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
	
	const Type* dst = &tu->types[ep->cast_type];
	return src != dst ? cast_reg(func, reg, src, dst) : reg;
}

static TB_Register irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, ExprIndex e) {
	return cvt2rval(tu, func, irgen_expr(tu, func, e), e);
}

InitNode* count_max_tb_init_objects(int node_count, InitNode* node, int* out_count) {
	for (int i = 0; i < node_count; i++) {
		if (node->kids_count == 0) {
			*out_count += 1;
			node += 1;
		} else {
			node = count_max_tb_init_objects(node->kids_count, node + 1, out_count);
		}
	}
	
	return node;
}

// TODO(NeGate): Revisit this code as a smarter man...
// if the addr is 0 then we only apply constant initializers.
// func doesn't need to be non-NULL if it's addr is NULL.
InitNode* eval_initializer_objects(TranslationUnit* tu, TB_Function* func, SourceLocIndex loc, TB_InitializerID init, TB_Register addr, TypeIndex t, int node_count, InitNode* node, int offset) {
	// Identify boundaries:
	//   Scalar types are 1
	//   Records depend on the member count
	//   Arrays are based on array count
	int bounds;
	{
		Type* restrict type = &tu->types[t];
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
		Type* restrict type = &tu->types[t];
		
		// if there's no designator then the cursor
		// just keeps incrementing
		int pos, pos_end;
		if (node->mode == INIT_MEMBER) {
			if (type->kind != KIND_STRUCT && type->kind != KIND_UNION) {
				internal_error("Cannot get the member of a non-record type.");
			}
			
			pos = pos_end = -1;
			
			MemberIndex start = type->record.kids_start;
			MemberIndex end = type->record.kids_end;
			for (MemberIndex m = start; m < end; m++) {
				Member* member = &tu->members[m];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(node->member_name, member->name)) {
					pos = (m - start);
					pos_end = pos + 1;
					cursor = pos_end;
					break;
				}
			}
			
			if (pos < 0) {
				internal_error("Could not find member under that name.");
			}
		} else if (node->mode == INIT_ARRAY) {
			if (type->kind != KIND_ARRAY) {
				internal_error("Cannot apply array initializer to non-array type.");
			}
			
			pos = node->start;
			pos_end = node->start + node->count;
			cursor = pos_end;
		} else {
			//if (type->kind != KIND_STRUCT && type->kind != KIND_UNION && type->kind != KIND_ARRAY) {
			//irgen_fatal(loc, "Compound literal with multiple elements must be a struct, union or array.");
			//}
			
			pos = cursor;
			pos_end = cursor + 1;
			cursor++;
		}
		
		// Validate indices
		if (pos < 0 || pos >= bounds) {
			internal_error("Initializer out of range, TODO error ugly");
		} else if (pos_end <= 0 && pos_end > bounds) {
			internal_error("Initializer out of range, TODO error ugly");
		}
		
		// TODO(NeGate): Implement array range initializer
		if (pos + 1 != pos_end) {
			internal_error("TODO");
		}
		
		// Identify entry type
		TypeIndex child_type;
		int relative_offset;
		if (type->kind == KIND_ARRAY) {
			child_type = type->array_of;
			relative_offset = tu->types[type->array_of].size * pos;
		} else if (type->kind == KIND_UNION || type->kind == KIND_STRUCT) {
			child_type = tu->members[type->record.kids_start + pos].type;
			relative_offset = tu->members[type->record.kids_start + pos].offset;
		} else {
			child_type = t;
			relative_offset = 0;
		}
		
		if (node->kids_count > 0) {
			node = eval_initializer_objects(tu, func, loc, init, addr,
											child_type, node->kids_count,
											node + 1, offset + relative_offset);
		} else {
			// initialize a value
			assert(node->expr);
			
			bool success = false;
			if (!func && tu->exprs[node->expr].op == EXPR_SYMBOL) {
				StmtIndex stmt = tu->exprs[node->expr].symbol;
				StmtOp stmt_op = tu->stmts[stmt].op;
				
				// hacky just to make it possible for some symbols to appear in the 
				// initializers
				// TODO(NeGate): Fix it up so that more operations can be
				// performed at compile time and baked into the initializer
				if (stmt_op == STMT_GLOBAL_DECL) {
					tb_initializer_add_global(mod, init, offset + relative_offset, tu->stmts[stmt].backing.g);
					success = true;
				} else if (stmt_op == STMT_FUNC_DECL) {
					tb_initializer_add_function(mod, init, offset + relative_offset, tu->stmts[stmt].backing.f);
					success = true;
				}
			}
			
			if (!success) {
				switch (tu->exprs[node->expr].op) {
					// TODO(NeGate): Implement constants for literals
					// to allow for more stuff to be precomputed.
					case EXPR_INT:
					case EXPR_ENUM:
					case EXPR_NEGATE:
					if (!func) {
						int size = tu->types[child_type].size;
						void* region = tb_initializer_add_region(mod, init, offset + relative_offset, size);
						
						ConstValue value = const_eval(tu, node->expr);
						
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#error "Stop this immoral bullshit please... until someone fixes this at least :p"
#else
						memcpy(region, &value.unsigned_value, size);
#endif
						break;
					}
					// fallthrough
					// dynamic expressions
					default: if (addr) {
						assert(func);
						
						TypeKind kind = tu->types[child_type].kind;
						int size = tu->types[child_type].size;
						int align = tu->types[child_type].align;
						
						if (kind == KIND_STRUCT || kind == KIND_UNION || kind == KIND_ARRAY) {
							IRVal v = irgen_expr(tu, func, node->expr);
							
							if (!type_equal(tu, v.type, child_type)) {
								internal_error("TODO: error messages");
							}
							
							// placing the address calculation here might improve performance or readability
							// of IR in the debug builds, for release builds it shouldn't matter
							TB_Register effective_addr;
							if (offset + relative_offset) {
								effective_addr = tb_inst_member_access(func, addr, offset + relative_offset);
							} else {
								effective_addr = addr;
							}
							
							TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
							tb_inst_memcpy(func, effective_addr, v.reg, size_reg, align);
						} else {
							// hacky but we set the cast so that the rvalue returns a normal value
							tu->exprs[node->expr].cast_type = child_type;
							
							TB_Register v = irgen_as_rvalue(tu, func, node->expr);
							
							// placing the address calculation here might improve performance or readability
							// of IR in the debug builds, for release builds it shouldn't matter
							TB_Register effective_addr;
							if (offset + relative_offset) {
								effective_addr = tb_inst_member_access(func, addr, offset + relative_offset);
							} else {
								effective_addr = addr;
							}
							
							tb_inst_store(func, ctype_to_tbtype(&tu->types[child_type]), effective_addr, v, align);
						}
						break;
					}
				}
			}
			
			node += 1;
		}
	}
	
	return node;
}

static void gen_local_initializer(TranslationUnit* tu, TB_Function* func, SourceLocIndex loc, TB_Register addr, TypeIndex t, int node_count, InitNode* nodes) {
	// Walk initializer for max constant expression initializers.
	int max_tb_objects = 0;
	count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
	
	TB_InitializerID init = tb_initializer_create(mod, 
												  tu->types[t].size,
												  tu->types[t].align, 
												  max_tb_objects);
	
	// Initialize all const expressions
	eval_initializer_objects(tu, func, loc, init, TB_NULL_REG, t, node_count, nodes, 0);
	tb_inst_initialize_mem(func, addr, init);
	
	// Initialize all dynamic expressions
	eval_initializer_objects(tu, func, loc, init, addr, t, node_count, nodes, 0);
}

static void insert_label(TB_Function* func) {
	if (tb_inst_get_current_label(func) == 0) {
		tb_inst_label(func, tb_inst_new_label_id(func));
	}
}

static IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, ExprIndex e) {
	Expr* restrict ep = &tu->exprs[e];
	
	switch (ep->op) {
		case EXPR_CHAR: {
			// TODO(NeGate): Maybe multichar literals
			assert(ep->str.start[0] == '\'');
			
			char ch = ep->str.start[1];
			if (ch == '\\') {
				switch (ep->str.start[2]) {
					case '0': ch = '\0'; break;
					case '\\': ch = '\\'; break;
					case 'a': ch = '\a'; break;
					case 'b': ch = '\b'; break;
					case 't': ch = '\t'; break;
					case 'n': ch = '\n'; break;
					case 'v': ch = '\v'; break;
					case 'f': ch = '\f'; break;
					case 'r': ch = '\r'; break;
					default: internal_error("Could not recognize escape char literal.");
				}
				
				assert(ep->str.start[3] == '\'');
			} else {
				assert(ep->str.start[2] == '\'');
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_CHAR,
				.reg = tb_inst_uint(func, TB_TYPE_I8, ch)
			};
		}
		case EXPR_INT: {
			TB_DataType dt = ctype_to_tbtype(&tu->types[ep->type]);
			
			if (tu->types[ep->type].is_unsigned) {
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
		case EXPR_ENUM: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_sint(func, TB_TYPE_I32, ep->enum_val.num)
			};
		}
		case EXPR_FLOAT32: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->cast_type,
				.reg = tb_inst_float(func, ep->cast_type == TYPE_DOUBLE ? TB_TYPE_F64 : TB_TYPE_F32, ep->float_num)
			};
		}
		case EXPR_FLOAT64: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->cast_type,
				.reg = tb_inst_float(func, ep->cast_type == TYPE_FLOAT ? TB_TYPE_F32 : TB_TYPE_F64, ep->float_num)
			};
		}
		case EXPR_STR: {
			// The string is preprocessed to be a flat and nice byte buffer by the semantics pass
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_string(func, ep->str.end - ep->str.start, (char*)ep->str.start)
			};
		}
		case EXPR_INITIALIZER: {
			Type* ty = &tu->types[ep->init.type];
			TB_Register addr = tb_inst_local(func, ty->size, ty->align);
			
			gen_local_initializer(tu, func, ep->loc, addr, ep->init.type, ep->init.count, ep->init.nodes);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->init.type,
				.reg = addr
			};
		}
		case EXPR_FUNCTION: {
			assert(tu->stmts[ep->func.src].op == STMT_FUNC_DECL);
			
			return (IRVal) {
				.value_type = LVALUE_FUNC,
				.type = ep->type,
				.func = tb_function_from_id(mod, tu->stmts[ep->func.src].backing.f)
			};
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			StmtOp stmt_op = tu->stmts[stmt].op;
			assert(stmt_op == STMT_DECL || stmt_op == STMT_LABEL || stmt_op == STMT_GLOBAL_DECL || stmt_op == STMT_FUNC_DECL);
			
			TypeIndex type_index = tu->stmts[stmt].decl.type;
			Type* type = &tu->types[type_index];
			
			if (stmt_op == STMT_GLOBAL_DECL) {
				return (IRVal) {
					.value_type = LVALUE,
					.type = type_index,
					.reg = tb_inst_get_global_address(func, tu->stmts[stmt].backing.g)
				};
			} else if (stmt_op == STMT_LABEL) {
				if (tu->stmts[stmt].backing.l == 0) {
					tu->stmts[stmt].backing.l = tb_inst_new_label_id(func);
				}
				
				return (IRVal) {
					.value_type = LVALUE_LABEL,
					.type = TYPE_NONE,
					.label = tu->stmts[stmt].backing.l
				};
			} else if (type->kind == KIND_FUNC) {
				if (stmt_op == STMT_FUNC_DECL) {
					return (IRVal) {
						.value_type = LVALUE_FUNC,
						.type = type_index,
						.func = tb_function_from_id(mod, tu->stmts[stmt].backing.f)
					};
				} else if (stmt_op == STMT_DECL) {
					return (IRVal) {
						.value_type = LVALUE_EFUNC,
						.type = type_index,
						.ext = tu->stmts[stmt].backing.e
					};
				}
			}
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type_index,
				.reg = tu->stmts[stmt].backing.r
			};
		}
		case EXPR_PARAM: {
			int param_num = ep->param_num;
			TB_Register reg = parameter_map[param_num];
			
			ParamIndex param = tu->types[function_type].func.param_list + param_num;
			TypeIndex arg_type = tu->params[param].type;
			assert(arg_type);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = arg_type,
				.reg = reg
			};
		}
		case EXPR_ADDR: {
			uint64_t dst;
			if (const_eval_try_offsetof_hack(tu, e, &dst)) {
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = tb_inst_uint(func, TB_TYPE_PTR, dst)
				};
			}
			
			IRVal src = irgen_expr(tu, func, ep->unary_op.src);
			
			if (src.value_type == LVALUE) {
				src.type = ep->type;
				src.value_type = RVALUE;
				return src;
			} else if (src.value_type == LVALUE_EFUNC) {
				src.type = ep->type;
				src.value_type = RVALUE;
				src.reg = tb_inst_get_extern_address(func, src.ext);
				return src;
			} else if (src.value_type == LVALUE_FUNC) {
				src.type = ep->type;
				src.value_type = RVALUE;
				src.reg = tb_inst_get_func_address(func, src.func);
				return src;
			} else {
				abort();
			}
		}
		case EXPR_LOGICAL_NOT: {
			TB_Register reg = irgen_as_rvalue(tu, func, ep->unary_op.src);
			TB_DataType dt = tb_function_get_node(func, reg)->dt;
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_cmp_eq(func, reg, tb_inst_uint(func, dt, 0))
			};
		}
		case EXPR_NOT: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_not(func, irgen_as_rvalue(tu, func, ep->unary_op.src))
			};
		}
		case EXPR_NEGATE: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_neg(func, irgen_as_rvalue(tu, func, ep->unary_op.src))
			};
		}
		case EXPR_CAST: {
			TB_Register src = irgen_as_rvalue(tu, func, ep->cast.src);
			
			// stuff like ((void) x)
			if (tu->types[ep->cast.type].kind == KIND_VOID) {
				return (IRVal) { .value_type = RVALUE, .type = TYPE_VOID, .reg = 0 };
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->cast.type,
				.reg = src
			};
		}
		case EXPR_DEREF: {
			// TODO(NeGate): Kinda messy...
			Type* restrict type = &tu->types[tu->exprs[ep->unary_op.src].type];
			TB_Register reg = irgen_as_rvalue(tu, func, ep->unary_op.src);
			
			if (type->kind == KIND_PTR && type->is_ptr_restrict) {
				reg = tb_inst_restrict(func, reg);
			}
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->type,
				.reg = reg
			};
		}
		case EXPR_CALL: {
			ExprIndex* args = ep->call.param_start;
			int arg_count = ep->call.param_count;
			
			// Try to see if it's an intrinsic
			if (tu->exprs[ep->call.target].op == EXPR_SYMBOL) {
				StmtIndex sym = tu->exprs[ep->call.target].symbol;
				
				if (tu->stmts[sym].op == STMT_DECL) {
					const char* name = (const char*)tu->stmts[sym].decl.name;
					
					// all builtins start with an underscore
					if (*name == '_') {
						ptrdiff_t search = shgeti(target_desc.builtin_func_map, name);
						if (search >= 0) {
							TB_Register val = target_desc.compile_builtin(tu, func, name, arg_count, args);
							
							return (IRVal) {
								.value_type = RVALUE,
								.type = ep->type,
								.reg = val
							};
						}
					}
				}
			}
			
			// NOTE(NeGate): returning aggregates requires us
			// to allocate our own space and pass it to the 
			// callee.
			bool is_aggregate_return = false;
			if (tu->types[ep->type].kind == KIND_STRUCT ||
				tu->types[ep->type].kind == KIND_UNION) {
				is_aggregate_return = true;
			}
			
			// Resolve parameters
			size_t real_arg_count = arg_count + is_aggregate_return;
			TB_Reg* ir_args = tls_push(real_arg_count * sizeof(TB_Reg));
			
			if (is_aggregate_return) {
				ir_args[0] = tb_inst_local(func, tu->types[ep->type].size, tu->types[ep->type].align);
			}
			
			// point at which it stops being know which parameter types we're
			// mapping to, if it's arg_count then there's really none
			size_t varargs_cutoff = arg_count;
			Type* func_type = &tu->types[tu->exprs[ep->call.target].type];
			if (func_type->func.has_varargs) {
				varargs_cutoff = func_type->func.param_count;
			}
			
			for (size_t i = 0; i < arg_count; i++) {
				TB_Reg arg = irgen_as_rvalue(tu, func, args[i]);
				TB_DataType dt = tb_function_get_node(func, arg)->dt;
				
				// convert any float variadic arguments into integers
				if (i >= varargs_cutoff && dt.type == TB_F64 && dt.width == 0) {
					arg = tb_inst_bitcast(func, arg, TB_TYPE_I64);
				}
				
				ir_args[is_aggregate_return+i] = arg;
			}
			
			// Resolve call target
			//
			// NOTE(NeGate): Could have been resized in the parameter's irgen_expr
			// so we reload the pointer.
			IRVal func_ptr = irgen_expr(tu, func, ep->call.target);
			
			TB_DataType dt = ctype_to_tbtype(&tu->types[ep->type]);
			if (is_aggregate_return) dt = TB_TYPE_VOID;
			
			TB_Reg r;
			if (func_ptr.value_type == LVALUE_FUNC) {
				r = tb_inst_call(func, dt, func_ptr.func, real_arg_count, ir_args);
			} else if (func_ptr.value_type == LVALUE_EFUNC) {
				r = tb_inst_ecall(func, dt, func_ptr.ext, real_arg_count, ir_args);
			} else {
				TB_Reg target_reg = cvt2rval(tu, func, func_ptr, ep->call.target);
				
				r = tb_inst_vcall(func, dt, target_reg, real_arg_count, ir_args);
			}
			
			if (is_aggregate_return) {
				TB_Register result = ir_args[0];
				tls_restore(ir_args);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = result
				};
			} else {
				tls_restore(ir_args);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = ep->type,
					.reg = r
				};
			}
		}
		case EXPR_SUBSCRIPT: {
			TB_Register base = irgen_as_rvalue(tu, func, ep->subscript.base);
			TB_Register index = irgen_as_rvalue(tu, func, ep->subscript.index);
			
			int stride = tu->types[ep->type].size;
			return (IRVal) {
				.value_type = LVALUE,
				.type = ep->type,
				.reg = tb_inst_array_access(func, base, index, stride)
			};
		}
		case EXPR_DOT: {
			IRVal src = irgen_expr(tu, func, ep->dot.base);
			assert(src.value_type == LVALUE);
			
			assert(ep->dot.member);
			Member* member = &tu->members[ep->dot.member];
			
			if (member->is_bitfield) {
				return (IRVal) {
					.value_type = LVALUE_BITS,
					.type = member->type,
					.bits = {
						.reg = tb_inst_member_access(func, src.reg, member->offset),
						.offset = member->bit_offset, .width = member->bit_width
					}
				};
			} else {
				return (IRVal) {
					.value_type = LVALUE,
					.type = member->type,
					.reg = tb_inst_member_access(func, src.reg, member->offset)
				};
			} 
		}
		case EXPR_ARROW: {
			TB_Register src = irgen_as_rvalue(tu, func, ep->arrow.base);
			
			assert(ep->arrow.member);
			Member* member = &tu->members[ep->arrow.member];
			
			if (member->is_bitfield) {
				return (IRVal) {
					.value_type = LVALUE_BITS,
					.type = member->type,
					.bits = {
						.reg = tb_inst_member_access(func, src, member->offset),
						.offset = member->bit_offset, .width = member->bit_width
					}
				};
			} else {
				return (IRVal) {
					.value_type = LVALUE,
					.type = member->type,
					.reg = tb_inst_member_access(func, src, member->offset)
				};
			}
		}
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC: {
			bool is_inc = (ep->op == EXPR_PRE_INC);
			
			IRVal src = irgen_expr(tu, func, ep->unary_op.src);
			assert(src.value_type == LVALUE);
			
			TB_Register loaded = cvt2rval(tu, func, src, ep->unary_op.src);
			Type* type = &tu->types[ep->type];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_sint(func, TB_TYPE_PTR, tu->types[type->ptr_to].size);
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
			
			IRVal src = irgen_expr(tu, func, ep->unary_op.src);
			assert(src.value_type == LVALUE);
			
			TB_Register loaded = cvt2rval(tu, func, src, ep->unary_op.src);
			Type* type = &tu->types[ep->type];
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_sint(func, TB_TYPE_PTR, tu->types[type->ptr_to].size);
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
		// based on this:
		// https://github.com/c3lang/c3c/blob/cf56825d26758044fe2a868e65717635864fd723/src/compiler/llvm_codegen_expr.c#L2526
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
			TB_Label phi_lbl = tb_inst_new_label_id(func);
			
			// Eval first operand
			TB_Register lhs = irgen_as_rvalue(tu, func, ep->bin_op.left);
			
			TB_Label start_block = tb_inst_get_current_label(func);
			TB_Register result_on_skip = tb_inst_uint(func, TB_TYPE_BOOL, is_and ? 0 : 1);
			if (is_and) {
				tb_inst_if(func, lhs, try_rhs_lbl, phi_lbl);
			} else {
				tb_inst_if(func, lhs, phi_lbl, try_rhs_lbl);
			}
			
			// Eval second operand
			tb_inst_label(func, try_rhs_lbl);
			
			TB_Register rhs = irgen_as_rvalue(tu, func, ep->bin_op.right);
			if (tb_function_get_node(func, rhs)->dt.type != TB_BOOL) {
				TB_DataType dt = tb_function_get_node(func, rhs)->dt;
				
				rhs = tb_inst_cmp_ne(func, rhs, tb_inst_uint(func, dt, 0));
			}
			
			TB_Label end_block = tb_inst_get_current_label(func);
			
			// phi node merging block
			tb_inst_label(func, phi_lbl);
			TB_Register phi = tb_inst_phi2(func, start_block, result_on_skip, end_block, rhs);
			
			// we delay placement of the labels so that we can
			// fold multiple shortcircuits together
			return (IRVal) {
				.value_type = RVALUE,
				.type = TYPE_BOOL,
				.reg = phi
			};
		}
		case EXPR_COMMA: {
			irgen_expr(tu, func, ep->bin_op.left);
			return irgen_expr(tu, func, ep->bin_op.right);
		}
		case EXPR_PTRADD:
		case EXPR_PTRSUB: {
			TB_Register l = irgen_as_rvalue(tu, func, ep->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, ep->bin_op.right);
			
			Type* restrict type = &tu->types[ep->type];
			
			// pointer arithmatic
			int dir = ep->op == EXPR_PTRADD ? 1 : -1;
			int stride = tu->types[type->ptr_to].size;
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = tb_inst_array_access(func, l, r, dir * stride)
			};
		}
		case EXPR_PTRDIFF: {
			TB_Register l = irgen_as_rvalue(tu, func, ep->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, ep->bin_op.right);
			
			Type* restrict type = &tu->types[tu->exprs[ep->bin_op.left].cast_type];
			int stride = tu->types[type->ptr_to].size;
			
			l = tb_inst_ptr2int(func, l, TB_TYPE_I64);
			r = tb_inst_ptr2int(func, r, TB_TYPE_I64);
			
			TB_Register diff = tb_inst_sub(func, l, r, TB_ASSUME_NSW);
			TB_Register diff_in_elems = tb_inst_div(func, diff, tb_inst_sint(func, tb_function_get_node(func, diff)->dt, stride), true);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = ep->type,
				.reg = diff_in_elems
			};
		}
		case EXPR_PLUS:
		case EXPR_MINUS:
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_PERCENT:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR: {
			TB_Register l = irgen_as_rvalue(tu, func, ep->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, ep->bin_op.right);
			Type* restrict type = &tu->types[ep->type];
			
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
					case EXPR_PERCENT: data = tb_inst_mod(func, l, r, !type->is_unsigned); break;
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
			TB_Register l = irgen_as_rvalue(tu, func, ep->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, ep->bin_op.right);
			
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
			TB_Register l = irgen_as_rvalue(tu, func, ep->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, ep->bin_op.right);
			
			Type* type = &tu->types[tu->exprs[ep->bin_op.left].cast_type];
			
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
			Type* restrict type = &tu->types[ep->type];
			
			// Load inputs
			IRVal lhs = irgen_expr(tu, func, ep->bin_op.left);
			
			TB_Register l = TB_NULL_REG;
			if (ep->op != EXPR_ASSIGN) {
				// don't do this conversion for ASSIGN, since it won't
				// be needing it
				l = cvt2rval(tu, func, lhs, ep->bin_op.left);
			}
			
			IRVal rhs = irgen_expr(tu, func, ep->bin_op.right);
			
			// Try pointer arithmatic
			if ((ep->op == EXPR_PLUS_ASSIGN || ep->op == EXPR_MINUS_ASSIGN) && type->kind == KIND_PTR) {
				int dir = ep->op == EXPR_PLUS_ASSIGN ? 1 : -1;
				int stride = tu->types[type->ptr_to].size;
				
				TB_Register r = cvt2rval(tu, func, rhs, ep->bin_op.right);
				TB_Register arith = tb_inst_array_access(func, l, r, dir * stride);
				
				assert(lhs.value_type == LVALUE);
				tb_inst_store(func, TB_TYPE_PTR, lhs.reg, arith, type->align);
				return lhs;
			}
			
			TB_DataType dt = ctype_to_tbtype(type);
			
			TB_Register data;
			if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
				if (ep->op != EXPR_ASSIGN) abort();
				
				if (type->record.intrin_type.type != TB_VOID) {
					tb_inst_store(func, type->record.intrin_type, lhs.reg, rhs.reg, type->align);
				} else {
					TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, type->size);
					tb_inst_memcpy(func, lhs.reg, rhs.reg, size_reg, type->align);
				}
			} else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				TB_Register r = cvt2rval(tu, func, rhs, ep->bin_op.right);
				
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
				TB_Register r = cvt2rval(tu, func, rhs, ep->bin_op.right);
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
				
				if (lhs.value_type == LVALUE_BITS && lhs.bits.width != (type->size*8)) {
					TB_Register old_value = tb_inst_load(func, dt, lhs.reg, type->align);
					
					// mask out the space for our bitfield member
					uint64_t clear_mask = ~((UINT64_MAX >> (64ull - lhs.bits.width)) << lhs.bits.offset);
					old_value = tb_inst_and(func, old_value, tb_inst_uint(func, dt, ~clear_mask));
					
					// mask source value and position it correctly
					uint64_t insert_mask = (UINT64_MAX >> (64ull - lhs.bits.width));
					data = tb_inst_and(func, data, tb_inst_uint(func, dt, insert_mask));
					
					if (lhs.bits.offset) {
						data = tb_inst_shl(func, data, tb_inst_uint(func, dt, lhs.bits.offset), TB_ASSUME_NUW);
					}
					
					// merge
					data = tb_inst_or(func, old_value, data);
					
					tb_inst_store(func, dt, lhs.reg, data, type->align);
				} else {
					assert(lhs.value_type == LVALUE);
					tb_inst_store(func, dt, lhs.reg, data, type->align);
				}
			}
			
			return lhs;
		}
		case EXPR_TERNARY: {
			TB_Register cond = irgen_as_rvalue(tu, func, ep->ternary_op.left);
			
			TB_Label if_true = tb_inst_new_label_id(func);
			TB_Label if_false = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			tb_inst_if(func, cond, if_true, if_false);
			
			TB_Register true_val;
			{
				tb_inst_label(func, if_true);
				
				true_val = irgen_as_rvalue(tu, func, ep->ternary_op.middle);
				tb_inst_goto(func, exit);
			}
			
			TB_Register false_val;
			{
				tb_inst_label(func, if_false);
				
				false_val = irgen_as_rvalue(tu, func, ep->ternary_op.right);
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

static void irgen_stmt(TranslationUnit* tu, TB_Function* func, StmtIndex s) {
	Stmt* restrict sp = &tu->stmts[s];
	
	if (settings.debug_info) {
		// TODO(NeGate): Fix this up later!!!
		static _Thread_local TB_FileID last_file_id = 0;
		static _Thread_local const char* last_filepath = NULL;
		
		SourceLoc* l = &tu->tokens->line_arena[sp->loc];
		if ((const char*)l->file != last_filepath) {
			last_filepath = (const char*)l->file;
			last_file_id = tb_file_create(mod, (const char*)l->file);
		}
		
		tb_inst_loc(func, last_file_id, l->line + 1);
	}
	
	switch (sp->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_LABEL: {
			if (sp->backing.l == 0) {
				sp->backing.l = tb_inst_new_label_id(func);
			}
			
			tb_inst_label(func, sp->backing.l);
			break;
		}
		case STMT_GOTO: {
			IRVal target = irgen_expr(tu, func, sp->goto_.target);
			
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
				irgen_stmt(tu, func, kids[i]);
			}
			break;
		}
		case STMT_FUNC_DECL: {
			// TODO(NeGate): No nested functions... maybe?
			abort();
		}
		case STMT_DECL: {
			Attribs attrs = sp->decl.attrs;
			
			TypeIndex type_index = sp->decl.type;
			int kind = tu->types[type_index].kind;
			int size = tu->types[type_index].size;
			int align = tu->types[type_index].align;
			
			TB_Register addr = 0;
			
			// Allocate from storage
			if (attrs.is_static) {
				TB_InitializerID init = tb_initializer_create(mod, size, align, 0);
				
				char* name = tls_push(1024);
				snprintf(name, 1024, "%s$%s@%d", function_name, sp->decl.name, s);
				
				TB_GlobalID g = tb_global_create(mod, name, TB_LINKAGE_PRIVATE);
				tb_global_set_initializer(mod, g, init);
				tls_restore(name);
				
				assert(sp->decl.initial == 0 && "TODO: Implement local static declaration initialization");
				addr = tb_inst_get_global_address(func, g);
			} else {
				addr = tb_inst_local(func, size, align);
			}
			
			if (sp->decl.initial) {
				Expr* restrict ep = &tu->exprs[sp->decl.initial];
				
				if (ep->op == EXPR_INITIALIZER) {
					gen_local_initializer(tu, func, ep->loc, addr, type_index, ep->init.count, ep->init.nodes);
				} else {
					if (kind == KIND_ARRAY && ep->op == EXPR_STR) {
						IRVal v = irgen_expr(tu, func, sp->decl.initial);
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						
						tb_inst_memcpy(func, addr, v.reg, size_reg, align);
					} else if (kind == KIND_STRUCT || kind == KIND_UNION) {
						if (tu->types[type_index].record.intrin_type.type != TB_VOID) {
							TB_Register v = irgen_as_rvalue(tu, func, sp->decl.initial);
							
							tb_inst_store(func, tu->types[type_index].record.intrin_type, addr, v, align);
						} else {
							IRVal v = irgen_expr(tu, func, sp->decl.initial);
							TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
							
							tb_inst_memcpy(func, addr, v.reg, size_reg, align);
						}
					} else {
						TB_Register v = irgen_as_rvalue(tu, func, sp->decl.initial);
						
						tb_inst_store(func, ctype_to_tbtype(&tu->types[type_index]), addr, v, align);
					}
				}
			} else {
				/* uninitialized */
			}
			
			sp->backing.r = addr;
			break;
		}
		case STMT_EXPR: {
			irgen_expr(tu, func, sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			ExprIndex e = sp->return_.expr;
			
			if (e) {
				TypeIndex type = tu->exprs[e].cast_type;
				
				if (tu->types[type].kind == KIND_STRUCT ||
					tu->types[type].kind == KIND_UNION) {
					if (tu->types[type].record.intrin_type.type != TB_VOID) {
						tb_inst_ret(func, irgen_as_rvalue(tu, func, e));
					} else {
						IRVal v = irgen_expr(tu, func, e);
						
						// returning aggregates just copies into the first parameter
						// which is agreed to be a caller owned buffer.
						int size = tu->types[type].size;
						int align = tu->types[type].align;
						
						TB_Register dst_address = tb_inst_load(func, TB_TYPE_PTR, return_value_address, 8);
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						
						tb_inst_memcpy(func, dst_address, v.reg, size_reg, align);
						tb_inst_ret(func, TB_NULL_REG);
					}
				} else {
					tb_inst_ret(func, irgen_as_rvalue(tu, func, e));
				}
			} else {
				tb_inst_ret(func, TB_NULL_REG);
			}
			break;
		}
		case STMT_IF: {
			// TODO(NeGate): Remove this later because it kills
			// the codegen a bit in certain cases.
			TB_Label entry_lbl = tb_inst_new_label_id(func);
			tb_inst_label(func, entry_lbl);
			
			TB_Register cond = irgen_as_rvalue(tu, func, sp->if_.cond);
			
			TB_Label if_true = tb_inst_new_label_id(func);
			TB_Label if_false = tb_inst_new_label_id(func);
			
			// Cast to bool
			tb_inst_if(func, cond, if_true, if_false);
			
			tb_inst_label(func, if_true);
			irgen_stmt(tu, func, sp->if_.body);
			
			if (sp->if_.next) {
				TB_Label exit = tb_inst_new_label_id(func);
				tb_inst_goto(func, exit);
				
				tb_inst_label(func, if_false);
				irgen_stmt(tu, func, sp->if_.next);
				
				// fallthrough
				tb_inst_label(func, exit);
			} else {
				tb_inst_label(func, if_false);
			}
			break;
		}
		case STMT_WHILE: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			sp->backing.l = exit;
			
			// NOTE(NeGate): this is hacky but as long as it doesn't
			// break we should be good... what am i saying im the 
			// developer of TB :p
			// essentially we can store both the header and exit labels
			// implicitly as one if they're next to each other
			assert(header == exit - 1);
			
			tb_inst_label(func, header);
			
			TB_Register cond = irgen_as_rvalue(tu, func, sp->while_.cond);
			tb_inst_if(func, cond, body, exit);
			
			tb_inst_label(func, body);
			if (sp->while_.body) {
				irgen_stmt(tu, func, sp->while_.body);
			}
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		case STMT_DO_WHILE: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			// NOTE(NeGate): this is hacky but as long as it doesn't
			// break we should be good... what am i saying im the 
			// developer of TB :p
			assert(body == exit - 1);
			sp->backing.l = exit;
			
			tb_inst_label(func, body);
			
			if (sp->do_while.body) {
				irgen_stmt(tu, func, sp->do_while.body);
			}
			
			TB_Register cond = irgen_as_rvalue(tu, func, sp->do_while.cond);
			tb_inst_if(func, cond, body, exit);
			
			tb_inst_label(func, exit);
			break;
		}
		case STMT_FOR: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			sp->backing.l = exit;
			
			// NOTE(NeGate): this is hacky but as long as it doesn't
			// break we should be good... what am i saying im the 
			// developer of TB :p
			// essentially we can store both the header and exit labels
			// implicitly as one if they're next to each other
			assert(header == exit - 1);
			
			if (sp->for_.first) {
				irgen_stmt(tu, func, sp->for_.first);
			}
			
			tb_inst_label(func, header);
			
			if (sp->for_.cond) {
				TB_Register cond = irgen_as_rvalue(tu, func, sp->for_.cond);
				tb_inst_if(func, cond, body, exit);
			} else {
				tb_inst_goto(func, body);
			}
			
			tb_inst_label(func, body);
			
			irgen_stmt(tu, func, sp->for_.body);
			
			if (sp->for_.next) {
				irgen_expr(tu, func, sp->for_.next);
			}
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		case STMT_CONTINUE: {
			// this is really hacky but we always store the loop header label one
			// behind the exit label in terms of IDs.
			TB_Label target = tu->stmts[sp->continue_.target].backing.l - 1;
			tb_inst_goto(func, target);
			break;
		}
		case STMT_BREAK: {
			TB_Label target = tu->stmts[sp->break_.target].backing.l;
			tb_inst_goto(func, target);
			break;
		}
		case STMT_CASE:
		case STMT_DEFAULT: {
			assert(sp->backing.l);
			tb_inst_label(func, sp->backing.l);
			break;
		}
		case STMT_SWITCH: {
			StmtIndex head = sp->switch_.next;
			
			size_t entry_count = 0;
			TB_SwitchEntry* entries = tls_save();
			
			TB_Label default_label = 0;
			while (head) {
				// reserve label
				assert((tu->stmts[head].op == STMT_CASE) ||
					   (tu->stmts[head].op == STMT_DEFAULT));
				
				TB_Label label = tb_inst_new_label_id(func);
				tu->stmts[head].backing.l = label;
				
				if (tu->stmts[head].op == STMT_CASE) {
					assert(tu->stmts[head].case_.key < UINT32_MAX);
					tls_push(sizeof(TB_SwitchEntry));
					entries[entry_count++] = (TB_SwitchEntry) { .key = tu->stmts[head].case_.key, .value = label };
					
					head = tu->stmts[head].case_.next;
				} else if (tu->stmts[head].op == STMT_DEFAULT) {
					assert(default_label == 0);
					default_label = label;
					
					head = tu->stmts[head].default_.next;
				} else assert(0);
			}
			
			TB_Label break_label = tb_inst_new_label_id(func);
			sp->backing.l = break_label;
			
			// default to fallthrough
			if (!default_label) {
				default_label = break_label;
			}
			
			TB_Register key = irgen_as_rvalue(tu, func, sp->switch_.condition);
			TB_DataType dt = tb_function_get_node(func, key)->dt;
			
			tb_inst_switch(func, dt, key, default_label, entry_count, entries);
			tb_inst_label(func, tb_inst_new_label_id(func));
			
			irgen_stmt(tu, func, sp->switch_.body);
			
			insert_label(func);
			break;
		}
		default:
		__builtin_unreachable();
	}
}

static void gen_func_body(TranslationUnit* tu, TypeIndex type, StmtIndex s) {
	// Clear TLS
	tls_init();
	assert(type);
	
	TB_Function* func = tb_function_from_id(mod, tu->stmts[s].backing.f);
	
	// Parameters
	ParamIndex param_count = tu->types[type].func.param_count;
	
	TB_Register* params = parameter_map = tls_push(param_count * sizeof(TB_Register));
	Type* restrict return_type = &tu->types[tu->types[type].func.return_type];
	
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
	StmtIndex body = (StmtIndex)tu->stmts[s].decl.initial;
	
	// main needs to call the static init
	if (tu->stmts[s].decl.name && 
		(strcmp((const char*)tu->stmts[s].decl.name, "main") == 0 ||
		 strcmp((const char*)tu->stmts[s].decl.name, "WinMain") == 0)) {
		tb_inst_call(func, TB_TYPE_VOID, static_init_func, 0, NULL);
	}
	
	// Body
	function_type = type;
	function_name = (const char*)tu->stmts[s].decl.name;
	
	irgen_stmt(tu, func, body);
	
	function_name = NULL;
	function_type = 0;
	
	{
		Type* restrict return_type = &tu->types[tu->types[type].func.return_type];
		TB_Register last = tb_node_get_last_register(func);
		if (tb_node_is_label(func, last) || !tb_node_is_terminator(func, last)) {
			if (return_type->kind != KIND_VOID &&
				return_type->kind != KIND_STRUCT &&
				return_type->kind != KIND_UNION) {
				// Needs return value
				//irgen_warn(tu->stmts[s].loc, "Expected return with value.");
			}
			
			tb_inst_ret(func, TB_NULL_REG);
		}
	}
	
	if (settings.print_tb_ir) {
		if (mtx_lock(&emit_ir_mutex) != thrd_success) {
			printf("internal compiler error: mtx_lock(...) failure!");
			abort();
		}
		
		tb_function_print(func, tb_default_print_callback, stdout);
		fprintf(stdout, "\n\n\n");
		fflush(stdout);
		
		if (mtx_unlock(&emit_ir_mutex) != thrd_success) {
			printf("internal compiler error: mtx_unlock(...) failure!");
			abort();
		}
	} else {
		if (!settings.optimize) tb_module_compile_func(mod, func, /* settings.optimize ? TB_ISEL_COMPLEX : */ TB_ISEL_FAST);
	}
	
	// if we're optimizing, we need to keep the IR longer so we can compile later
	if (!settings.optimize) tb_function_free(func);
	function_count++;
}

void irgen_init() {
	mtx_init(&emit_ir_mutex, mtx_plain);
	mtx_init(&static_init_mutex, mtx_plain);
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(target_arch, target_system, &features);
	
	// static initialization is stuffed into a function which is called at the
	// start of main(), regardless i wanna get rid of this soon enough but haven't...
	TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, TB_TYPE_VOID, 0, false);
	static_init_func = tb_prototype_build(mod, proto, "__static_init", TB_LINKAGE_PRIVATE);
}

void irgen_deinit() {
}

void irgen_finalize() {
	tb_inst_ret(static_init_func, TB_NULL_REG);
	
	if (settings.print_tb_ir) {
		mtx_lock(&emit_ir_mutex);
		
		tb_function_print(static_init_func, tb_default_print_callback, stdout);
		fprintf(stdout, "\n\n\n");
		fflush(stdout);
		
		mtx_unlock(&emit_ir_mutex);
	} else if (!settings.optimize) {
		tb_module_compile_func(mod, static_init_func, TB_ISEL_FAST);
	}
}

void irgen_top_level_stmt(TranslationUnit* tu, StmtIndex s) {
	Stmt* restrict sp = &tu->stmts[s];
	
	if (sp->op == STMT_FUNC_DECL) {
		TypeIndex type = sp->decl.type;
		assert(tu->types[type].kind == KIND_FUNC);
		
		if (sp->decl.attrs.is_static ||
			sp->decl.attrs.is_inline) {
			if (!sp->decl.attrs.is_used) return;
		}
		
		gen_func_body(tu, type, s);
	} else if (sp->op == STMT_DECL || sp->op == STMT_GLOBAL_DECL) {
		if (!sp->decl.attrs.is_used) return;
		if (sp->decl.attrs.is_typedef) return;
		if (sp->decl.attrs.is_extern || tu->types[sp->decl.type].kind == KIND_FUNC) return;
		
		TB_GlobalID global = sp->backing.g;
		Type* type = &tu->types[sp->decl.type];
		
		Expr* restrict ep = &tu->exprs[sp->decl.initial];
		if (ep->op == EXPR_INITIALIZER) {
			TypeIndex t = ep->init.type;
			if (t == TYPE_VOID) {
				ep->init.type = t = sp->decl.type;
			}
			
			size_t node_count = ep->init.count;
			InitNode* nodes = ep->init.nodes;
			
			// Walk initializer for max constant expression initializers.
			int max_tb_objects = 0;
			count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
			
			TB_InitializerID init;
			if (ep->op == EXPR_STR) {
				init = tb_initializer_create(mod, type->size, type->align, 1);
				
				char* dst = tb_initializer_add_region(mod, init, 0, type->size);
				Expr* restrict ep = &tu->exprs[sp->decl.initial];
				
				memcpy(dst, ep->str.start, ep->str.end - ep->str.start);
				
				if (tu->types[sp->decl.type].kind == KIND_PTR) {
					// if it's a string pointer, then we make a dummy string array
					// and point to that with another initializer
					char temp[1024];
					snprintf(temp, 1024, "%s@%d", sp->decl.name, s);
					TB_GlobalID dummy = tb_global_create(mod, temp, TB_LINKAGE_PRIVATE);
					tb_global_set_initializer(mod, dummy, init);
					
					init = tb_initializer_create(mod, 8, 8, 1);
					tb_initializer_add_global(mod, init, 0, dummy);
				}
			} else if (ep->op == EXPR_INITIALIZER) {
				Expr* restrict ep = &tu->exprs[sp->decl.initial];
				
				int node_count = ep->init.count;
				InitNode* nodes = ep->init.nodes;
				
				// Walk initializer for max constant expression initializers.
				int max_tb_objects = 0;
				count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
				
				init = tb_initializer_create(mod, type->size, type->align, max_tb_objects);
				
				// Initialize all const expressions
				eval_initializer_objects(tu, NULL, sp->loc, init, TB_NULL_REG, sp->decl.type, node_count, nodes, 0);
			} else {
				// uninitialized, just all zeroes
				init = tb_initializer_create(mod, type->size, type->align, 0);
			}
			tb_global_set_initializer(mod, global, init);
		} else {
			// uninitialized, just all zeroes
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, 0);
			tb_global_set_initializer(mod, global, init);
		}
	}
}
