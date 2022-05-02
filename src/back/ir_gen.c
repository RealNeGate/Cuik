#include "ir_gen.h"
#include "settings.h"
#include <compilation_unit.h>
#include <timer.h>
#include <targets/targets.h>

TB_Module* mod;
atomic_size_t function_count;
atomic_flag irgen_defined_tls_index;

// Maps param_num -> TB_Register
static thread_local TB_Register* parameter_map;

static thread_local Type* function_type;
static thread_local const char* function_name;

// For aggregate returns
static thread_local TB_Register return_value_address;

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
			if (src->is_unsigned) reg = tb_inst_zxt(func, reg, ctype_to_tbtype(dst));
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

static TB_Register cvt2rval(TranslationUnit* tu, TB_Function* func, const IRVal v, const Expr* e) {
	const Type* src = e->type;
	
	TB_Register reg = 0;
	switch (v.value_type) {
		case RVALUE: {
			reg = v.reg; 
			break;
		}
		case RVALUE_PHI: {
			TB_Label merger = tb_inst_new_label_id(func);
			
			tb_inst_label(func, v.phi.if_true);
			TB_Reg one = tb_inst_bool(func, true);
			tb_inst_goto(func, merger);
			
			tb_inst_label(func, v.phi.if_false);
			TB_Reg zero = tb_inst_bool(func, false);
			
			tb_inst_label(func, merger);
			reg = tb_inst_phi2(func, v.phi.if_true, one, v.phi.if_false, zero);
			break;
		}
		case LVALUE: {
			// Implicit array to pointer
			if (src->kind == KIND_ARRAY) {
				// just pass the address don't load
				src = e->cast_type;
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
	
	const Type* dst = e->cast_type;
	return src != dst ? cast_reg(func, reg, src, dst) : reg;
}

TB_Register irgen_as_rvalue(TranslationUnit* tu, TB_Function* func, Expr* e) {
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
InitNode* eval_initializer_objects(TranslationUnit* tu, TB_Function* func, SourceLocIndex loc, TB_InitializerID init, TB_Register addr, Type* type, int node_count, InitNode* node, int offset) {
	// Identify boundaries:
	//   Scalar types are 1
	//   Records depend on the member count
	//   Arrays are based on array count
	int bounds;
	{
		switch (type->kind) {
			case KIND_ARRAY:
			bounds = type->array_count; 
			break;
			
			case KIND_UNION: case KIND_STRUCT:
			bounds = type->record.kid_count;
			break;
			
			default: 
			bounds = 1;
			break;
		}
	}
	
	// Starts at the first node and just keep traversing through any nodes with children.
	int cursor = 0;
	for (int i = 0; i < node_count; i++) {
		// if there's no designator then the cursor
		// just keeps incrementing
		int pos, pos_end;
		if (node->mode == INIT_MEMBER) {
			if (type->kind != KIND_STRUCT && type->kind != KIND_UNION) {
				internal_error("Cannot get the member of a non-record type.");
			}
			
			pos = pos_end = -1;
			
			Member* kids = type->record.kids;
			size_t count = type->record.kid_count;
			
			for (size_t i = 0; i < count; i++) {
				Member* member = &kids[i];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(node->member_name, member->name)) {
					pos = i;
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
		Type* child_type;
		int relative_offset;
		if (type->kind == KIND_ARRAY) {
			child_type = type->array_of;
			relative_offset = type->array_of->size * pos;
		} else if (type->kind == KIND_UNION || type->kind == KIND_STRUCT) {
			child_type = type->record.kids[pos].type;
			relative_offset = type->record.kids[pos].offset;
		} else {
			child_type = type;
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
			if (!func && node->expr->op == EXPR_SYMBOL) {
				Stmt* stmt = node->expr->symbol;
				
				// hacky just to make it possible for some symbols to appear in the 
				// initializers
				// TODO(NeGate): Fix it up so that more operations can be
				// performed at compile time and baked into the initializer
				if (stmt->op == STMT_GLOBAL_DECL) {
					tb_initializer_add_global(mod, init, offset + relative_offset, stmt->backing.g);
					success = true;
				} else if (stmt->op == STMT_FUNC_DECL) {
					tb_initializer_add_function(mod, init, offset + relative_offset, stmt->backing.f);
					success = true;
				}
			}
			
			if (!success) {
				switch (node->expr->op) {
					// TODO(NeGate): Implement constants for literals
					// to allow for more stuff to be precomputed.
					case EXPR_INT:
					case EXPR_ENUM:
					case EXPR_NEGATE:
					if (!func) {
						int size = child_type->size;
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
						
						TypeKind kind = child_type->kind;
						int size = child_type->size;
						int align = child_type->align;
						
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
							node->expr->cast_type = child_type;
							
							TB_Register v = irgen_as_rvalue(tu, func, node->expr);
							
							// placing the address calculation here might improve performance or readability
							// of IR in the debug builds, for release builds it shouldn't matter
							TB_Register effective_addr;
							if (offset + relative_offset) {
								effective_addr = tb_inst_member_access(func, addr, offset + relative_offset);
							} else {
								effective_addr = addr;
							}
							
							tb_inst_store(func, ctype_to_tbtype(child_type), effective_addr, v, align);
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

static void gen_local_initializer(TranslationUnit* tu, TB_Function* func, SourceLocIndex loc, TB_Register addr, Type* type, int node_count, InitNode* nodes) {
	// Walk initializer for max constant expression initializers.
	int max_tb_objects = 0;
	count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
	
	TB_InitializerID init = tb_initializer_create(mod, 
												  type->size,
												  type->align, 
												  max_tb_objects);
	
	// Initialize all const expressions
	eval_initializer_objects(tu, func, loc, init, TB_NULL_REG, type, node_count, nodes, 0);
	tb_inst_initialize_mem(func, addr, init);
	
	// Initialize all dynamic expressions
	eval_initializer_objects(tu, func, loc, init, addr, type, node_count, nodes, 0);
}

static TB_InitializerID gen_global_initializer(TranslationUnit* tu, SourceLocIndex loc, Type* type, Expr* initial, const unsigned char* name) {
	assert(type != NULL);
	
	if (initial != NULL) {
		if (initial->op == EXPR_STR || initial->op == EXPR_WSTR) {
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, 1);
			char* dst = tb_initializer_add_region(mod, init, 0, type->size);
			memcpy(dst, initial->str.start, initial->str.end - initial->str.start);
			
			if (type->kind == KIND_PTR) {
				// if it's a string pointer, then we make a dummy string array
				// and point to that with another initializer
				char temp[1024];
				snprintf(temp, 1024, "%s@%p", name, initial);
				
				TB_GlobalID dummy = tb_global_create(mod, temp, TB_STORAGE_DATA, TB_LINKAGE_PRIVATE);
				tb_global_set_initializer(mod, dummy, init);
				
				init = tb_initializer_create(mod, 8, 8, 1);
				tb_initializer_add_global(mod, init, 0, dummy);
			}
			
			return init;
		} else if (initial->op == EXPR_INITIALIZER) {
			if (initial->init.type->kind == KIND_VOID) {
				initial->init.type = type;
			}
			
			int node_count = initial->init.count;
			InitNode* nodes = initial->init.nodes;
			
			// Walk initializer for max constant expression initializers.
			int max_tb_objects = 0;
			count_max_tb_init_objects(node_count, nodes, &max_tb_objects);
			
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, max_tb_objects);
			
			// Initialize all const expressions
			eval_initializer_objects(tu, NULL, loc, init, TB_NULL_REG, type, node_count, nodes, 0);
			return init;
		} else if (initial->op == EXPR_INT ||
				   initial->op == EXPR_ENUM ||
				   initial->op == EXPR_CHAR ||
				   initial->op == EXPR_CAST ||
				   initial->op == EXPR_NEGATE) {
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, 1);
			void* region = tb_initializer_add_region(mod, init, 0, type->size);
			
			// NOTE(NeGate): Endian amirite
			ConstValue value = const_eval(tu, initial);
			memcpy(region, &value.unsigned_value, type->size);
			
			return init;
		} else if (initial->op == EXPR_ADDR) {
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, 2);
			void* region = tb_initializer_add_region(mod, init, 0, type->size);
			
			uint64_t offset = 0;
			
			// &some_global[a][b][c]
			Expr* base = initial->unary_op.src;
			while (base->op == EXPR_SUBSCRIPT) {
				uint64_t stride = base->type->size;
				uint64_t index = const_eval(tu, base->subscript.index).unsigned_value;
				
				offset += (index * stride);
				base = base->subscript.base;
			}
			
			// TODO(NeGate): Assumes we're on a 64bit target...
			memcpy(region, &offset, sizeof(uint64_t));
			
			if (base->op == EXPR_SYMBOL) {
				Stmt* stmt = base->symbol;
				
				if (stmt->op == STMT_GLOBAL_DECL) {
					tb_initializer_add_global(mod, init, 0, stmt->backing.g);
				} else {
					internal_error("could not resolve as constant initializer");
				}
			} else {
				internal_error("could not resolve as constant initializer");
			}
			return init;
		} else {
			assert(0);
		}
	}
	
	// uninitialized, just all zeroes
	return tb_initializer_create(mod, type->size, type->align, 0);
}

static void insert_label(TB_Function* func) {
	if (tb_inst_get_current_label(func) == 0) {
		tb_inst_label(func, tb_inst_new_label_id(func));
	}
}

IRVal irgen_expr(TranslationUnit* tu, TB_Function* func, Expr* e) {
	switch (e->op) {
		case EXPR_CHAR: {
			// TODO(NeGate): Maybe multichar literals
			const unsigned char* start = e->str.start;
			start += (*start == 'L'); // skip the L
			
			size_t len = ((const unsigned char*)e->str.end) - start; 
			assert(start[0] == '\'');
			
			int ch;
			intptr_t distance = parse_char(len - 2, (const char*) &start[1], &ch);
			if (distance < 0) {
				internal_error("Could not recognize char literal.");
			}
			assert(start[1+distance] == '\'');
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_uint(func, e->type->kind == KIND_SHORT ? TB_TYPE_I16 : TB_TYPE_I8, ch)
			};
		}
		case EXPR_INT: {
			TB_DataType dt = ctype_to_tbtype(e->type);
			
			if (e->type->is_unsigned) {
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = tb_inst_uint(func, dt, e->int_num.num)
				};
			} else {
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = tb_inst_sint(func, dt, e->int_num.num)
				};
			}
		}
		case EXPR_ENUM: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_sint(func, TB_TYPE_I32, e->enum_val.num)
			};
		}
		case EXPR_FLOAT32: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->cast_type,
				.reg = tb_inst_float(func, e->cast_type->kind == KIND_DOUBLE ? TB_TYPE_F64 : TB_TYPE_F32, e->float_num)
			};
		}
		case EXPR_FLOAT64: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->cast_type,
				.reg = tb_inst_float(func, e->cast_type->kind == KIND_FLOAT ? TB_TYPE_F32 : TB_TYPE_F64, e->float_num)
			};
		}
		case EXPR_STR:
		case EXPR_WSTR: {
			// The string is preprocessed to be a flat and nice byte buffer by the semantics pass
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_string(func, e->str.end - e->str.start, (char*)e->str.start)
			};
		}
		case EXPR_INITIALIZER: {
			Type* type = e->init.type;
			TB_Register addr = tb_inst_local(func, type->size, type->align);
			
			gen_local_initializer(tu, func, e->loc, addr, type, e->init.count, e->init.nodes);
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type,
				.reg = addr
			};
		}
		case EXPR_FUNCTION: {
			assert(e->func.src->op == STMT_FUNC_DECL);
			
			return (IRVal) {
				.value_type = LVALUE_FUNC,
				.type = e->type,
				.func = tb_function_from_id(mod, e->func.src->backing.f)
			};
		}
		case EXPR_SYMBOL: {
			Stmt* stmt = e->symbol;
			assert(stmt->op == STMT_DECL  ||
				   stmt->op == STMT_LABEL ||
				   stmt->op == STMT_GLOBAL_DECL ||
				   stmt->op == STMT_FUNC_DECL);
			
			Type* type = stmt->decl.type;
			
			if (stmt->op == STMT_GLOBAL_DECL) {
				return (IRVal) {
					.value_type = LVALUE,
					.type = type,
					.reg = tb_inst_get_global_address(func, stmt->backing.g)
				};
			} else if (stmt->op == STMT_LABEL) {
				if (stmt->backing.l == 0) {
					stmt->backing.l = tb_inst_new_label_id(func);
				}
				
				return (IRVal) {
					.value_type = LVALUE_LABEL,
					.type = NULL,
					.label = stmt->backing.l
				};
			} else if (type->kind == KIND_FUNC) {
				if (stmt->op == STMT_FUNC_DECL) {
					return (IRVal) {
						.value_type = LVALUE_FUNC,
						.type = type,
						.func = tb_function_from_id(mod, stmt->backing.f)
					};
				} else if (stmt->op == STMT_DECL) {
					if (stmt->backing.e == 0) {
						// It's either a proper external or links to
						// a file within the compilation unit, we don't
						// know yet
						CompilationUnit* restrict cu = tu->parent;
						mtx_lock(&cu->mutex);
						
						const char* name = (const char*) stmt->decl.name;
						
						ptrdiff_t temp;
						ptrdiff_t search = shgeti_ts(cu->export_table, name, temp);
						
						IRVal val = { 0 };
						if (search >= 0) {
							// Figure out what the symbol is and link it together
							Stmt* real_symbol = cu->export_table[search].value;
							
							if (real_symbol->op == STMT_FUNC_DECL) {
								val = (IRVal) {
									.value_type = LVALUE_FUNC,
									.type = type,
									.func = tb_function_from_id(mod, real_symbol->backing.f)
								};
							} else if (real_symbol->op == STMT_GLOBAL_DECL) {
								val = (IRVal) {
									.value_type = LVALUE,
									.type = type,
									.reg = tb_inst_get_global_address(func, real_symbol->backing.g)
								};
							} else {
								abort();
							}
						} else {
							stmt->backing.e = tb_extern_create(mod, name);
							
							val = (IRVal) {
								.value_type = LVALUE_EFUNC,
								.type = type,
								.ext = stmt->backing.e
							};
						}
						
						// NOTE(NeGate): we might wanna move this mutex unlock earlier
						// it doesn't seem like we might need it honestly...
						mtx_unlock(&cu->mutex);
						return val;
					}
					
					// Proper external
					return (IRVal) {
						.value_type = LVALUE_EFUNC,
						.type = type,
						.ext = stmt->backing.e
					};
				}
			}
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = type,
				.reg = stmt->backing.r
			};
		}
		case EXPR_PARAM: {
			int param_num = e->param_num;
			TB_Register reg = parameter_map[param_num];
			
			Type* arg_type = function_type->func.param_list[param_num].type;
			assert(arg_type != NULL);
			
			if (arg_type->kind == KIND_STRUCT ||
				arg_type->kind == KIND_UNION) {
				// TODO(NeGate): Assumes pointer size
				reg = tb_inst_load(func, TB_TYPE_PTR, reg, 8);
			}
			
			return (IRVal) {
				.value_type = LVALUE,
				.type = arg_type,
				.reg = reg
			};
		}
		case EXPR_GENERIC: {
			assert(e->generic_.case_count == 0);
			return irgen_expr(tu, func, e->generic_.controlling_expr);
		}
		case EXPR_ADDR: {
			uint64_t dst;
			if (const_eval_try_offsetof_hack(tu, e->unary_op.src, &dst)) {
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = tb_inst_uint(func, TB_TYPE_PTR, dst)
				};
			}
			
			IRVal src = irgen_expr(tu, func, e->unary_op.src);
			
			if (src.value_type == LVALUE) {
				src.type = e->type;
				src.value_type = RVALUE;
				return src;
			} else if (src.value_type == LVALUE_EFUNC) {
				src.type = e->type;
				src.value_type = RVALUE;
				src.reg = tb_inst_get_extern_address(func, src.ext);
				return src;
			} else if (src.value_type == LVALUE_FUNC) {
				src.type = e->type;
				src.value_type = RVALUE;
				src.reg = tb_inst_get_func_address(func, src.func);
				return src;
			} else {
				abort();
			}
		}
		case EXPR_LOGICAL_NOT: {
			TB_Register reg = irgen_as_rvalue(tu, func, e->unary_op.src);
			TB_DataType dt = tb_function_get_node(func, reg)->dt;
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_cmp_eq(func, reg, tb_inst_uint(func, dt, 0))
			};
		}
		case EXPR_NOT: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_not(func, irgen_as_rvalue(tu, func, e->unary_op.src))
			};
		}
		case EXPR_NEGATE: {
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_neg(func, irgen_as_rvalue(tu, func, e->unary_op.src))
			};
		}
		case EXPR_CAST: {
			TB_Register src = irgen_as_rvalue(tu, func, e->cast.src);
			
			// stuff like ((void) x)
			if (e->cast.type->kind == KIND_VOID) {
				return (IRVal) { .value_type = RVALUE, .type = &builtin_types[TYPE_VOID], .reg = 0 };
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->cast.type,
				.reg = src
			};
		}
		case EXPR_DEREF: {
			TB_Register reg = irgen_as_rvalue(tu, func, e->unary_op.src);
			return (IRVal) {
				.value_type = LVALUE,
				.type = e->type,
				.reg = reg
			};
		}
		case EXPR_CALL: {
			Expr** args = e->call.param_start;
			int arg_count = e->call.param_count;
			
			// Try to see if it's an intrinsic
			if (e->call.target->op == EXPR_SYMBOL) {
				Stmt* sym = e->call.target->symbol;
				
				if (sym->op == STMT_DECL) {
					const char* name = (const char*) sym->decl.name;
					
					// all builtins start with an underscore
					if (*name == '_') {
						ptrdiff_t temp;
						ptrdiff_t search = shgeti_ts(target_desc.builtin_func_map, name, temp);
						
						if (search >= 0) {
							TB_Register val = target_desc.compile_builtin(tu, func, name, arg_count, args);
							
							return (IRVal) {
								.value_type = RVALUE,
								.type = e->type,
								.reg = val
							};
						}
					}
				}
			} else if (e->call.target->op == EXPR_UNKNOWN_SYMBOL) {
				// We can only get to this point with UNKNOWN_SYMBOL if it's
				// a builtin
				const char* name = (const char*) e->call.target->unknown_sym;
				TB_Register val = target_desc.compile_builtin(tu, func, name, arg_count, args);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = val
				};
			}
			
			// Resolve ABI arg count
			bool is_aggregate_return = target_desc.pass_return(tu, e->type);
			size_t real_arg_count = is_aggregate_return ? 1 : 0;
			
			for (size_t i = 0; i < arg_count; i++) {
				Type* arg_type = args[i]->type;
				real_arg_count += target_desc.deduce_parameter_usage(tu, arg_type);
			}
			
			TB_Reg* ir_args = tls_push(real_arg_count * sizeof(TB_Reg));
			if (is_aggregate_return) {
				ir_args[0] = tb_inst_local(func, e->type->size, e->type->align);
			}
			
			// point at which it stops being know which parameter types we're
			// mapping to, if it's arg_count then there's really none
			size_t varargs_cutoff = arg_count;
			Type* func_type = e->call.target->type;
			if (func_type->func.has_varargs) {
				varargs_cutoff = func_type->func.param_count;
			}
			
			size_t ir_arg_count = is_aggregate_return ? 1 : 0;
			for (size_t i = 0; i < arg_count; i++) {
				ir_arg_count += target_desc.pass_parameter(tu, func, args[i],
														   i >= varargs_cutoff,
														   &ir_args[ir_arg_count]);
			}
			
			assert(ir_arg_count == real_arg_count);
			
			// Resolve call target
			//
			// NOTE(NeGate): Could have been resized in the parameter's irgen_expr
			// so we reload the pointer.
			IRVal func_ptr = irgen_expr(tu, func, e->call.target);
			
			TB_DataType dt = ctype_to_tbtype(e->type);
			if (is_aggregate_return) dt = TB_TYPE_VOID;
			
			TB_Reg r;
			if (func_ptr.value_type == LVALUE_FUNC) {
				r = tb_inst_call(func, dt, func_ptr.func, real_arg_count, ir_args);
			} else if (func_ptr.value_type == LVALUE_EFUNC) {
				r = tb_inst_ecall(func, dt, func_ptr.ext, real_arg_count, ir_args);
			} else {
				TB_Reg target_reg = cvt2rval(tu, func, func_ptr, e->call.target);
				
				r = tb_inst_vcall(func, dt, target_reg, real_arg_count, ir_args);
			}
			
			if (is_aggregate_return) {
				TB_Register result = ir_args[0];
				tls_restore(ir_args);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = result
				};
			} else {
				tls_restore(ir_args);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = r
				};
			}
		}
		case EXPR_SUBSCRIPT: {
			TB_Register base = irgen_as_rvalue(tu, func, e->subscript.base);
			TB_Register index = irgen_as_rvalue(tu, func, e->subscript.index);
			
			int stride = e->type->size;
			return (IRVal) {
				.value_type = LVALUE,
				.type = e->type,
				.reg = tb_inst_array_access(func, base, index, stride)
			};
		}
		case EXPR_DOT: {
			IRVal src = irgen_expr(tu, func, e->dot_arrow.base);
			assert(src.value_type == LVALUE);
			
			Member* member = e->dot_arrow.member;
			assert(member != NULL);
			
			if (member->is_bitfield) {
				return (IRVal) {
					.value_type = LVALUE_BITS,
					.type = member->type,
					.bits = {
						.reg = tb_inst_member_access(func, src.reg, e->dot_arrow.offset),
						.offset = member->bit_offset, .width = member->bit_width
					}
				};
			} else {
				return (IRVal) {
					.value_type = LVALUE,
					.type = member->type,
					.reg = tb_inst_member_access(func, src.reg, e->dot_arrow.offset)
				};
			} 
		}
		case EXPR_ARROW: {
			TB_Register src = irgen_as_rvalue(tu, func, e->dot_arrow.base);
			
			Member* member = e->dot_arrow.member;
			assert(member != NULL);
			
			if (member->is_bitfield) {
				return (IRVal) {
					.value_type = LVALUE_BITS,
					.type = member->type,
					.bits = {
						.reg = tb_inst_member_access(func, src, e->dot_arrow.offset),
						.offset = member->bit_offset, .width = member->bit_width
					}
				};
			} else {
				return (IRVal) {
					.value_type = LVALUE,
					.type = member->type,
					.reg = tb_inst_member_access(func, src, e->dot_arrow.offset)
				};
			}
		}
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC: {
			bool is_inc = (e->op == EXPR_PRE_INC);
			
			IRVal src = irgen_expr(tu, func, e->unary_op.src);
			assert(src.value_type == LVALUE);
			
			TB_Register loaded = cvt2rval(tu, func, src, e->unary_op.src);
			Type* type = e->type;
			if (type->kind == KIND_PTR) {
				// pointer arithmatic
				TB_Register stride = tb_inst_sint(func, TB_TYPE_PTR, type->ptr_to->size);
				TB_ArithmaticBehavior ab = TB_CAN_WRAP;
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, stride, ab);
				else operation = tb_inst_sub(func, loaded, stride, ab);
				
				tb_inst_store(func, TB_TYPE_PTR, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
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
					.type = e->type,
					.reg = operation
				};
			}
		}
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			bool is_inc = (e->op == EXPR_POST_INC);
			
			IRVal src = irgen_expr(tu, func, e->unary_op.src);
			assert(src.value_type == LVALUE);
			
			Type* type = e->type;
			if (type->is_atomic) {
				TB_Register stride;
				if (type->kind == KIND_PTR) {
					// pointer arithmatic
					stride = tb_inst_sint(func, TB_TYPE_PTR, type->ptr_to->size);
				} else {
					stride = tb_inst_uint(func, ctype_to_tbtype(type), 1);
				}
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_atomic_add(func, src.reg, stride, TB_MEM_ORDER_SEQ_CST);
				else operation = tb_inst_atomic_sub(func, src.reg, stride, TB_MEM_ORDER_SEQ_CST);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
					.reg = operation
				};
			} else {
				TB_Register loaded = cvt2rval(tu, func, src, e->unary_op.src);
				
				TB_DataType dt;
				TB_Register stride;
				TB_ArithmaticBehavior ab;
				if (type->kind == KIND_PTR) {
					// pointer arithmatic
					dt = TB_TYPE_PTR;
					stride = tb_inst_sint(func, TB_TYPE_PTR, type->ptr_to->size);
					ab = TB_CAN_WRAP;
				} else {
					dt = ctype_to_tbtype(type);
					stride = type->is_unsigned ? tb_inst_uint(func, dt, 1) : tb_inst_sint(func, dt, 1);
					ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				}
				
				TB_Register operation;
				if (is_inc) operation = tb_inst_add(func, loaded, stride, ab);
				else operation = tb_inst_sub(func, loaded, stride, ab);
				
				tb_inst_store(func, TB_TYPE_PTR, src.reg, operation, type->align);
				
				return (IRVal) {
					.value_type = RVALUE,
					.type = e->type,
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
			bool is_and = (e->op == EXPR_LOGICAL_AND);
			
			TB_Label try_rhs_lbl = tb_inst_new_label_id(func);
			
			// Eval first operand
			IRVal a = irgen_expr(tu, func, e->bin_op.left);
			
			TB_Label true_lbl, false_lbl;
			if (a.value_type == RVALUE_PHI) {
				// chain with previous phi.
				// OR  chains on false,
				// AND chains on true
				if (is_and) {
					tb_inst_label(func, a.phi.if_true);
					tb_inst_goto(func,  try_rhs_lbl);
					
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
				
				TB_Reg a_reg = cvt2rval(tu, func, a, e->bin_op.left);
				if (is_and) {
					tb_inst_if(func, a_reg, try_rhs_lbl, false_lbl);
				} else {
					tb_inst_if(func, a_reg, true_lbl, try_rhs_lbl);
				}
			}
			
			// Eval second operand
			tb_inst_label(func, try_rhs_lbl);
			
			TB_Reg b = irgen_as_rvalue(tu, func, e->bin_op.right);
			tb_inst_if(func, b, true_lbl, false_lbl);
			
			// Just in case
			//insert_label(func);
			
			return (IRVal) {
				.value_type = RVALUE_PHI,
				.type = &builtin_types[TYPE_BOOL],
				.phi = { true_lbl, false_lbl }
			};
		}
		case EXPR_COMMA: {
			irgen_expr(tu, func, e->bin_op.left);
			return irgen_expr(tu, func, e->bin_op.right);
		}
		case EXPR_PTRADD:
		case EXPR_PTRSUB: {
			TB_Register l = irgen_as_rvalue(tu, func, e->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, e->bin_op.right);
			
			Type* restrict type = e->type;
			
			// pointer arithmatic
			int dir = e->op == EXPR_PTRADD ? 1 : -1;
			int stride = type->ptr_to->size;
			
			assert(stride);
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_array_access(func, l, r, dir * stride)
			};
		}
		case EXPR_PTRDIFF: {
			TB_Register l = irgen_as_rvalue(tu, func, e->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, e->bin_op.right);
			
			Type* type = e->bin_op.left->cast_type;
			int stride = type->ptr_to->size;
			
			l = tb_inst_ptr2int(func, l, TB_TYPE_I64);
			r = tb_inst_ptr2int(func, r, TB_TYPE_I64);
			
			TB_Register diff = tb_inst_sub(func, l, r, TB_ASSUME_NSW);
			TB_Register diff_in_elems = tb_inst_div(func, diff, tb_inst_sint(func, tb_function_get_node(func, diff)->dt, stride), true);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
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
			TB_Register l = irgen_as_rvalue(tu, func, e->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, e->bin_op.right);
			Type* restrict type = e->type;
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (e->op) {
					case EXPR_PLUS: data = tb_inst_fadd(func, l, r); break;
					case EXPR_MINUS: data = tb_inst_fsub(func, l, r); break;
					case EXPR_TIMES: data = tb_inst_fmul(func, l, r); break;
					case EXPR_SLASH: data = tb_inst_fdiv(func, l, r); break;
					default: abort();
				}
			} else {
				TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
				
				switch (e->op) {
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
				
				if (type->kind == KIND_BOOL) {
					// convert into proper bool
					data = tb_inst_cmp_ne(func, data, tb_inst_uint(func, TB_TYPE_BOOL, 0));
				}
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = data
			};
		}
		case EXPR_CMPEQ:
		case EXPR_CMPNE: {
			TB_Register l = irgen_as_rvalue(tu, func, e->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, e->bin_op.right);
			
			TB_Register result;
			if (e->op == EXPR_CMPEQ) {
				result = tb_inst_cmp_eq(func, l, r);
			} else {
				result = tb_inst_cmp_ne(func, l, r);
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = &builtin_types[TYPE_BOOL],
				.reg = result
			};
		}
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			TB_Register l = irgen_as_rvalue(tu, func, e->bin_op.left);
			TB_Register r = irgen_as_rvalue(tu, func, e->bin_op.right);
			
			Type* type = e->bin_op.left->cast_type;
			
			TB_Register data;
			if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
				switch (e->op) {
					case EXPR_CMPGT: data = tb_inst_cmp_fgt(func, l, r); break;
					case EXPR_CMPGE: data = tb_inst_cmp_fge(func, l, r); break;
					case EXPR_CMPLT: data = tb_inst_cmp_flt(func, l, r); break;
					case EXPR_CMPLE: data = tb_inst_cmp_fle(func, l, r); break;
					default: abort();
				}
			} else {
				switch (e->op) {
					case EXPR_CMPGT: data = tb_inst_cmp_igt(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPGE: data = tb_inst_cmp_ige(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPLT: data = tb_inst_cmp_ilt(func, l, r, !type->is_unsigned); break;
					case EXPR_CMPLE: data = tb_inst_cmp_ile(func, l, r, !type->is_unsigned); break;
					default: abort();
				}
			}
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = &builtin_types[TYPE_BOOL],
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
			Type* type = e->type;
			
			if (type->is_atomic) {
				// Load inputs
				IRVal lhs = irgen_expr(tu, func, e->bin_op.left);
				IRVal rhs = irgen_expr(tu, func, e->bin_op.right);
				assert(lhs.value_type == LVALUE);
				
				if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
					// Implement big atomic copy
					abort();
				} else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
					TB_Register r = cvt2rval(tu, func, rhs, e->bin_op.right);
					
					// float assignment can be done atomic by using the normal
					// integer stuff
					if (e->op == EXPR_ASSIGN) {
						tb_inst_atomic_xchg(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
						
						return (IRVal) {
							.value_type = RVALUE,
							.type = e->type,
							.reg = r
						};
					} else {
						// floats don't really have any atomic operations so just
						// emulate them all
						/*TB_Reg l = cvt2rval(tu, func, lhs, e->bin_op.left);
						
						TB_Label loop = tb_inst_new_label_id(func);
						TB_Label success = tb_inst_new_label_id(func);
						tb_inst_label(func, loop);
						{
							TB_Reg desired = TB_NULL_REG;
							switch (e->op) {
								case EXPR_PLUS_ASSIGN: desired = tb_inst_fadd(func, l, r); break;
								case EXPR_MINUS_ASSIGN: desired = tb_inst_fsub(func, l, r); break;
								case EXPR_TIMES_ASSIGN: desired = tb_inst_fmul(func, l, r); break;
								case EXPR_SLASH_ASSIGN: desired = tb_inst_fdiv(func, l, r); break;
								default: assert(0);
							}
							
							if (type->kind == KIND_FLOAT) {
								desired = tb_inst_bitcast(func, desired, TB_TYPE_I32);
							} else if (type->kind == KIND_DOUBLE) {
								desired = tb_inst_bitcast(func, desired, TB_TYPE_I64);
							} else assert(0);
							
							TB_CmpXchgResult r = tb_inst_atomic_cmpxchg(func, lhs.reg, expected, desired, TB_MEM_ORDER_SEQ_CST, TB_MEM_ORDER_SEQ_CST);
							tb_inst_if(func, r.success, success, loop);
						}
						tb_inst_label(func, success);
						
						return (IRVal) {
							.value_type = RVALUE,
							.type = e->type,
							.reg = l
						};*/
						internal_error("TODO");
					}
				} else {
					TB_Register r = cvt2rval(tu, func, rhs, e->bin_op.right);
					
					if (e->op == EXPR_ASSIGN) {
						tb_inst_atomic_xchg(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
						
						return (IRVal) {
							.value_type = RVALUE,
							.type = e->type,
							.reg = r
						};
					} else if (e->op == EXPR_PLUS_ASSIGN) {
						TB_Register op = tb_inst_atomic_add(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
						op = tb_inst_add(func, op, r, TB_CAN_WRAP);
						
						return (IRVal) {
							.value_type = RVALUE,
							.type = e->type,
							.reg = op
						};
					} else if (e->op == EXPR_MINUS_ASSIGN) {
						TB_Register op = tb_inst_atomic_sub(func, lhs.reg, r, TB_MEM_ORDER_SEQ_CST);
						op = tb_inst_sub(func, op, r, TB_CAN_WRAP);
						
						return (IRVal) {
							.value_type = RVALUE,
							.type = e->type,
							.reg = op
						};
					} else {
						internal_error("TODO: atomic operation not ready");
					}
				}
			} else {
				// Load inputs
				IRVal lhs = irgen_expr(tu, func, e->bin_op.left);
				
				TB_Register l = TB_NULL_REG;
				if (e->op != EXPR_ASSIGN) {
					// don't do this conversion for ASSIGN, since it won't
					// be needing it
					l = cvt2rval(tu, func, lhs, e->bin_op.left);
				}
				
				IRVal rhs = irgen_expr(tu, func, e->bin_op.right);
				
				// Try pointer arithmatic
				if ((e->op == EXPR_PLUS_ASSIGN || e->op == EXPR_MINUS_ASSIGN) && type->kind == KIND_PTR) {
					int dir = e->op == EXPR_PLUS_ASSIGN ? 1 : -1;
					int stride = type->ptr_to->size;
					assert(stride);
					
					TB_Register r = cvt2rval(tu, func, rhs, e->bin_op.right);
					TB_Register arith = tb_inst_array_access(func, l, r, dir * stride);
					
					assert(lhs.value_type == LVALUE);
					tb_inst_store(func, TB_TYPE_PTR, lhs.reg, arith, type->align);
					return lhs;
				}
				
				TB_DataType dt = ctype_to_tbtype(type);
				
				TB_Register data;
				if (type->kind == KIND_STRUCT || type->kind == KIND_UNION) {
					if (e->op != EXPR_ASSIGN) abort();
					
					TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, type->size);
					tb_inst_memcpy(func, lhs.reg, rhs.reg, size_reg, type->align);
				} else if (type->kind == KIND_FLOAT || type->kind == KIND_DOUBLE) {
					TB_Register r = cvt2rval(tu, func, rhs, e->bin_op.right);
					
					switch (e->op) {
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
					TB_Register r = cvt2rval(tu, func, rhs, e->bin_op.right);
					TB_ArithmaticBehavior ab = type->is_unsigned ? TB_CAN_WRAP : TB_ASSUME_NSW;
					
					switch (e->op) {
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
		}
		case EXPR_TERNARY: {
			TB_Register cond = irgen_as_rvalue(tu, func, e->ternary_op.left);
			
			TB_Label if_true = tb_inst_new_label_id(func);
			TB_Label if_false = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			
			tb_inst_if(func, cond, if_true, if_false);
			
			TB_Register true_val;
			{
				tb_inst_label(func, if_true);
				
				true_val = irgen_as_rvalue(tu, func, e->ternary_op.middle);
				if_true = tb_inst_get_current_label(func);
				tb_inst_goto(func, exit);
			}
			
			TB_Register false_val;
			{
				tb_inst_label(func, if_false);
				
				false_val = irgen_as_rvalue(tu, func, e->ternary_op.right);
				if_false = tb_inst_get_current_label(func);
				// fallthrough to exit
			}
			tb_inst_label(func, exit);
			
			return (IRVal) {
				.value_type = RVALUE,
				.type = e->type,
				.reg = tb_inst_phi2(func, if_true, true_val, if_false, false_val)
			};
		}
		default: abort();
	}
}

void irgen_stmt(TranslationUnit* tu, TB_Function* func, Stmt* restrict s) {
	if (settings.is_debug_info) {
		// TODO(NeGate): Fix this up later!!!
		static thread_local TB_FileID last_file_id = 0;
		static thread_local const char* last_filepath = NULL;
		
		SourceLoc* l = &tu->tokens.line_arena[s->loc];
		if ((const char*)l->file != last_filepath) {
			last_filepath = (const char*)l->file;
			last_file_id = tb_file_create(mod, (const char*)l->file);
		}
		
		tb_inst_loc(func, last_file_id, l->line);
	}
	
	insert_label(func);
	
	switch (s->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_LABEL: {
			if (s->backing.l == 0) {
				s->backing.l = tb_inst_new_label_id(func);
			}
			
			tb_inst_label(func, s->backing.l);
			break;
		}
		case STMT_GOTO: {
			IRVal target = irgen_expr(tu, func, s->goto_.target);
			
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
			Stmt** kids = s->compound.kids;
			size_t count = s->compound.kids_count;
			
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
			Attribs attrs = s->decl.attrs;
			
			Type* type = s->decl.type;
			int kind  = type->kind;
			int size  = type->size;
			int align = type->align;
			
			if (attrs.is_static) {
				// Static initialization
				TB_InitializerID init = gen_global_initializer(tu, s->loc,
															   type,
															   s->decl.initial,
															   s->decl.name);
				
				char* name = tls_push(1024);
				int name_len = snprintf(name, 1024, "%s$%s@%p", function_name, s->decl.name, s);
				if (name_len < 0 || name_len >= 1024) {
					internal_error("temporary global name too long!");
				}
				
				if (attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
					tb_module_set_tls_index(mod, tb_extern_create(mod, "_tls_index"));
				}
				
				TB_GlobalID g = tb_global_create(mod, name, attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, TB_LINKAGE_PRIVATE);
				tb_global_set_initializer(mod, g, init);
				tls_restore(name);
				
				s->backing.r = tb_inst_get_global_address(func, g);
				break;
			}
			
			TB_Reg addr = tb_inst_local(func, size, align);
			if (s->decl.initial) {
				Expr* e = s->decl.initial;
				
				if (e->op == EXPR_INITIALIZER) {
					gen_local_initializer(tu, func, e->loc, addr, type, e->init.count, e->init.nodes);
				} else {
					if (kind == KIND_ARRAY && (e->op == EXPR_STR || e->op == EXPR_WSTR)) {
						IRVal v = irgen_expr(tu, func, s->decl.initial);
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						
						tb_inst_memcpy(func, addr, v.reg, size_reg, align);
					} else if (kind == KIND_STRUCT || kind == KIND_UNION) {
						IRVal v = irgen_expr(tu, func, s->decl.initial);
						TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
						
						tb_inst_memcpy(func, addr, v.reg, size_reg, align);
					} else {
						TB_Register v = irgen_as_rvalue(tu, func, s->decl.initial);
						
						tb_inst_store(func, ctype_to_tbtype(type), addr, v, align);
					}
				}
			} else {
				/* uninitialized */
			}
			
			s->backing.r = addr;
			break;
		}
		case STMT_EXPR: {
			irgen_expr(tu, func, s->expr.expr);
			break;
		}
		case STMT_RETURN: {
			Expr* e = s->return_.expr;
			
			if (e) {
				Type* type = e->cast_type;
				
				if (type->kind == KIND_STRUCT ||
					type->kind == KIND_UNION) {
					IRVal v = irgen_expr(tu, func, e);
					
					// returning aggregates just copies into the first parameter
					// which is agreed to be a caller owned buffer.
					int size = type->size;
					int align = type->align;
					
					TB_Register dst_address = tb_inst_load(func, TB_TYPE_PTR, return_value_address, 8);
					TB_Register size_reg = tb_inst_uint(func, TB_TYPE_I64, size);
					
					tb_inst_memcpy(func, dst_address, v.reg, size_reg, align);
					tb_inst_ret(func, TB_NULL_REG);
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
			
			TB_Register cond = irgen_as_rvalue(tu, func, s->if_.cond);
			
			TB_Label if_true = tb_inst_new_label_id(func);
			TB_Label if_false = tb_inst_new_label_id(func);
			
			// Cast to bool
			tb_inst_if(func, cond, if_true, if_false);
			
			tb_inst_label(func, if_true);
			irgen_stmt(tu, func, s->if_.body);
			
			if (s->if_.next) {
				TB_Label exit = tb_inst_new_label_id(func);
				tb_inst_goto(func, exit);
				
				tb_inst_label(func, if_false);
				irgen_stmt(tu, func, s->if_.next);
				
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
			s->backing.l = exit;
			
			// NOTE(NeGate): this is hacky but as long as it doesn't
			// break we should be good... what am i saying im the 
			// developer of TB :p
			// essentially we can store both the header and exit labels
			// implicitly as one if they're next to each other
			assert(header == exit - 1);
			
			tb_inst_label(func, header);
			
			TB_Register cond = irgen_as_rvalue(tu, func, s->while_.cond);
			tb_inst_if(func, cond, body, exit);
			
			tb_inst_label(func, body);
			if (s->while_.body) {
				irgen_stmt(tu, func, s->while_.body);
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
			s->backing.l = exit;
			
			tb_inst_label(func, body);
			
			if (s->do_while.body) {
				irgen_stmt(tu, func, s->do_while.body);
			}
			
			insert_label(func);
			
			TB_Register cond = irgen_as_rvalue(tu, func, s->do_while.cond);
			tb_inst_if(func, cond, body, exit);
			
			tb_inst_label(func, exit);
			break;
		}
		case STMT_FOR: {
			TB_Label body = tb_inst_new_label_id(func);
			TB_Label header = tb_inst_new_label_id(func);
			TB_Label exit = tb_inst_new_label_id(func);
			s->backing.l = exit;
			
			// NOTE(NeGate): this is hacky but as long as it doesn't
			// break we should be good... what am i saying im the 
			// developer of TB :p
			// essentially we can store both the header and exit labels
			// implicitly as one if they're next to each other
			assert(header == exit - 1);
			
			if (s->for_.first) {
				irgen_stmt(tu, func, s->for_.first);
			}
			
			tb_inst_label(func, header);
			
			if (s->for_.cond) {
				TB_Register cond = irgen_as_rvalue(tu, func, s->for_.cond);
				tb_inst_if(func, cond, body, exit);
			} else {
				tb_inst_goto(func, body);
			}
			
			tb_inst_label(func, body);
			
			irgen_stmt(tu, func, s->for_.body);
			
			if (s->for_.next) {
				insert_label(func);
				irgen_expr(tu, func, s->for_.next);
			}
			
			tb_inst_goto(func, header);
			tb_inst_label(func, exit);
			break;
		}
		case STMT_CONTINUE: {
			// this is really hacky but we always store the loop header label one
			// behind the exit label in terms of IDs.
			tb_inst_goto(func, s->continue_.target->backing.l - 1);
			break;
		}
		case STMT_BREAK: {
			tb_inst_goto(func, s->break_.target->backing.l);
			break;
		}
		case STMT_CASE:
		case STMT_DEFAULT: {
			assert(s->backing.l);
			tb_inst_label(func, s->backing.l);
			break;
		}
		case STMT_SWITCH: {
			Stmt* head = s->switch_.next;
			
			size_t entry_count = 0;
			TB_SwitchEntry* entries = tls_save();
			
			TB_Label default_label = 0;
			while (head) {
				// reserve label
				assert(head->op == STMT_CASE || head->op == STMT_DEFAULT);
				
				TB_Label label = tb_inst_new_label_id(func);
				head->backing.l = label;
				
				if (head->op == STMT_CASE) {
					assert(head->case_.key < UINT32_MAX);
					tls_push(sizeof(TB_SwitchEntry));
					entries[entry_count++] = (TB_SwitchEntry) { .key = head->case_.key, .value = label };
					
					head = head->case_.next;
				} else if (head->op == STMT_DEFAULT) {
					assert(default_label == 0);
					default_label = label;
					
					head = head->default_.next;
				} else assert(0);
			}
			
			TB_Label break_label = tb_inst_new_label_id(func);
			s->backing.l = break_label;
			
			// default to fallthrough
			if (!default_label) {
				default_label = break_label;
			}
			
			TB_Register key = irgen_as_rvalue(tu, func, s->switch_.condition);
			TB_DataType dt = tb_function_get_node(func, key)->dt;
			
			tb_inst_switch(func, dt, key, default_label, entry_count, entries);
			tb_inst_label(func, tb_inst_new_label_id(func));
			
			irgen_stmt(tu, func, s->switch_.body);
			
			tb_inst_label(func, break_label);
			break;
		}
		default:
		__builtin_unreachable();
	}
}

static void gen_func_body(TranslationUnit* tu, Type* type, Stmt* restrict s) {
	// Clear TLS
	tls_init();
	assert(type);
	
	TB_Function* func = tb_function_from_id(mod, s->backing.f);
	
	// Parameters
	size_t param_count = type->func.param_count;
	
	TB_Register* params = parameter_map = tls_push(param_count * sizeof(TB_Register));
	Type* return_type = type->func.return_type;
	
	TB_AttributeID old_tb_scope = tb_inst_get_scope(func);
	tb_inst_set_scope(func, tb_function_attrib_scope(func, old_tb_scope));
	
	// mark return value address (if it applies)
	// and get stack slots for parameters
	if (return_type->kind == KIND_STRUCT ||
		return_type->kind == KIND_UNION) {
		return_value_address = tb_inst_param_addr(func, 0);
		
		// gimme stack slots
		for (size_t i = 0; i < param_count; i++) {
			params[i] = tb_inst_param_addr(func, 1+i);
		}
	} else {
		return_value_address = TB_NULL_REG;
		
		// gimme stack slots
		for (size_t i = 0; i < param_count; i++) {
			params[i] = tb_inst_param_addr(func, i);
		}
	}
	
	// TODO(NeGate): Ok fix this up later but essentially we need to prepass
	// over the nodes to find the label statements, then forward declare the labels
	// in TB.
	
	// compile body
	{
		function_type = type;
		function_name = (const char*) s->decl.name;
		
		irgen_stmt(tu, func, s->decl.initial_as_stmt);
		
		function_name = NULL;
		function_type = 0;
	}
	
	{
		Type* return_type = type->func.return_type;
		TB_Register last = tb_node_get_last_register(func);
		
		if (tb_node_is_label(func, last) || !tb_node_is_terminator(func, last)) {
			if (return_type->kind != KIND_VOID   &&
				return_type->kind != KIND_STRUCT &&
				return_type->kind != KIND_UNION) {
				// Needs return value
				//irgen_warn(s->loc, "Expected return with value.");
			}
			
			tb_inst_ret(func, TB_NULL_REG);
		}
	}
	
	tb_inst_set_scope(func, old_tb_scope);
	
	if (settings.stage_to_stop_at == STAGE_IR) {
		if (mtx_lock(&emit_ir_mutex) != thrd_success) {
			printf("internal compiler error: mtx_lock(...) failure!");
			abort();
		}
		
		tb_function_print(func, tb_default_print_callback, stdout);
		fprintf(stdout, "\n\n");
		fflush(stdout);
		
		if (mtx_unlock(&emit_ir_mutex) != thrd_success) {
			printf("internal compiler error: mtx_unlock(...) failure!");
			abort();
		}
	} else {
		if (!settings.optimize) tb_module_compile_func(mod, func, TB_ISEL_FAST);
	}
	
	
	// if we're optimizing, we need to keep the IR longer so we can compile later
	if (!settings.optimize) tb_function_free(func);
	else {
		// we don't need the function_count if we aren't optimizing
		function_count += 1;
	}
}

void irgen_init() {
	mtx_init(&emit_ir_mutex, mtx_plain);
	
	TB_FeatureSet features = { 0 };
	mod = tb_module_create(target_arch, target_system, &features);
}

void irgen_deinit() {
}

void irgen_top_level_stmt(TranslationUnit* tu, Stmt* restrict s) {
	if (s->op == STMT_FUNC_DECL) {
		Type* type = s->decl.type;
		assert(type->kind == KIND_FUNC);
		
		if (s->decl.attrs.is_static ||
			s->decl.attrs.is_inline) {
			if (!s->decl.attrs.is_used) return;
		}
		
		gen_func_body(tu, type, s);
	} else if (s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL) {
		if (!s->decl.attrs.is_used) return;
		if (s->decl.attrs.is_typedef) return;
		if (s->decl.attrs.is_extern || s->decl.type->kind == KIND_FUNC) return;
		
		TB_GlobalID global = s->backing.g;
		TB_InitializerID init = gen_global_initializer(tu, s->loc,
													   s->decl.type,
													   s->decl.initial,
													   s->decl.name);
		
		tb_global_set_initializer(mod, global, init);
	}
}
