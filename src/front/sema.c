#include "sema.h"
#include "settings.h"

#include <targets/targets.h>
#include <back/ir_gen.h>
#include <stdarg.h>

// when you're not in the semantic phase, we don't
// rewrite the contents of the DOT and ARROW exprs
// because it may screw with things
thread_local bool in_the_semantic_phase;

static thread_local StmtIndex function_stmt;

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

#define sema_info(loc, ...) report(REPORT_INFO, &tu->tokens.line_arena[loc], __VA_ARGS__)
#define sema_warn(loc, ...) report(REPORT_WARNING, &tu->tokens.line_arena[loc], __VA_ARGS__)
#define sema_error(loc, ...) report(REPORT_ERROR, &tu->tokens.line_arena[loc], __VA_ARGS__)

static bool is_scalar_type(TranslationUnit* tu, TypeIndex type_index) {
	Type* restrict type = &tu->types[type_index];
	return (type->kind >= KIND_BOOL && type->kind <= KIND_FUNC);
}

// Also checks if expression is an integer literal because we
// have a special case for 0 to pointer conversions.
static bool type_compatible(TranslationUnit* tu, TypeIndex a, TypeIndex b, ExprIndex a_expr) {
	if (a == b) return true;
	
	Type* restrict src = &tu->types[a];
	Type* restrict dst = &tu->types[b];
	
	// implictly convert arrays into pointers
	if (src->kind == KIND_ARRAY) {
		TypeIndex as_ptr = new_pointer(tu, src->array_of);
		src = &tu->types[as_ptr];
	}
	
	if (dst->kind == KIND_ARRAY) {
		TypeIndex as_ptr = new_pointer(tu, dst->array_of);
		dst = &tu->types[as_ptr];
	}
	
	if (src->kind != dst->kind) {
		if (src->kind >= KIND_BOOL &&
			src->kind <= KIND_LONG &&
			dst->kind >= KIND_BOOL && 
			dst->kind <= KIND_LONG) {
#if 0
			// we allow for implicit up-casts (char -> long)
			if (dst->kind >= src->kind) return true;
			else if (dst->kind == KIND_BOOL) return true;
			else if (tu->exprs[a_expr].op == EXPR_INT) {
				// allow integer literals to represent any integer
				return true;
			}
#else
			// just all integer casts are good
			return true;
#endif
		} else if (src->kind >= KIND_BOOL &&
				   src->kind <= KIND_LONG &&
				   dst->kind == KIND_PTR) {
			if (tu->exprs[a_expr].op == EXPR_INT &&
				tu->exprs[a_expr].int_num.num == 0) {
				return true;
			}
		} else if (src->kind == KIND_FLOAT ||
				   dst->kind == KIND_DOUBLE) {
			return true;
		} else if (src->kind == KIND_DOUBLE ||
				   dst->kind == KIND_FLOAT) {
			return true;
		} else if (src->kind == KIND_PTR &&
				   dst->kind == KIND_BOOL) {
			return true;
		} else if (src->kind == KIND_FUNC &&
				   dst->kind == KIND_BOOL) {
			return true;
		} else if (src->kind == KIND_FUNC &&
				   dst->kind == KIND_PTR) {
			if (tu->types[dst->ptr_to].kind == KIND_FUNC) {
				return type_equal(tu, a, dst->ptr_to);
			}
		}
		
		return false;
	}
	
	if (src->kind == KIND_FUNC) {
		return type_equal(tu, a, b);
	} else if (dst->kind == KIND_PTR) {
		// void* -> T* is fine
		if (tu->types[src->ptr_to].kind == KIND_VOID) {
			return true;
		}
		
		// T* -> void* is fine
		if (tu->types[dst->ptr_to].kind == KIND_VOID) {
			return true;
		}
		
		return type_equal(tu, src->ptr_to, dst->ptr_to);
	}
	
	// but by default kind matching is enough
	// like for integers, booleans and floats
	return true;
}

static InitNode* walk_initializer_for_sema(TranslationUnit* tu, int node_count, InitNode* node) {
	for (int i = 0; i < node_count; i++) {
		if (node->kids_count == 0) {
			sema_expr(tu, node->expr);
			
			node += 1;
		} else {
			node = walk_initializer_for_sema(tu, node->kids_count, node + 1);
		}
	}
	
	return node;
}

static void try_resolve_typeof(TranslationUnit* tu, TypeIndex type) {
	Type* restrict ty = &tu->types[type];
	
	if (ty->kind == KIND_TYPEOF) {
		// spoopy...
		TypeIndex resolved = sema_expr(tu, ty->typeof_.src);
		*ty = tu->types[resolved];
	}
}

static InitNode* sema_infer_initializer_array_count(TranslationUnit* tu, int node_count, InitNode* node, int depth, int* out_array_count) {
	size_t cursor = 0;
	size_t max = 0;
	
	for (int i = 0; i < node_count; i++) {
		if (depth == 0) {
			// members shouldn't be here :p
			if (node->mode == INIT_MEMBER) return 0;
			else if (node->mode == INIT_ARRAY) {
				cursor = node->start + node->count;
				if (cursor > max) max = cursor;
			}
			else if (node->mode == INIT_NONE) {
				cursor++;
				if (cursor > max) max = cursor;
			}
		}
		
		if (node->kids_count == 0) {
			node += 1;
		} else {
			node = sema_infer_initializer_array_count(tu, node->kids_count, node + 1, depth + 1, NULL);
		}
	}
	
	if (depth == 0) {
		assert(max == (int)max);
		
		*out_array_count = max;
	}
	return node;
}

static bool is_assignable_expr(TranslationUnit* tu, ExprIndex e) {
	Expr* restrict ep = &tu->exprs[e];
	
	switch (ep->op) {
		case EXPR_DEREF:
		case EXPR_SUBSCRIPT:
		case EXPR_ARROW:
		case EXPR_DOT:
		return true;
		
		case EXPR_SYMBOL:
		case EXPR_PARAM:
		// TODO(NeGate): const-check
		return true;
		
		default:
		return false;
	}
}

Member* sema_traverse_members(TranslationUnit* tu, Type* record_type, Atom name, uint32_t* out_offset) {
	MemberIndex start = record_type->record.kids_start;
	MemberIndex end = record_type->record.kids_end;
	for (MemberIndex m = start; m < end; m++) {
		Member* member = &tu->members[m];
		
		// TODO(NeGate): String interning would be nice
		if (member->name == NULL) {
			// unnamed fields are traversed as well
			Type* child = &tu->types[member->type];
			assert(child->kind == KIND_STRUCT || child->kind == KIND_UNION);
			
			Member* search = sema_traverse_members(tu, child, name, out_offset);
			if (search) {
				*out_offset += member->offset;
				return search;
			}
		} else if (cstr_equals(name, member->name)) {
			*out_offset += member->offset;
			return member;
		}
	}
	
	return NULL;
}

Member* sema_resolve_member_access(TranslationUnit* tu, ExprIndex e, uint32_t* out_offset) {
	Expr* restrict ep = &tu->exprs[e];
	bool is_arrow = (ep->op == EXPR_ARROW);
	TypeIndex base_type = sema_expr(tu, ep->dot_arrow.base);
	
	Type* record_type = NULL;
	if (is_arrow) {
		Type* ptr_type = &tu->types[base_type];
		
		if (ptr_type->kind != KIND_PTR && ptr_type->kind != KIND_ARRAY) {
			sema_error(ep->loc, "Cannot do arrow operator on non-pointer type.");
			return NULL;
		}
		
		record_type = &tu->types[ptr_type->ptr_to];
	} else {
		record_type = &tu->types[base_type];
		
		// Implicit dereference
		if (record_type->kind == KIND_PTR) {
			record_type = &tu->types[record_type->ptr_to];
			
			if (settings.pedantic) {
				sema_error(ep->loc, "Implicit dereference is a non-standard extension (disable -P to allow it).");
				return NULL;
			}
		}
	}
	
	if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
		sema_error(ep->loc, "Cannot get the member of a non-record type.");
		return NULL;
	}
	
	uint32_t offset = 0;
	Member* search = sema_traverse_members(tu, record_type, ep->dot_arrow.name, &offset);
	if (search) {
		*out_offset += offset;
		return search;
	}
	
	sema_error(ep->loc, "Could not find member called '%s'", ep->dot_arrow.name);
	return NULL;
}

TypeIndex sema_expr(TranslationUnit* tu, ExprIndex e) {
	Expr* restrict ep = &tu->exprs[e];
	
	switch (ep->op) {
		case EXPR_UNKNOWN_SYMBOL: {
			return (ep->type = TYPE_VOID);
		}
		case EXPR_INT: {
			switch (ep->int_num.suffix) {
				case INT_SUFFIX_NONE: {
					unsigned int original = (unsigned int)ep->int_num.num;
					unsigned long long expected = (unsigned long long)ep->int_num.num;
					
					if (original != expected) {
						sema_error(ep->loc, "Could not represent integer literal as int. (%llu or %llx)", expected, expected);
					}
					
					return (ep->type = TYPE_INT);
				}
				
				case INT_SUFFIX_U: {
					unsigned int original = (unsigned int)ep->int_num.num;
					unsigned long long expected = (unsigned long long)ep->int_num.num;
					
					if (original != expected) {
						sema_error(ep->loc, "Could not represent integer literal as unsigned int.");
					}
					
					return (ep->type = TYPE_UINT);
				}
				
				case INT_SUFFIX_L:
				return (ep->type = settings.is_windows_long ? TYPE_INT : TYPE_LONG);
				case INT_SUFFIX_UL:
				return (ep->type = settings.is_windows_long ? TYPE_UINT : TYPE_ULONG);
				
				case INT_SUFFIX_LL:
				return (ep->type = TYPE_LONG);
				case INT_SUFFIX_ULL:
				return (ep->type = TYPE_ULONG);
				
				default:
				sema_error(ep->loc, "Could not represent integer literal.");
				return (ep->type = TYPE_VOID);
			}
		}
		case EXPR_ENUM: {
			return (ep->type = TYPE_INT);
		}
		case EXPR_FLOAT32: {
			return (ep->type = TYPE_FLOAT);
		}
		case EXPR_FLOAT64: {
			return (ep->type = TYPE_DOUBLE);
		}
		case EXPR_CHAR: {
			return (ep->type = TYPE_CHAR);
		}
		case EXPR_STR: {
			if (ep->str.start[0] == 'L') {
				const char* in = (const char*)(ep->str.start + 2);
				size_t len = ((const char*)ep->str.end - 1) - in;
				
				// it can't be bigger than the original
				wchar_t* out = arena_alloc((len * 2) + 1, 1);
				
				size_t out_i = 0, in_i = 0;
				while (in_i < len) {
					int ch;
					intptr_t distance = parse_char(len - in_i, &in[in_i], &ch);
					if (distance < 0) abort();
					
					assert(ch < 0x80);
					out[out_i++] = ch;
					in_i += distance;
				}
				
				assert(out_i <= len);
				out[out_i++] = '\0';
				
				ep->str.start = (unsigned char*) &out[0];
				ep->str.end = (unsigned char*) &out[out_i];
				
				return (ep->type = new_array(tu, TYPE_SHORT, out_i));
			} else {
				const char* in = (const char*)(ep->str.start + 1);
				size_t len = ((const char*)ep->str.end - 1) - in;
				
				// it can't be bigger than the original
				char* out = arena_alloc(len + 1, 1);
				
				size_t out_i = 0, in_i = 0;
				while (in_i < len) {
					int ch;
					intptr_t distance = parse_char(len - in_i, &in[in_i], &ch);
					if (distance < 0) abort();
					
					out[out_i++] = ch;
					in_i += distance;
				}
				
				assert(out_i <= len);
				out[out_i++] = '\0';
				
				ep->str.start = (unsigned char*) &out[0];
				ep->str.end = (unsigned char*) &out[out_i];
				
				return (ep->type = new_array(tu, TYPE_CHAR, out_i));
			}
		}
		case EXPR_SIZEOF: {
			TypeIndex src = sema_expr(tu, ep->x_of_expr.expr);
			
			//assert(tu->types[src].size && "Something went wrong...");
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[src].size, INT_SUFFIX_ULL }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF: {
			TypeIndex src = sema_expr(tu, ep->x_of_expr.expr);
			
			//assert(tu->types[src].align && "Something went wrong...");
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[src].align, INT_SUFFIX_ULL }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_SIZEOF_T: {
			try_resolve_typeof(tu, ep->x_of_type.type);
			
			if (tu->types[ep->x_of_type.type].kind == KIND_FUNC) {
				sema_warn(ep->loc, "sizeof(function type) is undefined (Cuik will always resolve it to 1)");
			}
			
			assert(tu->types[ep->x_of_type.type].size && "Something went wrong...");
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[ep->x_of_type.type].size, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF_T: {
			try_resolve_typeof(tu, ep->x_of_type.type);
			
			if (tu->types[ep->x_of_type.type].kind == KIND_FUNC) {
				sema_warn(ep->loc, "alignof(function type) is undefined (Cuik will always resolve it to 1)");
			}
			
			assert(tu->types[ep->x_of_type.type].align && "Something went wrong...");
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[ep->x_of_type.type].align, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_FUNCTION: {
			// ep->type is already set for this bad boy... maybe we
			// shouldn't do that for consistency sake...
			return ep->type;
		}
		case EXPR_INITIALIZER: {
			try_resolve_typeof(tu, ep->init.type);
			
			Type* type = &tu->types[ep->init.type];
			
			if (type->kind == KIND_ARRAY) {
				int old_array_count = type->array_count;
				
				int new_array_count;
				sema_infer_initializer_array_count(tu, ep->init.count, ep->init.nodes, 0, &new_array_count);
				
				// if it's 0, then it's unsized and anything goes
				if (old_array_count != 0) {
					// verify that everything fits correctly
					if (old_array_count < new_array_count) {
						sema_error(ep->loc, "Array cannot fit into declaration (needs %d, got %d)", old_array_count, new_array_count);
					}
				} else {
					ep->init.type = new_array(tu, type->array_of, new_array_count);
				}
			}
			
			walk_initializer_for_sema(tu, ep->init.count, ep->init.nodes);
			return (ep->type = ep->init.type);
		}
		case EXPR_LOGICAL_NOT: {
			/* TypeIndex src = */ sema_expr(tu, ep->unary_op.src);
			return (ep->type = TYPE_BOOL);
		}
		case EXPR_NOT:
		case EXPR_NEGATE:
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC:
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			TypeIndex src = sema_expr(tu, ep->unary_op.src);
			
			return (ep->type = src);
		}
		case EXPR_ADDR: {
			uint64_t dst;
			if (in_the_semantic_phase &&
				const_eval_try_offsetof_hack(tu, ep->unary_op.src, &dst)) {

				*ep = (Expr) {
					.op = EXPR_INT,
					.type = TYPE_ULONG,
					.int_num = { dst, INT_SUFFIX_ULL }
				};
				return TYPE_ULONG;
			}

			TypeIndex src = sema_expr(tu, ep->unary_op.src);
			return (ep->type = new_pointer(tu, src));
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			
			if (tu->stmts[stmt].op == STMT_LABEL) {
				return (ep->type = 0);
			} else {
				TypeIndex type = tu->stmts[stmt].decl.type;
				
				if (tu->types[type].kind == KIND_ARRAY) {
					// this is the only example where something sets it's own
					// cast_type it's an exception to the rules.
					ep->cast_type = new_pointer(tu, tu->types[type].array_of);
				}
				
				return (ep->type = type);
			}
		}
		case EXPR_PARAM: {
			int param_num = ep->param_num;
			
			Type* func_type = &tu->types[tu->stmts[function_stmt].decl.type];
			Param* params = &tu->params[func_type->func.param_list];
			return (ep->type = params[param_num].type);
		}
		case EXPR_CAST: {
			try_resolve_typeof(tu, ep->cast.type);
			
			/* TypeIndex src = */ sema_expr(tu, ep->cast.src);
			
			// set child's cast type
			tu->exprs[ep->cast.src].cast_type = ep->cast.type;
			return (ep->type = ep->cast.type);
		}
		case EXPR_SUBSCRIPT: {
			TypeIndex base = sema_expr(tu, ep->subscript.base);
			TypeIndex index = sema_expr(tu, ep->subscript.index);
			
			if (tu->types[index].kind == KIND_PTR ||
				tu->types[index].kind == KIND_ARRAY) {
				swap(base, index);
				swap(ep->subscript.base, ep->subscript.index);
			}
			
			if (tu->types[base].kind == KIND_ARRAY) {
				base = new_pointer(tu, tu->types[base].array_of);
			}
			
			if (tu->types[base].kind != KIND_PTR) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, base);
				sema_error(ep->loc, "Cannot perform subscript [] with base type '%s'", temp_string0);
				return (ep->type = TYPE_NONE);
			}
			
			tu->exprs[ep->subscript.base].cast_type = base;
			tu->exprs[ep->subscript.index].cast_type = TYPE_LONG;
			return (ep->type = tu->types[base].ptr_to);
		}
		case EXPR_DEREF: {
			TypeIndex base = sema_expr(tu, ep->unary_op.src);
			Type* type = &tu->types[base];
			
			if (type->kind == KIND_PTR) {
				return (ep->type = type->ptr_to);
			} else if (type->kind == KIND_ARRAY) {
				return (ep->type = type->array_of);
			} else {
				sema_error(ep->loc, "TODO");
				abort();
			}
		}
		case EXPR_CALL: {
			// Call function
			TypeIndex func_ptr = sema_expr(tu, ep->call.target);
			
			// implicit dereference
			if (tu->types[func_ptr].kind == KIND_PTR) {
				func_ptr = tu->types[func_ptr].ptr_to;
			}
			
			tu->exprs[ep->call.target].cast_type = func_ptr;
			
			Type* func_type = &tu->types[func_ptr];
			if (func_type->kind != KIND_FUNC) {
				sema_error(ep->loc, "function call target must be a function-type.");
				goto failure;
			}
			
			ExprIndex* args = ep->call.param_start;
			int arg_count = ep->call.param_count;
			
			Param* params = &tu->params[func_type->func.param_list];
			int param_count = func_type->func.param_count;
			
			if (func_type->func.has_varargs) {
				if (arg_count < param_count) {
					sema_error(ep->loc, "Not enough arguments (expected at least %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				// type-check the parameters with a known type
				for (size_t i = 0; i < param_count; i++) {
					TypeIndex arg_type = sema_expr(tu, args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(tu, arg_type, params[i].type, args[i])) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(tu, sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = tu->exprs[args[i]].loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					tu->exprs[args[i]].cast_type = params[i].type;
				}
				
				// type-check the untyped arguments
				for (size_t i = param_count; i < arg_count; i++) {
					TypeIndex src = sema_expr(tu, args[i]);
					
					// all integers ranked lower than int are promoted to int
					if (tu->types[src].kind >= KIND_BOOL && tu->types[src].kind < KIND_INT) {
						src = TYPE_INT;
					}
					
					// all floats ranked lower than double are promoted to double
					if (tu->types[src].kind == KIND_FLOAT) {
						src = TYPE_DOUBLE;
					}
					
					tu->exprs[args[i]].cast_type = src;
				}
			} else {
				if (arg_count != param_count) {
					sema_error(ep->loc, "Argument count mismatch (expected %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				for (size_t i = 0; i < arg_count; i++) {
					TypeIndex arg_type = sema_expr(tu, args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(tu, arg_type, params[i].type, args[i])) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(tu, sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = tu->exprs[args[i]].loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					tu->exprs[args[i]].cast_type = params[i].type;
				}
			}
			
			failure:
			return (ep->type = tu->types[func_ptr].func.return_type);
		}
		case EXPR_TERNARY: {
			TypeIndex cond_type = sema_expr(tu, ep->ternary_op.left);
			if (!is_scalar_type(tu, cond_type)) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(ep->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			tu->exprs[ep->ternary_op.left].cast_type = TYPE_BOOL;
			
			TypeIndex type = get_common_type(tu,
											 sema_expr(tu, ep->ternary_op.middle),
											 sema_expr(tu, ep->ternary_op.right));
			
			tu->exprs[ep->ternary_op.middle].cast_type = type;
			tu->exprs[ep->ternary_op.right].cast_type = type;
			
			return (ep->type = type);
		}
		case EXPR_COMMA: {
			sema_expr(tu, ep->bin_op.left);
			
			return (ep->type = sema_expr(tu, ep->bin_op.right));
		}
		case EXPR_DOT:
		case EXPR_ARROW: {
			uint32_t offset = 0;
			Member* m = sema_resolve_member_access(tu, e, &offset);
			if (m) {
				if (in_the_semantic_phase) {
					ep->dot_arrow.member = m - tu->members;
					ep->dot_arrow.offset = offset;
				}
				return (ep->type = m->type);
			}
			
			return (ep->type = TYPE_VOID);
		}
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR: {
			sema_expr(tu, ep->bin_op.left);
			sema_expr(tu, ep->bin_op.right);
			
			tu->exprs[ep->bin_op.left].cast_type = TYPE_BOOL;
			tu->exprs[ep->bin_op.right].cast_type = TYPE_BOOL;
			
			return (ep->type = TYPE_BOOL);
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
			TypeIndex lhs = sema_expr(tu, ep->bin_op.left);
			TypeIndex rhs = sema_expr(tu, ep->bin_op.right);
			
			if ((ep->op == EXPR_PLUS ||
				 ep->op == EXPR_MINUS) &&
				(tu->types[lhs].kind == KIND_PTR || 
				 tu->types[lhs].kind == KIND_ARRAY || 
				 tu->types[rhs].kind == KIND_PTR ||
				 tu->types[rhs].kind == KIND_ARRAY)) {
				// Pointer arithmatic
				if (ep->op == EXPR_PLUS && (tu->types[rhs].kind == KIND_PTR || tu->types[rhs].kind == KIND_ARRAY)) {
					swap(lhs, rhs);
					swap(ep->bin_op.left, ep->bin_op.right);
				}
				
				if (tu->types[rhs].kind == KIND_PTR || tu->types[rhs].kind == KIND_ARRAY) {
					if (ep->op == EXPR_MINUS) {
						// ptr - ptr = ptrdiff_t
						tu->exprs[ep->bin_op.left].cast_type = lhs;
						tu->exprs[ep->bin_op.right].cast_type = rhs;
						
						ep->op = EXPR_PTRDIFF;
						return (ep->type = TYPE_LONG);
					} else {
						sema_error(ep->loc, "Cannot do pointer addition with two pointer operands, one must be an integral type.");
						return (ep->type = TYPE_VOID);
					}
				} else {
					tu->exprs[ep->bin_op.left].cast_type = lhs;
					tu->exprs[ep->bin_op.right].cast_type = TYPE_ULONG;
					
					ep->op = (ep->op == EXPR_PLUS) ? EXPR_PTRADD : EXPR_PTRSUB;
					return (ep->type = lhs);
				}
			} else {
				if (!(tu->types[lhs].kind >= KIND_BOOL && 
					  tu->types[lhs].kind <= KIND_DOUBLE && 
					  tu->types[rhs].kind >= KIND_BOOL && 
					  tu->types[rhs].kind <= KIND_DOUBLE)) {
					type_as_string(tu, sizeof(temp_string0), temp_string0, lhs);
					type_as_string(tu, sizeof(temp_string1), temp_string1, rhs);
					
					sema_error(ep->loc, "Cannot apply binary operator to %s and %s.", temp_string0, temp_string1);
					return (ep->type = TYPE_VOID);
				}
				
				TypeIndex type = get_common_type(tu, lhs, rhs);
				tu->exprs[ep->bin_op.left].cast_type = type;
				tu->exprs[ep->bin_op.right].cast_type = type;
				
				return (ep->type = type);
			}
		}
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			TypeIndex type = get_common_type(tu,
											 sema_expr(tu, ep->bin_op.left),
											 sema_expr(tu, ep->bin_op.right));
			
			tu->exprs[ep->bin_op.left].cast_type = type;
			tu->exprs[ep->bin_op.right].cast_type = type;
			
			return (ep->type = TYPE_BOOL);
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
			if (!is_assignable_expr(tu, ep->bin_op.left)) {
				sema_error(tu->exprs[ep->bin_op.left].loc, "Left-hand side is not assignable");
				
				tu->exprs[ep->bin_op.left].cast_type = 0;
				tu->exprs[ep->bin_op.right].cast_type = 0;
				return (ep->type = 0);
			}
			
			TypeIndex lhs = sema_expr(tu, ep->bin_op.left);
			sema_expr(tu, ep->bin_op.right);
			
			tu->exprs[ep->bin_op.left].cast_type = lhs;
			tu->exprs[ep->bin_op.right].cast_type = lhs;
			
			return (ep->type = lhs);
		}
		default:
		break;
	}
	
	abort();
}

void sema_stmt(TranslationUnit* tu, StmtIndex s) {
	Stmt* restrict sp = &tu->stmts[s];
	
	switch (sp->op) {
		case STMT_NONE: break;
		case STMT_LABEL: break;
		case STMT_GOTO: {
			sema_expr(tu, sp->goto_.target);
			break;
		}
		case STMT_COMPOUND: {
			StmtIndex* kids = sp->compound.kids;
			size_t count = sp->compound.kids_count;
			
			StmtIndex killer = 0;
			for (size_t i = 0; i < count; i++) {
				StmtIndex kid = kids[i];
				sema_stmt(tu, kid);
				
				if (killer) {
					if (tu->stmts[kid].op == STMT_LABEL) {
						killer = 0;
					} else {
						sema_error(tu->stmts[kid].loc, "Dead code");
						sema_info(tu->stmts[killer].loc, "After");
						goto compound_failure;
					}
				} else {
					if (tu->stmts[kid].op == STMT_RETURN ||
						tu->stmts[kid].op == STMT_GOTO) {
						killer = kid;
					}
				}
			}
			
			compound_failure:
			break;
		}
		case STMT_DECL: {
			if (sp->decl.initial) {
				try_resolve_typeof(tu, sp->decl.type);
				
				if (tu->exprs[sp->decl.initial].op == EXPR_INITIALIZER &&
					tu->exprs[sp->decl.initial].init.type == 0) {
					// give it something to go off of
					tu->exprs[sp->decl.initial].init.type = sp->decl.type;
				}
				
				TypeIndex expr_type_index = sema_expr(tu, sp->decl.initial);
				
				Expr* restrict ep = &tu->exprs[sp->decl.initial];
				if (ep->op == EXPR_INITIALIZER) {
					Type* restrict tp = &tu->types[sp->decl.type];
					Type* restrict expr_type = &tu->types[sp->decl.type];
					
					// Auto-detect array count from initializer
					if (tp->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
						if (tp->array_count != 0 && tp->array_count < expr_type->array_count) {
							sema_error(sp->loc, "Array initializer does not fit into declaration (expected %d, got %d)", tp->array_count, expr_type->array_count);
						} else {
							sp->decl.type = expr_type_index;
						}
					}
				} else if (ep->op == EXPR_STR) {
					Type* restrict tp = &tu->types[sp->decl.type];
					
					// Auto-detect array count from string
					if (tp->kind == KIND_ARRAY && tp->array_count == 0) {
						sp->decl.type = expr_type_index;
					}
				}
				
				ep->cast_type = sp->decl.type;
				if (!type_compatible(tu, expr_type_index, sp->decl.type, sp->decl.initial)) {
					type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type_index);
					type_as_string(tu, sizeof(temp_string1), temp_string1, sp->decl.type);
					
					sema_error(sp->loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
				}
			}
			break;
		}
		case STMT_EXPR: {
			sema_expr(tu, sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			if (sp->return_.expr) {
				TypeIndex expr_type = sema_expr(tu, sp->return_.expr);
				TypeIndex return_type = tu->types[tu->stmts[function_stmt].decl.type].func.return_type;
				
				if (!type_compatible(tu, expr_type, return_type, sp->return_.expr)) {
					//sema_warn(sp->loc, "Value in return statement does not match function signature. (TODO this should be an error)");
				}
				
				tu->exprs[sp->return_.expr].cast_type = return_type;
			}
			break;
		}
		case STMT_IF: {
			TypeIndex cond_type = sema_expr(tu, sp->if_.cond);
			if (!is_scalar_type(tu, cond_type)) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(sp->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			tu->exprs[sp->if_.cond].cast_type = TYPE_BOOL;
			
			sema_stmt(tu, sp->if_.body);
			if (sp->if_.next) sema_stmt(tu, sp->if_.next);
			break;
		}
		case STMT_WHILE: {
			sema_expr(tu, sp->while_.cond);
			if (sp->while_.body) {
				sema_stmt(tu, sp->while_.body);
			}
			break;
		}
		case STMT_DO_WHILE: {
			if (sp->do_while.body) {
				sema_stmt(tu, sp->do_while.body);
			}
			sema_expr(tu, sp->do_while.cond);
			break;
		}
		case STMT_FOR: {
			if (sp->for_.first) {
				sema_stmt(tu, sp->for_.first);
			}
			
			if (sp->for_.cond) {
				sema_expr(tu, sp->for_.cond);
			}
			
			if (sp->for_.body) {
				sema_stmt(tu, sp->for_.body);
			}
			
			if (sp->for_.next) {
				sema_expr(tu, sp->for_.next);
			}
			break;
		}
		case STMT_SWITCH: {
			sema_expr(tu, sp->switch_.condition);
			sema_stmt(tu, sp->switch_.body);
			break;
		}
		case STMT_CASE: {
			sema_stmt(tu, sp->case_.body);
			break;
		}
		case STMT_DEFAULT: {
			sema_stmt(tu, sp->default_.body);
			break;
		}
		case STMT_CONTINUE: 
		case STMT_BREAK: {
			break;
		}
		default:
		assert(0);
	}
}

TypeIndex sema_guess_type(TranslationUnit* tu, StmtIndex s) {
	Stmt* sp = &tu->stmts[s];
	char* name = (char*) sp->decl.name;
	
	TypeIndex type_index = sp->decl.type;
	Type* type = &tu->types[type_index];
	
	if (sp->decl.attrs.is_static && sp->decl.attrs.is_extern) {
		sema_error(sp->loc, "Global declaration '%s' cannot be both static and extern.", name);
		sp->backing.g = 0;
		return TYPE_NONE;
	}
	
	if (type->is_incomplete) {
		if (type->kind == KIND_STRUCT) {
			sema_error(sp->loc, "Incomplete type (struct %s) in declaration", type->record.name);
		} else if (type->kind == KIND_UNION) {
			sema_error(sp->loc, "Incomplete type (union %s) in declaration", type->record.name);
		} else {
			sema_error(sp->loc, "Incomplete type in declaration");
		}
	}
	
	if (sp->decl.attrs.is_extern || type->kind == KIND_FUNC) {
		return TYPE_NONE;
	}
	
	if (sp->decl.initial) {
		Expr* ep = &tu->exprs[sp->decl.initial];
		
		if (type->kind == KIND_ARRAY && ep->op == EXPR_INITIALIZER) {
			// check how many top level statements we have
			int array_count;
			sema_infer_initializer_array_count(tu, ep->init.count, ep->init.nodes, 0, &array_count);
			
			return new_array(tu, type->array_of, array_count);
		}
	}
	
	return sp->decl.type;
}

static void sema_top_level(TranslationUnit* tu, StmtIndex s, bool frontend_only) {
	Stmt* restrict sp = &tu->stmts[s];
	
	TypeIndex type_index = sp->decl.type;
	Type* type = &tu->types[type_index];
	
	char* name = (char*) sp->decl.name;
	switch (sp->op) {
		case STMT_FUNC_DECL: {
			assert(type->kind == KIND_FUNC);
			const Type* return_type = &tu->types[type->func.return_type];
			
			if (sp->decl.attrs.is_static && sp->decl.attrs.is_extern) {
				sema_error(sp->loc, "Function '%s' cannot be both static and extern.", name);
				sp->backing.f = 0;
				break;
			}
			
			if (sp->decl.attrs.is_static && !sp->decl.attrs.is_inline) {
				if (!sp->decl.attrs.is_used) {
					sema_warn(sp->loc, "Function '%s' is never used.", name);
					sp->backing.f = 0;
					break;
				}
			}
			
			if (sp->decl.attrs.is_inline && !sp->decl.attrs.is_used) {
				sp->backing.f = 0;
				break;
			}
			
			bool is_aggregate_return = false;
			if (return_type->kind == KIND_STRUCT ||
				return_type->kind == KIND_UNION) {
				is_aggregate_return = true;
			}
			
			if (frontend_only) {
				sp->backing.f = 0;
				
				// type check function body
				function_stmt = s;
				sema_stmt(tu, (StmtIndex)sp->decl.initial);
				function_stmt = 0;
				break;
			}
			
			// parameters
			ParamIndex param_list = type->func.param_list;
			ParamIndex param_count = type->func.param_count;
			
			// aggregate return values take up the first parameter slot.
			ParamIndex real_param_count = param_count + is_aggregate_return;
			
			TB_DataType return_dt = ctype_to_tbtype(return_type);
			TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, return_dt, real_param_count, type->func.has_varargs);
			
			if (is_aggregate_return) {
				tb_prototype_add_param(proto, TB_TYPE_PTR);
			}
			
			for (size_t i = 0; i < param_count; i++) {
				Param* p = &tu->params[param_list + i];
				
				// Decide on the data type
				Type* param_type = &tu->types[p->type];
				TB_DataType dt = ctype_to_tbtype(param_type);
				
				assert(dt.width < 8);
				tb_prototype_add_param(proto, dt);
			}
			
			TB_Linkage linkage = sp->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
			
			// TODO(NeGate): Fix this up because it's possibly wrong, essentially
			// inline linkage means all the definitions must match which isn't
			// necessarily the same as static where they all can share a name but
			// are different and internal.
			TB_Function* func;
			if (sp->decl.attrs.is_inline) {
				linkage = TB_LINKAGE_PRIVATE;
				
				char temp[1024];
				snprintf(temp, 1024, "_K%d_%s", s, name ? name : "<unnamed>");
				
				func = tb_prototype_build(mod, proto, temp, linkage);
			} else {
				func = tb_prototype_build(mod, proto, name, linkage);
			}
			sp->backing.f = tb_function_get_id(mod, func);
			
			// type check function body
			function_stmt = s;
			sema_stmt(tu, (StmtIndex)sp->decl.initial);
			function_stmt = 0;
			break;
		}
		case STMT_DECL:
		case STMT_GLOBAL_DECL: {
			if (!sp->decl.attrs.is_used) break;
			if (sp->decl.attrs.is_typedef) break;
			
			if (sp->decl.attrs.is_static && sp->decl.attrs.is_extern) {
				sema_error(sp->loc, "Global declaration '%s' cannot be both static and extern.", name);
				sp->backing.g = 0;
				break;
			}
			
			if (!sp->decl.attrs.is_extern && type->kind != KIND_FUNC) {
				Type* expr_type = NULL;
				
				if (sp->decl.initial) {
					if (tu->exprs[sp->decl.initial].op == EXPR_INITIALIZER &&
						tu->exprs[sp->decl.initial].init.type == 0) {
						// give it something to go off of
						//
						// doesn't have to be complete in terms of array count
						// just enough to infer the rest in a sec
						tu->exprs[sp->decl.initial].init.type = sp->decl.type;
					}
					
					TypeIndex expr_type_index = sema_expr(tu, sp->decl.initial);
					expr_type = &tu->types[expr_type_index];
					
					if (tu->exprs[sp->decl.initial].op == EXPR_INITIALIZER ||
						tu->exprs[sp->decl.initial].op == EXPR_STR) {
						if (type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
							if (type_equal(tu, type->array_of, expr_type->array_of)) {
								if (type->array_count != 0 && type->array_count < expr_type->array_count) {
									sema_error(sp->loc, "Array initializer does not fit into declaration (expected %d, got %d)", type->array_count, expr_type->array_count);
								} else {
									assert(expr_type->array_count);
									
									// preserve constness
									bool is_const = type->is_const;
									
									type_index = copy_type(tu, expr_type_index);
									type = &tu->types[type_index];
									type->is_const = is_const;
									
									sp->decl.type = type_index;
								}
							} else {
								type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type->array_of);
								type_as_string(tu, sizeof(temp_string1), temp_string1, type->array_of);
								
								sema_error(sp->loc, "Array initializer type mismatch (got '%s', expected '%s')", temp_string0, temp_string1);
							}
						}
					}
					
					if (!type_compatible(tu, expr_type_index, type_index, sp->decl.initial)) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, type_index);
						type_as_string(tu, sizeof(temp_string1), temp_string1, expr_type_index);
						
						sema_error(sp->loc, "Declaration type does not match (got '%s', expected '%s')", temp_string0, temp_string1);
					}
				}
				
				if (type->is_incomplete) {
					if (type->kind == KIND_STRUCT) {
						sema_error(sp->loc, "Incomplete type (struct %s) in declaration", type->record.name);
					} else if (type->kind == KIND_UNION) {
						sema_error(sp->loc, "Incomplete type (union %s) in declaration", type->record.name);
					} else {
						sema_error(sp->loc, "Incomplete type in declaration");
					}
				}
				
				if (frontend_only) {
					sp->backing.g = 0;
				} else { 
					if (sp->decl.attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
						tb_module_set_tls_index(mod, tb_extern_create(mod, "_tls_index"));
					}

					TB_Linkage linkage = sp->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
					sp->backing.g = tb_global_create(mod, name, sp->decl.attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, linkage);
				}
			}
			break;
		}
		default: assert(0);
	}
}

static void sema_mark_children(TranslationUnit* tu, ExprIndex e) {
	Expr* restrict ep = &tu->exprs[e];
	if (ep->op == EXPR_ENUM) return;
	if (ep->op == EXPR_UNKNOWN_SYMBOL) return;

	assert(ep->op == EXPR_SYMBOL);
	Stmt* restrict sp = &tu->stmts[ep->symbol];
	
	if (sp->op == STMT_FUNC_DECL ||
		sp->op == STMT_DECL ||
		sp->op == STMT_GLOBAL_DECL) {
		if (!sp->decl.attrs.is_used) {
			sp->decl.attrs.is_used = true;
			ExprIndex sym = sp->decl.first_symbol;
			
			while (sym) {
				sema_mark_children(tu, sym);
				sym = tu->exprs[sym].next_symbol_in_chain;
			}
		}
	}
}

void sema_pass(CompilationUnit* cu, TranslationUnit* tu, bool frontend_only) {
	size_t count = arrlen(tu->top_level_stmts);
	in_the_semantic_phase = true;
	
	// simple mark and sweep to remove unused symbols
	timed_block("sema: collection") {
		for (size_t i = 0; i < count; i++) {
			Stmt* restrict sp = &tu->stmts[tu->top_level_stmts[i]];
			assert(sp->op == STMT_FUNC_DECL || sp->op == STMT_DECL || sp->op == STMT_GLOBAL_DECL);
			
			if (sp->decl.attrs.is_root) {
				sp->decl.attrs.is_used = true;
				
				ExprIndex sym = sp->decl.first_symbol;
				while (sym) {
					sema_mark_children(tu, sym);
					sym = tu->exprs[sym].next_symbol_in_chain;
				}
			}
		}
	}
	
	// go through all top level statements and type check
	timed_block("sema: top level") {
		for (size_t i = 0; i < count; i++) {
			sema_top_level(tu, tu->top_level_stmts[i], frontend_only);
		}
	}

	in_the_semantic_phase = false;
}
