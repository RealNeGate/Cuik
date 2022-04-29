#include "sema.h"
#include "settings.h"

#include <targets/targets.h>
#include <back/ir_gen.h>
#include <stdarg.h>

// when you're not in the semantic phase, we don't
// rewrite the contents of the DOT and ARROW exprs
// because it may screw with things
thread_local bool in_the_semantic_phase;
static thread_local Stmt* function_stmt;

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];

static bool is_scalar_type(TranslationUnit* tu, TypeIndex type_index) {
	Type* restrict type = &tu->types[type_index];
	return (type->kind >= KIND_BOOL && type->kind <= KIND_FUNC);
}

// doesn't do implicit casts
bool type_very_compatible(TranslationUnit* tu, TypeIndex a, TypeIndex b) {
	if (a == b) return true;
	
	Type* restrict src = &tu->types[a];
	Type* restrict dst = &tu->types[b];
	
	if (src->kind != dst->kind) return false;
	
	switch (src->kind) {
		case KIND_BOOL:
		case KIND_CHAR:
		case KIND_SHORT:
		case KIND_INT:
		case KIND_LONG:
		return src->is_unsigned == dst->is_unsigned;
		
		case KIND_FLOAT:
		case KIND_DOUBLE:
		return true;
		
		case KIND_PTR:
		return type_very_compatible(tu, src->ptr_to, dst->ptr_to);
		case KIND_FUNC:
		return type_equal(tu, a, b);
		case KIND_ARRAY:
		if (!type_very_compatible(tu, src->array_of, dst->array_of)) {
			return false;
		}
		
		return src->array_count == dst->array_count;
		
		default:
		return true;
	}
}

// Also checks if expression is an integer literal because we
// have a special case for 0 to pointer conversions.
bool type_compatible(TranslationUnit* tu, TypeIndex a, TypeIndex b, Expr* a_expr) {
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
			else if (a_expr->op == EXPR_INT) {
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
			if (a_expr->op == EXPR_INT &&
				a_expr->int_num.num == 0) {
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

static bool is_assignable_expr(TranslationUnit* tu, Expr* restrict e) {
	switch (e->op) {
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
	Member* kids = record_type->record.kids;
	size_t count = record_type->record.kid_count;
	
	for (size_t i = 0; i < count; i++) {
		Member* member = &kids[i];
		
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

Member* sema_resolve_member_access(TranslationUnit* tu, Expr* restrict e, uint32_t* out_offset) {
	bool is_arrow = (e->op == EXPR_ARROW);
	TypeIndex base_type = sema_expr(tu, e->dot_arrow.base);
	
	Type* record_type = NULL;
	if (is_arrow) {
		Type* ptr_type = &tu->types[base_type];
		
		if (ptr_type->kind != KIND_PTR && ptr_type->kind != KIND_ARRAY) {
			sema_error(e->loc, "Cannot do arrow operator on non-pointer type.");
			return NULL;
		}
		
		record_type = &tu->types[ptr_type->ptr_to];
	} else {
		record_type = &tu->types[base_type];
		
		// Implicit dereference
		if (record_type->kind == KIND_PTR) {
			record_type = &tu->types[record_type->ptr_to];
			
			if (settings.pedantic) {
				sema_error(e->loc, "Implicit dereference is a non-standard extension (disable -P to allow it).");
				return NULL;
			}
		}
	}
	
	if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
		sema_error(e->loc, "Cannot get the member of a non-record type.");
		return NULL;
	}
	
	uint32_t offset = 0;
	Member* search = sema_traverse_members(tu, record_type, e->dot_arrow.name, &offset);
	if (search) {
		*out_offset += offset;
		return search;
	}
	
	sema_error(e->loc, "Could not find member called '%s'", e->dot_arrow.name);
	return NULL;
}

TypeIndex sema_expr(TranslationUnit* tu, Expr* restrict e) {
	switch (e->op) {
		case EXPR_UNKNOWN_SYMBOL: {
			return (e->type = TYPE_VOID);
		}
		case EXPR_INT: {
			switch (e->int_num.suffix) {
				case INT_SUFFIX_NONE: {
					unsigned int original = (unsigned int)e->int_num.num;
					unsigned long long expected = (unsigned long long)e->int_num.num;
					
					if (original != expected) {
						sema_error(e->loc, "Could not represent integer literal as int. (%llu or %llx)", expected, expected);
					}
					
					return (e->type = TYPE_INT);
				}
				
				case INT_SUFFIX_U: {
					unsigned int original = (unsigned int)e->int_num.num;
					unsigned long long expected = (unsigned long long)e->int_num.num;
					
					if (original != expected) {
						sema_error(e->loc, "Could not represent integer literal as unsigned int.");
					}
					
					return (e->type = TYPE_UINT);
				}
				
				case INT_SUFFIX_L:
				return (e->type = settings.is_windows_long ? TYPE_INT : TYPE_LONG);
				case INT_SUFFIX_UL:
				return (e->type = settings.is_windows_long ? TYPE_UINT : TYPE_ULONG);
				
				case INT_SUFFIX_LL:
				return (e->type = TYPE_LONG);
				case INT_SUFFIX_ULL:
				return (e->type = TYPE_ULONG);
				
				default:
				sema_error(e->loc, "Could not represent integer literal.");
				return (e->type = TYPE_VOID);
			}
		}
		case EXPR_ENUM: {
			return (e->type = TYPE_INT);
		}
		case EXPR_FLOAT32: {
			return (e->type = TYPE_FLOAT);
		}
		case EXPR_FLOAT64: {
			return (e->type = TYPE_DOUBLE);
		}
		case EXPR_CHAR: {
			return (e->type = TYPE_CHAR);
		}
		case EXPR_WSTR: {
			const char* in = (const char*)(e->str.start + 1);
			size_t len = ((const char*)e->str.end - 1) - in;
			
			// it can't be bigger than the original
			wchar_t* out = arena_alloc(&thread_arena, (len + 1) * 2, 1);
			
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
			
			e->str.start = (unsigned char*) &out[0];
			e->str.end = (unsigned char*) &out[out_i];
			
			return (e->type = new_array(tu, TYPE_SHORT, out_i));
		}
		case EXPR_STR: {
			const char* in = (const char*)(e->str.start + 1);
			size_t len = ((const char*)e->str.end - 1) - in;
			
			// it can't be bigger than the original
			char* out = arena_alloc(&thread_arena, len + 1, 1);
			
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
			
			e->str.start = (unsigned char*) &out[0];
			e->str.end = (unsigned char*) &out[out_i];
			
			return (e->type = new_array(tu, TYPE_CHAR, out_i));
		}
		case EXPR_SIZEOF: {
			TypeIndex src = sema_expr(tu, e->x_of_expr.expr);
			
			//assert(tu->types[src].size && "Something went wrong...");
			*e = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[src].size, INT_SUFFIX_ULL }
			};
			return (e->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF: {
			TypeIndex src = sema_expr(tu, e->x_of_expr.expr);
			
			//assert(tu->types[src].align && "Something went wrong...");
			*e = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[src].align, INT_SUFFIX_ULL }
			};
			return (e->type = TYPE_ULONG);
		}
		case EXPR_SIZEOF_T: {
			try_resolve_typeof(tu, e->x_of_type.type);
			
			if (tu->types[e->x_of_type.type].kind == KIND_FUNC) {
				sema_warn(e->loc, "sizeof(function type) is undefined (Cuik will always resolve it to 1)");
			}
			
			assert(tu->types[e->x_of_type.type].size && "Something went wrong...");
			*e = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[e->x_of_type.type].size, INT_SUFFIX_NONE }
			};
			return (e->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF_T: {
			try_resolve_typeof(tu, e->x_of_type.type);
			
			if (tu->types[e->x_of_type.type].kind == KIND_FUNC) {
				sema_warn(e->loc, "alignof(function type) is undefined (Cuik will always resolve it to 1)");
			}
			
			assert(tu->types[e->x_of_type.type].align && "Something went wrong...");
			*e = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { tu->types[e->x_of_type.type].align, INT_SUFFIX_NONE }
			};
			return (e->type = TYPE_ULONG);
		}
		case EXPR_FUNCTION: {
			// e->type is already set for this bad boy... maybe we
			// shouldn't do that for consistency sake...
			return e->type;
		}
		case EXPR_INITIALIZER: {
			try_resolve_typeof(tu, e->init.type);
			
			Type* type = &tu->types[e->init.type];
			
			if (type->kind == KIND_ARRAY) {
				int old_array_count = type->array_count;
				
				int new_array_count;
				sema_infer_initializer_array_count(tu, e->init.count, e->init.nodes, 0, &new_array_count);
				
				// if it's 0, then it's unsized and anything goes
				if (old_array_count != 0) {
					// verify that everything fits correctly
					if (old_array_count < new_array_count) {
						sema_error(e->loc, "Array cannot fit into declaration (needs %d, got %d)", old_array_count, new_array_count);
					}
				} else {
					e->init.type = new_array(tu, type->array_of, new_array_count);
				}
			}
			
			walk_initializer_for_sema(tu, e->init.count, e->init.nodes);
			return (e->type = e->init.type);
		}
		case EXPR_LOGICAL_NOT: {
			/* TypeIndex src = */ sema_expr(tu, e->unary_op.src);
			return (e->type = TYPE_BOOL);
		}
		case EXPR_NOT:
		case EXPR_NEGATE:
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC:
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			TypeIndex src = sema_expr(tu, e->unary_op.src);
			
			return (e->type = src);
		}
		case EXPR_ADDR: {
			uint64_t dst;
			if (in_the_semantic_phase &&
				const_eval_try_offsetof_hack(tu, e->unary_op.src, &dst)) {
				
				*e = (Expr) {
					.op = EXPR_INT,
					.type = TYPE_ULONG,
					.int_num = { dst, INT_SUFFIX_ULL }
				};
				return TYPE_ULONG;
			}
			
			TypeIndex src = sema_expr(tu, e->unary_op.src);
			return (e->type = new_pointer(tu, src));
		}
		case EXPR_SYMBOL: {
			Stmt* restrict sym = e->symbol;
			
			if (sym->op == STMT_LABEL) {
				return (e->type = 0);
			} else {
				TypeIndex type = sym->decl.type;
				
				if (tu->types[type].kind == KIND_ARRAY) {
					// this is the only example where something sets it's own
					// cast_type it's an exception to the rules.
					e->cast_type = new_pointer(tu, tu->types[type].array_of);
				}
				
				return (e->type = type);
			}
		}
		case EXPR_PARAM: {
			int param_num = e->param_num;
			
			Type* func_type = &tu->types[function_stmt->decl.type];
			return (e->type = func_type->func.param_list[param_num].type);
		}
		case EXPR_GENERIC: {
			TypeIndex src = sema_expr(tu, e->generic_.controlling_expr);
			
			// _Generic's controlling expression does rvalue conversions so
			// an array is treated as a pointer not an array
			if (tu->types[src].kind == KIND_ARRAY) {
				src = new_pointer(tu, tu->types[src].array_of);
			} else if (tu->types[src].kind == KIND_FUNC) {
				src = new_pointer(tu, src);
			}
			
			Expr* default_case = 0;
			Expr* match = 0;
			
			for (size_t i = 0; i < e->generic_.case_count; i++) {
				if (e->generic_.cases[i].key == 0) {
					default_case = e->generic_.cases[i].value;
				} else if (type_very_compatible(tu, e->generic_.cases[i].key, src)) {
					match = e->generic_.cases[i].value;
				}
			}
			
			if (match == 0) {
				if (default_case == 0) {
					// if we didn't match anything and there's no default case, error out
					sema_error(e->loc, "Could not match _Generic against any cases");
					return 0;
				}
				
				e->generic_.controlling_expr = default_case;
			} else {
				e->generic_.controlling_expr = match;
			}
			
			// once we set case_count to 0, we've resolved the _Generic
			e->generic_.cases = NULL;
			e->generic_.case_count = 0;
			
			return (e->type = sema_expr(tu, e->generic_.controlling_expr));
		}
		case EXPR_CAST: {
			try_resolve_typeof(tu, e->cast.type);
			
			/* TypeIndex src = */ sema_expr(tu, e->cast.src);
			
			// set child's cast type
			e->cast.src->cast_type = e->cast.type;
			return (e->type = e->cast.type);
		}
		case EXPR_SUBSCRIPT: {
			TypeIndex base = sema_expr(tu, e->subscript.base);
			TypeIndex index = sema_expr(tu, e->subscript.index);
			
			if (tu->types[index].kind == KIND_PTR ||
				tu->types[index].kind == KIND_ARRAY) {
				swap(base, index);
				swap(e->subscript.base, e->subscript.index);
			}
			
			if (tu->types[base].kind == KIND_ARRAY) {
				base = new_pointer(tu, tu->types[base].array_of);
			}
			
			if (tu->types[base].kind != KIND_PTR) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, base);
				sema_error(e->loc, "Cannot perform subscript [] with base type '%s'", temp_string0);
				return (e->type = TYPE_NONE);
			}
			
			e->subscript.base->cast_type = base;
			e->subscript.index->cast_type = TYPE_LONG;
			return (e->type = tu->types[base].ptr_to);
		}
		case EXPR_DEREF: {
			TypeIndex base = sema_expr(tu, e->unary_op.src);
			Type* type = &tu->types[base];
			
			if (type->kind == KIND_PTR) {
				return (e->type = type->ptr_to);
			} else if (type->kind == KIND_ARRAY) {
				return (e->type = type->array_of);
			} else {
				sema_error(e->loc, "TODO");
				abort();
			}
		}
		case EXPR_CALL: {
			if (e->call.target->op == EXPR_UNKNOWN_SYMBOL) {
				// We can only get to this point with UNKNOWN_SYMBOL if it's a builtin
				const char* name = (const char*) e->call.target->unknown_sym;
				
				Expr** args = e->call.param_start;
				int arg_count = e->call.param_count;
				
				TypeIndex ty = target_desc.type_check_builtin(tu, e->loc, name, arg_count, args);
				return (e->type = ty);
			}
			
			// Call function
			TypeIndex func_ptr = sema_expr(tu, e->call.target);
			
			// implicit dereference
			if (tu->types[func_ptr].kind == KIND_PTR) {
				func_ptr = tu->types[func_ptr].ptr_to;
			}
			
			e->call.target->cast_type = func_ptr;
			
			Type* func_type = &tu->types[func_ptr];
			if (func_type->kind != KIND_FUNC) {
				sema_error(e->loc, "function call target must be a function-type.");
				goto failure;
			}
			
			Expr** args = e->call.param_start;
			int arg_count = e->call.param_count;
			
			Param* params = func_type->func.param_list;
			int param_count = func_type->func.param_count;
			
			if (func_type->func.has_varargs) {
				if (arg_count < param_count) {
					sema_error(e->loc, "Not enough arguments (expected at least %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				// type-check the parameters with a known type
				for (size_t i = 0; i < param_count; i++) {
					TypeIndex arg_type = sema_expr(tu, args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(tu, arg_type, params[i].type, args[i])) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(tu, sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = args[i]->loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					args[i]->cast_type = params[i].type;
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
					
					args[i]->cast_type = src;
				}
			} else {
				if (arg_count != param_count) {
					sema_error(e->loc, "Argument count mismatch (expected %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				for (size_t i = 0; i < arg_count; i++) {
					TypeIndex arg_type = sema_expr(tu, args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(tu, arg_type, params[i].type, args[i])) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(tu, sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = args[i]->loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					args[i]->cast_type = params[i].type;
				}
			}
			
			failure:
			return (e->type = tu->types[func_ptr].func.return_type);
		}
		case EXPR_TERNARY: {
			TypeIndex cond_type = sema_expr(tu, e->ternary_op.left);
			if (!is_scalar_type(tu, cond_type)) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(e->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			e->ternary_op.left->cast_type = TYPE_BOOL;
			
			TypeIndex type = get_common_type(tu,
											 sema_expr(tu, e->ternary_op.middle),
											 sema_expr(tu, e->ternary_op.right));
			
			e->ternary_op.middle->cast_type = type;
			e->ternary_op.right->cast_type = type;
			
			return (e->type = type);
		}
		case EXPR_COMMA: {
			sema_expr(tu, e->bin_op.left);
			
			return (e->type = sema_expr(tu, e->bin_op.right));
		}
		case EXPR_DOT:
		case EXPR_ARROW: {
			uint32_t offset = 0;
			Member* m = sema_resolve_member_access(tu, e, &offset);
			if (m) {
				if (in_the_semantic_phase) {
					e->dot_arrow.member = m;
					e->dot_arrow.offset = offset;
				}
				
				return (e->type = m->type);
			}
			
			return (e->type = TYPE_VOID);
		}
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR: {
			sema_expr(tu, e->bin_op.left);
			sema_expr(tu, e->bin_op.right);
			
			e->bin_op.left->cast_type = TYPE_BOOL;
			e->bin_op.right->cast_type = TYPE_BOOL;
			
			return (e->type = TYPE_BOOL);
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
			TypeIndex lhs = sema_expr(tu, e->bin_op.left);
			TypeIndex rhs = sema_expr(tu, e->bin_op.right);
			
			if ((e->op == EXPR_PLUS ||
				 e->op == EXPR_MINUS) &&
				(tu->types[lhs].kind == KIND_PTR || 
				 tu->types[lhs].kind == KIND_ARRAY || 
				 tu->types[rhs].kind == KIND_PTR ||
				 tu->types[rhs].kind == KIND_ARRAY)) {
				// Pointer arithmatic
				if (e->op == EXPR_PLUS && (tu->types[rhs].kind == KIND_PTR || tu->types[rhs].kind == KIND_ARRAY)) {
					swap(lhs, rhs);
					swap(e->bin_op.left, e->bin_op.right);
				}
				
				if (tu->types[rhs].kind == KIND_PTR || tu->types[rhs].kind == KIND_ARRAY) {
					if (e->op == EXPR_MINUS) {
						// ptr - ptr = ptrdiff_t
						e->bin_op.left->cast_type = lhs;
						e->bin_op.right->cast_type = rhs;
						
						e->op = EXPR_PTRDIFF;
						return (e->type = TYPE_LONG);
					} else {
						sema_error(e->loc, "Cannot do pointer addition with two pointer operands, one must be an integral type.");
						return (e->type = TYPE_VOID);
					}
				} else {
					e->bin_op.left->cast_type = lhs;
					e->bin_op.right->cast_type = TYPE_ULONG;
					
					e->op = (e->op == EXPR_PLUS) ? EXPR_PTRADD : EXPR_PTRSUB;
					return (e->type = lhs);
				}
			} else {
				if (!(tu->types[lhs].kind >= KIND_BOOL && 
					  tu->types[lhs].kind <= KIND_DOUBLE && 
					  tu->types[rhs].kind >= KIND_BOOL && 
					  tu->types[rhs].kind <= KIND_DOUBLE)) {
					type_as_string(tu, sizeof(temp_string0), temp_string0, lhs);
					type_as_string(tu, sizeof(temp_string1), temp_string1, rhs);
					
					sema_error(e->loc, "Cannot apply binary operator to %s and %s.", temp_string0, temp_string1);
					return (e->type = TYPE_VOID);
				}
				
				TypeIndex type = get_common_type(tu, lhs, rhs);
				e->bin_op.left->cast_type = type;
				e->bin_op.right->cast_type = type;
				
				return (e->type = type);
			}
		}
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			TypeIndex type = get_common_type(tu,
											 sema_expr(tu, e->bin_op.left),
											 sema_expr(tu, e->bin_op.right));
			
			e->bin_op.left->cast_type = type;
			e->bin_op.right->cast_type = type;
			
			return (e->type = TYPE_BOOL);
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
			if (!is_assignable_expr(tu, e->bin_op.left)) {
				sema_error(e->bin_op.left->loc, "Left-hand side is not assignable");
				
				e->bin_op.left->cast_type = 0;
				e->bin_op.right->cast_type = 0;
				return (e->type = 0);
			}
			
			TypeIndex lhs = sema_expr(tu, e->bin_op.left);
			sema_expr(tu, e->bin_op.right);
			
			e->bin_op.left->cast_type = lhs;
			e->bin_op.right->cast_type = lhs;
			
			return (e->type = lhs);
		}
		default:
		break;
	}
	
	abort();
}

void sema_stmt(TranslationUnit* tu, Stmt* restrict s) {
	switch (s->op) {
		case STMT_NONE: break;
		case STMT_LABEL: break;
		case STMT_GOTO: {
			sema_expr(tu, s->goto_.target);
			break;
		}
		case STMT_COMPOUND: {
			Stmt** kids = s->compound.kids;
			size_t count = s->compound.kids_count;
			
			Stmt* killer = 0;
			for (size_t i = 0; i < count; i++) {
				Stmt* kid = kids[i];
				sema_stmt(tu, kid);
				
				if (killer) {
					if (kid->op == STMT_LABEL ||
						kid->op == STMT_CASE  ||
						kid->op == STMT_DEFAULT) {
						killer = 0;
					} else {
						sema_error(kid->loc, "Dead code");
						sema_info(killer->loc, "After");
						goto compound_failure;
					}
				} else {
					if (kid->op == STMT_RETURN ||
						kid->op == STMT_GOTO   ||
						kid->op == STMT_BREAK  ||
						kid->op == STMT_CONTINUE) {
						killer = kid;
					}
				}
			}
			
			compound_failure:
			break;
		}
		case STMT_DECL: {
			if (s->decl.initial) {
				try_resolve_typeof(tu, s->decl.type);
				
				if (s->decl.initial->op == EXPR_INITIALIZER &&
					s->decl.initial->init.type == 0) {
					// give it something to go off of
					s->decl.initial->init.type = s->decl.type;
				}
				
				TypeIndex expr_type_index = sema_expr(tu, s->decl.initial);
				
				Expr* restrict e = s->decl.initial;
				if (e->op == EXPR_INITIALIZER) {
					Type* restrict tp = &tu->types[s->decl.type];
					Type* restrict expr_type = &tu->types[s->decl.type];
					
					// Auto-detect array count from initializer
					if (tp->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
						if (tp->array_count != 0 && tp->array_count < expr_type->array_count) {
							sema_error(s->loc, "Array initializer does not fit into declaration (expected %d, got %d)", tp->array_count, expr_type->array_count);
						} else {
							s->decl.type = expr_type_index;
						}
					}
				} else if (e->op == EXPR_STR || e->op == EXPR_WSTR) {
					Type* restrict tp = &tu->types[s->decl.type];
					
					// Auto-detect array count from string
					if (tp->kind == KIND_ARRAY && tp->array_count == 0) {
						s->decl.type = expr_type_index;
					}
				}
				
				e->cast_type = s->decl.type;
				if (!type_compatible(tu, expr_type_index, s->decl.type, s->decl.initial)) {
					type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type_index);
					type_as_string(tu, sizeof(temp_string1), temp_string1, s->decl.type);
					
					sema_error(s->loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
				}
			}
			break;
		}
		case STMT_EXPR: {
			sema_expr(tu, s->expr.expr);
			break;
		}
		case STMT_RETURN: {
			if (s->return_.expr) {
				TypeIndex expr_type = sema_expr(tu, s->return_.expr);
				TypeIndex return_type = tu->types[function_stmt->decl.type].func.return_type;
				
				if (!type_compatible(tu, expr_type, return_type, s->return_.expr)) {
					//sema_warn(s->loc, "Value in return statement does not match function signature. (TODO this should be an error)");
				}
				
				s->return_.expr->cast_type = return_type;
			}
			break;
		}
		case STMT_IF: {
			TypeIndex cond_type = sema_expr(tu, s->if_.cond);
			if (!is_scalar_type(tu, cond_type)) {
				type_as_string(tu, sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(s->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			s->if_.cond->cast_type = TYPE_BOOL;
			
			sema_stmt(tu, s->if_.body);
			if (s->if_.next) sema_stmt(tu, s->if_.next);
			break;
		}
		case STMT_WHILE: {
			sema_expr(tu, s->while_.cond);
			if (s->while_.body) {
				sema_stmt(tu, s->while_.body);
			}
			break;
		}
		case STMT_DO_WHILE: {
			if (s->do_while.body) {
				sema_stmt(tu, s->do_while.body);
			}
			sema_expr(tu, s->do_while.cond);
			break;
		}
		case STMT_FOR: {
			if (s->for_.first) {
				sema_stmt(tu, s->for_.first);
			}
			
			if (s->for_.cond) {
				sema_expr(tu, s->for_.cond);
			}
			
			if (s->for_.body) {
				sema_stmt(tu, s->for_.body);
			}
			
			if (s->for_.next) {
				sema_expr(tu, s->for_.next);
			}
			break;
		}
		case STMT_SWITCH: {
			sema_expr(tu, s->switch_.condition);
			sema_stmt(tu, s->switch_.body);
			break;
		}
		case STMT_CASE: {
			sema_stmt(tu, s->case_.body);
			break;
		}
		case STMT_DEFAULT: {
			sema_stmt(tu, s->default_.body);
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

TypeIndex sema_guess_type(TranslationUnit* tu, Stmt* restrict s) {
	char* name = (char*) s->decl.name;
	
	TypeIndex type_index = s->decl.type;
	Type* type = &tu->types[type_index];
	
	if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
		sema_error(s->loc, "Global declaration '%s' cannot be both static and extern.", name);
		s->backing.g = 0;
		return TYPE_NONE;
	}
	
	if (type->is_incomplete) {
		if (type->kind == KIND_STRUCT) {
			sema_error(s->loc, "Incomplete type (struct %s) in declaration", type->record.name);
		} else if (type->kind == KIND_UNION) {
			sema_error(s->loc, "Incomplete type (union %s) in declaration", type->record.name);
		} else {
			sema_error(s->loc, "Incomplete type in declaration");
		}
	}
	
	if (s->decl.attrs.is_extern || type->kind == KIND_FUNC) {
		return TYPE_NONE;
	}
	
	if (s->decl.initial) {
		Expr* e = s->decl.initial;
		
		if (type->kind == KIND_ARRAY && e->op == EXPR_INITIALIZER) {
			// check how many top level statements we have
			int array_count;
			sema_infer_initializer_array_count(tu, e->init.count, e->init.nodes, 0, &array_count);
			
			return new_array(tu, type->array_of, array_count);
		}
	}
	
	return s->decl.type;
}

static void sema_top_level(TranslationUnit* tu, Stmt* restrict s, bool frontend_only) {
	TypeIndex type_index = s->decl.type;
	Type* type = &tu->types[type_index];
	
	char* name = (char*) s->decl.name;
	switch (s->op) {
		case STMT_FUNC_DECL: {
			assert(type->kind == KIND_FUNC);
			
			if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
				sema_error(s->loc, "Function '%s' cannot be both static and extern.", name);
				s->backing.f = 0;
				break;
			}
			
			if (s->decl.attrs.is_static && !s->decl.attrs.is_inline) {
				if (!s->decl.attrs.is_used) {
					sema_warn(s->loc, "Function '%s' is never used.", name);
					s->backing.f = 0;
					break;
				}
			}
			
			if (s->decl.attrs.is_inline && !s->decl.attrs.is_used) {
				s->backing.f = 0;
				break;
			}
			
			if (frontend_only) {
				s->backing.f = 0;
				
				// type check function body
				function_stmt = s;
				sema_stmt(tu, s->decl.initial_as_stmt);
				function_stmt = 0;
				break;
			}
			
			TB_FunctionPrototype* proto = target_desc.create_prototype(tu, type_index);
			TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
			
			// TODO(NeGate): Fix this up because it's possibly wrong, essentially
			// inline linkage means all the definitions must match which isn't
			// necessarily the same as static where they all can share a name but
			// are different and internal.
			TB_Function* func;
			if (s->decl.attrs.is_inline) {
				linkage = TB_LINKAGE_PRIVATE;
				
				char temp[1024];
				snprintf(temp, 1024, "_K%p_%s", s, name ? name : "<unnamed>");
				
				func = tb_prototype_build(mod, proto, temp, linkage);
			} else {
				func = tb_prototype_build(mod, proto, name, linkage);
			}
			s->backing.f = tb_function_get_id(mod, func);
			
			// type check function body
			function_stmt = s;
			sema_stmt(tu, s->decl.initial_as_stmt);
			function_stmt = 0;
			break;
		}
		case STMT_DECL:
		case STMT_GLOBAL_DECL: {
			if (!s->decl.attrs.is_used) break;
			if (s->decl.attrs.is_typedef) break;
			
			if (s->decl.attrs.is_static && s->decl.attrs.is_extern) {
				sema_error(s->loc, "Global declaration '%s' cannot be both static and extern.", name);
				s->backing.g = 0;
				break;
			}
			
			if (!s->decl.attrs.is_extern && type->kind != KIND_FUNC) {
				Type* expr_type = NULL;
				
				if (s->decl.initial) {
					if (s->decl.initial->op == EXPR_INITIALIZER &&
						s->decl.initial->init.type == 0) {
						// give it something to go off of
						//
						// doesn't have to be complete in terms of array count
						// just enough to infer the rest in a sec
						s->decl.initial->init.type = s->decl.type;
					}
					
					TypeIndex expr_type_index = sema_expr(tu, s->decl.initial);
					expr_type = &tu->types[expr_type_index];
					
					if (s->decl.initial->op == EXPR_INITIALIZER ||
						s->decl.initial->op == EXPR_STR ||
						s->decl.initial->op == EXPR_WSTR) {
						if (type->kind == KIND_ARRAY && expr_type->kind == KIND_ARRAY) {
							if (type_equal(tu, type->array_of, expr_type->array_of)) {
								if (type->array_count != 0 && type->array_count < expr_type->array_count) {
									sema_error(s->loc, "Array initializer does not fit into declaration (expected %d, got %d)", type->array_count, expr_type->array_count);
								} else {
									assert(expr_type->array_count);
									
									// preserve constness
									bool is_const = type->is_const;
									
									type_index = copy_type(tu, expr_type_index);
									type = &tu->types[type_index];
									type->is_const = is_const;
									
									s->decl.type = type_index;
								}
							} else {
								type_as_string(tu, sizeof(temp_string0), temp_string0, expr_type->array_of);
								type_as_string(tu, sizeof(temp_string1), temp_string1, type->array_of);
								
								sema_error(s->loc, "Array initializer type mismatch (got '%s', expected '%s')", temp_string0, temp_string1);
							}
						}
					}
					
					if (!type_compatible(tu, expr_type_index, type_index, s->decl.initial)) {
						type_as_string(tu, sizeof(temp_string0), temp_string0, type_index);
						type_as_string(tu, sizeof(temp_string1), temp_string1, expr_type_index);
						
						sema_error(s->loc, "Declaration type does not match (got '%s', expected '%s')", temp_string0, temp_string1);
					}
				}
				
				if (type->is_incomplete) {
					if (type->kind == KIND_STRUCT) {
						sema_error(s->loc, "Incomplete type (struct %s) in declaration", type->record.name);
					} else if (type->kind == KIND_UNION) {
						sema_error(s->loc, "Incomplete type (union %s) in declaration", type->record.name);
					} else {
						sema_error(s->loc, "Incomplete type in declaration");
					}
				}
				
				if (frontend_only) {
					s->backing.g = 0;
				} else { 
					if (s->decl.attrs.is_tls && !atomic_flag_test_and_set(&irgen_defined_tls_index)) {
						tb_module_set_tls_index(mod, tb_extern_create(mod, "_tls_index"));
					}
					
					TB_Linkage linkage = s->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
					s->backing.g = tb_global_create(mod, name, s->decl.attrs.is_tls ? TB_STORAGE_TLS : TB_STORAGE_DATA, linkage);
				}
			}
			break;
		}
		default: assert(0);
	}
}

static void sema_mark_children(TranslationUnit* tu, Expr* restrict e) {
	if (e->op == EXPR_ENUM) return;
	if (e->op == EXPR_UNKNOWN_SYMBOL) return;
	
	assert(e->op == EXPR_SYMBOL);
	Stmt* restrict s = e->symbol;
	
	if (s->op == STMT_FUNC_DECL ||
		s->op == STMT_DECL ||
		s->op == STMT_GLOBAL_DECL) {
		if (!s->decl.attrs.is_used) {
			s->decl.attrs.is_used = true;
			Expr* sym = s->decl.first_symbol;
			
			while (sym) {
				sema_mark_children(tu, sym);
				sym = sym->next_symbol_in_chain;
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
			Stmt* restrict s = tu->top_level_stmts[i];
			assert(s->op == STMT_FUNC_DECL || s->op == STMT_DECL || s->op == STMT_GLOBAL_DECL);
			
			if (s->decl.attrs.is_root) {
				s->decl.attrs.is_used = true;
				
				Expr* sym = s->decl.first_symbol;
				while (sym) {
					sema_mark_children(tu, sym);
					sym = sym->next_symbol_in_chain;
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
