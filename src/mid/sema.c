#include "sema.h"
#include "settings.h"

#include <back/ir_gen.h>
#include <stdarg.h>

_Atomic int sema_error_count;
_Thread_local StmtIndex function_stmt;

// two simple temporary buffers to represent type_as_string results
_Thread_local char temp_string0[256], temp_string1[256];

static size_t type_as_string(size_t max_len, char buffer[static max_len], TypeIndex type_index) {
	Type* restrict type = &type_arena.data[type_index];
	
	size_t i = 0;
	switch (type->kind) {
		case KIND_VOID:  i += snprintf(&buffer[i], max_len - i, "void"); break;
		case KIND_BOOL:  i += snprintf(&buffer[i], max_len - i, "_Bool"); break;
		case KIND_CHAR:  i += snprintf(&buffer[i], max_len - i, "char"); break;
		case KIND_SHORT: i += snprintf(&buffer[i], max_len - i, "short"); break;
		case KIND_INT:   i += snprintf(&buffer[i], max_len - i, "int"); break;
		case KIND_LONG:  i += snprintf(&buffer[i], max_len - i, "long"); break;
		case KIND_FLOAT: i += snprintf(&buffer[i], max_len - i, "float"); break;
		case KIND_DOUBLE:i += snprintf(&buffer[i], max_len - i, "double"); break;
		case KIND_ENUM: {
			i += cstr_copy(max_len - i, &buffer[i], "enum ");
			
			if (type->enumerator.name) {
				i += cstr_copy(max_len - i, &buffer[i], (char*)type->enumerator.name);
			} else {
				i += cstr_copy(max_len - i, &buffer[i], "__unnamed__");
			}
			break;
		}
		case KIND_UNION: {
			i += cstr_copy(max_len - i, &buffer[i], "union ");
			
			if (type->record.name) {
				i += cstr_copy(max_len - i, &buffer[i], (char*)type->record.name);
			} else {
				i += cstr_copy(max_len - i, &buffer[i], "__unnamed__");
			}
			break;
		}
		case KIND_STRUCT: {
			i += cstr_copy(max_len - i, &buffer[i], "struct ");
			
			if (type->record.name) {
				i += cstr_copy(max_len - i, &buffer[i], (char*)type->record.name);
			} else {
				i += cstr_copy(max_len - i, &buffer[i], "__unnamed__");
			}
			break;
		}
		case KIND_PTR: {
			i += type_as_string(max_len - i, &buffer[i], type->ptr_to);
			buffer[i++] = '*';
			break;
		}
		case KIND_ARRAY: {
			i += type_as_string(max_len - i, &buffer[i], type->array_of);
			
			if (i+12 < max_len) {
				buffer[i++] = '[';
				
				i += sprintf_s(&buffer[i], max_len - i, "%zu", type->array_count);
				
				buffer[i++] = ']';
			} else {
				abort();
			}
			break;
		}
		case KIND_FUNC: {
			ParamIndex param_list = type->func.param_list;
			ParamIndex param_count = type->func.param_count;
			
			i += type_as_string(max_len - i, &buffer[i], type->func.return_type);
			if (type->func.name) {
				buffer[i++] = ' ';
				i += cstr_copy(max_len - i, &buffer[i], (char*)type->func.name);
			}
			
			buffer[i++] = '(';
			for (size_t i = 0; i < param_count; i++) {
				if (i) buffer[i++] = ',';
				
				Param* p = &param_arena.data[param_list + i];
				
				i += type_as_string(max_len - i, &buffer[i], p->type);
				if (p->name) {
					buffer[i++] = ' ';
					i += cstr_copy(max_len - i, &buffer[i], (char*)p->name);
				}
			}
			
			buffer[i++] = ')';
			break;
		}
		default: abort();
	}
	
	buffer[i] = '\0';
	return i;
}

static void sema_error(SourceLocIndex loc, const char* fmt, ...) {
	SourceLoc* l = &ir_gen_tokens.line_arena[loc];
	printf("%s:%d: error: ", l->file, l->line);
	
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
	
	printf("\n");
	
	if (++sema_error_count == 20) {
		printf("Too many errors!\n");
		abort();
	}
}

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

static bool is_scalar_type(TypeIndex type_index) {
	Type* restrict type = &type_arena.data[type_index];
	return (type->kind >= KIND_BOOL && type->kind <= KIND_FUNC);
}

// Also checks if expression is an integer literal because we
// have a special case for 0 to pointer conversions.
static bool type_compatible(TypeIndex a, TypeIndex b, ExprIndex a_expr) {
	if (a == b) return true;
	
	Type* types = type_arena.data;
	Type* restrict src = &types[a];
	Type* restrict dst = &types[b];
	
	// implictly convert arrays into pointers
	if (src->kind == KIND_ARRAY) {
		TypeIndex as_ptr = new_pointer_locked(src->array_of);
		src = &types[as_ptr];
	}
	
	if (dst->kind == KIND_ARRAY) {
		TypeIndex as_ptr = new_pointer_locked(dst->array_of);
		dst = &types[as_ptr];
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
			else if (expr_arena.data[a_expr].op == EXPR_INT) {
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
			bool is_nullptr = expr_arena.data[a_expr].op == EXPR_INT &&
				expr_arena.data[a_expr].int_num.num == 0;
			
			if (is_nullptr) {
				return true;
			}
		} else if (src->kind == KIND_FLOAT ||
				   dst->kind == KIND_DOUBLE) {
			return true;
		} else if (src->kind == KIND_DOUBLE ||
				   dst->kind == KIND_FLOAT) {
			return true;
		}
		
		return false;
	}
	
	if (src->kind == KIND_FUNC) {
		return type_equal(a, b);
	} else if (dst->kind == KIND_PTR) {
		// void* -> T* is fine
		if (type_arena.data[src->ptr_to].kind == KIND_VOID) {
			return true;
		}
		
		// T* -> void* is fine
		if (type_arena.data[dst->ptr_to].kind == KIND_VOID) {
			return true;
		}
		
		return type_equal(src->ptr_to, dst->ptr_to);
	}
	
	// but by default kind matching is enough
	// like for integers, booleans and floats
	return true;
}

static TypeIndex sema_expr(ExprIndex e) {
	Expr* restrict ep = &expr_arena.data[e];
	
	switch (ep->op) {
		case EXPR_INT: {
			switch (ep->int_num.suffix) {
				case INT_SUFFIX_NONE: {
					int original = (int)ep->int_num.num;
					long long expected = (long long)ep->int_num.num;
					
					if (original != expected) {
						sema_error(ep->loc, "Could not represent integer literal as int.");
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
				case INT_SUFFIX_LL:
				return (ep->type = TYPE_LONG);
				
				case INT_SUFFIX_UL: 
				case INT_SUFFIX_ULL: 
				return (ep->type = TYPE_ULONG);
			}
		}
		case EXPR_FLOAT: {
			return (ep->type = TYPE_DOUBLE);
		}
		case EXPR_CHAR: {
			return (ep->type = TYPE_CHAR);
		}
		case EXPR_STR: {
			bool is_wide_string = ep->str.start[0] == 'L';
			
			return (ep->type = (is_wide_string ? TYPE_WSTRING : TYPE_STRING));
		}
		case EXPR_SIZEOF: {
			TypeIndex src = sema_expr(ep->x_of_expr.expr);
			
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { type_arena.data[src].size, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF: {
			TypeIndex src = sema_expr(ep->x_of_expr.expr);
			
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { type_arena.data[src].align, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_SIZEOF_T: {
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { type_arena.data[ep->x_of_type.type].size, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_ALIGNOF_T: {
			*ep = (Expr) {
				.op = EXPR_INT,
				.type = TYPE_ULONG,
				.int_num = { type_arena.data[ep->x_of_type.type].align, INT_SUFFIX_NONE }
			};
			return (ep->type = TYPE_ULONG);
		}
		case EXPR_INITIALIZER: {
			for (size_t i = 0; i < ep->init.count; i++) {
				if (ep->init.nodes[i].kids_count == 0) sema_expr(ep->init.nodes[i].expr);
			}
			
			return (ep->type = ep->init.type);
		}
		case EXPR_NOT:
		case EXPR_NEGATE:
		case EXPR_PRE_INC:
		case EXPR_PRE_DEC:
		case EXPR_POST_INC:
		case EXPR_POST_DEC: {
			TypeIndex src = sema_expr(ep->unary_op.src);
			
			return (ep->type = src);
		}
		case EXPR_ADDR: {
			TypeIndex src = sema_expr(ep->unary_op.src);
			return (ep->type = new_pointer_locked(src));
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			
			return (ep->type = stmt_arena.data[stmt].decl.type);
		}
		case EXPR_PARAM: {
			int param_num = ep->param_num;
			
			Type* func_type = &type_arena.data[stmt_arena.data[function_stmt].decl.type];
			Param* params = &param_arena.data[func_type->func.param_list];
			return (ep->type = params[param_num].type);
		}
		case EXPR_CAST: {
			/* TypeIndex src = */ sema_expr(ep->cast.src);
			
			// set child's cast type
			expr_arena.data[ep->cast.src].cast_type = ep->cast.type;
			return (ep->type = ep->cast.type);
		}
		case EXPR_SUBSCRIPT: {
			TypeIndex base = sema_expr(ep->subscript.base);
			TypeIndex index = sema_expr(ep->subscript.index);
			
			if (type_arena.data[index].kind == KIND_PTR ||
				type_arena.data[index].kind == KIND_ARRAY) {
				swap(base, index);
				swap(ep->subscript.base, ep->subscript.index);
			}
			
			if (type_arena.data[base].kind == KIND_ARRAY) {
				base = new_pointer_locked(type_arena.data[base].array_of);
			}
			
			return (ep->type = type_arena.data[base].ptr_to);
		}
		case EXPR_DEREF: {
			TypeIndex base = sema_expr(ep->unary_op.src);
			Type* type = &type_arena.data[base];
			
			if (type->kind == KIND_PTR) {
				return (ep->type = type->ptr_to);
			} else if (type->kind == KIND_ARRAY) {
				return (ep->type = type->array_of);
			} else {
				sema_fatal(ep->loc, "TODO");
			}
		}
		case EXPR_CALL: {
			// Call function
			TypeIndex func_ptr = sema_expr(ep->call.target);
			
			// implicit dereference
			if (type_arena.data[func_ptr].kind == KIND_PTR) {
				func_ptr = type_arena.data[func_ptr].ptr_to;
			}
			
			Type* func_type = &type_arena.data[func_ptr];
			if (func_type->kind != KIND_FUNC) {
				sema_error(ep->loc, "function call target must be a function-type.");
				goto failure;
			}
			
			ExprIndex* args = ep->call.param_start;
			int arg_count = ep->call.param_count;
			
			Param* params = &param_arena.data[func_type->func.param_list];
			int param_count = func_type->func.param_count;
			
			if (func_type->func.has_varargs) {
				if (arg_count < param_count) {
					sema_error(ep->loc, "Not enough arguments (expected at least %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				// type-check the parameters with a known type
				for (size_t i = 0; i < param_count; i++) {
					TypeIndex arg_type = sema_expr(args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(arg_type, params[i].type, args[i])) {
						type_as_string(sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = expr_arena.data[args[i]].loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					expr_arena.data[args[i]].cast_type = params[i].type;
				}
				
				// type-check the untyped arguments
				for (size_t i = param_count; i < arg_count; i++) {
					sema_expr(args[i]);
					
					expr_arena.data[args[i]].cast_type = expr_arena.data[args[i]].type;
				}
			} else {
				if (arg_count != param_count) {
					sema_error(ep->loc, "Argument count mismatch (expected %d, got %d)", param_count, arg_count);
					goto failure;
				}
				
				// type-check the untyped arguments
				for (size_t i = 0; i < arg_count; i++) {
					TypeIndex arg_type = sema_expr(args[i]);
					
					// TODO(NeGate): Error messages
					if (!type_compatible(arg_type, params[i].type, args[i])) {
						type_as_string(sizeof(temp_string0), temp_string0, arg_type);
						type_as_string(sizeof(temp_string1), temp_string1, params[i].type);
						
						SourceLocIndex loc = expr_arena.data[args[i]].loc;
						sema_error(loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
						goto failure;
					}
					
					expr_arena.data[args[i]].cast_type = expr_arena.data[args[i]].type;
				}
			}
			
			failure:
			// reload because of possible clobbering of type_arena :p
			func_type = &type_arena.data[func_ptr];
			
			return (ep->type = func_type->func.return_type);
		}
		case EXPR_TERNARY: {
			TypeIndex cond_type = sema_expr(ep->ternary_op.left);
			if (!is_scalar_type(cond_type)) {
				type_as_string(sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(ep->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			expr_arena.data[ep->ternary_op.left].cast_type = TYPE_BOOL;
			
			TypeIndex type = get_common_type(sema_expr(ep->ternary_op.middle),
											 sema_expr(ep->ternary_op.right));
			
			expr_arena.data[ep->ternary_op.middle].cast_type = type;
			expr_arena.data[ep->ternary_op.right].cast_type = type;
			
			return (ep->type = type);
		}
		case EXPR_COMMA: {
			sema_expr(ep->bin_op.left);
			
			return (ep->type = sema_expr(ep->bin_op.right));
		}
		case EXPR_DOT: {
			TypeIndex base_type = sema_expr(ep->dot.base);
			Type* restrict record_type = &type_arena.data[base_type];
			
			// Implicit dereference
			if (record_type->kind == KIND_PTR) {
				record_type = &type_arena.data[record_type->ptr_to];
				
				if (settings.pedantic) {
					sema_error(ep->loc, "Implicit dereference is a non-standard extension (disable -P to allow it).");
					return (ep->type = TYPE_VOID);
				}
			}
			
			if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
				sema_error(ep->loc, "Cannot get the member of a non-record type.");
				return (ep->type = TYPE_VOID);
			}
			
			Atom name = ep->dot.name;
			
			MemberIndex start = record_type->record.kids_start;
			MemberIndex end = record_type->record.kids_end;
			for (MemberIndex m = start; m < end; m++) {
				Member* member = &member_arena.data[m];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(name, member->name)) {
					// TODO(NeGate): Implement bitfields
					assert(!member->is_bitfield);
					return (ep->type = member->type);
				}
			}
			
			sema_error(ep->loc, "Could not find member under that name.");
			return (ep->type = TYPE_VOID);
		}
		case EXPR_ARROW: {
			TypeIndex base_type = sema_expr(ep->arrow.base);
			
			Type* restrict ptr_type = &type_arena.data[base_type];
			if (ptr_type->kind != KIND_PTR && ptr_type->kind != KIND_ARRAY) {
				sema_error(ep->loc, "Cannot do arrow operator on non-pointer type.");
				return (ep->type = TYPE_VOID);
			}
			
			Type* restrict record_type = &type_arena.data[ptr_type->ptr_to];
			if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
				sema_error(ep->loc, "Cannot get the member of a non-record type.");
				return (ep->type = TYPE_VOID);
			}
			
			Atom name = ep->arrow.name;
			
			MemberIndex start = record_type->record.kids_start;
			MemberIndex end = record_type->record.kids_end;
			for (MemberIndex m = start; m < end; m++) {
				Member* member = &member_arena.data[m];
				
				// TODO(NeGate): String interning would be nice
				if (cstr_equals(name, member->name)) {
					// TODO(NeGate): Implement bitfields
					assert(!member->is_bitfield);
					return (ep->type = member->type);
				}
			}
			
			sema_error(ep->loc, "Could not find member under that name.");
			return (ep->type = TYPE_VOID);
		}
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR: {
			sema_expr(ep->bin_op.left);
			sema_expr(ep->bin_op.right);
			
			expr_arena.data[ep->bin_op.left].cast_type = TYPE_BOOL;
			expr_arena.data[ep->bin_op.right].cast_type = TYPE_BOOL;
			
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
			TypeIndex lhs = sema_expr(ep->bin_op.left);
			TypeIndex rhs = sema_expr(ep->bin_op.right);
			
			if ((ep->op == EXPR_PLUS ||
				 ep->op == EXPR_MINUS) &&
				(type_arena.data[lhs].kind == KIND_PTR || 
				 type_arena.data[rhs].kind == KIND_PTR)) {
				// Pointer arithmatic
				if (type_arena.data[rhs].kind == KIND_PTR || type_arena.data[rhs].kind == KIND_ARRAY) {
					swap(lhs, rhs);
					swap(ep->bin_op.left, ep->bin_op.right);
				}
				
				if (type_arena.data[rhs].kind == KIND_PTR || type_arena.data[rhs].kind == KIND_ARRAY) {
					if (ep->op == EXPR_MINUS) {
						// ptr - ptr = ptrdiff_t
						expr_arena.data[ep->bin_op.left].cast_type = lhs;
						expr_arena.data[ep->bin_op.right].cast_type = rhs;
						
						return (ep->type = TYPE_LONG);
					} else {
						sema_error(ep->loc, "Cannot do pointer addition with two pointer operands, one must be an integral type.");
						return (ep->type = TYPE_VOID);
					}
				} else {
					expr_arena.data[ep->bin_op.left].cast_type = lhs;
					expr_arena.data[ep->bin_op.right].cast_type = TYPE_ULONG;
					
					return (ep->type = lhs);
				}
			} else {
				TypeIndex type = get_common_type(lhs, rhs);
				expr_arena.data[ep->bin_op.left].cast_type = type;
				expr_arena.data[ep->bin_op.right].cast_type = type;
				
				return (ep->type = type);
			}
		}
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE: {
			TypeIndex type = get_common_type(sema_expr(ep->bin_op.left),
											 sema_expr(ep->bin_op.right));
			
			expr_arena.data[ep->bin_op.left].cast_type = type;
			expr_arena.data[ep->bin_op.right].cast_type = type;
			
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
			TypeIndex type = get_common_type(sema_expr(ep->bin_op.left),
											 sema_expr(ep->bin_op.right));
			
			expr_arena.data[ep->bin_op.left].cast_type = type;
			expr_arena.data[ep->bin_op.right].cast_type = type;
			
			return (ep->type = type);
		}
		default:
		break;
	}
	
	abort();
}

void sema_stmt(StmtIndex s) {
	Stmt* restrict sp = &stmt_arena.data[s];
	
	switch (sp->op) {
		case STMT_NONE:
		case STMT_LABEL: {
			break;
		}
		case STMT_GOTO: {
			sema_expr(sp->goto_.target);
			break;
		}
		case STMT_COMPOUND: {
			StmtIndex* kids = sp->compound.kids;
			size_t count = sp->compound.kids_count;
			
			for (size_t i = 0; i < count; i++) {
				sema_stmt(kids[i]);
			}
			break;
		}
		case STMT_DECL: {
			if (sp->decl.initial) {
				if (expr_arena.data[sp->decl.initial].op == EXPR_INITIALIZER) {
					expr_arena.data[sp->decl.initial].init.type = sp->decl.type;
				}
				
				TypeIndex expr_type = sema_expr(sp->decl.initial);
				
				if (!type_compatible(expr_type, sp->decl.type, sp->decl.initial)) {
					type_as_string(sizeof(temp_string0), temp_string0, expr_type);
					type_as_string(sizeof(temp_string1), temp_string1, sp->decl.type);
					
					sema_error(sp->loc, "Could not implicitly convert type %s into %s.", temp_string0, temp_string1);
				}
			}
			break;
		}
		case STMT_EXPR: {
			sema_expr(sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			if (sp->return_.expr) {
				TypeIndex expr_type = sema_expr(sp->return_.expr);
				TypeIndex return_type = type_arena.data[stmt_arena.data[function_stmt].decl.type].func.return_type;
				
				if (!type_compatible(expr_type, return_type, sp->return_.expr)) {
					sema_error(sp->loc, "Value in return statement does not match function signature.");
				}
				
				expr_arena.data[sp->return_.expr].cast_type = return_type;
			}
			break;
		}
		case STMT_IF: {
			TypeIndex cond_type = sema_expr(sp->if_.cond);
			if (!is_scalar_type(cond_type)) {
				type_as_string(sizeof(temp_string0), temp_string0, cond_type);
				
				sema_error(sp->loc, "Could not convert type %s into boolean.", temp_string0);
			}
			expr_arena.data[sp->if_.cond].cast_type = TYPE_BOOL;
			
			sema_stmt(sp->if_.body);
			if (sp->if_.next) sema_stmt(sp->if_.next);
			break;
		}
		case STMT_WHILE: {
			sema_expr(sp->while_.cond);
			sema_stmt(sp->while_.body);
			break;
		}
		case STMT_DO_WHILE: {
			sema_stmt(sp->do_while.body);
			sema_expr(sp->do_while.cond);
			break;
		}
		case STMT_FOR: {
			sema_stmt(sp->for_.first);
			if (sp->for_.cond) {
				sema_expr(sp->for_.cond);
			}
			
			sema_stmt(sp->for_.body);
			if (sp->for_.next) {
				sema_expr(sp->for_.next);
			}
			break;
		}
		case STMT_SWITCH: {
			sema_expr(sp->switch_.condition);
			break;
		}
		case STMT_CASE: {
			sema_stmt(sp->case_.body);
			break;
		}
		case STMT_DEFAULT: {
			sema_stmt(sp->default_.body);
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

void sema_check(TopLevel tl, size_t i) {
	StmtIndex s = tl.arr[i];
	Stmt* restrict sp = &stmt_arena.data[s];
	
	TypeIndex type_index = sp->decl.type;
	Type* restrict type = &type_arena.data[type_index];
	
	char* name = (char*) sp->decl.name;
	switch (sp->op) {
		case STMT_FUNC_DECL: {
			assert(type->kind == KIND_FUNC);
			const Type* return_type = &type_arena.data[type->func.return_type];
			
			if (sp->decl.attrs.is_static && sp->decl.attrs.is_extern) {
				sema_error(sp->loc, "Function '%s' cannot be both static and extern.", name);
				sp->backing.g = 0;
				break;
			}
			
			bool is_aggregate_return = false;
			if (return_type->kind == KIND_STRUCT ||
				return_type->kind == KIND_UNION) {
				is_aggregate_return = true;
			}
			
			// parameters
			ParamIndex param_list = type->func.param_list;
			ParamIndex param_count = type->func.param_count;
			
			// aggregate return values take up the first parameter slot.
			ParamIndex real_param_count = param_count + is_aggregate_return;
			
			TB_DataType return_dt = ctype_to_tbtype(return_type);
			TB_FunctionPrototype* proto = tb_prototype_create(mod, TB_STDCALL, return_dt, real_param_count, false);
			
			if (is_aggregate_return) {
				tb_prototype_add_param(proto, TB_TYPE_PTR);
			}
			
			for (size_t i = 0; i < param_count; i++) {
				Param* p = &param_arena.data[param_list + i];
				
				// Decide on the data type
				Type* param_type = &type_arena.data[p->type];
				TB_DataType dt = ctype_to_tbtype(param_type);
				
				tb_prototype_add_param(proto, dt);
			}
			
			TB_Linkage linkage = sp->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
			
			// TODO(NeGate): Fix this up because it's possibly wrong, essentially
			// inline linkage means all the definitions must match which isn't necessarily
			// the same as static where they all can share a name but are different and
			// internal.
			if (sp->decl.attrs.is_inline) linkage = TB_LINKAGE_PRIVATE;
			
			TB_Function* func = tb_prototype_build(mod, proto, name, linkage);
			sp->backing.f = tb_function_get_id(mod, func);
			
			// type check function body
			function_stmt = s;
			sema_stmt((StmtIndex)stmt_arena.data[s].decl.initial);
			function_stmt = 0;
			break;
		}
		case STMT_DECL: {
			if (!sp->decl.attrs.is_used) return;
			
			// forward decls
			sp->backing.e = tb_extern_create(mod, name);
			break;
		}
		case STMT_GLOBAL_DECL: {
			if (!sp->decl.attrs.is_used) return;
			
			if (sp->decl.attrs.is_static && sp->decl.attrs.is_extern) {
				sema_error(sp->loc, "Global declaration '%s' cannot be both static and extern.", name);
				sp->backing.g = 0;
				break;
			}
			
			// TODO(NeGate): Implement real global initializers
			TB_InitializerID init = tb_initializer_create(mod, type->size, type->align, 0);
			
			TB_Linkage linkage = sp->decl.attrs.is_static ? TB_LINKAGE_PRIVATE : TB_LINKAGE_PUBLIC;
			sp->backing.g = tb_global_create(mod, init, name, linkage);
			break;
		}
		default: assert(0);
	}
}
