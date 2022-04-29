#include "parser.h"

#define unsigned_const(x) (ConstValue){ false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue){ true, .signed_value = (x) }

// Const eval probably needs some rework...
TypeIndex sema_expr(TranslationUnit* tu, ExprIndex e);
TypeIndex sema_guess_type(TranslationUnit* tu, Stmt* restrict s);
Member* sema_traverse_members(TranslationUnit* tu, Type* record_type, Atom name, uint32_t* out_offset);

// I've forsaken god by doing this, im sorry... it's ugly because it has to be i swear...
typedef struct { TypeIndex type; uint32_t offset; } WalkMemberReturn;

// just returns the byte offset it traveled in these DOTs and ARROWs.
// the type we pass in is that of whatever's at the end of the DOT and ARROW chain
static WalkMemberReturn walk_member_accesses(TranslationUnit* tu, ExprIndex e, TypeIndex type) {
	const Expr* ep = &tu->exprs[e];
	const Expr* base_expr = &tu->exprs[ep->dot_arrow.base];
	
	WalkMemberReturn base = { 0 };
	if (base_expr->op != EXPR_ARROW && base_expr->op != EXPR_DOT) {
		// use that base type we've been patiently keeping around
		base = (WalkMemberReturn) { type, 0 };
	} else {
		base = walk_member_accesses(tu, ep->dot_arrow.base, type);
	}
	
	uint32_t relative = 0;
	Member* member = sema_traverse_members(tu, &tu->types[base.type], ep->dot_arrow.name, &relative);
	if (!member) abort();
	if (member->is_bitfield) abort();
	
	return (WalkMemberReturn){ member->type, base.offset + relative };
}

bool const_eval_try_offsetof_hack(TranslationUnit* tu, ExprIndex e, uint64_t* out) {
	const Expr* ep = &tu->exprs[e];
	
	// hacky but handles things like: 
	//   &(((T*)0)->apple)
	//   sizeof(((T*)0).apple)
	if (ep->op == EXPR_DOT || ep->op == EXPR_ARROW) {
		ExprIndex base_e = ep->dot_arrow.base;
		
		while (tu->exprs[base_e].op == EXPR_ARROW ||
			   tu->exprs[base_e].op == EXPR_DOT) {
			// traverse any dot/arrow chains
			base_e = tu->exprs[base_e].dot_arrow.base;
		}
		
		uint32_t offset = 0;
		Type* record_type = NULL;
		Expr* arrow_base = &tu->exprs[base_e];
		if (arrow_base->op == EXPR_CAST) {
			record_type = &tu->types[arrow_base->cast.type];
			
			// dereference
			if (record_type->kind == KIND_PTR) {
				record_type = &tu->types[record_type->ptr_to];
			} else {
				abort();
			}
			
			if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
				return false;
			}
			
			ConstValue pointer_value = const_eval(tu, arrow_base->cast.src);
			assert(pointer_value.unsigned_value < UINT32_MAX);
			offset += pointer_value.unsigned_value;
		} else {
			return false;
		}
		
		offset += walk_member_accesses(tu, e, record_type - tu->types).offset; 
		*out = offset;
		return true;
	}
	
	return false;
}

static ConstValue const_eval_bin_op(ExprOp op, ConstValue a, ConstValue b) {
	bool is_signed = a.is_signed | b.is_signed;
	
	switch (op) {
		case EXPR_PLUS: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value + b.unsigned_value };
		case EXPR_MINUS: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value - b.unsigned_value };
		case EXPR_TIMES: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value * b.unsigned_value };
		case EXPR_SLASH: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value / b.unsigned_value };
		case EXPR_AND: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value & b.unsigned_value };
		case EXPR_OR: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value | b.unsigned_value };
		
		case EXPR_SHL: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value << b.unsigned_value };
		case EXPR_SHR: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value >> b.unsigned_value };
		
		case EXPR_CMPEQ: return unsigned_const(a.unsigned_value == b.unsigned_value);
		case EXPR_CMPNE: return unsigned_const(a.unsigned_value == b.unsigned_value);
		
		case EXPR_CMPGE: 
		if (is_signed) return unsigned_const(a.signed_value >= b.signed_value);
		else return unsigned_const(a.unsigned_value >= b.unsigned_value); 
		
		case EXPR_CMPLE: 
		if (is_signed) return unsigned_const(a.signed_value <= b.signed_value);
		else return unsigned_const(a.unsigned_value <= b.unsigned_value);
		
		case EXPR_CMPGT: 
		if (is_signed) return unsigned_const(a.signed_value > b.signed_value);
		else return unsigned_const(a.unsigned_value > b.unsigned_value);
		
		case EXPR_CMPLT: 
		if (is_signed) return unsigned_const(a.signed_value < b.signed_value);
		else return unsigned_const(a.unsigned_value < b.unsigned_value);
		
		default: __builtin_unreachable();
	}
}

// TODO(NeGate): Type check this better
ConstValue const_eval(TranslationUnit* tu, ExprIndex e) {
	const Expr* ep = &tu->exprs[e];
	
	switch (ep->op) {
		case EXPR_INT: {
			switch (ep->int_num.suffix) {
				case INT_SUFFIX_NONE: 
				case INT_SUFFIX_L:
				case INT_SUFFIX_LL:
				return signed_const(ep->int_num.num);
				
				case INT_SUFFIX_U: 
				case INT_SUFFIX_UL:
				case INT_SUFFIX_ULL:
				return unsigned_const(ep->int_num.num);
				
				default: __builtin_unreachable();
			}
		}
		
		case EXPR_ENUM: {
			return signed_const(ep->enum_val.num);
		}
		
		case EXPR_CHAR: {
			const char* start = (const char*)(ep->str.start + 1);
			const char* end = (const char*)(ep->str.end - 1);
			
			uint32_t codepoint = 0;
			uint32_t shift = 0;
			while (start != end) {
				// TODO(NeGate): Error messages
				if (shift > 32) abort();
				
				codepoint |= (*start) << shift;
				
				shift += 8;
				start += 1;
			}
			
			return unsigned_const(codepoint);
		}
		
		case EXPR_TERNARY: {
			ConstValue cond = const_eval(tu, ep->ternary_op.left);
			if (cond.unsigned_value != 0) {
				return const_eval(tu, ep->ternary_op.middle);
			} else {
				return const_eval(tu, ep->ternary_op.right);
			}
		}
		
		case EXPR_PLUS:
		case EXPR_MINUS:
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGE:
		case EXPR_CMPLE:
		case EXPR_CMPGT:
		case EXPR_CMPLT: {
			ConstValue a = const_eval(tu, ep->bin_op.right);
			
			ExprOp op = ep->op;
			ExprIndex current = ep->bin_op.left;
			if (tu->exprs[current].op != op) {
				ConstValue b = const_eval(tu, current);
				return const_eval_bin_op(op, b, a);
			} else {
				// try tail calling
				do {
					ConstValue b = const_eval(tu, tu->exprs[current].bin_op.right);
					a = const_eval_bin_op(op, b, a);
					
					current = tu->exprs[current].bin_op.left;
				} while (tu->exprs[current].op == op);
				
				ConstValue b = const_eval(tu, current);
				return const_eval_bin_op(op, b, a);
			}
		}
		case EXPR_SIZEOF: {
			// TODO(NeGate): this is super hacky since it calls semantic pass stuff
			// early and thus it might not resolve correct... we wanna drop this later
			if (tu->exprs[ep->x_of_expr.expr].op == EXPR_DOT ||
				tu->exprs[ep->x_of_expr.expr].op == EXPR_ARROW) {
				TypeIndex src = sema_expr(tu, ep->x_of_expr.expr);
				
				return unsigned_const(tu->types[src].size);
			} else if (tu->exprs[ep->x_of_expr.expr].op == EXPR_SUBSCRIPT) {
				ExprIndex base_e = tu->exprs[ep->x_of_expr.expr].subscript.base;
				Expr* sym = &tu->exprs[base_e];
				
				if (sym->op == EXPR_UNKNOWN_SYMBOL) {
					Stmt* s = resolve_unknown_symbol(tu, base_e);
					
					if (s != NULL) {
						TypeIndex t = sema_guess_type(tu, s);
						if (t) return unsigned_const(tu->types[t].size);
					}
				} else if (sym->op == EXPR_SYMBOL) {
					return unsigned_const(tu->types[sym->symbol->decl.type].size);
				}
			} else if (tu->exprs[ep->x_of_expr.expr].op == EXPR_SYMBOL) {
				Stmt* stmt = tu->exprs[ep->x_of_expr.expr].symbol;
				return unsigned_const(tu->types[stmt->decl.type].size);
			} else if (tu->exprs[ep->x_of_expr.expr].op == EXPR_UNKNOWN_SYMBOL) {
				return unsigned_const(0);
			}
			
			break;
		}
		case EXPR_SIZEOF_T: {
			return unsigned_const(tu->types[ep->x_of_type.type].size);
		}
		case EXPR_ALIGNOF_T: {
			return unsigned_const(tu->types[ep->x_of_type.type].align);
		}
		case EXPR_NEGATE: {
			ConstValue src = const_eval(tu, ep->unary_op.src);
			return signed_const(-src.signed_value);
		}
		
		case EXPR_ADDR: {
			uint64_t dst;
			if (const_eval_try_offsetof_hack(tu, ep->unary_op.src, &dst)) {
				return unsigned_const(dst); 
			}
			break;
		}
		
		case EXPR_CAST: {
			return const_eval(tu, ep->cast.src);
		}
		default: break;
	}
	
	report(REPORT_ERROR, &tu->tokens.line_arena[ep->loc], "Could not resolve as constant expression");
	abort();
}
