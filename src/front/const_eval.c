#include "parser.h"

#define unsigned_const(x) (ConstValue){ false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue){ true, .signed_value = (x) }

// TODO(NeGate): Type check this better
ConstValue const_eval(ExprIndex e) {
	const Expr* restrict ep = &expr_arena.data[e];
	
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
			ConstValue cond = const_eval(ep->ternary_op.left);
			if (cond.unsigned_value != 0) {
				return const_eval(ep->ternary_op.middle);
			} else {
				return const_eval(ep->ternary_op.right);
			}
		}
		
		case EXPR_PLUS:
		case EXPR_TIMES:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGE:
		case EXPR_CMPLE:
		case EXPR_CMPGT:
		case EXPR_CMPLT: {
			ConstValue a = const_eval(ep->bin_op.left);
			ConstValue b = const_eval(ep->bin_op.right);
			
			bool is_signed = a.is_signed | b.is_signed;
			switch (ep->op) {
				case EXPR_PLUS: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value + b.unsigned_value };
				case EXPR_TIMES: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value * b.unsigned_value };
				
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
			
			case EXPR_ALIGNOF_T: {
				return signed_const(type_arena.data[ep->x_of_type.type].align);
			}
			
			case EXPR_CAST: {
				return const_eval(ep->cast.src);
			}
			
			case EXPR_UNKNOWN_SYMBOL: {
				const unsigned char* name = expr_arena.data[e].unknown_sym;
				StmtIndex s = resolve_unknown_symbol(e);
				
				if (!s) {
					// try enum names
					// NOTE(NeGate): this might be slow
					size_t i = enum_entry_arena.count;
					while (i--) {
						if (cstr_equals(name, enum_entry_arena.data[i].name)) {
							return signed_const(enum_entry_arena.data[i].value);
						}
					}
					
					// TODO(NeGate): error messages
					printf("error: could not find symbol: %s\n", name);
					abort();
				}
				
				Type* restrict type = &type_arena.data[stmt_arena.data[s].decl.type];
				if (!type->is_const) {
					// TODO(NeGate): error messages
					printf("error: not const :(\n");
					abort();
				}
				
				if (!stmt_arena.data[s].decl.initial) {
					// TODO(NeGate): error messages
					printf("error: no expression :(\n");
					abort();
				}
				
				if (type->kind >= KIND_BOOL && type->kind <= KIND_LONG) {
					return const_eval(stmt_arena.data[s].decl.initial);
				} else {
					// TODO(NeGate): error messages
					printf("error: bad type\n");
					abort();
				}
			}
		}
		
		default: break;
	}
	
	// TODO(NeGate): error messages
	abort();
}
