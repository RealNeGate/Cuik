#include "parser.h"

#define unsigned_const(x) (ConstValue){ false, .unsigned_value = (x) }
#define signed_const(x) (ConstValue){ true, .signed_value = (x) }

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
			ConstValue a = const_eval(tu, ep->bin_op.left);
			ConstValue b = const_eval(tu, ep->bin_op.right);
			
			bool is_signed = a.is_signed | b.is_signed;
			switch (ep->op) {
				case EXPR_PLUS: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value + b.unsigned_value };
				case EXPR_MINUS: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value - b.unsigned_value };
				case EXPR_TIMES: return (ConstValue){ is_signed, .unsigned_value = a.unsigned_value * b.unsigned_value };
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
				// hacky but it just needs to support &(((T*)0)->apple)
				const Expr* restrict arrow = &tu->exprs[ep->unary_op.src];
				if (arrow->op == EXPR_ARROW) {
					const Expr* restrict arrow_base = &tu->exprs[arrow->arrow.base];
					const Type* restrict record_type = NULL;
					
					if (arrow_base->op == EXPR_CAST) {
						record_type = &tu->types[arrow_base->cast.type];
						
						// dereference
						if (record_type->kind == KIND_PTR) {
							record_type = &tu->types[record_type->ptr_to];
						} else {
							// TODO(NeGate): error messages
							abort();
						}
						
						if (record_type->kind != KIND_STRUCT && record_type->kind != KIND_UNION) {
							abort();
						}
						
						ConstValue pointer_value = const_eval(tu, arrow_base->cast.src);
						assert(pointer_value.unsigned_value == 0 && "I mean i'm not honestly sure if we wanna allow for a non null pointer here");
						((void)pointer_value);
					} else {
						// TODO(NeGate): error messages
						abort();
					}
					
					Atom name = arrow->arrow.name;
					MemberIndex start = record_type->record.kids_start;
					MemberIndex end = record_type->record.kids_end;
					for (MemberIndex m = start; m < end; m++) {
						Member* member = &tu->members[m];
						
						// TODO(NeGate): String interning would be nice
						if (cstr_equals(name, member->name)) {
							// TODO(NeGate): error messages
							if (member->is_bitfield) abort();
							
							return unsigned_const(member->offset);
						}
					}
					
					// TODO(NeGate): error messages
					abort();
				}
				
				// TODO(NeGate): error messages
				abort();
			}
			
			case EXPR_CAST: {
				return const_eval(tu, ep->cast.src);
			}
			
			case EXPR_UNKNOWN_SYMBOL: {
				const unsigned char* name = ep->unknown_sym;
				StmtIndex s = resolve_unknown_symbol(tu, e);
				
				if (!s) {
					// try enum names
					// NOTE(NeGate): this might be slow
					for (size_t j = 1, count = big_array_length(tu->enum_entries); j < count; j++) {
						if (cstr_equals(name, tu->enum_entries[j].name)) {
							return signed_const(tu->enum_entries[j].value);
						}
					}
					
					// TODO(NeGate): error messages
					printf("error: could not find symbol: %s\n", name);
					abort();
				}
				
				Type* restrict type = &tu->types[tu->stmts[s].decl.type];
				if (!type->is_const) {
					// TODO(NeGate): error messages
					printf("error: not const :(\n");
					abort();
				}
				
				if (!tu->stmts[s].decl.initial) {
					// TODO(NeGate): error messages
					printf("error: no expression :(\n");
					abort();
				}
				
				if (type->kind >= KIND_BOOL && type->kind <= KIND_LONG) {
					return const_eval(tu, tu->stmts[s].decl.initial);
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
