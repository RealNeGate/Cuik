#include "parser.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

impl_arena(Stmt, stmt_arena)
impl_arena(StmtIndex, stmt_ref_arena)
impl_arena(Expr, expr_arena)
impl_arena(ExprIndex, expr_ref_arena)

// for the global hash table
typedef struct SymbolEntry {
	char* key;
	Symbol value;
} SymbolEntry;

static SymbolEntry* global_symbols;

static int local_symbol_count = 0;
static Symbol local_symbols[64 * 1024];

static int typedef_count = 0;
static char* typedef_names[64 * 1024];
static TypeIndex typedefs[64 * 1024];

static void expect(Lexer* l, char ch);
static Symbol* find_local_symbol(Lexer* l);
static Symbol* find_global_symbol(char* name);
static StmtIndex parse_stmt(Lexer* l);
static ExprIndex parse_expr(Lexer* l);
static StmtIndex parse_compound_stmt(Lexer* l);
static bool try_parse_declspec(Lexer* l, Attribs* attr);
static TypeIndex parse_declspec(Lexer* l, Attribs* attr);
static Decl parse_declarator(Lexer* l, TypeIndex type);
static bool is_typename(Lexer* l);
static _Noreturn void generic_error(Lexer* l, const char* msg);

inline static int align_up(int a, int b) { return a + (b - (a % b)) % b; }

TopLevel parse_file(Lexer* l) {
	////////////////////////////////
	// Parsing
	////////////////////////////////
	tls_init();
	
	init_stmt_arena(4 * 1024);
	init_stmt_ref_arena(4 * 1024);
	init_arg_arena(4 * 1024);
	init_expr_arena(4 * 1024);
	init_expr_ref_arena(4 * 1024);
	init_type_arena(4 * 1024);
	init_member_arena(4 * 1024);
	
	const static Type default_types[] = {
		[TYPE_NONE] = { KIND_VOID, 0, 0 },
		
		[TYPE_VOID] = { KIND_VOID, 0, 0 },
		[TYPE_BOOL] = { KIND_BOOL, 1, 1 },
		
		[TYPE_CHAR] = { KIND_CHAR, 1, 1 },
		[TYPE_SHORT] = { KIND_SHORT, 2, 2 },
		[TYPE_INT] = { KIND_INT, 4, 4 },
		[TYPE_LONG] = { KIND_LONG, 8, 8 },
		
		[TYPE_UCHAR] = { KIND_CHAR, 1, 1, .is_unsigned = true },
		[TYPE_USHORT] = { KIND_SHORT, 2, 2, .is_unsigned = true },
		[TYPE_UINT] = { KIND_INT, 4, 4, .is_unsigned = true },
		[TYPE_ULONG] = { KIND_LONG, 8, 8, .is_unsigned = true },
		
		[TYPE_FLOAT] = { KIND_FLOAT, 4, 4 },
		[TYPE_DOUBLE] = { KIND_DOUBLE, 8, 8 }
	};
	
	memcpy(type_arena.data, default_types, sizeof(default_types));
	type_arena.count = sizeof(default_types) / sizeof(default_types[0]);
	
	////////////////////////////////
	// Parse translation unit
	////////////////////////////////
	StmtIndex* top_level = NULL;
	
	lexer_read(l);
	while (l->token_type) {
		// program = (typedef | function-definition | global-variable)*
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(l, &attr);
		
		if (attr.is_typedef) {
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (l->token_type != ';') {
				if (expect_comma) {
					expect(l, ',');
				} else expect_comma = true;
				
				Decl decl = parse_declarator(l, type);
				assert(decl.name);
				
				int i = typedef_count++;
				typedefs[i] = decl.type;
				typedef_names[i] = decl.name;
			}
			
			expect(l, ';');
		} else {
			if (l->token_type == ';') {
				lexer_read(l);
				continue;
			}
			
			Decl decl = parse_declarator(l, type);
			
			if (type_arena.data[decl.type].kind == KIND_FUNC) {
				// function
				// TODO(NeGate): Check for redefines
				StmtIndex n = push_stmt_arena(1);
				stmt_arena.data[n] = (Stmt) {
					.op = STMT_DECL,
					.decl_type = decl.type,
					.decl_name = decl.name,
				};
				
				Symbol func_symbol = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC,
					.stmt = n
				};
				shput(global_symbols, decl.name, func_symbol);
				
				if (l->token_type == '{') {
					ArgIndex arg_start = type_arena.data[decl.type].func.arg_start;
					ArgIndex arg_end = type_arena.data[decl.type].func.arg_end;
					
					int p = 0; // param counter
					for (ArgIndex i = arg_start; i != arg_end; i++) {
						Arg* a = &arg_arena.data[i];
						
						local_symbols[local_symbol_count++] = (Symbol){
							.name = a->name,
							.type = a->type,
							.storage_class = STORAGE_PARAM,
							.param_num = p
						};
						p += 1;
					}
					
					lexer_read(l);
					
					stmt_arena.data[n].op = STMT_FUNC_DECL;
					
					// NOTE(NeGate): STMT_FUNC_DECL is always followed by a compound block
#if NDEBUG
					parse_compound_stmt(l);
#else
					StmtIndex body = parse_compound_stmt(l);
					assert(body == n + 1);
#endif
				} else if (l->token_type == ';') {
					// Forward decl
					lexer_read(l);
				} else {
					abort();
				}
				
				arrput(top_level, n);
				local_symbol_count = 0;
			} else {
				// TODO(NeGate): Normal decls
				abort();
			}
		}
	}
	
	////////////////////////////////
	// Semantics
	////////////////////////////////
	// NOTE(NeGate): Best thing about tables is that I don't actually have
	// walk a tree to check all nodes :)
	for (size_t i = 0; i < expr_arena.count; i++) {
		if (expr_arena.data[i].op == EXPR_UNKNOWN_SYMBOL) {
			Symbol* sym = find_global_symbol(expr_arena.data[i].unknown_sym);
			
			// TODO(NeGate): Give a decent error message
			if (!sym) abort();
			
			// Parameters are local and a special case how tf
			assert(sym->storage_class != STORAGE_PARAM);
			
			expr_arena.data[i].op = EXPR_SYMBOL;
			expr_arena.data[i].symbol = sym->stmt;
		}
	}
	
	return (TopLevel) { top_level };
}

static Symbol* find_local_symbol(Lexer* l) {
	const char* name = l->token_start;
	size_t length = l->token_end - l->token_start;
	
	// Try local variables
	size_t i = local_symbol_count;
	while (i--) {
		// TODO(NeGate): Implement string interning
		const char* sym = local_symbols[i].name;
		size_t sym_length = strlen(sym);
		
		if (sym_length == length && memcmp(name, sym, length) == 0) {
			return &local_symbols[i];
		}
	}
	
	return NULL;
}

static Symbol* find_global_symbol(char* name) {
	ptrdiff_t search = shgeti(global_symbols, name);
	
	if (search >= 0) return &global_symbols[search].value;
	return NULL;
}

////////////////////////////////
// STATEMENTS
////////////////////////////////
static StmtIndex parse_compound_stmt(Lexer* l) {
	// mark when the local scope starts
	int saved = local_symbol_count;
	
	StmtIndex node = push_stmt_arena(1);
	stmt_arena.data[node] = (Stmt) {
		.op = STMT_COMPOUND
	};
	
	size_t count = 0; // He be fuckin
	void* body = tls_save();
	
	while (l->token_type != '}') {
		StmtIndex s = parse_stmt(l);
		
		if (s) {
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = s;
			count++;
		} else if (is_typename(l)) {
			Attribs attr = { 0 };
			TypeIndex type = parse_declspec(l, &attr);
			
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (l->token_type != ';') {
				if (expect_comma) {
					expect(l, ',');
				} else expect_comma = true;
				
				Decl decl = parse_declarator(l, type);
				
				StmtIndex n = push_stmt_arena(1);
				stmt_arena.data[n] = (Stmt) {
					.op = STMT_DECL,
					.decl_type = decl.type,
					.decl_name = decl.name,
				};
				local_symbols[local_symbol_count++] = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = STORAGE_LOCAL,
					.stmt = n
				};
				
				if (l->token_type == '=') {
					// initial value
					lexer_read(l);
					
					stmt_arena.data[n].expr = parse_expr(l);
				}
				
				*((StmtIndex*)tls_push(sizeof(StmtIndex))) = n;
				count++;
			}
			
			expect(l, ';');
		} else {
			s = push_stmt_arena(1);
			stmt_arena.data[s].op = STMT_EXPR;
			stmt_arena.data[s].expr = parse_expr(l);
			
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = s;
			count++;
			
			expect(l, ';');
		}
	}
	expect(l, '}');
	local_symbol_count = saved;
	
	StmtIndexIndex start = push_stmt_ref_arena(count);
	memcpy(&stmt_ref_arena.data[start], body, count * sizeof(ArgIndex));
	
	stmt_arena.data[node].kids_start = start;
	stmt_arena.data[node].kids_end = start + count;
	
	tls_restore(body);
	return node;
}

// TODO(NeGate): Doesn't handle declarators or expression-statements
static StmtIndex parse_stmt(Lexer* l) {
	if (l->token_type == '{') {
		lexer_read(l);
		return parse_compound_stmt(l);
	}
	
	if (l->token_type == TOKEN_KW_return) {
		lexer_read(l);
		
		StmtIndex n = push_stmt_arena(1);
		stmt_arena.data[n].op = STMT_RETURN;
		
		if (l->token_type != ';') {
			stmt_arena.data[n].expr = parse_expr(l);
		}
		
		expect(l, ';');
		return n;
	}
	
	if (l->token_type == TOKEN_KW_if) {
		lexer_read(l);
		
		StmtIndex n = push_stmt_arena(1);
		stmt_arena.data[n].op = STMT_IF;
		
		expect(l, '(');
		stmt_arena.data[n].expr = parse_expr(l);
		expect(l, ')');
		
		stmt_arena.data[n].body = parse_stmt(l);
		
		if (l->token_type == TOKEN_KW_else) {
			lexer_read(l);
			stmt_arena.data[n].body2 = parse_stmt(l);
		} else {
			stmt_arena.data[n].body2 = 0;
		}
		
		return n;
	}
	
	if (l->token_type == TOKEN_KW_while) {
		lexer_read(l);
		
		StmtIndex n = push_stmt_arena(1);
		stmt_arena.data[n].op = STMT_WHILE;
		
		expect(l, '(');
		stmt_arena.data[n].expr = parse_expr(l);
		expect(l, ')');
		
		stmt_arena.data[n].body = parse_stmt(l);
		return n;
	}
	
	if (l->token_type == TOKEN_KW_do) {
		lexer_read(l);
		
		StmtIndex n = push_stmt_arena(1);
		stmt_arena.data[n].op = STMT_DO_WHILE;
		stmt_arena.data[n].body = parse_stmt(l);
		
		if (l->token_type != TOKEN_KW_while) {
			int loc = lexer_get_location(l);
			
			printf("error on line %d: expected 'while' got '%.*s'", loc, (int)(l->token_end - l->token_start), l->token_start);
			abort();
		}
		lexer_read(l);
		
		expect(l, '(');
		stmt_arena.data[n].expr = parse_expr(l);
		expect(l, ')');
		expect(l, ';');
		return n;
	}
	
	if (l->token_type == ';') {
		lexer_read(l);
		return 0;
	}
	
	return 0;
}

////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
static ExprIndex parse_expr_l0(Lexer* l) {
	if (l->token_type == '(') {
		lexer_read(l);
		ExprIndex e = parse_expr(l);
		expect(l, ')');
		
		return e;
	} else if (l->token_type == TOKEN_IDENTIFIER) {
		Symbol* sym = find_local_symbol(l);
		
		ExprIndex e = push_expr_arena(1);
		if (sym) {
			if (sym->storage_class == STORAGE_PARAM) {
				expr_arena.data[e] = (Expr) {
					.op = EXPR_PARAM,
					.param_num = sym->param_num
				};
			} else {
				expr_arena.data[e] = (Expr) {
					.op = EXPR_SYMBOL,
					.symbol = sym->stmt
				};
			}
		} else {
			// We'll defer any global identifier resolution
			char* name = atoms_put(l->token_end - l->token_start, l->token_start);
			
			expr_arena.data[e] = (Expr) {
				.op = EXPR_UNKNOWN_SYMBOL,
				.unknown_sym = name
			};
		}
		
		lexer_read(l);
		return e;
	} else if (l->token_type == TOKEN_NUMBER) {
		char temp[16];
		memcpy_s(temp, 16, l->token_start, l->token_end - l->token_start);
		temp[l->token_end - l->token_start] = '\0';
		
		long long i = atoll(temp);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_NUM,
			.num = i
		};
		
		lexer_read(l);
		return e;
	} else {
		generic_error(l, "Could not parse expression!");
	}
}

static ExprIndex parse_expr_l1(Lexer* l) {
	ExprIndex e = parse_expr_l0(l);
	
	// after any of the: [] () . ->
	// it'll restart and take a shot at matching another
	// piece of the expression.
	try_again: {
		if (l->token_type == '[') {
			ExprIndex base = e;
			e = push_expr_arena(1);
			
			lexer_read(l);
			ExprIndex index = parse_expr(l);
			expect(l, ']');
			
			expr_arena.data[e].op = EXPR_SUBSCRIPT;
			expr_arena.data[e].subscript.base = base;
			expr_arena.data[e].subscript.index = index;
			goto try_again;
		}
		
		// Member access
		if (l->token_type == '.') {
			lexer_read(l);
			if (l->token_type != TOKEN_IDENTIFIER) {
				generic_error(l, "Expected identifier after member access a.b");
			}
			
			char* name = atoms_put(l->token_end - l->token_start, l->token_start);
			
			ExprIndex base = e;
			e = push_expr_arena(1);
			expr_arena.data[e] = (Expr) {
				.op = EXPR_DOT,
				.dot = { base, name }
			};
			goto try_again;
		}
		
		// Function call
		if (l->token_type == '(') {
			lexer_read(l);
			
			ExprIndex target = e;
			e = push_expr_arena(1);
			
			size_t param_count = 0;
			void* params = tls_save();
			
			while (l->token_type != ')') {
				if (param_count) {
					expect(l, ',');
				}
				
				*((ExprIndex*) tls_push(sizeof(ExprIndex))) = parse_expr(l);
				param_count++;
			}
			
			if (l->token_type != ')') {
				generic_error(l, "Unclosed parameter list!");
			}
			lexer_read(l);
			
			ExprIndexIndex start = push_expr_ref_arena(param_count);
			memcpy(&expr_ref_arena.data[start], params, param_count * sizeof(ExprIndex));
			
			expr_arena.data[e] = (Expr) {
				.op = EXPR_CALL,
				.call = { target, start, start + param_count }
			};
			
			tls_restore(params);
			goto try_again;
		}
		
		// post fix, you can only put one and just after all the other operators
		// in this precendence.
		if (l->token_type == TOKEN_INCREMENT || l->token_type == TOKEN_DECREMENT) {
			bool is_inc = l->token_type == TOKEN_INCREMENT;
			lexer_read(l);
			
			ExprIndex src = e;
			
			e = push_expr_arena(1);
			expr_arena.data[e].op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC;
			expr_arena.data[e].unary_op.src = src;
		}
		
		return e;
	}
}

// deref* address&
static ExprIndex parse_expr_l2(Lexer* l) {
	if (l->token_type == '&') {
		lexer_read(l);
		
		ExprIndex value = parse_expr_l1(l);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_ADDR,
			.unary_op.src = value
		};
		return e;
	}
	
	int derefs = 0;
	while (l->token_type == '*') {
		lexer_read(l);
		derefs++;
	}
	
	ExprIndex e = parse_expr_l1(l);
	
	while (derefs--) {
		ExprIndex base = e;
		e = push_expr_arena(1);
		
		expr_arena.data[e] = (Expr) {
			.op = EXPR_DEREF,
			.unary_op.src = base
		};
	}
	
	return e;
}

// * / %
static ExprIndex parse_expr_l3(Lexer* l) {
	ExprIndex lhs = parse_expr_l2(l);
	
	while (l->token_type == TOKEN_TIMES ||
		   l->token_type == TOKEN_SLASH ||
		   l->token_type == TOKEN_PERCENT) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (l->token_type) {
			case TOKEN_TIMES: op = EXPR_TIMES; break;
			case TOKEN_SLASH: op = EXPR_SLASH; break;
			case TOKEN_PERCENT: op = EXPR_PERCENT; break;
			default: __builtin_unreachable();
		}
		lexer_read(l);
		
		ExprIndex rhs = parse_expr_l2(l);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// + -
static ExprIndex parse_expr_l4(Lexer* l) {
	ExprIndex lhs = parse_expr_l3(l);
	
	while (l->token_type == TOKEN_PLUS ||
		   l->token_type == TOKEN_MINUS) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (l->token_type) {
			case TOKEN_PLUS: op = EXPR_PLUS; break;
			case TOKEN_MINUS: op = EXPR_MINUS; break;
			default: __builtin_unreachable();
		}
		lexer_read(l);
		
		ExprIndex rhs = parse_expr_l3(l);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// >= > <= <
static ExprIndex parse_expr_l6(Lexer* l) {
	ExprIndex lhs = parse_expr_l4(l);
	
	while (l->token_type == TOKEN_GREATER_EQUAL ||
		   l->token_type == TOKEN_LESS_EQUAL || 
		   l->token_type == TOKEN_GREATER ||
		   l->token_type == TOKEN_LESS) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (l->token_type) {
			case TOKEN_LESS:          op = EXPR_CMPLT; break;
			case TOKEN_LESS_EQUAL:    op = EXPR_CMPLE; break;
			case TOKEN_GREATER:       op = EXPR_CMPGT; break;
			case TOKEN_GREATER_EQUAL: op = EXPR_CMPGE; break;
			default: __builtin_unreachable();
		}
		lexer_read(l);
		
		ExprIndex rhs = parse_expr_l4(l);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// = += -= *= /= %= <<= >>= &= ^= |=
static ExprIndex parse_expr_l14(Lexer* l) {
	ExprIndex lhs = parse_expr_l6(l);
	
	while (l->token_type == TOKEN_ASSIGN ||
		   l->token_type == TOKEN_PLUS_EQUAL ||
		   l->token_type == TOKEN_MINUS_EQUAL ||
		   l->token_type == TOKEN_TIMES_EQUAL ||
		   l->token_type == TOKEN_SLASH_EQUAL ||
		   l->token_type == TOKEN_PERCENT_EQUAL ||
		   l->token_type == TOKEN_AND_EQUAL ||
		   l->token_type == TOKEN_OR_EQUAL ||
		   l->token_type == TOKEN_XOR_EQUAL ||
		   l->token_type == TOKEN_LEFT_SHIFT_EQUAL ||
		   l->token_type == TOKEN_RIGHT_SHIFT_EQUAL) {
		ExprIndex e = push_expr_arena(1);
		
		ExprOp op;
		switch (l->token_type) {
			case TOKEN_ASSIGN: op = EXPR_ASSIGN; break;
			case TOKEN_PLUS_EQUAL: op = EXPR_PLUS_ASSIGN; break;
			case TOKEN_MINUS_EQUAL: op = EXPR_MINUS_ASSIGN; break;
			case TOKEN_TIMES_EQUAL: op = EXPR_TIMES_ASSIGN; break;
			case TOKEN_SLASH_EQUAL: op = EXPR_SLASH_ASSIGN; break;
			case TOKEN_PERCENT_EQUAL: op = EXPR_PERCENT_ASSIGN; break;
			case TOKEN_AND_EQUAL: op = EXPR_AND_ASSIGN; break;
			case TOKEN_OR_EQUAL: op = EXPR_OR_ASSIGN; break;
			case TOKEN_XOR_EQUAL: op = EXPR_XOR_ASSIGN; break;
			case TOKEN_LEFT_SHIFT_EQUAL: op = EXPR_SHL_ASSIGN; break;
			case TOKEN_RIGHT_SHIFT_EQUAL: op = EXPR_SHR_ASSIGN; break;
			default: __builtin_unreachable();
		}
		lexer_read(l);
		
		ExprIndex rhs = parse_expr_l6(l);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

static ExprIndex parse_expr(Lexer* l) {
	return parse_expr_l14(l);
}

////////////////////////////////
// TYPES
////////////////////////////////
static TypeIndex parse_type_suffix(Lexer* l, TypeIndex type, char* name);

static Decl parse_declarator(Lexer* l, TypeIndex type) {
	// handle pointers
	while (l->token_type == '*') {
		type = new_pointer(type);
		lexer_read(l);
		
		// TODO(NeGate): parse qualifiers
		switch (l->token_type) {
			case TOKEN_KW_const:
			case TOKEN_KW_volatile:
			case TOKEN_KW_restrict: 
			lexer_read(l);
			break;
			default: break;
		}
	}
	
	if (l->token_type == '(') {
		// TODO(NeGate): I don't like this code...
		// it essentially just skips over the stuff in the
		// parenthesis to do the suffix then comes back
		// for the parenthesis after wards, restoring back to
		// the end of the declarator when it's done.
		//
		// should be right after the (
		lexer_read(l);
		Lexer saved = *l;
		
		parse_declarator(l, TYPE_NONE);
		
		expect(l, ')');
		type = parse_type_suffix(l, type, NULL);
		
		Lexer saved_end = *l;
		*l = saved;
		
		Decl d = parse_declarator(l, type);
		
		// inherit name
		if (!d.name) {
			TypeIndex t = d.type;
			
			if (type_arena.data[t].kind == KIND_PTR) {
				t = type_arena.data[d.type].ptr_to;
				
				if (type_arena.data[t].kind == KIND_FUNC) {
					d.name = type_arena.data[t].func.name;
				} else if (type_arena.data[t].kind == KIND_STRUCT) {
					d.name = type_arena.data[t].record.name;
				} else if (type_arena.data[t].kind == KIND_UNION) {
					d.name = type_arena.data[t].record.name;
				}
			}
		}
		
		*l = saved_end;
		return d;
	}
	
	char* name = NULL;
	if (l->token_type == TOKEN_IDENTIFIER) {
		name = atoms_put(l->token_end - l->token_start, l->token_start);
		lexer_read(l);
	}
	
	type = parse_type_suffix(l, type, name);
	return (Decl){ type, name };
}

static TypeIndex parse_type_suffix(Lexer* l, TypeIndex type, char* name) {
	// type suffixes like array [] and function ()
	if (l->token_type == '(') {
		lexer_read(l);
		
		// TODO(NeGate): implement (void) param
		TypeIndex return_type = type;
		
		type = new_func();
		type_arena.data[type].func.name = name;
		type_arena.data[type].func.return_type = return_type;
		
		size_t arg_count = 0;
		void* args = tls_save();
		
		while (l->token_type != ')') {
			if (arg_count) {
				expect(l, ',');
			}
			
			Attribs arg_attr = { 0 };
			TypeIndex arg_base_type = parse_declspec(l, &arg_attr);
			
			Decl arg_decl = parse_declarator(l, arg_base_type);
			TypeIndex arg_type = arg_decl.type;
			
			if (type_arena.data[arg_type].kind == KIND_ARRAY) {
				// Array parameters are desugared into pointers
				arg_type = new_pointer(type_arena.data[arg_type].array_of);
			} else if (type_arena.data[arg_type].kind == KIND_FUNC) {
				// Function parameters are desugared into pointers
				arg_type = new_pointer(arg_type);
			}
			
			// TODO(NeGate): Error check that no attribs are set
			*((Arg*)tls_push(sizeof(Arg))) = (Arg) {
				.type = arg_type,
				.name = arg_decl.name
			};
			arg_count++;
		}
		
		if (l->token_type != ')') {
			generic_error(l, "Unclosed parameter list!");
		}
		lexer_read(l);
		
		ArgIndex start = push_arg_arena(arg_count);
		memcpy(&arg_arena.data[start], args, arg_count * sizeof(Arg));
		
		type_arena.data[type].func.arg_start = start;
		type_arena.data[type].func.arg_end = start + arg_count;
		
		tls_restore(args);
	} else if (l->token_type == '[') {
		do {
			lexer_read(l);
			
			// TODO(NeGate): Implement array typedecl properly
			long long count;
			if (l->token_type == ']') {
				count = 0; 
				lexer_read(l);
			} else if (l->token_type == TOKEN_NUMBER) {
				char temp[16];
				memcpy_s(temp, 16, l->token_start, l->token_end - l->token_start);
				temp[l->token_end - l->token_start] = '\0';
				lexer_read(l);
				
				count = atoll(temp);
				
				expect(l, ']');
			} else {
				abort();
			}
			
			type = new_array(type, count);
		} while (l->token_type == '[');
	}
	
	return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static TypeIndex parse_declspec(Lexer* l, Attribs* attr) {
	enum {
		VOID     = 1 << 0,
		BOOL     = 1 << 2,
		CHAR     = 1 << 4,
		SHORT    = 1 << 6,
		INT      = 1 << 8,
		LONG     = 1 << 10,
		FLOAT    = 1 << 12,
		DOUBLE   = 1 << 14,
		OTHER    = 1 << 16,
		SIGNED   = 1 << 17,
		UNSIGNED = 1 << 18,
	};
	
	int counter = 0;
	TypeIndex type = TYPE_NONE;
	
	bool is_atomic = false;
	bool is_const = false;
	do {
		switch (l->token_type) {
			case TOKEN_KW_void: counter += VOID; break;
			case TOKEN_KW_Bool: counter += BOOL; break;
			case TOKEN_KW_char: counter += CHAR; break;
			case TOKEN_KW_short: counter += SHORT; break;
			case TOKEN_KW_int: counter += INT; break;
			case TOKEN_KW_long: counter += LONG; break;
			case TOKEN_KW_float: counter += FLOAT; break;
			case TOKEN_KW_double: counter += DOUBLE; break;
			
			case TOKEN_KW_unsigned: counter |= UNSIGNED; break;
			case TOKEN_KW_signed: counter |= SIGNED; break;
			
			case TOKEN_KW_static: attr->is_static = true; break;
			case TOKEN_KW_typedef: attr->is_typedef = true; break;
			case TOKEN_KW_inline: attr->is_inline = true; break;
			case TOKEN_KW_Thread_local: attr->is_tls = true; break;
			
			case TOKEN_KW_Atomic: is_atomic = true; break;
			case TOKEN_KW_const: is_const = true; break;
			case TOKEN_KW_auto: break;
			
			case TOKEN_KW_struct: {
				if (counter) goto done;
				lexer_read(l);
				
				char* name = NULL;
				if (l->token_type == TOKEN_IDENTIFIER) {
					name = atoms_put(l->token_end - l->token_start, l->token_start);
					lexer_read(l);
				}
				
				if (l->token_type == '{') {
					lexer_read(l);
					
					type = new_struct();
					type_arena.data[type].record.name = name;
					counter += OTHER;
					
					size_t member_count = 0;
					Member* members = tls_save();
					
					int offset = 0;
					
					// struct/union are aligned to the biggest member alignment
					int align = 0;
					
					while (l->token_type != '}') {
						Attribs member_attr = { 0 };
						TypeIndex member_base_type = parse_declspec(l, &member_attr);
						
						Decl decl = parse_declarator(l, member_base_type);
						TypeIndex member_type = decl.type;
						
						if (type_arena.data[member_type].kind == KIND_FUNC) {
							generic_error(l, "Naw dawg");
						}
						
						offset = align_up(offset, type_arena.data[member_type].align);
						
						// TODO(NeGate): Error check that no attribs are set
						tls_push(sizeof(Member));
						members[member_count++] = (Member) {
							.type = member_type,
							.name = decl.name
						};
						
						offset += type_arena.data[member_type].size;
						
						if (type_arena.data[member_type].align > align) {
							align = type_arena.data[member_type].align;
						}
						
						expect(l, ';');
					}
					
					if (l->token_type != '}') {
						generic_error(l, "Unclosed member list!");
					}
					
					offset = align_up(offset, align);
					type_arena.data[type].size = offset;
					type_arena.data[type].align = align;
					
					MemberIndex start = push_arg_arena(member_count);
					memcpy(&member_arena.data[start], members, member_count * sizeof(Member));
					
					type_arena.data[type].record.kids_start = start;
					type_arena.data[type].record.kids_end = start + member_count;
					
					tls_restore(members);
				} else {
					// must be a forward decl
					abort();
				}
				break;
			}
			
			case TOKEN_IDENTIFIER: {
				if (counter) goto done;
				
				size_t len = l->token_end - l->token_start;
				
				int i = typedef_count;
				while (i--) {
					size_t typedef_len = strlen(typedef_names[i]);
					
					if (len == typedef_len &&
						memcmp(l->token_start, typedef_names[i], len) == 0) {
						type = typedefs[i];
						counter += OTHER;
						break;
					}
				}
				
				if (counter) break;
				
				// if not a typename, this isn't a typedecl
				goto done;
			}
			default: goto done;
		}
		
		switch (counter) {
			case 0: break; // not resolved yet
			case VOID:
			type = TYPE_VOID;
			break;
			case BOOL:
			type = TYPE_BOOL;
			break;
			case CHAR:
			case SIGNED + CHAR:
			type = TYPE_CHAR;
			break;
			case UNSIGNED + CHAR:
			type = TYPE_UCHAR;
			break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
			type = TYPE_SHORT;
			break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
			type = TYPE_USHORT;
			break;
			case INT:
			case SIGNED:
			case SIGNED + INT:
			type = TYPE_INT;
			break;
			case UNSIGNED:
			case UNSIGNED + INT:
			type = TYPE_UINT;
			break;
			case LONG:
			case LONG + INT:
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
			type = TYPE_LONG;
			break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
			type = TYPE_ULONG;
			break;
			case FLOAT:
			type = TYPE_FLOAT;
			break;
			case DOUBLE:
			case LONG + DOUBLE:
			type = TYPE_DOUBLE;
			break;
			case OTHER:
			assert(type);
			break;
			default:
			generic_error(l, "invalid type");
			break;
		}
		
		lexer_read(l);
	} while (true);
	
	done:
	if (type == 0) {
		generic_error(l, "Unknown typename");
	}
	
	if (is_atomic || is_const) {
		type = copy_type(type);
		type_arena.data[type].is_atomic = is_atomic;
		type_arena.data[type].is_const = is_const;
	}
	
	return type;
}

static bool is_typename(Lexer* l) {
	switch (l->token_type) {
		case TOKEN_KW_void:
		case TOKEN_KW_char:
		case TOKEN_KW_short:
		case TOKEN_KW_int:
		case TOKEN_KW_long:
		case TOKEN_KW_float:
		case TOKEN_KW_double:
		case TOKEN_KW_signed:
		case TOKEN_KW_unsigned:
		case TOKEN_KW_static:
		case TOKEN_KW_typedef:
		case TOKEN_KW_inline:
		case TOKEN_KW_Thread_local:
		case TOKEN_KW_auto:
		return true;
		
		case TOKEN_IDENTIFIER: {
			size_t len = l->token_end - l->token_start;
			
			int i = typedef_count;
			while (i--) {
				size_t typedef_len = strlen(typedef_names[i]);
				
				if (len == typedef_len &&
					memcmp(l->token_start, typedef_names[i], len) == 0) {
					return true;
				}
			}
			
			return false;
		}
		default:
		return false;
	}
}

////////////////////////////////
// ERRORS
////////////////////////////////
static _Noreturn void generic_error(Lexer* l, const char* msg) {
	int loc = lexer_get_location(l);
	
	printf("error on line %d: %s\n", loc, msg);
	abort();
}

static void expect(Lexer* l, char ch) {
	if (l->token_type != ch) {
		int loc = lexer_get_location(l);
		
		printf("error on line %d: expected '%c' got '%.*s'", loc, ch, (int)(l->token_end - l->token_start), l->token_start);
		abort();
	}
	
	lexer_read(l);
}
