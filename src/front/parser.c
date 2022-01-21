#include "parser.h"

impl_arena(Stmt, stmt_arena)
impl_arena(Expr, expr_arena)

// this means types which can be incomplete
// not just currently imcomplete so structs,
// unions and enums
typedef struct IncompleteType {
	Atom key;
	TypeIndex value;
} IncompleteType;

static IncompleteType* incomplete_types;

// for the global hash table
typedef struct SymbolEntry {
	Atom key;
	Symbol value;
} SymbolEntry;

static SymbolEntry* global_symbols;

static int local_symbol_count = 0;
static Symbol local_symbols[64 * 1024];

// for the global hash table
typedef struct TypedefEntry {
	Atom key;
	TypeIndex value;
} TypedefEntry;

static TypedefEntry* typedefs;

// for the global hash table
typedef struct LabelEntry {
	Atom key;
	StmtIndex value;
} LabelEntry;

static LabelEntry* labels;

static StmtIndex current_switch_or_case;
static StmtIndex current_breakable;

static void expect(TokenStream* restrict s, char ch);
static Symbol* find_local_symbol(TokenStream* restrict s);
static Symbol* find_global_symbol(char* name);
static StmtIndex parse_stmt(TokenStream* restrict s);
static StmtIndex parse_stmt_or_expr(TokenStream* restrict s);
static StmtIndex parse_compound_stmt(TokenStream* restrict s);
static bool try_parse_declspec(TokenStream* restrict s, Attribs* attr);
static TypeIndex parse_declspec(TokenStream* restrict s, Attribs* attr);
static Decl parse_declarator(TokenStream* restrict s, TypeIndex type);
static TypeIndex parse_typename(TokenStream* restrict s);
static intmax_t parse_const_expr(TokenStream* restrict s);
static bool is_typename(TokenStream* restrict s);
static _Noreturn void generic_error(TokenStream* restrict s, const char* msg);
static void parse_decl_or_expr(TokenStream* restrict s, size_t* body_count);
static ExprIndex parse_expr(TokenStream* restrict s);
static ExprIndex parse_initializer(TokenStream* restrict s, TypeIndex type);
static bool skip_over_declspec(TokenStream* restrict s);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static ExprIndex parse_expr_l14(TokenStream* restrict s);

inline static int align_up(int a, int b) { return a + (b - (a % b)) % b; }

inline static StmtIndex make_stmt(TokenStream* restrict s, StmtOp op) {
	StmtIndex n = push_stmt_arena(1);
	stmt_arena.data[n].op = op;
	stmt_arena.data[n].loc = tokens_get(s)->location;
	return n;
}

TopLevel parse_file(TokenStream* restrict s) {
	////////////////////////////////
	// Parsing
	////////////////////////////////
	tls_init();
	
	init_stmt_arena(4 * 1024);
	init_enum_entry_arena(4 * 1024);
	init_expr_arena(4 * 1024);
	init_types();
	
	////////////////////////////////
	// Parse translation unit
	////////////////////////////////
	StmtIndex* top_level = NULL;
	
	while (tokens_get(s)->type) {
		// TODO(NeGate): Correctly parse pragmas instead of
		// ignoring them.
		if (tokens_get(s)->type == TOKEN_KW_Pragma) {
			tokens_next(s);
			expect(s, '(');
			
			int depth = 0;
			while (tokens_get(s)->type) {
				if (tokens_get(s)->type == '(') depth++;
				else if (tokens_get(s)->type == ')') {
					if (depth == 0) break;
					depth--;
				}
				
				tokens_next(s);
			}
			
			expect(s, ')');
			continue;
		}
		
		if (skip_over_declspec(s)) {
			continue;
		}
		
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(s, &attr);
		
		if (attr.is_typedef) {
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (tokens_get(s)->type != ';') {
				if (expect_comma) {
					expect(s, ',');
				} else expect_comma = true;
				
				Decl decl = parse_declarator(s, type);
				assert(decl.name);
				
				shput(typedefs, decl.name, decl.type);
			}
			
			expect(s, ';');
		} else {
			if (tokens_get(s)->type == ';') {
				tokens_next(s);
				continue;
			}
			
			Decl decl = parse_declarator(s, type);
			
			if (type_arena.data[decl.type].kind == KIND_FUNC) {
				// function
				Symbol* sym = find_global_symbol((char*)decl.name);
				
				StmtIndex n;
				bool is_redefine = false;
				bool is_redefining_body = false;
				if (sym) {
					is_redefine = true;
					
					// TODO(NeGate): Error messages
					StorageClass sclass = attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC;
					if (sym->storage_class != sclass) abort();
					
					// convert forward decl into proper function
					n = sym->stmt;
					if (stmt_arena.data[n].op == STMT_FUNC_DECL) {
						is_redefining_body = true;
					}
					
					// TODO(NeGate): Error messages
					if (!type_equal(stmt_arena.data[n].decl.type, decl.type)) abort();
				} else {
					// New symbol
					n = make_stmt(s, STMT_DECL);
					stmt_arena.data[n].decl = (struct StmtDecl){
						.type = decl.type,
						.name = decl.name,
						.attrs = attr
					};
					
					Symbol func_symbol = (Symbol){
						.name = decl.name,
						.type = decl.type,
						.storage_class = attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC,
						.stmt = n
					};
					shput(global_symbols, decl.name, func_symbol);
				}
				
				if (tokens_get(s)->type == '{') {
					// TODO(NeGate): Error messages
					if (is_redefining_body) {
						generic_error(s, "Cannot redefine function decl");
					}
					
					ParamIndex param_list = type_arena.data[decl.type].func.param_list;
					ParamIndex param_count = type_arena.data[decl.type].func.param_count;
					
					for (size_t i = 0; i < param_count; i++) {
						Param* p = &param_arena.data[param_list + i];
						
						if (p->name) {
							local_symbols[local_symbol_count++] = (Symbol){
								.name = p->name,
								.type = p->type,
								.storage_class = STORAGE_PARAM,
								.param_num = i
							};
						}
					}
					tokens_next(s);
					
					ExprIndex starting_point = expr_arena.count;
					
					stmt_arena.data[n].op = STMT_FUNC_DECL;
					stmt_arena.data[n].decl.initial = (StmtIndex)parse_compound_stmt(s);
					
					// resolve any unresolved label references 
					for (size_t i = starting_point; i < expr_arena.count; i++) {
						if (expr_arena.data[i].op == EXPR_UNKNOWN_SYMBOL) {
							const unsigned char* name = expr_arena.data[i].unknown_sym;
							
							ptrdiff_t search = shgeti(labels, name);
							if (search >= 0) {
								expr_arena.data[i].op = EXPR_SYMBOL;
								expr_arena.data[i].symbol = labels[search].value;
							}
						}
					}
					
					shfree(labels);
				} else if (tokens_get(s)->type == ';') {
					// Forward decl
					stmt_arena.data[n].decl.initial = (StmtIndex)0;
					tokens_next(s);
				} else {
					abort();
				}
				
				if (!is_redefine) {
					arrput(top_level, n);
				}
				local_symbol_count = 0;
			} else {
				// Normal decls
				assert(decl.name);
				
				StmtIndex n = make_stmt(s, STMT_GLOBAL_DECL);
				stmt_arena.data[n].decl = (struct StmtDecl){
					.type = decl.type,
					.name = decl.name
				};
				arrput(top_level, n);
				
				Symbol sym = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL,
					.stmt = n
				};
				shput(global_symbols, decl.name, sym);
				
				if (tokens_get(s)->type == '=') {
					tokens_next(s);
					
					if (tokens_get(s)->type == '{') {
						tokens_next(s);
						
						stmt_arena.data[n].decl.initial = parse_initializer(s, TYPE_VOID);
					} else {
						stmt_arena.data[n].decl.initial = parse_expr_l14(s);
					}
				}
				
				if (tokens_get(s)->type == ',') {
					while (tokens_get(s)->type != ';') {
						expect(s, ',');
						
						Decl decl = parse_declarator(s, type);
						assert(decl.name);
						
						StmtIndex n = make_stmt(s, STMT_GLOBAL_DECL);
						stmt_arena.data[n].decl = (struct StmtDecl){
							.type = decl.type,
							.name = decl.name
						};
						arrput(top_level, n);
						
						Symbol sym = (Symbol){
							.name = decl.name,
							.type = decl.type,
							.storage_class = attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL,
							.stmt = n
						};
						shput(global_symbols, decl.name, sym);
						
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							if (tokens_get(s)->type == '{') {
								tokens_next(s);
								
								stmt_arena.data[n].decl.initial = parse_initializer(s, TYPE_VOID);
							} else {
								stmt_arena.data[n].decl.initial = parse_expr_l14(s);
							}
						}
					}
				}
				
				expect(s, ';');
			}
		}
	}
	
	////////////////////////////////
	// Semantics
	////////////////////////////////
	// NOTE(NeGate): This is a C extension, it allows normal symbols like
	// functions to declared out of order.
	for (size_t i = 0; i < expr_arena.count; i++) {
		if (expr_arena.data[i].op == EXPR_UNKNOWN_SYMBOL) {
			const unsigned char* name = expr_arena.data[i].unknown_sym;
			
			if (!resolve_unknown_symbol(i)) {
				// try enum names
				// NOTE(NeGate): this might be slow
				for (size_t j = 1; j < enum_entry_arena.count; j++) {
					if (cstr_equals(name, enum_entry_arena.data[j].name)) {
						int value = enum_entry_arena.data[j].value;
						
						expr_arena.data[i].op = EXPR_INT;
						expr_arena.data[i].int_num = (struct ExprInt){ value, INT_SUFFIX_NONE };
						goto success;
					}
				}
				
				Token* t = tokens_get(s);
				SourceLoc* loc = &s->line_arena[t->location];
				
				printf("%s:%d: error: could not find symbol: %s\n", loc->file, loc->line, name);
				abort();
				
				success:;
			}
		}
	}
	
	local_symbol_count = 0;
	shfree(global_symbols);
	
	return (TopLevel) { top_level };
}

StmtIndex resolve_unknown_symbol(StmtIndex i) {
	Symbol* sym = find_global_symbol((char*)expr_arena.data[i].unknown_sym);
	if (!sym) return 0;
	
	// Parameters are local and a special case how tf
	assert(sym->storage_class != STORAGE_PARAM);
	stmt_arena.data[sym->stmt].decl.attrs.is_used = true;
	
	expr_arena.data[i].op = EXPR_SYMBOL;
	expr_arena.data[i].symbol = sym->stmt;
	return sym->stmt;
}

static bool skip_over_declspec(TokenStream* restrict s) {
	if (tokens_get(s)->type == TOKEN_KW_declspec) {
		tokens_next(s);
		expect(s, '(');
		
		// TODO(NeGate): Correctly parse declspec instead of
		// ignoring them.
		int depth = 1;
		while (depth) {
			if (tokens_get(s)->type == '(') depth++;
			else if (tokens_get(s)->type == ')') depth--;
			
			tokens_next(s);
		}
		return true;
	}
	
	return false;
}

static Symbol* find_local_symbol(TokenStream* restrict s) {
	Token* t = tokens_get(s);
	const unsigned char* name = t->start;
	size_t length = t->end - t->start;
	
	// Try local variables
	size_t i = local_symbol_count;
	while (i--) {
		// TODO(NeGate): Implement string interning
		const unsigned char* sym = local_symbols[i].name;
		size_t sym_length = strlen((const char*)sym);
		
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

static TypeIndex find_incomplete_type(char* name) {
	ptrdiff_t search = shgeti(incomplete_types, name);
	
	if (search >= 0) return incomplete_types[search].value;
	return 0;
}

////////////////////////////////
// STATEMENTS
////////////////////////////////
static StmtIndex parse_compound_stmt(TokenStream* restrict s) {
	// mark when the local scope starts
	int saved = local_symbol_count;
	
	StmtIndex node = make_stmt(s, STMT_COMPOUND);
	
	size_t body_count = 0; // He be fuckin
	void* body = tls_save();
	
	while (tokens_get(s)->type != '}') {
		while (tokens_get(s)->type == ';') tokens_next(s);
		
		StmtIndex stmt = parse_stmt(s);
		if (stmt) {
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = stmt;
			body_count++;
		} else {
			parse_decl_or_expr(s, &body_count);
		}
	}
	expect(s, '}');
	local_symbol_count = saved;
	
	StmtIndex* stmt_array = arena_alloc(body_count * sizeof(StmtIndex), _Alignof(StmtIndex));
	memcpy(stmt_array, body, body_count * sizeof(StmtIndex));
	
	stmt_arena.data[node].compound = (struct StmtCompound) {
		.kids = stmt_array,
		.kids_count = body_count
	};
	
	tls_restore(body);
	return node;
}

// TODO(NeGate): Doesn't handle declarators or expression-statements
static StmtIndex parse_stmt(TokenStream* restrict s) {
	if (tokens_get(s)->type == '{') {
		tokens_next(s);
		return parse_compound_stmt(s);
	} else if (tokens_get(s)->type == TOKEN_KW_return) {
		tokens_next(s);
		
		ExprIndex e = 0;
		if (tokens_get(s)->type != ';') {
			e = parse_expr(s);
		}
		
		StmtIndex n = make_stmt(s, STMT_RETURN);
		stmt_arena.data[n].return_ = (struct StmtReturn){
			.expr = e
		};
		
		expect(s, ';');
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_if) {
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_IF);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(s);
		expect(s, ')');
		
		StmtIndex body = parse_stmt_or_expr(s);
		assert(body);
		
		StmtIndex next = 0;
		if (tokens_get(s)->type == TOKEN_KW_else) {
			tokens_next(s);
			next = parse_stmt_or_expr(s);
		}
		
		stmt_arena.data[n].if_ = (struct StmtIf){
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_switch) {
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_SWITCH);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(s);
		expect(s, ')');
		
		stmt_arena.data[n].switch_ = (struct StmtSwitch){
			.condition = cond
		};
		
		// begin a new chain but keep the old one
		StmtIndex old_switch = current_switch_or_case;
		current_switch_or_case = n;
		
		StmtIndex old_breakable = current_breakable;
		current_breakable = n;
		{
			StmtIndex body = parse_stmt_or_expr(s);
			stmt_arena.data[n].switch_.body = body;
		}
		
		current_breakable = old_breakable;
		current_switch_or_case = old_switch;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_case) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_CASE);
		
		Stmt* last_node = &stmt_arena.data[current_switch_or_case];
		if (last_node->op == STMT_CASE) {
			last_node->case_.next = n;
		} else if (last_node->op == STMT_DEFAULT) {
			last_node->default_.next = n;
		} else if (last_node->op == STMT_SWITCH) {
			last_node->switch_.next = n;
		} else {
			abort();
		}
		
		intmax_t key = parse_const_expr(s);
		
		current_switch_or_case = n;
		expect(s, ':');
		
		stmt_arena.data[n].case_ = (struct StmtCase){
			.key = key, .body = 0, .next = 0
		};
		
		StmtIndex body = parse_stmt_or_expr(s);
		stmt_arena.data[n].case_.body = body;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_default) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_DEFAULT);
		
		Stmt* last_node = &stmt_arena.data[current_switch_or_case];
		if (last_node->op == STMT_CASE) {
			last_node->case_.next = n;
		} else if (last_node->op == STMT_DEFAULT) {
			last_node->default_.next = n;
		} else if (last_node->op == STMT_SWITCH) {
			last_node->switch_.next = n;
		} else {
			abort();
		}
		current_switch_or_case = n;
		expect(s, ':');
		
		stmt_arena.data[n].default_ = (struct StmtDefault){
			.body = 0, .next = 0
		};
		
		StmtIndex body = parse_stmt_or_expr(s);
		stmt_arena.data[n].default_.body = body;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_break) {
		// TODO(NeGate): error messages
		assert(current_breakable);
		
		tokens_next(s);
		expect(s, ';');
		
		StmtIndex n = make_stmt(s, STMT_BREAK);
		stmt_arena.data[n].break_ = (struct StmtBreak){
			.target = current_breakable
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_while) {
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_WHILE);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(s);
		expect(s, ')');
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			
			body = parse_stmt_or_expr(s);
			
			current_breakable = old_breakable;
		}
		
		stmt_arena.data[n].while_ = (struct StmtWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_for) {
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_FOR);
		
		int saved = local_symbol_count;
		expect(s, '(');
		
		// it's either nothing, a declaration, or an expression
		StmtIndex first = 0;
		if (tokens_get(s)->type == ';') {
			/* nothing */
			tokens_next(s);
		} else {
			// NOTE(NeGate): This is just a decl list or a single expression.
			first = make_stmt(s, STMT_COMPOUND);
			
			size_t body_count = 0; // He be fuckin
			void* body = tls_save();
			{
				parse_decl_or_expr(s, &body_count);
			}
			
			StmtIndex* stmt_array = arena_alloc(body_count * sizeof(StmtIndex), _Alignof(StmtIndex));
			memcpy(stmt_array, body, body_count * sizeof(StmtIndex));
			
			stmt_arena.data[first].compound = (struct StmtCompound) {
				.kids = stmt_array,
				.kids_count = body_count
			};
			tls_restore(body);
		}
		
		ExprIndex cond = 0;
		if (tokens_get(s)->type == ';') {
			/* nothing */
			tokens_next(s);
		} else {
			cond = parse_expr(s);
			expect(s, ';');
		}
		
		ExprIndex next = 0;
		if (tokens_get(s)->type == ')') {
			/* nothing */
			tokens_next(s);
		} else {
			next = parse_expr(s);
			expect(s, ')');
		}
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			
			body = parse_stmt_or_expr(s);
			
			current_breakable = old_breakable;
		}
		
		// restore local symbol scope
		local_symbol_count = saved;
		
		stmt_arena.data[n].for_ = (struct StmtFor){
			.first = first,
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_do) {
		tokens_next(s);
		StmtIndex n = make_stmt(s, STMT_DO_WHILE);
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			
			body = parse_stmt_or_expr(s);
			
			current_breakable = old_breakable;
		}
		
		if (tokens_get(s)->type != TOKEN_KW_while) {
			Token* t = tokens_get(s);
			SourceLoc* loc = &s->line_arena[t->location];
			
			printf("%s:%d: error: expected 'while' got '%.*s'\n", loc->file, loc->line, (int)(t->end - t->start), t->start);
			abort();
		}
		tokens_next(s);
		
		expect(s, '(');
		
		ExprIndex cond = parse_expr(s);
		
		expect(s, ')');
		expect(s, ';');
		
		stmt_arena.data[n].do_while = (struct StmtDoWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_goto) {
		tokens_next(s);
		
		StmtIndex n = make_stmt(s, STMT_GOTO);
		
		ExprIndex target = parse_expr(s);
		stmt_arena.data[n].goto_ = (struct StmtGoto){
			.target = target
		};
		
		expect(s, ';');
		return n;
	} else if (tokens_get(s)->type == TOKEN_IDENTIFIER &&
			   tokens_peek(s)->type == TOKEN_COLON) {
		Token* t = tokens_get(s);
		Atom name = atoms_put(t->end - t->start, t->start);
		
		StmtIndex n = make_stmt(s, STMT_LABEL);
		stmt_arena.data[n].label = (struct StmtLabel){
			.name = name
		};
		shput(labels, name, n);
		
		tokens_next(s);
		tokens_next(s);
		return n;
	} else {
		return 0;
	}
}

static void parse_decl_or_expr(TokenStream* restrict s, size_t* body_count) {
	if (tokens_get(s)->type == ';') {
		tokens_next(s);
	} else if (is_typename(s)) {
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(s, &attr);
		
		// TODO(NeGate): Kinda ugly
		// don't expect one the first time
		bool expect_comma = false;
		while (tokens_get(s)->type != ';') {
			if (expect_comma) {
				expect(s, ',');
			} else expect_comma = true;
			
			Decl decl = parse_declarator(s, type);
			ExprIndex initial = 0;
			
			if (tokens_get(s)->type == '=') {
				tokens_next(s);
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					initial = parse_initializer(s, TYPE_VOID);
				} else {
					initial = parse_expr_l14(s);
				}
			}
			
			StmtIndex n = make_stmt(s, STMT_DECL);
			stmt_arena.data[n].decl = (struct StmtDecl){
				.type = decl.type,
				.name = decl.name,
				.initial = initial
			};
			
			local_symbols[local_symbol_count++] = (Symbol){
				.name = decl.name,
				.type = decl.type,
				.storage_class = STORAGE_LOCAL,
				.stmt = n
			};
			
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = n;
			*body_count += 1;
		}
		
		expect(s, ';');
	} else {
		StmtIndex n = make_stmt(s, STMT_EXPR);
		
		ExprIndex expr = parse_expr(s);
		stmt_arena.data[n].expr = (struct StmtExpr){
			.expr = expr
		};
		
		*((StmtIndex*)tls_push(sizeof(StmtIndex))) = n;
		*body_count += 1;
		expect(s, ';');
	}
}

static StmtIndex parse_stmt_or_expr(TokenStream* restrict s) {
	if (tokens_get(s)->type == ';') {
		tokens_next(s);
		return 0;
	} else {
		StmtIndex stmt = parse_stmt(s);
		
		if (stmt) {
			return stmt;
		} else {
			StmtIndex n = make_stmt(s, STMT_EXPR);
			
			ExprIndex expr = parse_expr(s);
			stmt_arena.data[n].expr = (struct StmtExpr){
				.expr = expr
			};
			
			expect(s, ';');
			
			return n;
		}
	}
}

////////////////////////////////
// EXPRESSIONS
//
// Quick reference:
// https://en.cppreference.com/w/c/language/operator_precedence
////////////////////////////////
static ExprIndex parse_expr_l2(TokenStream* restrict s);

// NOTE(NeGate): This function will push all nodes it makes onto the temporary
// storage where parse_initializer will move them into permanent storage.
static void parse_initializer_member(TokenStream* restrict s) {
	// Parse designator, it's just chains of:
	// [const-expr]
	// .identifier
	InitNode* current = NULL;
	try_again: {
		if (tokens_get(s)->type == '[') {
			int start = parse_const_expr(s);
			if (start < 0) {
				// TODO(NeGate): Error messages
				generic_error(s, "Array initializer range is broken.");
			}
			
			// GNU-extension: array range initializer
			int count = 1;
			if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
				tokens_next(s);
				
				count = parse_const_expr(s) - start;
				if (count <= 1) {
					// TODO(NeGate): Error messages
					generic_error(s, "Array initializer range is broken.");
				}
			}
			expect(s, ']');
			
			current = (InitNode*) tls_push(sizeof(InitNode));
			*current = (InitNode) {
				.kids_count = 1,
				.start = start,
				.count = count
			};
			goto try_again;
		}
		
		if (tokens_get(s)->type == '.') {
			tokens_next(s);
			
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			tokens_next(s);
			
			current = (InitNode*) tls_push(sizeof(InitNode));
			*current = (InitNode) {
				.kids_count = 1,
				.member_name = name
			};
			goto try_again;
		}
	}
	
	// if it has no designator make a dummy one.
	if (!current) {
		current = (InitNode*) tls_push(sizeof(InitNode));
		*current = (InitNode) {
			.kids_count = 1,
			.member_name = NULL
		};
	} else {
		expect(s, '=');
	}
	
	// it can either be a normal expression
	// or a nested designated initializer
	if (tokens_get(s)->type == '{') {
		tokens_next(s);
		
		size_t local_count = 0;
		
		// don't expect one the first time
		bool expect_comma = false;
		while (tokens_get(s)->type == '}') {
			if (expect_comma) {
				expect(s, ',');
			} else expect_comma = true;
			
			parse_initializer_member(s);
			local_count += 1;
		}
		
		current->kids_count = local_count;
	} else {
		// parse without comma operator
		current->kids_count = 0;
		current->expr = parse_expr_l14(s);
	}
}

static ExprIndex parse_initializer(TokenStream* restrict s, TypeIndex type) {
	// TODO(NeGate): Handle compound literal
	size_t count = 0;
	InitNode* start = tls_save();
	
	// don't expect one the first time
	bool expect_comma = false;
	while (tokens_get(s)->type != '}') {
		if (expect_comma) {
			expect(s, ',');
			
			if (tokens_get(s)->type == '}') break;
		} else expect_comma = true;
		
		parse_initializer_member(s);
		count += 1;
	}
	
	expect(s, '}');
	
	InitNode* permanent_store = arena_alloc(count * sizeof(InitNode), _Alignof(InitNode));
	memcpy(permanent_store, start, count * sizeof(InitNode));
	tls_restore(start);
	
	ExprIndex e = push_expr_arena(1);
	expr_arena.data[e] = (Expr) {
		.op = EXPR_INITIALIZER,
		.loc = tokens_get(s)->location,
		.init = { type, count, permanent_store }
	};
	return e;
}

static ExprIndex parse_expr_l0(TokenStream* restrict s) {
	SourceLocIndex loc = tokens_get(s)->location;
	
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		ExprIndex e = parse_expr(s);
		expect(s, ')');
		
		return e;
	} else if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
		Symbol* sym = find_local_symbol(s);
		
		ExprIndex e = push_expr_arena(1);
		if (sym) {
			if (sym->storage_class == STORAGE_PARAM) {
				expr_arena.data[e] = (Expr) {
					.op = EXPR_PARAM,
					.loc = loc,
					.param_num = sym->param_num
				};
			} else {
				stmt_arena.data[sym->stmt].decl.attrs.is_used = true;
				
				expr_arena.data[e] = (Expr) {
					.op = EXPR_SYMBOL,
					.loc = loc,
					.symbol = sym->stmt
				};
			}
		} else {
			// We'll defer any global identifier resolution
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			ptrdiff_t search = shgeti(labels, name);
			if (search >= 0) {
				expr_arena.data[e] = (Expr) {
					.op = EXPR_SYMBOL,
					.loc = loc,
					.symbol = labels[search].value
				};
			} else {
				expr_arena.data[e] = (Expr) {
					.op = EXPR_UNKNOWN_SYMBOL,
					.loc = loc,
					.unknown_sym = name
				};
			}
		}
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_KW_sizeof ||
			   tokens_get(s)->type == TOKEN_KW_Alignof) {
		tokens_next(s);
		
		bool is_sizeof = tokens_get(s)->type == TOKEN_KW_sizeof;
		
		bool has_paren = false;
		if (tokens_get(s)->type == '(') {
			has_paren = true;
			
			tokens_next(s);
		}
		
		ExprIndex e = push_expr_arena(1);
		if (is_typename(s)) {
			TypeIndex type = parse_typename(s);
			
			expr_arena.data[e] = (Expr) {
				.op = is_sizeof ? EXPR_SIZEOF_T : EXPR_ALIGNOF_T,
				.loc = tokens_get(s)->location,
				.x_of_type = { type }
			};
		} else {
			ExprIndex expr = parse_expr_l14(s);
			
			expr_arena.data[e] = (Expr) {
				.op = is_sizeof ? EXPR_SIZEOF : EXPR_ALIGNOF,
				.loc = tokens_get(s)->location,
				.x_of_expr = { expr }
			};
		}
		
		if (has_paren) expect(s, ')');
		return e;
	} else if (tokens_get(s)->type == TOKEN_FLOAT) {
		Token* t = tokens_get(s);
		double i = parse_float(t->end - t->start, (const char*)t->start);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_FLOAT,
			.loc = loc,
			.float_num = i
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_INTEGER) {
		Token* t = tokens_get(s);
		IntSuffix suffix;
		int64_t i = parse_int(t->end - t->start, (const char*)t->start, &suffix);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_INT,
			.loc = loc,
			.int_num = { i, suffix }
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_STRING_SINGLE_QUOTE) {
		Token* t = tokens_get(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_CHAR,
			.loc = loc,
			.str.start = t->start,
			.str.end = t->end
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
		Token* t = tokens_get(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_STR,
			.loc = loc,
			.str.start = t->start,
			.str.end = t->end
		};
		tokens_next(s);
		
		// TODO(NeGate): Kinda janky and ideally i remove the mallocs and reallocs
		if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
			Token* t = tokens_get(s);
			
			size_t len = 0;
			char* buffer = malloc((t->end - t->start) + 1);
			
			memcpy(&buffer[0], t->start, t->end - t->start);
			len += (t->end - t->start) - 1;
			
			printf("%.*s\n\n", (int) len, buffer);
			
			do {
				t = tokens_get(s);
				buffer = realloc(buffer, len + (t->end - t->start));
				
				memcpy(&buffer[len], t->start + 1, t->end - t->start);
				len += (t->end - t->start) - 2;
				
				printf("%.*s\n\n", (int) len, buffer);
				
				tokens_next(s);
			} while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE);
			
			buffer = realloc(buffer, len + 1);
			buffer[len] = '\"';
			printf("%.*s\n\n", (int) len, buffer);
			
			expr_arena.data[e].str.start = (const unsigned char*)buffer;
			expr_arena.data[e].str.end = (const unsigned char*)(buffer + len);
		}
		
		return e;
	} else {
		generic_error(s, "Could not parse expression!");
	}
}

static ExprIndex parse_expr_l1(TokenStream* restrict s) {
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		
		if (is_typename(s)) {
			TypeIndex type = parse_typename(s);
			expect(s, ')');
			
			if (tokens_get(s)->type == '{') {
				tokens_next(s);
				
				return parse_initializer(s, type);
			} else {
				ExprIndex base = parse_expr_l2(s);
				ExprIndex e = push_expr_arena(1);
				
				expr_arena.data[e] = (Expr) {
					.op = EXPR_CAST,
					.loc = tokens_get(s)->location,
					.cast = { type, base }
				};
				return e;
			}
		}
		
		tokens_prev(s);
	}
	
	ExprIndex e = parse_expr_l0(s);
	
	// after any of the: [] () . ->
	// it'll restart and take a shot at matching another
	// piece of the expression.
	try_again: {
		if (tokens_get(s)->type == '[') {
			ExprIndex base = e;
			e = push_expr_arena(1);
			
			tokens_next(s);
			ExprIndex index = parse_expr(s);
			expect(s, ']');
			
			expr_arena.data[e] = (Expr) {
				.op = EXPR_SUBSCRIPT,
				.loc = tokens_get(s)->location,
				.subscript = { base, index }
			};
			goto try_again;
		}
		
		// Pointer member access
		if (tokens_get(s)->type == TOKEN_ARROW) {
			tokens_next(s);
			if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
				generic_error(s, "Expected identifier after member access a.b");
			}
			
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			ExprIndex base = e;
			e = push_expr_arena(1);
			expr_arena.data[e] = (Expr) {
				.op = EXPR_ARROW,
				.loc = tokens_get(s)->location,
				.arrow = { .base = base, .name = name }
			};
			
			tokens_next(s);
			goto try_again;
		}
		
		// Member access
		if (tokens_get(s)->type == '.') {
			tokens_next(s);
			if (tokens_get(s)->type != TOKEN_IDENTIFIER) {
				generic_error(s, "Expected identifier after member access a.b");
			}
			
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			ExprIndex base = e;
			e = push_expr_arena(1);
			expr_arena.data[e] = (Expr) {
				.op = EXPR_DOT,
				.loc = tokens_get(s)->location,
				.dot = { .base = base, .name = name }
			};
			
			tokens_next(s);
			goto try_again;
		}
		
		// Function call
		if (tokens_get(s)->type == '(') {
			tokens_next(s);
			
			ExprIndex target = e;
			e = push_expr_arena(1);
			
			size_t param_count = 0;
			void* params = tls_save();
			
			while (tokens_get(s)->type != ')') {
				if (param_count) {
					expect(s, ',');
				}
				
				// NOTE(NeGate): This is a funny little work around because
				// i don't wanna parse the comma operator within the expression
				// i wanna parse it here so we just skip it.
				*((ExprIndex*) tls_push(sizeof(ExprIndex))) = parse_expr_l14(s);
				param_count++;
			}
			
			if (tokens_get(s)->type != ')') {
				generic_error(s, "Unclosed parameter list!");
			}
			tokens_next(s);
			
			// Copy parameter refs into more permanent storage
			ExprIndex* param_start = arena_alloc(param_count * sizeof(ExprIndex), _Alignof(ExprIndex));
			memcpy(param_start, params, param_count * sizeof(ExprIndex));
			
			expr_arena.data[e] = (Expr) {
				.op = EXPR_CALL,
				.loc = tokens_get(s)->location,
				.call = { target, param_count, param_start }
			};
			
			tls_restore(params);
			goto try_again;
		}
		
		// post fix, you can only put one and just after all the other operators
		// in this precendence.
		if (tokens_get(s)->type == TOKEN_INCREMENT || tokens_get(s)->type == TOKEN_DECREMENT) {
			bool is_inc = tokens_get(s)->type == TOKEN_INCREMENT;
			tokens_next(s);
			
			ExprIndex src = e;
			
			e = push_expr_arena(1);
			expr_arena.data[e] = (Expr) {
				.op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
				.loc = tokens_get(s)->location,
				.unary_op.src = src
			};
		}
		
		return e;
	}
}

// deref* address& negate-
static ExprIndex parse_expr_l2(TokenStream* restrict s) {
	// TODO(NeGate): Convert this code into a loop... please?
	SourceLocIndex loc = tokens_get(s)->location;
	
	if (tokens_get(s)->type == '!') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_LOGICAL_NOT,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == TOKEN_DOUBLE_EXCLAMATION) {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_CAST,
			.loc = loc,
			.cast = { TYPE_BOOL, value }
		};
		return e;
	} else if (tokens_get(s)->type == '-') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_NEGATE,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '~') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_NOT,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '+') {
		tokens_next(s);
		return parse_expr_l2(s);
	} else if (tokens_get(s)->type == TOKEN_INCREMENT) {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_PRE_INC,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == TOKEN_DECREMENT) {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_PRE_DEC,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '&') {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_ADDR,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	}
	
	int derefs = 0;
	while (tokens_get(s)->type == '*') {
		tokens_next(s);
		derefs++;
	}
	
	ExprIndex e = parse_expr_l1(s);
	
	while (derefs--) {
		ExprIndex base = e;
		e = push_expr_arena(1);
		
		expr_arena.data[e] = (Expr) {
			.op = EXPR_DEREF,
			.loc = tokens_get(s)->location,
			.unary_op.src = base
		};
	}
	
	return e;
}

// * / %
static ExprIndex parse_expr_l3(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l2(s);
	
	while (tokens_get(s)->type == TOKEN_TIMES ||
		   tokens_get(s)->type == TOKEN_SLASH ||
		   tokens_get(s)->type == TOKEN_PERCENT) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_TIMES: op = EXPR_TIMES; break;
			case TOKEN_SLASH: op = EXPR_SLASH; break;
			case TOKEN_PERCENT: op = EXPR_PERCENT; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l2(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// + -
static ExprIndex parse_expr_l4(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l3(s);
	
	while (tokens_get(s)->type == TOKEN_PLUS ||
		   tokens_get(s)->type == TOKEN_MINUS) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_PLUS: op = EXPR_PLUS; break;
			case TOKEN_MINUS: op = EXPR_MINUS; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l3(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// + -
static ExprIndex parse_expr_l5(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l4(s);
	
	while (tokens_get(s)->type == TOKEN_LEFT_SHIFT ||
		   tokens_get(s)->type == TOKEN_RIGHT_SHIFT) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_LEFT_SHIFT: op = EXPR_SHL; break;
			case TOKEN_RIGHT_SHIFT: op = EXPR_SHR; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l4(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// >= > <= <
static ExprIndex parse_expr_l6(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l5(s);
	
	while (tokens_get(s)->type == TOKEN_GREATER_EQUAL ||
		   tokens_get(s)->type == TOKEN_LESS_EQUAL || 
		   tokens_get(s)->type == TOKEN_GREATER ||
		   tokens_get(s)->type == TOKEN_LESS) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_LESS:          op = EXPR_CMPLT; break;
			case TOKEN_LESS_EQUAL:    op = EXPR_CMPLE; break;
			case TOKEN_GREATER:       op = EXPR_CMPGT; break;
			case TOKEN_GREATER_EQUAL: op = EXPR_CMPGE; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l5(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// == !=
static ExprIndex parse_expr_l7(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l6(s);
	
	while (tokens_get(s)->type == TOKEN_NOT_EQUAL ||
		   tokens_get(s)->type == TOKEN_EQUALITY) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = tokens_get(s)->type == TOKEN_EQUALITY ? EXPR_CMPEQ : EXPR_CMPNE;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l6(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// &
static ExprIndex parse_expr_l8(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l7(s);
	
	while (tokens_get(s)->type == '&') {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_AND;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l7(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ^
static ExprIndex parse_expr_l9(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l8(s);
	
	while (tokens_get(s)->type == '^') {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_XOR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l8(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// |
static ExprIndex parse_expr_l10(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l9(s);
	
	while (tokens_get(s)->type == '|') {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_OR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l9(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// &&
static ExprIndex parse_expr_l11(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l10(s);
	
	while (tokens_get(s)->type == TOKEN_DOUBLE_AND) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_LOGICAL_AND;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l10(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ||
static ExprIndex parse_expr_l12(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l11(s);
	
	while (tokens_get(s)->type == TOKEN_DOUBLE_OR) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_LOGICAL_OR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l11(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ternary
static ExprIndex parse_expr_l13(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l12(s);
	
	while (tokens_get(s)->type == '?') {
		SourceLocIndex loc = tokens_get(s)->location;
		tokens_next(s);
		
		ExprIndex mhs = parse_expr(s);
		
		expect(s, ':');
		
		ExprIndex rhs = parse_expr_l12(s);
		
		ExprIndex e = push_expr_arena(1);
		expr_arena.data[e] = (Expr) {
			.op = EXPR_TERNARY,
			.loc = loc,
			.ternary_op = { lhs, mhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// = += -= *= /= %= <<= >>= &= ^= |=
static ExprIndex parse_expr_l14(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l13(s);
	
	while (tokens_get(s)->type == TOKEN_ASSIGN ||
		   tokens_get(s)->type == TOKEN_PLUS_EQUAL ||
		   tokens_get(s)->type == TOKEN_MINUS_EQUAL ||
		   tokens_get(s)->type == TOKEN_TIMES_EQUAL ||
		   tokens_get(s)->type == TOKEN_SLASH_EQUAL ||
		   tokens_get(s)->type == TOKEN_PERCENT_EQUAL ||
		   tokens_get(s)->type == TOKEN_AND_EQUAL ||
		   tokens_get(s)->type == TOKEN_OR_EQUAL ||
		   tokens_get(s)->type == TOKEN_XOR_EQUAL ||
		   tokens_get(s)->type == TOKEN_LEFT_SHIFT_EQUAL ||
		   tokens_get(s)->type == TOKEN_RIGHT_SHIFT_EQUAL) {
		ExprIndex e = push_expr_arena(1);
		
		ExprOp op;
		switch (tokens_get(s)->type) {
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
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l13(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

static ExprIndex parse_expr_l15(TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l14(s);
	
	while (tokens_get(s)->type == TOKEN_COMMA) {
		ExprIndex e = push_expr_arena(1);
		ExprOp op = EXPR_COMMA;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l14(s);
		expr_arena.data[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

static ExprIndex parse_expr(TokenStream* restrict s) {
	return parse_expr_l15(s);
}

////////////////////////////////
// TYPES
////////////////////////////////
static TypeIndex parse_type_suffix(TokenStream* restrict s, TypeIndex type, Atom name);

static Decl parse_declarator(TokenStream* restrict s, TypeIndex type) {
	// handle calling convention
	// TODO(NeGate): Actually pass these to the AST
	parse_another_qualifier2: {
		switch (tokens_get(s)->type) {
			case TOKEN_KW_cdecl:
			case TOKEN_KW_stdcall:
			tokens_next(s);
			goto parse_another_qualifier2;
			default: break;
		}
	}
	
	// handle pointers
	while (tokens_get(s)->type == '*') {
		type = new_pointer(type);
		tokens_next(s);
		
		parse_another_qualifier: {
			switch (tokens_get(s)->type) {
				case TOKEN_KW_restrict: {
					type_arena.data[type].is_ptr_restrict = true;
					tokens_next(s);
					goto parse_another_qualifier;
				}
				
				case TOKEN_KW_const:
				case TOKEN_KW_volatile:
				case TOKEN_KW_cdecl:
				case TOKEN_KW_stdcall: {
					tokens_next(s);
					goto parse_another_qualifier;
				}
				
				default: break;
			}
		}
	}
	
	if (tokens_get(s)->type == '(') {
		// TODO(NeGate): I don't like this code...
		// it essentially just skips over the stuff in the
		// parenthesis to do the suffix then comes back
		// for the parenthesis after wards, restoring back to
		// the end of the declarator when it's done.
		//
		// should be right after the (
		tokens_next(s);
		size_t saved = s->current;
		
		parse_declarator(s, TYPE_NONE);
		
		expect(s, ')');
		type = parse_type_suffix(s, type, NULL);
		
		size_t saved_end = s->current;
		s->current = saved;
		
		Decl d = parse_declarator(s, type);
		
		// inherit name
		// TODO(NeGate): I'm not sure if this is correct ngl
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
		
		s->current = saved_end;
		return d;
	}
	
	Atom name = NULL;
	Token* t = tokens_get(s);
	if (t->type == TOKEN_IDENTIFIER) {
		name = atoms_put(t->end - t->start, t->start);
		tokens_next(s);
	}
	
	type = parse_type_suffix(s, type, name);
	return (Decl){ type, name };
}

// it's like a declarator with a skin fade,
// int*        int[16]       const char* restrict
static TypeIndex parse_abstract_declarator(TokenStream* restrict s, TypeIndex type) {
	// handle calling convention
	// TODO(NeGate): Actually pass these to the AST
	parse_another_qualifier2: {
		switch (tokens_get(s)->type) {
			case TOKEN_KW_cdecl:
			case TOKEN_KW_stdcall:
			tokens_next(s);
			goto parse_another_qualifier2;
			default: break;
		}
	}
	
	// handle pointers
	while (tokens_get(s)->type == '*') {
		type = new_pointer(type);
		tokens_next(s);
		
		// TODO(NeGate): parse qualifiers
		parse_another_qualifier: {
			switch (tokens_get(s)->type) {
				case TOKEN_KW_const:
				case TOKEN_KW_volatile:
				case TOKEN_KW_cdecl:
				case TOKEN_KW_stdcall:
				tokens_next(s);
				goto parse_another_qualifier;
				default: break;
			}
		}
	}
	
	if (tokens_get(s)->type == '(') {
		// TODO(NeGate): I don't like this code...
		// it essentially just skips over the stuff in the
		// parenthesis to do the suffix then comes back
		// for the parenthesis after wards, restoring back to
		// the end of the declarator when it's done.
		//
		// should be right after the (
		tokens_next(s);
		size_t saved = s->current;
		
		parse_abstract_declarator(s, TYPE_NONE);
		
		expect(s, ')');
		type = parse_type_suffix(s, type, NULL);
		
		size_t saved_end = s->current;
		s->current = saved;
		
		TypeIndex d = parse_abstract_declarator(s, type);
		s->current = saved_end;
		return d;
	}
	
	Atom name = NULL;
	Token* t = tokens_get(s);
	if (t->type == TOKEN_IDENTIFIER) {
		name = atoms_put(t->end - t->start, t->start);
		tokens_next(s);
	}
	
	type = parse_type_suffix(s, type, name);
	return type;
}

static TypeIndex parse_typename(TokenStream* restrict s) {
	// TODO(NeGate): Check if attributes are set, they shouldn't
	// be in this context.
	Attribs attr = { 0 };
	TypeIndex type = parse_declspec(s, &attr);
	return parse_abstract_declarator(s, type);
}

static TypeIndex parse_type_suffix(TokenStream* restrict s, TypeIndex type, Atom name) {
	// type suffixes like array [] and function ()
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		
		TypeIndex return_type = type;
		
		type = new_func();
		type_arena.data[type].func.name = name;
		type_arena.data[type].func.return_type = return_type;
		
		if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
			tokens_next(s);
			tokens_next(s);
			
			type_arena.data[type].func.param_list = 0;
			type_arena.data[type].func.param_count = 0;
			return type;
		}
		
		size_t param_count = 0;
		void* params = tls_save();
		bool has_varargs = false;
		
		while (tokens_get(s)->type != ')') {
			if (param_count) {
				expect(s, ',');
			}
			
			if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
				tokens_next(s);
				
				has_varargs = true;
				break;
			}
			
			Attribs arg_attr = { 0 };
			TypeIndex arg_base_type = parse_declspec(s, &arg_attr);
			
			Decl param_decl = parse_declarator(s, arg_base_type);
			TypeIndex param_type = param_decl.type;
			
			if (type_arena.data[param_type].kind == KIND_ARRAY) {
				// Array parameters are desugared into pointers
				param_type = new_pointer(type_arena.data[param_type].array_of);
			} else if (type_arena.data[param_type].kind == KIND_FUNC) {
				// Function parameters are desugared into pointers
				param_type = new_pointer(param_type);
			}
			
			// TODO(NeGate): Error check that no attribs are set
			*((Param*)tls_push(sizeof(Param))) = (Param) {
				.type = param_type,
				.name = param_decl.name
			};
			param_count++;
		}
		
		if (tokens_get(s)->type != ')') {
			generic_error(s, "Unclosed parameter list!");
		}
		tokens_next(s);
		
		ParamIndex start = push_param_arena(param_count);
		memcpy(&param_arena.data[start], params, param_count * sizeof(Param));
		
		type_arena.data[type].func.param_list = start;
		type_arena.data[type].func.param_count = param_count;
		type_arena.data[type].func.has_varargs = has_varargs;
		
		tls_restore(params);
	} else if (tokens_get(s)->type == '[') {
		do {
			tokens_next(s);
			
			long long count;
			if (tokens_get(s)->type == ']') {
				count = 0; 
				tokens_next(s);
			} else {
				count = parse_const_expr(s);
				expect(s, ']');
			}
			
			type = new_array(type, count);
		} while (tokens_get(s)->type == '[');
	}
	
	return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static TypeIndex parse_declspec(TokenStream* restrict s, Attribs* attr) {
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
		TknType tkn_type = tokens_get(s)->type;
		switch (tkn_type) {
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
			case TOKEN_KW_extern: attr->is_extern = true; break;
			case TOKEN_KW_Thread_local: attr->is_tls = true; break;
			
			case TOKEN_KW_cdecl: break;
			case TOKEN_KW_stdcall: break;
			
			case TOKEN_KW_Atomic: is_atomic = true; break;
			case TOKEN_KW_const: is_const = true; break;
			case TOKEN_KW_volatile: break;
			case TOKEN_KW_auto: break;
			
			case TOKEN_KW_struct:
			case TOKEN_KW_union: {
				if (counter) goto done;
				tokens_next(s);
				
				bool is_union = tkn_type == TOKEN_KW_union;
				
				while (skip_over_declspec(s)) {
					printf("Btw don't forget about declspecs...\n");
				}
				
				Atom name = NULL;
				Token* t = tokens_get(s);
				if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
					name = atoms_put(t->end - t->start, t->start);
					tokens_next(s);
				}
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					type = name ? find_incomplete_type((char*)name) : 0;
					if (type) {
						// can't re-complete a struct
						assert(type_arena.data[type].is_incomplete);
					} else {
						type = new_record(is_union);
						type_arena.data[type].is_incomplete = false;
						type_arena.data[type].record.name = name;
						
						if (name && strcmp((const char*)name, "__m128") == 0) {
							type_arena.data[type].record.intrin_type = tb_vector_type(TB_F32, 4);
						} else {
							type_arena.data[type].record.intrin_type = TB_TYPE_VOID;
						}
						
						// can't forward decl unnamed records so we
						// don't track it
						if (name) shput(incomplete_types, name, type);
					}
					counter += OTHER;
					
					size_t member_count = 0;
					Member* members = tls_save();
					
					// for unions this just represents the max size
					int offset = 0;
					
					// struct/union are aligned to the biggest member alignment
					int align = 0;
					
					while (tokens_get(s)->type != '}') {
						Attribs member_attr = { 0 };
						TypeIndex member_base_type = parse_declspec(s, &member_attr);
						
						// TODO(NeGate): Kinda ugly
						// don't expect one the first time
						bool expect_comma = false;
						while (tokens_get(s)->type != ';') {
							if (expect_comma) {
								expect(s, ',');
							} else expect_comma = true;
							
							Decl decl = parse_declarator(s, member_base_type);
							TypeIndex member_type = decl.type;
							
							if (type_arena.data[member_type].kind == KIND_FUNC) {
								generic_error(s, "Naw dawg");
							}
							
							int member_align = type_arena.data[member_type].align;
							int member_size = type_arena.data[member_type].size;
							if (!is_union) {
								offset = align_up(offset, member_align);
							}
							
							// TODO(NeGate): Error check that no attribs are set
							tls_push(sizeof(Member));
							
							Member* member = &members[member_count++];
							*member = (Member) {
								.type = member_type,
								.name = decl.name,
								.offset = is_union ? 0 : offset,
								.align = member_align
							};
							
							if (tokens_get(s)->type == ':') {
								tokens_next(s);
								
								// TODO(NeGate): Handle bitfields a lot better
								member->is_bitfield = true;
								member->bit_offset = 0;
								member->bit_width = parse_const_expr(s);
							}
							
							if (is_union) {
								if (member_size > offset) offset = member_size;
							} else {
								offset += member_size;
							}
							
							if (member_align > align) align = member_align;
						}
						
						expect(s, ';');
					}
					
					if (tokens_get(s)->type != '}') {
						generic_error(s, "Unclosed member list!");
					}
					
					offset = align_up(offset, align);
					type_arena.data[type].size = offset;
					type_arena.data[type].align = align;
					
					MemberIndex start = push_member_arena(member_count);
					memcpy(&member_arena.data[start], members, member_count * sizeof(Member));
					
					type_arena.data[type].record.kids_start = start;
					type_arena.data[type].record.kids_end = start + member_count;
					
					tls_restore(members);
				} else {
					// TODO(NeGate): must be a forward decl, handle it
					if (name == NULL) generic_error(s, "Cannot have unnamed forward struct reference.");
					
					type = find_incomplete_type((char*)name);
					if (!type) {
						type = new_record(is_union);
						type_arena.data[type].record.name = name;
						type_arena.data[type].is_incomplete = true;
						
						shput(incomplete_types, name, type);
					}
					counter += OTHER;
					
					// push back one because we push it forward one later but 
					// shouldn't
					tokens_prev(s);
				}
				break;
			}
			
			case TOKEN_KW_enum: {
				if (counter) goto done;
				tokens_next(s);
				
				Token* t = tokens_get(s);
				Atom name = NULL;
				if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
					name = atoms_put(t->end - t->start, t->start);
					tokens_next(s);
				}
				
				type = new_enum();
				type_arena.data[type].enumerator.name = name;
				
				expect(s, '{');
				type_arena.data[type].enumerator.start = enum_entry_arena.count;
				
				// starts at zero and after any entry it increments
				// you can override it by using:
				// identifier = int-const-expr
				int cursor = 0;
				
				while (tokens_get(s)->type != '}') {
					EnumEntryIndex e = push_enum_entry_arena(1);
					
					// parse name
					Token* t = tokens_get(s);
					if (t->type != TOKEN_IDENTIFIER) {
						generic_error(s, "expected identifier for enum name entry.");
					}
					
					enum_entry_arena.data[e].name = atoms_put(t->end - t->start, t->start);
					tokens_next(s);
					
					if (tokens_get(s)->type == '=') {
						tokens_next(s);
						
						cursor = parse_const_expr(s);
					}
					
					enum_entry_arena.data[e].value = cursor++;
					if (tokens_get(s)->type == ',') tokens_next(s);
				}
				
				if (tokens_get(s)->type != '}') {
					generic_error(s, "Unclosed enum list!");
				}
				
				type_arena.data[type].enumerator.end = enum_entry_arena.count;
				break;
			}
			
			case TOKEN_IDENTIFIER: {
				if (counter) goto done;
				
				Token* t = tokens_get(s);
				Atom name = atoms_put(t->end - t->start, t->start);
				
				ptrdiff_t search = shgeti(typedefs, name);
				
				if (search >= 0) {
					type = typedefs[search].value;
					counter += OTHER;
					break;
				}
				
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
			generic_error(s, "invalid type");
			break;
		}
		
		tokens_next(s);
	} while (true);
	
	done:
	if (type == 0) {
		generic_error(s, "Unknown typename");
	}
	
	if (is_atomic || is_const) {
		type = copy_type(type);
		type_arena.data[type].is_atomic = is_atomic;
		type_arena.data[type].is_const = is_const;
	}
	
	return type;
}

static bool is_typename(TokenStream* restrict s) {
	Token* t = tokens_get(s);
	
	switch (t->type) {
		case TOKEN_KW_void:
		case TOKEN_KW_char:
		case TOKEN_KW_short:
		case TOKEN_KW_int:
		case TOKEN_KW_long:
		case TOKEN_KW_float:
		case TOKEN_KW_double:
		case TOKEN_KW_Bool:
		case TOKEN_KW_signed:
		case TOKEN_KW_unsigned:
		case TOKEN_KW_struct:
		case TOKEN_KW_union:
		case TOKEN_KW_enum:
		case TOKEN_KW_extern:
		case TOKEN_KW_static:
		case TOKEN_KW_typedef:
		case TOKEN_KW_inline:
		case TOKEN_KW_const:
		case TOKEN_KW_volatile:
		case TOKEN_KW_Thread_local:
		case TOKEN_KW_Atomic:
		case TOKEN_KW_auto:
		case TOKEN_KW_cdecl:
		case TOKEN_KW_stdcall:
		return true;
		
		case TOKEN_IDENTIFIER: {
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			ptrdiff_t search = shgeti(typedefs, name);
			return (search >= 0);
		}
		default:
		return false;
	}
}

// TODO(NeGate): Correctly handle const-expr
static int parse_const_expr_l0(TokenStream* restrict s) {
	Token* restrict t = tokens_get(s);
	
	if (t->type == TOKEN_INTEGER) {
		IntSuffix suffix;
		int64_t i = parse_int(t->end - t->start, (const char*)t->start, &suffix);
		
		tokens_next(s);
		return i;
	}
	
	generic_error(s, "Could not parse constant expression");
}

static intmax_t parse_const_expr(TokenStream* restrict s) {
	ConstValue v = const_eval(parse_expr_l14(s));
	intmax_t vi = v.signed_value;
	
	if (!v.is_signed && vi != v.unsigned_value) {
		generic_error(s, "Constant integer cannot be represented as signed integer.");
	}
	
	return vi;
}

////////////////////////////////
// ERRORS
////////////////////////////////
static _Noreturn void generic_error(TokenStream* restrict s, const char* msg) {
	Token* t = tokens_get(s);
	SourceLoc* loc = &s->line_arena[t->location];
	
	printf("%s:%d: error: %s\n", loc->file, loc->line, msg);
	abort();
}

static void expect(TokenStream* restrict s, char ch) {
	if (tokens_get(s)->type != ch) {
		Token* t = tokens_get(s);
		SourceLoc* loc = &s->line_arena[t->location];
		
		printf("%s:%d: error: expected '%c' got '%.*s'", loc->file, loc->line, ch, (int)(t->end - t->start), t->start);
		abort();
	}
	
	tokens_next(s);
}
