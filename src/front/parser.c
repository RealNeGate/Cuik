// NOTE(NeGate): Something to note is that 
//  a = b
// has an unspecified eval order so if you do
//  a[i] = parse(...)
// and it can somehow resize a then make sure to split it up
//  e = parse(...)
//  a[i] = e
#include "parser.h"

typedef struct IncompleteType {
	Atom key;
	TypeIndex value;
} IncompleteType;

typedef struct SymbolEntry {
	Atom key;
	Symbol value;
} SymbolEntry;

typedef struct TypedefEntry {
	Atom key;
	TypeIndex value;
} TypedefEntry;

typedef struct LabelEntry {
	Atom key;
	StmtIndex value;
} LabelEntry;

_Thread_local static int local_symbol_count = 0;
_Thread_local static Symbol local_symbols[64 * 1024];

_Thread_local static IncompleteType* incomplete_types; // stb_ds hash map
_Thread_local static SymbolEntry* global_symbols; // stb_ds hash map
_Thread_local static TypedefEntry* typedefs; // stb_ds hash map
_Thread_local static LabelEntry* labels; // stb_ds hash map

_Thread_local static StmtIndex current_switch_or_case;
_Thread_local static StmtIndex current_breakable;
_Thread_local static StmtIndex current_continuable;

// two simple temporary buffers to represent type_as_string results
static _Thread_local char temp_string0[256], temp_string1[256];

static void expect(TokenStream* restrict s, char ch);
static Symbol* find_local_symbol(TokenStream* restrict s);
static Symbol* find_global_symbol(char* name);

static StmtIndex parse_stmt(TranslationUnit* tu, TokenStream* restrict s);
static StmtIndex parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s);
static StmtIndex parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s);
static void parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static TypeIndex parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type);
static TypeIndex parse_abstract_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type);
static TypeIndex parse_typename(TranslationUnit* tu, TokenStream* restrict s);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static ExprIndex parse_expr_l14(TranslationUnit* tu, TokenStream* restrict s);
static ExprIndex parse_expr(TranslationUnit* tu, TokenStream* restrict s);
static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s);
static ExprIndex parse_initializer(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type);

static bool is_typename(TokenStream* restrict s);

static _Noreturn void generic_error(TokenStream* restrict s, const char* msg);
static void generic_warn(TokenStream* restrict s, const char* msg);

static int align_up(int a, int b) { 
	if (b == 0) return 0;
	
	return a + (b - (a % b)) % b;
}

static StmtIndex make_stmt(TranslationUnit* tu, TokenStream* restrict s, StmtOp op) {
	Stmt stmt = {
		.op = op,
		.loc = tokens_get(s)->location
	};
	big_array_put(tu->stmts, stmt);
	return big_array_length(tu->stmts) - 1;
}

static ExprIndex make_expr(TranslationUnit* tu) {
	big_array_put_uninit(tu->exprs, 1);
	return big_array_length(tu->exprs) - 1;
}

TranslationUnit parse_file(TokenStream* restrict s) {
	TranslationUnit tu = {};
	tu.types = big_array_create(Type);
	tu.members = big_array_create(Member);
	tu.params = big_array_create(Param);
	tu.enum_entries = big_array_create(EnumEntry);
	tu.stmts = big_array_create(Stmt);
	tu.exprs = big_array_create(Expr);
	
	init_types(&tu);
	tls_init();
	
	////////////////////////////////
	// Parse translation unit
	////////////////////////////////
	while (tokens_get(s)->type) {
		while (tokens_get(s)->type == ';') tokens_next(s);
		
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
		
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(&tu, s, &attr);
		attr.is_root = !(attr.is_extern || attr.is_static || attr.is_inline);
		
		if (attr.is_typedef) {
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (tokens_get(s)->type != ';') {
				if (expect_comma) {
					expect(s, ',');
				} else expect_comma = true;
				
				Decl decl = parse_declarator(&tu, s, type);
				assert(decl.name);
				
				shput(typedefs, decl.name, decl.type);
			}
			
			expect(s, ';');
		} else {
			if (tokens_get(s)->type == ';') {
				tokens_next(s);
				continue;
			}
			
			Decl decl = parse_declarator(&tu, s, type);
			
			if (tu.types[decl.type].kind == KIND_FUNC) {
				// function
				Symbol* sym = find_global_symbol((char*)decl.name);
				
				StmtIndex n;
				bool is_redefine = false;
				bool is_redefining_body = false;
				if (sym) {
					is_redefine = true;
					
					StorageClass sclass = attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC;
					sym->storage_class = sclass;
					
					// convert forward decl into proper function
					n = sym->stmt;
					if (tu.stmts[n].op == STMT_FUNC_DECL && !attr.is_inline) {
						is_redefining_body = true;
					}
					
					if (!type_equal(&tu, tu.stmts[n].decl.type, decl.type)) {
						type_as_string(&tu, sizeof(temp_string0), temp_string0, decl.type);
						type_as_string(&tu, sizeof(temp_string1), temp_string1, tu.stmts[n].decl.type);
						
						report(REPORT_ERROR, &s->line_arena[decl.loc], "conflicting types for '%s'; currently '%s'", decl.name, temp_string0);
						report(REPORT_INFO, &s->line_arena[tu.stmts[n].loc], "before it was '%s'", temp_string1);
					}
					
					tu.stmts[n].loc = decl.loc;
				} else {
					// New symbol
					n = make_stmt(&tu, s, STMT_DECL);
					tu.stmts[n].loc = decl.loc;
					tu.stmts[n].decl = (struct StmtDecl){
						.type = decl.type,
						.name = decl.name,
						.attrs = attr,
						.initial = (StmtIndex)0
					};
					tu.stmts[n].decl.attrs.is_root = false;
					
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
					} else if (strcmp((const char*)decl.name, "WinMain") == 0) {
						settings.using_winmain = true;
					}
					
					ParamIndex param_list = tu.types[decl.type].func.param_list;
					ParamIndex param_count = tu.types[decl.type].func.param_count;
					
					assert(local_symbol_count == 0);
					if (param_count >= INT16_MAX) {
						report(REPORT_ERROR, &s->line_arena[decl.loc], "Function parameter count cannot exceed %d (got %d)", param_count, MAX_LOCAL_SYMBOLS);
						abort();
					}
					
					for (size_t i = 0; i < param_count; i++) {
						Param* p = &tu.params[param_list + i];
						
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
					
					ExprIndex starting_point = big_array_length(tu.exprs);
					StmtIndex body = parse_compound_stmt(&tu, s);
					
					tu.stmts[n].op = STMT_FUNC_DECL;
					tu.stmts[n].decl.initial = (StmtIndex)body;
					tu.stmts[n].decl.attrs = attr;
					
					ExprIndex symbol_list_start = 0;
					ExprIndex symbol_list_end = 0;
					
					// resolve any unresolved label references
					for (size_t i = starting_point, count = big_array_length(tu.exprs); i < count; i++) {
						if (tu.exprs[i].op == EXPR_SYMBOL ||
							tu.exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
							if (symbol_list_start == 0) {
								// initialize chain
								symbol_list_start = symbol_list_end = i;
							} else {
								// append
								tu.exprs[symbol_list_end].next_symbol_in_chain = i;
								tu.exprs[i].next_symbol_in_chain = 0;
								symbol_list_end = i;
							}
						}
						
						if (tu.exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
							const unsigned char* name = tu.exprs[i].unknown_sym;
							
							ptrdiff_t search = shgeti(labels, name);
							if (search >= 0) {
								tu.exprs[i].op = EXPR_SYMBOL;
								tu.exprs[i].symbol = labels[search].value;
							}
						}
					}
					
					if (symbol_list_end) {
						tu.exprs[symbol_list_end].next_symbol_in_chain = 0;
					}
					
					tu.stmts[body].compound.first_symbol = symbol_list_start;
					shfree(labels);
				} else if (tokens_get(s)->type == ';') {
					// Forward decl
					tokens_next(s);
				} else {
					abort();
				}
				
				if (!is_redefine) {
					arrput(tu.top_level_stmts, n);
				}
				local_symbol_count = 0;
			} else {
				// Normal decls
				if (!decl.name) {
					generic_error(s, "declaration is missing a name!");
				}
				
				StmtIndex n = make_stmt(&tu, s, STMT_GLOBAL_DECL);
				tu.stmts[n].loc = decl.loc;
				tu.stmts[n].decl = (struct StmtDecl){
					.type = decl.type,
					.name = decl.name,
					.attrs = attr
				};
				arrput(tu.top_level_stmts, n);
				
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
						
						tu.stmts[n].decl.initial = parse_initializer(&tu, s, TYPE_NONE);
					} else {
						tu.stmts[n].decl.initial = parse_expr_l14(&tu, s);
					}
				}
				
				if (tokens_get(s)->type == ',') {
					while (tokens_get(s)->type != ';') {
						expect(s, ',');
						
						Decl decl = parse_declarator(&tu, s, type);
						assert(decl.name);
						
						StmtIndex n = make_stmt(&tu, s, STMT_GLOBAL_DECL);
						tu.stmts[n].loc = decl.loc;
						tu.stmts[n].decl = (struct StmtDecl){
							.type = decl.type,
							.name = decl.name,
							.attrs = attr
						};
						arrput(tu.top_level_stmts, n);
						
						Symbol sym = (Symbol){
							.name = decl.name,
							.type = decl.type,
							.storage_class = attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL,
							.stmt = n
						};
						shput(global_symbols, decl.name, sym);
						
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							ExprIndex initial;
							if (tokens_get(s)->type == '{') {
								tokens_next(s);
								
								initial = parse_initializer(&tu, s, TYPE_NONE);
							} else {
								initial = parse_expr_l14(&tu, s);
							}
							tu.stmts[n].decl.initial = initial;
						}
					}
				}
				
				expect(s, ';');
			}
		}
	}
	
	// NOTE(NeGate): This is a Cuik extension, it allows normal symbols
	// like functions to declared out of order.
	for (size_t i = 1, count = big_array_length(tu.exprs); i < count; i++) {
		if (tu.exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
			if (!resolve_unknown_symbol(&tu, i)) {
				// try enum names
				const unsigned char* name = tu.exprs[i].unknown_sym;
				
				// NOTE(NeGate): this might be slow
				for (size_t j = 1, count = big_array_length(tu.enum_entries); j < count; j++) {
					if (cstr_equals(name, tu.enum_entries[j].name)) {
						int value = tu.enum_entries[j].value;
						ExprIndex chain = tu.exprs[i].next_symbol_in_chain;
						
						tu.exprs[i].op = EXPR_ENUM;
						tu.exprs[i].enum_val = (struct ExprEnum){ value, chain };
						goto success;
					}
				}
				
				SourceLoc* loc = &s->line_arena[tu.exprs[i].loc];
				report(REPORT_ERROR, loc, "could not resolve symbol: %s", name);
			}
		}
		success:;
	}
	
	local_symbol_count = 0;
	shfree(global_symbols);
	return tu;
}

StmtIndex resolve_unknown_symbol(TranslationUnit* tu, StmtIndex i) {
	Symbol* sym = find_global_symbol((char*)tu->exprs[i].unknown_sym);
	if (!sym) return 0;
	
	// Parameters are local and a special case how tf
	assert(sym->storage_class != STORAGE_PARAM);
	
	tu->exprs[i].op = EXPR_SYMBOL;
	tu->exprs[i].symbol = sym->stmt;
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
static StmtIndex parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s) {
	// mark when the local scope starts
	int saved = local_symbol_count;
	
	StmtIndex node = make_stmt(tu, s, STMT_COMPOUND);
	
	size_t body_count = 0; // He be fuckin
	void* body = tls_save();
	
	while (tokens_get(s)->type != '}') {
		while (tokens_get(s)->type == ';') tokens_next(s);
		
		StmtIndex stmt = parse_stmt(tu, s);
		if (stmt) {
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = stmt;
			body_count++;
		} else {
			parse_decl_or_expr(tu, s, &body_count);
		}
	}
	expect(s, '}');
	local_symbol_count = saved;
	
	StmtIndex* stmt_array = arena_alloc(body_count * sizeof(StmtIndex), _Alignof(StmtIndex));
	memcpy(stmt_array, body, body_count * sizeof(StmtIndex));
	
	tu->stmts[node].compound = (struct StmtCompound) {
		.kids = stmt_array,
		.kids_count = body_count
	};
	
	tls_restore(body);
	return node;
}

// TODO(NeGate): Doesn't handle declarators or expression-statements
static StmtIndex parse_stmt(TranslationUnit* tu, TokenStream* restrict s) {
	if (tokens_get(s)->type == '{') {
		tokens_next(s);
		return parse_compound_stmt(tu, s);
	} else if (tokens_get(s)->type == TOKEN_KW_return) {
		tokens_next(s);
		
		ExprIndex e = 0;
		if (tokens_get(s)->type != ';') {
			e = parse_expr(tu, s);
		}
		
		StmtIndex n = make_stmt(tu, s, STMT_RETURN);
		tu->stmts[n].return_ = (struct StmtReturn){
			.expr = e
		};
		
		expect(s, ';');
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_if) {
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_IF);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(tu, s);
		expect(s, ')');
		
		StmtIndex body = parse_stmt_or_expr(tu, s);
		assert(body);
		
		StmtIndex next = 0;
		if (tokens_get(s)->type == TOKEN_KW_else) {
			tokens_next(s);
			next = parse_stmt_or_expr(tu, s);
		}
		
		tu->stmts[n].if_ = (struct StmtIf){
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_switch) {
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_SWITCH);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(tu, s);
		expect(s, ')');
		
		tu->stmts[n].switch_ = (struct StmtSwitch){
			.condition = cond
		};
		
		// begin a new chain but keep the old one
		StmtIndex old_switch = current_switch_or_case;
		current_switch_or_case = n;
		
		StmtIndex old_breakable = current_breakable;
		current_breakable = n;
		{
			StmtIndex body = parse_stmt_or_expr(tu, s);
			tu->stmts[n].switch_.body = body;
		}
		
		current_breakable = old_breakable;
		current_switch_or_case = old_switch;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_case) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_CASE);
		
		Stmt* last_node = &tu->stmts[current_switch_or_case];
		if (last_node->op == STMT_CASE) {
			last_node->case_.next = n;
		} else if (last_node->op == STMT_DEFAULT) {
			last_node->default_.next = n;
		} else if (last_node->op == STMT_SWITCH) {
			last_node->switch_.next = n;
		} else {
			abort();
		}
		
		intmax_t key = parse_const_expr(tu, s);
		
		current_switch_or_case = n;
		expect(s, ':');
		
		tu->stmts[n].case_ = (struct StmtCase){
			.key = key, .body = 0, .next = 0
		};
		
		StmtIndex body = parse_stmt_or_expr(tu, s);
		tu->stmts[n].case_.body = body;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_default) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_DEFAULT);
		
		Stmt* last_node = &tu->stmts[current_switch_or_case];
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
		
		tu->stmts[n].default_ = (struct StmtDefault){
			.body = 0, .next = 0
		};
		
		StmtIndex body = parse_stmt_or_expr(tu, s);
		tu->stmts[n].default_.body = body;
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_break) {
		// TODO(NeGate): error messages
		assert(current_breakable);
		
		tokens_next(s);
		expect(s, ';');
		
		StmtIndex n = make_stmt(tu, s, STMT_BREAK);
		tu->stmts[n].break_ = (struct StmtBreak){
			.target = current_breakable
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_continue) {
		// TODO(NeGate): error messages
		assert(current_continuable);
		
		tokens_next(s);
		expect(s, ';');
		
		StmtIndex n = make_stmt(tu, s, STMT_CONTINUE);
		tu->stmts[n].continue_ = (struct StmtContinue){
			.target = current_continuable
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_while) {
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_WHILE);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(tu, s);
		expect(s, ')');
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			StmtIndex old_continuable = current_continuable;
			current_continuable = n;
			
			body = parse_stmt_or_expr(tu, s);
			
			current_breakable = old_breakable;
			current_continuable = old_continuable;
		}
		
		tu->stmts[n].while_ = (struct StmtWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_for) {
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_FOR);
		
		int saved = local_symbol_count;
		expect(s, '(');
		
		// it's either nothing, a declaration, or an expression
		StmtIndex first = 0;
		if (tokens_get(s)->type == ';') {
			/* nothing */
			tokens_next(s);
		} else {
			// NOTE(NeGate): This is just a decl list or a single expression.
			first = make_stmt(tu, s, STMT_COMPOUND);
			
			size_t body_count = 0; // He be fuckin
			void* body = tls_save();
			{
				parse_decl_or_expr(tu, s, &body_count);
			}
			StmtIndex* stmt_array = arena_alloc(body_count * sizeof(StmtIndex), _Alignof(StmtIndex));
			memcpy(stmt_array, body, body_count * sizeof(StmtIndex));
			
			tu->stmts[first].compound = (struct StmtCompound) {
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
			cond = parse_expr(tu, s);
			expect(s, ';');
		}
		
		ExprIndex next = 0;
		if (tokens_get(s)->type == ')') {
			/* nothing */
			tokens_next(s);
		} else {
			next = parse_expr(tu, s);
			expect(s, ')');
		}
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			StmtIndex old_continuable = current_continuable;
			current_continuable = n;
			
			body = parse_stmt_or_expr(tu, s);
			
			current_breakable = old_breakable;
			current_continuable = old_continuable;
		}
		
		// restore local symbol scope
		local_symbol_count = saved;
		
		tu->stmts[n].for_ = (struct StmtFor){
			.first = first,
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_do) {
		tokens_next(s);
		StmtIndex n = make_stmt(tu, s, STMT_DO_WHILE);
		
		// Push this as a breakable statement
		StmtIndex body;
		{
			StmtIndex old_breakable = current_breakable;
			current_breakable = n;
			StmtIndex old_continuable = current_continuable;
			current_continuable = n;
			
			body = parse_stmt_or_expr(tu, s);
			
			current_breakable = old_breakable;
			current_continuable = old_continuable;
		}
		
		if (tokens_get(s)->type != TOKEN_KW_while) {
			Token* t = tokens_get(s);
			SourceLoc* loc = &s->line_arena[t->location];
			
			printf("%s:%d: error: expected 'while' got '%.*s'\n", loc->file, loc->line, (int)(t->end - t->start), t->start);
			abort();
		}
		tokens_next(s);
		
		expect(s, '(');
		
		ExprIndex cond = parse_expr(tu, s);
		
		expect(s, ')');
		expect(s, ';');
		
		tu->stmts[n].do_while = (struct StmtDoWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (tokens_get(s)->type == TOKEN_KW_goto) {
		tokens_next(s);
		
		StmtIndex n = make_stmt(tu, s, STMT_GOTO);
		
		ExprIndex target = parse_expr(tu, s);
		tu->stmts[n].goto_ = (struct StmtGoto){
			.target = target
		};
		
		expect(s, ';');
		return n;
	} else if (tokens_get(s)->type == TOKEN_IDENTIFIER &&
			   tokens_peek(s)->type == TOKEN_COLON) {
		Token* t = tokens_get(s);
		Atom name = atoms_put(t->end - t->start, t->start);
		
		StmtIndex n = make_stmt(tu, s, STMT_LABEL);
		tu->stmts[n].label = (struct StmtLabel){
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

static void parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count) {
	if (tokens_get(s)->type == ';') {
		tokens_next(s);
	} else if (is_typename(s)) {
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(tu, s, &attr);
		
		// TODO(NeGate): Kinda ugly
		// don't expect one the first time
		bool expect_comma = false;
		while (tokens_get(s)->type != ';') {
			if (expect_comma) {
				expect(s, ',');
			} else expect_comma = true;
			
			Decl decl = parse_declarator(tu, s, type);
			
			StmtIndex n = make_stmt(tu, s, STMT_DECL);
			tu->stmts[n].loc = decl.loc;
			tu->stmts[n].decl = (struct StmtDecl){
				.type = decl.type,
				.name = decl.name,
				.attrs = attr,
				.initial = 0
			};
			
			if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
				report(REPORT_ERROR, &s->line_arena[decl.loc], "Local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
				abort();
			}
			
			local_symbols[local_symbol_count++] = (Symbol){
				.name = decl.name,
				.type = decl.type,
				.storage_class = STORAGE_LOCAL,
				.stmt = n
			};
			
			ExprIndex initial = 0;
			if (tokens_get(s)->type == '=') {
				tokens_next(s);
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					initial = parse_initializer(tu, s, TYPE_NONE);
				} else {
					initial = parse_expr_l14(tu, s);
				}
			}
			
			tu->stmts[n].decl.initial = initial;
			*((StmtIndex*)tls_push(sizeof(StmtIndex))) = n;
			*body_count += 1;
		}
		
		expect(s, ';');
	} else {
		StmtIndex n = make_stmt(tu, s, STMT_EXPR);
		
		ExprIndex expr = parse_expr(tu, s);
		tu->stmts[n].expr = (struct StmtExpr){
			.expr = expr
		};
		
		*((StmtIndex*)tls_push(sizeof(StmtIndex))) = n;
		*body_count += 1;
		expect(s, ';');
	}
}

static StmtIndex parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s) {
	if (tokens_get(s)->type == ';') {
		tokens_next(s);
		return 0;
	} else {
		StmtIndex stmt = parse_stmt(tu, s);
		
		if (stmt) {
			return stmt;
		} else {
			StmtIndex n = make_stmt(tu, s, STMT_EXPR);
			
			ExprIndex expr = parse_expr(tu, s);
			tu->stmts[n].expr = (struct StmtExpr){
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
static ExprIndex parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s);

// NOTE(NeGate): This function will push all nodes it makes onto the temporary
// storage where parse_initializer will move them into permanent storage.
static void parse_initializer_member(TranslationUnit* tu, TokenStream* restrict s) {
	// Parse designator, it's just chains of:
	// [const-expr]
	// .identifier
	InitNode* current = NULL;
	try_again: {
		if (tokens_get(s)->type == '[') {
			tokens_next(s);
			
			int start = parse_const_expr(tu, s);
			if (start < 0) {
				// TODO(NeGate): Error messages
				generic_error(s, "Array initializer range is broken.");
			}
			
			// GNU-extension: array range initializer
			int count = 1;
			if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
				tokens_next(s);
				
				count = parse_const_expr(tu, s) - start;
				if (count <= 1) {
					// TODO(NeGate): Error messages
					generic_error(s, "Array initializer range is broken.");
				}
			}
			expect(s, ']');
			
			current = (InitNode*) tls_push(sizeof(InitNode));
			*current = (InitNode) {
				.mode = INIT_ARRAY,
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
				.mode = INIT_MEMBER,
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
		while (tokens_get(s)->type != '}') {
			if (expect_comma) {
				expect(s, ',');
			} else expect_comma = true;
			
			parse_initializer_member(tu, s);
			local_count += 1;
		}
		
		current->kids_count = local_count;
		expect(s, '}');
	} else {
		// parse without comma operator
		current->kids_count = 0;
		current->expr = parse_expr_l14(tu, s);
	}
}

static ExprIndex parse_initializer(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type) {
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
		
		parse_initializer_member(tu, s);
		count += 1;
	}
	
	size_t total_node_count = ((InitNode*)tls_save()) - start;
	expect(s, '}');
	
	InitNode* permanent_store = arena_alloc(total_node_count * sizeof(InitNode), _Alignof(InitNode));
	memcpy(permanent_store, start, total_node_count * sizeof(InitNode));
	tls_restore(start);
	
	ExprIndex e = make_expr(tu);
	tu->exprs[e] = (Expr) {
		.op = EXPR_INITIALIZER,
		.loc = tokens_get(s)->location,
		.init = { type, count, permanent_store }
	};
	return e;
}

static ExprIndex parse_expr_l0(TranslationUnit* tu, TokenStream* restrict s) {
	Token* t = tokens_get(s);
	SourceLocIndex loc = t->location;
	
	if (t->type == '(') {
		tokens_next(s);
		ExprIndex e = parse_expr(tu, s);
		expect(s, ')');
		
		return e;
	} else if (t->type == TOKEN_IDENTIFIER) {
		Symbol* sym = find_local_symbol(s);
		
		ExprIndex e = make_expr(tu);
		if (sym) {
			if (sym->storage_class == STORAGE_PARAM) {
				tu->exprs[e] = (Expr) {
					.op = EXPR_PARAM,
					.loc = loc,
					.param_num = sym->param_num
				};
			} else {
				tu->exprs[e] = (Expr) {
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
				tu->exprs[e] = (Expr) {
					.op = EXPR_SYMBOL,
					.loc = loc,
					.symbol = labels[search].value
				};
			} else {
				tu->exprs[e] = (Expr) {
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
		TknType operation_type = tokens_get(s)->type;
		tokens_next(s);
		
		bool has_paren = false;
		if (tokens_get(s)->type == '(') {
			has_paren = true;
			
			tokens_next(s);
		}
		
		ExprIndex e = make_expr(tu);
		if (is_typename(s)) {
			TypeIndex type = parse_typename(tu, s);
			
			tu->exprs[e] = (Expr) {
				.op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF_T : EXPR_ALIGNOF_T,
				.loc = tokens_get(s)->location,
				.x_of_type = { type }
			};
		} else {
			ExprIndex expr = parse_expr_l14(tu, s);
			
			tu->exprs[e] = (Expr) {
				.op = operation_type == TOKEN_KW_sizeof ? EXPR_SIZEOF : EXPR_ALIGNOF,
				.loc = tokens_get(s)->location,
				.x_of_expr = { expr }
			};
		}
		
		if (has_paren) expect(s, ')');
		return e;
	} else if (tokens_get(s)->type == TOKEN_FLOAT) {
		Token* t = tokens_get(s);
		bool is_float32 = t->end[-1] == 'f';
		double i = parse_float(t->end - t->start, (const char*)t->start);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = is_float32 ? EXPR_FLOAT32 : EXPR_FLOAT64,
			.loc = loc,
			.float_num = i
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_INTEGER) {
		Token* t = tokens_get(s);
		IntSuffix suffix;
		uint64_t i = parse_int(t->end - t->start, (const char*)t->start, &suffix);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_INT,
			.loc = loc,
			.int_num = { i, suffix }
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_STRING_SINGLE_QUOTE) {
		Token* t = tokens_get(s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_CHAR,
			.loc = loc,
			.str.start = t->start,
			.str.end = t->end
		};
		
		tokens_next(s);
		return e;
	} else if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
		Token* t = tokens_get(s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_STR,
			.loc = loc,
			.str.start = t->start,
			.str.end = t->end
		};
		
		size_t saved_lexer_pos = s->current;
		tokens_next(s);
		
		if (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
			// Precompute length
			s->current = saved_lexer_pos;
			size_t total_len = (t->end - t->start);
			while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
				Token* segment = tokens_get(s);
				total_len += (segment->end - segment->start) - 2;
				tokens_next(s);
			}
			
			size_t curr = 0;
			char* buffer = arena_alloc(total_len + 2, 4);
			buffer[curr++] = '\"';
			
			// Fill up the buffer
			s->current = saved_lexer_pos;
			while (tokens_get(s)->type == TOKEN_STRING_DOUBLE_QUOTE) {
				Token* segment = tokens_get(s);
				
				// TODO(NeGate): Implement concat on wide strings
				assert(segment->start[0] != 'L');
				
				size_t len = segment->end - segment->start;
				memcpy(&buffer[curr], segment->start + 1, len - 2);
				curr += len - 2;
				
				tokens_next(s);
			}
			
			buffer[curr++] = '\"';
			
			tu->exprs[e].str.start = (const unsigned char*)buffer;
			tu->exprs[e].str.end = (const unsigned char*)(buffer + curr);
		}
		
		return e;
	} else {
		generic_error(s, "Could not parse expression!");
	}
}

static ExprIndex parse_expr_l1(TranslationUnit* tu, TokenStream* restrict s) {
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		
		if (is_typename(s)) {
			TypeIndex type = parse_typename(tu, s);
			expect(s, ')');
			
			if (tokens_get(s)->type == '{') {
				tokens_next(s);
				
				return parse_initializer(tu, s, type);
			} else {
				ExprIndex base = parse_expr_l2(tu, s);
				ExprIndex e = make_expr(tu);
				
				tu->exprs[e] = (Expr) {
					.op = EXPR_CAST,
					.loc = tokens_get(s)->location,
					.cast = { type, base }
				};
				return e;
			}
		}
		
		tokens_prev(s);
	}
	
	SourceLocIndex loc = tokens_get(s)->location;
	ExprIndex e = parse_expr_l0(tu, s);
	
	// after any of the: [] () . ->
	// it'll restart and take a shot at matching another
	// piece of the expression.
	try_again: {
		if (tokens_get(s)->type == '[') {
			ExprIndex base = e;
			e = make_expr(tu);
			
			tokens_next(s);
			ExprIndex index = parse_expr(tu, s);
			expect(s, ']');
			
			tu->exprs[e] = (Expr) {
				.op = EXPR_SUBSCRIPT,
				.loc = loc,
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
			e = make_expr(tu);
			tu->exprs[e] = (Expr) {
				.op = EXPR_ARROW,
				.loc = loc,
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
			e = make_expr(tu);
			tu->exprs[e] = (Expr) {
				.op = EXPR_DOT,
				.loc = loc,
				.dot = { .base = base, .name = name }
			};
			
			tokens_next(s);
			goto try_again;
		}
		
		// Function call
		if (tokens_get(s)->type == '(') {
			tokens_next(s);
			
			ExprIndex target = e;
			e = make_expr(tu);
			
			size_t param_count = 0;
			void* params = tls_save();
			
			while (tokens_get(s)->type != ')') {
				if (param_count) {
					expect(s, ',');
				}
				
				// NOTE(NeGate): This is a funny little work around because
				// i don't wanna parse the comma operator within the expression
				// i wanna parse it here so we just skip it.
				ExprIndex e = parse_expr_l14(tu, s);
				*((ExprIndex*) tls_push(sizeof(ExprIndex))) = e;
				param_count++;
			}
			
			if (tokens_get(s)->type != ')') {
				generic_error(s, "Unclosed parameter list!");
			}
			tokens_next(s);
			
			// Copy parameter refs into more permanent storage
			ExprIndex* param_start = arena_alloc(param_count * sizeof(ExprIndex), _Alignof(ExprIndex));
			memcpy(param_start, params, param_count * sizeof(ExprIndex));
			
			tu->exprs[e] = (Expr) {
				.op = EXPR_CALL,
				.loc = loc,
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
			
			e = make_expr(tu);
			tu->exprs[e] = (Expr) {
				.op = is_inc ? EXPR_POST_INC : EXPR_POST_DEC,
				.loc = loc,
				.unary_op.src = src
			};
		}
		
		return e;
	}
}

// deref* address& negate-
static ExprIndex parse_expr_l2(TranslationUnit* tu, TokenStream* restrict s) {
	// TODO(NeGate): Convert this code into a loop... please?
	SourceLocIndex loc = tokens_get(s)->location;
	
	if (tokens_get(s)->type == '*') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_DEREF,
			.loc = tokens_get(s)->location,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '!') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_LOGICAL_NOT,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == TOKEN_DOUBLE_EXCLAMATION) {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_CAST,
			.loc = loc,
			.cast = { TYPE_BOOL, value }
		};
		return e;
	} else if (tokens_get(s)->type == '-') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_NEGATE,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '~') {
		tokens_next(s);
		ExprIndex value = parse_expr_l2(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_NOT,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '+') {
		tokens_next(s);
		return parse_expr_l2(tu, s);
	} else if (tokens_get(s)->type == TOKEN_INCREMENT) {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_PRE_INC,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == TOKEN_DECREMENT) {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_PRE_DEC,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else if (tokens_get(s)->type == '&') {
		tokens_next(s);
		ExprIndex value = parse_expr_l1(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_ADDR,
			.loc = loc,
			.unary_op.src = value
		};
		return e;
	} else {
		return parse_expr_l1(tu, s);
	}
}

// * / %
static ExprIndex parse_expr_l3(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l2(tu, s);
	
	while (tokens_get(s)->type == TOKEN_TIMES ||
		   tokens_get(s)->type == TOKEN_SLASH ||
		   tokens_get(s)->type == TOKEN_PERCENT) {
		ExprIndex e = make_expr(tu);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_TIMES: op = EXPR_TIMES; break;
			case TOKEN_SLASH: op = EXPR_SLASH; break;
			case TOKEN_PERCENT: op = EXPR_PERCENT; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l2(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = tokens_get(s)->location,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// + -
static ExprIndex parse_expr_l4(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l3(tu, s);
	
	while (tokens_get(s)->type == TOKEN_PLUS ||
		   tokens_get(s)->type == TOKEN_MINUS) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_PLUS: op = EXPR_PLUS; break;
			case TOKEN_MINUS: op = EXPR_MINUS; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l3(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// + -
static ExprIndex parse_expr_l5(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l4(tu, s);
	
	while (tokens_get(s)->type == TOKEN_LEFT_SHIFT ||
		   tokens_get(s)->type == TOKEN_RIGHT_SHIFT) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_LEFT_SHIFT: op = EXPR_SHL; break;
			case TOKEN_RIGHT_SHIFT: op = EXPR_SHR; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l4(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// >= > <= <
static ExprIndex parse_expr_l6(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l5(tu, s);
	
	while (tokens_get(s)->type == TOKEN_GREATER_EQUAL ||
		   tokens_get(s)->type == TOKEN_LESS_EQUAL || 
		   tokens_get(s)->type == TOKEN_GREATER ||
		   tokens_get(s)->type == TOKEN_LESS) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op;
		switch (tokens_get(s)->type) {
			case TOKEN_LESS:          op = EXPR_CMPLT; break;
			case TOKEN_LESS_EQUAL:    op = EXPR_CMPLE; break;
			case TOKEN_GREATER:       op = EXPR_CMPGT; break;
			case TOKEN_GREATER_EQUAL: op = EXPR_CMPGE; break;
			default: __builtin_unreachable();
		}
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l5(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// == !=
static ExprIndex parse_expr_l7(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l6(tu, s);
	
	while (tokens_get(s)->type == TOKEN_NOT_EQUAL ||
		   tokens_get(s)->type == TOKEN_EQUALITY) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = tokens_get(s)->type == TOKEN_EQUALITY ? EXPR_CMPEQ : EXPR_CMPNE;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l6(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// &
static ExprIndex parse_expr_l8(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l7(tu, s);
	
	while (tokens_get(s)->type == '&') {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_AND;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l7(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ^
static ExprIndex parse_expr_l9(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l8(tu, s);
	
	while (tokens_get(s)->type == '^') {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_XOR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l8(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// |
static ExprIndex parse_expr_l10(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l9(tu, s);
	
	while (tokens_get(s)->type == '|') {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_OR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l9(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// &&
static ExprIndex parse_expr_l11(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l10(tu, s);
	
	while (tokens_get(s)->type == TOKEN_DOUBLE_AND) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_LOGICAL_AND;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l10(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ||
static ExprIndex parse_expr_l12(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l11(tu, s);
	
	while (tokens_get(s)->type == TOKEN_DOUBLE_OR) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_LOGICAL_OR;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l11(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// ternary
static ExprIndex parse_expr_l13(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l12(tu, s);
	
	while (tokens_get(s)->type == '?') {
		SourceLocIndex loc = tokens_get(s)->location;
		tokens_next(s);
		
		ExprIndex mhs = parse_expr(tu, s);
		
		expect(s, ':');
		
		ExprIndex rhs = parse_expr_l12(tu, s);
		
		ExprIndex e = make_expr(tu);
		tu->exprs[e] = (Expr) {
			.op = EXPR_TERNARY,
			.loc = loc,
			.ternary_op = { lhs, mhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

// = += -= *= /= %= <<= >>= &= ^= |=
static ExprIndex parse_expr_l14(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l13(tu, s);
	
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
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		
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
		
		ExprIndex rhs = parse_expr_l13(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

static ExprIndex parse_expr_l15(TranslationUnit* tu, TokenStream* restrict s) {
	ExprIndex lhs = parse_expr_l14(tu, s);
	
	while (tokens_get(s)->type == TOKEN_COMMA) {
		SourceLocIndex loc = tokens_get(s)->location;
		ExprIndex e = make_expr(tu);
		ExprOp op = EXPR_COMMA;
		tokens_next(s);
		
		ExprIndex rhs = parse_expr_l14(tu, s);
		tu->exprs[e] = (Expr) {
			.op = op,
			.loc = loc,
			.bin_op = { lhs, rhs }
		};
		
		lhs = e;
	}
	
	return lhs;
}

static ExprIndex parse_expr(TranslationUnit* tu, TokenStream* restrict s) {
	return parse_expr_l15(tu, s);
}

////////////////////////////////
// TYPES
////////////////////////////////
static TypeIndex parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, Atom name);

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type) {
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
		type = new_pointer(tu, type);
		tokens_next(s);
		
		parse_another_qualifier: {
			switch (tokens_get(s)->type) {
				case TOKEN_KW_restrict: {
					tu->types[type].is_ptr_restrict = true;
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
	
	skip_over_declspec(s);
	
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
		
		parse_declarator(tu, s, TYPE_NONE);
		
		expect(s, ')');
		type = parse_type_suffix(tu, s, type, NULL);
		
		size_t saved_end = s->current;
		s->current = saved;
		
		Decl d = parse_declarator(tu, s, type);
		
		// inherit name
		// TODO(NeGate): I'm not sure if this is correct ngl
		if (!d.name) {
			TypeIndex t = d.type;
			
			if (tu->types[t].kind == KIND_PTR) {
				t = tu->types[d.type].ptr_to;
				
				if (tu->types[t].kind == KIND_FUNC) {
					d.name = tu->types[t].func.name;
				} else if (tu->types[t].kind == KIND_STRUCT) {
					d.name = tu->types[t].record.name;
				} else if (tu->types[t].kind == KIND_UNION) {
					d.name = tu->types[t].record.name;
				}
			}
		}
		
		s->current = saved_end;
		return d;
	}
	
	Atom name = NULL;
	Token* t = tokens_get(s);
	SourceLocIndex loc = 0;
	if (t->type == TOKEN_IDENTIFIER) {
		loc = t->location;
		name = atoms_put(t->end - t->start, t->start);
		tokens_next(s);
	}
	
	type = parse_type_suffix(tu, s, type, name);
	return (Decl){ type, name, loc };
}

// it's like a declarator with a skin fade,
// int*        int[16]       const char* restrict
static TypeIndex parse_abstract_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type) {
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
		type = new_pointer(tu, type);
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
		
		parse_abstract_declarator(tu, s, TYPE_NONE);
		
		expect(s, ')');
		type = parse_type_suffix(tu, s, type, NULL);
		
		size_t saved_end = s->current;
		s->current = saved;
		
		TypeIndex d = parse_abstract_declarator(tu, s, type);
		s->current = saved_end;
		return d;
	}
	
	Atom name = NULL;
	Token* t = tokens_get(s);
	if (t->type == TOKEN_IDENTIFIER) {
		name = atoms_put(t->end - t->start, t->start);
		tokens_next(s);
	}
	
	type = parse_type_suffix(tu, s, type, name);
	return type;
}

static TypeIndex parse_typename(TranslationUnit* tu, TokenStream* restrict s) {
	// TODO(NeGate): Check if attributes are set, they shouldn't
	// be in this context.
	Attribs attr = { 0 };
	TypeIndex type = parse_declspec(tu, s, &attr);
	return parse_abstract_declarator(tu, s, type);
}

static TypeIndex parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, Atom name) {
	assert(s->current > 0);
	SourceLoc* loc = &s->line_arena[s->tokens[s->current - 1].location];
	
	// type suffixes like array [] and function ()
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		
		TypeIndex return_type = type;
		
		type = new_func(tu);
		tu->types[type].func.name = name;
		tu->types[type].func.return_type = return_type;
		
		if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
			tokens_next(s);
			tokens_next(s);
			
			tu->types[type].func.param_list = 0;
			tu->types[type].func.param_count = 0;
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
			TypeIndex arg_base_type = parse_declspec(tu, s, &arg_attr);
			
			Decl param_decl = parse_declarator(tu, s, arg_base_type);
			TypeIndex param_type = param_decl.type;
			
			if (tu->types[param_type].kind == KIND_ARRAY) {
				// Array parameters are desugared into pointers
				param_type = new_pointer(tu, tu->types[param_type].array_of);
			} else if (tu->types[param_type].kind == KIND_FUNC) {
				// Function parameters are desugared into pointers
				param_type = new_pointer(tu, param_type);
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
		
		// Allocate some more permanent storage
		ParamIndex start = big_array_length(tu->params);
		big_array_put_uninit(tu->params, param_count);
		memcpy(&tu->params[start], params, param_count * sizeof(Param));
		
		tu->types[type].func.param_list = start;
		tu->types[type].func.param_count = param_count;
		tu->types[type].func.has_varargs = has_varargs;
		
		tls_restore(params);
	} else if (tokens_get(s)->type == '[') {
		size_t depth = 0;
		size_t* counts = tls_save();
		
		// TODO(NeGate): read some array qualifiers... then throw them away
		do {
			tokens_next(s);
			
			long long count;
			if (tokens_get(s)->type == ']') {
				count = 0; 
				tokens_next(s);
			} else {
				count = parse_const_expr(tu, s);
				expect(s, ']');
			}
			
			tls_push(sizeof(size_t));
			counts[depth++] = count;
		} while (tokens_get(s)->type == '[');
		
		size_t expected_size = tu->types[type].size;
		while (depth--) {
			assert(tu->types[type].size == expected_size);
			
			uint64_t a = expected_size;
			uint64_t b = counts[depth];
			uint64_t result = a * b;
			
			// size checks
			if (result >= INT32_MAX) {
				report(REPORT_ERROR, loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX)", result);
				abort();
			}
			
			type = new_array(tu, type, counts[depth]);
			expected_size = result;
		}
		
		tls_restore(counts);
	}
	
	return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static TypeIndex parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr) {
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
	
	// _Alignas(N) or __declspec(align(N))
	// 0 means no forced alignment
	int forced_align = 0;
	
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
			
			case TOKEN_KW_register: /* lmao */ break;
			case TOKEN_KW_static: attr->is_static = true; break;
			case TOKEN_KW_typedef: attr->is_typedef = true; break;
			case TOKEN_KW_inline: attr->is_inline = true; break;
			case TOKEN_KW_extern: attr->is_extern = true; break;
			case TOKEN_KW_Thread_local: attr->is_tls = true; break;
			
			case TOKEN_KW_cdecl: break;
			case TOKEN_KW_stdcall: break;
			
			case TOKEN_KW_Complex:
			case TOKEN_KW_Imaginary: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				report(REPORT_ERROR, loc, "Complex types are not supported in CuikC");
				break;
			}
			
			case TOKEN_KW_Atomic: is_atomic = true; break;
			case TOKEN_KW_const: is_const = true; break;
			case TOKEN_KW_volatile: break;
			case TOKEN_KW_auto: break;
			
			case TOKEN_KW_Typeof: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				
				tokens_next(s);
				if (tokens_get(s)->type != '(') {
					report(REPORT_ERROR, loc, "expected opening parenthesis for _Typeof");
					return 0;
				}
				tokens_next(s);
				
				if (is_typename(s)) {
					type = parse_typename(tu, s);
				} else {
					// we don't particularly resolve typeof for expressions immediately.
					// instead we just wait until all symbols are resolved properly
					ExprIndex src = parse_expr(tu, s);
					type = new_typeof(tu, src);
				}
				
				if (tokens_get(s)->type != ')') {
					report(REPORT_ERROR, loc, "expected closing parenthesis for _Typeof");
					return 0;
				}
				break;
			}
			
			case TOKEN_KW_Alignas: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				
				tokens_next(s);
				expect(s, '(');
				
				intmax_t new_align = parse_const_expr(tu, s);
				if (new_align >= INT16_MAX) {
					report(REPORT_ERROR, loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
				} else {
					forced_align = new_align;
				}
				
				if (tokens_get(s)->type != ')') {
					report(REPORT_ERROR, loc, "expected closing parenthesis for _Alignas");
					return 0;
				}
				break;
			}
			
			case TOKEN_KW_declspec: {
				// TODO(NeGate): Correctly parse declspec instead of
				// ignoring them.
				tokens_next(s);
				tokens_next(s);
				
				int depth = 1;
				while (depth) {
					if (tokens_get(s)->type == '(') depth++;
					else if (tokens_get(s)->type == ')') depth--;
					
					tokens_next(s);
				}
				
				tokens_prev(s);
				break;
			}
			
			case TOKEN_KW_struct:
			case TOKEN_KW_union: {
				if (counter) goto done;
				tokens_next(s);
				
				bool is_union = tkn_type == TOKEN_KW_union;
				
				while (skip_over_declspec(s)) {
					// TODO(NeGate): printf("Don't forget about declspec\n");
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
						//assert(!tu->types[type].is_incomplete);
					} else {
						type = new_record(tu, is_union);
						tu->types[type].is_incomplete = false;
						tu->types[type].record.name = name;
						
						if (name && strcmp((const char*)name, "__m128") == 0) {
							tu->types[type].record.intrin_type = tb_vector_type(TB_F32, 4);
						} else {
							tu->types[type].record.intrin_type = TB_TYPE_VOID;
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
						TypeIndex member_base_type = parse_declspec(tu, s, &member_attr);
						
						// TODO(NeGate): Kinda ugly
						// don't expect one the first time
						int last_member_size = 0;
						int current_bit_offset = 0;
						
						bool expect_comma = false;
						while (tokens_get(s)->type != ';') {
							if (expect_comma) {
								expect(s, ',');
							} else expect_comma = true;
							
							Decl decl = parse_declarator(tu, s, member_base_type);
							TypeIndex member_type = decl.type;
							
							if (tu->types[member_type].kind == KIND_FUNC) {
								generic_error(s, "Naw dawg");
							}
							
							int member_align = tu->types[member_type].align;
							int member_size = tu->types[member_type].size;
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
								
								// if we change data type halfway we split things off
								if (last_member_size != member_size) {
									current_bit_offset = 0;
								}
								
								int bit_width = parse_const_expr(tu, s);
								int bits_in_region = member_size * 8;
								if (bit_width > bits_in_region) {
									generic_error(s, "Bitfield cannot fit in this type.");
								}
								
								if (current_bit_offset + bit_width > bits_in_region) {
									current_bit_offset = 0;
								}
								
								member->is_bitfield = true;
								member->bit_offset = current_bit_offset;
								member->bit_width = bit_width;
							}
							last_member_size = member_size;
							
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
					tu->types[type].size = offset;
					tu->types[type].align = align;
					
					MemberIndex start = big_array_length(tu->members);
					
					tu->types[type].record.kids_start = start;
					tu->types[type].record.kids_end = start + member_count;
					
					big_array_put_uninit(tu->members, member_count);
					memcpy(&tu->members[start], members, member_count * sizeof(Member));
					
					tls_restore(members);
				} else {
					// TODO(NeGate): must be a forward decl, handle it
					if (name == NULL) generic_error(s, "Cannot have unnamed forward struct reference.");
					
					type = find_incomplete_type((char*)name);
					if (!type) {
						type = new_record(tu, is_union);
						tu->types[type].record.name = name;
						tu->types[type].is_incomplete = true;
						
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
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					type = name ? find_incomplete_type((char*)name) : 0;
					if (type) {
						// can't re-complete a enum
						// TODO(NeGate): error messages
						assert(tu->types[type].enumerator.start != tu->types[type].enumerator.start);
					} else {
						type = new_enum(tu);
						tu->types[type].enumerator.name = name;
						tu->types[type].enumerator.start = big_array_length(tu->enum_entries);
						
						if (name) shput(incomplete_types, name, type);
					}
					
					// starts at zero and after any entry it increments
					// you can override it by using:
					// identifier = int-const-expr
					int cursor = 0;
					
					while (tokens_get(s)->type != '}') {
						// parse name
						Token* t = tokens_get(s);
						if (t->type != TOKEN_IDENTIFIER) {
							generic_error(s, "expected identifier for enum name entry.");
						}
						
						Atom name = atoms_put(t->end - t->start, t->start);
						tokens_next(s);
						
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							cursor = parse_const_expr(tu, s);
						}
						
						EnumEntry entry = {
							.name = name,
							.value = cursor++
						};
						big_array_put(tu->enum_entries, entry);
						
						if (tokens_get(s)->type == ',') tokens_next(s);
					}
					
					if (tokens_get(s)->type != '}') {
						generic_error(s, "Unclosed enum list!");
					}
					
					tu->types[type].enumerator.end = big_array_length(tu->enum_entries);
				} else {
					type = find_incomplete_type((char*)name);
					if (!type) {
						type = new_enum(tu);
						tu->types[type].record.name = name;
						tu->types[type].is_incomplete = true;
						
						shput(incomplete_types, name, type);
					}
					counter += OTHER;
					
					// push back one because we push it forward one later but 
					// shouldn't
					tokens_prev(s);
				}
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
			case LONG:
			case LONG + INT:
			case SIGNED:
			type = TYPE_INT;
			break;
			case SIGNED + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
			type = settings.is_windows_long ? TYPE_INT : TYPE_LONG;
			break;
			case UNSIGNED:
			case UNSIGNED + INT:
			type = TYPE_UINT;
			break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
			type = settings.is_windows_long ? TYPE_UINT : TYPE_ULONG;
			break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
			type = TYPE_LONG;
			break;
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
	SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
	if (type == 0) {
		report(REPORT_ERROR, loc, "unknown typename");
		return 0;
	}
	
	if (is_atomic || is_const || (forced_align && tu->types[type].align != forced_align)) {
		if (forced_align && forced_align < tu->types[type].align) {
			report(REPORT_ERROR, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, tu->types[type].align);
			return 0;
		}
		
		type = copy_type(tu, type);
		
		if (forced_align) {
			tu->types[type].align = forced_align;
		}
		
		tu->types[type].is_atomic = is_atomic;
		tu->types[type].is_const = is_const;
	}
	
	return type;
}

static bool is_typename(TokenStream* restrict s) {
	Token* t = tokens_get(s);
	
	switch (t->type) {
		case TOKEN_KW_void: case TOKEN_KW_char: case TOKEN_KW_short:
		case TOKEN_KW_int: case TOKEN_KW_long: case TOKEN_KW_float:
		case TOKEN_KW_double: case TOKEN_KW_Bool: case TOKEN_KW_signed:
		case TOKEN_KW_unsigned: case TOKEN_KW_struct: case TOKEN_KW_union:
		case TOKEN_KW_enum: case TOKEN_KW_extern: case TOKEN_KW_static:
		case TOKEN_KW_typedef: case TOKEN_KW_inline: case TOKEN_KW_const:
		case TOKEN_KW_volatile: case TOKEN_KW_declspec: case TOKEN_KW_Thread_local:
		case TOKEN_KW_Alignas: case TOKEN_KW_Atomic: case TOKEN_KW_auto:
		case TOKEN_KW_cdecl: case TOKEN_KW_stdcall: case TOKEN_KW_Typeof:
		return true;
		
		case TOKEN_IDENTIFIER: {
			// good question...
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			ptrdiff_t search = shgeti(typedefs, name);
			return (search >= 0);
		}
		
		default:
		return false;
	}
}

static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s) {
	ConstValue v = const_eval(tu, parse_expr_l14(tu, s));
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
	
	report(REPORT_ERROR, loc, msg);
	abort();
}

static void generic_warn(TokenStream* restrict s, const char* msg) {
	Token* t = tokens_get(s);
	SourceLoc* loc = &s->line_arena[t->location];
	
	printf("%s:%d: warning: %s\n", loc->file, loc->line, msg);
}

static void expect(TokenStream* restrict s, char ch) {
	if (tokens_get(s)->type != ch) {
		Token* t = tokens_get(s);
		SourceLoc* loc = &s->line_arena[t->location];
		
		report(REPORT_ERROR, loc, "expected '%c' got '%.*s'", ch, (int)(t->end - t->start), t->start);
		abort();
	}
	
	tokens_next(s);
}
