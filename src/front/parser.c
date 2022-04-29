// NOTE(NeGate): Something to note is that 
//  a = b
// has an unspecified eval order so if you do
//  a[i] = parse(...)
// and it can somehow resize a then make sure to split it up
//  e = parse(...)
//  a[i] = e
//
// TODO list
//   - make sure all extensions are errors on pedantic mode
//
//   - enable some sort of error recovery when we encounter bad syntax within a
//   statement, basically just find the semicolon and head out :p
//   
//   - ugly ass code
//
//   - make the expect(...) code be a little smarter, if it knows that it's expecting
//   a token for a specific operation... tell the user
#include "parser.h"
#include <timer.h>
#include <targets/targets.h>
#undef VOID // winnt.h loves including garbage

#define OUT_OF_ORDER_CRAP 1

typedef struct {
	Atom key;
	TypeIndex value;
} IncompleteType;

typedef struct {
	Atom key;
	Symbol value;
} SymbolEntry;

typedef struct {
	Atom key;
	Stmt* value;
} LabelEntry;

typedef struct {
	int  current; // position in the TokenStream
	char terminator;
} PendingExpr;

// starting point is used to disable the function literals
// from reading from their parent function
thread_local static int local_symbol_start = 0;
thread_local static int local_symbol_count = 0;
thread_local static Symbol local_symbols[MAX_LOCAL_SYMBOLS];

// Global symbol stuff
thread_local static IncompleteType* incomplete_tags; // stb_ds hash map
thread_local static SymbolEntry* global_symbols; // stb_ds hash map

thread_local static LabelEntry* labels; // stb_ds hash map

thread_local static Stmt* current_switch_or_case;
thread_local static Stmt* current_breakable;
thread_local static Stmt* current_continuable;

thread_local static bool out_of_order_mode;

static void expect(TokenStream* restrict s, char ch);
static void expect_closing_paren(TokenStream* restrict s, SourceLoc* opening);
static void expect_with_reason(TokenStream* restrict s, char ch, const char* reason);
static Symbol* find_local_symbol(TokenStream* restrict s);

static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s);
static Stmt* parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s);
static void parse_decl_or_expr(TranslationUnit* tu, TokenStream* restrict s, size_t* body_count);

static bool skip_over_declspec(TokenStream* restrict s);
static bool try_parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);
static TypeIndex parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr);

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, bool is_abstract, bool disabled_paren);
static TypeIndex parse_typename(TranslationUnit* tu, TokenStream* restrict s);

// It's like parse_expr but it doesn't do anything with comma operators to avoid
// parsing issues.
static intmax_t parse_const_expr(TranslationUnit* tu, TokenStream* restrict s);
static ExprIndex parse_initializer(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type);
static ExprIndex parse_function_literal(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type);
static void parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* n);
static TypeIndex parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, Atom name);
static void create_symbol_use_list(TranslationUnit* tu, Stmt* n, ExprIndex starting_point, bool has_labels);

static bool is_typename(TokenStream* restrict s);

static _Noreturn void generic_error(TokenStream* restrict s, const char* msg);

static int align_up(int a, int b) { 
	if (b == 0) return 0;
	
	return a + (b - (a % b)) % b;
}

static Stmt* make_stmt(TranslationUnit* tu, TokenStream* restrict s, StmtOp op) {
	Stmt* stmt = ARENA_ALLOC(&tu->ast_arena, Stmt);
	*stmt = (Stmt){
		.op = op,
		.loc = tokens_get(s)->location
	};
	return stmt;
}

static ExprIndex make_expr(TranslationUnit* tu) {
	big_array_put_uninit(tu->exprs, 1);
	return big_array_length(tu->exprs) - 1;
}

static Symbol* find_global_symbol(const char* name) {
	ptrdiff_t search = shgeti(global_symbols, name);
	return (search >= 0) ? &global_symbols[search].value : NULL;
}

static TypeIndex find_incomplete_tag(const char* name) {
	ptrdiff_t search = shgeti(incomplete_tags, name);
	return (search >= 0) ? incomplete_tags[search].value : 0;
}

// ( SOMETHING )
// ( SOMETHING ,
static size_t skip_expression_in_parens(TokenStream* restrict s, TknType* out_terminator) {
	size_t saved = s->current;
	
	// by default we expect to exit with closing parens
	*out_terminator = ')';
	
	int depth = 1;
	while (depth) {
		Token* t = tokens_get(s);
		
		if (t->type == '\0') {
			*out_terminator = '\0';
			break;
		} else if (t->type == '(') {
			depth++;
		} else if (t->type == ')') {
			depth--;
		} else if (t->type == ',' && depth == 1) {
			*out_terminator = ',';
			depth--;
		}
		
		tokens_next(s);
	}
	
	return saved;
}

#include "expr_parser.h"
#include "decl_parser.h"

typedef struct {
	// shared state, every run of phase2_parse_task will decrement this by one
	atomic_size_t* tasks_remaining;
	size_t start, end;
	
	TranslationUnit* tu;
	IncompleteType* incomplete_tags; // stb_ds hash map
	SymbolEntry* global_symbols; // stb_ds hash map
	
	const TokenStream* base_token_stream;
} ParserTaskInfo;

static void parse_global_symbols(TranslationUnit* tu, size_t start, size_t end, TokenStream tokens) {
	for (size_t i = start; i < end; i++) {
		Symbol* sym = &global_symbols[i].value;
		//printf("SYMBOL: %s %d\n", sym->name, sym->current);
		
		if (sym->current != 0) {
			// Spin up a mini parser here
			tokens.current = sym->current;
			
			if (sym->storage_class == STORAGE_STATIC_VAR ||
				sym->storage_class == STORAGE_GLOBAL) {
				ExprIndex starting_point = big_array_length(tu->exprs);
				
				ExprIndex e;
				if (tokens_get(&tokens)->type == '@') {
					// function literals are a Cuik extension
					// TODO(NeGate): error messages
					tokens_next(&tokens);
					e = parse_function_literal(tu, &tokens, sym->type);
				} else if (tokens_get(&tokens)->type == '{') {
					tokens_next(&tokens);
					
					e = parse_initializer(tu, &tokens, TYPE_NONE);
				} else {
					e = parse_expr_l14(tu, &tokens);
				}
				expect(&tokens, sym->terminator);
				
				sym->stmt->decl.initial = e;
				create_symbol_use_list(tu, sym->stmt, starting_point, false);
			} else if (sym->storage_class == STORAGE_STATIC_FUNC ||
					   sym->storage_class == STORAGE_FUNC) {
				// Some sanity checks in case a local symbol is leaked funny.
				// they should be isolated
				assert(local_symbol_start == 0);
				assert(local_symbol_count == 0);
				
				parse_function_definition(tu, &tokens, sym->stmt);
				
				local_symbol_start = local_symbol_count = 0;
			}
		}
	}
}

static void phase2_parse_task(void* arg) {
	ParserTaskInfo task = *((ParserTaskInfo*)arg);
	
	// intitialize any thread local state that might not be set on this thread
	current_switch_or_case = current_breakable = current_continuable = NULL;
	incomplete_tags = task.incomplete_tags;
	global_symbols  = task.global_symbols;
	
	tls_init();
	atoms_init();
	
	parse_global_symbols(task.tu, task.start, task.end, *task.base_token_stream);
	*task.tasks_remaining -= 1;
}

void translation_unit_parse(TranslationUnit* restrict tu, const char* filepath, threadpool_t* thread_pool) {
	tu->filepath = filepath;
	tu->types = big_array_create(Type, true);
	tu->params = big_array_create(Param, true);
	tu->enum_entries = big_array_create(EnumEntry, true);
	tu->exprs = big_array_create(Expr, true);
	
	init_types(tu);
	tls_init();
	atoms_init();
	
	////////////////////////////////
	// Parse translation unit
	////////////////////////////////
	TokenStream* restrict s = &tu->tokens;
	
#if OUT_OF_ORDER_CRAP
	//BigArray(PendingExpr) pending_exprs = big_array_create(PendingExpr, false);
	out_of_order_mode = true;
	
	// Phase 1: resolve all top level statements
	timed_block("phase 1") {
		while (tokens_get(s)->type) {
			while (tokens_get(s)->type == ';') tokens_next(s);
			
			// TODO(NeGate): Correctly parse pragmas instead of ignoring them.
			if (tokens_get(s)->type == TOKEN_KW_Pragma) {
				tokens_next(s);
				expect(s, '(');
				
				if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
					generic_error(s, "pragma declaration expects string literal");
				}
				tokens_next(s);
				
				expect(s, ')');
			} else if (tokens_get(s)->type == TOKEN_KW_Static_assert) {
				tokens_next(s);
				expect(s, '(');
				
				intmax_t condition = parse_const_expr(tu, s);
				if (tokens_get(s)->type == ',') {
					tokens_next(s);
					
					Token* t = tokens_get(s);
					if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
						generic_error(s, "static assertion expects string literal");
					}
					tokens_next(s);
					
					if (condition == 0) {
						report(REPORT_ERROR, &s->line_arena[tokens_get(s)->location], "Static assertion failed: '%.*s'", (int)(t->end - t->start), t->start);
					}
				} else {
					if (condition == 0) {
						report(REPORT_ERROR, &s->line_arena[tokens_get(s)->location], "Static assertion failed");
					}
				}
				
				expect(s, ')');
			} else {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				
				// must be a declaration since it's a top level statement
				Attribs attr = { 0 };
				TypeIndex type = parse_declspec(tu, s, &attr);
				attr.is_root = !(attr.is_static || attr.is_inline);
				
				if (attr.is_typedef) {
					// declarator (',' declarator)+ ';'
					while (true) {
						Decl decl = parse_declarator(tu, s, type, false, false);
						assert(decl.name);
						
						// make typedef
						Stmt* n = make_stmt(tu, s, STMT_DECL);
						n->loc = decl.loc;
						n->decl = (struct StmtDecl){
							.name = decl.name,
							.type = decl.type,
							.attrs = attr
						};
						
						arrput(tu->top_level_stmts, n);
						
						// check for collision
						Symbol* search = find_global_symbol((const char*) decl.name);
						if (search != NULL) {
							if (search->storage_class != STORAGE_TYPEDEF) {
								report(REPORT_ERROR, loc, "typedef '%s' overrides previous declaration.", decl.name);
								abort();
							}
							
							Type* placeholder_space = &tu->types[search->type];
							if (placeholder_space->kind != KIND_PLACEHOLDER) {
								report(REPORT_ERROR, loc, "typedef '%s' overrides previous declaration.", decl.name);
								abort();
							}
							
							// replace placeholder with actual entry
							memcpy(placeholder_space, &tu->types[decl.type], sizeof(Type));
						} else {
							// add new entry
							Symbol sym = (Symbol){
								.name = decl.name,
								.type = decl.type,
								.storage_class = STORAGE_TYPEDEF
							};
							shput(global_symbols, decl.name, sym);
						}
						
						if (tokens_get(s)->type == 0) {
							report(REPORT_ERROR, loc, "declaration list ended with EOF instead of semicolon.");
							abort();
						} else if (tokens_get(s)->type == '=') {
							report(REPORT_ERROR, loc, "why did you just try that woofy shit wit me. You cannot assign a typedef.");
							
							// error recovery
						} else if (tokens_get(s)->type == ';') {
							tokens_next(s);
							break;
						} else if (tokens_get(s)->type == ',') {
							tokens_next(s);
							continue;
						}
					}
				} else {
					if (tokens_get(s)->type == ';') {
						tokens_next(s);
						continue;
					}
					
					// normal variable lists
					// declarator (',' declarator )+ ';'
					while (true) {
						Decl decl = parse_declarator(tu, s, type, false, false);
						assert(decl.name);
						
						Stmt* n = make_stmt(tu, s, STMT_GLOBAL_DECL);
						n->loc = decl.loc;
						n->decl = (struct StmtDecl){
							.name = decl.name,
							.type = decl.type,
							.attrs = attr
						};
						arrput(tu->top_level_stmts, n);
						
						// extern variables are forward decls
						if (n->decl.attrs.is_extern) {
							n->decl.attrs.is_root = false;
						}
						
						Symbol sym = (Symbol){
							.name = decl.name,
							.type = decl.type,
							.stmt = n
						};
						
						Symbol* old_definition = find_global_symbol((const char*) decl.name);
						if (tu->types[decl.type].kind == KIND_FUNC) {
							sym.storage_class = (attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC);
						} else {
							sym.storage_class = (attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL);
						}
						
						bool requires_terminator = true;
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							// '=' EXPRESSION ','
							// '=' EXPRESSION ';'
							sym.current = s->current;
							sym.terminator = ';';
							
							int depth = 1;
							while (depth) {
								Token* t = tokens_get(s);
								
								if (t->type == '\0') {
									report(REPORT_ERROR, &s->line_arena[decl.loc], "Declaration ended in EOF");
									abort();
								} else if (t->type == '(') {
									depth++;
								} else if (t->type == ')') {
									depth--;
									
									if (depth == 0) {
										report(REPORT_ERROR, &s->line_arena[decl.loc], "Unbalanced parenthesis");
										abort();
									}
								} else if (t->type == ';' || t->type == ',') {
									if (depth > 1 && t->type == ';') {
										report(REPORT_ERROR, &s->line_arena[decl.loc], "Declaration's expression has a weird semicolon");
										abort();
									} else if (depth == 1) {
										sym.terminator = t->type;
										depth--;
									}
								}
								
								tokens_next(s);
							}
							
							// we ate the terminator but the code right below it
							// does need to know what it is...
							tokens_prev(s);
						} else if (tokens_get(s)->type == '{') {
							// function bodies dont end in semicolon or comma, it just terminates
							// the declaration list
							requires_terminator = false;
							
							if (tu->types[decl.type].kind != KIND_FUNC) {
								report(REPORT_ERROR, &s->line_arena[decl.loc], "Declaration's expression has a weird semicolon");
								abort();
							}
							
							if (old_definition && old_definition->current != 0) {
								report_two_spots(REPORT_ERROR, 
												 &s->line_arena[decl.loc],
												 &s->line_arena[old_definition->stmt->loc],
												 "Cannot redefine function declaration",
												 NULL, NULL, "previous definition was:");
								abort();
							}
							
							//n->decl.type = decl.type;
							//n->decl.attrs = attr;
							n->decl.attrs.is_root = !(attr.is_static || attr.is_inline);
							
							sym.terminator = '}';
							sym.current = s->current;
							tokens_next(s);
							
							// we postpone parsing the function bodies
							// balance some brackets: '{' SOMETHING '}'
							int depth = 1;
							while (depth) {
								Token* t = tokens_get(s);
								
								if (t->type == '\0') {
									report(REPORT_ERROR, &s->line_arena[t->location], "Function body ended in EOF");
									abort();
								} else if (t->type == '{') {
									depth++;
								} else if (t->type == '}') {
									depth--;
								}
								
								tokens_next(s);
							}
						}
						
						// slap that bad boy into the symbol table
						shput(global_symbols, decl.name, sym);
						
						if (!requires_terminator) {
							// function bodies just end the declaration list
							break;
						}
						
						if (tokens_get(s)->type == 0) {
							report(REPORT_ERROR, loc, "declaration list ended with EOF instead of semicolon.");
							abort();
						} else if (tokens_get(s)->type == ';') {
							tokens_next(s);
							break;
						} else if (tokens_get(s)->type == ',') {
							tokens_next(s);
							continue;
						}
					}
				}
			}
		}
	}
	
	// Phase 2: resolve all expressions or function bodies
	// This part is parallel because im the fucking GOAT
	out_of_order_mode = false;
	
	timed_block("phase 2") {
		if (false /*thread_pool != NULL*/) {
			// disabled until we change the tables to arenas
			size_t count = shlen(global_symbols);
			size_t padded = (count + 4095) & ~4095;
			
			// passed to the threads to identify when things are done
			atomic_size_t tasks_remaining = (count + 4095) / 4096;
			
			for (size_t i = 0; i < padded; i += 4096) {
				size_t limit = i+4096;
				if (limit > count) limit = count;
				
				ParserTaskInfo* task = tls_push(sizeof(ParserTaskInfo));
				*task = (ParserTaskInfo){ 
					.tasks_remaining = &tasks_remaining,
					.start = i,
					.end = limit
				};
				
				// we transfer a bunch of our thread local state to the task
				// and they'll use that to continue and build up parse on other
				// threads.
				task->tu = tu;
				task->incomplete_tags = incomplete_tags;
				task->global_symbols = global_symbols;
				task->base_token_stream = s;
				
				threadpool_submit(thread_pool, phase2_parse_task, task);
			}
			
			while (tasks_remaining != 0) {
				thrd_yield();
			}
		} else {
			// single threaded mode
			parse_global_symbols(tu, 0, shlen(global_symbols), *s);
		}
	}
#else
	while (tokens_get(s)->type) {
		while (tokens_get(s)->type == ';') tokens_next(s);
		
		// TODO(NeGate): Correctly parse pragmas instead of ignoring them.
		if (tokens_get(s)->type == TOKEN_KW_Pragma) {
			tokens_next(s);
			expect(s, '(');
			
			if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
				generic_error(s, "pragma declaration expects string literal");
			}
			tokens_next(s);
			
			expect(s, ')');
			continue;
		} else if (tokens_get(s)->type == TOKEN_KW_Static_assert) {
			tokens_next(s);
			expect(s, '(');
			
			intmax_t condition = parse_const_expr(tu, s);
			if (tokens_get(s)->type == ',') {
				tokens_next(s);
				
				Token* t = tokens_get(s);
				if (t->type != TOKEN_STRING_DOUBLE_QUOTE) {
					generic_error(s, "static assertion expects string literal");
				}
				tokens_next(s);
				
				if (condition == 0) {
					report(REPORT_ERROR, &s->line_arena[tokens_get(s)->location], "Static assertion failed: '%.*s'", (int)(t->end - t->start), t->start);
				}
			} else {
				if (condition == 0) {
					report(REPORT_ERROR, &s->line_arena[tokens_get(s)->location], "Static assertion failed");
				}
			}
			
			expect(s, ')');
			continue;
		}
		
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(tu, s, &attr);
		attr.is_root = !(attr.is_static || attr.is_inline);
		
		if (attr.is_typedef) {
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (tokens_get(s)->type != ';') {
				if (expect_comma) {
					expect(s, ',');
				} else expect_comma = true;
				
				Decl decl = parse_declarator(tu, s, type, false, false);
				assert(decl.name);
				
				// make typedef
				Stmt* n = make_stmt(tu, s, STMT_DECL);
				n->loc = decl.loc;
				n->decl = (struct StmtDecl){
					.name = decl.name,
					.type = decl.type,
					.attrs = attr
				};
				arrput(tu->top_level_stmts, n);
				
				Symbol sym = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = STORAGE_TYPEDEF
				};
				shput(global_symbols, decl.name, sym);
			}
			
			expect(s, ';');
		} else {
			if (tokens_get(s)->type == ';') {
				tokens_next(s);
				continue;
			}
			
			Decl decl = parse_declarator(tu, s, type, false, false);
			if (tu->types[decl.type].kind == KIND_FUNC) {
				// function
				Symbol* sym = find_global_symbol((const char*)decl.name);
				
				Stmt* n;
				bool is_redefine = false;
				bool is_redefining_body = false;
				if (sym) {
					is_redefine = true;
					
					StorageClass sclass = attr.is_static ? STORAGE_STATIC_FUNC : STORAGE_FUNC;
					sym->storage_class = sclass;
					
					// convert forward decl into proper function
					n = sym->stmt;
					if (n->op == STMT_FUNC_DECL && !attr.is_inline) {
						is_redefining_body = true;
					}
					
					if (!type_equal(tu, n->decl.type, decl.type)) {
						if (1) {
							report(REPORT_ERROR, &s->line_arena[decl.loc], "redefinition of '%s' as different kind of symbol", decl.name);
							report(REPORT_INFO, &s->line_arena[n->loc], "previous definition is here");
							printf("\n");
						} else {
							char msg[100];
							snprintf(msg, 100, "redefinition of '%s' as different kind of symbol", decl.name);
							
							report_two_spots(REPORT_ERROR, 
											 &s->line_arena[n->loc],
											 &s->line_arena[decl.loc],
											 msg, NULL, NULL, "previous definition was:");
						}
					}
					
					n->loc = decl.loc;
				} else {
					// New symbol
					n = make_stmt(tu, s, STMT_DECL);
					n->loc = decl.loc;
					n->decl = (struct StmtDecl){
						.type = decl.type,
						.name = decl.name,
						.attrs = attr,
						.initial_as_stmt = NULL
					};
					n->decl.attrs.is_root = false;
					
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
						report_two_spots(REPORT_ERROR, 
										 &s->line_arena[n->loc],
										 &s->line_arena[decl.loc],
										 "Cannot redefine function decl",
										 NULL, NULL, "previous definition was:");
					} else if (strcmp((const char*)decl.name, "WinMain") == 0) {
						settings.using_winmain = true;
					}
					
					assert(local_symbol_start == 0);
					assert(local_symbol_count == 0);
					
					n->decl.type  = decl.type;
					n->decl.attrs = attr;
					
					parse_function_definition(tu, s, n);
				} else if (tokens_get(s)->type == ';') {
					// Forward decl
					tokens_next(s);
				} else {
					abort();
				}
				
				if (!is_redefine) {
					arrput(tu->top_level_stmts, n);
				}
				
				local_symbol_start = local_symbol_count = 0;
			} else {
				// Normal decls
				// TODO(NeGate): we should merge the global decls and local
				// decls codepaths...
				if (!decl.name) {
					generic_error(s, "declaration is missing a name!");
				}
				
				if (attr.is_extern) {
					attr.is_root = false;
				}
				
				Stmt* n = make_stmt(tu, s, STMT_GLOBAL_DECL);
				n->loc = decl.loc;
				n->decl = (struct StmtDecl){
					.type = decl.type,
					.name = decl.name,
					.attrs = attr
				};
				arrput(tu->top_level_stmts, n);
				
				Symbol sym = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL,
					.stmt = n
				};
				shput(global_symbols, decl.name, sym);
				
				if (tokens_get(s)->type == '=') {
					tokens_next(s);
					
					ExprIndex starting_point = big_array_length(tu->exprs);
					ExprIndex e;
					if (tokens_get(s)->type == '@') {
						// function literals are a Cuik extension
						// TODO(NeGate): error messages
						tokens_next(s);
						e = parse_function_literal(tu, s, decl.type);
					} else if (tokens_get(s)->type == '{') {
						tokens_next(s);
						
						e = parse_initializer(tu, s, TYPE_NONE);
					} else {
						e = parse_expr_l14(tu, s);
					}
					n->decl.initial = e;
					create_symbol_use_list(tu, n, starting_point, false);
				}
				
				if (tokens_get(s)->type == ',') {
					while (tokens_get(s)->type != ';') {
						expect(s, ',');
						
						Decl decl = parse_declarator(tu, s, type, false, false);
						assert(decl.name);
						
						Stmt* n = make_stmt(tu, s, STMT_GLOBAL_DECL);
						n->loc = decl.loc;
						n->decl = (struct StmtDecl){
							.type = decl.type,
							.name = decl.name,w
								.attrs = attr
						};
						arrput(tu->top_level_stmts, n);
						
						Symbol sym = (Symbol){
							.name = decl.name,
							.type = decl.type,
							.storage_class = attr.is_static ? STORAGE_STATIC_VAR : STORAGE_GLOBAL,
							.stmt = n
						};
						shput(global_symbols, decl.name, sym);
						
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							ExprIndex starting_point = big_array_length(tu->exprs);
							ExprIndex e;
							if (tokens_get(s)->type == '@') {
								// function literals are a Cuik extension
								// TODO(NeGate): error messages
								tokens_next(s);
								e = parse_function_literal(tu, s, decl.type);
							} else if (tokens_get(s)->type == '{') {
								tokens_next(s);
								
								e = parse_initializer(tu, s, TYPE_NONE);
							} else {
								e = parse_expr_l14(tu, s);
							}
							n->decl.initial = e;
							create_symbol_use_list(tu, n, starting_point, false);
						}
					}
				}
				
				expect(s, ';');
			}
		}
	}
	
	//assert(s->current == arrlen(s->tokens) - 1);
	
	// NOTE(NeGate): This is a Cuik extension, it allows normal symbols
	// like functions to declared out of order.
	for (size_t i = 1, count = big_array_length(tu->exprs); i < count; i++) {
		if (tu->exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
			if (!resolve_unknown_symbol(tu, i)) {
				// try enum names
				const unsigned char* name = tu->exprs[i].unknown_sym;
				
				// NOTE(NeGate): this might be slow
				for (size_t j = 1, count = big_array_length(tu->enum_entries); j < count; j++) {
					if (cstr_equals(name, tu->enum_entries[j].name)) {
						int value = tu->enum_entries[j].value;
						ExprIndex chain = tu->exprs[i].next_symbol_in_chain;
						
						tu->exprs[i].op = EXPR_ENUM;
						tu->exprs[i].enum_val = (struct ExprEnum){ value, chain };
						goto success;
					}
				}
				
				// check if it's builtin
				ptrdiff_t search = shgeti(target_desc.builtin_func_map, name);
				if (search < 0) {
					SourceLoc* loc = &s->line_arena[tu->exprs[i].loc];
					report(REPORT_ERROR, loc, "could not resolve symbol: %s", name);
				}
			}
		}
		success:;
	}
#endif
	
	if (tu->hack.name) {
		Symbol* sym = find_global_symbol(tu->hack.name);
		
		if (sym != NULL) {
			tu->hack.type = sym->type;
		} else {
			tu->hack.type = TYPE_NONE;
		}
	}
	
	current_switch_or_case = 0;
	current_breakable = 0;
	current_continuable = 0;
	
	local_symbol_start = 0;
	local_symbol_count = 0;
	
	shfree(incomplete_tags);
	shfree(global_symbols);
	shfree(labels);
}

void translation_unit_deinit(TranslationUnit* tu) {
	if (tu->is_free) return;
	
	big_array_destroy(tu->types);
	big_array_destroy(tu->params);
	big_array_destroy(tu->enum_entries);
	big_array_destroy(tu->exprs);
	arrfree(tu->top_level_stmts);
	
	arena_free(&tu->ast_arena);
	tu->is_free = true;
}

Stmt* resolve_unknown_symbol(TranslationUnit* tu, ExprIndex e) {
	Symbol* sym = find_global_symbol((char*)tu->exprs[e].unknown_sym);
	if (!sym) return 0;
	
	// Parameters are local and a special case how tf
	assert(sym->storage_class != STORAGE_PARAM);
	
	tu->exprs[e].op = EXPR_SYMBOL;
	tu->exprs[e].symbol = sym->stmt;
	return sym->stmt;
}

static Symbol* find_local_symbol(TokenStream* restrict s) {
	Token* t = tokens_get(s);
	const unsigned char* name = t->start;
	size_t length = t->end - t->start;
	
	// Try local variables
	size_t i = local_symbol_count;
	size_t start = local_symbol_start;
	while (i-- > start) {
		const unsigned char* sym = local_symbols[i].name;
		size_t sym_length = strlen((const char*)sym);
		
		if (sym_length == length && memcmp(name, sym, length) == 0) {
			return &local_symbols[i];
		}
	}
	
	return NULL;
}

////////////////////////////////
// STATEMENTS
////////////////////////////////
static void create_symbol_use_list(TranslationUnit* tu, Stmt* n, ExprIndex starting_point, bool has_labels) {
	ExprIndex symbol_list_start = 0;
	ExprIndex symbol_list_end = 0;
	
	// resolve any unresolved label references
	for (size_t i = starting_point, count = big_array_length(tu->exprs); i < count; i++) {
		if (tu->exprs[i].op == EXPR_SYMBOL ||
			tu->exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
			if (symbol_list_start == 0) {
				// initialize chain
				symbol_list_start = symbol_list_end = i;
			} else {
				// append
				tu->exprs[symbol_list_end].next_symbol_in_chain = i;
				tu->exprs[i].next_symbol_in_chain = 0;
				symbol_list_end = i;
			}
		}
		
		if (has_labels && tu->exprs[i].op == EXPR_UNKNOWN_SYMBOL) {
			const unsigned char* name = tu->exprs[i].unknown_sym;
			
			ptrdiff_t search = shgeti(labels, name);
			if (search >= 0) {
				tu->exprs[i].op = EXPR_SYMBOL;
				tu->exprs[i].symbol = labels[search].value;
			}
		}
	}
	
	if (symbol_list_end) {
		tu->exprs[symbol_list_end].next_symbol_in_chain = 0;
	}
	
	n->decl.first_symbol = symbol_list_start;
}

static void parse_function_definition(TranslationUnit* tu, TokenStream* restrict s, Stmt* n) {
	TypeIndex type = n->decl.type;
	
	ParamIndex param_list = tu->types[type].func.param_list;
	ParamIndex param_count = tu->types[type].func.param_count;
	
	assert(local_symbol_start == local_symbol_count);
	if (param_count >= INT16_MAX) {
		report(REPORT_ERROR, &s->line_arena[n->loc], "Function parameter count cannot exceed %d (got %d)", param_count, MAX_LOCAL_SYMBOLS);
		abort();
	}
	
	for (size_t i = 0; i < param_count; i++) {
		Param* p = &tu->params[param_list + i];
		
		if (p->name) {
			local_symbols[local_symbol_count++] = (Symbol){
				.name = p->name,
				.type = p->type,
				.storage_class = STORAGE_PARAM,
				.param_num = i
			};
		}
	}
	
	//report(REPORT_INFO, &s->line_arena[tokens_get(s)->location], "Opening brace for function");
	tokens_next(s);
	
	ExprIndex starting_point = big_array_length(tu->exprs);
	Stmt* body = parse_compound_stmt(tu, s);
	
	n->op = STMT_FUNC_DECL;
	n->decl.initial_as_stmt = body;
	
	create_symbol_use_list(tu, n, starting_point, true);
	
	// hmm... how tf do labels operate in this case...
	// TODO(NeGate): redo the label look in a sec
	shfree(labels);
}

static Stmt* parse_compound_stmt(TranslationUnit* tu, TokenStream* restrict s) {
	// mark when the local scope starts
	int saved = local_symbol_count;
	
	Stmt* node = make_stmt(tu, s, STMT_COMPOUND);
	
	size_t body_count = 0; // He be fuckin
	void* body = tls_save();
	
	while (tokens_get(s)->type != '}') {
		if (tokens_get(s)->type == ';') {
			tokens_next(s);
		} else {
			//report(REPORT_INFO, &s->line_arena[tokens_get(s)->location], "Woah!!!");
			
			Stmt* stmt = parse_stmt(tu, s);
			if (stmt) {
				*((Stmt**)tls_push(sizeof(Stmt*))) = stmt;
				body_count++;
			} else {
				parse_decl_or_expr(tu, s, &body_count);
			}
		}
	}
	//report(REPORT_INFO, &s->line_arena[tokens_get(s)->location], "Closing brace");
	expect(s, '}');
	
	local_symbol_count = saved;
	
	Stmt** stmt_array = arena_alloc(&thread_arena, body_count * sizeof(Stmt*), _Alignof(Stmt*));
	memcpy(stmt_array, body, body_count * sizeof(Stmt*));
	
	node->compound = (struct StmtCompound) {
		.kids = stmt_array,
		.kids_count = body_count
	};
	
	tls_restore(body);
	return node;
}

// TODO(NeGate): Doesn't handle declarators or expression-statements
static Stmt* parse_stmt(TranslationUnit* tu, TokenStream* restrict s) {
	TknType peek = tokens_get(s)->type;
	
	if (peek == '{') {
		//report(REPORT_INFO, &s->line_arena[tokens_get(s)->location], "Opening brace for local compound");
		
		tokens_next(s);
		return parse_compound_stmt(tu, s);
	} else if (peek == TOKEN_KW_return) {
		tokens_next(s);
		
		ExprIndex e = 0;
		if (tokens_get(s)->type != ';') {
			e = parse_expr(tu, s);
		}
		
		Stmt* n = make_stmt(tu, s, STMT_RETURN);
		n->return_ = (struct StmtReturn){
			.expr = e
		};
		
		expect_with_reason(s, ';', "return");
		return n;
	} else if (peek == TOKEN_KW_if) {
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_IF);
		
		ExprIndex cond;
		{
			SourceLoc* opening_loc = &s->line_arena[tokens_get(s)->location];
			expect(s, '(');
			
			cond = parse_expr(tu, s);
			
			expect_closing_paren(s, opening_loc);
		}
		Stmt* body = parse_stmt_or_expr(tu, s);
		
		Stmt* next = 0;
		if (tokens_get(s)->type == TOKEN_KW_else) {
			tokens_next(s);
			next = parse_stmt_or_expr(tu, s);
		}
		
		n->if_ = (struct StmtIf){
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (peek == TOKEN_KW_switch) {
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_SWITCH);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(tu, s);
		expect(s, ')');
		
		n->switch_ = (struct StmtSwitch){
			.condition = cond
		};
		
		// begin a new chain but keep the old one
		Stmt* old_switch = current_switch_or_case;
		current_switch_or_case = n;
		
		Stmt* old_breakable = current_breakable;
		current_breakable = n;
		{
			Stmt* body = parse_stmt_or_expr(tu, s);
			n->switch_.body = body;
		}
		current_breakable = old_breakable;
		current_switch_or_case = old_switch;
		return n;
	} else if (peek == TOKEN_KW_case) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_CASE);
		
		switch (current_switch_or_case->op) {
			case STMT_CASE:    current_switch_or_case->case_.next    = n; break;
			case STMT_DEFAULT: current_switch_or_case->default_.next = n; break;
			case STMT_SWITCH:  current_switch_or_case->switch_.next  = n; break;
			default: abort();
		}
		current_switch_or_case = n;
		
		intmax_t key = parse_const_expr(tu, s);
		expect(s, ':');
		
		n->case_ = (struct StmtCase){
			.key = key, .body = 0, .next = 0
		};
		
		Stmt* body = parse_stmt_or_expr(tu, s);
		n->case_.body = body;
		return n;
	} else if (peek == TOKEN_KW_default) {
		// TODO(NeGate): error messages
		assert(current_switch_or_case);
		
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_DEFAULT);
		
		switch (current_switch_or_case->op) {
			case STMT_CASE:    current_switch_or_case->case_.next    = n; break;
			case STMT_DEFAULT: current_switch_or_case->default_.next = n; break;
			case STMT_SWITCH:  current_switch_or_case->switch_.next  = n; break;
			default: abort();
		}
		current_switch_or_case = n;
		expect(s, ':');
		
		n->default_ = (struct StmtDefault){
			.body = 0, .next = 0
		};
		
		Stmt* body = parse_stmt_or_expr(tu, s);
		n->default_.body = body;
		return n;
	} else if (peek == TOKEN_KW_break) {
		// TODO(NeGate): error messages
		assert(current_breakable);
		
		tokens_next(s);
		expect(s, ';');
		
		Stmt* n = make_stmt(tu, s, STMT_BREAK);
		n->break_ = (struct StmtBreak){
			.target = current_breakable
		};
		return n;
	} else if (peek == TOKEN_KW_continue) {
		// TODO(NeGate): error messages
		assert(current_continuable);
		
		tokens_next(s);
		expect(s, ';');
		
		Stmt* n = make_stmt(tu, s, STMT_CONTINUE);
		n->continue_ = (struct StmtContinue){
			.target = current_continuable
		};
		return n;
	} else if (peek == TOKEN_KW_while) {
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_WHILE);
		
		expect(s, '(');
		ExprIndex cond = parse_expr(tu, s);
		expect(s, ')');
		
		// Push this as a breakable statement
		Stmt* body;
		{
			Stmt* old_breakable = current_breakable;
			current_breakable = n;
			Stmt* old_continuable = current_continuable;
			current_continuable = n;
			
			body = parse_stmt_or_expr(tu, s);
			
			current_breakable = old_breakable;
			current_continuable = old_continuable;
		}
		
		n->while_ = (struct StmtWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (peek == TOKEN_KW_for) {
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_FOR);
		
		int saved = local_symbol_count;
		
		expect(s, '(');
		
		// it's either nothing, a declaration, or an expression
		Stmt* first = 0;
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
			Stmt** stmt_array = arena_alloc(&thread_arena, body_count * sizeof(Stmt*), _Alignof(Stmt*));
			memcpy(stmt_array, body, body_count * sizeof(Stmt*));
			
			first->compound = (struct StmtCompound) {
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
		Stmt* body;
		{
			Stmt* old_breakable = current_breakable;
			current_breakable = n;
			Stmt* old_continuable = current_continuable;
			current_continuable = n;
			
			body = parse_stmt_or_expr(tu, s);
			
			current_breakable = old_breakable;
			current_continuable = old_continuable;
		}
		
		// restore local symbol scope
		local_symbol_count = saved;
		
		n->for_ = (struct StmtFor){
			.first = first,
			.cond = cond,
			.body = body,
			.next = next
		};
		return n;
	} else if (peek == TOKEN_KW_do) {
		tokens_next(s);
		Stmt* n = make_stmt(tu, s, STMT_DO_WHILE);
		
		// Push this as a breakable statement
		Stmt* body;
		{
			Stmt* old_breakable = current_breakable;
			current_breakable = n;
			Stmt* old_continuable = current_continuable;
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
		
		n->do_while = (struct StmtDoWhile){
			.cond = cond,
			.body = body
		};
		return n;
	} else if (peek == TOKEN_KW_goto) {
		tokens_next(s);
		
		Stmt* n = make_stmt(tu, s, STMT_GOTO);
		
		ExprIndex target = parse_expr(tu, s);
		n->goto_ = (struct StmtGoto){
			.target = target
		};
		
		expect(s, ';');
		return n;
	} else if (peek == TOKEN_IDENTIFIER &&
			   tokens_peek(s)->type == TOKEN_COLON) {
		// label amirite
		// IDENTIFIER COLON STMT
		Token* t = tokens_get(s);
		Atom name = atoms_put(t->end - t->start, t->start);
		
		Stmt* n = make_stmt(tu, s, STMT_LABEL);
		n->label = (struct StmtLabel){
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
	if (tokens_get(s)->type == TOKEN_KW_Pragma) {
		tokens_next(s);
		expect(s, '(');
		
		if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
			generic_error(s, "pragma declaration expects string literal");
		}
		tokens_next(s);
		
		expect(s, ')');
	} else if (tokens_get(s)->type == ';') {
		tokens_next(s);
	} else if (is_typename(s)) {
		Attribs attr = { 0 };
		TypeIndex type = parse_declspec(tu, s, &attr);
		
		if (attr.is_typedef) {
			// don't expect one the first time
			bool expect_comma = false;
			while (tokens_get(s)->type != ';') {
				if (expect_comma) {
					expect_with_reason(s, ',', "typedef");
				} else expect_comma = true;
				
				Decl decl = parse_declarator(tu, s, type, false, false);
				assert(decl.name);
				
				// make typedef
				Stmt* n = make_stmt(tu, s, STMT_DECL);
				n->loc = decl.loc;
				n->decl = (struct StmtDecl){
					.name = decl.name,
					.type = decl.type,
					.attrs = attr
				};
				
				if (local_symbol_count >= MAX_LOCAL_SYMBOLS) {
					report(REPORT_ERROR, &s->line_arena[decl.loc], "Local symbol count exceeds %d (got %d)", MAX_LOCAL_SYMBOLS, local_symbol_count);
					abort();
				}
				
				local_symbols[local_symbol_count++] = (Symbol){
					.name = decl.name,
					.type = decl.type,
					.storage_class = STORAGE_TYPEDEF
				};
			}
			
			expect(s, ';');
		} else {
			// TODO(NeGate): Kinda ugly
			// don't expect one the first time
			bool expect_comma = false;
			while (tokens_get(s)->type != ';') {
				if (expect_comma) {
					if (tokens_get(s)->type == '{') {
						generic_error(s, "nested functions are not allowed... yet");
					}
					
					expect_with_reason(s, ',', "declaration");
				} else expect_comma = true;
				
				Decl decl = parse_declarator(tu, s, type, false, false);
				
				Stmt* n = make_stmt(tu, s, STMT_DECL);
				n->loc = decl.loc;
				n->decl = (struct StmtDecl){
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
					
					if (tokens_get(s)->type == '@') {
						// function literals are a Cuik extension
						// TODO(NeGate): error messages
						tokens_next(s);
						initial = parse_function_literal(tu, s, decl.type);
					} else if (tokens_get(s)->type == '{') {
						tokens_next(s);
						
						initial = parse_initializer(tu, s, TYPE_NONE);
					} else {
						initial = parse_expr_l14(tu, s);
					}
				}
				
				n->decl.initial = initial;
				*((Stmt**)tls_push(sizeof(Stmt*))) = n;
				*body_count += 1;
			}
			
			expect(s, ';');
		}
	} else {
		Stmt* n = make_stmt(tu, s, STMT_EXPR);
		
		ExprIndex expr = parse_expr(tu, s);
		n->expr = (struct StmtExpr){
			.expr = expr
		};
		
		*((Stmt**)tls_push(sizeof(Stmt*))) = n;
		*body_count += 1;
		expect(s, ';');
	}
}

static Stmt* parse_stmt_or_expr(TranslationUnit* tu, TokenStream* restrict s) {
	if (tokens_get(s)->type == TOKEN_KW_Pragma) {
		tokens_next(s);
		expect(s, '(');
		
		if (tokens_get(s)->type != TOKEN_STRING_DOUBLE_QUOTE) {
			generic_error(s, "pragma declaration expects string literal");
		}
		tokens_next(s);
		
		expect(s, ')');
		return 0;
	} else if (tokens_get(s)->type == ';') {
		tokens_next(s);
		return 0;
	} else {
		Stmt* stmt = parse_stmt(tu, s);
		
		if (stmt) {
			return stmt;
		} else {
			Stmt* n = make_stmt(tu, s, STMT_EXPR);
			
			ExprIndex expr = parse_expr(tu, s);
			n->expr = (struct StmtExpr){
				.expr = expr
			};
			
			expect(s, ';');
			return n;
		}
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

static void expect(TokenStream* restrict s, char ch) {
	if (tokens_get(s)->type != ch) {
		Token* t = tokens_get(s);
		SourceLoc* loc = &s->line_arena[t->location];
		
		report(REPORT_ERROR, loc, "expected '%c' got '%.*s'", ch, (int)(t->end - t->start), t->start);
		abort();
	}
	
	tokens_next(s);
}

static void expect_closing_paren(TokenStream* restrict s, SourceLoc* opening) {
	if (tokens_get(s)->type != ')') {
		Token* t = tokens_get(s);
		SourceLoc* loc = &s->line_arena[t->location];
		
		report_two_spots(REPORT_ERROR, opening,
						 loc,
						 "expected closing parenthesis",
						 "open", "close?", NULL);
		abort();
	}
	
	tokens_next(s);
}

static void expect_with_reason(TokenStream* restrict s, char ch, const char* reason) {
	if (tokens_get(s)->type != ch) {
		Token* t = tokens_get(s);
		SourceLoc* loc = &s->line_arena[t->location];
		
		report(REPORT_ERROR, loc, "expected '%c' for %s", ch, reason);
		abort();
	}
	
	tokens_next(s);
}
