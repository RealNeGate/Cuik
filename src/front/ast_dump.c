// It's only *sorta* part of the semantics pass
#include "sema.h"

// two simple temporary buffers to represent type_as_string results
static thread_local char temp_string0[1024], temp_string1[1024];
static thread_local StmtIndex function_stmt;
static thread_local bool barz[64];

static void print_barz(int depth, bool last_node) {
	if (last_node) {
		for (int i = 0; i < depth-1; i++) {
			printf(barz[i] ? "| " : "  ");
		}
		printf("`-");
	} else {
		for (int i = 0; i < depth-1; i++) {
			printf(barz[i] ? "| " : "  ");
		}
		printf("|-");
	}
	barz[depth-1] = !last_node;
}

static void dump_expr(TranslationUnit* tu, FILE* stream, ExprIndex e, int depth, bool last_node) {
	print_barz(depth, last_node);
	
	Expr* restrict ep = &tu->exprs[e];
	if (ep->cast_type != ep->type) {
		type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
		type_as_string(tu, sizeof(temp_string1), temp_string1, ep->cast_type);
		
		if (ep->op != EXPR_CAST && ep->cast_type > TYPE_VOID) {
			// we don't wanna place implicit casts to void, it's weird
			fprintf(stream, "ImplicitCast '%s' -> '%s'\n", temp_string0, temp_string1);
			
			depth++;
			print_barz(depth, true);
		}
	}
	
	switch (ep->op) {
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
			
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "CharLiteral %u '%s'\n", codepoint, temp_string0);
			break;
		}
		case EXPR_INT: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "IntegerLiteral %llu '%s'\n", ep->int_num.num, temp_string0);
			break;
		}
		case EXPR_ENUM: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "EnumLiteral %lld '%s'\n", (long long)ep->enum_val.num, temp_string0);
			break;
		}
		case EXPR_FLOAT32:
		case EXPR_FLOAT64: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "FloatLiteral %f '%s'\n", ep->float_num, temp_string0);
			break;
		}
		case EXPR_SYMBOL: {
			StmtIndex stmt = ep->symbol;
			
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			if (tu->stmts[stmt].op == STMT_LABEL) {
				fprintf(stream, "LabelRef\n");
			} else {
				fprintf(stream, "Symbol %s '%s'\n", tu->stmts[stmt].decl.name, temp_string0);
			}
			break;
		}
		case EXPR_PARAM: {
			int param_num = ep->param_num;
			
			Type* func_type = &tu->types[tu->stmts[function_stmt].decl.type];
			Param* params = &tu->params[func_type->func.param_list];
			
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Symbol %s '%s'\n", params[param_num].name, temp_string0);
			break;
		}
		case EXPR_STR: {
			// TODO(NeGate): Convert the string back into a C string literal so we don't cause any weird text printing
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			
			const unsigned char* start = ep->str.start;
			const unsigned char* end = ep->str.end;
			
			char* out_start = tls_push(1024);
			char* out = out_start;
			
			while (start != end) {
				// it's either printed as normal or \x00
				unsigned int ch = *start;
				bool is_normal = (*start >= 0x20 && *start < 0x7F);
				
				ptrdiff_t len = (out-out_start) + (is_normal ? 1 : 4);
				if (len >= 1024) break;
				
				if (is_normal) {
					*out++ = ch;
				} else {
					*out++ = '\\';
					*out++ = 'x';
					*out++ = "0123456789ABCDEF"[(ch>>8) & 0xF];
					*out++ = "0123456789ABCDEF"[ch & 0xF];
				}
				
				start++;
			}
			
			fprintf(stream, "StringLiteral \"%.*s\" '%s'\n", (int)(out-out_start), out_start, temp_string0);
			tls_restore(out_start);
			break;
		}
		case EXPR_INITIALIZER: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Initializer '%s'\n", temp_string0);
			break;
		}
		case EXPR_CALL: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "FunctionCall '%s'\n", temp_string0);
			
			ExprIndex* args = ep->call.param_start;
			int arg_count = ep->call.param_count;
			
			dump_expr(tu, stream, ep->call.target, depth + 1, arg_count == 0);
			
			for (size_t i = 0; i < arg_count; i++) {
				dump_expr(tu, stream, args[i], depth + 1, i == (arg_count-1));
			}
			break;
		}
		case EXPR_TERNARY: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Ternary '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->ternary_op.left, depth + 1, false);
			dump_expr(tu, stream, ep->ternary_op.middle, depth + 1, false);
			dump_expr(tu, stream, ep->ternary_op.right, depth + 1, true);
			break;
		}
		case EXPR_ARROW: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Arrow %s '%s'\n", ep->arrow.name, temp_string0);
			
			dump_expr(tu, stream, ep->arrow.base, depth + 1, true);
			break;
		}
		case EXPR_DOT: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Dot %s '%s'\n", ep->dot.name, temp_string0);
			
			dump_expr(tu, stream, ep->dot.base, depth + 1, true);
			break;
		}
		case EXPR_SUBSCRIPT: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Subscript '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->subscript.base, depth + 1, false);
			dump_expr(tu, stream, ep->subscript.index, depth + 1, true);
			break;
		}
		case EXPR_DEREF: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Deref '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_ADDR: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Addr '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_POST_INC: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "PostIncrement '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_POST_DEC: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "PostDecrement '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_PRE_INC: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "PreIncrement '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_PRE_DEC: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "PreDecrement '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_LOGICAL_NOT: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "LogicalNot '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_NOT: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "BinaryNot '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_NEGATE: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "Negate '%s'\n", temp_string0);
			
			dump_expr(tu, stream, ep->unary_op.src, depth + 1, true);
			break;
		}
		case EXPR_CAST: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			type_as_string(tu, sizeof(temp_string1), temp_string1, ep->cast.type);
			
			fprintf(stream, "Cast '%s' -> '%s'\n", temp_string0, temp_string1);
			dump_expr(tu, stream, ep->cast.src, depth + 1, true);
			break;
		}
		case EXPR_COMMA:
		case EXPR_PLUS:
		case EXPR_MINUS:
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_PERCENT:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_PLUS_ASSIGN:
		case EXPR_MINUS_ASSIGN:
		case EXPR_ASSIGN:
		case EXPR_TIMES_ASSIGN:
		case EXPR_SLASH_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPGT:
		case EXPR_CMPGE:
		case EXPR_CMPLT:
		case EXPR_CMPLE:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR:
		case EXPR_PTRADD:
		case EXPR_PTRSUB:
		case EXPR_PTRDIFF: {
			static const char* names[] = {
				[EXPR_COMMA] = "Comma",
				
				[EXPR_PLUS] = "Plus",
				[EXPR_MINUS] = "Minus",
				[EXPR_TIMES] = "Times",
				[EXPR_SLASH] = "Slash",
				[EXPR_PERCENT] = "Percent",
				[EXPR_AND] = "And",
				[EXPR_OR] = "Or",
				[EXPR_XOR] = "Xor",
				[EXPR_SHL] = "ShiftLeft",
				[EXPR_SHR] = "ShiftRight",
				
				[EXPR_PLUS_ASSIGN]  = "PlusAssign",
				[EXPR_MINUS_ASSIGN] = "MinusAssign",
				[EXPR_ASSIGN]       = "Assign",
				[EXPR_TIMES_ASSIGN] = "TimesAssign",
				[EXPR_SLASH_ASSIGN] = "SlashAssign",
				[EXPR_AND_ASSIGN]   = "AndAssign",
				[EXPR_OR_ASSIGN]    = "OrAssign",
				[EXPR_XOR_ASSIGN]   = "XorAssign",
				[EXPR_SHL_ASSIGN]   = "ShiftLeftAssign",
				[EXPR_SHR_ASSIGN]   = "ShiftRightAssign",
				
				[EXPR_CMPEQ] = "CompareEqual",
				[EXPR_CMPNE] = "CompareNotEqual",
				[EXPR_CMPGT] = "CompareGreater",
				[EXPR_CMPGE] = "CompareGreaterOrEqual",
				[EXPR_CMPLT] = "CompareLesser",
				[EXPR_CMPLE] = "CompareLesserOrEqual",
				
				[EXPR_LOGICAL_AND] = "LogicalAnd",
				[EXPR_LOGICAL_OR] = "LogicalOr",
				
				[EXPR_PTRADD] = "PointerAdd",
				[EXPR_PTRSUB] = "PointerSub",
				[EXPR_PTRDIFF] = "PointerDiff"
			};
			
			type_as_string(tu, sizeof(temp_string0), temp_string0, ep->type);
			fprintf(stream, "%s '%s'\n", names[ep->op], temp_string0);
			
			dump_expr(tu, stream, ep->bin_op.left, depth + 1, false);
			dump_expr(tu, stream, ep->bin_op.right, depth + 1, true);
			break;
		}
		default: abort();
	}
	
	barz[depth-1] = false;
}

static void dump_stmt(TranslationUnit* tu, FILE* stream, StmtIndex s, int depth, bool last_node) {
	print_barz(depth, last_node);
	
	const Stmt* restrict sp = &tu->stmts[s];
	switch (sp->op) {
		case STMT_DECL:
		case STMT_GLOBAL_DECL: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, sp->decl.type);
			
			if (sp->decl.attrs.is_typedef) {
				fprintf(stream, "TypedefDecl %s '%s'\n", sp->decl.name, temp_string0);
			} else {
				fprintf(stream, "VarDecl %s '%s'\n", sp->decl.name, temp_string0);
				
				if (sp->decl.initial) {
					dump_expr(tu, stream, tu->stmts[s].decl.initial, depth + 1, true);
				}
			}
			break;
		}
		case STMT_FUNC_DECL: {
			type_as_string(tu, sizeof(temp_string0), temp_string0, sp->decl.type);
			fprintf(stream, "FunctionDecl %s '%s'\n", sp->decl.name, temp_string0);
			
			StmtIndex old_function_stmt = function_stmt;
			function_stmt = s;
			
			dump_stmt(tu, stream, (StmtIndex)sp->decl.initial, depth + 1, true);
			
			function_stmt = old_function_stmt;
			break;
		}
		case STMT_COMPOUND: {
			fprintf(stream, "Compound\n");
			
			StmtIndex* kids = sp->compound.kids;
			size_t count = sp->compound.kids_count;
			
			for (size_t i = 0; i < count; i++) {
				dump_stmt(tu, stream, kids[i], depth + 1, i == (count-1));
			}
			break;
		}
		case STMT_EXPR: {
			fprintf(stream, "Expr\n");
			dump_expr(tu, stream, sp->expr.expr, depth + 1, true);
			break;
		}
		case STMT_RETURN: {
			fprintf(stream, "Return\n");
			if (sp->return_.expr) {
				dump_expr(tu, stream, sp->return_.expr, depth + 1, true);
			}
			break;
		}
		case STMT_IF: {
			fprintf(stream, "If\n");
			dump_expr(tu, stream, sp->if_.cond, depth + 1, sp->if_.body == 0);
			
			if (sp->if_.body) {
				dump_stmt(tu, stream, sp->if_.body, depth + 1, sp->if_.next == 0);
				
				if (sp->if_.next) {
					for (int i = 0; i < depth; i++) printf("  ");
					fprintf(stream, "Else\n");
					dump_stmt(tu, stream, sp->if_.next, depth + 1, true);
				}
			}
			break;
		}
		case STMT_DO_WHILE: { 
			fprintf(stream, "DoWhile\n");
			
			if (sp->do_while.body) {
				dump_expr(tu, stream, sp->do_while.cond, depth + 1, false);
				dump_stmt(tu, stream, sp->do_while.body, depth + 1, true);
			} else {
				dump_expr(tu, stream, sp->do_while.cond, depth + 1, true);
			}
			break;
		}
		case STMT_WHILE: { 
			fprintf(stream, "While\n");
			
			if (sp->while_.body) {
				dump_expr(tu, stream, sp->while_.cond, depth + 1, false);
				dump_stmt(tu, stream, sp->while_.body, depth + 1, true);
			} else {
				dump_expr(tu, stream, sp->while_.cond, depth + 1, true);
			}
			break;
		}
		case STMT_FOR: {
			fprintf(stream, "For\n");
			if (sp->for_.first) {
				print_barz(depth+1, false);
				fprintf(stream, "Init:\n");
				
				if (tu->stmts[sp->for_.first].op == STMT_COMPOUND) {
					Stmt* restrict sp_first = &tu->stmts[sp->for_.first];
					
					StmtIndex* kids = sp_first->compound.kids;
					size_t count = sp_first->compound.kids_count;
					
					for (size_t i = 0; i < count; i++) {
						dump_stmt(tu, stream, kids[i], depth + 2, i == (count-1));
					}
				} else {
					dump_stmt(tu, stream, sp->for_.first, depth + 2, true);
				}
			}
			
			if (sp->for_.cond) {
				print_barz(depth+1, false);
				fprintf(stream, "Cond:\n");
				dump_expr(tu, stream, sp->for_.cond, depth + 2, true);
			}
			
			print_barz(depth+1, sp->for_.next == 0);
			fprintf(stream, "Body:\n");
			dump_stmt(tu, stream, sp->for_.body, depth + 2, true);
			
			if (sp->for_.next) {
				print_barz(depth+1, true);
				fprintf(stream, "Next:\n");
				dump_expr(tu, stream, sp->for_.next, depth + 2, true);
			}
			break;
		}
		default: abort();
	}
	
	barz[depth-1] = false;
}

void ast_dump(TranslationUnit* tu, FILE* stream) {
	fprintf(stream, "TranslationUnit\n");
	barz[0] = true;
	
	if (settings.emit_ast == EMIT_AST_MINIMAL) {
		for (size_t i = 0, count = arrlen(tu->top_level_stmts); i < count; i++) {
			StmtIndex stmt = tu->top_level_stmts[i];
			if (!tu->stmts[stmt].decl.attrs.is_used || tu->stmts[stmt].decl.attrs.is_typedef) continue;
			
			bool is_last = (i == (count-1));
			if (!is_last) {
				size_t j = i+1;
				for (; j < count; j++) {
					if (!tu->stmts[stmt].decl.attrs.is_used || tu->stmts[stmt].decl.attrs.is_typedef) break;
				}
				
				is_last = (j == (count-1));
			}
			
			dump_stmt(tu, stream, stmt, 1, is_last);
		}
	} else {
		for (size_t i = 0, count = arrlen(tu->top_level_stmts); i < count; i++) {
			dump_stmt(tu, stream, tu->top_level_stmts[i], 1, i == (count-1));
		}
	}
}

void ast_dump_stats(TranslationUnit* tu, FILE* stream) {
	printf("\n~~~ AST STATS ~~~\n");
	printf("# Top level stmts: %zu\n", arrlen(tu->top_level_stmts));
	
	printf("# Expressions: %8zu (%zu kB)\n",
		   big_array_length(tu->exprs),
		   (big_array_length(tu->exprs) * sizeof(Expr)) / 1024);
	
	printf("# Statements:  %8zu (%zu kB)\n",
		   big_array_length(tu->stmts),
		   (big_array_length(tu->stmts) * sizeof(Stmt)) / 1024);
	
	printf("# Types:       %8zu (%zu kB)\n",
		   big_array_length(tu->types),
		   (big_array_length(tu->types) * sizeof(Type)) / 1024);
}
