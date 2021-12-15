#include "common.h"
#include "parser.h"

void print_expr(ExprIndex e) {
	const Expr* restrict ep = &expr_arena.data[e];
	
	switch (ep->op) {
		case EXPR_NUM: {
			printf("%lld", ep->num);
			break;
		}
		case EXPR_STR: {
			const char* start = (const char*)(ep->str.start + 1);
			const char* end = (const char*)(ep->str.end - 1);
			
			printf("\"%.*s\"", (int)(end-start), start); 
			break;
		}
		case EXPR_SYMBOL: {
			const Stmt* restrict sp = &stmt_arena.data[ep->symbol];
			
			printf("%s", sp->decl_name);
			break;
		}
		case EXPR_PARAM: {
			printf("__arg%d", ep->param_num);
			break;
		}
		case EXPR_CAST: {
			printf("__cast(");
			print_expr(ep->cast.src);
			printf(")");
			break;
		}
		case EXPR_ADDR: {
			printf("&");
			print_expr(ep->unary_op.src);
			break;
		}
		case EXPR_DEREF: {
			printf("*");
			print_expr(ep->unary_op.src);
			break;
		}
		case EXPR_SUBSCRIPT: {
			print_expr(ep->subscript.base);
			printf("[");
			print_expr(ep->subscript.index);
			printf("]");
			break;
		}
		case EXPR_DOT: {
			print_expr(ep->dot.base);
			printf(".%s", ep->dot.name);
			break;
		}
		case EXPR_PLUS:
		case EXPR_MINUS:
		case EXPR_TIMES:
		case EXPR_SLASH:
		case EXPR_AND:
		case EXPR_OR:
		case EXPR_XOR:
		case EXPR_SHL:
		case EXPR_SHR:
		case EXPR_CMPEQ:
		case EXPR_CMPNE:
		case EXPR_CMPLE:
		case EXPR_CMPLT:
		case EXPR_CMPGE:
		case EXPR_CMPGT:
		case EXPR_ASSIGN:
		case EXPR_PLUS_ASSIGN:
		case EXPR_MINUS_ASSIGN:
		case EXPR_TIMES_ASSIGN:
		case EXPR_SLASH_ASSIGN:
		case EXPR_AND_ASSIGN:
		case EXPR_OR_ASSIGN:
		case EXPR_XOR_ASSIGN:
		case EXPR_SHL_ASSIGN:
		case EXPR_SHR_ASSIGN:
		case EXPR_LOGICAL_AND:
		case EXPR_LOGICAL_OR: {
			printf("(");
			print_expr(ep->bin_op.left); 
			
			static const char* names[] = {
				[EXPR_PLUS] = "+",
				[EXPR_MINUS] = "-",
				[EXPR_TIMES] = "*",
				[EXPR_SLASH] = "/",
				[EXPR_AND] = "&",
				[EXPR_OR] = "|",
				[EXPR_XOR] = "^",
				[EXPR_SHL] = "<<",
				[EXPR_SHR] = ">>",
				[EXPR_CMPEQ] = "==",
				[EXPR_CMPNE] = "!=",
				[EXPR_CMPLE] = "<=",
				[EXPR_CMPLT] = "<",
				[EXPR_CMPGE] = ">=",
				[EXPR_CMPGT] = ">",
				[EXPR_ASSIGN] = "=",
				[EXPR_PLUS_ASSIGN] = "+=",
				[EXPR_MINUS_ASSIGN] = "-=",
				[EXPR_TIMES_ASSIGN] = "*=",
				[EXPR_SLASH_ASSIGN] = "/=",
				[EXPR_AND_ASSIGN] = "&=",
				[EXPR_OR_ASSIGN] = "|=",
				[EXPR_XOR_ASSIGN] = "^=",
				[EXPR_SHL_ASSIGN] = "<<=",
				[EXPR_SHR_ASSIGN] = ">>=",
				[EXPR_LOGICAL_AND] = "&&",
				[EXPR_LOGICAL_OR] = "||",
			};
			printf(" %s ", names[ep->op]);
			
			print_expr(ep->bin_op.right); 
			printf(")");
			break;
		}
		case EXPR_CALL: {
			print_expr(ep->call.target);
			
			ExprIndexIndex param_start = ep->call.param_start;
			ExprIndexIndex param_end = ep->call.param_end;
			
			printf("(");
			for (size_t i = param_start; i < param_end; i++) {
				if (i != param_start) printf(", ");
				print_expr(expr_ref_arena.data[i]);
			}
			printf(")");
			break;
		}
		default: abort();
	}
}

void print_stmt(StmtIndex s, int depth) {
	const Stmt* restrict sp = &stmt_arena.data[s];
	for (int i = 0; i < depth; i++) printf("  ");
	
	switch (sp->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_COMPOUND: {
			printf("{\n");
			
			for (StmtIndexIndex i = sp->kids_start; i != sp->kids_end; i++) {
				print_stmt(stmt_ref_arena.data[i], depth+1);
			}
			
			for (int i = 0; i < depth; i++) printf("  ");
			printf("}\n");
			break;
		}
		case STMT_EXPR: {
			printf("EXPR ");
			print_expr(sp->expr);
			printf("\n");
			break;
		}
		case STMT_DECL: {
			if (sp->expr) {
				printf("DECL %s: ", sp->decl_name);
				print_expr(sp->expr);
				printf("\n");
			} else {
				printf("DECL %s;\n", sp->decl_name);
			}
			break;
		}
		case STMT_FUNC_DECL: {
			printf("FUNC DECL %s:\n", sp->decl_name);
			print_stmt(s + 1, depth + 1);
			break;
		}
		case STMT_IF: {
			printf("IF ");
			print_expr(sp->expr);
			printf("\n");
			
			print_stmt(sp->body, depth + 1);
			
			if (sp->body2) {
				for (int i = 0; i < depth; i++) printf("  ");
				printf("ELSE\n");
				
				print_stmt(sp->body2, depth + 1);
			}
			break;
		}
		case STMT_WHILE: {
			printf("WHILE ");
			print_expr(sp->expr);
			printf("\n");
			
			print_stmt(sp->body, depth + 1);
			break;
		}
		case STMT_DO_WHILE: {
			printf("DO\n");
			
			print_stmt(sp->body, depth + 1);
			
			for (int i = 0; i < depth; i++) printf("  ");
			printf("WHILE");
			print_expr(sp->expr);
			printf("\n");
			break;
		}
		case STMT_RETURN: {
			if (sp->expr) {
				printf("RETURN: ");
				print_expr(sp->expr);
				printf(";\n");
			} else {
				printf("RETURN;\n");
			}
			break;
		}
		default: abort();
	}
}

void print_tree(TopLevel tl) {
	size_t len = arrlen(tl.arr);
	
	printf("FILE:\n");
	for (size_t i = 0; i < len; i++) {
		print_stmt(tl.arr[i], 1);
	}
}
