#include "common.h"
#include "parser.h"

static const int precedence[EXPR_MAX] = {
	[EXPR_TIMES] = 3, [EXPR_SLASH] = 3,
	[EXPR_PLUS] = 4, [EXPR_MINUS] = 4,
	[EXPR_SHL] = 5, [EXPR_SHR] = 5,
	[EXPR_CMPLE] = 6, [EXPR_CMPLT] = 6, [EXPR_CMPGE] = 6, [EXPR_CMPGT] = 6,
	[EXPR_CMPEQ] = 7, [EXPR_CMPNE] = 7,
	[EXPR_AND] = 8,
	[EXPR_XOR] = 9,
	[EXPR_OR] = 10,
	[EXPR_LOGICAL_AND] = 11,
	[EXPR_LOGICAL_OR] = 12,
	[EXPR_ASSIGN] = 14, [EXPR_PLUS_ASSIGN] = 14,
	[EXPR_MINUS_ASSIGN] = 14, [EXPR_TIMES_ASSIGN] = 14,
	[EXPR_SLASH_ASSIGN] = 14, [EXPR_AND_ASSIGN] = 14,
	[EXPR_OR_ASSIGN] = 14, [EXPR_XOR_ASSIGN] = 14,
	[EXPR_SHL_ASSIGN] = 14, [EXPR_SHR_ASSIGN] = 14
};

bool print_type(TypeIndex t) {
	Type* restrict type = &type_arena.data[t];
	
	switch (type->kind) {
		case KIND_VOID: printf("void"); return false;
		case KIND_BOOL: printf("_Bool"); return false;
		case KIND_CHAR: printf("char"); return false;
		case KIND_SHORT: printf("short"); return false;
		case KIND_INT: printf("int"); return false;
		case KIND_LONG: printf("long"); return false;
		case KIND_FLOAT: printf("float"); return false;
		case KIND_DOUBLE: printf("double"); return false;
		case KIND_ENUM: {
			if (type->enumerator.name) printf("enum %s", type->enumerator.name);
			else printf("enum __unnamed_%d", t);
			return false;
		}
		case KIND_UNION: {
			if (type->record.name) printf("union %s", type->record.name);
			else printf("union __unnamed_%d", t);
			return false;
		}
		case KIND_STRUCT: {
			if (type->record.name) printf("struct %s", type->record.name);
			else printf("struct __unnamed_%d", t);
			return false;
		}
		case KIND_PTR: {
			print_type(type->ptr_to);
			printf("*");
			return false;
		}
		case KIND_ARRAY: {
			print_type(type->array_of);
			printf("[%d]", type->size / type->align);
			return false;
		}
		case KIND_FUNC: {
			ArgIndex arg_start = type->func.arg_start;
			ArgIndex arg_end = type->func.arg_end;
			
			print_type(type->func.return_type);
			if (type->func.name) printf(" %s", type->func.name);
			
			printf("(");
			for (ArgIndex i = arg_start; i < arg_end; i++) {
				if (i != arg_start) printf(", ");
				
				print_type(arg_arena.data[i].type);
				if (arg_arena.data[i].name) {
					printf(" %s", arg_arena.data[i].name);
				}
			}
			printf(")");
			
			return (type->func.name);
		}
		default: abort();
	}
}

void print_expr(ExprIndex e, int last_precedence) {
	const Expr* restrict ep = &expr_arena.data[e];
	
	// You only need parenthesis if the precendence disagrees with
	// the real eval order
	int pred = precedence[ep->op];
	bool needs_paren = (pred > last_precedence);
	if (needs_paren) printf("(");
	
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
			
			printf("%s", sp->decl.name);
			break;
		}
		case EXPR_PARAM: {
			printf("__arg%d", ep->param_num);
			break;
		}
		case EXPR_CAST: {
			printf("(");
			print_type(ep->cast.type);
			printf(")");
			print_expr(ep->cast.src, pred);
			break;
		}
		case EXPR_ADDR: {
			printf("&");
			print_expr(ep->unary_op.src, pred);
			break;
		}
		case EXPR_DEREF: {
			printf("*");
			print_expr(ep->unary_op.src, pred);
			break;
		}
		case EXPR_SUBSCRIPT: {
			print_expr(ep->subscript.base, pred);
			printf("[");
			print_expr(ep->subscript.index, 100);
			printf("]");
			break;
		}
		case EXPR_DOT: {
			print_expr(ep->dot.base, pred);
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
			
			print_expr(ep->bin_op.left, pred); 
			printf(" %s ", names[ep->op]);
			print_expr(ep->bin_op.right, pred); 
			break;
		}
		case EXPR_CALL: {
			print_expr(ep->call.target, 100);
			
			ExprIndexIndex param_start = ep->call.param_start;
			ExprIndexIndex param_end = ep->call.param_end;
			
			printf("(");
			for (size_t i = param_start; i < param_end; i++) {
				if (i != param_start) printf(", ");
				print_expr(expr_ref_arena.data[i], 100);
			}
			printf(")");
			break;
		}
		default: abort();
	}
	
	if (needs_paren) printf(")");
}

void print_stmt(StmtIndex s, int depth, bool is_idented) {
	const Stmt* restrict sp = &stmt_arena.data[s];
	
	if (is_idented) {
		for (int i = 0; i < depth; i++) printf("  ");
	}
	
	switch (sp->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_COMPOUND: {
			printf("{\n");
			
			for (StmtIndexIndex i = sp->kids_start; i != sp->kids_end; i++) {
				print_stmt(stmt_ref_arena.data[i], depth+1, true);
			}
			
			for (int i = 0; i < depth; i++) printf("  ");
			printf("}\n");
			break;
		}
		case STMT_EXPR: {
			print_expr(sp->expr, 100);
			printf(";\n");
			break;
		}
		case STMT_DECL: {
			if (!print_type(sp->decl.type)) {
				printf(" %s", sp->decl.name);
			}
			
			if (sp->expr) {
				printf(" = ");
				print_expr(sp->expr, 100);
				printf(";\n");
			} else {
				printf(";\n");
			}
			break;
		}
		case STMT_FUNC_DECL: {
			printf("FUNC DECL %s: ", sp->decl.name);
			print_stmt(s + 1, depth, false);
			break;
		}
		case STMT_IF: {
			printf("if (");
			print_expr(sp->expr, 100);
			printf(") ");
			
			print_stmt(sp->body, depth, false);
			
			if (sp->body2) {
				for (int i = 0; i < depth; i++) printf("  ");
				printf("else ");
				
				print_stmt(sp->body2, depth, false);
			}
			break;
		}
		case STMT_WHILE: {
			printf("while (");
			print_expr(sp->expr, 100);
			printf(") ");
			
			print_stmt(sp->body, depth, false);
			break;
		}
		case STMT_DO_WHILE: {
			printf("do ");
			
			print_stmt(sp->body, depth, false);
			
			for (int i = 0; i < depth; i++) printf("  ");
			printf("while (");
			print_expr(sp->expr, 100);
			printf(")\n");
			break;
		}
		case STMT_RETURN: {
			if (sp->expr) {
				printf("return ");
				print_expr(sp->expr, 100);
				printf(";\n");
			} else {
				printf("return;\n");
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
		print_stmt(tl.arr[i], 1, true);
	}
}
