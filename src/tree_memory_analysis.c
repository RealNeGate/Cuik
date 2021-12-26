#include "common.h"
#include "arena.h"
#include "parser.h"

static bool last_mode = false;

void DEBUG_access_expr(ExprIndex e) {
	if (last_mode != true) {
		printf("\n");
		last_mode = true;
	}
	printf("\t\t\tExpr: %d (%zu)\n", e, sizeof(Expr));
}

void DEBUG_access_stmt(StmtIndex s) {
	Stmt* restrict sp = &stmt_arena.data[s];
	
	if (last_mode != false) {
		printf("\n");
		last_mode = false;
	}
	printf("Stmt: %d (%zu)\n", s, sizeof(Stmt));
	
	switch (sp->op) {
		case STMT_NONE: {
			break;
		}
		case STMT_LABEL: {
			break;
		}
		case STMT_GOTO: {
			DEBUG_access_expr(sp->goto_.target);
			break;
		}
		case STMT_COMPOUND: {
			StmtIndexIndex start = sp->compound.kids_start;
			StmtIndexIndex end = sp->compound.kids_end;
			
			for (StmtIndexIndex i = start; i != end; i++) {
				StmtIndex s = stmt_ref_arena.data[i];
				
				DEBUG_access_stmt(s);
			}
			break;
		}
		case STMT_FUNC_DECL: {
			DEBUG_access_stmt(s + 1);
			break;
		}
		case STMT_DECL: {
			if (sp->decl.initial) {
				DEBUG_access_expr(sp->decl.initial);
			}
			break;
		}
		case STMT_EXPR: {
			DEBUG_access_expr(sp->expr.expr);
			break;
		}
		case STMT_RETURN: {
			DEBUG_access_expr(sp->return_.expr);
			break;
		}
		case STMT_IF: {
			DEBUG_access_expr(sp->if_.cond);
			DEBUG_access_stmt(sp->if_.body);
			
			if (sp->if_.next) {
				DEBUG_access_stmt(sp->if_.next);
			}
			break;
		}
		case STMT_WHILE: {
			DEBUG_access_expr(sp->while_.cond);
			DEBUG_access_stmt(sp->while_.body);
			break;
		}
		case STMT_DO_WHILE: {
			DEBUG_access_stmt(sp->while_.body);
			DEBUG_access_expr(sp->while_.cond);
			break;
		}
		case STMT_FOR: {
			DEBUG_access_stmt(sp->for_.first);
			DEBUG_access_expr(sp->for_.cond);
			DEBUG_access_stmt(sp->for_.body);
			DEBUG_access_expr(sp->for_.next);
			break;
		}
		default:
		__builtin_unreachable();
	}
}

void DEBUG_print_tree_memory_patterns(TopLevel tl) {
	size_t len = arrlen(tl.arr);
	
	for (size_t i = 0; i < len; i++) {
		StmtIndex s = tl.arr[i];
		
		if (stmt_arena.data[s].op == STMT_FUNC_DECL) {
			DEBUG_access_stmt(s);
		}
	}
}

