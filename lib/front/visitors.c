#include <front/sema.h>

// used by cuik_visit_top_level_threaded
typedef struct {
    TranslationUnit* tu;
    atomic_size_t* tasks_remaining;
    Cuik_StmtVisitor* visitor;
    void* user_data;
    size_t start, end;
} TaskInfo;

CUIK_API void cuik_visit_stmt(TranslationUnit* restrict tu, Stmt* restrict s, void* user_data, Cuik_StmtVisitor* visitor) {
    switch (s->op) {
        case STMT_NONE:
        case STMT_LABEL:
        case STMT_GOTO:
        case STMT_EXPR:
        case STMT_RETURN:
        case STMT_CONTINUE:
        case STMT_BREAK:
        case STMT_DECL:
        break;

        case STMT_COMPOUND: {
            Stmt** kids = s->compound.kids;
            size_t count = s->compound.kids_count;

            for (size_t i = 0; i < count; i++) {
                visitor(tu, kids[i], user_data);
            }
            break;
        }

        case STMT_IF:
        visitor(tu, s->if_.body, user_data);
        if (s->if_.next) {
            visitor(tu, s->if_.next, user_data);
        }
        break;

        case STMT_WHILE:
        if (s->while_.body) {
            visitor(tu, s->while_.body, user_data);
        }
        break;

        case STMT_DO_WHILE:
        if (s->do_while.body) {
            visitor(tu, s->do_while.body, user_data);
        }
        break;

        case STMT_FOR:
        if (s->for_.first) {
            visitor(tu, s->for_.first, user_data);
        }

        if (s->for_.body) {
            visitor(tu, s->for_.body, user_data);
        }
        break;

        case STMT_SWITCH:
        visitor(tu, s->switch_.body, user_data);
        break;

        case STMT_CASE:
        visitor(tu, s->case_.body, user_data);
        break;

        case STMT_DEFAULT:
        visitor(tu, s->default_.body, user_data);
        break;

        default: assert(0);
    }
}

CUIK_API void cuik_visit_expr(TranslationUnit* restrict tu, Expr* restrict e, void* user_data, Cuik_ExprVisitor* visitor) {
    switch (e->op) {
        case EXPR_UNKNOWN_SYMBOL:
        case EXPR_VA_ARG:
        case EXPR_INT:
        case EXPR_ENUM:
        case EXPR_FLOAT32:
        case EXPR_FLOAT64:
        case EXPR_CHAR:
        case EXPR_WCHAR:
        case EXPR_STR:
        case EXPR_WSTR:
        case EXPR_SIZEOF:
        case EXPR_ALIGNOF:
        case EXPR_SIZEOF_T:
        case EXPR_ALIGNOF_T:
        case EXPR_FUNCTION:
        case EXPR_SYMBOL:
        case EXPR_PARAM:
        break;

        case EXPR_INITIALIZER:
        assert(0);
        break;

        case EXPR_NOT:
        case EXPR_ADDR:
        case EXPR_DEREF:
        case EXPR_NEGATE:
        case EXPR_PRE_INC:
        case EXPR_PRE_DEC:
        case EXPR_POST_INC:
        case EXPR_POST_DEC:
        case EXPR_LOGICAL_NOT:
        visitor(tu, e->unary_op.src, user_data);
        break;

        case EXPR_CAST:
        visitor(tu, e->cast.src, user_data);
        break;

        case EXPR_GENERIC:
        assert(e->generic_.case_count == 0);
        visitor(tu, e->generic_.controlling_expr, user_data);
        break;

        case EXPR_SUBSCRIPT:
        visitor(tu, e->subscript.base, user_data);
        visitor(tu, e->subscript.index, user_data);
        break;

        case EXPR_CALL: {
            visitor(tu, e->call.target, user_data);

            Expr** args = e->call.param_start;
            int arg_count = e->call.param_count;

            for (size_t i = 0; i < arg_count; i++) {
                visitor(tu, args[i], user_data);
            }
            break;
        }

        case EXPR_TERNARY:
        visitor(tu, e->ternary_op.left, user_data);
        visitor(tu, e->ternary_op.middle, user_data);
        visitor(tu, e->ternary_op.right, user_data);
        break;

        case EXPR_DOT:
        case EXPR_ARROW:
        visitor(tu, e->dot_arrow.base, user_data);
        break;

        case EXPR_COMMA:
        case EXPR_PTRADD:
        case EXPR_PTRSUB:
        case EXPR_PTRDIFF:
        case EXPR_DOT_R:
        case EXPR_ARROW_R:
        case EXPR_LOGICAL_AND:
        case EXPR_LOGICAL_OR:
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
        case EXPR_CMPEQ:
        case EXPR_CMPNE:
        case EXPR_CMPGT:
        case EXPR_CMPGE:
        case EXPR_CMPLT:
        case EXPR_CMPLE:
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
        visitor(tu, e->bin_op.left, user_data);
        visitor(tu, e->bin_op.right, user_data);
        break;

        default: assert(0);
    }
}

CUIK_API void cuik_visit_top_level(TranslationUnit* restrict tu, void* user_data, Cuik_StmtVisitor* visitor) {
    size_t count = arrlen(tu->top_level_stmts);
    CUIK_TIMED_BLOCK("top level visitor (%zu statements)", count) {
        for (size_t i = 0; i < count; i++) {
            visitor(tu, tu->top_level_stmts[i], user_data);
        }
    }
}

static void task_caller(void* arg) {
    TaskInfo task = *((TaskInfo*)arg);

    CUIK_TIMED_BLOCK("top level visitor (%zu - %zu)", task.start, task.end) {
        for (size_t i = task.start; i < task.end; i++) {
            task.visitor(task.tu, task.tu->top_level_stmts[i], task.user_data);
        }
    }

    *task.tasks_remaining -= 1;
}

CUIK_API void cuik_visit_top_level_threaded(TranslationUnit* restrict tu, const Cuik_IThreadpool* restrict thread_pool, int batch_size, ThreadedWaiter* restrict waiter, void* user_data, Cuik_StmtVisitor* visitor) {
    static_assert(sizeof(atomic_size_t) == sizeof(size_t), "size_t isn't lock free?");

    assert(thread_pool != NULL);
    assert(waiter != NULL);
    assert((batch_size & (batch_size-1)) == 0);
    tls_init();

    // split up the top level statement tasks into
    // chunks to avoid spawning too many tiny tasks
    size_t count = arrlen(tu->top_level_stmts);
    size_t padded = (count + (batch_size - 1)) & ~(batch_size - 1);
    size_t da_crumbs = padded - batch_size;

    size_t task_count = (count + (batch_size - 1)) / batch_size;
    TaskInfo* tasks = malloc(sizeof(TaskInfo) * task_count);

    waiter->opaque = tasks;
    waiter->remaining = task_count;

    size_t j = 0;
    for (size_t i = 0; i < padded; i += batch_size) {
        size_t limit = i + batch_size;
        if (limit > da_crumbs) limit = count;

        tasks[j] = (TaskInfo){tu, (atomic_size_t*) &waiter->remaining, visitor, user_data, i, limit};
        CUIK_CALL(thread_pool, submit, task_caller, &tasks[j]);
        // printf("Group: %zu - %zu\n", i, limit);

        j += 1;
    }
}

CUIK_API void cuik_wait_on_waiter(const Cuik_IThreadpool* restrict thread_pool, ThreadedWaiter* restrict waiter) {
    static_assert(sizeof(atomic_size_t) == sizeof(size_t), "size_t isn't lock free?");

    // "highway robbery on steve jobs" job stealing amirite...
    while (atomic_load((atomic_size_t*) &waiter->remaining) != 0) {
        CUIK_CALL(thread_pool, work_one_job);
    }

    free(waiter->opaque);
}
