// we use quite a few macros in the different optimization passes
static void mark_changes(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Reg r) {
    if (ctx->changed_ir_count > 0 && ctx->changed_ir[ctx->changed_ir_count - 1] == r) {
        return;
    }

    if (ctx->changed_ir_count + 1 >= ctx->changed_ir_cap) {
        ctx->changed_ir_cap *= 2;

        if (ctx->is_changed_ir_on_heap) {
            ctx->changed_ir = tb_platform_heap_realloc(ctx->changed_ir, ctx->changed_ir_cap * sizeof(TB_Reg));
        }
        ctx->is_changed_ir_on_heap = true;
    }
    ctx->changed_ir[ctx->changed_ir_count++] = r;
}

static TB_Reg set_reg(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Node* n, TB_Reg old_reg, TB_Reg new_reg) {
    TB_Reg r = n - f->nodes;
    __debugbreak();

    for (Use* u = ctx->users[old_reg]; u; u = u->next) {
        if (u->r == r) {
        }
    }

    return new_reg;
}

static void swap_regs(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Node* n) {
    tb_swap(TB_Reg, n->i_arith.a, n->i_arith.b);
}

static void remove_refs(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Node* n) {
    TB_Reg r = n - f->nodes;
    TB_FOR_INPUT_IN_NODE(it, f, n) {
        Use* p = NULL;
        for (Use* u = ctx->users[it.r]; u; p = u, u = u->next) {
            if (u->r == r) {
                // remove from list
                if (p) p->next = u->next;
                else ctx->users[it.r] = u->next;

                mark_changes(f, ctx, it.r);
                break;
            }
        }
    }
}

static void only_one_user(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Node* n, TB_Reg x) {
    // allow allocating new users
    assert(ctx->users[n - f->nodes] != NULL);
    ctx->users[n - f->nodes]->r = x;
    ctx->users[n - f->nodes]->next = NULL;
}

static void transmute_to_pass(TB_Function* f, TB_OptimizerCtx* restrict ctx, TB_Node* n) {
    tb_swap(TB_Reg, n->i_arith.a, n->i_arith.b);
}

#define MARK(r) mark_changes(f, ctx, r)
#define SWAP_BINOP_REGS(n) (mark_changes(f, ctx, n - f->nodes), swap_regs(f, ctx, n))
#define SET_REG(n, x, y) (n->x = set_reg(f, ctx, n, &n->x, y))

// converts node into integer constant.
// PRESERVES OLD DATA TYPE
#define TRANSMUTE_TO_INT(n, imm) ( \
    remove_refs(f, ctx, n),        \
    n->type = TB_INTEGER_CONST,    \
    n->integer.num_words = 1,      \
    n->integer.single_word = (imm) \
)

#define TRANSMUTE_TO_FLOAT32(n, imm) ( \
    remove_refs(f, ctx, n), n->type = TB_FLOAT32_CONST, n->dt = TB_TYPE_F32, n->flt32.value = (imm) \
)

#define TRANSMUTE_TO_FLOAT64(n, imm) ( \
    remove_refs(f, ctx, n), n->type = TB_FLOAT64_CONST, n->dt = TB_TYPE_F64, n->flt64.value = (imm) \
)

#define TRANSMUTE_TO_PASS(n, x) ( \
    only_one_user(f, ctx, n, x), n->type = TB_PASS, n->pass.value = (x) \
)

#define TRANSMUTE_TO_POISON(n) ( \
    remove_refs(f, ctx, n), n->type = TB_POISON \
)
