#include "../tb_internal.h"

// this is a conservative algorithm, if we don't handle a node in here
// it'll just fail to compare
static bool is_node_the_same(TB_Node* a, TB_Node* b) {
    if (a->type != b->type) return false;
    if (!TB_DATA_TYPE_EQUALS(a->dt, b->dt)) return false;

    size_t bytes = 0;
    switch (a->type) {
        case TB_INTEGER_CONST:
        if (a->integer.num_words == 1 &&
            b->integer.num_words == 1 &&
            a->integer.single_word == b->integer.single_word) {
            return true;
        }

        // TODO: handle large integer comparisons
        return false;

        case TB_KEEPALIVE:
        case TB_VA_START:
        case TB_NOT:
        case TB_NEG:
        case TB_X86INTRIN_SQRT:
        case TB_X86INTRIN_RSQRT:
        case TB_INT2PTR:
        case TB_PTR2INT:
        case TB_UINT2FLOAT:
        case TB_FLOAT2UINT:
        case TB_INT2FLOAT:
        case TB_FLOAT2INT:
        case TB_TRUNCATE:
        case TB_X86INTRIN_LDMXCSR:
        case TB_BITCAST:
        case TB_ZERO_EXT:
        case TB_SIGN_EXT:
        bytes = sizeof(struct TB_NodeUnary);
        break;

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        bytes = sizeof(struct TB_NodeAtomicRMW);
        break;

        case TB_MEMBER_ACCESS:
        bytes = sizeof(struct TB_NodeMemberAccess);
        break;

        case TB_ARRAY_ACCESS:
        bytes = sizeof(struct TB_NodeArrayAccess);
        break;

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD:
        case TB_SAR:
        case TB_SHL:
        case TB_SHR:
        bytes = sizeof(struct TB_NodeIArith);
        break;

        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        bytes = sizeof(struct TB_NodeFArith);
        break;

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        bytes = sizeof(struct TB_NodeCompare);
        break;

        default: return false;
    }
    assert(bytes != 0 && "this shouldn't be possible");

    void* a_start = &a->integer;
    void* b_start = &b->integer;
    return memcmp(a_start, b_start, bytes) == 0;
}

typedef struct {
    size_t count;
    TB_Reg* regs;
} BasicBlockDefs;

static BasicBlockDefs* generate_def_table(TB_Function* f, TB_TemporaryStorage* tls) {
    BasicBlockDefs* table = tb_tls_push(tls, f->bb_count * sizeof(BasicBlockDefs));

    TB_FOR_BASIC_BLOCK(bb, f) {
        table[bb].count = 0;
        table[bb].regs = tb_tls_push(tls, 0);

        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (!TB_IS_NODE_SIDE_EFFECT(n->type) && n->type != TB_LOAD) {
                tb_tls_push(tls, sizeof(TB_Reg));

                size_t i = table[bb].count++;
                table[bb].regs[i] = r;
            }
        }
    }

    return table;
}

static TB_Reg find_similar_def_in_bb(TB_Function* f, BasicBlockDefs* bb_defs, TB_Reg r) {
    TB_Node* restrict n = &f->nodes[r];

    FOREACH_N(i, 0, bb_defs->count) {
        TB_Reg other = bb_defs->regs[i];
        if (is_node_the_same(n, &f->nodes[other])) {
            return other;
        }
    }

    return 0;
}

static TB_Reg walk_dominators_for_similar_def(TB_Function* f, BasicBlockDefs* defs, TB_Label* doms, TB_Label curr, TB_Reg r) {
    TB_Label parent = doms[curr];

    if (parent != 0 && curr != parent) {
        TB_Reg result = walk_dominators_for_similar_def(f, defs, doms, parent, r);
        if (result != TB_NULL_REG) {
            return result;
        }
    }

    return find_similar_def_in_bb(f, &defs[curr], r);
}

typedef struct {
    void* start;
    BasicBlockDefs* defs;

    size_t resolved_reg_count;
    TB_Reg* resolved_regs;
    TB_Reg* doms;
    TB_Reg bb;
} CSE_Context;

static void cse_create(CSE_Context* ctx, TB_TemporaryStorage* tls, TB_Function* f) {
    memset(ctx, 0, sizeof(CSE_Context));

    TB_Predeccesors preds = tb_get_temp_predeccesors(f, tls);
    ctx->doms = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    tb_get_dominators(f, preds, ctx->doms);

    // list of defined nodes in for every basic block relevant to global CSE
    ctx->start = tb_tls_push(tls, 0);
    ctx->defs = generate_def_table(f, tls);

    // list of resolved nodes in this basic block, used for local CSE
    ctx->resolved_reg_count = 0;
    ctx->resolved_regs = tb_tls_push(tls, 0);
}

static void cse_destroy(CSE_Context* ctx, TB_TemporaryStorage* tls) {
    tb_tls_restore(tls, ctx->start);
}

static void cse_set_bb(CSE_Context* ctx, TB_TemporaryStorage* tls, TB_Label id) {
    ctx->bb = id;

    // reset list
    tb_tls_restore(tls, ctx->resolved_regs);
    ctx->resolved_reg_count = 0;
}

static TB_Reg cse_attempt(TB_Function* f, CSE_Context* ctx, TB_TemporaryStorage* tls, TB_Node* n) {
    while (n->type == TB_PASS) {
        n = &f->nodes[n->pass.value];
    }

    if (!TB_IS_NODE_SIDE_EFFECT(n->type) && n->type != TB_LOAD) {
        TB_Reg r = n - f->nodes;

        // try the Global CSE:
        // check dominators for value, we dont need the same checks of resolution
        // as local CSE since we can guarentee the entire BB is resolved at this point
        TB_Reg found = walk_dominators_for_similar_def(f, ctx->defs, ctx->doms, ctx->doms[ctx->bb], r);
        if (found != TB_NULL_REG && r != found) {
            OPTIMIZER_LOG(r, "Removed BB-global duplicate expression");
            tb_function_find_replace_reg(f, r, found);
            tb_murder_reg(f, r);
            return true;
        }

        // try local CSE:
        FOREACH_N(i, 0, ctx->resolved_reg_count) {
            TB_Reg other = ctx->resolved_regs[i];

            if (r != other && is_node_the_same(n, &f->nodes[other])) {
                OPTIMIZER_LOG(r, "Removed BB-local duplicate expression");
                tb_function_find_replace_reg(f, r, other);
                tb_murder_reg(f, r);
                return true;
            }
        }

        tb_tls_push(tls, sizeof(TB_Reg));
        ctx->resolved_regs[ctx->resolved_reg_count++] = r;
    }

    return false;
}

static bool cse(TB_Function* f) {
    TB_TemporaryStorage* tls = tb_tls_allocate();

    CSE_Context cse;
    cse_create(&cse, tls, f);

    int changes = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        cse_set_bb(&cse, tls, bb);

        TB_FOR_NODE(r, f, bb) {
            if (cse_attempt(f, &cse, tls, &f->nodes[r])) {
                changes++;
            }
        }
    }

    // don't free CSE, doesn't matter
    return changes;
}
