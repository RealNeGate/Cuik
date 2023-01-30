#include "tb_internal.h"
#include "coroutine.h"

TB_API bool tb_next_bb(TB_Function* f, TB_BBIter* it) {
    CO_SCOPE(it) {
        CO_START();
        for (; it->l < f->bb_count; it->l++) {
            CO_YIELD(it, true);
        }
    }

    CO_DONE();
}

TB_API TB_NodeInputIter tb_node_input_iter(TB_Reg r) {
    return (TB_NodeInputIter){ .parent_ = r };
}

TB_API bool tb_next_node_input(const TB_Function* f, TB_NodeInputIter* iter) {
    TB_Node* restrict n = &f->nodes[iter->parent_];

    switch (n->type) {
        case TB_NULL:
        case TB_INTEGER_CONST:
        case TB_FLOAT32_CONST:
        case TB_FLOAT64_CONST:
        case TB_STRING_CONST:
        case TB_LOCAL:
        case TB_PARAM:
        case TB_GOTO:
        case TB_LINE_INFO:
        case TB_GET_SYMBOL_ADDRESS:
        case TB_X86INTRIN_RDTSC:
        case TB_X86INTRIN_STMXCSR:
        case TB_UNREACHABLE:
        case TB_DEBUGBREAK:
        case TB_POISON:
        case TB_TRAP:
        return false;

        case TB_INITIALIZE:
        switch (iter->index_++) {
            case 0: return (iter->r = n->init.addr, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_KEEPALIVE:
        case TB_VA_START:
        case TB_BSWAP:
        case TB_CLZ:
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
        case TB_FLOAT_EXT:
        switch (iter->index_++) {
            case 0: return (iter->r = n->unary.src, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_ATOMIC_LOAD:
        case TB_ATOMIC_XCHG:
        case TB_ATOMIC_ADD:
        case TB_ATOMIC_SUB:
        case TB_ATOMIC_AND:
        case TB_ATOMIC_XOR:
        case TB_ATOMIC_OR:
        case TB_ATOMIC_CMPXCHG:
        switch (iter->index_++) {
            case 0: return (iter->r = n->atomic.addr, true);
            case 1: return (iter->r = n->atomic.src, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_ATOMIC_CMPXCHG2:
        switch (iter->index_++) {
            case 0: return (iter->r = n->atomic.src, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_MEMCPY:
        case TB_MEMSET:
        switch (iter->index_++) {
            case 0: return (iter->r = n->mem_op.dst, true);
            case 1: return (iter->r = n->mem_op.src, true);
            case 2: return (iter->r = n->mem_op.size, true);
            case 3: return false;
            default: tb_unreachable();
        }
        break;

        case TB_MEMBER_ACCESS:
        switch (iter->index_++) {
            case 0: return (iter->r = n->member_access.base, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_ARRAY_ACCESS:
        switch (iter->index_++) {
            case 0: return (iter->r = n->array_access.base, true);
            case 1: return (iter->r = n->array_access.index, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_PARAM_ADDR:
        switch (iter->index_++) {
            case 0: return (iter->r = n->param_addr.param, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_PASS:
        switch (iter->index_++) {
            case 0: return (iter->r = n->pass.value, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_PHI1:
        switch (iter->index_++) {
            case 0: return (iter->r = n->phi1.inputs[0].val, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_PHI2:
        switch (iter->index_++) {
            case 0: return (iter->r = n->phi1.inputs[0].val, true);
            case 1: return (iter->r = n->phi1.inputs[1].val, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_PHIN: {
            size_t i = iter->index_++;
            if (i >= n->phi.count) return false;

            iter->r = n->phi.inputs[i].val;
            return true;
        }

        case TB_LOAD:
        switch (iter->index_++) {
            case 0: return (iter->r = n->load.address, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_STORE:
        switch (iter->index_++) {
            case 0: return (iter->r = n->store.address, true);
            case 1: return (iter->r = n->store.value, true);
            case 2: return false;
            default: tb_unreachable();
        }
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
        switch (iter->index_++) {
            case 0: return (iter->r = n->i_arith.a, true);
            case 1: return (iter->r = n->i_arith.b, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_FADD:
        case TB_FSUB:
        case TB_FMUL:
        case TB_FDIV:
        switch (iter->index_++) {
            case 0: return (iter->r = n->f_arith.a, true);
            case 1: return (iter->r = n->f_arith.b, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        switch (iter->index_++) {
            case 0: return (iter->r = n->cmp.a, true);
            case 1: return (iter->r = n->cmp.b, true);
            case 2: return false;
            default: tb_unreachable();
        }
        break;

        case TB_SCALL: {
            size_t i = iter->index_++;
            size_t count = n->scall.param_end - n->scall.param_start;

            if (i == 0) {
                return (iter->r = n->scall.target, true);
            } else if (i - 1 < count) {
                return (iter->r = f->vla.data[n->scall.param_start + (i - 1)], true);
            } else {
                return false;
            }
        }

        case TB_VCALL: {
            size_t i = iter->index_++;
            size_t count = n->call.param_end - n->call.param_start;

            if (i == 0) {
                return (iter->r = n->vcall.target, true);
            } else if (i - 1 < count) {
                return (iter->r = f->vla.data[n->call.param_start + (i - 1)], true);
            } else {
                return false;
            }
        }

        case TB_CALL:
        case TB_ICALL: {
            size_t i = iter->index_++;
            size_t count = n->call.param_end - n->call.param_start;

            if (i >= count) return false;
            return (iter->r = f->vla.data[n->call.param_start + i], true);
        }

        case TB_SWITCH:
        switch (iter->index_++) {
            case 0: return (iter->r = n->switch_.key, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_IF:
        switch (iter->index_++) {
            case 0: return (iter->r = n->if_.cond, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;

        case TB_RET:
        switch (iter->index_++) {
            case 0: return (iter->r = n->ret.value, true);
            case 1: return false;
            default: tb_unreachable();
        }
        break;
    }

    tb_todo();
}
