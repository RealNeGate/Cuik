#include "tb_internal.h"

// IR ANALYSIS
void tb_function_calculate_use_count(const TB_Function* f, int use_count[]) {
    for (size_t i = 0; i < f->node_count; i++) {
        use_count[i] = 0;
    }

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            TB_FOR_INPUT_IN_NODE(it, f, n) {
                use_count[it.r] += 1;
            }
        }
    }
}

int tb_function_find_uses_of_node(const TB_Function* f, TB_Reg def, TB_Reg uses[]) {
    size_t count = 0;

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            TB_FOR_INPUT_IN_NODE(it, f, n) {
                if (it.r == def) uses[count++] = r;
            }
        }
    }

    return count;
}

void tb_function_find_replace_reg(TB_Function* f, TB_Reg find, TB_Reg replace) {
    #define X(reg) if (reg == find) reg = replace;

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

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
                case TB_TRAP:
                case TB_POISON:
                break;

                case TB_INITIALIZE:
                X(n->init.addr);
                break;

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
                case TB_CLZ:
                X(n->unary.src);
                break;

                case TB_ATOMIC_LOAD:
                case TB_ATOMIC_XCHG:
                case TB_ATOMIC_ADD:
                case TB_ATOMIC_SUB:
                case TB_ATOMIC_AND:
                case TB_ATOMIC_XOR:
                case TB_ATOMIC_OR:
                case TB_ATOMIC_CMPXCHG:
                X(n->atomic.addr);
                X(n->atomic.src);
                break;

                case TB_ATOMIC_CMPXCHG2:
                X(n->atomic.src);
                break;

                case TB_MEMCPY:
                case TB_MEMSET:
                X(n->mem_op.dst);
                X(n->mem_op.src);
                X(n->mem_op.size);
                break;

                case TB_MEMBER_ACCESS:
                X(n->member_access.base);
                break;

                case TB_ARRAY_ACCESS:
                X(n->array_access.base);
                X(n->array_access.index);
                break;

                case TB_PARAM_ADDR:
                X(n->param_addr.param);
                break;

                case TB_PASS:
                X(n->pass.value);
                break;

                case TB_PHI1:
                X(n->phi1.inputs[0].val);
                break;

                case TB_PHI2:
                FOREACH_N(it, 0, 2) {
                    X(n->phi2.inputs[it].val);
                }
                break;

                case TB_PHIN:
                FOREACH_N(it, 0, n->phi.count) {
                    X(n->phi.inputs[it].val);
                }
                break;

                case TB_LOAD:
                X(n->load.address);
                break;

                case TB_STORE:
                X(n->store.address);
                X(n->store.value);
                break;

                case TB_ZERO_EXT:
                case TB_SIGN_EXT:
                case TB_FLOAT_EXT:
                X(n->unary.src);
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
                X(n->i_arith.a);
                X(n->i_arith.b);
                break;

                case TB_FADD:
                case TB_FSUB:
                case TB_FMUL:
                case TB_FDIV:
                X(n->f_arith.a);
                X(n->f_arith.b);
                break;

                case TB_CMP_EQ:
                case TB_CMP_NE:
                case TB_CMP_SLT:
                case TB_CMP_SLE:
                case TB_CMP_ULT:
                case TB_CMP_ULE:
                case TB_CMP_FLT:
                case TB_CMP_FLE:
                X(n->cmp.a);
                X(n->cmp.b);
                break;

                case TB_SCALL: {
                    X(n->scall.target);

                    FOREACH_N(it, n->scall.param_start, n->scall.param_end) {
                        X(f->vla.data[it]);
                    }
                    break;
                }

                case TB_VCALL: {
                    X(n->vcall.target);

                    FOREACH_N(it, n->vcall.param_start, n->vcall.param_end) {
                        X(f->vla.data[it]);
                    }
                    break;
                }

                case TB_CALL:
                case TB_ICALL: {
                    FOREACH_N(it, n->call.param_start, n->call.param_end) {
                        X(f->vla.data[it]);
                    }
                    break;
                }

                case TB_SWITCH: X(n->switch_.key); break;
                case TB_IF: X(n->if_.cond); break;
                case TB_RET: X(n->ret.value); break;

                default: tb_todo();
            }
        }

        // if it matches find, then remove find from the basic block
        if (f->bbs[bb].start == find) {
            f->bbs[bb].start = f->nodes[f->bbs[bb].start].next;
        }

        if (f->bbs[bb].end == find) {
            f->bbs[bb].end = tb_node_get_previous(f, f->bbs[bb].end);
        }
    }

    #undef X
}

TB_Label tb_find_label_from_reg(TB_Function* f, TB_Reg target) {
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (target == r) return bb;
        }
    }

    return TB_NULL_REG;
}

TB_Reg tb_node_get_previous(TB_Function* f, TB_Reg at) {
    TB_Reg prev = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (r == at) return prev;
            prev = r;
        }
    }

    return 0;
}

TB_Reg tb_function_insert_before(TB_Function* f, TB_Reg at) {
    assert(at != 0);

    TB_Reg prev = 0;
    TB_Label bb = 0;
    for (; bb < f->bb_count; bb++) {
        TB_FOR_NODE(o, f, bb) {
            if (at == o) goto done;
            prev = o;
        }

        prev = 0;
    }

    tb_panic("No register to insert before: r%d", at);

    done:;
    tb_function_reserve_nodes(f, 1);
    TB_Reg r = f->node_count++;

    if (prev == 0) {
        TB_Reg next = f->nodes[prev].next;

        f->nodes[r] = (TB_Node) { .type = TB_NULL, .dt = TB_TYPE_VOID, .next = next };
        f->bbs[bb].start = r;
    } else {
        TB_Reg next = f->nodes[prev].next;

        f->nodes[r] = (TB_Node) { .type = TB_NULL, .dt = TB_TYPE_VOID, .next = next };
        f->nodes[prev].next = r;
    }

    if (f->bbs[bb].end == at) {
        f->bbs[bb].end = r;
    }
    return r;
}

TB_Reg tb_function_insert_after(TB_Function* f, TB_Label bb, TB_Reg at) {
    tb_function_reserve_nodes(f, 1);
    assert(tb_find_label_from_reg(f, at) == bb);
    assert(at != 0);

    TB_Reg next = f->nodes[at].next;
    TB_Reg r = f->node_count++;

    f->nodes[r] = (TB_Node) { .type = TB_NULL, .dt = TB_TYPE_VOID, .next = next };
    f->nodes[at].next = r;

    if (f->bbs[bb].end == at) {
        f->bbs[bb].end = r;
    }
    return r;
}

// NOTE(NeGate): Any previous TB_Reg you have saved locally,
// update them or at least shift over all the indices based on `at`
//
// TODO(NeGate): Move this out of this file once it's relevant
// TODO(NeGate): Implement multiple return statements, VLA insertion, and proper labels
TB_Reg tb_insert_copy_ops(TB_Function* f, const TB_Reg* params, TB_Reg at, const TB_Function* src_func, TB_Reg src_base, int count) {
    tb_panic("TODO: implement tb_insert_copy_ops");
    return 0;
}

TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count) {
    size_t count = 0;

    TB_Label* preds;
    if (tls) preds = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    else preds = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Label));

    TB_FOR_BASIC_BLOCK(bb, f) {
        // Empty BB
        if (f->bbs[bb].end == 0) continue;

        TB_Node* end = &f->nodes[f->bbs[bb].end];
        switch (end->type) {
            case TB_IF:
            if (l == end->if_.if_true) preds[count++] = bb;
            if (l == end->if_.if_false) preds[count++] = bb;
            break;

            case TB_GOTO:
            if (l == end->goto_.label) preds[count++] = bb;
            break;

            case TB_SWITCH: {
                size_t entry_count = (end->switch_.entries_end - end->switch_.entries_start) / 2;
                TB_SwitchEntry* entries = (TB_SwitchEntry*) &f->vla.data[end->switch_.entries_start];

                FOREACH_N(i, 0, entry_count) {
                    if (l == entries[i].value) preds[count++] = bb;
                }

                if (l == end->switch_.default_label) preds[count++] = bb;
                break;
            }
            // these blocks have no successors
            case TB_UNREACHABLE: case TB_RET: break;
            default: tb_todo();
        }
    }

    assert(count <= f->bb_count);

    // trim the fat
    if (tls) tb_tls_restore(tls, &preds[count]);
    else preds = tb_platform_heap_realloc(preds, count * sizeof(TB_Label));

    *dst_count = count;
    return preds;
}
