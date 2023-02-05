#include "../tb_internal.h"

// TODO(NeGate): implement restrict semantics
static bool tb_address_may_alias(TB_Function* f, TB_Reg r, TB_Reg target) {
    return true;
}

static bool load_store_elim(TB_Function* f) {
    int changes = 0;

    #if 0
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            TB_Node* n = &f->nodes[r];

            if (n->type == TB_LOAD) {
                TB_DataType dt = n->dt;
                TB_Reg addr = n->load.address;
                uint32_t alignment = n->load.alignment;

                // Find any duplicates
                for (TB_Node* other = &f->nodes[n->next]; other != &f->nodes[0]; other = &f->nodes[other->next]) {
                    TB_NodeTypeEnum t = other->type;

                    if (t == TB_LOAD &&
                        other->load.alignment == alignment &&
                        other->load.address == addr &&
                        TB_DATA_TYPE_EQUALS(dt, other->dt)) {
                        other->type = TB_PASS;
                        other->pass.value = i;
                        changes++;
                    } else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
                        // Can't read past side effects or terminators, don't
                        // know what might happen
                        if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, n->store.address, addr))) {
                            break;
                        }
                    }
                }
            }
        }

        // STORE *p, _1 #
        // ...          # anything but a possible store to addr, terminator,
        // _2 = LOAD *p # or side effect then _2 = _1
        TB_FOR_NODE(n, f, bb) {
            if (n->type == TB_STORE) {
                TB_DataType dt = n->dt;
                TB_Reg value = n->store.value;
                TB_Reg addr = n->store.address;
                uint32_t alignment = n->store.alignment;

                // Find any duplicates
                TB_FOR_NODE_AFTER(other, n, bb) {
                    TB_NodeTypeEnum t = other->type;

                    if (t == TB_LOAD &&
                        other->load.alignment == alignment &&
                        other->load.address == addr &&
                        TB_DATA_TYPE_EQUALS(dt, other->dt)) {
                        other->type = TB_PASS;
                        other->pass.value = value;
                        changes++;
                    } else if (TB_IS_NODE_TERMINATOR(t) || TB_IS_NODE_SIDE_EFFECT(t)) {
                        // Can't read past side effects or terminators, don't
                        // know what might happen
                        if (t != TB_STORE || (t == TB_STORE && !tb_address_may_alias(f, other->store.address, addr))) {
                            break;
                        }
                    }
                }
            }
        }
    }
    #endif

    return changes;
}

TB_API TB_Pass tb_opt_load_store_elim(void) {
    return (TB_Pass){
        .mode = TB_FUNCTION_PASS,
        .name = "LoadStoreElimination",
        .func_run = load_store_elim,
    };
}
