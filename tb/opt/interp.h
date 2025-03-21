
typedef union {
    void* ptr;
    float f32;
    double f64;
    uint64_t i;
} NodeValue;

typedef struct {
    NodeValue* values;
    bool* ready;
} NodeInterp;

typedef struct NodeMemoryEffect {
    struct NodeMemoryEffect* prev;
    void* addr;
    uintptr_t val;
    size_t size;
} NodeMemoryEffect;

static NodeValue node_eval(TB_Function* f, TB_Worklist* ws, NodeInterp* interp, TB_Node* bot) {
    if (interp->ready[bot->gvn]) {
        return interp->values[bot->gvn];
    }

    // evaluate all the inputs to the next node
    worklist_push(ws, bot);
    for (;;) retry: {
        TB_ASSERT(dyn_array_length(ws->items));
        TB_Node* n = ws->items[dyn_array_length(ws->items) - 1];

        // compute all inputs first
        FOR_N(i, 1, n->input_count) {
            // we never eval control nodes here, or those which are ready
            TB_Node* in = n->inputs[i];
            if (!interp->ready[in->gvn] && !worklist_test_n_set(ws, in)) {
                dyn_array_put(ws->items, in);
                goto retry;
            }
        }

        // now we can evaluate ourselves
        #if TB_OPTDEBUG_INTERP
        printf("       ");
        tb_print_dumb_node(NULL, n);
        printf("\n");
        #endif

        NodeValue v;
        switch (n->type) {
            case TB_ICONST: v.i = TB_NODE_GET_EXTRA_T(n, TB_NodeInt)->value; break;
            case TB_F32CONST: v.f32 = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat32)->value; break;
            case TB_F64CONST: v.f64 = TB_NODE_GET_EXTRA_T(n, TB_NodeFloat64)->value; break;
            // arithmetic
            case TB_ADD: v.i = (interp->values[n->inputs[1]->gvn].i + interp->values[n->inputs[2]->gvn].i) & tb__mask(tb_data_type_bit_size(NULL, n->dt.type)); break;
            case TB_SUB: v.i = (interp->values[n->inputs[1]->gvn].i - interp->values[n->inputs[2]->gvn].i) & tb__mask(tb_data_type_bit_size(NULL, n->dt.type)); break;
            case TB_MUL: v.i = (interp->values[n->inputs[1]->gvn].i * interp->values[n->inputs[2]->gvn].i) & tb__mask(tb_data_type_bit_size(NULL, n->dt.type)); break;
            // integer shifts
            case TB_SHL: v.i = (interp->values[n->inputs[1]->gvn].i << interp->values[n->inputs[2]->gvn].i) & tb__mask(tb_data_type_bit_size(NULL, n->dt.type)); break;
            case TB_SHR: v.i = (interp->values[n->inputs[1]->gvn].i >> interp->values[n->inputs[2]->gvn].i) & tb__mask(tb_data_type_bit_size(NULL, n->dt.type)); break;
            // compares
            case TB_CMP_ULT: v.i = (interp->values[n->inputs[1]->gvn].i < interp->values[n->inputs[2]->gvn].i); break;
            // pointer
            case TB_PTR_OFFSET: v.i = interp->values[n->inputs[1]->gvn].i + interp->values[n->inputs[2]->gvn].i; break;
            // memory
            case TB_STORE: {
                NodeMemoryEffect* effect = tb_arena_alloc(&f->tmp_arena, sizeof(NodeMemoryEffect));
                effect->prev = interp->values[n->inputs[1]->gvn].ptr;
                effect->addr = interp->values[n->inputs[2]->gvn].ptr;
                effect->val = interp->values[n->inputs[3]->gvn].i;
                effect->size = tb_data_type_byte_size(f->super.module, n->inputs[3]->dt.type);
                v.ptr = effect;
                break;
            }
            default: tb_todo();
        }

        dyn_array_pop(ws->items);
        interp->ready[n->gvn] = true;

        if (n == bot){
            worklist_clear(ws);
            return v;
        } else {
            interp->values[n->gvn] = v;
        }
    }
}

uint64_t tb_interpret(TB_Function* f, TB_Worklist* ws, uint64_t* params) {
    printf("====== INTERP %-20s ======\n", f->super.name);

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    NodeInterp interp;
    interp.values = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(NodeValue));
    interp.ready = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(bool));
    FOR_N(i, 0, f->node_count) {
        interp.ready[i] = false;
    }

    TB_Node* ctrl = USERN(proj_with_index(f->root_node, 0));

    #if TB_OPTDEBUG_INTERP
    printf("CTRL:  ");
    tb_print_dumb_node(NULL, ctrl);
    printf("\n");
    #endif

    // populate the function projections
    FOR_USERS(u, f->root_node) {
        TB_Node* un = USERN(u);
        if (is_proj(un)) {
            int i = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
            if (i >= 3) {
                interp.values[un->gvn].i = params[i - 3];
            } else {
                interp.values[un->gvn].i = 0;
            }
            interp.ready[un->gvn] = true;

            #if TB_OPTDEBUG_INTERP
            printf("    >  ");
            tb_print_dumb_node(NULL, un);
            printf("\n");
            #endif
        }
    }

    DynArray(NodeValue) phis = dyn_array_create(NodeValue, 16);
    for (;;) {
        TB_User* next = cfg_next_user(ctrl);
        TB_Node* next_n = USERN(next);

        // phi transition
        if (cfg_is_region(next_n)) {
            int path = 1 + USERI(next);

            dyn_array_clear(phis);
            FOR_USERS(u, next_n) {
                if (USERN(u)->type == TB_PHI) {
                    // we have to revert the write because we need the rest of the phis to
                    // see the changes happen all at once
                    NodeValue v = node_eval(f, ws, &interp, USERN(u)->inputs[path]);
                    dyn_array_put(phis, v);
                    worklist_push(ws, USERN(u));
                }
            }

            // invalidate users of phis
            TB_Node* n;
            while (n = worklist_pop(f->worklist), n) {
                FOR_USERS(u, n) {
                    TB_Node* un = USERN(u);
                    if (interp.ready[un->gvn] && un->type != TB_PHI) {
                        interp.ready[un->gvn] = false;

                        worklist_push(ws, un);
                    }
                }
            }

            // phi writeback
            size_t i = 0;
            FOR_USERS(u, next_n) {
                if (USERN(u)->type == TB_PHI) {
                    interp.values[USERN(u)->gvn] = phis[i++];
                    interp.ready[USERN(u)->gvn] = true;

                    #if TB_OPTDEBUG_INTERP
                    printf("    >  ");
                    tb_print_dumb_node(NULL, USERN(u));
                    printf(" val = %"PRId64"\n", interp.values[USERN(u)->gvn].i);
                    #endif
                }
            }
        } else {
            // process all inputs
            FOR_N(i, 1, next_n->input_count) {
                interp.values[next_n->inputs[i]->gvn] = node_eval(f, ws, &interp, next_n->inputs[i]);
            }
        }
        ctrl = next_n;

        // process control node now
        #if TB_OPTDEBUG_INTERP
        printf("CTRL:  ");
        tb_print_dumb_node(NULL, ctrl);
        printf("\n");
        #endif

        switch (ctrl->type) {
            case TB_RETURN: {
                NodeMemoryEffect* curr = interp.values[ctrl->inputs[1]->gvn].ptr;
                // reverse the effects
                NodeMemoryEffect* next = NULL;
                while (curr) {
                    NodeMemoryEffect* prev = curr->prev;
                    curr->prev = next;

                    next = curr;
                    curr = prev;
                }
                // memory writeback
                curr = next;
                while (curr) {
                    memcpy(curr->addr, &curr->val, curr->size);
                    curr = curr->prev;
                }

                uint64_t ret = ctrl->input_count > 3 ? interp.values[ctrl->inputs[3]->gvn].i : 0;
                tb_arena_clear(&f->tmp_arena);
                return ret;
            }

            case TB_TRAP:
            case TB_UNREACHABLE:
            return 0;

            case TB_REGION:
            case TB_NATURAL_LOOP:
            case TB_AFFINE_LOOP:
            break;

            case TB_BRANCH:
            case TB_AFFINE_LATCH: {
                uint64_t key = interp.values[ctrl->inputs[1]->gvn].i;
                bool match = false;
                TB_Node* default_proj = NULL;

                FOR_USERS(u, ctrl) {
                    TB_Node* un = USERN(u);
                    if (cfg_is_cproj(un)) {
                        int i = TB_NODE_GET_EXTRA_T(un, TB_NodeProj)->index;
                        if (i == 0) {
                            default_proj = un;
                        } else if (TB_NODE_GET_EXTRA_T(un, TB_NodeBranchProj)->key == key) {
                            ctrl = un;
                            match = true;
                            break;
                        }
                    }
                }

                if (!match) {
                    ctrl = default_proj;
                }

                #if TB_OPTDEBUG_INTERP
                printf("CTRL:  ");
                tb_print_dumb_node(NULL, ctrl);
                printf("\n");
                #endif
                break;
            }

            default: tb_todo();
        }
    }
}

