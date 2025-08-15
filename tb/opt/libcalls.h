
// this is a peephole lmao
static TB_Node* ideal_libcall(TB_Function* f, TB_Node* n) {
    if (n->inputs[2]->type != TB_SYMBOL) {
        return NULL;
    }

    const TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeSymbol)->sym;

    bool is_memcpy = strcmp(sym->name, "memcpy") == 0;
    bool is_memset = strcmp(sym->name, "memset") == 0;
    if (is_memcpy || is_memset) {
        // remove from callgraph
        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            if (un->type == TB_CALLGRAPH) {
                TB_Node* last = un->inputs[un->input_count - 1];
                set_input(f, un, NULL, un->input_count - 1);
                set_input(f, un, last, USERI(u));
                un->input_count--;
                break;
    	    }
        }

        TB_Node* n2 = tb_alloc_node(f, is_memset ? TB_MEMSET : TB_MEMCPY, TB_TYPE_MEMORY, 5, sizeof(TB_NodeMemAccess));
        set_input(f, n2, n->inputs[0], 0); // ctrl
        set_input(f, n2, n->inputs[1], 1); // mem
        set_input(f, n2, n->inputs[3], 2); // dst
        set_input(f, n2, n->inputs[4], 3); // val
        set_input(f, n2, n->inputs[5], 4); // size
        TB_NODE_SET_EXTRA(n2, TB_NodeMemAccess, .align = 1);

        TB_Node* dst_ptr = n->inputs[2];
        TB_Node* ctrl = n->inputs[0];

        // returns the destination pointer, convert any users of that to dst
        // and CALL has a projection we wanna get rid of
        TB_NodeCall* c = TB_NODE_GET_EXTRA_T(n, TB_NodeCall);

        TB_Node* proj0 = USERN(proj_with_index(n, 0));
        subsume_node(f, proj0, ctrl);
        TB_Node* proj1 = USERN(proj_with_index(n, 1));
        subsume_node(f, proj1, n2);
        TB_Node* proj2 = USERN(proj_with_index(n, 2));
        subsume_node(f, proj2, dst_ptr);

        return dst_ptr;
    } else if (strcmp(sym->name, "sqrt") == 0) {
        TB_Node* proj0 = USERN(proj_with_index(n, 0));
        TB_Node* proj1 = USERN(proj_with_index(n, 1));
        TB_Node* proj2 = USERN(proj_with_index(n, 2));

        if (proj2->dt.type == TB_TAG_F64) {
            TB_Node* zero = make_f64_node(f, 0.0);

            // if it's already gated, skip it
            if (n->inputs[0]->type == TB_PROJ && n->inputs[0]->inputs[0]->type == TB_IF) {
                TB_Node* cmp = n->inputs[0]->inputs[0]->inputs[1];
                if (cmp->type == TB_CMP_FLT && cmp->inputs[2] == zero && cmp->inputs[1] == n->inputs[3]) {
                    return NULL;
                }
            }

            // If we support a builtin sqrt intrinsic, we can wrap it in a guard
            TB_Node* n2 = tb_alloc_node(f, TB_X86INTRIN_SQRT, proj2->dt, 2, 0);
            set_input(f, n2, n->inputs[3], 1); // mem
            mark_node(f, n2);

            // if we're above zero or unordered, we don't use the libcall
            TB_Node* cmp_node = tb_alloc_node(f, TB_CMP_FLT, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
            set_input(f, cmp_node, n->inputs[3], 1);
            set_input(f, cmp_node, zero, 2);
            TB_NODE_SET_EXTRA(cmp_node, TB_NodeCompare, .cmp_dt = TB_TYPE_F64);

            TB_Node* br = tb_alloc_node(f, TB_IF, TB_TYPE_TUPLE, 2, sizeof(TB_NodeIf));
            set_input(f, br, n->inputs[0], 0);
            set_input(f, br, cmp_node, 1);
            mark_node(f, br);
            TB_NODE_SET_EXTRA(br, TB_NodeIf, .prob = 0.001f);

            TB_Node* ift = make_proj_node(f, TB_TYPE_CONTROL, br, 0);
            TB_Node* iff = make_proj_node(f, TB_TYPE_CONTROL, br, 1);

            TB_Node* join = tb_alloc_node(f, TB_REGION, TB_TYPE_CONTROL, 2, sizeof(TB_NodeRegion));
            subsume_node2(f, proj0, join);
            TB_Node* phi_mem = tb_alloc_node(f, TB_PHI, TB_TYPE_MEMORY, 3, 0);
            subsume_node2(f, proj1, phi_mem);
            TB_Node* phi_data = tb_alloc_node(f, TB_PHI, proj2->dt, 3, 0);
            subsume_node2(f, proj2, phi_data);

            set_input(f, n, ift, 0);

            set_input(f, join, iff, 0);
            set_input(f, join, proj0, 1);
            mark_node(f, join);

            set_input(f, phi_mem, join, 0);
            set_input(f, phi_mem, n->inputs[1], 1);
            set_input(f, phi_mem, proj1, 2);
            mark_node(f, phi_mem);

            set_input(f, phi_data, join, 0);
            set_input(f, phi_data, n2, 1);
            set_input(f, phi_data, proj2, 2);
            mark_node(f, phi_data);
            return n;
        }
    }

    return NULL;
}

