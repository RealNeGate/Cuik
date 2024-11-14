
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
    }

    return NULL;
}

