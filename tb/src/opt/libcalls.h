
// this is a peephole lmao
static TB_Node* ideal_libcall(TB_Passes* restrict passes, TB_Function* f, TB_Node* n) {
    if (n->inputs[1]->type != TB_GET_SYMBOL_ADDRESS) {
        return NULL;
    }

    const TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[1], TB_NodeSymbol)->sym;

    // wacky? our memcpy shouldn't be allowed to lower into the builtin since
    // it might self reference?
    if (sym == &f->super) {
        return NULL;
    }

    char* fname = sym->name;
    if (strcmp(fname, "memcpy") == 0) {
        TB_Node* n2 = tb_alloc_node(f, TB_MEMCPY, TB_TYPE_VOID, 4, sizeof(TB_NodeMemAccess));
        set_input(passes, n2, n->inputs[0], 0); // control
        set_input(passes, n2, n->inputs[2], 1); // dst
        set_input(passes, n2, n->inputs[3], 2); // val
        set_input(passes, n2, n->inputs[4], 3); // size
        TB_NODE_SET_EXTRA(n2, TB_NodeMemAccess, .align = 1, .is_volatile = false);

        TB_Node* dst_ptr = n->inputs[2];

        // memcpy returns the destination pointer, convert any non-control users
        // and pass them to dst
        for (User* use = find_users(passes, n); use; use = use->next) {
            if (use->slot != 0) {
                subsume_node(passes, f, use->n, dst_ptr);
            }
        }

        return n2;
    } else if (strcmp(fname, "memset") == 0) {
        TB_Node* n2 = tb_alloc_node(f, TB_MEMSET, TB_TYPE_VOID, 4, sizeof(TB_NodeMemAccess));
        set_input(passes, n2, n->inputs[0], 0); // control
        set_input(passes, n2, n->inputs[2], 1); // dst
        set_input(passes, n2, n->inputs[3], 2); // val
        set_input(passes, n2, n->inputs[4], 3); // size
        TB_NODE_SET_EXTRA(n2, TB_NodeMemAccess, .align = 1, .is_volatile = false);

        TB_Node* dst_ptr = n->inputs[2];

        // memset returns the destination pointer, convert any non-control users
        // and pass them to dst
        for (User* use = find_users(passes, n); use; use = use->next) {
            if (use->slot != 0) {
                subsume_node(passes, f, use->n, dst_ptr);
            }
        }

        return n2;
    } else {
        return NULL;
    }
}
