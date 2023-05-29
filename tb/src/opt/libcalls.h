
static bool libcalls(TB_Function* f) {
    // TODO(NeGate): come back
    tb_todo();

    #if 0
    bool changes = false;

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            // convert call into builtins
            if (n->type == TB_CALL && n->inputs[0]->type == TB_GET_SYMBOL_ADDRESS) {
                const TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n->inputs[0], TB_NodeSymbol)->sym;

                // wacky? our memcpy shouldn't be allowed to lower into the builtin since
                // it might self reference?
                if (sym == &f->super) continue;

                char* fname = sym->name;
                if (strcmp(fname, "memcpy") == 0) {
                    TB_Node* n2 = tb_alloc_node(f, TB_MEMCPY, TB_TYPE_VOID, 3, sizeof(TB_NodeMemAccess));
                    tb_insert_node(f, bb, n, n2);

                    n2->type = TB_MEMCPY;
                    n2->inputs[0] = n->inputs[1];
                    n2->inputs[1] = n->inputs[2];
                    n2->inputs[2] = n->inputs[3];

                    // memcpy returns the destination pointer
                    tb_transmute_to_pass(n, n->inputs[1]);
                    changes = true;
                }
            }
        }
    }

    return changes;
    #endif
}

TB_Pass tb_opt_libcalls(TB_Function* f) {
    return (TB_Pass){ .name = "Libcalls", .func_run = libcalls };
}
