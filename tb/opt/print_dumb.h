
void tb_print_dumb_edge(Lattice** types, TB_Node* n, OutStream* s) {
    if (n) {
        s_color(s, cool_ansi_color(n->gvn));
        s_writef(s, "%%%u", n->gvn);
        s_color(s, 0);

        // i put the assert at the bottom so the last thing printed is the erroneous node ID
        // TB_ASSERT(n->type != TB_NULL);
    } else {
        s_writef(s, "___");
    }
}

void tb_print_dumb_node_raw(Lattice** types, TB_Node* n, OutStream* s) {
    if (n == NULL) {
        s_writef(s, "___");
        return;
    }

    s_color(s, cool_ansi_color(n->gvn));
    s_writef(s, "%%%u: ", n->gvn);
    s_color(s, 0);

    if (types && types[n->gvn] != NULL && types[n->gvn] != &TOP_IN_THE_SKY) {
        print_lattice(types[n->gvn]);
    } else {
        int l = print_type(s, n->dt);
        FOR_N(i, l, 5) { s_writef(s, " "); }
    }
    s_writef(s, " = %s ", tb_node_get_name(n->type));
    if (IS_PROJ(n)) {
        int idx = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
        s_writef(s, "%d", idx);
        if (n->type == TB_BRANCH_PROJ && idx > 0) {
            s_writef(s, ", key=%"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeBranchProj)->key);
        } else if (n->type == TB_MACH_PROJ) {
            TB_NodeMachProj* p = TB_NODE_GET_EXTRA(n);
            s_writef(s, ", mask=");
            tb__print_regmask(s, p->def);
        }
        s_writef(s, " ");
    } else if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
        s_writef(s, "def=");
        tb__print_regmask(s, cpy->def);
        s_writef(s, ", use=");
        tb__print_regmask(s, cpy->use);
        s_writef(s, " ");
    } else if (n->type == TB_IF) {
        TB_NodeIf* br = TB_NODE_GET_EXTRA(n);
        s_writef(s, "prob=%f ", br->prob);
    } else if (n->type == TB_MACH_TEMP) {
        TB_NodeMachTemp* tmp = TB_NODE_GET_EXTRA(n);
        s_writef(s, "def=");
        tb__print_regmask(s, tmp->def);
        s_writef(s, " ");
    } else if (n->type == TB_VSHUFFLE) {
        TB_NodeVShuffle* shuf = TB_NODE_GET_EXTRA(n);
        FOR_N(i, 0, shuf->width) {
            if (i) { s_writef(s, ", "); }
            s_writef(s, "%d", shuf->indices[i]);
        }
        s_writef(s, " ");
    } else if (n->type == TB_MACH_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
        if (sym->name[0]) {
            s_writef(s, "%s ", sym->name);
        } else {
            s_writef(s, "sym%p ", sym);
        }
    } else if (n->type == TB_STORE) {
        print_type(s, n->inputs[3]->dt);
        s_writef(s, " ");
    } if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            s_writef(s, "'%s' ", sym->name);
        } else {
            s_writef(s, "%p ", sym);
        }
    } else if (n->type >= TB_AND && n->type <= TB_SMOD) {
        TB_NodeBinopInt* b = TB_NODE_GET_EXTRA(n);
        if (b->ab & TB_ARITHMATIC_NSW) s_writef(s, "nsw ");
        if (b->ab & TB_ARITHMATIC_NUW) s_writef(s, "nuw ");
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->value < 0xFFFF) {
            s_writef(s, "%"PRId64" ", num->value);
        } else {
            s_writef(s, "%#0"PRIx64" ", num->value);
        }
    } else if (n->type == TB_LOCAL) {
        TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
        s_writef(s, "size=%u, align=%u ", l->size, l->align);
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        s_writef(s, "%.10f ", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        s_writef(s, "%.10f ", f->value);
    } else if (n->type >= 0x100) {
        int family = n->type / 0x100;
        TB_ASSERT(family >= 1 && family < TB_ARCH_MAX);
        tb_codegen_families[family].print_extra(s, n);
        s_writef(s, " ");
    }
    s_writef(s, "( ");
    FOR_N(i, 0, n->input_count) {
        tb_print_dumb_edge(types, n->inputs[i], s);
        s_writef(s, " ");
    }

    bool first = true;
    FOR_N(i, n->input_count, n->input_cap) if (n->inputs[i]) {
        if (first) { s_writef(s, "| "), first = false; }
        tb_print_dumb_edge(types, n->inputs[i], s);
        s_writef(s, " ");
    }

    s_writef(s, ")");

    #if 0
    s_writef(s, "   [[ ");
    FOR_USERS(u, n) {
        tb_print_dumb_edge(types, USERN(u));
        s_writef(s, ":%d ", USERI(u));
    }
    s_writef(s, "]]");
    #endif
}

static void dumb_walk(TB_Function* f, TB_Worklist* ws, TB_Node* n) {
    if (worklist_test_n_set(ws, n)) { return; }
    if (cfg_is_control(n)) {
        FOR_USERS(u, n) {
            if (cfg_is_control(USERN(u))) {
                dumb_walk(f, ws, USERN(u));
            }
        }
    }
    if (n->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, n) if (USERN(u)->type != TB_PROJ) {
            dumb_walk(f, ws, USERN(u));
        }
        FOR_USERS(u, n) if (USERN(u)->type == TB_PROJ) {
            dumb_walk(f, ws, USERN(u));
        }
    } else if (NODE_ISA(n, REGION)) {
        FOR_USERS(u, n) if (USERN(u)->type != TB_PHI) {
            dumb_walk(f, ws, USERN(u));
        }
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            dumb_walk(f, ws, USERN(u));
        }
    } else {
        FOR_USERS(u, n) { dumb_walk(f, ws, USERN(u)); }
    }

    if (IS_PROJ(n) || n->type == TB_PHI) { return; }
    // we wanna place projs & phis underneath the region
    if (NODE_ISA(n, REGION)) {
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            dyn_array_put(ws->items, USERN(u));
        }
    } else if (n->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, n) if (IS_PROJ(USERN(u))) {
            dyn_array_put(ws->items, USERN(u));
        }
    }
    dyn_array_put(ws->items, n);
}

static bool cfg_is_fork_proj(TB_Node* n) { return cfg_is_cproj(n) && tb_node_is_fork_ctrl(n->inputs[0]); }
void tb_print_dumb_raw(TB_Function* f, OutStream* s, bool use_fancy_types) {
    s_writef(s, "====== DUMP %-20s ======\n", f->super.name);

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    TB_Node* root   = f->root_node;
    Lattice** types = use_fancy_types ? f->types : NULL;

    dumb_walk(f, &ws, root);

    FOR_REV_N(i, 0, dyn_array_length(ws.items)) {
        // extra newline on BB boundaries
        if (i + 1 < dyn_array_length(ws.items) && cfg_is_fork_proj(ws.items[i + 1]) && !cfg_is_fork_proj(ws.items[i])) {
            s_writef(s, "\n");
        } else if (cfg_is_region(ws.items[i])) {
            s_writef(s, "\n");
        }
        tb_print_dumb_node_raw(types, ws.items[i], s);
        s_writef(s, "\n");
    }
    worklist_free(&ws);
    s_writef(s, "=======================================\n");
}

void tb_print_dumb_node(Lattice** types, TB_Node* n) {
    tb_print_dumb_node_raw(types, n, &OUT_STREAM_DEFAULT);
}

void tb_print_dumb(TB_Function* f) {
    tb_print_dumb_raw(f, &OUT_STREAM_DEFAULT, false);
}

void tb_save_dump(TB_Function* f, const char* path) {
    FILE* fp = fopen(path, "wb");
    FileOutStream s = fos_make(fp);
    tb_print_dumb_raw(f, &s.header, false);
    fclose(fp);
}
