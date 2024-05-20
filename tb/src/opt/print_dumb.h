
void tb_print_dumb_edge(Lattice** types, TB_Node* n) {
    if (n) {
        assert(n->type != TB_NULL);
        printf("%%%u ", n->gvn);
    } else {
        printf("___ ");
    }
}

void tb_print_dumb_node(Lattice** types, TB_Node* n) {
    printf("%%%-4u: ", n->gvn);
    if (types && types[n->gvn] != NULL && types[n->gvn] != &TOP_IN_THE_SKY) {
        print_lattice(types[n->gvn]);
    } else {
        int l = print_type(n->dt);
        FOR_N(i, l, 5) { printf(" "); }
    }
    printf(" = %s ", tb_node_get_name(n->type));
    if (is_proj(n)) {
        printf("%d ", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
    } else if (n->type == TB_MACH_PROJ) {
        printf("%d ", TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->index);
    } else if (n->type == TB_MACH_COPY) {
        TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(n);
        printf("def=");
        tb__print_regmask(cpy->def);
        printf(", use=");
        tb__print_regmask(cpy->use);
        printf(" ");
    } else if (n->type == TB_MACH_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
        if (sym->name[0]) {
            printf("%s ", sym->name);
        } else {
            printf("sym%p ", sym);
        }
    } else if (n->type == TB_STORE) {
        print_type(n->inputs[3]->dt);
        printf(" ");
    } if (n->type == TB_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name[0]) {
            printf("'%s' ", sym->name);
        } else {
            printf("%p ", sym);
        }
    } else if (n->type >= TB_AND && n->type <= TB_SMOD) {
        TB_NodeBinopInt* b = TB_NODE_GET_EXTRA(n);
        if (b->ab & TB_ARITHMATIC_NSW) printf("nsw ");
        if (b->ab & TB_ARITHMATIC_NUW) printf("nuw ");
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->value < 0xFFFF) {
            printf("%"PRId64" ", num->value);
        } else {
            printf("%#0"PRIx64" ", num->value);
        }
    } else if (n->type == TB_LOCAL) {
        TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
        printf("size=%u align=%u ", l->size, l->align);
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        printf("%f ", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        printf("%f ", f->value);
    } else if (n->type > 0x100) {
        int family = n->type / 0x100;
        assert(family >= 1 && family < TB_ARCH_MAX);
        tb_codegen_families[family].print_dumb_extra(n);
    }
    printf("( ");
    FOR_N(i, 0, n->input_count) {
        tb_print_dumb_edge(types, n->inputs[i]);
    }
    printf(")");
}

static void dumb_walk(TB_Function* f, TB_Worklist* ws, Lattice** types, TB_Node* n) {
    if (worklist_test_n_set(ws, n)) { return; }
    if (cfg_is_control(n)) {
        FOR_USERS(u, n) {
            if (cfg_is_control(USERN(u))) {
                dumb_walk(f, ws, types, USERN(u));
            }
        }
    }
    if (n->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, n) if (USERN(u)->type != TB_PROJ) {
            dumb_walk(f, ws, types, USERN(u));
        }
        FOR_USERS(u, n) if (USERN(u)->type == TB_PROJ) {
            dumb_walk(f, ws, types, USERN(u));
        }
    } else if (cfg_is_region(n)) {
        FOR_USERS(u, n) if (USERN(u)->type != TB_PHI) {
            dumb_walk(f, ws, types, USERN(u));
        }
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            dumb_walk(f, ws, types, USERN(u));
        }
    } else {
        FOR_USERS(u, n) { dumb_walk(f, ws, types, USERN(u)); }
    }

    if (is_proj(n) || n->type == TB_PHI) { return; }
    // we wanna place projs & phis underneath the region
    if (cfg_is_region(n)) {
        FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
            dyn_array_put(ws->items, USERN(u));
        }
    } else if (n->dt.type == TB_TAG_TUPLE) {
        FOR_USERS(u, n) if (is_proj(USERN(u))) {
            dyn_array_put(ws->items, USERN(u));
        }
    }
    dyn_array_put(ws->items, n);
}

static bool cfg_is_fork_proj(TB_Node* n) { return cfg_is_cproj(n) && cfg_is_fork(n->inputs[0]); }
void tb_print_dumb(TB_Function* f, bool use_fancy_types) {
    printf("=== DUMP %s ===\n", f->super.name);

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    TB_Node* root   = f->root_node;
    Lattice** types = use_fancy_types ? f->types : NULL;

    dumb_walk(f, &ws, types, root);

    FOR_REV_N(i, 0, dyn_array_length(ws.items)) {
        // extra newline on BB boundaries
        if (i + 1 < dyn_array_length(ws.items) && cfg_is_fork_proj(ws.items[i + 1]) && !cfg_is_fork_proj(ws.items[i])) {
            printf("\n");
        } else if (cfg_is_region(ws.items[i])) {
            printf("\n");
        }
        tb_print_dumb_node(types, ws.items[i]);
        printf("\n");
    }
    worklist_free(&ws);
}
