
void tb_print_dumb_edge(Lattice** types, TB_Node* n) {
    if (n) {
        if (is_proj(n)) {
            int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            n = n->inputs[0];

            printf("%%%u.%d ", n->gvn, index);
        } else {
            printf("%%%u ", n->gvn);
        }
    } else {
        printf("___ ");
    }
}

void tb_print_dumb_node(Lattice** types, TB_Node* n) {
    printf("%%%u: ", n->gvn);
    if (types && types[n->gvn] != NULL && types[n->gvn] != &TOP_IN_THE_SKY) {
        print_lattice(types[n->gvn]);
    } else {
        if (n->dt.type == TB_TAG_TUPLE) {
            // print with multiple returns
            TB_Node* projs[32] = { 0 };
            FOR_USERS(u, n) {
                if (is_proj(USERN(u))) {
                    int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                    projs[index] = USERN(u);
                }
            }

            printf("{ ");
            FOR_N(i, 0, 32) {
                if (projs[i] == NULL) break;
                if (i) printf(", ");
                printf("%%%u:", projs[i]->gvn);
                print_type(projs[i]->dt);
            }
            printf(" }");
        } else {
            print_type(n->dt);
        }
    }
    printf(" = %s ", tb_node_get_name(n));
    if (is_proj(n)) {
        printf("%d ", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
    } else if (n->type == TB_MACH_PROJ) {
        printf("%d ", TB_NODE_GET_EXTRA_T(n, TB_NodeMachProj)->index);
    } else if (n->type == TB_MACH_SYMBOL) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeMachSymbol)->sym;
        if (sym->name[0]) {
            printf("%s ", sym->name);
        } else {
            printf("sym%p ", sym);
        }
    } else if (n->type == TB_MEMBER_ACCESS) {
        printf("%"PRIi64" ", TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset);
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
        if (b->ab & TB_ARITHMATIC_NSW) printf(" nsw");
        if (b->ab & TB_ARITHMATIC_NUW) printf(" nuw");
    } else if (n->type == TB_ICONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->value < 0xFFFF) {
            int bits = n->dt.type == TB_TAG_PTR ? 64 : n->dt.data;
            printf("%"PRId64" ", tb__sxt(num->value, bits, 64));
        } else {
            printf("%#0"PRIx64" ", num->value);
        }
    } else if (n->type == TB_F32CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        printf("%f ", f->value);
    } else if (n->type == TB_F64CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        printf("%f ", f->value);
    }
    printf("( ");
    FOR_N(i, 0, n->input_count) {
        tb_print_dumb_edge(types, n->inputs[i]);
    }
    printf(")");
}

static void dumb_walk(TB_Function* f, Lattice** types, TB_Node* n, uint64_t* visited) {
    if (visited[n->gvn / 64] & (1ull << (n->gvn % 64))) {
        return;
    }
    visited[n->gvn / 64] |= (1ull << (n->gvn % 64));

    if (n->type == TB_PHI) {
        FOR_N(i, 1, n->input_count) {
            dumb_walk(f, types, n->inputs[i], visited);
        }
        dumb_walk(f, types, n->inputs[0], visited);
    } else {
        FOR_REV_N(i, 0, n->input_count) if (n->inputs[i]) {
            TB_Node* in = n->inputs[i];
            if (is_proj(in)) { in = in->inputs[0]; }
            dumb_walk(f, types, in, visited);
        }

        tb_print_dumb_node(types, n);
        printf("\n");

        if (cfg_is_region(n)) {
            // we want the phis to appear below the region node, looks nice
            FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
                assert(USERI(u) == 0);
                uint32_t un_gvn = USERN(u)->gvn;
                visited[un_gvn / 64] |= (1ull << (un_gvn % 64));
                tb_print_dumb_node(types, USERN(u));
                printf("\n");
            }
        }
    }
}

void tb_print_dumb(TB_Function* f, bool use_fancy_types) {
    printf("=== DUMP %s ===\n", f->super.name);

    uint64_t* visited = tb_platform_heap_alloc(((f->node_count + 63) / 64) * sizeof(uint64_t));
    memset(visited, 0, ((f->node_count + 63) / 64) * sizeof(uint64_t));

    TB_Node* root   = f->root_node;
    Lattice** types = use_fancy_types ? f->types : NULL;

    tb_print_dumb_node(types, root);
    printf("\n");

    visited[root->gvn / 64] |= (1ull << (root->gvn % 64));

    FOR_N(i, 0, root->input_count) {
        dumb_walk(f, types, root->inputs[i], visited);
    }

    tb_platform_heap_free(visited);
}
