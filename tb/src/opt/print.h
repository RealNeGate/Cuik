
typedef struct {
    TB_FuncOpt* opt;
    TB_Function* f;
    TB_PostorderWalk order;

    int count;
    NL_Map(TB_Node*, char) visited;
    NL_Map(TB_Node*, int) ordinals;
} PrinterCtx;

static size_t find_print_label(PrinterCtx* ctx, TB_Node* n) {
    FOREACH_N(i, 0, ctx->order.count) {
        if (ctx->order.traversal[i] == n) {
            return ctx->order.count - i - 1;
        }
    }

    return tb_assert(0, "printer refers to unknown region");
}

static void print_ref_to_node(PrinterCtx* ctx, TB_Node* n) {
    if (n->type == TB_START) {
        printf("%s", ctx->f->super.name);
    } else if (n->type == TB_REGION) {
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(n);
        if (r->tag != NULL) {
            printf(".%s", r->tag);
        } else {
            printf(".bb%zu", find_print_label(ctx, n));
        }
    } else if (n->type == TB_FLOAT32_CONST) {
        TB_NodeFloat32* f = TB_NODE_GET_EXTRA(n);
        printf("%f", f->value);
    } else if (n->type == TB_FLOAT64_CONST) {
        TB_NodeFloat64* f = TB_NODE_GET_EXTRA(n);
        printf("%f", f->value);
    } else if (n->type == TB_GET_SYMBOL_ADDRESS) {
        TB_Symbol* sym = TB_NODE_GET_EXTRA_T(n, TB_NodeSymbol)->sym;
        if (sym->name) {
            printf("%s", sym->name);
        } else {
            printf("%p", sym);
        }
    } else if (n->type == TB_INTEGER_CONST) {
        TB_NodeInt* num = TB_NODE_GET_EXTRA(n);

        if (num->num_words == 1) {
            printf("%"PRId64, num->words[0]);
        } else {
            printf("0x");
            FOREACH_REVERSE_N(i, 0, num->num_words) {
                if (num) printf("'");
                printf("%016"PRIx64, num->words[i]);
            }
        }
    } else if (n->type == TB_PHI || n->type == TB_LOAD) {
        ptrdiff_t search = nl_map_get(ctx->ordinals, n);
        if (search < 0) {
            // alloc new ID
            int id = ctx->count++;
            nl_map_put(ctx->ordinals, n, id);

            printf("v%d", id);
        } else {
            printf("v%d", ctx->ordinals[search].v);
        }
    } else {
        ptrdiff_t search = nl_map_get(ctx->ordinals, n);
        assert(search >= 0);
        printf("v%d", ctx->ordinals[search].v);
    }
}

static void print_type(TB_DataType dt) {
    assert(dt.width < 8 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            if (dt.data == 0) printf("void");
            else printf("i%d", dt.data);
            break;
        }
        case TB_PTR: {
            if (dt.data == 0) printf("ptr");
            else printf("ptr%d", dt.data);
            break;
        }
        case TB_FLOAT: {
            if (dt.data == TB_FLT_32) printf("f32");
            if (dt.data == TB_FLT_64) printf("f64");
            break;
        }
        case TB_TUPLE: {
            printf("tuple");
            break;
        }
        case TB_CONTROL: {
            printf("control");
            break;
        }
        default: tb_todo();
    }
}

// returns true if it's the first time
static void print_node(PrinterCtx* ctx, TB_Node* n, TB_Node* parent) {
    if (n->type == TB_REGION || n->type == TB_START || n->type == TB_STORE || n->type == TB_INTEGER_CONST || n->type == TB_FLOAT32_CONST || n->type == TB_FLOAT64_CONST || n->type == TB_GET_SYMBOL_ADDRESS) {
        return;
    }

    if (n->type == TB_PROJ && n->dt.type == TB_CONTROL) {
        return;
    }

    if (nl_map_get(ctx->visited, n) >= 0) return;
    nl_map_put(ctx->visited, n, 0);

    int id;
    ptrdiff_t search = nl_map_get(ctx->ordinals, n);
    if (search >= 0) {
        id = ctx->ordinals[search].v;
    } else {
        // alloc new ID
        id = ctx->count++;
        nl_map_put(ctx->ordinals, n, id);
    }

    // print operands
    FOREACH_N(i, 1, n->input_count) {
        switch (n->inputs[i]->type) {
            case TB_LOAD: case TB_PHI:
            if (n->inputs[i]->inputs[0] != parent) {
                break;
            }

            default:
            print_node(ctx, n->inputs[i], parent);
            break;
        }
    }

    // print as instruction
    if (n->dt.type == TB_INT && n->dt.data == 0) {
        printf("  %s.", tb_node_get_name(n));
    } else {
        bool actually_used = false;
        for (User* use = find_users(ctx->opt, n); use; use = use->next) {
            if (use->slot != 0) { actually_used = true; break; }
        }

        if (actually_used) {
            printf("  v%d = %s.", id, tb_node_get_name(n));
        } else {
            printf("  _ = %s.", tb_node_get_name(n));
        }
    }

    TB_DataType dt = n->dt;
    if (n->type >= TB_CMP_EQ && n->type <= TB_CMP_FLE) {
        dt = TB_NODE_GET_EXTRA_T(n, TB_NodeCompare)->cmp_dt;
    }
    print_type(dt);
    printf(" ");

    size_t first = n->type == TB_PROJ ? 0 : 1;
    FOREACH_N(i, first, n->input_count) {
        if (i != first) printf(", ");
        print_ref_to_node(ctx, n->inputs[i]);
    }

    // print extra data
    switch (n->type) {
        case TB_CMP_EQ:
        case TB_CMP_NE:
        case TB_CMP_ULT:
        case TB_CMP_ULE:
        case TB_CMP_SLT:
        case TB_CMP_SLE:
        case TB_CMP_FLT:
        case TB_CMP_FLE:
        case TB_SELECT:
        break;

        case TB_MEMBER_ACCESS: {
            printf(", %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset);
            break;
        }

        case TB_ARRAY_ACCESS: {
            printf(", %"PRId64, TB_NODE_GET_EXTRA_T(n, TB_NodeArray)->stride);
            break;
        }

        case TB_PROJ: {
            printf(", %d", TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index);
            break;
        }

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SHL:
        case TB_SHR:
        case TB_ROL:
        case TB_ROR:
        case TB_SAR:
        case TB_UDIV:
        case TB_SDIV:
        case TB_UMOD:
        case TB_SMOD:
        {
            TB_NodeBinopInt* b = TB_NODE_GET_EXTRA(n);
            if (b->ab & TB_ARITHMATIC_NSW) printf(" !nsw");
            if (b->ab & TB_ARITHMATIC_NUW) printf(" !nuw");
            break;
        }

        case TB_LOAD:
        case TB_STORE:
        case TB_MEMSET:
        case TB_MEMCPY: {
            TB_NodeMemAccess* mem = TB_NODE_GET_EXTRA(n);
            if (mem->is_volatile) printf(" !volatile");
            break;
        }

        case TB_CALL: break;

        case TB_LOCAL: {
            TB_NodeLocal* l = TB_NODE_GET_EXTRA(n);
            printf("!size %u !align %u", l->size, l->align);
            break;
        }

        default: tb_assert(n->extra_count == 0, "TODO");
    }

    for (TB_Attrib* attrib = n->first_attrib; attrib != NULL; attrib = attrib->next) {
        if (attrib->type == TB_ATTRIB_VARIABLE) {
            printf(" !var(%s)", attrib->var.name);
        }
    }

    printf("\n");
}

static void print_effect(PrinterCtx* ctx, TB_Node* n) {
    bool aint_region = n->type != TB_REGION && n->type != TB_START;
    if (aint_region) {
        print_effect(ctx, n->inputs[0]);
        printf("  # fence\n");
    }

    // has control dependencies on this node, we put these after
    for (User* use = find_users(ctx->opt, n); use; use = use->next) {
        if (use->slot == 0 && (use->n->type == TB_PHI || use->n->type == TB_PROJ)) {
            print_node(ctx, use->n, n);
        }
    }

    if (aint_region) {
        FOREACH_N(i, 1, n->input_count) {
            print_node(ctx, n->inputs[i], n);
        }

        switch (n->type) {
            case TB_DEBUGBREAK: printf("  debugbreak\n"); break;
            case TB_UNREACHABLE: printf("  unreachable\n"); break;

            case TB_STORE: {
                printf("  store.");
                print_type(n->inputs[2]->dt);
                printf(" ");
                print_ref_to_node(ctx, n->inputs[1]);
                printf(", ");
                print_ref_to_node(ctx, n->inputs[2]);
                printf("\n");
                break;
            }

            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
                TB_NodeRegion* region = TB_NODE_GET_EXTRA(tb_get_parent_region(n));

                if (region->succ_count == 1) {
                    printf("  goto ");
                    print_ref_to_node(ctx, region->succ[0]);
                    printf("\n");
                } else if (region->succ_count == 2) {
                    printf("  if ");
                    FOREACH_N(i, 1, n->input_count) {
                        if (i != 1) printf(", ");
                        print_ref_to_node(ctx, n->inputs[i]);
                    }
                    if (br->keys[0] == 0) {
                        printf(" then ");
                    } else {
                        printf(" != %"PRId64" then ", br->keys[0]);
                    }
                    print_ref_to_node(ctx, region->succ[0]);
                    printf(" else ");
                    print_ref_to_node(ctx, region->succ[1]);
                    printf("\n");
                } else {
                    printf("  br ");
                    FOREACH_N(i, 1, n->input_count) {
                        if (i != 1) printf(", ");
                        print_ref_to_node(ctx, n->inputs[i]);
                    }
                    printf("%s=> {", n->input_count > 1 ? " " : "");

                    FOREACH_N(i, 0, region->succ_count) {
                        if (i != 0) printf(", %"PRId64": ", br->keys[i - 1]);
                        else printf("default: ");

                        TB_Node* target = region->succ[i];
                        if (TB_NODE_GET_EXTRA_T(target, TB_NodeRegion)->tag) {
                            printf("%s", TB_NODE_GET_EXTRA_T(target, TB_NodeRegion)->tag);
                        } else {
                            printf(".bb%zu", find_print_label(ctx, target));
                        }
                    }
                    printf("}\n");
                }
                break;
            }

            case TB_MEMSET:
            case TB_MEMCPY:
            case TB_CALL: {
                print_node(ctx, n, n);
                break;
            }

            case TB_RET: {
                printf("  ret ");
                FOREACH_N(i, 1, n->input_count) {
                    if (i != 1) printf(", ");
                    print_ref_to_node(ctx, n->inputs[i]);
                }
                printf("\n");
                break;
            }

            default: tb_todo();
        }
    }

    for (User* use = find_users(ctx->opt, n); use; use = use->next) {
        if (use->slot == 0 && use->n->type == TB_LOAD) {
            print_node(ctx, use->n, n);
        }
    }
}

bool tb_funcopt_print(TB_FuncOpt* opt) {
    TB_Function* f = opt->f;

    PrinterCtx ctx = { opt, f };
    ctx.order = tb_function_get_postorder(f);

    FOREACH_REVERSE_N(i, 0, ctx.order.count) {
        TB_Node* bb = ctx.order.traversal[i];
        const char* tag = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->tag;

        if (bb->type == TB_START) {
            printf("%s:", f->super.name);
        } else if (tag != NULL) {
            printf(".%s:", tag);
        } else {
            printf(".bb%zu:", ctx.order.count - i - 1);
        }

        // print predecessors
        if (bb->input_count > 0) {
            printf(" # preds: ");
            FOREACH_N(j, 0, bb->input_count) {
                print_ref_to_node(&ctx, tb_get_parent_region(bb->inputs[j]));
                printf(" ");
            }
        }
        printf("\n");

        if (bb->type == TB_START) {
            dyn_array_for(i, opt->locals) {
                print_node(&ctx, opt->locals[i], NULL);
            }
        }

        TB_Node* end = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end;
        print_effect(&ctx, end);
    }

    nl_map_free(ctx.ordinals);
    nl_map_free(ctx.visited);
    tb_function_free_postorder(&ctx.order);
    return false;
}
