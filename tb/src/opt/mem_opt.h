// Certain aliasing optimizations technically count as peepholes lmao, these can get fancy
// so the sliding window notion starts to break down but there's no global analysis and
// i can make them incremental technically so we'll go wit it.
typedef struct {
    TB_Node* base;
    int64_t offset;
} KnownPointer;

static bool is_local_ptr(TB_Node* n) {
    while (n->type == TB_PTR_OFFSET) { n = n->inputs[1]; }
    return n->type == TB_LOCAL;
}

static KnownPointer known_pointer(TB_Node* n) {
    if (n->type == TB_PTR_OFFSET && n->inputs[2]->type == TB_ICONST) {
        return (KnownPointer){ n->inputs[1], TB_NODE_GET_EXTRA_T(n->inputs[2], TB_NodeInt)->value };
    } else {
        return (KnownPointer){ n, 0 };
    }
}

static TB_Node* ideal_load(TB_Function* f, TB_Node* n) {
    TB_Node* ctrl = n->inputs[0];
    TB_Node* mem = n->inputs[1];
    TB_Node* addr = n->inputs[2];

    if (ctrl != NULL) {
        // we've dependent on code which must always be run (ROOT.mem)
        if (n->inputs[0]->type == TB_PROJ && n->inputs[0]->inputs[0]->type == TB_ROOT) {
            set_input(f, n, NULL, 0);
            return n;
        } else {
            TB_Node* base = addr;
            while (base->type == TB_PTR_OFFSET) {
                base = base->inputs[1];
            }

            // loads based on LOCALs don't need control-dependence, it's actually kinda annoying
            if (base->type == TB_LOCAL) {
                set_input(f, n, NULL, 0);
                return n;
            }
        }

        // if all paths are dominated by a load of some address then it's safe
        // to relax ctrl deps.
        ICodeGen* cg = f->super.module->codegen;
        int bits_read = bits_in_data_type(cg->pointer_size, n->dt);

        // find other users of the address which read the same size (or more)
        FOR_USERS(u, addr) {
            TB_NodeTypeEnum type = 0;
            if (USERN(u) != n && USERI(u) == 2 && USERN(u)->type == TB_LOAD) {
                TB_DataType mem_dt = n->type == TB_LOAD ? n->dt : n->inputs[3]->dt;
                int other_bits_read = bits_in_data_type(cg->pointer_size, mem_dt);
                if (bits_read <= other_bits_read) {
                    TB_Node* other_ctrl = USERN(u)->inputs[0];
                    if (other_ctrl == NULL || (fast_dommy(other_ctrl, ctrl) && other_ctrl != ctrl)) {
                        set_input(f, n, other_ctrl, 0);
                        return n;
                    }
                }
            }
        }
    }

    return NULL;
}

static TB_Node* identity_load(TB_Function* f, TB_Node* n) {
    // god i need a pattern matcher
    //   (load (store X A Y) A) => Y
    TB_Node *mem = n->inputs[1], *addr = n->inputs[2];
    if (mem->type == TB_STORE && mem->inputs[2] == addr &&
        n->dt.raw == mem->inputs[3]->dt.raw) {
        return mem->inputs[3];
    }

    return n;
}

static Lattice* value_split_mem(TB_Function* f, TB_Node* n) {
    TB_NodeMemSplit* s = TB_NODE_GET_EXTRA(n);
    TB_Arena* arena = get_permanent_arena(f->super.module);

    size_t size = sizeof(Lattice) + s->alias_cnt*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = s->alias_cnt };
    FOR_N(i, 0, s->alias_cnt) {
        l->elems[i] = lattice_alias(f, s->alias_idx[i]);
    }

    Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static TB_Node* ideal_merge_mem(TB_Function* f, TB_Node* n) {
    bool progress = false;
    TB_Node* split_node = n->inputs[1];
    TB_NodeMemSplit* split = TB_NODE_GET_EXTRA(split_node);

    // remove useless split edges
    size_t i = 2;
    while (i < n->input_count) {
        if (n->inputs[i]->type == TB_PROJ && n->inputs[i]->inputs[0] == split_node && single_use(n->inputs[i])) {
            int j = i - 2;
            progress = true;

            assert(split->alias_cnt > 0);
            assert(split->alias_cnt + 2 == n->input_count);

            split->alias_cnt -= 1;
            split->alias_idx[j] = split->alias_idx[split->alias_cnt];

            TB_User* proj = proj_with_index(split_node, split->alias_cnt);
            TB_NODE_SET_EXTRA(USERN(proj), TB_NodeProj, .index = j);

            tb_kill_node(f, n->inputs[i]);

            // simple remove-swap
            set_input(f, n, n->inputs[n->input_count - 1], i);
            set_input(f, n, NULL, n->input_count - 1);
            n->input_count -= 1;

            // we didn't *really* change the memory type, just reordered it (all the live projs
            // are the same so we don't need to push them onto the worklist)
            Lattice* new_split_type = value_split_mem(f, split_node);
            latuni_set(f, split_node, new_split_type);
        } else {
            i += 1;
        }
    }

    return progress ? n : NULL;
}

static bool no_alias_with(KnownPointer us, int us_bytes, TB_Node* them, int ptr_size) {
    int them_bytes = bytes_in_data_type(ptr_size, them->inputs[3]->dt);
    KnownPointer them_addr = known_pointer(them->inputs[2]);

    if (them_addr.base == us.base) {
        // if the stores don't intersect
        if (!(them_addr.offset < us.offset+us_bytes && us.offset < them_addr.offset+them_bytes)) {
            return true;
        }
    }

    return false;
}

static TB_Node* ideal_store(TB_Function* f, TB_Node* n) {
    TB_Node *mem = n->inputs[1], *addr = n->inputs[2], *val = n->inputs[3];
    TB_DataType dt = val->dt;

    // store is next to a non-aliasing adjacent store (or merge to non-adjacent stores)
    ICodeGen* cg = f->super.module->codegen;
    int curr_bytes = bytes_in_data_type(cg->pointer_size, val->dt);
    KnownPointer curr_ptr = known_pointer(addr);

    #if 0
    if (mem->type == TB_MERGEMEM && mem->inputs[0] == n->inputs[0]) {
        tb_print(f, f->tmp_arena);

        bool good = true;
        FOR_N(i, 2, mem->input_count) {
            if (mem->inputs[i]->type != TB_STORE || !no_alias_with(curr_ptr, curr_bytes, mem->inputs[i], cg->pointer_size)) {
                good = false;
                break;
            }
        }

        if (good) {
            TB_Node* split = mem->inputs[1];
            TB_NodeMemSplit* split_info = TB_NODE_GET_EXTRA(split);
            assert(split->type == TB_SPLITMEM);

            if (split_info->same_edges) {
                TB_Node* new_proj = make_proj_node(f, TB_TYPE_MEMORY, split, split_info->alias_cnt++);

                set_input(f, n, new_proj, 1);
                subsume_node2(f, n, mem);
                add_input_late(f, mem, n);

                mark_node(f, mem);
                mark_node(f, split);
                mark_users(f, split);
                mark_users(f, mem);

                __debugbreak();
                return n;
            }
        }
    } else if (mem->type == TB_STORE && mem->inputs[0] == n->inputs[0]) {
        if (no_alias_with(curr_ptr, curr_bytes, mem, cg->pointer_size)) {
            TB_Node* split = tb_alloc_node(f, TB_SPLITMEM, TB_TYPE_TUPLE, 2, sizeof(TB_NodeMemSplit) + 2*sizeof(int));
            set_input(f, split, mem->inputs[0], 0);
            set_input(f, split, mem->inputs[1], 1);

            TB_Node* us_proj   = make_proj_node(f, TB_TYPE_MEMORY, split, 0);
            TB_Node* them_proj = make_proj_node(f, TB_TYPE_MEMORY, split, 1);
            TB_NodeMemSplit* split_info = TB_NODE_GET_EXTRA(split);

            split_info->alias_cnt = 2;
            split_info->same_edges = true;

            set_input(f, n,   us_proj,   1);
            set_input(f, mem, them_proj, 1);

            // merge node
            TB_Node* merge = tb_alloc_node(f, TB_MERGEMEM, TB_TYPE_MEMORY, 4, 0);
            subsume_node2(f, mem, merge);
            subsume_node2(f, n, merge);

            set_input(f, merge, n->inputs[0], 0);
            set_input(f, merge, split,        1);
            set_input(f, merge, n,            2);
            set_input(f, merge, mem,          3);

            mark_node(f, them_proj);
            mark_node(f, us_proj);
            mark_node(f, merge);
            mark_node(f, split);
            mark_users(f, merge);

            // technically we've substituted for the peeps because
            // we're wrapping the store in a split and don't want
            // peeps to fuck that up.
            return n;
        }
    }
    #endif

    // if a store has only one user in this chain it means it's only job was
    // to facilitate the creation of that user store... if we can detect that
    // user store is itself dead, everything in the middle is too.
    if (mem->type == TB_STORE && single_use(mem) && mem->inputs[2] == addr && mem->inputs[3]->dt.raw == dt.raw) {
        // choose the bigger alignment (we wanna keep this sort of info)
        TB_NodeMemAccess* a = TB_NODE_GET_EXTRA(mem);
        TB_NodeMemAccess* b = TB_NODE_GET_EXTRA(n);
        if (a->align > b->align) b->align = a->align;

        // make sure to kill the stores to avoid problems
        TB_Node* parent = mem->inputs[1];
        tb_kill_node(f, mem);

        set_input(f, n, parent, 1);
        return n;
    }

    return NULL;
}

static TB_Node* ideal_return(TB_Function* f, TB_Node* n) {
    return NULL;
}

