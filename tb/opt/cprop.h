
// An Efficient Representation for Sparse Sets (1993), Briggs and Torczon
typedef struct {
    size_t count;

    int* array;
    ArenaArray(int) stack;
} SparseSet;

static SparseSet sparse_set_alloc(TB_Arena* arena, size_t count) {
    SparseSet s;
    s.count = count;
    s.array = tb_arena_alloc(arena, count * sizeof(int));
    s.stack = aarray_create(arena, int, 16);
    FOR_N(i, 0, s.count) {
        s.array[i] = -1;
    }
    return s;
}

static void sparse_set_put(SparseSet* set, int v) {
    TB_ASSERT(v < set->count);
    if (set->array[v] < 0) {
        set->array[v] = aarray_length(set->stack);
        aarray_push(set->stack, v);
    }
}

static void sparse_set_clear(SparseSet* set) {
    aarray_for(i, set->stack) {
        set->array[set->stack[i]] = -1;
    }
    aarray_clear(set->stack);
}

static int sparse_set_pop(SparseSet* set) {
    if (aarray_length(set->stack) > 0) {
        int v = aarray_pop(set->stack);
        set->array[v] = -1;
        return v;
    } else {
        return -1;
    }
}

static void sparse_set_remove(SparseSet* set, int v) {
    if (set->array[v] >= 0) {
        aarray_remove(set->stack, set->array[v]);
        set->array[v] = -1;
    }
}

static bool sparse_set_test(SparseSet* set, int v) {
    return set->array[v] >= 0;
}

struct CProp_Partition {
    int id, input_count;

    // all nodes in the partition must share type
    Lattice* type;

    // members includes both leaders and followers, im
    // too lazy to split them into separate lists.
    //
    // if it becomes relevant for perf reasons i'll deal with
    // that later...
    size_t follower_count;
    size_t member_count;
    CProp_Node* members;

    size_t touched_count;
    CProp_Node* touched;

    size_t cprop_count;
    CProp_Node* cprop;
};

struct CProp_Node {
    // partition-list
    CProp_Node* next;
    CProp_Partition* partition;
    CProp_Partition* old_partition;

    // local "touched" set
    CProp_Node* next_touched;

    // local "cprop" set
    CProp_Node* prev_cprop;
    CProp_Node* next_cprop;

    // if we're a follower, whose the leader
    CProp_Node* leader;

    // membership tests
    bool in_worklist : 1;
    bool in_touched  : 1;
    bool in_cprop    : 1;

    TB_Node* n;
};

TB_Node* gcf_congruent_leader(TB_Function* f, TB_Node* n) {
    // pessimistic? this can't do shit
    if (f->gcf_nodes == NULL) {
        return n;
    }

    CProp_Node* leader = f->gcf_nodes[n->gvn];
    while (leader->leader) {
        leader = leader->leader;
    }
    return leader->n;
}

bool gcf_is_congruent(TB_Function* f, TB_Node* a, TB_Node* b) {
    // pessimistic? just use value number equivalence
    if (f->gcf_nodes == NULL) {
        return a == b;
    }

    if (f->gcf_nodes[a->gvn]->partition == f->gcf_nodes[b->gvn]->partition) {
        Lattice* aa = f->types[a->gvn];
        Lattice* bb = f->types[b->gvn];
        return aa == &TOP_IN_THE_SKY || bb == &TOP_IN_THE_SKY ? true : aa == bb;
    }

    return false;
}

static void cprop_dump(TB_Function* f, CProp_Partition* p) {
    if (p->members == NULL) {
        printf("Partition: EMPTY???\n");
    } else {
        printf("Partition: ");
        tb_print_dumb_node(f->types, p->members->n);
        printf("\n  ");
        for (CProp_Node* node = p->members; node; node = node->next) {
            printf("%%%u ", node->n->gvn);
        }
        printf("\n");
    }
}

// decide the initial partitions
static bool is_opcode_equal(TB_Node* a, TB_Node* b) {
    if (a->type != b->type || a->input_count != b->input_count) {
        return false;
    }

    size_t extra = extra_bytes(a);
    return extra == 0 || memcmp(a->extra, b->extra, extra) == 0;
}

struct CProp {
    ArenaArray(CProp_Partition*) partitions;
    CProp_Node** nodes;

    SparseSet cprop_ws;
    SparseSet split_ws;

    SparseSet touched;
    SparseSet fallen;

    int da_index;
    NL_Table da_map[3];
    DynArray(CProp_Partition*) da_array[3];
};

static void push_cprop(TB_Function* f, CProp* cprop, TB_Node* n) {
    CProp_Node* node = cprop->nodes[n->gvn];
    if (!node->in_cprop) {
        node->in_cprop = true;

        // add to cprop worklist
        node->prev_cprop = NULL;
        node->next_cprop = node->partition->cprop;
        node->partition->cprop = node;
        node->partition->cprop_count += 1;

        sparse_set_put(&cprop->cprop_ws, node->partition->id);
    }
}

static void push_cprop_users(TB_Function* f, CProp* cprop, TB_Node* n) {
    // push affected users
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (is_proj(un) || un->type == TB_CALLGRAPH) {
            continue;
        }

        if (cfg_is_region(un)) {
            FOR_USERS(phi, un) if (USERN(phi)->type == TB_PHI) {
                push_cprop(f, cprop, USERN(phi));
            }
        }
        push_cprop(f, cprop, un);
    }
}

static bool cprop_what_touched(TB_Function* f, CProp* cprop, CProp_Node* node) {
    return node->in_touched;
}

static bool cprop_what_fallen(TB_Function* f, CProp* cprop, CProp_Node* node) {
    return sparse_set_test(&cprop->fallen, node->n->gvn);
}

static CProp_Partition* cprop_split(TB_Function* f, CProp* cprop, CProp_Partition* z, bool what(TB_Function* f, CProp* cprop, CProp_Node* node)) {
    // create new partition Z'
    CProp_Partition* new_z = tb_arena_alloc(&f->tmp_arena, sizeof(CProp_Partition));
    *new_z = (CProp_Partition){
        .id = aarray_length(cprop->partitions),
        .type = z->type,
        .input_count = z->input_count,
    };
    aarray_push(cprop->partitions, new_z);

    // split Z from the touched part of Z
    CProp_Node* old_list = NULL;
    for (CProp_Node* node = z->members; node;) {
        CProp_Node* next = node->next;

        // followers should be moved to whatever partition to leader went to
        CProp_Node* leader = node;
        while (leader->leader) {
            leader = leader->leader;
        }

        if (what(f, cprop, leader)) {
            node->partition = new_z;
            node->next = new_z->members;
            new_z->members = node;

            if (node->leader) {
                z->follower_count -= 1;
                new_z->follower_count += 1;
            }

            if (node->in_cprop) {
                // doubly-linked list removal
                CProp_Node* prev_cprop = node->prev_cprop;
                if (prev_cprop) {
                    prev_cprop->next_cprop = node->next_cprop;
                }
                node->prev_cprop = prev_cprop;
                if (z->cprop == node) {
                    z->cprop = node->next_cprop;
                }
                z->cprop_count -= 1;

                // add to new cprop worklist
                node->prev_cprop = NULL;
                node->next_cprop = new_z->cprop;
                new_z->cprop = node;
                new_z->cprop_count += 1;

                // update global cprop WS
                sparse_set_put(&cprop->cprop_ws, new_z->id);
            }

            z->member_count -= 1;
            new_z->member_count += 1;
            TB_ASSERT(z->member_count != 0);
        } else {
            node->next = old_list;
            old_list = node;
        }
        node = next;
    }
    z->members = old_list;

    if (sparse_set_test(&cprop->split_ws, z->id)) {
        // unprocessed? so is the split then
        sparse_set_put(&cprop->split_ws, new_z->id);
    } else {
        CProp_Partition* smaller = new_z->member_count < z->member_count ? new_z : z;
        sparse_set_put(&cprop->split_ws, smaller->id);
    }
    return new_z;
}

static void* cprop_what_type(TB_Function* f, CProp* cprop, CProp_Node* node) {
    // return lattice_remove_widen(f, latuni_get(f, node->n));
    return latuni_get(f, node->n);
}

static void* cprop_what_opcode(TB_Function* f, CProp* cprop, CProp_Node* node) {
    return node;
}

static void* cprop_what_cfg(TB_Function* f, CProp* cprop, CProp_Node* node) {
    static int dummy;

    if (cfg_is_control(node->n)) {
        return node->n;
    }
    return &dummy;
}

static void* cprop_what_partition(TB_Function* f, CProp* cprop, CProp_Node* node) {
    // just needs to be a value that old_partition can't return
    static int dummy;

    TB_Node* n  = node->n;
    TB_ASSERT(cprop->da_index < n->input_count);
    TB_Node* in = n->inputs[cprop->da_index];
    if (in == NULL) {
        return &dummy;
    }

    CProp_Partition* p = cprop->nodes[in->gvn]->old_partition;
    return p ? p : cprop->nodes[in->gvn]->partition;
}

static uint32_t cprop_opcode_hash(void* a) {
    TB_Node* n = ((CProp_Node*) a)->n;
    size_t extra = extra_bytes(n);
    uint32_t h = n->type + n->dt.raw + extra;

    // locals can't be put into the GVN table
    if (n->type == TB_LOCAL) {
        h += n->gvn;
    } else {
        // fnv1a the extra space
        uint32_t* extra_arr = (uint32_t*) n->extra;
        FOR_N(i, 0, extra / 4) {
            h += extra_arr[i];
        }

        FOR_N(i, extra & ~0x7, extra) {
            h += n->extra[i];
        }
    }

    // fib hashing amirite
    return ((uint64_t) h * 11400714819323198485llu) >> 32llu;
}

static bool cprop_opcode_compare(void* a, void* b) {
    TB_Node* x = ((CProp_Node*) a)->n;
    TB_Node* y = ((CProp_Node*) b)->n;

    // early outs
    if (x->type != y->type || x->dt.raw != y->dt.raw || x->input_count != y->input_count) {
        return false;
    }

    size_t extra = extra_bytes(x);
    return extra == 0 || memcmp(x->extra, y->extra, extra) == 0;
}

typedef void* SplitByWhat(TB_Function* f, CProp* cprop, CProp_Node* node);
static void cprop_split_by_what(TB_Function* f, CProp* cprop, CProp_Partition* root, SplitByWhat* what, int depth) {
    nl_table_clear(&cprop->da_map[depth]);
    dyn_array_clear(cprop->da_array[depth]);

    size_t og_member_count = root->member_count;
    bool recycled_root = false;
    CProp_Node* new_root_list = NULL;
    for (CProp_Node* node = root->members; node;) {
        CProp_Node* next = node->next;

        // followers should be moved to whatever partition to leader went to
        CProp_Node* leader = node;
        while (leader->leader) {
            leader = leader->leader;
        }

        void* key = what(f, cprop, leader);
        void* val = leader == key ? nl_table_get2(&cprop->da_map[depth], key, cprop_opcode_hash, cprop_opcode_compare) : nl_table_get(&cprop->da_map[depth], key);

        CProp_Partition* p;
        if (val == NULL) {
            // new partition (the first one will just stay in the original partition)
            if (recycled_root) {
                p = tb_arena_alloc(&f->tmp_arena, sizeof(CProp_Partition));
                *p = (CProp_Partition){
                    .id = aarray_length(cprop->partitions),
                    .type = latuni_get(f, leader->n),
                    .input_count = leader->n->input_count,
                };
                aarray_push(cprop->partitions, p);
                dyn_array_put(cprop->da_array[depth], p);
            } else {
                p = root;
                p->type = latuni_get(f, leader->n);
                p->input_count = leader->n->input_count;
                recycled_root = true;
            }

            if (leader == key) {
                nl_table_put2(&cprop->da_map[depth], key, p, cprop_opcode_hash, cprop_opcode_compare);
            } else {
                nl_table_put(&cprop->da_map[depth], key, p);
            }
        } else {
            p = val;
        }

        if (p != root) {
            node->partition = p;
            node->next = p->members;
            p->members = node;

            if (node->leader) {
                root->follower_count -= 1;
                p->follower_count += 1;
            }

            root->member_count -= 1;
            p->member_count += 1;
        } else {
            node->next = new_root_list;
            new_root_list = node;
        }
        node = next;
    }

    root->members = new_root_list;
    TB_ASSERT(root->member_count != 0);

    size_t c = 0;
    // place onto worklist
    dyn_array_for(i, cprop->da_array[depth]) {
        CProp_Partition* p = cprop->da_array[depth][i];
        if (p == root) { continue; }

        // place onto worklist "as if" we split them
        // in the order of the hash table. this should
        // maintain the invariant of only placing nodes
        // log(n) times on the worklist
        og_member_count -= p->member_count;
        if (sparse_set_test(&cprop->split_ws, root->id)) {
            // unprocessed? so is the split then
            sparse_set_put(&cprop->split_ws, p->id);
            c += p->member_count;
        } else {
            CProp_Partition* smaller = p->member_count < og_member_count ? p : root;
            sparse_set_put(&cprop->split_ws, smaller->id);
            c += smaller->member_count;
        }
    }
    // printf("     PUSH %zu nodes\n", c);
}

// performs SCCP, splits partitions such that all nodes in the same
// partition share a type.
static void cprop_propagate(TB_Function* f, CProp* cprop) {
    while (aarray_length(cprop->cprop_ws.stack) > 0) {
        int p_idx = sparse_set_pop(&cprop->cprop_ws);
        CProp_Partition* p = cprop->partitions[p_idx];

        #if TB_OPTDEBUG_SCCP
        printf("PARTITION%d: PROPAGATE\n", p_idx);
        #endif

        Lattice* old_p_type = p->type;
        CProp_Node* old_opcode = p->members;

        bool progress = false;
        while (p->cprop_count > 0) {
            // pop a cprop node from the worklist
            CProp_Node* node = p->cprop;
            p->cprop = node->next_cprop;
            if (p->cprop) {
                p->cprop->prev_cprop = NULL;
            }
            p->cprop_count -= 1;

            TB_ASSERT(node->partition == p);
            TB_ASSERT(node->in_cprop);
            node->in_cprop = false;

            TB_Node* n = node->n;
            DO_IF(TB_OPTDEBUG_SCCP)(printf("     t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

            // Follower => Leader transition
            if (node->leader) {
                TB_Node* leader = identity(f, n);
                if (n == leader) {
                    // we'll need to split it if we've lost our special constant and aren't
                    // the same opcode as the rest of the partition.
                    if (!cprop_opcode_compare(node, old_opcode)) {
                        sparse_set_put(&cprop->fallen, n->gvn);
                    }

                    p->follower_count -= 1;
                    node->leader = NULL;
                } else {
                    DO_IF(TB_OPTDEBUG_SCCP)(printf(" => \x1b[33m"), tb_print_dumb_node(NULL, leader), printf("\x1b[0m"));
                }
            }

            // SCCP transfer function, we check monotonicity
            // TB_OPTDEBUG(STATS)(uint64_t start = cuik_time_in_nanos());
            Lattice* old_type = latuni_get(f, n);
            Lattice* new_type = value_of(f, n);
            // TB_OPTDEBUG(STATS)(n->type < TB_NODE_TYPE_MAX ? (f->stats.cprop_t[n->type] += (cuik_time_in_nanos() - start), 0) : 0);

            DO_IF(TB_OPTDEBUG_SCCP)(printf(" => \x1b[93m"), print_lattice(new_type), printf("\x1b[0m\n"));
            if (old_type != new_type) {
                #ifndef NDEBUG
                // validate int
                if (new_type->tag == LATTICE_INT) {
                    TB_ASSERT_MSG((new_type->_int.known_ones & new_type->_int.known_zeros) == 0, "overlapping known bits?");
                }

                Lattice* glb = lattice_meet(f, old_type, new_type);
                if (glb != new_type) {
                    TB_OPTDEBUG(SCCP)(printf("\n\nFORWARD PROGRESS ASSERT!\n"));
                    TB_OPTDEBUG(SCCP)(printf("  "), print_lattice(old_type), printf("  =//=>  "), print_lattice(new_type), printf(", MEET: "), print_lattice(glb), printf("\n\n"));
                    TB_ASSERT_MSG(0, "forward progress assert!");
                }
                #endif

                latuni_set(f, n, new_type);
                sparse_set_put(&cprop->fallen, n->gvn);
                progress = true;

                if (n->dt.type == TB_TAG_TUPLE) {
                    TB_ASSERT(new_type->tag == LATTICE_TUPLE);
                    FOR_USERS(u, n) {
                        if (is_proj(USERN(u))) {
                            int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
                            TB_ASSERT(index < new_type->_elem_count);

                            if (latuni_set_progress(f, USERN(u), new_type->elems[index])) {
                                push_cprop_users(f, cprop, USERN(u));
                                sparse_set_put(&cprop->fallen, USERN(u)->gvn);
                            }
                        }
                    }
                }
                push_cprop_users(f, cprop, n);
            }
        }

        // only fallen nodes can actually be in the wrong partition and need to re-eval
        // for splitting, we first split that group then handle the more precise splitting.
        CProp_Partition* y = p;
        size_t fallen_cnt = aarray_length(cprop->fallen.stack);
        if (fallen_cnt > 0 && fallen_cnt < p->member_count) {
            y = cprop_split(f, cprop, p, cprop_what_fallen);
            // TB_ASSERT(y->member_count - y->follower_count == fallen_cnt);

            /*printf("Split Z (%d vs %d):\n", p->id, y->id);
            cprop_dump(f, p);
            cprop_dump(f, y);*/
        } else {
            // update partition type based on the first member
            y->type = latuni_get(f, y->members->n);
        }
        sparse_set_clear(&cprop->fallen);

        // Leader => Follower transition
        if (progress && lattice_is_top_or_constant(old_p_type)) {
            sparse_set_put(&cprop->split_ws, y->id);
            TB_OPTDEBUG(SCCP)(printf("LEADER => FOLLOWERS\n"));

            for (CProp_Node* node = y->members; node; node = node->next) {
                TB_Node* n = node->n;
                Lattice* old_type = latuni_get(f, n);
                if (node->leader == NULL && (old_type == &TOP_IN_THE_SKY || !lattice_is_top_or_constant(old_type))) {
                    TB_Node* leader = identity(f, n);
                    if (leader && n != leader) {
                        TB_OPTDEBUG(SCCP)(printf("     t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n), printf(" => "), tb_print_dumb_node(NULL, leader), printf(" (IDENTITY)\n"));

                        y->follower_count += 1;
                        node->leader = cprop->nodes[leader->gvn];
                    }
                }
            }
        }

        cuikperf_region_start("splits", NULL);
        if (y->member_count > 1) {
            /*printf("Split by type:\n");
            cprop_dump(f, y);
            printf("VVV\n");*/

            cprop_split_by_what(f, cprop, y, cprop_what_type, 0);
            dyn_array_for(i, cprop->da_array[0]) {
                CProp_Partition* p = cprop->da_array[0][i];
                // we don't split constants or TOP partitions
                if (lattice_is_top_or_constant(p->type)) { continue; }

                cprop_split_by_what(f, cprop, p, cprop_what_opcode, 1);
                dyn_array_for(j, cprop->da_array[1]) {
                    CProp_Partition* q = cprop->da_array[1][j];

                    // cache the old partitions, this is necessary for the next layer of splitting
                    for (CProp_Node* node = q->members; node; node = node->next) {
                        node->old_partition = node->partition;

                        // misusing the sparse set because i don't wanna
                        // fill in the visited set for something like this
                        // but i also don't wanna allocate yet another
                        // dynamic array.
                        aarray_push(cprop->fallen.stack, node->n->gvn);
                    }

                    // every member of the partition should have the same input count
                    FOR_N(i, 0, q->input_count) {
                        cprop->da_index = i;
                        cprop_split_by_what(f, cprop, q, cprop_what_partition, 2);
                    }

                    aarray_for(i, cprop->fallen.stack) {
                        CProp_Node* node = cprop->nodes[cprop->fallen.stack[i]];
                        node->old_partition = NULL;
                    }
                    aarray_clear(cprop->fallen.stack);
                }
            }
            // printf("\n\n");
        }
        cuikperf_region_end();
    }
}

static void cprop_cause_splits(TB_Function* f, CProp* cprop) {
    int p_idx = sparse_set_pop(&cprop->split_ws);
    CProp_Partition* p = cprop->partitions[p_idx];

    #if TB_OPTDEBUG_SCCP
    printf("PARTITION%d: CAUSE SPLITS\n", p_idx);
    cprop_dump(f, p);
    #endif

    int input_count = 0;
    for (CProp_Node* node = p->members; node; node = node->next) {
        FOR_USERS(u, node->n) {
            int ui = USERI(u);
            input_count = TB_MAX(input_count, ui+1);
        }
    }

    #if 0
    TB_Node* p_node = NULL;
    for (CProp_Node* node = p->members; node; node = node->next) {
        if (node->leader == NULL) { p_node = node->n; break; }
    }

    // the opcode of all leader nodes in a partition is the same, if it's a
    // CFG tuple node like call, it'll never discover new splits.
    if (cfg_is_control(p_node)) {
        return;
    }
    #endif

    FOR_N(i, 0, input_count) {
        // partitions is used as the touched set
        cuikperf_region_start("iter", NULL);
        sparse_set_clear(&cprop->touched);

        for (CProp_Node* node = p->members; node; node = node->next) {
            TB_Node* x = node->n;

            TB_OPTDEBUG(STATS)(uint64_t start = cuik_time_in_nanos());
            FOR_USERS(u, x) {
                TB_Node* y = USERN(u);
                CProp_Node* y_node = cprop->nodes[y->gvn];
                if (USERI(u) != i) { continue; }

                // don't follow follower edges
                if (y_node->leader) {
                    push_cprop(f, cprop, y);
                    continue;
                }

                bool split = true;
                if (lattice_is_top_or_constant(y_node->partition->type)) {
                    // TOP nor constants should split based on their input congruence,
                    // if they're constant then clearly those things don't matter at the
                    // moment.
                    split = false;

                    // SUB and COMPARE have congruence based folding rules, to trigger those
                    // we need visit the main node in CPROP when one of its inputs is visited
                    // here in SPLIT.
                    if (y->type == TB_SUB || (y->type >= TB_CMP_EQ && y->type <= TB_CMP_FLE)) {
                        push_cprop(f, cprop, y);
                    }
                } else if (y->type == TB_PHI && i > 0) {
                    TB_Node* r = y->inputs[0];
                    Lattice* ctrl = latuni_get(f, r->inputs[i - 1]);

                    // don't split based on dead phi edges
                    if (lattice_meet(f, ctrl, &LIVE_IN_THE_SKY) != ctrl) {
                        split = false;
                    }
                }

                if (split) {
                    sparse_set_put(&cprop->touched, y_node->partition->id);

                    // Add y to the set y.partition.touched
                    if (!y_node->in_touched) {
                        y_node->in_touched = true;

                        // add to touched list
                        y_node->next_touched = y_node->partition->touched;
                        y_node->partition->touched = y_node;
                        y_node->partition->touched_count += 1;
                    }
                }
            }
            TB_OPTDEBUG(STATS)(x->type < TB_NODE_TYPE_MAX ? (f->stats.cprop_t[x->type] += x->user_count, 0) : 0); // (cuik_time_in_nanos() - start), 0) : 0);
        }
        cuikperf_region_end();
        // printf("\n\n\n");

        cuikperf_region_start("touch", NULL);
        aarray_for(j, cprop->touched.stack) {
            CProp_Partition* z = cprop->partitions[cprop->touched.stack[j]];
            if (z->member_count - z->follower_count != z->touched_count) {
                TB_ASSERT(z->touched_count > 0);

                #if TB_OPTDEBUG_SCCP
                printf("  Touched: ");
                for (CProp_Node* node = z->touched; node; node = node->next_touched) {
                    printf("%%%u ", node->n->gvn);
                }
                printf("\n");
                #endif

                CProp_Partition* new_z = cprop_split(f, cprop, z, cprop_what_touched);
                // TB_ASSERT(new_z->member_count - z->follower_count >= z->touched_count);

                // printf("Split Z (%d vs %d):\n", z->id, new_z->id);
                // cprop_dump(f, z);
                // cprop_dump(f, new_z);
            }

            // clear Z's touched
            for (CProp_Node* node = z->touched; node; node = node->next_touched) {
                node->in_touched = false;
            }
            z->touched_count = 0;
            z->touched = NULL;
        }
        cuikperf_region_end();
    }
}

CProp tb_opt_cprop_init(TB_Function* f) {
    TB_ASSERT(worklist_count(f->worklist) == 0);

    TB_OPTDEBUG(SCCP)(tb_print(f));

    // disjoint-set for the eqclasses
    CProp cprop;
    cprop.nodes = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(CProp_Node*));
    FOR_N(i, 0, f->node_count) { cprop.nodes[i] = NULL; }

    FOR_N(i, 0, sizeof(cprop.da_map) / sizeof(cprop.da_map[0])) {
        cprop.da_map[i] = nl_table_alloc(20);
    }
    FOR_N(i, 0, sizeof(cprop.da_array) / sizeof(cprop.da_array[0])) {
        cprop.da_array[i] = dyn_array_create(CProp_Partition*, 32);
    }
    cprop.partitions = aarray_create(&f->tmp_arena, CProp_Partition*, 32);

    // put everyone on the worklist
    TB_Worklist* ws = f->worklist;
    worklist_push(ws, f->root_node);

    // place everyone into the TOP partition
    CProp_Partition* top_p = tb_arena_alloc(&f->tmp_arena, sizeof(CProp_Partition));
    *top_p = (CProp_Partition){
        .id = aarray_length(cprop.partitions),
        .type = &TOP_IN_THE_SKY,
    };
    aarray_push(cprop.partitions, top_p);

    // place everyone into a partition based on opcode
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = ws->items[i];
        FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }

        CProp_Node* node = tb_arena_alloc(&f->tmp_arena, sizeof(CProp_Node));
        *node = (CProp_Node){
            .next = top_p->members,
            .partition = top_p,
            .n = n
        };
        top_p->members = node;
        top_p->member_count += 1;
        cprop.nodes[n->gvn] = node;
    }

    cprop.cprop_ws = sparse_set_alloc(&f->tmp_arena, f->node_count);
    cprop.split_ws = sparse_set_alloc(&f->tmp_arena, f->node_count);

    cprop.touched = sparse_set_alloc(&f->tmp_arena, f->node_count);
    cprop.fallen = sparse_set_alloc(&f->tmp_arena, f->node_count);

    // init SCCP stuff
    {
        alloc_types(f);
        if (UNLIKELY(f->node_count+1 >= f->type_cap)) {
            latuni_grow(f, f->node_count+1);
        }
        //   reset all types into TOP
        FOR_N(i, 0, f->node_count) { f->types[i] = &TOP_IN_THE_SKY; }
        //   anything unallocated should stay as NULL tho
        FOR_N(i, f->node_count, f->type_cap) { f->types[i] = NULL; }
        // callgraph is bookkeeping so it isn't considered in the CPROP
        TB_ASSERT(f->root_node->inputs[0]->type == TB_CALLGRAPH);
        f->types[f->root_node->inputs[0]->gvn] = &BOT_IN_THE_SKY;
    }

    #if 0
    cprop_split_by_what(f, &cprop, cprop.partitions[0], cprop_what_cfg, 0);
    printf("\n\n\n");
    aarray_for(i, cprop.partitions) {
        cprop_dump(f, cprop.partitions[i]);
    }
    __debugbreak();
    #endif

    // place ROOT as starting point of cprop work
    {
        TB_ASSERT(f->root_node->gvn == 0);

        sparse_set_put(&cprop.cprop_ws, cprop.nodes[0]->partition->id);
        cprop.nodes[0]->in_cprop = true;
        cprop.nodes[0]->partition->cprop = cprop.nodes[0];
        cprop.nodes[0]->partition->cprop_count = 1;
    }

    f->gcf_nodes = cprop.nodes;
    return cprop;
}

void tb_opt_cprop_deinit(TB_Function* f, CProp* cprop) {
    FOR_N(i, 0, sizeof(cprop->da_map) / sizeof(cprop->da_map[0])) {
        nl_table_free(cprop->da_map[i]);
    }
    FOR_N(i, 0, sizeof(cprop->da_array) / sizeof(cprop->da_array[0])) {
        dyn_array_destroy(cprop->da_array[i]);
    }
}

// if IPSCCP is enabled, we check for changes in the
static TB_Function* static_call_site(TB_Node* n);
void tb_opt_cprop_analyze(TB_Function* f, CProp* cprop, bool ipsccp) {
    #if TB_OPTDEBUG_STATS
    // our rate is measured against our time complexity "n * log(n)"
    uint64_t start = cuik_time_in_nanos();
    uint64_t log2_n = 64 - tb_clz64(f->node_count);
    f->stats.solver_n = f->node_count;
    f->stats.solver_big_o = f->node_count * log2_n;
    #endif

    if (ipsccp) {
        // each of the function calls will be placed on the CPROP worklist again,
        // this could be costly but probably not...
        TB_Node* callgraph = f->root_node->inputs[0];
        cuikperf_region_start("inflate", NULL);
        for (size_t i = 1; i < callgraph->input_count;) {
            TB_Node* call = callgraph->inputs[i++];
            TB_Function* target = static_call_site(call);
            if (target) {
                push_cprop(f, cprop, call);
            }
        }
        cuikperf_region_end();
    }

    Lattice* old_args = f->types[0];
    do {
        if (ipsccp && f->ipsccp_args != old_args) {
            push_cprop(f, cprop, f->root_node);
            old_args = f->ipsccp_args;
        }

        cuikperf_region_start("cprop", NULL);
        cprop_propagate(f, cprop);
        cuikperf_region_end();

        if (aarray_length(cprop->split_ws.stack) > 0) {
            cuikperf_region_start("cause_splits", NULL);
            cprop_cause_splits(f, cprop);
            cuikperf_region_end();
        }
    } while (aarray_length(cprop->split_ws.stack) > 0 || aarray_length(cprop->cprop_ws.stack) > 0);

    if (ipsccp) {
        cuikperf_region_start("EA", NULL);
        FOR_USERS(u, f->root_node) {
            if (USERN(u)->type != TB_SYMBOL) { continue; }
            TB_Node* target = USERN(u);
            TB_Symbol* sym = TB_NODE_GET_EXTRA_T(target, TB_NodeSymbol)->sym;
            if (sym->tag != TB_SYMBOL_FUNCTION) { continue; }

            // if there's any users of the symbol which aren't function
            // calls, we consider the symbol as escaped.
            bool escape = false;
            FOR_USERS(u2, target) {
                if (USERN(u2)->type != TB_CALL || USERI(u2) != 2) {
                    escape = true;
                    break;
                }
            }

            if (escape) {
                TB_Function* callee = (TB_Function*) sym;

                // if we moved the type, then push it
                Lattice* new_type = lattice_tuple_from_node(f, callee->root_node);
                Lattice* old_type = atomic_exchange(&callee->ipsccp_args, new_type);
                if (old_type != new_type) {
                    #if 0
                    mtx_lock(&aaa);
                    printf("%s: ESCAPE!!!\n", sym->name);
                    mtx_unlock(&aaa);
                    #endif

                    push_ipsccp_job(f->super.module, callee);
                }
            }
        }
        cuikperf_region_end();
    }

    #if TB_OPTDEBUG_STATS
    f->stats.solver_time += (cuik_time_in_nanos() - start);
    #endif

    #if 0
    printf("\n\n\n");
    aarray_for(i, cprop->partitions) {
        cprop_dump(f, cprop->partitions[i]);
    }
    tb_print_dumb_fancy(f);
    tb_print(f);
    __debugbreak();
    #endif
}

// rewrite the graph according to the maximal fixed point
// Pass 2: ok replace with constants now
//   fills up the entire worklist again
int tb_opt_cprop_rewrite(TB_Function* f) {
    TB_Worklist* ws = f->worklist;
    CProp_Node** nodes = f->gcf_nodes;
    TB_ASSERT(nodes);

    int rewrites = 0;
    size_t node_barrier = f->node_count;
    for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
        TB_Node* n = f->worklist->items[i];
        if (n->gvn >= node_barrier){
            continue;
        }

        TB_OPTDEBUG(SCCP)(printf("COMBO t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

        // subsume into leader of partition
        CProp_Node* node = nodes[n->gvn];
        while (node->leader) {
            node = node->leader;
        }

        TB_Node* leader = node->n;
        if (leader != n) {
            TB_OPTDEBUG(SCCP)(printf(" => "), tb_print_dumb_node(NULL, leader), printf(" (FOLLOW)\n"));

            rewrites++;
            subsume_node(f, n, leader);
            continue;
        }

        /*if (cfg_is_region(n)) {
            FOR_N(j, 0, n->input_count) {
                printf("N%d -> N%d\n", n->inputs[j]->gvn, n->gvn);
            }
        } else if (cfg_is_cproj(n)) {
            int index = TB_NODE_GET_EXTRA_T(n, TB_NodeProj)->index;
            printf("N%d -> N%d [label=\"%d\"]\n", n->inputs[0]->gvn, n->gvn, index);
        } else if (cfg_is_control(n)) {
            printf("N%d -> N%d\n", n->inputs[0]->gvn, n->gvn);
        }*/

        TB_Node* k = try_as_const(f, n, latuni_get(f, n));
        if (k != NULL) {
            TB_OPTDEBUG(STATS)(inc_nums(f->stats.opto_constants, n->type));
            TB_OPTDEBUG(SCCP)(printf(" => "), tb_print_dumb_node(NULL, k), printf(" (CONST)\n"));

            node->n = k;
            rewrites++;

            worklist_push(ws, k);
            subsume_node2(f, n, k);
            if (n->user_count == 0 && !is_proj(n)) {
                tb_kill_node(f, n);
            }
            continue;
        }

        TB_OPTDEBUG(SCCP)(printf("\n"));
    }

    tb_arena_clear(&f->tmp_arena);
    f->gcf_nodes = NULL;

    if (rewrites == 0) {
        worklist_clear(ws);
    }

    return rewrites;
}

