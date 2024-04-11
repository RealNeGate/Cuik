
static void b_set(uint32_t* arr, size_t i) { arr[i / 32] |= 1u << (i % 32); }
static void b_reset(uint32_t* arr, size_t i) { arr[i / 32] &= ~(1u << (i % 32)); }
static bool b_get(uint32_t* arr, size_t i) { return arr[i / 32] & (1u << (i % 32)); }

static void cycle_check(TB_Function* f, TB_Node* n, uint32_t* visited, uint32_t* progress) {
    assert(n->gvn < f->node_count);
    if (b_get(visited, n->gvn)) {
        if (b_get(progress, n->gvn)) {
            tb_print(f, f->tmp_arena);
            __debugbreak();
        }
    } else {
        b_set(visited, n->gvn);
        b_set(progress, n->gvn);
        FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
            TB_Node* in = n->inputs[i];
            if (in->type != TB_ROOT && in->type != TB_PHI && !cfg_is_region(in)) {
                cycle_check(f, in, visited, progress);
            }
        }
        b_reset(progress, n->gvn);
    }
}

void tb_verify(TB_Function* f, TB_Arena* tmp) {
    TB_ArenaSavepoint sp = tb_arena_save(tmp);

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    uint32_t* progress = tb_arena_alloc(tmp, ((f->node_count + 31) / 32) * sizeof(uint32_t));
    uint32_t* visited  = tb_arena_alloc(tmp, ((f->node_count + 31) / 32) * sizeof(uint32_t));
    memset(progress, 0, ((f->node_count + 31) / 32) * sizeof(uint32_t));

    // find all nodes
    worklist_push(&ws, f->root_node);
    for (size_t i = 0; i < dyn_array_length(ws.items); i++) {
        TB_Node* n = ws.items[i];
        FOR_USERS(u, n) { worklist_push(&ws, USERN(u)); }
    }

    // cycles are allowed for root, phis and regions
    for (size_t i = 1; i < dyn_array_length(ws.items); i++) {
        TB_Node* n = ws.items[i];
        if (n->type != TB_PHI && !cfg_is_region(n)) {
            memset(visited, 0, ((f->node_count + 31) / 32) * sizeof(uint32_t));
            cycle_check(f, n, visited, progress);
        }
    }

    worklist_free(&ws);
    tb_arena_restore(tmp, sp);
}
