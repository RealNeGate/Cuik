// I'm trying to transition most people over since it's a bit nicer.

typedef struct {
    int pos; // where the stack head was before doing the code
    bool is_loop;

    // LOOP: [0] body, [1] break
    // IF:   [0] true, [1] false
    TB_Node* paths[2];

    // loop header
    TB_Node* header;
    TB_Node* join;

    // preserved names
    TB_ArenaSavepoint sp;
    int val_cnt;
    TB_Node* vals[];
} TB_GraphCtrl;

typedef struct {
} TB_GraphNames;

struct TB_GraphBuilder {
    TB_Function* f;
    TB_Arena* arena;

    // active control flow
    TB_Node *top_ctrl, *bot_ctrl;

    // ctrl stack
    int ctrl_cap, ctrl_cnt;
    TB_GraphCtrl** ctrl;

    // value stack
    int val_cap, val_cnt;
    TB_Node** vals;
};

static TB_Node* xfer_ctrl(TB_GraphBuilder* g, TB_Node* n) {
    TB_Node* prev = g->bot_ctrl;
    g->bot_ctrl = n;
    return prev;
}

static void push(TB_GraphBuilder* g, TB_Node* n) {
    if (g->val_cnt == g->val_cap) {
        g->val_cap *= 2;
        g->vals = tb_platform_heap_realloc(g->vals, g->val_cap * sizeof(TB_Node*));
    }
    g->vals[g->val_cnt++] = tb_pass_gvn_node(g->f, n);
}

static TB_Node* pop(TB_GraphBuilder* g) {
    assert(g->val_cnt > 0 && "nothing on the value stack!!!");
    return g->vals[--g->val_cnt];
}

TB_GraphBuilder* tb_builder_enter(TB_Function* f, TB_Arena* arena) {
    TB_GraphBuilder* g = tb_arena_alloc(arena, sizeof(TB_GraphBuilder));
    *g = (TB_GraphBuilder){ .f = f, .arena = arena, .ctrl_cap = 8, .val_cap = 32 };
    g->ctrl = tb_platform_heap_alloc(g->ctrl_cap * sizeof(TB_GraphCtrl));
    g->vals = tb_platform_heap_alloc(g->val_cap * sizeof(TB_Node*));

    assert(g->val_cap >= 1 + f->param_count);
    g->val_cnt = 1 + f->param_count;
    g->vals[0] = f->params[2];
    FOREACH_N(i, 0, f->param_count) {
        g->vals[1 + i] = f->params[3 + i];
    }

    return g;
}

void tb_builder_exit(TB_GraphBuilder* g) {
}

void tb_builder_uint(TB_GraphBuilder* g, TB_DataType dt, uint64_t x) {
    assert(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));
    if (dt.type == TB_INT && dt.data < 64) {
        uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);
        x &= mask;
    }

    TB_Node* n = tb_alloc_node(g->f, TB_INTEGER_CONST, dt, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    push(g, n);
}

void tb_builder_sint(TB_GraphBuilder* g, TB_DataType dt, int64_t x) {
    assert(TB_IS_POINTER_TYPE(dt) || TB_IS_INTEGER_TYPE(dt));

    TB_Node* n = tb_alloc_node(g->f, TB_INTEGER_CONST, dt, 1, sizeof(TB_NodeInt));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeInt, .value = x);
    push(g, n);
}

void tb_builder_float32(TB_GraphBuilder* g, float imm) {
    TB_Node* n = tb_alloc_node(g->f, TB_FLOAT32_CONST, TB_TYPE_F32, 1, sizeof(TB_NodeFloat32));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat32, .value = imm);
    push(g, n);
}

void tb_builder_float64(TB_GraphBuilder* g, double imm) {
    TB_Node* n = tb_alloc_node(g->f, TB_FLOAT64_CONST, TB_TYPE_F64, 1, sizeof(TB_NodeFloat64));
    set_input(g->f, n, g->f->root_node, 0);
    TB_NODE_SET_EXTRA(n, TB_NodeFloat64, .value = imm);
    push(g, n);
}

void tb_builder_string(TB_GraphBuilder* g, ptrdiff_t len, const char* str) {
    if (len < 0) len = strlen(str) + 1;

    TB_Function* f = g->f;
    TB_Global* dummy = tb_global_create(f->super.module, 0, NULL, NULL, TB_LINKAGE_PRIVATE);
    tb_global_set_storage(f->super.module, tb_module_get_rdata(f->super.module), dummy, len, 1, 1);

    char* dst = tb_global_add_region(f->super.module, dummy, 0, len);
    memcpy(dst, str, len);

    push(g, tb_inst_get_symbol_address(f, (TB_Symbol*) dummy));
}

void tb_builder_binop_int(TB_GraphBuilder* g, int type, TB_ArithmeticBehavior ab) {
    assert(type >= TB_AND && type <= TB_SMOD);
    TB_Node* b = pop(g);
    TB_Node* a = pop(g);

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, a->dt, 3, sizeof(TB_NodeBinopInt));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeBinopInt, .ab = ab);
    push(g, n);
}

void tb_builder_cmp(TB_GraphBuilder* g, int type, TB_DataType dt) {
    TB_Node* b = pop(g);
    TB_Node* a = pop(g);
    tb_assert(type >= TB_CMP_EQ && type <= TB_CMP_FLE, "'type' wasn't a comparison node type (see TB_NodeTypeEnum)");
    tb_assert(TB_DATA_TYPE_EQUALS(a->dt, b->dt), "datatype mismatch");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, type, TB_TYPE_BOOL, 3, sizeof(TB_NodeCompare));
    set_input(f, n, a, 1);
    set_input(f, n, b, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeCompare, .cmp_dt = a->dt);
    push(g, n);
}

void tb_builder_array(TB_GraphBuilder* g, int64_t stride) {
    TB_Node* index = pop(g);
    TB_Node* base  = pop(g);
    tb_assert(base->dt.type == TB_PTR,  "base on ARRAY must be an integer");
    tb_assert(index->dt.type == TB_INT, "index on ARRAY must be an integer");

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_ARRAY_ACCESS, TB_TYPE_PTR, 3, sizeof(TB_NodeArray));
    set_input(f, n, base,  1);
    set_input(f, n, index, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeArray, .stride = stride);
    push(g, n);
}

void tb_builder_member(TB_GraphBuilder* g, int64_t offset) {
    if (offset == 0) { return; }
    TB_Node* base = pop(g);

    TB_Function* f = g->f;
    TB_Node* n = tb_alloc_node(f, TB_MEMBER_ACCESS, TB_TYPE_PTR, 2, sizeof(TB_NodeMember));
    set_input(f, n, base, 1);
    TB_NODE_SET_EXTRA(n, TB_NodeMember, .offset = offset);
    push(g, n);
}

void tb_builder_load(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_DataType dt, TB_CharUnits alignment) {
    TB_Function* f = g->f;

    TB_Node* addr = pop(g);
    TB_Node* n = tb_alloc_node(f, TB_LOAD, dt, 3, sizeof(TB_NodeMemAccess));
    if (ctrl_dep) {
        set_input(f, n, g->bot_ctrl, 0);
    }
    set_input(f, n, g->vals[mem_var], 1);
    set_input(f, n, addr, 2);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment);

    push(g, n);
}

void tb_builder_store(TB_GraphBuilder* g, int mem_var, TB_CharUnits alignment) {
    TB_Function* f = g->f;

    TB_Node* val  = pop(g);
    TB_Node* addr = pop(g);

    TB_Node* n = tb_alloc_node(f, TB_STORE, TB_TYPE_MEMORY, 4, sizeof(TB_NodeMemAccess));
    set_input(f, n, f->trace.bot_ctrl, 0);
    set_input(f, n, g->vals[mem_var], 1);
    set_input(f, n, addr, 2);
    set_input(f, n, val, 3);
    TB_NODE_SET_EXTRA(n, TB_NodeMemAccess, .align = alignment);

    // assign ROOT memory
    g->vals[mem_var] = n;
}

int tb_builder_var(TB_GraphBuilder* g) {
    return g->val_cnt - 1;
}

void tb_builder_get_var(TB_GraphBuilder* g, int id) {
    assert(id < g->val_cnt);
    push(g, g->vals[id]);
}

void tb_builder_assign(TB_GraphBuilder* g, int id) {
    assert(id < g->val_cnt - 1);
    g->vals[id] = pop(g);
}

void tb_builder_if(TB_GraphBuilder* g) {
    TB_Function* f = g->f;

    TB_ArenaSavepoint sp = tb_arena_save(g->arena);
    TB_GraphCtrl* ctrl = tb_arena_alloc(g->arena, sizeof(TB_GraphCtrl) + g->val_cnt*sizeof(TB_Node*));

    // clone incoming state so we can diff & insert phis later
    ctrl->sp = sp;
    ctrl->val_cnt = g->val_cnt;
    FOREACH_N(i, 0, g->val_cnt) {
        ctrl->vals[i] = g->vals[i];
    }

    __debugbreak();

    assert(g->ctrl_cnt < g->ctrl_cap);
    g->ctrl[g->ctrl_cnt++] = ctrl;

    // generate branch op
    {
        TB_Node* cond = pop(g);
        TB_Node* n = tb_alloc_node(f, TB_BRANCH, TB_TYPE_TUPLE, 2, sizeof(TB_NodeBranch) + sizeof(int64_t));
        set_input(f, n, xfer_ctrl(g, NULL), 0);
        set_input(f, n, cond, 1);

        ctrl->paths[0] = tb__make_proj(f, TB_TYPE_CONTROL, n, 0);
        ctrl->paths[1] = tb__make_proj(f, TB_TYPE_CONTROL, n, 1);

        TB_NodeBranch* br = TB_NODE_GET_EXTRA(n);
        br->succ_count = 2;
        br->keys[0] = 0;
    }

    // add merge region (no phis yet, they'll be back)
    ctrl->header = NULL;
    ctrl->join = tb_alloc_node_dyn(f, TB_REGION, TB_TYPE_CONTROL, 0, 2, sizeof(TB_NodeRegion));
    TB_NODE_SET_EXTRA(ctrl->join, TB_NodeRegion, .freq = 1.0f);

    // goto the true path
    g->top_ctrl = g->bot_ctrl = ctrl->paths[0];
}

void tb_builder_else(TB_GraphBuilder* g) {
    assert(g->ctrl_cnt > 0);
    TB_GraphCtrl* ctrl = g->ctrl[g->ctrl_cnt - 1];

    if (g->bot_ctrl) {
        // goto true => join
        add_input_late(g->f, ctrl->join, g->bot_ctrl);
    }

    // goto the false path
    g->top_ctrl = g->bot_ctrl = ctrl->paths[1];
}

void tb_builder_endif(TB_GraphBuilder* g) {
    assert(g->ctrl_cnt > 0);
    TB_GraphCtrl* ctrl = g->ctrl[--g->ctrl_cnt];

    if (g->bot_ctrl) {
        // goto false => join
        add_input_late(g->f, ctrl->join, g->bot_ctrl);
    }

    // insert phis
    assert(g->val_cnt == ctrl->val_cnt && "paths on branch mismatch in outgoing variables?");
    TB_Node* join = ctrl->join;
    FOREACH_N(i, 0, g->val_cnt) {
        if (g->vals[i] != ctrl->vals[i]) {
            g->vals[i] = tb_inst_phi2(g->f, join, ctrl->vals[i], g->vals[i]);
        }
    }

    __debugbreak();

    g->top_ctrl = g->bot_ctrl = ctrl->join;
    tb_arena_restore(g->arena, ctrl->sp);
}

void tb_builder_ret(TB_GraphBuilder* g, int count) {
    assert(g->val_cnt >= count);
    tb_inst_ret(g->f, count, &g->vals[g->val_cnt - count]);
    g->val_cnt -= count;
}
