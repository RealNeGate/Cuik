// Certain load pimizations technically count as peepholes lmao, these can get fancy
// so the sliding window notion starts to break down but there's no global analysis and
// i can make them incremental technically so we'll go wit it.
typedef struct {
    TB_Node* base;
    int64_t offset;
} KnownPointer;

static KnownPointer known_pointer(TB_Node* n) {
    if (n->type == TB_MEMBER_ACCESS) {
        return (KnownPointer){ n->inputs[1], TB_NODE_GET_EXTRA_T(n, TB_NodeMember)->offset };
    } else {
        return (KnownPointer){ n, 0 };
    }
}

static TB_Node* ideal_load(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // if a load is control dependent on a store and it doesn't alias we can move the
    // dependency up a bit.
    if (n->inputs[0]->type != TB_STORE) return NULL;

    KnownPointer ld_ptr = known_pointer(n->inputs[1]);
    KnownPointer st_ptr = known_pointer(n->inputs[0]->inputs[1]);
    if (ld_ptr.base != st_ptr.base) return NULL;

    // it's probably not the fastest way to grab this value ngl...
    ICodeGen* cg = tb__find_code_generator(f->super.module);
    ld_ptr.offset *= cg->minimum_addressable_size;
    st_ptr.offset *= cg->minimum_addressable_size;

    size_t loaded_end = ld_ptr.offset + bits_in_data_type(cg->pointer_size, n->dt);
    size_t stored_end = st_ptr.offset + bits_in_data_type(cg->pointer_size, n->inputs[0]->inputs[2]->dt);

    // both bases match so if the effective ranges don't intersect, they don't alias.
    if (ld_ptr.offset <= stored_end && st_ptr.offset <= loaded_end) return NULL;

    set_input(p, n, n->inputs[0]->inputs[0], 0);
    return n;
}

static TB_Node* ideal_memset(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // small memsets will get replaced with stores
    if (n->inputs[2]->type != TB_INTEGER_CONST) return NULL;

    TB_NodeInt* i = TB_NODE_GET_EXTRA(n->inputs[2]);
    if (i->num_words > 1 || i->words[0] > 8) return NULL;

    set_input(p, n, NULL, 3);
    n->type = TB_STORE;
    n->dt = TB_TYPE_INTN(i->words[0] * 8);
    n->input_count = 3;

    return n;
}
