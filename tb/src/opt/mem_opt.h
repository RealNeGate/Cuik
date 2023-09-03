// Certain aliasing optimizations technically count as peepholes lmao, these can get fancy
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
    /*if (n->inputs[1]->type != TB_STORE) return NULL;

    KnownPointer ld_ptr = known_pointer(n->inputs[2]);
    KnownPointer st_ptr = known_pointer(n->inputs[1]->inputs[2]);
    if (ld_ptr.base != st_ptr.base) return NULL;

    // it's probably not the fastest way to grab this value ngl...
    ICodeGen* cg = tb__find_code_generator(f->super.module);
    ld_ptr.offset *= cg->minimum_addressable_size;
    st_ptr.offset *= cg->minimum_addressable_size;

    size_t loaded_end = ld_ptr.offset + bits_in_data_type(cg->pointer_size, n->dt);
    size_t stored_end = st_ptr.offset + bits_in_data_type(cg->pointer_size, n->inputs[0]->inputs[2]->dt);

    // both bases match so if the effective ranges don't intersect, they don't alias.
    if (ld_ptr.offset <= stored_end && st_ptr.offset <= loaded_end) return NULL;

    set_input(p, n, n->inputs[1]->inputs[1], 1);
    return n;*/
    return NULL;
}

static TB_Node* identity_load(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // god i need a pattern matcher
    //   (load (store X A Y) A) => Y
    TB_Node *mem = n->inputs[1], *addr = n->inputs[2];
    if (mem->type == TB_STORE && mem->inputs[2] == addr &&
        n->dt.raw == mem->inputs[3]->dt.raw && is_same_align(n, mem)) {
        return mem->inputs[3];
    }

    return n;
}

static TB_Node* ideal_store(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    // god i need a pattern matcher
    //   (store (store X A Y) A Z) => (store X A Y Z)
    TB_Node *mem = n->inputs[1], *addr = n->inputs[2];
    TB_DataType dt = n->inputs[3]->dt;

    if (mem->type == TB_STORE && mem->inputs[2] == addr &&
        dt.raw == mem->inputs[3]->dt.raw && is_same_align(n, mem)) {
        // remove link to parent
        set_input(p, n, mem->inputs[1], 1);
        return n;
    }

    return NULL;
}

static TB_Node* ideal_memcpy(TB_Passes* restrict p, TB_Function* f, TB_Node* n) {
    return NULL;
}
