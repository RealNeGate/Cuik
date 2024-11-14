// Certain aliasing optimizations technically count as peepholes lmao, these can get fancy
// so the sliding window notion starts to break down but there's no global analysis and
// i can make them incremental technically so we'll go wit it.
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

static TB_Node* ideal_return(TB_Function* f, TB_Node* n) {
    return NULL;
}

