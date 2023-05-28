#include "tb_internal.h"

uint64_t* tb_transmute_to_int(TB_Function* f, TB_Node* n, int num_words) {
    TB_Node* new_n = tb_alloc_node(f, TB_INTEGER_CONST, n->dt, 0, sizeof(TB_NodeInt) + (num_words * sizeof(uint64_t)));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(new_n);
    i->num_words = num_words;

    tb_transmute_to_pass(n, new_n);
    return i->words;
}

void tb_transmute_to_poison(TB_Node* n) {
    n->type = TB_POISON;
    n->input_count = 1;
    n->extra_count = 0;
}

void tb_transmute_to_pass(TB_Node* n, TB_Node* point_to) {
    assert(n->input_count >= 1 && "TODO: Too small to transmute");

    n->type = TB_PASS;
    n->input_count = 1;
    n->extra_count = 0;
    n->dt = point_to->dt;
    n->inputs[0] = point_to;
}
