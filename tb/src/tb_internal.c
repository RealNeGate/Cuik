#define NL_MAP_IMPL
#define NL_HASH_MAP_INLINE
#include "tb_internal.h"

size_t tb_node_get_expected(TB_Node* n) {
    return sizeof(TB_Node) + (n->input_count * sizeof(TB_Node*)) + n->extra_count;
}

uint64_t* tb_transmute_to_int(TB_Function* f, TB_Label bb, TB_Node* n, int num_words) {
    TB_Node* new_n = tb_alloc_node(f, TB_INTEGER_CONST, n->dt, 0, sizeof(TB_NodeInt) + (num_words * sizeof(uint64_t)));
    TB_NodeInt* i = TB_NODE_GET_EXTRA(new_n);
    i->num_words = num_words;
    tb_insert_node(f, bb, n, new_n);

    tb_transmute_to_pass(n, new_n);
    return i->words;
}

void tb_transmute_to_poison(TB_Node* n) {
    n->type = TB_POISON;
    n->input_count = 1;
    n->extra_count = 0;
}

void tb_transmute_to_pass(TB_Node* n, TB_Node* point_to) {
    size_t size = sizeof(TB_Node) + (n->input_count * sizeof(TB_Node*)) + n->extra_count;
    (void) size;
    assert(size >= sizeof(TB_Node) + sizeof(TB_Node*));

    n->type = TB_PASS;
    n->input_count = 1;
    n->extra_count = 0;
    n->dt = point_to->dt;
    n->inputs[0] = point_to;
}

// IR ANALYSIS
void tb_function_calculate_use_count(const TB_Function* f, TB_UseCount* uc) {
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            TB_FOR_INPUT_IN_NODE(in, n) {
                ptrdiff_t search = nl_map_get(uc->use_count, *in);

                if (search >= 0) {
                    nl_map_put(uc->use_count, *in, uc->use_count[search].v + 1);
                } else {
                    nl_map_put(uc->use_count, *in, 0);
                }
            }
        }
    }
}

void tb_function_find_replace_reg(TB_Function* f, TB_Node* find, TB_Node* replace) {
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            TB_FOR_INPUT_IN_NODE(in, n) {
                if (*in == find) *in = replace;
            }
        }

        // if it matches find, then remove find from the basic block
        if (f->bbs[bb].start == find) {
            f->bbs[bb].start = f->bbs[bb].start->next;
        }

        if (f->bbs[bb].end == find) {
            f->bbs[bb].end = tb_node_get_previous(f, f->bbs[bb].end);
        }
    }
}

TB_Label tb_find_label_from_reg(TB_Function* f, TB_Node* target) {
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (target == r) return bb;
        }
    }

    return 0;
}

TB_Node* tb_node_get_previous(TB_Function* f, TB_Node* at) {
    TB_Node* prev = NULL;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(r, f, bb) {
            if (r == at) return prev;
            prev = r;
        }
    }

    return NULL;
}

TB_Label* tb_calculate_immediate_predeccessors(TB_Function* f, TB_TemporaryStorage* tls, TB_Label l, int* dst_count) {
    size_t count = 0;

    TB_Label* preds;
    if (tls) preds = tb_tls_push(tls, f->bb_count * sizeof(TB_Label));
    else preds = tb_platform_heap_alloc(f->bb_count * sizeof(TB_Label));

    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_Node* end = f->bbs[bb].end;
        if (end == NULL) continue;

        switch (end->type) {
            case TB_BRANCH: {
                TB_NodeBranch* br = TB_NODE_GET_EXTRA(end);
                FOREACH_N(i, 0, br->count) {
                    if (l == br->targets[i].value) preds[count++] = bb;
                }

                if (l == br->default_label) preds[count++] = bb;
                break;
            }

            // these blocks have no successors
            case TB_UNREACHABLE: case TB_RET: break;
            default: tb_todo();
        }
    }

    assert(count <= f->bb_count);

    // trim the fat
    if (tls) tb_tls_restore(tls, &preds[count]);
    else preds = tb_platform_heap_realloc(preds, count * sizeof(TB_Label));

    *dst_count = count;
    return preds;
}
