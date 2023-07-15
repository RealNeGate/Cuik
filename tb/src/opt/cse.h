#include "../tb_internal.h"

uint32_t cse_hash(void* a) {
    TB_Node* n = a;

    uint32_t h = n->type
        + n->dt.raw
        + n->input_count
        + n->extra_count;

    // fib hashing amirite
    h = ((uint64_t) h * 11400714819323198485llu) >> 32llu;

    FOREACH_N(i, 0, n->input_count) {
        h ^= ((uintptr_t) n->inputs[i] * 11400714819323198485llu) >> 32llu;
    }

    // fnv1a the extra space
    FOREACH_N(i, 0, n->extra_count) {
        h = (n->extra[i] ^ h) * 0x01000193;
    }

    return h;
}

bool cse_compare(void* a, void* b) {
    TB_Node *x = a, *y = b;

    // early outs
    if (x->type != y->type ||
        x->input_count != y->input_count ||
        x->extra_count != y->extra_count ||
        x->dt.raw != y->dt.raw) {
        return false;
    }

    // match up inputs
    FOREACH_N(i, 0, x->input_count) {
        if (x->inputs[i] != y->inputs[i]) {
            return false;
        }
    }

    // If there's no extra data, we good
    if (x->extra_count == 0) return true;

    switch (x->type) {
        case TB_INTEGER_CONST: {
            TB_NodeInt* ai = TB_NODE_GET_EXTRA(x);
            TB_NodeInt* bi = TB_NODE_GET_EXTRA(y);

            return ai->num_words == bi->num_words && memcmp(ai->words, bi->words, ai->num_words * sizeof(BigInt_t)) == 0;
        }

        case TB_AND:
        case TB_OR:
        case TB_XOR:
        case TB_ADD:
        case TB_SUB:
        case TB_MUL:
        case TB_SDIV:
        case TB_UDIV:
        case TB_SMOD:
        case TB_UMOD:
        case TB_SHL:
        case TB_SHR:
        case TB_SAR:
        {
            TB_NodeBinopInt* ai = TB_NODE_GET_EXTRA(x);
            TB_NodeBinopInt* bi = TB_NODE_GET_EXTRA(y);
            return ai->ab == bi->ab;
        }

        // in practice, two volatiles accesses can't refer to the same
        // effect without there being malformed so we won't match against
        // that.
        case TB_LOAD: {
            TB_NodeMemAccess* am = TB_NODE_GET_EXTRA(x);
            TB_NodeMemAccess* bm = TB_NODE_GET_EXTRA(y);
            return am->align == bm->align;
        }

        case TB_MEMBER_ACCESS: {
            TB_NodeMember* aa = TB_NODE_GET_EXTRA(x);
            TB_NodeMember* bb = TB_NODE_GET_EXTRA(y);
            return aa->offset == bb->offset;
        }

        case TB_ARRAY_ACCESS: {
            TB_NodeArray* aa = TB_NODE_GET_EXTRA(x);
            TB_NodeArray* bb = TB_NODE_GET_EXTRA(y);
            return aa->stride == bb->stride;
        }

        case TB_GET_SYMBOL_ADDRESS: {
            TB_NodeSymbol* aa = TB_NODE_GET_EXTRA(x);
            TB_NodeSymbol* bb = TB_NODE_GET_EXTRA(y);
            return aa->sym == bb->sym;
        }

        default: return false;
    }
}
