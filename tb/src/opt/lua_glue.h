#ifdef TB_USE_LUAJIT
#include "../tb_internal.h"

#ifdef _WIN32
#define LJ_EXPORT __attribute__((dllexport))
#else
#define LJ_EXPORT __attribute__((visibility("default")))
#endif

typedef struct {
    TB_Function* func;
    TB_Reg reg;
} TB_NodeRef;

LJ_EXPORT TB_NodeRef tb__first_node(TB_Function* f) {
    return (TB_NodeRef){ f, 1 };
}

LJ_EXPORT TB_NodeRef tb__next_node(TB_NodeRef r) {
    return (TB_NodeRef){ r.func, r.func->nodes[r.reg].next };
}

LJ_EXPORT TB_Node* tb__nodes(TB_Function* f) {
    return f->nodes;
}

LJ_EXPORT void tb__print_func(TB_Function* f) {
    tb_function_print(f, tb_default_print_callback, stdout, false);
}

LJ_EXPORT bool tb__is_iconst(TB_Function* f, TB_Reg r, uint64_t imm) {
    if (f->nodes[r].type == TB_INTEGER_CONST && f->nodes[r].integer.num_words == 1) {
        return (f->nodes[r].integer.single_word == imm);
    }

    return false;
}

LJ_EXPORT bool tb__is_izero(TB_Function* f, TB_Reg r) {
    TB_Node* n = &f->nodes[r];
    while (n->type == TB_PASS) {
        n = &f->nodes[n->pass.value];
    }

	if (n->type == TB_INTEGER_CONST) {
		if (n->integer.num_words == 1) {
			if (n->integer.single_word == 0) {
				return true;
			}
		} else {
			if (BigInt_is_zero(n->integer.num_words, n->integer.words)) {
				return true;
			}
		}
	}

	return false;
}
#endif