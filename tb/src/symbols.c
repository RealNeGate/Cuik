#include "tb_internal.h"
#include <stdatomic.h>

TB_Symbol* tb_symbol_alloc(TB_Module* m, enum TB_SymbolTag tag, ptrdiff_t len, const char* name, size_t size) {
    // TODO(NeGate): probably wanna have a custom heap for the symbol table
    assert(tag != TB_SYMBOL_NONE);
    TB_Symbol* s = tb_platform_heap_alloc(size);
    memset(s, 0, size);

    s->tag = tag;
    s->name = tb__arena_strdup(m, len, name);
    s->module = m;
    s->next = NULL;

    tb_symbol_append(m, s);
    return s;
}

void tb_symbol_append(TB_Module* m, TB_Symbol* s) {
    enum TB_SymbolTag tag = s->tag;
    atomic_fetch_add(&m->symbol_count[tag], 1);

    TB_Symbol* old_top;
    do {
        old_top = atomic_load(&m->first_symbol_of_tag[tag]);
        s->next = old_top;
    } while (!atomic_compare_exchange_strong(&m->first_symbol_of_tag[tag], &old_top, s));
}

// NOTE(NeGate): the external symbols must be externally
// synchronized because threading is hard
//
// converts external into global
TB_Global* tb_extern_transmute(TB_External* e, TB_DebugType* dbg_type, TB_Linkage linkage) {
    TB_Module* m = e->super.module;

    TB_Symbol* prev = m->first_symbol_of_tag[TB_SYMBOL_EXTERNAL];
    while (prev != NULL) {
        if (prev->next == &e->super) {
            prev->next = e->super.next;
            goto good;
        }
        prev = prev->next;
    }
    abort();

    good:
    m->symbol_count[TB_SYMBOL_EXTERNAL] -= 1;

    // convert into global
    TB_Global* g = (TB_Global*) e;
    g->super.tag = TB_SYMBOL_GLOBAL;
    g->super.next = NULL;
    g->dbg_type = dbg_type;
    g->linkage = linkage;

    tb_symbol_append(m, &g->super);
    return g;
}

TB_API TB_Function* tb_symbol_as_function(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_FUNCTION ? (TB_Function*) s : NULL;
}

TB_API TB_External* tb_symbol_as_external(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_EXTERNAL ? (TB_External*) s : NULL;
}

TB_API TB_Global* tb_symbol_as_global(TB_Symbol* s) {
    return s && s->tag == TB_SYMBOL_GLOBAL ? (TB_Global*) s : NULL;
}
