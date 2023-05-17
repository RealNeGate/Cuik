#include "tb_internal.h"

// something to note in this setup, we're not iterating the lists while the atomics are happening.
// they just need to not corrupt each other's values and first_symbol_of_tag isn't really used in
// the construction phase so it is technically built async from last_symbol_of_tag which is necessary.
TB_Symbol* tb_symbol_alloc(TB_Module* m, enum TB_SymbolTag tag, const char* name, size_t size) {
    // TODO(NeGate): probably wanna have a custom heap for the symbol table
    assert(tag != TB_SYMBOL_NONE);
    TB_Symbol* s = tb_platform_heap_alloc(size);
    memset(s, 0, size);

    s->tag = tag;
    s->name = tb__arena_strdup(m, name);
    s->module = m;
    s->next = NULL;

    tb_atomic_size_add(&m->symbol_count[tag], 1);
    tb_atomic_ptr_cmpxchg((void**) &m->first_symbol_of_tag[tag], NULL, s);

    TB_Symbol* last = tb_atomic_ptr_exchange((void**) &m->last_symbol_of_tag[tag], s);
    if (last) {
        // doesn't need to be atomic because every 'last' on any thread is going to be unique
        s->prev = last;
        last->next = s;
    }

    return s;
}

void tb_symbol_append(TB_Module* m, TB_Symbol* s) {
    enum TB_SymbolTag tag = s->tag;
    tb_atomic_size_add(&m->symbol_count[tag], 1);
    tb_atomic_ptr_cmpxchg((void**) &m->first_symbol_of_tag[tag], NULL, s);

    // atomic append (linked lists are kinda based ngl)
    TB_Symbol* last = tb_atomic_ptr_exchange((void**) &m->last_symbol_of_tag[tag], s);
    if (last) {
        s->prev = last;
        last->next = s;
    }
}

// converts external into global
TB_Global* tb_extern_transmute(TB_External* e, TB_DebugType* dbg_type, TB_Linkage linkage) {
    TB_Module* m = e->super.module;

    // remove from external list
    TB_Symbol* next = e->super.next;
    tb_atomic_ptr_cmpxchg((void**) &m->first_symbol_of_tag[TB_SYMBOL_EXTERNAL], &e->super, next);

    TB_Symbol* prev = e->super.prev;
    tb_atomic_ptr_cmpxchg((void**) &m->last_symbol_of_tag[TB_SYMBOL_EXTERNAL], &e->super, prev);

    // convert into global
    TB_Global* g = (TB_Global*) e;
    g->super.tag = TB_SYMBOL_GLOBAL;
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
