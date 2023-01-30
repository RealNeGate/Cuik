#include "tb_internal.h"

TB_Symbol* tb_symbol_alloc(TB_Module* m, enum TB_SymbolTag tag, const char* name, size_t size) {
    // TODO(NeGate): probably wanna have a custom heap for the symbol table
    assert(tag != TB_SYMBOL_NONE);
    TB_Symbol* s = tb_platform_heap_alloc(size);
    memset(s, 0, size);

    s->tag = tag;
    s->name = tb_platform_string_alloc(name);
    s->module = m;
    s->next = NULL;

    tb_atomic_size_add(&m->symbol_count[tag], 1);
    tb_atomic_ptr_cmpxchg((void**) &m->first_symbol_of_tag[tag], NULL, s);

    // atomic append (linked lists are kinda based ngl)
    TB_Symbol* last = tb_atomic_ptr_exchange((void**) &m->last_symbol_of_tag[tag], s);
    if (last) last->next = s;
    return s;
}

void tb_symbol_append(TB_Module* m, TB_Symbol* s) {
    enum TB_SymbolTag tag = s->tag;
    tb_atomic_size_add(&m->symbol_count[tag], 1);
    tb_atomic_ptr_cmpxchg((void**) &m->first_symbol_of_tag[tag], NULL, s);

    // atomic append (linked lists are kinda based ngl)
    TB_Symbol* last = tb_atomic_ptr_exchange((void**) &m->last_symbol_of_tag[tag], s);
    if (last) last->next = s;
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