#include <hashes.h>
#include <math.h>

static Lattice TOMBSTONE         = { 0 };

static Lattice TOP_IN_THE_SKY    = { LATTICE_TOP    };
static Lattice BOT_IN_THE_SKY    = { LATTICE_BOT    };
static Lattice CTRL_IN_THE_SKY   = { LATTICE_CTRL   };
static Lattice XNULL_IN_THE_SKY  = { LATTICE_XNULL  };
static Lattice NULL_IN_THE_SKY   = { LATTICE_NULL   };
static Lattice FLT32_IN_THE_SKY  = { LATTICE_FLT32  };
static Lattice FLT64_IN_THE_SKY  = { LATTICE_FLT64  };
static Lattice NAN32_IN_THE_SKY  = { LATTICE_NAN32  };
static Lattice NAN64_IN_THE_SKY  = { LATTICE_NAN64  };
static Lattice XNAN32_IN_THE_SKY = { LATTICE_XNAN32 };
static Lattice XNAN64_IN_THE_SKY = { LATTICE_XNAN64 };
static Lattice ANYMEM_IN_THE_SKY = { LATTICE_ANYMEM };
static Lattice ALLMEM_IN_THE_SKY = { LATTICE_ALLMEM };
static Lattice ALLPTR_IN_THE_SKY = { LATTICE_ALLPTR };
static Lattice ANYPTR_IN_THE_SKY = { LATTICE_ANYPTR };
static Lattice FALSE_IN_THE_SKY  = { LATTICE_INT, ._int = {  0, 0, 1, 0 } };
static Lattice TRUE_IN_THE_SKY   = { LATTICE_INT, ._int = {  1, 1, 0, 1 } };
static Lattice BOOL_IN_THE_SKY   = { LATTICE_INT, ._int = { -1, 0, 0, 0 } };

static Lattice* lattice_from_dt(TB_Function* f, TB_DataType dt);

static uint32_t lattice_hash(void* a) {
    Lattice* l = a;
    uint64_t h = l->tag + 1000;
    switch (l->tag) {
        case LATTICE_TUPLE: {
            // just add pointers together, what's the worst that could happen
            FOR_N(i, 0, l->_elem_count) {
                h += (uintptr_t) l->elems[i];
            }
            h += l->_elem_count;
            break;
        }

        case LATTICE_INT:
        h += l->_int.min,         h += l->_int.max;
        h += l->_int.known_zeros, h += l->_int.known_ones, h += l->_int.widen;
        break;

        case LATTICE_PTRCON:
        h += (uintptr_t) l->_ptr;
        break;

        case LATTICE_MEM_SLICE:
        h += l->_alias_n;
        FOR_N(i, 0, l->_alias_n) { h += l->alias[i]; }
        break;

        case LATTICE_FLTCON32: memcpy(&h, &l->_f32, sizeof(float));  break;
        case LATTICE_FLTCON64: memcpy(&h, &l->_f64, sizeof(double)); break;

        // no fields
        case LATTICE_BOT:   case LATTICE_TOP:
        case LATTICE_FLT32: case LATTICE_FLT64:
        case LATTICE_NAN32: case LATTICE_XNAN32:
        case LATTICE_NAN64: case LATTICE_XNAN64:
        case LATTICE_ALLPTR:case LATTICE_ANYPTR:
        case LATTICE_NULL:  case LATTICE_XNULL:
        case LATTICE_ALLMEM:case LATTICE_ANYMEM:
        case LATTICE_CTRL:
        break;

        default: tb_todo();
    }

    // fib hashing amirite
    return (h * 11400714819323198485llu) >> 32llu;
}

static bool lattice_cmp(void* a, void* b) {
    Lattice *aa = a, *bb = b;
    if (aa->tag != bb->tag) { return false; }

    switch (aa->tag) {
        case LATTICE_TUPLE:
        if (aa->_elem_count != bb->_elem_count) { return false; }
        FOR_N(i, 0, aa->_elem_count) {
            if (aa->elems[i] != bb->elems[i]) { return false; }
        }
        return true;

        case LATTICE_INT:
        return memcmp(&aa->_int, &bb->_int, sizeof(LatticeInt)) == 0;

        case LATTICE_PTRCON:
        return aa->_ptr == bb->_ptr;

        case LATTICE_FLTCON32:
        return memcmp(&aa->_f32, &bb->_f32, sizeof(float)) == 0;

        case LATTICE_FLTCON64:
        return memcmp(&aa->_f64, &bb->_f64, sizeof(double)) == 0;

        case LATTICE_MEM_SLICE:
        if (aa->_alias_n != bb->_alias_n) { return false; }
        FOR_N(i, 0, aa->_alias_n) {
            if (aa->alias[i] != bb->alias[i]) { return false; }
        }
        return true;

        // no fields
        default: return true;
    }
}

static uintptr_t lattice_raw_get_entry(LatticeTable* table, size_t i) {
    // either NULL or complete can go thru without waiting
    uintptr_t decision;
    while (decision = atomic_load(&table->data[i]), decision & 1) {
        // idk if i should do a nicer wait than a spin-lock
    }
    return decision;
}

static Lattice* lattice_raw_lookup(LatticeTable* table, uint32_t h, Lattice* l) {
    size_t mask = (1 << table->exp) - 1;
    size_t first = h & mask, i = first;
    do {
        uintptr_t entry = atomic_load(&table->data[i]);
        if (entry == 0) {
            return NULL;
        }

        Lattice* entry_p = (Lattice*) (entry & ~1ull);
        if (lattice_cmp(entry_p, l)) {
            if ((entry & 1) == 0) {
                // not reserved, yay
                return entry_p;
            }

            return (Lattice*) lattice_raw_get_entry(table, i);
        }

        i = (i + 1) & mask;
    } while (i != first);

    return NULL;
}

// only done during initialization
static void lattice_raw_insert(LatticeTable* table, Lattice* l) {
    uint32_t h = lattice_hash(l);
    size_t mask = (1 << table->exp) - 1;
    size_t first = h & mask, i = first;
    do {
        uintptr_t entry = atomic_load_explicit(&table->data[i], memory_order_relaxed);
        if (entry == 0) {
            atomic_store_explicit(&table->data[i], (uintptr_t) l, memory_order_relaxed);
            atomic_fetch_add_explicit(&table->count, 1, memory_order_relaxed);
            return;
        }

        assert(!lattice_cmp((Lattice*) entry, l));
        i = (i + 1) & mask;
    } while (i != first);

    tb_todo();
}

static Lattice* lattice_raw_intern(TB_Module* mod, Lattice* l, bool inner) {
    // cuikperf_region_start("intern", NULL);
    LatticeTable* latest = atomic_load(&mod->lattice.table);

    #if 0
    printf("TABLE:\n");
    for (LatticeTable* c = latest; c; c = c->prev) {
        uint32_t threshold = ((1 << c->exp) * 3) / 4;
        printf("{cnt=%d, threshold=%d}\n", c->count, threshold);
    }
    #endif

    // if there's previous tables, we'd ideally want them gone soon
    // let's pick 32 entries from the table and shuffle them up.
    LatticeTable* prev = atomic_load(&latest->prev);
    if (prev && !inner) {
        // printf("%p: MIGRATE\n", latest);

        size_t cap = 1ull << prev->exp;
        uint32_t done;

        // snatch up some number of items
        uint32_t old, new;
        do {
            old = atomic_load(&prev->moved);
            if (old == cap) {
                done = atomic_load(&prev->move_done);
                goto skip;
            }
            new = TB_MIN(old + 32, cap);
        } while (!atomic_compare_exchange_strong(&prev->moved, &(uint32_t){ old }, new));

        // printf("%p: %p: MOVING %u .. %u items (of %zu)\n", latest, prev, old, new, cap);
        FOR_N(i, old, new) {
            uintptr_t decision = lattice_raw_get_entry(prev, i);
            if (decision != 0) {
                lattice_raw_intern(mod, (Lattice*) decision, true);

                // now the data here is marked as stale, this isn't necessary btw
                // prev->data[i] = (uintptr_t) &TOMBSTONE;
            }
        }

        done = atomic_fetch_add(&prev->move_done, new - old);
        done += new - old;

        skip:;
        assert(done <= cap);
        if (done == cap) {
            // dettach now
            // printf("%p: prev is now %p\n", latest, prev->prev);
            latest->prev = prev->prev;

            // TODO(NeGate): we're leaking the memory rn
        }
    }

    uint32_t threshold = ((1 << latest->exp) * 3) / 4;
    if (latest->count >= threshold) CUIK_TIMED_BLOCK("new table") {
        // make resized table, we'll amortize the moves upward
        size_t new_cap = 1ull << (latest->exp + 1);
        // printf("TRY ALLOC! %u (exp=%zu, after %p)\n", latest->exp, new_cap, latest);

        LatticeTable* new_top = tb_platform_heap_alloc(sizeof(LatticeTable) + new_cap*sizeof(uintptr_t));
        *new_top = (LatticeTable){ .exp = latest->exp + 1 };
        memset(new_top->data, 0, new_cap*sizeof(uintptr_t));

        // printf("ALLOC! %p\n", new_top);

        // CAS latest -> new_table, if another thread wins the race we'll use its table
        LatticeTable* top = atomic_load_explicit(&mod->lattice.table, memory_order_relaxed);
        new_top->prev = top;
        if (!atomic_compare_exchange_strong(&mod->lattice.table, &top, new_top)) {
            assert(top != latest);
            latest = top;

            // printf("INSTA-FREE! %p\n", new_top);
            tb_platform_heap_free(new_top);
        } else {
            latest = new_top;
        }
    }

    uint32_t h = lattice_hash(l);
    uintptr_t reserved_form = (uintptr_t)l | 1;
    Lattice* result = NULL;
    for (;;) {
        size_t mask = (1 << latest->exp) - 1;
        size_t first = h & mask, i = first;

        do { // insertion
            uintptr_t entry = 0;

            // if we spot a NULL, the entry has never been in this table, that doesn't mean
            // it's not appeared already so we'll only reserve the slot until we figure that
            // out.
            if (atomic_compare_exchange_strong(&latest->data[i], &entry, reserved_form)) {
                // find out if there's already an entry, it's possible that this entry is already
                // reserved which is annoying because now we're waiting for some other thread to
                // sort itself out (given it's making forward progress we shouldn't be able to
                // dead-lock here).
                void* old = NULL;
                LatticeTable* curr = atomic_load(&latest->prev);
                while (curr != NULL && old == NULL) {
                    old = lattice_raw_lookup(curr, h, l);
                    curr = atomic_load(&curr->prev);
                }

                // count doesn't care that it's a migration, it's at least not replacing an existing
                // slot in this version of the table.
                atomic_fetch_add_explicit(&latest->count, 1, memory_order_relaxed);

                if (old != NULL) {
                    atomic_exchange(&latest->data[i], (uintptr_t) old);
                    result = old;
                    break;
                } else {
                    // no entry was found, good (reserved -> l)
                    atomic_exchange(&latest->data[i], (uintptr_t) l);
                    result = l;
                    break;
                }
            }

            Lattice* entry_p = (Lattice*) (entry & ~1ull);
            if (lattice_cmp(entry_p, l)) {
                if ((entry & 1) == 0) {
                    // not reserved, yay
                    result = entry_p;
                    break;
                }

                // printf("RESERVED? %p (wait we're holding)\n", entry_p);

                // ok if it's a reserved form and it matches us, we'll just wait for the other thread
                // to decide on things.
                uintptr_t decision = lattice_raw_get_entry(latest, i);
                result = (Lattice*) decision;
                break;
            }

            i = (i + 1) & mask;
        } while (i != first);

        // if the table changed before our eyes, it means someone resized which sucks
        // but it just means we need to retry
        LatticeTable* new_latest = atomic_load(&mod->lattice.table);
        if (latest == new_latest) {
            // cuikperf_region_end();
            return result;
        }
        latest = new_latest;
    }
}

static Lattice* lattice_intern(TB_Function* f, Lattice l) {
    assert(l.tag != LATTICE_TUPLE);

    // thread-local but permanent so we can keeps it's items alive the entire
    // module's lifetime but also quickly unwind if the allocation didn't go thru.
    TB_Arena* arena = get_permanent_arena(f->super.module);
    Lattice* k = tb_arena_alloc(arena, sizeof(Lattice));
    memcpy(k, &l, sizeof(l));

    Lattice* interned = lattice_raw_intern(f->super.module, k, false);
    if (interned != k) {
        // fast unwind, we finna use the old val
        tb_arena_free(arena, k, sizeof(Lattice));
    }
    return interned;
}

void tb__lattice_init(TB_Module* m) {
    int initial_exp = 5;

    LatticeTable* latest = tb_platform_heap_alloc(sizeof(LatticeTable) + (1<<initial_exp)*sizeof(uintptr_t));
    *latest = (LatticeTable){ .exp = initial_exp };
    memset(latest->data, 0, (1<<initial_exp)*sizeof(uintptr_t));
    m->lattice.table = latest;

    lattice_raw_insert(latest, &BOT_IN_THE_SKY);
    lattice_raw_insert(latest, &TOP_IN_THE_SKY);
    lattice_raw_insert(latest, &CTRL_IN_THE_SKY);
    lattice_raw_insert(latest, &NULL_IN_THE_SKY);
    lattice_raw_insert(latest, &XNULL_IN_THE_SKY);
    lattice_raw_insert(latest, &FLT32_IN_THE_SKY);
    lattice_raw_insert(latest, &FLT64_IN_THE_SKY);
    lattice_raw_insert(latest, &NAN32_IN_THE_SKY);
    lattice_raw_insert(latest, &NAN64_IN_THE_SKY);
    lattice_raw_insert(latest, &XNAN32_IN_THE_SKY);
    lattice_raw_insert(latest, &XNAN64_IN_THE_SKY);
    lattice_raw_insert(latest, &ANYMEM_IN_THE_SKY);
    lattice_raw_insert(latest, &ALLMEM_IN_THE_SKY);
    lattice_raw_insert(latest, &ANYPTR_IN_THE_SKY);
    lattice_raw_insert(latest, &ALLPTR_IN_THE_SKY);
    lattice_raw_insert(latest, &FALSE_IN_THE_SKY);
    lattice_raw_insert(latest, &TRUE_IN_THE_SKY);
    lattice_raw_insert(latest, &BOOL_IN_THE_SKY);
}

static bool lattice_is_const_int(Lattice* l) { return l->_int.min == l->_int.max; }
static bool lattice_is_const(Lattice* l) { return l->tag == LATTICE_INT && l->_int.min == l->_int.max; }

static void latuni_grow(TB_Function* f, size_t top) {
    size_t new_cap = tb_next_pow2(top + 16);
    f->types = tb_platform_heap_realloc(f->types, new_cap * sizeof(Lattice*));
    // clear new space
    FOR_N(i, f->type_cap, new_cap) { f->types[i] = NULL; }
    f->type_cap = new_cap;
}

static bool latuni_set_progress(TB_Function* f, TB_Node* n, Lattice* l) {
    if (UNLIKELY(n->gvn >= f->type_cap)) { latuni_grow(f, n->gvn); }
    Lattice* old = f->types[n->gvn];
    f->types[n->gvn] = l;
    return old != l;
}

static void latuni_set(TB_Function* f, TB_Node* n, Lattice* l) {
    if (UNLIKELY(n->gvn >= f->type_cap)) { latuni_grow(f, n->gvn); }
    f->types[n->gvn] = l;
}

Lattice* latuni_get(TB_Function* f, TB_Node* n) {
    if (UNLIKELY(n->gvn >= f->type_cap)) { latuni_grow(f, n->gvn); }
    if (f->types[n->gvn] == NULL) { f->types[n->gvn] = lattice_from_dt(f, n->dt); }
    return f->types[n->gvn];
}

static bool lattice_top_or_bot(Lattice* l) {
    return l->tag <= LATTICE_TOP;
}

Lattice* lattice_truthy(Lattice* l) {
    switch (l->tag) {
        case LATTICE_INT:
        if (l->_int.min == l->_int.max) {
            return l->_int.min ? &TRUE_IN_THE_SKY : &FALSE_IN_THE_SKY;
        }
        return &BOT_IN_THE_SKY;

        case LATTICE_NAN32: return &FALSE_IN_THE_SKY;
        case LATTICE_NAN64: return &FALSE_IN_THE_SKY;

        case LATTICE_NULL:  return &FALSE_IN_THE_SKY;
        case LATTICE_XNULL: return &TRUE_IN_THE_SKY;

        default:
        return &BOT_IN_THE_SKY;
    }
}

static uint64_t lattice_int_min(int bits) { return (1ll << (bits - 1)) | ~tb__mask(bits); }
static uint64_t lattice_int_max(int bits) { return (1ll << (bits - 1)) - 1; }
static uint64_t lattice_uint_max(int bits) { return UINT64_MAX >> (64 - bits); }

static Lattice* lattice_from_dt(TB_Function* f, TB_DataType dt) {
    switch (dt.type) {
        case TB_INT: {
            assert(dt.data <= 64);
            if (dt.data == 0) {
                return &BOT_IN_THE_SKY;
            } else if (dt.data == 1) {
                return &BOOL_IN_THE_SKY;
            }

            return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { lattice_int_min(dt.data), lattice_int_max(dt.data) } });
        }

        case TB_FLOAT32: {
            return &FLT32_IN_THE_SKY;
        }

        case TB_FLOAT64: {
            return &FLT64_IN_THE_SKY;
        }

        case TB_PTR: {
            return &ALLPTR_IN_THE_SKY;
        }

        case TB_MEMORY: {
            return &ALLMEM_IN_THE_SKY;
        }

        case TB_CONTROL: return &CTRL_IN_THE_SKY;
        default: return &BOT_IN_THE_SKY;
    }
}

static Lattice* lattice_branch_goto(TB_Function* f, int succ_count, int taken) {
    TB_Arena* arena = get_permanent_arena(f->super.module);

    size_t size = sizeof(Lattice) + succ_count*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = succ_count };
    FOR_N(i, 0, succ_count) {
        l->elems[i] = i == taken ? &CTRL_IN_THE_SKY : &TOP_IN_THE_SKY;
    }

    Lattice* k = lattice_raw_intern(f->super.module, l, false);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static Lattice* lattice_tuple_from_node(TB_Function* f, TB_Node* n) {
    assert(n->dt.type == TB_TUPLE);
    TB_Arena* arena = get_permanent_arena(f->super.module);

    // count projs
    int projs = 0;
    FOR_USERS(u, n) {
        if (is_proj(USERN(u))) { projs++; }
    }

    size_t size = sizeof(Lattice) + projs*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = projs };
    FOR_USERS(u, n) {
        if (!is_proj(USERN(u))) { continue; }
        int index = TB_NODE_GET_EXTRA_T(USERN(u), TB_NodeProj)->index;
        l->elems[index] = lattice_from_dt(f, USERN(u)->dt);
    }

    Lattice* k = lattice_raw_intern(f->super.module, l, false);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

#define MASK_UPTO(pos) (UINT64_MAX >> (64 - pos))
#define BEXTR(src,pos) (((src) >> (pos)) & 1)
uint64_t tb__sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits) {
    uint64_t sign_bit = BEXTR(src, src_bits-1);
    uint64_t mask = MASK_UPTO(dst_bits) & ~MASK_UPTO(src_bits);

    uint64_t dst = src & ~mask;
    return dst | (sign_bit ? mask : 0);
}

static bool lattice_signed(LatticeInt* l) { return l->min > l->max; }

static LatticeInt lattice_into_unsigned(LatticeInt i, int bits) {
    if (i.min > i.max) {
        i.min = 0;
        i.max = lattice_uint_max(bits);
        return i;
    } else {
        return i;
    }
}

static Lattice* lattice_gimme_int(TB_Function* f, int64_t min, int64_t max) {
    assert(min <= max);
    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max } });
}

static Lattice* lattice_gimme_uint(TB_Function* f, uint64_t min, uint64_t max) {
    assert(min <= max);
    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max } });
}

static Lattice* lattice_alias(TB_Function* f, int alias_idx) {
    TB_Arena* arena = get_permanent_arena(f->super.module);

    size_t alias_n = (alias_idx + 64) / 64;
    size_t size = sizeof(Lattice) + alias_n*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_MEM_SLICE, ._alias_n = alias_n };

    // just flip aliases
    FOR_N(i, 0, alias_n) { l->alias[i] = 0; }
    l->alias[alias_idx / 64] |= 1ull << (alias_idx % 64);

    Lattice* k = lattice_raw_intern(f->super.module, l, false);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static Lattice* lattice_dual(TB_Function* f, Lattice* type) {
    switch (type->tag) {
        case LATTICE_BOT: return &TOP_IN_THE_SKY;
        case LATTICE_TOP: return &BOT_IN_THE_SKY;

        case LATTICE_INT: {
            LatticeInt i = type->_int;
            return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { i.max, i.min, ~i.known_zeros, ~i.known_ones, INT_WIDEN_LIMIT - i.widen } });
        }

        case LATTICE_ALLPTR: return &ANYPTR_IN_THE_SKY;
        case LATTICE_ANYPTR: return &ALLPTR_IN_THE_SKY;

        case LATTICE_ALLMEM: return &ANYMEM_IN_THE_SKY;
        case LATTICE_ANYMEM: return &ALLMEM_IN_THE_SKY;

        case LATTICE_MEM_SLICE: {
            TB_Arena* arena = get_permanent_arena(f->super.module);
            size_t size = sizeof(Lattice) + type->_alias_n*sizeof(Lattice*);
            Lattice* l = tb_arena_alloc(arena, size);
            *l = (Lattice){ LATTICE_TUPLE, ._alias_n = type->_alias_n };

            // just flip aliases
            FOR_N(i, 0, type->_alias_n) { l->alias[i] = ~type->alias[i]; }

            Lattice* k = lattice_raw_intern(f->super.module, l, false);
            if (k != l) { tb_arena_free(arena, l, size); }
            return k;
        }

        case LATTICE_TUPLE: {
            TB_Arena* arena = get_permanent_arena(f->super.module);
            size_t size = sizeof(Lattice) + type->_elem_count*sizeof(Lattice*);
            Lattice* l = tb_arena_alloc(arena, size);
            *l = (Lattice){ LATTICE_TUPLE, ._elem_count = type->_elem_count };
            FOR_N(i, 0, type->_elem_count) {
                l->elems[i] = lattice_dual(f, type->elems[i]);
            }

            Lattice* k = lattice_raw_intern(f->super.module, l, false);
            if (k != l) { tb_arena_free(arena, l, size); }
            return k;
        }

        default: return type; // lands on the centerline
    }
}

// greatest lower bound between a and b, note that we expect interned lattices here
static Lattice* lattice_meet(TB_Function* f, Lattice* a, Lattice* b) {
    // a ^ a = a
    if (a == b) return a;

    // it's commutative, so let's simplify later code this way.
    // now a will always be smaller tag than b.
    if (a->tag > b->tag) {
        SWAP(Lattice*, a, b);
    }

    switch (a->tag) {
        case LATTICE_BOT: return &BOT_IN_THE_SKY;
        case LATTICE_TOP: return b;

        case LATTICE_INT: {
            if (b->tag != LATTICE_INT) {
                return &BOT_IN_THE_SKY;
            }

            // [amin, amax] /\ [bmin, bmax] => [min(amin, bmin), max(amax, bmax)]
            LatticeInt i = {
                .min         = TB_MIN(a->_int.min, b->_int.min),
                .max         = TB_MAX(a->_int.max, b->_int.max),
                .known_zeros = a->_int.known_zeros & b->_int.known_zeros,
                .known_ones  = a->_int.known_ones  & b->_int.known_ones,
                .widen       = TB_MAX(a->_int.widen, b->_int.widen),
            };

            return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = i });
        }

        // here's the cases we need to handle here for floats (a is rows):
        //   Fc means float constant, N is NaN and ~N is not-NaN
        //
        //    F  N ~N Fc
        //  F
        //  N F
        // ~N F  F
        // Fc *  *  *  *
        //
        // * Fc meeting with things has to worry about mismatching constants and which
        //   bucket the constant fits into (nans or not).
        case LATTICE_FLT32: case LATTICE_NAN32: case LATTICE_XNAN32: return &FLT32_IN_THE_SKY;
        case LATTICE_FLT64: case LATTICE_NAN64: case LATTICE_XNAN64: return &FLT64_IN_THE_SKY;
        case LATTICE_FLTCON32: if (b->tag == a->tag) {
            bool anan = isnan(a->_f32), bnan = isnan(b->_f32);
            return anan == bnan ? (anan ? &NAN32_IN_THE_SKY : &XNAN32_IN_THE_SKY) : &FLT32_IN_THE_SKY;
        } else {
            return &BOT_IN_THE_SKY;
        }
        case LATTICE_FLTCON64: if (b->tag == a->tag) {
            bool anan = isnan(a->_f64), bnan = isnan(b->_f64);
            return anan == bnan ? (anan ? &NAN64_IN_THE_SKY : &XNAN64_IN_THE_SKY) : &FLT64_IN_THE_SKY;
        } else {
            return &BOT_IN_THE_SKY;
        }

        // so long as it's a ptr-y thing, allptr will be below it
        case LATTICE_ALLPTR: return b->tag <= LATTICE_PTRCON ? &ALLPTR_IN_THE_SKY : &BOT_IN_THE_SKY;
        // anyptr is gonna be above whatever else might get throw here
        case LATTICE_ANYPTR: return b;
        // null ^ ~null = ptr       null ^ sym = ptr
        case LATTICE_NULL:   return b->tag <= LATTICE_PTRCON ? &ALLPTR_IN_THE_SKY : &BOT_IN_THE_SKY;
        // ~null ^ sym = ~null
        case LATTICE_XNULL:  return b->tag <= LATTICE_PTRCON ? a : &BOT_IN_THE_SKY;
        // symA ^ symB = ~null (they're different due to the early out)
        case LATTICE_PTRCON: return b->tag <= LATTICE_PTRCON ? &XNULL_IN_THE_SKY : &BOT_IN_THE_SKY;

        // already handled ctrl ^ ctrl, top ^ any, bot ^ any so everything else is bot
        case LATTICE_CTRL: return &BOT_IN_THE_SKY;

        case LATTICE_ALLMEM: return &ALLMEM_IN_THE_SKY;
        case LATTICE_ANYMEM: return b;
        case LATTICE_MEM_SLICE: {
            if (b->tag != LATTICE_MEM_SLICE) { return &BOT_IN_THE_SKY; }
            size_t alias_n = TB_MAX(a->_alias_n, b->_alias_n);

            TB_Arena* arena = get_permanent_arena(f->super.module);
            size_t size = sizeof(Lattice) + alias_n*sizeof(Lattice*);
            Lattice* l = tb_arena_alloc(arena, size);
            *l = (Lattice){ LATTICE_MEM_SLICE, ._alias_n = alias_n };

            FOR_N(i, 0, a->_alias_n) { l->alias[i] = a->alias[i]; }
            FOR_N(i, a->_alias_n, alias_n) { l->alias[i] = 0; }
            FOR_N(i, 0, b->_alias_n) { l->alias[i] |= b->alias[i]; }

            Lattice* k = lattice_raw_intern(f->super.module, l, false);
            if (k != l) { tb_arena_free(arena, l, size); }
            return k;
        }

        case LATTICE_TUPLE: {
            if (b->tag != LATTICE_TUPLE || a->_elem_count != b->_elem_count) {
                return &BOT_IN_THE_SKY;
            }

            TB_Arena* arena = get_permanent_arena(f->super.module);
            size_t size = sizeof(Lattice) + a->_elem_count*sizeof(Lattice*);
            Lattice* l = tb_arena_alloc(arena, size);
            *l = (Lattice){ LATTICE_TUPLE, ._elem_count = a->_elem_count };
            FOR_N(i, 0, a->_elem_count) {
                l->elems[i] = lattice_meet(f, a->elems[i], b->elems[i]);
            }

            Lattice* k = lattice_raw_intern(f->super.module, l, false);
            if (k != l) { tb_arena_free(arena, l, size); }
            return k;
        }

        default: tb_todo();
    }
}

// least upper bound between a and b
static Lattice* lattice_join(TB_Function* f, Lattice* a, Lattice* b) {
    a = lattice_dual(f, a);
    b = lattice_dual(f, b);
    return lattice_dual(f, lattice_meet(f, a, b));
}
