#include <hashes.h>
#include <math.h>

static Lattice TOMBSTONE         = { 0 };

static Lattice TOP_IN_THE_SKY    = { LATTICE_TOP    };
static Lattice BOT_IN_THE_SKY    = { LATTICE_BOT    };
static Lattice LIVE_IN_THE_SKY   = { LATTICE_LIVE   };
static Lattice DEAD_IN_THE_SKY   = { LATTICE_DEAD   };
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
static Lattice FALSE_IN_THE_SKY  = { LATTICE_INT, ._int = {  0, 0, UINT64_MAX, 0 } };
static Lattice TRUE_IN_THE_SKY   = { LATTICE_INT, ._int = { -1,-1, 0, UINT64_MAX } };
static Lattice BOOL_IN_THE_SKY   = { LATTICE_INT, ._int = { -1, 0, 0, 0          } };

static Lattice* lattice_from_dt(TB_Function* f, TB_DataType dt);

static uint32_t lattice_hash(const void* a) {
    const Lattice* l = a;
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
        case LATTICE_LIVE:  case LATTICE_DEAD:
        break;

        default: tb_todo();
    }

    // fib hashing amirite
    return (h * 11400714819323198485llu) >> 32llu;
}

static bool lattice_cmp(const void* a, const void* b) {
    const Lattice *aa = a, *bb = b;
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

static void* lattice_hs_alloc(size_t size) {
    void* ptr = tb_platform_heap_alloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static void lattice_hs_free(void* ptr, size_t size) {
    tb_platform_heap_free(ptr);
}

static Lattice* lattice_intern(TB_Function* f, Lattice l) {
    assert(l.tag != LATTICE_TUPLE);

    // thread-local but permanent so we can keeps it's items alive the entire
    // module's lifetime but also quickly unwind if the allocation didn't go thru.
    TB_Module* m = f->super.module;
    TB_Arena* arena = get_permanent_arena(m);
    Lattice* k = tb_arena_alloc(arena, sizeof(Lattice));
    memcpy(k, &l, sizeof(l));

    Lattice* interned = nbhs_intern(&m->lattice_elements, k);
    if (interned != k) {
        // fast unwind, we finna use the old val
        tb_arena_free(arena, k, sizeof(Lattice));
    }
    return interned;
}

void tb__lattice_init(TB_Module* m) {
    m->lattice_elements = nbhs_alloc(64, lattice_hs_alloc, lattice_hs_free, lattice_cmp, lattice_hash);

    nbhs_raw_insert(&m->lattice_elements, &BOT_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &TOP_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &LIVE_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &DEAD_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &NULL_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &XNULL_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &FLT32_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &FLT64_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &NAN32_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &NAN64_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &XNAN32_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &XNAN64_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &ANYMEM_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &ALLMEM_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &ANYPTR_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &ALLPTR_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &FALSE_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &TRUE_IN_THE_SKY);
    nbhs_raw_insert(&m->lattice_elements, &BOOL_IN_THE_SKY);
}

static bool lattice_is_const(Lattice* l) { return l->tag == LATTICE_INT && l->_int.min == l->_int.max; }
static bool lattice_is_unsigned(Lattice* l) { return l->tag == LATTICE_INT && (uint64_t)l->_int.min <= (uint64_t)l->_int.max; }

// all of these functions return false if the lattice isn't a constant.
static bool lattice_int_eq(Lattice* l, int64_t x) { return l->_int.min == l->_int.max && l->_int.min == x; }
static bool lattice_int_ne(Lattice* l, int64_t x) { return l->_int.min == l->_int.max && l->_int.min != x; }
static bool lattice_int_gt(Lattice* l, int64_t x) { return l->_int.min > x  && l->_int.max > x; }
static bool lattice_int_lt(Lattice* l, int64_t x) { return l->_int.min < x  && l->_int.max < x; }
static bool lattice_int_ge(Lattice* l, int64_t x) { return l->_int.min >= x && l->_int.max >= x; }
static bool lattice_int_le(Lattice* l, int64_t x) { return l->_int.min <= x && l->_int.max <= x; }

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
        case TB_TAG_INT: {
            assert(dt.data <= 64);
            if (dt.data == 0) {
                return &BOT_IN_THE_SKY;
            } else if (dt.data == 1) {
                return &BOOL_IN_THE_SKY;
            }

            uint64_t imin  =  lattice_int_min(dt.data);
            uint64_t imax  =  lattice_int_max(dt.data);
            uint64_t zeros = ~lattice_uint_max(dt.data);
            return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { imin, imax, zeros } });
        }

        case TB_TAG_F32:     return &FLT32_IN_THE_SKY;
        case TB_TAG_F64:     return &FLT64_IN_THE_SKY;
        case TB_TAG_PTR:     return &ALLPTR_IN_THE_SKY;
        case TB_TAG_MEMORY:  return &ALLMEM_IN_THE_SKY;
        case TB_TAG_CONTROL: return &LIVE_IN_THE_SKY;
        default: return &BOT_IN_THE_SKY;
    }
}

static Lattice* lattice_branch_goto(TB_Function* f, int succ_count, int taken) {
    TB_Arena* arena = get_permanent_arena(f->super.module);

    size_t size = sizeof(Lattice) + succ_count*sizeof(Lattice*);
    Lattice* l = tb_arena_alloc(arena, size);
    *l = (Lattice){ LATTICE_TUPLE, ._elem_count = succ_count };
    FOR_N(i, 0, succ_count) {
        l->elems[i] = i == taken ? &LIVE_IN_THE_SKY : &DEAD_IN_THE_SKY;
    }

    Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static Lattice* lattice_tuple_from_node(TB_Function* f, TB_Node* n) {
    assert(n->dt.type == TB_TAG_TUPLE);
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

    Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
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

static Lattice* lattice_int_const(TB_Function* f, uint64_t con) {
    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { con, con, ~con, con } });
}

static Lattice* lattice_gimme_int(TB_Function* f, int64_t min, int64_t max, int bits) {
    assert(min <= max);

    uint64_t umin = min;
    uint64_t umax = max;
    if (umin > umax) {
        umin = 0, umax = lattice_uint_max(bits);
    }

    uint64_t zeros = ~umin | ~lattice_uint_max(bits);
    uint64_t ones  =  umin;

    if (umin != umax) {
        // wherever the highest differing bit is we just clear everything below that
        int msb_diff = 64 - __builtin_clzll(umin ^ umax);
        zeros &= ~(UINT64_MAX >> (64 - msb_diff));
        ones  &= ~(UINT64_MAX >> (64 - msb_diff));
    }

    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
}

static Lattice* lattice_gimme_uint(TB_Function* f, uint64_t min, uint64_t max, int bits) {
    assert(min <= max);

    uint64_t zeros = ~min | ~lattice_uint_max(bits);
    uint64_t ones  =  min;

    if (min != max) {
        // wherever the highest differing bit is we just clear everything below that
        int msb_diff = 64 - __builtin_clzll(min ^ max);
        zeros &= ~(UINT64_MAX >> (64 - msb_diff));
        ones  &= ~(UINT64_MAX >> (64 - msb_diff));
    }

    return lattice_intern(f, (Lattice){ LATTICE_INT, ._int = { min, max, zeros, ones } });
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

    Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
    if (k != l) { tb_arena_free(arena, l, size); }
    return k;
}

static Lattice* lattice_dual(TB_Function* f, Lattice* type) {
    switch (type->tag) {
        case LATTICE_BOT: return &TOP_IN_THE_SKY;
        case LATTICE_TOP: return &BOT_IN_THE_SKY;

        // putting ctrl on the centerline is weird which is why ~ctrl
        // exists lol.
        case LATTICE_LIVE: return &DEAD_IN_THE_SKY;
        case LATTICE_DEAD: return &LIVE_IN_THE_SKY;

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

            Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
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

            Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
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
        case LATTICE_LIVE: return b == &DEAD_IN_THE_SKY ? &LIVE_IN_THE_SKY : &BOT_IN_THE_SKY;
        // only leftover case here is gonna be live ^ dead
        case LATTICE_DEAD: return b;

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

            Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
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

            Lattice* k = nbhs_intern(&f->super.module->lattice_elements, l);
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
