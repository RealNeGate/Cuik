
typedef struct {
    uint64_t bot, top;

    // for known bit analysis
    uint64_t known_zeros;
    uint64_t known_ones;
} LatticeInt;

// a simplification of the set of all pointers (or floats)
// we can easily JOIN two trifectas with a min.
typedef enum {
    LATTICE_UNKNOWN,        // top aka {nan, non-nan} or for pointers {null, non-null}

    LATTICE_KNOWN_NAN = 1,  // {nan}
    LATTICE_KNOWN_NOT_NAN,  // {non-nan}

    LATTICE_KNOWN_NULL = 1, // {null}
    LATTICE_KNOWN_NOT_NULL  // {non-null}
} LatticeTrifecta;

typedef struct {
    LatticeTrifecta trifecta;
} LatticeFloat;

// TODO(NeGate): we might wanna store more info like aliasing, ownership and alignment.
typedef struct {
    LatticeTrifecta trifecta;
} LatticePointer;

// Represents the fancier type system within the optimizer, it's
// all backed by my shitty understanding of lattice theory
typedef struct {
    enum {
        LATTICE_INT,
        LATTICE_FLOAT32,
        LATTICE_FLOAT64,
        LATTICE_POINTER,
    } tag;
    union {
        LatticeInt _int;
        LatticeFloat _float;
        LatticePointer _ptr;
    };
} Lattice;

// maximal subset
static Lattice lattice_top(TB_DataType dt) {
    switch (dt.type) {
        case TB_INT: {
            assert(dt.data <= 64);
            uint64_t max_bits = UINT64_MAX >> dt.data;
            tb_todo();

            return (Lattice){ LATTICE_INT, ._int = { 0, max_bits } };
        }

        case TB_FLOAT: {
            assert(dt.data == TB_FLT_32 || dt.data == TB_FLT_64);
            return (Lattice){ dt.data == TB_FLT_64 ? LATTICE_FLOAT64 : LATTICE_FLOAT32, ._float = { LATTICE_UNKNOWN } };
        }

        case TB_PTR: {
            return (Lattice){ LATTICE_POINTER, ._ptr = { LATTICE_UNKNOWN } };
        }

        default:
        tb_todo();
    }
}

static LatticeTrifecta lattice_trifecta_meet(LatticeTrifecta a, LatticeTrifecta b) {
    return a < b ? a : b;
}

// generates the greatest lower bound between a and b
static Lattice lattice_meet(const Lattice* a, const Lattice* b) {
    assert(a->tag == b->tag);
    switch (a->tag) {
        case LATTICE_INT: {
            // [amin, amax] ^ [bmin, bmax] => [min(amin, bmin), max(amax, bmax)]
            LatticeInt aa = a->_int;
            LatticeInt bb = b->_int;

            LatticeInt i = { aa.bot, aa.top };
            if (i.bot > bb.bot) i.bot = bb.bot;
            if (i.top < bb.top) i.top = bb.top;

            i.known_zeros = aa.known_zeros & bb.known_zeros;
            i.known_ones = aa.known_ones & bb.known_ones;
            return (Lattice){ LATTICE_INT, ._int = i };
        }

        case LATTICE_FLOAT32:
        case LATTICE_FLOAT64: {
            LatticeFloat aa = a->_float;
            LatticeFloat bb = b->_float;

            LatticeFloat f = { .trifecta = lattice_trifecta_meet(aa.trifecta, bb.trifecta) };
            return (Lattice){ a->tag, ._float = f };
        }

        case LATTICE_POINTER: {
            LatticePointer aa = a->_ptr;
            LatticePointer bb = b->_ptr;

            LatticePointer p = { .trifecta = lattice_trifecta_meet(aa.trifecta, bb.trifecta) };
            return (Lattice){ LATTICE_POINTER, ._ptr = p };
        }

        default: tb_todo();
    }
}

