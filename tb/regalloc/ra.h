#pragma once

typedef struct {
    uint32_t target;
} SplitDecision;

typedef struct {
    int pos;
    TB_Node* n;
} RAInsert;

// High reg pressure block data, the splitter
// will avoid keeping split values alive in this
// region when possible.
typedef struct {
    int curr, max;
    // index in the block where the pressure went from lo->hi
    int lo2hi;
} RAPressure;

typedef struct RABase RABase;
struct RABase {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;

    // how many stack slots did the RA introduce
    int num_spills;
    int max_regs_in_class;

    // coalesce disjoint set
    int* uf;
    int* uf_size;
    int uf_len;

    // leader -> list of members
    NL_Table coalesce_set;

    // for doing local interference mask calcs
    int mask_cap;
    uint64_t* mask;

    RAPressure* hrp[MAX_REG_CLASSES];

    DynArray(SplitDecision) splits;

    // do we need to build any alternative structures since we've
    // changed the graph
    bool interfere_dirty;

    void (*rebuild_intr)(Ctx* ctx, RABase* ra);
    // hard splits and the initial coalescing need at least
    // some definition of this even if it's crude.
    bool (*interfere)(Ctx* ctx, RABase* ra, TB_Node* a, TB_Node* b);
};

static TB_Node** coalesce_set_array(RABase* ra, TB_Node** n_ptr, size_t* out_count) {
    int leader = uf_find(ra->uf, ra->uf_len, (*n_ptr)->gvn);
    ArenaArray(TB_Node*) set = nl_table_get(&ra->coalesce_set, (void*) (uintptr_t) (leader + 1));
    if (set) {
        *out_count = aarray_length(set);
        return set;
    } else {
        *out_count = 1;
        return n_ptr;
    }
}

void tb__ra_resize_uf(RABase* ra, size_t new_len);
int tb__ra_coalesce(RABase* ra, TB_Node* xn, TB_Node* yn);
bool tb__ra_can_coalesce(RABase* ra, TB_Node* xn, TB_Node* yn);

void tb__ra_init(RABase* ra, TB_Arena* arena);
void tb__ra_deinit(RABase* ra);

double tb__ra_get_spill_cost(RABase* ra, VReg* vreg);

void tb__ra_bulk_insert(Ctx* ctx, TB_BasicBlock* bb, DynArray(RAInsert) inserts);
void tb__ra_bulk_insert_rev(Ctx* ctx, TB_BasicBlock* bb, DynArray(RAInsert) inserts);

// Complex splitter
void tb__insert_splits(Ctx* ctx, RABase* ra, SplitDecision* splits, size_t num_spills);

// TODO(NeGate): namespacing
void insert_op_at_end(Ctx* ctx, RABase* ra, TB_BasicBlock* bb, TB_Node* n);
