
// An Efficient Representation for Sparse Sets (1993), Briggs and Torczon
typedef struct {
    size_t count;

    int* array;
    ArenaArray(int) stack;
} SparseSet;

static SparseSet sparse_set_alloc(TB_Arena* arena, size_t count) {
    SparseSet s;
    s.count = count;
    s.array = tb_arena_alloc(arena, count * sizeof(int));
    s.stack = aarray_create(arena, int, 16);
    FOR_N(i, 0, s.count) {
        s.array[i] = -1;
    }
    return s;
}

static void sparse_set_put(SparseSet* set, int v) {
    TB_ASSERT(v < set->count);
    if (set->array[v] < 0) {
        set->array[v] = aarray_length(set->stack);
        aarray_push(set->stack, v);
    }
}

static void sparse_set_clear(SparseSet* set) {
    aarray_for(i, set->stack) {
        set->array[set->stack[i]] = -1;
    }
    aarray_clear(set->stack);
}

static int sparse_set_pop(SparseSet* set) {
    if (aarray_length(set->stack) > 0) {
        int v = aarray_pop(set->stack);
        set->array[v] = -1;
        return v;
    } else {
        return -1;
    }
}

static void sparse_set_remove(SparseSet* set, int v) {
    int index = set->array[v]; // stack's index
    if (index >= 0) {
        // move last entry to the removed space
        int last = aarray_pop(set->stack);
        set->array[last]  = -1;
        set->array[v]     = -1;
        set->stack[index] = last;
    }
}

static bool sparse_set_test(SparseSet* set, int v) {
    return set->array[v] >= 0;
}

