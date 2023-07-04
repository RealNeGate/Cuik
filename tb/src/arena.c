
////////////////////////////////
// Fallback arena
////////////////////////////////
// If you don't specify an allocator for your functions this is used
typedef struct TB_NodePage TB_NodePage;
struct TB_NodePage {
    TB_NodePage* next;
    size_t used, cap;
    char data[];
};

enum {
    TB_NODE_PAGE_GENERAL_CAP = 4096 - sizeof(TB_NodePage),
};

typedef struct {
    TB_Arena super;
    TB_NodePage *head, *tail;
    Arena data;
} TB_NodeArena;

static void* node_arena_alloc(TB_Arena* arena, size_t necessary_size, size_t align) {
    TB_NodeArena* na = (TB_NodeArena*) arena;

    size_t align_mask = align - 1;
    necessary_size = (necessary_size + align_mask) & ~align_mask;

    // no pages or no fitting? make a page
    if (na->tail == NULL || na->tail->used + necessary_size >= na->tail->cap) {
        size_t cap = necessary_size < TB_NODE_PAGE_GENERAL_CAP ? TB_NODE_PAGE_GENERAL_CAP : tb_next_pow2(necessary_size);
        TB_NodePage* page = tb_platform_valloc(sizeof(TB_NodePage) + cap);
        page->next = NULL;
        page->used = necessary_size;
        page->cap  = cap;

        // attach to list
        if (na->tail) na->tail->next = page, na->tail = page;
        else na->head = na->tail = page;

        return page->data;
    } else {
        // we have a node and we can fit our allocation there
        char* dst = &na->tail->data[na->tail->used];
        na->tail->used += necessary_size;
        return dst;
    }
}

static void node_arena_clear(TB_Arena* arena) {
    TB_NodeArena* na = (TB_NodeArena*) arena;
    TB_NodePage* p = na->head;
    while (p) {
        TB_NodePage* next = p->next;
        tb_platform_vfree(p, 4096);
        p = next;
    }
    na->head = na->tail = NULL;
}

static void node_arena_free(TB_Arena* arena) {
    node_arena_clear(arena);
    tb_platform_heap_free(arena);
}

TB_Arena* tb_node_arena(void) {
    TB_NodeArena* na = tb_platform_heap_alloc(sizeof(TB_NodeArena));
    *na = (TB_NodeArena){ { node_arena_alloc, node_arena_clear, node_arena_free } };
    return &na->super;
}

////////////////////////////////
// Default arena, allocates 16MiB blocks
////////////////////////////////
typedef struct {
    TB_Arena super;
    Arena data;
} TB_DefaultArena;

static void* default_arena_alloc(TB_Arena* arena, size_t size, size_t align) {
    TB_DefaultArena* da = (TB_DefaultArena*) arena;
    return arena_alloc(&da->data, size, align);
}

static void default_arena_clear(TB_Arena* arena) {
    TB_DefaultArena* da = (TB_DefaultArena*) arena;
    arena_clear(&da->data);
}

static void default_arena_free(TB_Arena* arena) {
    TB_DefaultArena* da = (TB_DefaultArena*) arena;
    arena_free(&da->data);
    tb_platform_heap_free(da);
}

TB_Arena* tb_default_arena(void) {
    TB_DefaultArena* da = tb_platform_heap_alloc(sizeof(TB_DefaultArena));
    *da = (TB_DefaultArena){ { default_arena_alloc, default_arena_clear, default_arena_free } };
    return &da->super;
}
