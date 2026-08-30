// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>
#include <stdlib.h>

#if USE_INTRIN && CUIK__IS_X64
#include <x86intrin.h>
#endif

#include "regalloc/ra.h"

#define FOREACH_SET(it, set) \
    FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    uint64_t key; // key
    int last_use; // val
} InactiveCacheEntry;

typedef struct {
    RABase base;

    DynArray(SplitDecision) splits;
    DynArray(int) potential_spills; // [gvn]

    DynArray(int) prio_alloc;

    int order_cap;
    int* order;
    TB_Node** gvn2node;

    // [class][reg]
    int* future_active[MAX_REG_CLASSES];
    int* active[MAX_REG_CLASSES];
    int stack_reg_count;

    // linked list of future active
    int* next_inactive; // [gvn]
    Set is_inactive;

    Set been_spilled;
    Set live;

    // bitset of which nodes are vregs
    uint64_t* is_vreg;

    // last use in a BB
    InactiveCacheEntry* inactive_cache;

    // where is the linear scan at
    int where_bb;
    int where_order;
} Rogers;

static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void allocate_loop2(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);

static void rogers_remat(Ctx* ctx, Rogers* ra, TB_Node* n, bool kill_node);
static void better_spill_range(Ctx* ctx, Rogers* restrict ra, TB_Node* to_spill, size_t old_node_count);
static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn);

static bool foobar;
static int stats_aaa;
static int histo[10000];

static int rogers_insert_op(Ctx* ctx, int bb_id, TB_Node* n, int pos) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];

    // skip phis and projections so that they stay nice and snug
    size_t cnt = aarray_length(bb->items);
    aarray_push(bb->items, 0);
    if (cnt > pos) {
        memmove(&bb->items[pos + 1], &bb->items[pos], (cnt - pos) * sizeof(TB_Node*));
    }
    bb->items[pos] = n;
    tb__insert(ctx, ctx->f, bb, n);
    return pos;
}

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    double cost = tb__ra_get_spill_cost(&ra->base, vreg);
    printf("# V%-4"PRIdPTR" cost=%.2f area=%"PRIu64" bias=%.2f ", vreg - ctx->vregs, cost, vreg->area, vreg->spill_bias);
    tb__print_regmask(&OUT_STREAM_DEFAULT, vreg->mask);
    printf("\n");
}

static void rogers_dump_block(Ctx* restrict ctx, int old_node_count, size_t i) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[i];
    printf("BB%zu (freq=%f, %%%u):\n", i, bb->freq, bb->start->gvn);

    TB_Node* end = bb->end;
    aarray_for(i, bb->items) {
        // if the first node is a region or
        if (bb->start == bb->items[i]) {
            continue;
        }

        printf("  ");
        // tb_print_dumb_node(NULL, bb->items[i]);
        ctx->print_pretty(ctx, bb->items[i]);
        if (bb->items[i]->gvn >= old_node_count) {
            printf("  #  NEW!!!");
        }
        printf("\n");
    }

    if (!tb_node_is_terminator(bb->end)) {
        TB_Node* succ_n = cfg_next_control(bb->end);
        TB_BasicBlock* succ_bb = nl_map_get_checked(ctx->cfg.node_to_block, succ_n);
        int b = succ_bb - ctx->cfg.blocks;
        if (ctx->cfg.blocks[b].fwd > 0) {
            while (b != ctx->cfg.blocks[b].fwd) {
                b = ctx->cfg.blocks[b].fwd;
            }
        }

        printf("    jmp BB%d\n", b);
    }
}

static void rogers_dump_sched(Ctx* restrict ctx, int old_node_count) {
    FOR_N(i, 0, ctx->bb_count) {
        rogers_dump_block(ctx, old_node_count, i);
    }
}

static void rogers_dump_split(Ctx* restrict ctx, Rogers* restrict ra, TB_BasicBlock* block, TB_Node* aa, TB_Node* bb) {
    int a[2], b[2];

    bool entry_block = block == &ctx->cfg.blocks[0];
    a[0] = set_get(&block->live_in, aa->gvn) ? 0 : ra->order[aa->gvn] - 1;
    b[0] = set_get(&block->live_in, bb->gvn) ? 0 : ra->order[bb->gvn] - 1;
    a[1] = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, block, aa, aa->gvn) - 1;
    b[1] = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, block, bb, bb->gvn) - 1;

    // if (IS_PROJ(aa)) { aa = aa->inputs[0]; }
    // if (IS_PROJ(bb)) { bb = bb->inputs[0]; }

    int start = TB_MIN(a[0], b[0]);
    int end   = TB_MAX(a[1], b[1]);
    if (end < aarray_length(block->items)) {
        end += 1;
    }

    printf("  A B:\n");
    FOR_N(i, start, end) {
        printf(
               "  %c %c  ",
               i >= a[0] && i <= a[1] ? '*' : ' ',
               i >= b[0] && i <= b[1] ? '*' : ' '
               );
        tb_print_dumb_node(NULL, block->items[i]);
        printf("\n");
    }
}

// mark program point as HRP
static void mark_point_as_hrp(Ctx* ctx, Rogers* ra, TB_Node* n, int reg_class) {
    // TB_ASSERT(reg_class > 0);
    // TB_OPTDEBUG(REGALLOC6)(printf("#       %%%u is considered HRP\n", n->gvn));

    uint32_t gvn = n->gvn;
    TB_BasicBlock* bb = ctx->f->scheduled[gvn];
    int bb_id = bb - ctx->cfg.blocks;
    int t = ra->order[gvn] - 1;

    int end_t = t;
    while (end_t+1 < aarray_length(bb->items) && IS_PROJ(bb->items[end_t+1]) && bb->items[end_t+1]->inputs[0] == n) {
        end_t++;
    }

    /*HRPRegion* hrp = &ra->base.hrp[bb_id];
    if (hrp->start[reg_class] < 0) {
    hrp->start[reg_class] = t;
    hrp->end[reg_class]   = end_t;
    } else {
    hrp->start[reg_class] = TB_MIN(hrp->start[reg_class], t);
    hrp->end[reg_class]   = TB_MAX(hrp->end[reg_class], end_t);
    } */
}

static void gimme_lifetime(Ctx* ctx, Rogers* ra, TB_BasicBlock** scheduled, TB_BasicBlock* bb, TB_Node* n, int* range) {
    uint32_t gvn = n->gvn;
    int start_t = 0;
    if (scheduled[gvn] == bb) {
        if (n->type == TB_PHI) {
            start_t = 0;
        } else if (IS_PROJ(n)) {
            uint32_t tuple_gvn = n->inputs[0]->gvn;
            start_t = ra->order[tuple_gvn] - 1;
        } else {
            start_t = ra->order[gvn] - 1;
        }
    } else if (!set_get(&bb->live_in, gvn)) {
        range[0] = INT_MAX;
        range[1] = -1;
        return;
    }

    int end_t = last_use_in_bb(ctx->cfg.blocks, scheduled, ra, bb, n, gvn) - 1;
    TB_ASSERT(end_t >= start_t);

    range[0] = start_t;
    range[1] = end_t;
}

static void mark_node_as_hrp(Ctx* ctx, Rogers* ra, uint32_t gvn, uint32_t failed_gvn, int reg_class) {
    TB_ASSERT(reg_class > 0);
    TB_ASSERT(ctx->vreg_map[gvn]);

    // we're gonna spill it, so that's not really increasing HRP
    if (ctx->vregs[ctx->vreg_map[gvn]].mask->may_spill) {
        return;
    }

    cuikperf_region_start("mark", NULL);
    // TB_OPTDEBUG(REGALLOC6)(printf("#       %%%u /\\ %%%u (%s) is considered HRP range\n", gvn, failed_gvn, reg_class_name(reg_class)));

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* n = ra->gvn2node[gvn];
    TB_Node* failed = ra->gvn2node[failed_gvn];

    // only one def BB

    // each active range outside of the def BB is live in

    /*FOR_N(bb_id, 0, ctx->bb_count) {
    TB_BasicBlock* bb = &ctx->cfg.blocks[bb_id];
    HRPRegion* hrp = &ra->base.hrp[bb_id];

    // Fully HRP already
    if (hrp->start[reg_class] == 0 && hrp->end[reg_class] == aarray_length(bb->items)-1) {
    continue;
    }

    int A[2], B[2];
    gimme_lifetime(ctx, ra, scheduled, bb, n,      A);
    gimme_lifetime(ctx, ra, scheduled, bb, failed, B);

    int start_t = TB_MAX(A[0], B[0]);
    int end_t   = TB_MIN(A[1], B[1]);
    if (end_t < 0) {
    continue;
    }

    // TB_OPTDEBUG(REGALLOC6)(printf("#         BB%zu [%d (%%%u), %d (%%%u)] (%u items)\n", bb_id, start_t, bb->items[start_t]->gvn, end_t, bb->items[end_t]->gvn, aarray_length(bb->items)));

    if (hrp->start[reg_class] < 0) {
    hrp->start[reg_class] = start_t;
    hrp->end[reg_class]   = end_t;
    } else {
    hrp->start[reg_class] = TB_MIN(hrp->start[reg_class], start_t);
    hrp->end[reg_class]   = TB_MAX(hrp->end[reg_class], end_t);
    }
    // stats_aaa++;
    }*/
    cuikperf_region_end();
}

static thread_local Ctx* compare_split__ctx;
static thread_local Rogers* compare_split__ra;
static int compare_split2(const void* a, const void* b) {
    VReg* vregs = compare_split__ctx->vregs;
    VReg* aa = &vregs[*(int*) a];
    VReg* bb = &vregs[*(int*) b];

    double as = tb__ra_get_spill_cost(&compare_split__ra->base, aa);
    double bs = tb__ra_get_spill_cost(&compare_split__ra->base, bb);
    if (as != bs) {
        // highest score at the end
        return as < bs ? -1 : 1;
    }
    return 0;
}

static thread_local VReg* compare_split__vregs;
static int compare_split(const void* a, const void* b) {
    VReg* vregs = compare_split__vregs;
    VReg* aa = &vregs[((const SplitDecision*) a)->target];
    VReg* bb = &vregs[((const SplitDecision*) b)->target];

    float as = aa->spill_cost - aa->area*0.2;
    float bs = bb->spill_cost - bb->area*0.2;
    if (as != bs) {
        return as > bs ? -1 : 1;
    }
    return 0;
}

enum { INACTIVE_CACHE_LOG2 = 9 };

#if TB_OPTDEBUG_STATS
static int stats_c = 0;
#endif

static uint32_t inactive_hash_index(uint64_t x) {
    x ^= x >> 30;
    x *= 0xbf58476d1ce4e5b9U;
    x ^= x >> 27;
    x *= 0x94d049bb133111ebU;
    x ^= x >> 31;

    // grab top bits
    return x >> (64ull - INACTIVE_CACHE_LOG2);
}

static int last_use_in_bb(TB_BasicBlock* blocks, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n, uint32_t n_gvn) {
    // printf("Last use in BB%zu for %%%u\n", bb - blocks, n->gvn);

    #if TB_OPTDEBUG_STATS
    stats_c++;
    #endif

    uint64_t key = n_gvn | ((bb - blocks) << 32ull);
    int hash_index = inactive_hash_index(key);
    TB_ASSERT(hash_index < (1ull << INACTIVE_CACHE_LOG2));
    if (ra->inactive_cache && ra->inactive_cache[hash_index].key == key) {
        #if TB_OPTDEBUG_STATS
        stats_hit += 1;
        #endif

        return ra->inactive_cache[hash_index].last_use;
    }

    #if TB_OPTDEBUG_STATS
    stats_miss += 1;
    #endif

    int l = 1;
    if (set_get(&bb->live_out, n->gvn)) {
        // if there's no uses, we'll assume it's live out so the
        // "last use" is the BB end
        l = aarray_length(bb->items);
    } else {
        if (scheduled[n->gvn] == bb) {
            l = ra->order[n->gvn];
        }

        FOR_USERS(u, n) {
            TB_Node* un = USERN(u);
            if (USERI(u) < un->input_count &&
                scheduled[un->gvn] == bb &&
                l < ra->order[un->gvn]) {
                l = ra->order[un->gvn];
            }
        }
    }

    if (ra->inactive_cache) {
        ra->inactive_cache[hash_index].key = key;
        ra->inactive_cache[hash_index].last_use = l;
    }
    return l;
}

static bool order_after(int* order, TB_BasicBlock** scheduled, uint32_t a_gvn, uint32_t b_gvn) {
    // the basic blocks are sequentially allocated in reverse post-order
    return scheduled[a_gvn] > scheduled[b_gvn] || (scheduled[a_gvn] == scheduled[b_gvn] && order[a_gvn] > order[b_gvn]);
}

static bool interfere_in_block(TB_BasicBlock* blocks, int* order, TB_BasicBlock** scheduled, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs, uint32_t lhs_gvn, uint32_t rhs_gvn, TB_BasicBlock* block) {
    TB_ASSERT(lhs != rhs && "i... why?");

    // phis might have a liveness hole in the middle
    bool lhs_live_out = set_get(&block->live_out, lhs_gvn);
    bool rhs_live_out = set_get(&block->live_out, rhs_gvn);
    if (lhs_live_out && rhs_live_out) {
        return true;
    } else if (!lhs_live_out && !rhs_live_out) {
        TB_Node *first = lhs, *last = rhs;
        uint32_t first_gvn = lhs_gvn, last_gvn = rhs_gvn;
        if (order_after(order, scheduled, lhs_gvn, rhs_gvn)) {
            first = rhs, last = lhs;
            first_gvn = rhs_gvn, last_gvn = lhs_gvn;
        }

        int last_t = order[last_gvn];
        int last_use = last_use_in_bb(blocks, scheduled, ra, block, first, first_gvn);
        return scheduled[last_gvn] == block && last_use > last_t;
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
            SWAP(uint32_t, lhs_gvn, rhs_gvn);
        }

        int rhs_t = scheduled[rhs_gvn] == block ? ra->order[rhs_gvn] : 0;
        int last_use = last_use_in_bb(blocks, scheduled, ra, block, lhs, lhs_gvn);
        return last_use > rhs_t;
    }
}

static bool interfere(Ctx* restrict ctx, RABase* ra_base, TB_Node* lhs, TB_Node* rhs) {
    Rogers* ra = (Rogers*) ra_base;
    TB_BasicBlock** scheduled = ctx->f->scheduled;
    int* order = ra->order;

    uint32_t lhs_gvn = lhs->gvn, rhs_gvn = rhs->gvn;
    TB_BasicBlock* lhs_block = scheduled[lhs_gvn];
    TB_BasicBlock* rhs_block = scheduled[rhs_gvn];

    if (interfere_in_block(ctx->cfg.blocks, order, scheduled, ra, lhs, rhs, lhs_gvn, rhs_gvn, lhs_block)) {
        return true;
    }

    return lhs_block != rhs_block && interfere_in_block(ctx->cfg.blocks, order, scheduled, ra, rhs, lhs, rhs_gvn, lhs_gvn, rhs_block);
}

static void rebuild_intr(Ctx* ctx, RABase* ra) {
    compute_ordinals(ctx, (Rogers*) ra, ra->arena);
}

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena) {
    Rogers ra = { { .ctx = ctx, .arena = arena, .rebuild_intr = rebuild_intr, .interfere = interfere } };
    tb__ra_init(&ra.base, arena);
    ra.splits = dyn_array_create(SplitDecision, 32);

    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;
    int starting_spills = ra.base.num_spills;

    cuikperf_region_start("alloc regs", NULL);
    allocate_loop2(ctx, &ra, arena);
    ctx->num_spills += ra.base.num_spills - starting_spills;
    cuikperf_region_end();

    tb__ra_deinit(&ra.base);
    cuikperf_region_end();
}

static void future_active_put(Rogers* restrict ra, int class, int assigned, int gvn) {
    TB_OPTDEBUG(REGALLOC)(printf("#   \x1b[33msleep\x1b[0m  %%%u\n", gvn));
    TB_ASSERT(ra->next_inactive[gvn] == -1);

    // attach to chain
    int head = ra->future_active[class][assigned];
    ra->next_inactive[gvn] = head;
    ra->future_active[class][assigned] = gvn;
    set_put(&ra->is_inactive, gvn);
}

static bool future_active_test(Ctx* restrict ctx, Rogers* restrict ra, int gvn) {
    return set_get(&ra->is_inactive, gvn);
}

static void future_active_remove(Ctx* restrict ctx, Rogers* restrict ra, int class, int assigned, int gvn) {
    // remove from chain
    int prev = -1;
    int head = ra->future_active[class][assigned];
    while (head >= 0) {
        int next = ra->next_inactive[head];
        if (head == gvn) {
            if (prev < 0) {
                ra->future_active[class][assigned] = next;
            } else {
                ra->next_inactive[prev] = next;
            }
            break;
        }
        prev = head, head = next;
    }

    ra->next_inactive[gvn] = -1;
    set_remove(&ra->is_inactive, gvn);
}

static void future_active_remove_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    size_t cnt;
    TB_Node** arr = coalesce_set_array(&ra->base, &vreg->n, &cnt);
    FOR_N(j, 0, cnt) {
        future_active_remove(ctx, ra, vreg->class, vreg->assigned, arr[j]->gvn);
    }

    // reset assignment
    vreg->class = 0;
    vreg->assigned = -1;
}

static void future_active_put_vreg(Ctx* restrict ctx, Rogers* restrict ra, int class, int assigned, size_t cnt, TB_Node** arr, TB_Node* n, int vreg_id) {
    FOR_N(j, 0, cnt) {
        if (arr[j] != n) {
            TB_ASSERT(arr[j]->gvn < ctx->f->node_count);
            TB_ASSERT(ctx->vreg_map[arr[j]->gvn] == vreg_id);
            future_active_put(ra, class, assigned, arr[j]->gvn);
        }
    }
}

typedef struct {
    TB_Node* n;
    int vreg_id;
    float score;
} BestSpill;

static void future_active_interfere(Ctx* restrict ctx, Rogers* restrict ra, int class, int assigned, size_t cnt, TB_Node** arr, BestSpill* best) {
    int head = ra->future_active[class][assigned];
    while (head >= 0) {
        TB_Node* rhs = ra->gvn2node[head];
        int vreg_id  = ctx->vreg_map[head];

        // interfere against all nodes within vreg_id
        FOR_N(j, 0, cnt) {
            TB_Node* lhs = arr[j];
            if (interfere(ctx, &ra->base, lhs, rhs)) {
                float score = tb__ra_get_spill_cost(&ra->base, &ctx->vregs[vreg_id]);
                if (score <= best->score) {
                    best->n       = rhs;
                    best->score   = score;
                    best->vreg_id = vreg_id;
                }
            }
        }

        head = ra->next_inactive[head];
    }
}

// TB_Node** arr[cnt] is the body of the vreg_id
static VReg* future_active_interfere_first(Ctx* restrict ctx, Rogers* restrict ra, int class, int assigned, size_t cnt, TB_Node** arr) {
    int head = ra->future_active[class][assigned];
    while (head >= 0) {
        TB_Node* rhs = ra->gvn2node[head];

        // interfere against all nodes within vreg_id
        FOR_N(j, 0, cnt) {
            TB_Node* lhs = arr[j];
            if (interfere(ctx, &ra->base, lhs, rhs)) {
                return &ctx->vregs[ctx->vreg_map[head]];
            }
        }

        head = ra->next_inactive[head];
    }

    return NULL;
}

// check if RHS interferes with the def site of LHS
static bool interfere_with_point(Ctx* restrict ctx, Rogers* restrict ra, TB_BasicBlock* block, int t, TB_Node* rhs, uint32_t rhs_gvn) {
    int start_t = 0;
    if (ctx->f->scheduled[rhs_gvn] == block) {
        start_t = ra->order[rhs_gvn];
    } else if (!set_get(&block->live_in, rhs_gvn)) {
        // neither live in or defined here? then it can't interfere at all
        return false;
    }

    int end_t = last_use_in_bb(ctx->cfg.blocks, ctx->f->scheduled, ra, block, rhs, rhs_gvn);
    return start_t <= t && t < end_t;
}

// TB_Node** arr[cnt] is the body of the vreg_id
static VReg* future_active_interfere_point_first(Ctx* restrict ctx, Rogers* restrict ra, int class, int assigned, TB_Node* n) {
    TB_BasicBlock* block = ctx->f->scheduled[n->gvn];
    int t = ra->order[n->gvn];

    int head = ra->future_active[class][assigned];
    while (head >= 0) {
        TB_Node* rhs = ra->gvn2node[head];
        TB_ASSERT(rhs->gvn == head);

        if (interfere_with_point(ctx, ra, block, t, rhs, head)) {
            return &ctx->vregs[ctx->vreg_map[head]];
        }
        head = ra->next_inactive[head];
    }

    return NULL;
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    size_t new_cap = tb_next_pow2(ctx->f->node_count + 16);
    ra->order = tb_arena_alloc(arena, new_cap * sizeof(int));
    ra->gvn2node = tb_arena_alloc(arena, new_cap * sizeof(TB_Node*));
    ra->order_cap = new_cap;

    // just give the root node a fake ordinal
    TB_ASSERT(ctx->f->root_node->gvn == 0);
    ra->order[0] = 1;
    ra->gvn2node[0] = NULL;

    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        int timeline = 1;
        for (size_t j = 0; j < aarray_length(bb->items); j++) {
            TB_Node* n = bb->items[j];

            ra->gvn2node[n->gvn] = n;
            ra->order[n->gvn] = timeline++;
        }
    }
}

// Probably slow...
static DynArray(int) compute_areas(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena, DynArray(int) prio_queue) {
    size_t node_count = ctx->f->node_count;
    size_t new_cap = tb_next_pow2(node_count + 16);
    ra->order = tb_arena_alloc(arena, new_cap * sizeof(int));
    ra->gvn2node = tb_arena_alloc(arena, new_cap * sizeof(TB_Node*));
    ra->order_cap = new_cap;

    // just give the root node a fake ordinal
    TB_ASSERT(ctx->f->root_node->gvn == 0);
    ra->order[0] = 1;
    ra->gvn2node[0] = NULL;
    ra->is_vreg = tb_arena_alloc(arena, ((node_count + 63) / 64) * sizeof(uint64_t));

    CUIK_TIMED_BLOCK("areas") {
        // Sparse set repr
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        int* array = tb_arena_alloc(arena, node_count * sizeof(int));
        ArenaArray(int) stack = aarray_create(arena, int, 30);

        aarray_for(i, ctx->vregs) {
            ctx->vregs[i].area = 0;
        }

        FOR_N(i, 0, (node_count + 63) / 64) {
            uint64_t mask = 0;
            size_t end = i*64 + 64;
            if (end > node_count) { end = node_count; }

            FOR_N(j, 0, end - i*64) {
                size_t k = i*64 + j;
                if (ctx->vreg_map[k] > 0) {
                    mask |= 1ull << j;
                }
                array[k] = -1;
            }
            ra->is_vreg[i] = mask;
        }

        FOR_REV_N(i, 0, ctx->bb_count) {
            TB_BasicBlock* bb = &ctx->cfg.blocks[i];
            uint64_t freq = bb->freq >= 0.1 ? (bb->freq * 10) : 1;
            TB_ASSERT(freq > 0);

            // clear live
            aarray_for(j, stack) {
                array[stack[j]] = -1;
            }
            aarray_clear(stack);

            int last_phi = 0;
            do {
                TB_Node* n = bb->items[last_phi++];
                ra->gvn2node[n->gvn] = n;
                ra->order[n->gvn] = last_phi;
            } while (last_phi < aarray_length(bb->items) && (bb->items[last_phi]->type == TB_PHI || NODE_ISA(bb->items[last_phi], PROJ)));
            uint64_t inst_count = aarray_length(bb->items) - last_phi;

            // start int
            BITS64_FOR_AND(j, bb->live_out.data, ra->is_vreg, bb->live_out.capacity) {
                int vreg_id = ctx->vreg_map[j];
                TB_ASSERT(vreg_id > 0);

                TB_ASSERT(array[j] < 0);
                array[j] = aarray_length(stack);
                aarray_push(stack, j);

                VReg* v = &ctx->vregs[vreg_id];
                v->area += inst_count*freq;
                if (prio_queue != NULL) {
                    prio_queue = add_if_null(prio_queue, vreg_id);
                }
            }

            uint64_t proj_count = 0;
            FOR_REV_N(j, last_phi, aarray_length(bb->items)) {
                TB_Node* n = bb->items[j];

                // expire intervals
                if (array[n->gvn] >= 0) {
                    int last_gvn = stack[aarray_length(stack) - 1];
                    aarray_remove(stack, array[n->gvn]);
                    array[last_gvn] = array[n->gvn];
                    array[n->gvn] = -1;

                    // trim area
                    int vreg_id = ctx->vreg_map[n->gvn];
                    uint64_t delta = (proj_count + inst_count)*freq;
                    TB_ASSERT(ctx->vregs[vreg_id].area >= delta);
                    ctx->vregs[vreg_id].area -= delta;
                }

                // start intervals
                if (n->type != TB_PHI) {
                    FOR_N(k, 1, n->input_count) {
                        TB_Node* in = n->inputs[k];
                        if (in && ctx->vreg_map[in->gvn] > 0) {
                            // alive
                            if (array[in->gvn] < 0) {
                                array[in->gvn] = aarray_length(stack);
                                aarray_push(stack, in->gvn);

                                VReg* v = &ctx->vregs[ctx->vreg_map[in->gvn]];
                                v->area += (proj_count + inst_count)*freq;
                                if (prio_queue != NULL) {
                                    prio_queue = add_if_null(prio_queue, ctx->vreg_map[in->gvn]);
                                }
                            }
                        }
                    }
                }

                int kill_count = ctx->constraint_kill(ctx, n, ctx->ins);
                if (kill_count > 0) {
                    FOR_N(k, 0, kill_count) {
                        int vreg_id = aarray_length(ctx->vregs);
                        aarray_push(ctx->vregs, (VReg){ .n = n, .assigned = -1, .spill_cost = NAN });

                        VReg* kill_vreg = &ctx->vregs[vreg_id];
                        kill_vreg->mask = ctx->ins[k];
                        kill_vreg->area = 0;
                        kill_vreg->reg_width = 1;
                        kill_vreg->kill_lrg = true;
                        if (prio_queue != NULL) {
                            prio_queue = add_if_null(prio_queue, kill_vreg - ctx->vregs);
                        }
                    }
                }

                ra->gvn2node[n->gvn] = n;
                ra->order[n->gvn] = 1 + j;

                proj_count += IS_PROJ(n);
                inst_count--;
            }
        }

        tb_arena_restore(arena, sp);
    }
    return prio_queue;
}

static void allocate_loop2(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    size_t arena_size_start = tb_arena_current_size(arena);

    cuikperf_region_start("init", NULL);
    ra->inactive_cache = tb_arena_alloc(arena, (1ull << INACTIVE_CACHE_LOG2) * sizeof(InactiveCacheEntry));
    memset(ra->inactive_cache, 0, (1ull << INACTIVE_CACHE_LOG2) * sizeof(InactiveCacheEntry));
    cuikperf_region_end();

    ra->base.mask_cap = ra->base.max_regs_in_class;
    ra->base.mask = tb_arena_alloc(arena, ((ra->base.mask_cap+63)/64) * sizeof(uint64_t));

    TB_ArenaSavepoint sp = tb_arena_save(arena);
    ra->is_inactive = set_create(ctx->f->node_count);

    // Compute areas and find live intervals
    DynArray(int) prio_queue = dyn_array_create(int, aarray_length(ctx->vregs));
    prio_queue = compute_areas(ctx, ra, arena, prio_queue);

    // Sort by spill score
    compare_split__ctx = ctx;
    compare_split__ra = ra;
    qsort(prio_queue, dyn_array_length(prio_queue), sizeof(int), compare_split2);

    // Tracks VRegs which went live and represent clobbered ranges
    DynArray(int) kill_points = NULL;
    DynArray(int) stack_slots = NULL;

    // Tracking which nodes are allocated at this point
    ra->future_active[0] = NULL;
    FOR_N(i, 1, ctx->num_classes) {
        int nr = (ctx->num_regs[i] + 15) & -16;
        ra->future_active[i] = cuik_malloc(nr * sizeof(int));
        memset(ra->future_active[i], 0xFF, nr * sizeof(int));
    }
    ra->next_inactive = cuik_malloc(ctx->f->node_count * sizeof(int));
    memset(ra->next_inactive, 0xFF, ctx->f->node_count * sizeof(int));

    // Whenever a failure happens we create new VRegs and new nodes, this means we
    // need to reallocate a few different arrays.
    //
    // * Dataflow
    // * Order
    // * GVN2Node
    int rounds = 0;
    for (;;) {
        rounds++;

        // reset HRP regions
        /* FOR_N(i, 0, ctx->bb_count) {
        FOR_N(j, 1, ctx->num_classes) {
        ra->base.hrp[i].start[j] = -1;
        ra->base.hrp[i].end[j]   = -1;
        }
        } */

        while (dyn_array_length(prio_queue)) {
            int vreg_id = dyn_array_pop(prio_queue);
            VReg* vreg  = &ctx->vregs[vreg_id];
            if (vreg->n == NULL || vreg->mask->class == REG_CLASS_STK) {
                // We handle stack coloring later because it's not capable of
                // splitting or "failing"
                if (vreg->n != NULL && vreg->mask->class == REG_CLASS_STK) {
                    dyn_array_put(stack_slots, vreg_id);
                }
                continue;
            }

            double cost = tb__ra_get_spill_cost(&ra->base, vreg);

            printf("\n%s [ V%-4d cost=%10.2f ", vreg->kill_lrg ? "KILL " : "ALLOC", vreg_id, cost);
            tb__print_regmask(&OUT_STREAM_DEFAULT, vreg->mask);
            printf("]\n");

            if (vreg->kill_lrg) {
                printf("  * ");
                ctx->print_pretty(ctx, vreg->n);
                printf(" (%d uses)\n", vreg->n->user_count);
            } else {
                size_t cnt;
                TB_Node** arr = coalesce_set_array(&ra->base, &vreg->n, &cnt);
                FOR_N(j, 0, cnt) {
                    TB_Node* n = arr[j];

                    printf("  * ");
                    ctx->print_pretty(ctx, n);
                    printf(" (%d uses)\n", n->user_count);
                    TB_ASSERT(ctx->vreg_map[n->gvn] == vreg_id);

                    /* FOR_USERS(u, n) {
                    printf("||  ");
                    ctx->print_pretty(ctx, USERN(u));
                    printf("\n");
                    } */
                }
            }

            // Try to alloc:
            //   Each bit that's set is one we can't allocate from
            RegMask* mask = vreg->mask;
            size_t nr = ctx->num_regs[mask->class];
            uint64_t* ra_mask = ra->base.mask;
            {
                FOR_N(j, 0, mask->count) { ra_mask[j] = ~mask->mask[j]; }
                FOR_N(j, mask->count, (nr + 63) / 64) { ra_mask[j] = UINT64_MAX; }
                if (nr % 64) {
                    ra_mask[nr / 64] &= UINT64_MAX >> (64ull - (nr % 64));
                }
            }
            if (vreg->kill_lrg) {
                // Kill points don't "allocate" like normal vregs, they mostly just evict
                // existing VRegs and stop future ones from taking the space
                TB_Node* n = vreg->n;
                FOR_N(i, 0, ctx->num_regs[mask->class]) {
                    if ((ra_mask[i / 64ull] >> (i % 64)) & 1) {
                        continue;
                    }

                    VReg* first = future_active_interfere_point_first(ctx, ra, mask->class, i, n);
                    if (first != NULL) {
                        TB_ASSERT(first->reg_width == 1);
                        TB_ASSERT(first->class     == mask->class);
                        TB_ASSERT(first->assigned  == i);
                        ra_mask[first->assigned / 64ull] |= 1 << (first->assigned % 64ull);
                    }
                }

                // Verify that none of the kill bits are set
                bool good = true;
                FOR_N(i, 0, mask->count) {
                    if ((ra_mask[i] & mask->mask[i]) != 0) {
                        good = false;
                        break;
                    }
                }

                if (good) {
                    dyn_array_put(kill_points, vreg_id);
                } else {
                    // We need to retry this coloring after splitting
                    dyn_array_put(ra->prio_alloc, vreg_id);
                    printf("KILL FAILURE!!!\n");
                }
            } else {
                //  Interfere against all live ranges, we wanna cut this down later
                size_t cnt;
                TB_Node** arr = coalesce_set_array(&ra->base, &vreg->n, &cnt);
                FOR_N(i, 0, ctx->num_regs[mask->class]) {
                    if ((ra_mask[i / 64ull] >> (i % 64)) & 1) {
                        continue;
                    }

                    VReg* first = future_active_interfere_first(ctx, ra, mask->class, i, cnt, arr);
                    if (first != NULL) {
                        TB_ASSERT(first->reg_width == 1);
                        TB_ASSERT(first->class     == mask->class);
                        TB_ASSERT(first->assigned  == i);
                        ra_mask[first->assigned / 64ull] |= 1 << (first->assigned % 64ull);
                    }
                }
                // Interfere against kill points
                dyn_array_for(i, kill_points) {
                    int kill_vreg = kill_points[i];
                    RegMask* kill = ctx->vregs[kill_vreg].mask;
                    if (kill->class != mask->class) {
                        continue;
                    }

                    TB_Node* kill_site = ctx->vregs[kill_vreg].n;
                    TB_BasicBlock* block = ctx->f->scheduled[kill_site->gvn];
                    int t = ra->order[kill_site->gvn];

                    FOR_N(j, 0, cnt) {
                        if (interfere_with_point(ctx, ra, block, t, arr[j], arr[j]->gvn)) {
                            FOR_N(k, 0, kill->count) {
                                ra_mask[k] |= kill->mask[k];
                            }
                        }
                    }
                }

                // Choose to spill
                int hint_vreg = vreg->hint_vreg;
                int hint_reg = hint_vreg > 0
                    && ctx->vregs[hint_vreg].class == mask->class
                    ?  ctx->vregs[hint_vreg].assigned
                :  -1;

                if (hint_vreg < 0) {
                    hint_vreg = -hint_vreg;

                    int hint_class = hint_vreg >> 16;
                    hint_reg = hint_class == mask->class ? hint_vreg & 0xFFFF : -1;
                }

                int spilled = -1;
                if (hint_reg >= 0 && (ra_mask[hint_reg / 64ull] & (1ull << (hint_reg % 64ull))) == 0) {
                    vreg->class    = mask->class;
                    vreg->assigned = hint_reg;
                } else if (!reg_assign(ctx, vreg, ra_mask, ctx->num_regs[mask->class])) {
                    printf("  FAILURE!!!\n");

                    // Check interference against all nodes, not just the first
                    BestSpill best = { NULL, 0, INFINITY };
                    FOR_N(i, 0, ctx->num_regs[mask->class]) {
                        if (((mask->mask[i / 64ull] >> (i % 64)) & 1) == 0) {
                            continue;
                        }
                        future_active_interfere(ctx, ra, mask->class, i, cnt, arr, &best);
                    }

                    // Interfere against kill points
                    printf("  SPLIT AROUND KILL SITE: ");
                    dyn_array_for(i, kill_points) {
                        int kill_vreg = kill_points[i];
                        RegMask* kill = ctx->vregs[kill_vreg].mask;
                        if (kill->class != mask->class) {
                            continue;
                        }

                        TB_Node* kill_site = ctx->vregs[kill_vreg].n;
                        TB_BasicBlock* block = ctx->f->scheduled[kill_site->gvn];
                        int t = ra->order[kill_site->gvn];

                        FOR_N(j, 0, cnt) {
                            if (interfere_with_point(ctx, ra, block, t, arr[j], arr[j]->gvn)) {
                                mark_point_as_hrp(ctx, ra, kill_site, mask->class);
                                printf("  %%%u", kill_site->gvn);
                            }
                        }
                    }
                    printf("\n");

                    double our_score = tb__ra_get_spill_cost(&ra->base, vreg);
                    if (best.n != NULL) {
                        // First try to evict
                        VReg* other_vreg = &ctx->vregs[best.vreg_id];
                        if (our_score > best.score && other_vreg->stage == VREG_STAGE_EVICT) {
                            printf("  EVICT %%%u (V%d, score=%f)\n", best.n->gvn, best.vreg_id, best.score);

                            vreg->class    = mask->class;
                            vreg->assigned = other_vreg->assigned;

                            // reset other_vreg
                            future_active_remove_vreg(ctx, ra, other_vreg);
                            other_vreg->stage = VREG_STAGE_EVICT;

                            // we'll implement a real priority queue eventually
                            dyn_array_put(prio_queue, 0);
                            memmove(prio_queue + 1, prio_queue, (dyn_array_length(prio_queue) - 1) * sizeof(int));
                            prio_queue[0] = best.vreg_id;
                            continue;
                        }

                        // Can't evict, must split/spill now
                        FOR_N(i, 0, cnt) {
                            mark_node_as_hrp(ctx, ra, best.n->gvn, arr[i]->gvn, mask->class);
                        }
                    }

                    // If we're a "mayspill" then just become a stack slot now
                    if (mask->may_spill) {
                        printf("  ASSIGN TO SPILL (was MAYSPILL)\n");

                        vreg->spill_cost = INFINITY;
                        vreg->mask = intern_regmask(ctx, REG_CLASS_STK, true, 0);
                        vreg->reg_width = tb__reg_width_from_dt(REG_CLASS_STK, arr[0]->dt);

                        dyn_array_put(stack_slots, vreg_id);
                    } else if (our_score > best.score) {
                        printf("  SPILL %%%u (V%d, score=%f)\n", best.n->gvn, best.vreg_id, best.score);

                        TB_ASSERT(best.score != INFINITY);
                        dyn_array_put(ra->splits, (SplitDecision){ best.vreg_id });

                        VReg* spill_vreg = &ctx->vregs[best.vreg_id];
                        future_active_remove_vreg(ctx, ra, spill_vreg);
                    } else {
                        printf("  SPILL SELF (score=%f)\n", our_score);

                        TB_ASSERT(our_score != INFINITY);
                        dyn_array_put(ra->splits, (SplitDecision){ vreg_id });
                    }

                    // if we've collected failures we'll handle them together
                    if (dyn_array_length(ra->splits) >= 64) {
                        break;
                    }
                    continue;
                }

                printf("  ASSIGN TO ");
                print_reg_name(vreg->class, vreg->assigned);
                printf("\n");

                if (vreg->stage == VREG_STAGE_UNDEF) {
                    vreg->stage = VREG_STAGE_ASSIGN;
                }
                future_active_put_vreg(ctx, ra, vreg->class, vreg->assigned, cnt, arr, NULL, vreg_id);
            }
        }

        size_t num_spills = dyn_array_length(ra->splits);
        if (num_spills == 0) {
            // We're done coloring!
            break;
        }

        printf("  SPLIT BARRIER!!! %zu\n", num_spills);
        __builtin_debugtrap();

        size_t old_node_count = ctx->f->node_count;
        tb__insert_splits(ctx, &ra->base, ra->splits, num_spills);
        dyn_array_clear(ra->splits);

        // Reconstruct tmp arena state
        tb_arena_restore(arena, sp);
        {
            redo_dataflow(ctx, arena);
            // Reset cache since the order table is gonna change a lot
            memset(ra->inactive_cache, 0, (1ull << INACTIVE_CACHE_LOG2) * sizeof(InactiveCacheEntry));
            // Rebuild without putting anything on the prio queue
            compute_areas(ctx, ra, arena, NULL);
            // Resize next-inactive LL
            ra->next_inactive = cuik_realloc(ra->next_inactive, ctx->f->node_count * sizeof(int));
            FOR_N(i, old_node_count, ctx->f->node_count) {
                ra->next_inactive[i] = -1;
            }
        }

        // Compact dead entries
        size_t j = 0;
        dyn_array_for(i, ra->prio_alloc) {
            TB_Node* n = ctx->vregs[ra->prio_alloc[i]].n;
            if (n != NULL && n->type != TB_NULL) {
                ra->prio_alloc[j++] = ra->prio_alloc[i];
            }
        }
        dyn_array_set_length(ra->prio_alloc, j);

        // Sort by spill score
        compare_split__ctx = ctx;
        compare_split__ra = ra;
        qsort(ra->prio_alloc, dyn_array_length(ra->prio_alloc), sizeof(int), compare_split2);

        // Add dirty nodes from splitting into the prio queue at the top
        printf("  RETRYING %zu VREGS...\n  ", dyn_array_length(ra->prio_alloc));
        dyn_array_for(i, ra->prio_alloc) {
            printf("  V%d ", ra->prio_alloc[i]);
            prio_queue = add_if_null(prio_queue, ra->prio_alloc[i]);

            VReg* vreg = &ctx->vregs[ra->prio_alloc[i]];
            if (!vreg->kill_lrg && vreg->assigned >= 0) {
                future_active_remove_vreg(ctx, ra, vreg);
            }
        }
        printf("\n");
        dyn_array_clear(ra->prio_alloc);
    }

    dyn_array_for(i, stack_slots) {
        VReg* v = &ctx->vregs[stack_slots[i]];

        // just assign without interference... for now
        v->class = 0;
        v->assigned = ra->base.num_spills;
        ra->base.num_spills += v->reg_width;
    }

    __builtin_debugtrap();
    printf("DONE!\n");
}
