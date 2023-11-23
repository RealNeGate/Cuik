// Linear scan register allocator:
//   https://ssw.jku.at/Research/Papers/Wimmer04Master/Wimmer04Master.pdf
#include "codegen.h"

#ifdef NDEBUG
#define REG_ALLOC_LOG if (0)
#else
#define REG_ALLOC_LOG if (1)
#endif

#define FOREACH_SET(it, set) \
FOREACH_N(_i, 0, ((set).capacity + 63) / 64) FOREACH_BIT(it, _i*64, (set).data[_i])

typedef struct {
    int num_classes;
    int* num_regs;
    uint64_t* callee_saved;

    // time when the physical registers will be free again
    int* free_pos;
    int* block_pos;

    // spill slots will be used later, new intervals will alias spill slots
    // with whatever they might've been spilled from.
    int* spills;

    // waiting to get registers, sorted such that the top most item is the youngest
    DynArray(Tile*) unhandled;
    DynArray(Tile*) inactive;

    Set active_set[MAX_REG_CLASSES];
    Tile** active[MAX_REG_CLASSES];

    Tile* fixed[MAX_REG_CLASSES];
} LSRA;

// Forward decls... yay
static void cuiksort_defs(Tile** tiles, ptrdiff_t lo, ptrdiff_t hi);
static bool update_interval(LSRA* restrict ra, Tile* tile, bool is_active, int time, int inactive_index);
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, Tile* tile);
static void move_to_active(LSRA* restrict ra, Tile* tile);

static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static const char* XMM_NAMES[] = { "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7", "XMM8",  "XMM9", "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15" };
static const char* reg_name(int rg, int num) {
    return (rg == 1 ? XMM_NAMES : GPR_NAMES)[num];
}

// Helpers
static int tile_start(Tile* t) { return t->ranges[t->range_count - 1].start; }
static int tile_end(Tile* t)   { return t->ranges[1].end; }
static int tile_class(Tile* t) { return t->mask.class; }

static int range_intersect(LiveRange* a, LiveRange* b) {
    if (b->start <= a->end && a->start <= b->end) {
        return a->start > b->start ? a->start : b->start;
    } else {
        return -1;
    }
}

static int tile_intersect(Tile* a, Tile* b) {
    FOREACH_REVERSE_N(i, 1, a->active_range+1) {
        FOREACH_REVERSE_N(j, 1, b->active_range+1) {
            int t = range_intersect(&a->ranges[i], &b->ranges[j]);
            if (t >= 0) {
                return t;
            }
        }
    }

    return -1;
}

static void add_use_pos(Tile* interval, int t, int kind) {
    UsePos u = { t, kind };
    dyn_array_put(interval->uses, u);
}

static void add_range(Tile* interval, int start, int end) {
    assert(start <= end);
    assert(interval->range_count > 0);

    if (interval->ranges[interval->range_count - 1].start <= end) {
        LiveRange* top = &interval->ranges[interval->range_count - 1];

        // coalesce
        top->start = TB_MIN(top->start, start);
        top->end   = TB_MAX(top->end,   end);
    } else {
        if (interval->range_cap == interval->range_count) {
            interval->range_cap *= 2;
            interval->ranges = tb_platform_heap_realloc(interval->ranges, interval->range_cap * sizeof(LiveRange));
        }

        interval->active_range = interval->range_count;
        interval->ranges[interval->range_count++] = (LiveRange){ start, end };
    }
}

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena) {
    LSRA ra = { 0 };

    // create physical intervals
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("create physical intervals") {
        ra.num_classes = ctx->num_classes;
        ra.num_regs = ctx->num_regs;
        ra.callee_saved = ctx->callee_saved;

        FOREACH_N(i, 0, ctx->num_classes) {
            if (max_regs_in_class < ctx->num_regs[i]) {
                max_regs_in_class = ctx->num_regs[i];
            }

            Tile* tiles = TB_ARENA_ALLOC(arena, ctx->num_regs[i] * sizeof(Tile));
            FOREACH_N(j, 0, ctx->num_regs[i]) {
                tiles[j] = (Tile){
                    .tag = TILE_NORMAL,
                    .assigned = j, .hint = j, .reg = j,
                    .mask = { i, 1u << j },
                    .id = -1,
                    .range_cap = 4,
                };
                tiles[j].ranges = tb_platform_heap_alloc(tiles[j].range_cap * sizeof(LiveRange));
            }
            ra.fixed[i] = tiles;

            ra.active_set[i] = set_create_in_arena(arena, ctx->num_regs[i]);
            ra.active[i] = tb_arena_alloc(arena, ctx->num_regs[i] * sizeof(Tile*));
            memset(ra.active[i], 0, ctx->num_regs[i] * sizeof(Tile*));
        }

        // only need enough to store for the biggest register class
        ra.free_pos  = TB_ARENA_ARR_ALLOC(tmp_arena, max_regs_in_class, int);
        ra.block_pos = TB_ARENA_ARR_ALLOC(tmp_arena, max_regs_in_class, int);
    }

    // build intervals from dataflow
    CUIK_TIMED_BLOCK("build intervals") {
        FOREACH_REVERSE_N(i, 0, ctx->bb_count) {
            MachineBB* mbb = &ctx->machine_bbs[i];

            int bb_start = mbb->start->time;
            int bb_end = mbb->end->time + 2;

            // live outs define a full range across the BB (if they're defined
            // in the block, the later reverse walk will fix that up)
            Set* live_out = &mbb->live_out;
            FOREACH_N(j, 0, (ctx->tile_count + 63) / 64) {
                uint64_t bits = live_out->data[j];
                if (bits == 0) continue;

                FOREACH_N(k, 0, 64) if (bits & (1ull << k)) {
                    add_range(ctx->id2tile[j*64 + k], bb_start, bb_end);
                }
            }

            for (Tile* t = mbb->end; t; t = t->prev) {
                TB_Node* n = t->n;
                int time = t->time;

                // mark output
                if (t->mask.mask) {
                    if (t->range_count == 1) {
                        add_range(t, time, time);
                    } else {
                        t->ranges[t->range_count - 1].start = time;
                    }

                    dyn_array_put(ra.unhandled, t);
                }

                // mark inputs
                FOREACH_N(j, 0, t->in_count) {
                    Tile* in_def = t->ins[j].tile;
                    RegMask in_mask = t->ins[j].mask;

                    // if the use mask is more constrained than the def, we'll make a temporary
                    if ((in_def->mask.mask & in_mask.mask) != in_def->mask.mask) {
                        // temporary forces the use of the fixed interval as late as possible (to avoid bad
                        // constraints) but hints early.
                        assert(in_def->mask.class == in_mask.class);
                        printf("  TEMP "), print_node_sexpr(n, 0), printf(" %zu (def=%#04llx, use=%#04llx)\n", j, in_def->mask.mask, in_mask.mask);

                        Tile* tmp = TB_ARENA_ALLOC(arena, Tile);
                        *tmp = (Tile){
                            .prev = t->prev,
                            .next = t,
                            .id = ctx->tile_count++,
                            .tag = TILE_SPILL_MOVE,
                            .reg = -1, .hint = -1, .assigned = -1,
                            .mask = in_mask,
                            .time = t->time - 1,
                            .range_cap = 2, .range_count = 1,
                            .ranges = tb_platform_heap_alloc(2 * sizeof(LiveRange))
                        };
                        tmp->ranges[0] = (LiveRange){ INT_MAX, INT_MAX };
                        tmp->ins = tb_arena_alloc(tmp_arena, sizeof(Tile*));
                        tmp->in_count = 1;
                        tmp->ins[0].tile = in_def;
                        tmp->ins[0].mask = in_def->mask;

                        t->prev->next = tmp;
                        t->prev = tmp;

                        if (tb_popcount64(in_mask.mask) == 1) {
                            int hint = 63 - tb_clz64(in_mask.mask);
                            in_def->hint = hint;
                        }

                        // replace use site with temporary that legalized the constraint
                        t->ins[j].tile = in_def = tmp;
                    }

                    add_range(in_def, bb_start, time);
                    add_use_pos(in_def, time, USE_REG);
                }
            }
        }
    }

    ra.spills = TB_ARENA_ALLOC(arena, ctx->tile_count * sizeof(int));
    FOREACH_N(i, 0, ctx->tile_count) {
        ra.spills[i] = 0;
    }

    // sort intervals:
    CUIK_TIMED_BLOCK("sort intervals") {
        cuiksort_defs(ra.unhandled, 0, dyn_array_length(ra.unhandled) - 1);
    }

    // linear scan:
    //   expire old => allocate free or spill/split => rinse & repeat.
    CUIK_TIMED_BLOCK("linear scan") {
        while (dyn_array_length(ra.unhandled)) {
            Tile* tile = dyn_array_pop(ra.unhandled);

            int time = tile_start(tile);
            int end = tile_end(tile);

            assert(tile->assigned < 0);
            assert(tile->reg < 0);
            assert(time != INT_MAX);

            REG_ALLOC_LOG {
                printf("  # v%-4d t=[%-4d - %4d)   ", tile->id, time, end);
                if (tile->n != NULL) {
                    print_node_sexpr(tile->n, 0);
                }
                printf("\n");
            }

            // update intervals (inactive <-> active along with expiring)
            FOREACH_N(rc, 0, ctx->num_classes) {
                FOREACH_SET(reg, ra.active_set[rc]) {
                    update_interval(&ra, ra.active[rc][reg], true, time, -1);
                }
            }

            // allocate free reg
            ptrdiff_t reg = allocate_free_reg(&ra, tile);

            // add to active set
            if (reg >= 0) {
                tile->assigned = reg;
                move_to_active(&ra, tile);
            }

            // display active set
            REG_ALLOC_LOG {
                printf("  \x1b[32m{ ");
                FOREACH_N(rc, 0, ctx->num_classes) {
                    FOREACH_SET(reg, ra.active_set[rc]) {
                        Tile* t = ra.active[rc][reg];
                        if (t->reg < 0) {
                            printf("v%d:", t->id);
                        }
                        printf("%s ", reg_name(rc, reg));
                    }
                }
                printf("}\x1b[0m\n");
            }
        }

        __debugbreak();
    }

    // move resolver:
    //   when a split happens, all indirect paths that cross the split will have
    //   moves inserted.
}

// returns -1 if no registers are available
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, Tile* tile) {
    int rc = tile_class(tile);
    uint64_t mask = tile->mask.mask;

    // callee saved will be biased to have nearer free positions to avoid incurring
    // a spill on them early.
    int half_free = INT_MAX >> 1;
    FOREACH_N(i, 0, ra->num_regs[rc]) {
        int p = 0;

        // it can be allocated and isn't in use rn
        if ((mask & (1u << i)) && !set_get(&ra->active_set[rc], i)) {
            p = INT_MAX;
            if (ra->callee_saved[rc] & (1ull << i)) {
                p = half_free;
            }
        }

        ra->free_pos[i] = p;
    }

    // for each inactive which intersects current
    dyn_array_for(i, ra->inactive) {
        Tile* other = ra->inactive[i];
        int fp = ra->free_pos[tile->assigned];
        if (fp > 0) {
            int p = tile_intersect(tile, other);
            if (p >= 0 && p < fp) {
                ra->free_pos[other->assigned] = p;
            }
        }
    }

    // it's better in the long run to aggressively split
    int hint_reg = tile->hint;
    int highest = hint_reg >= 0 ? hint_reg : -1;

    // pick highest free pos
    if (highest < 0) {
        highest = 15;
        FOREACH_REVERSE_N(i, 0, 15) if (ra->free_pos[i] > ra->free_pos[highest]) {
            highest = i;
        }
    }

    int pos = ra->free_pos[highest];
    if (UNLIKELY(pos == 0)) {
        // alloc failure
        return -1;
    } else {
        if (UNLIKELY(ra->callee_saved[rc] & (1ull << highest))) {
            ra->callee_saved[rc] &= ~(1ull << highest);

            REG_ALLOC_LOG printf("  #   spill callee saved register %s\n", reg_name(rc, highest));
            tb_todo();

            /*int size = rc ? 16 : 8;
            int vreg = (rc ? FIRST_XMM : FIRST_GPR) + highest;
            ra->stack_usage = align_up(ra->stack_usage + size, size);

            SpillSlot* s = TB_ARENA_ALLOC(tmp_arena, SpillSlot);
            s->pos = ra->stack_usage;

            LiveInterval it = {
                .is_spill = true,
                .spill = s,
                .dt = ra->intervals[vreg].dt,
                .assigned = -1,
                .reg = -1,
                .split_kid = -1,
            };

            int old_reg = interval - ra->intervals;
            int spill_slot = dyn_array_length(ra->intervals);
            dyn_array_put(ra->intervals, it);

            // insert spill and reload
            insert_split_move(ra, 0, vreg, spill_slot);
            dyn_array_for(i, ra->epilogues) {
                insert_split_move(ra, ra->epilogues[i] - 1, spill_slot, vreg);
            }

            // adding to intervals might resized this
            interval = &ra->intervals[old_reg];*/
        }

        if (tile_end(tile) <= pos) {
            // we can steal it completely
            REG_ALLOC_LOG printf("  #   assign to %s", reg_name(rc, highest));

            if (tile->hint >= 0) {
                if (highest == hint_reg) {
                    REG_ALLOC_LOG printf(" (HINTED)\n");
                } else {
                    REG_ALLOC_LOG printf(" (FAILED HINT %s)\n", reg_name(rc, hint_reg));
                }
            } else {
                REG_ALLOC_LOG printf("\n");
            }
        } else {
            // TODO(NeGate): split current at optimal position before current
            tile->assigned = highest;
            tb_todo();
            // split_intersecting(ra, pos - 1, interval, true);
        }

        return highest;
    }
}

// update active range to match where the position is currently
static bool update_interval(LSRA* restrict ra, Tile* tile, bool is_active, int time, int inactive_index) {
    // get to the right range first
    while (tile->ranges[tile->active_range].end <= time) {
        assert(tile->active_range > 0);
        tile->active_range -= 1;
    }

    int hole_end = tile->ranges[tile->active_range].start;
    int active_end = tile->ranges[tile->active_range].end;
    bool is_now_active = time >= hole_end;

    int rc = tile_class(tile);
    int reg = tile->assigned;

    if (tile->active_range == 0) { // expired
        if (is_active) {
            REG_ALLOC_LOG printf("  #   active %s has expired (v%d)\n", reg_name(rc, reg), tile->id);
            set_remove(&ra->active_set[rc], reg);
        } else {
            REG_ALLOC_LOG printf("  #   inactive %s has expired (v%d)\n", reg_name(rc, reg), tile->id);
            dyn_array_remove(ra->inactive, inactive_index);
            return true;
        }
    } else if (is_now_active != is_active) { // if we moved, change which list we're in
        if (is_now_active) { // inactive -> active
            REG_ALLOC_LOG printf("  #   inactive %s is active again (until t=%d, v%d)\n", reg_name(rc, reg), active_end, tile->id);

            move_to_active(ra, tile);
            dyn_array_remove(ra->inactive, inactive_index);
            return true;
        } else { // active -> inactive
            REG_ALLOC_LOG printf("  #   active %s is going quiet for now (until t=%d, v%d)\n", reg_name(rc, reg), hole_end, tile->id);

            set_remove(&ra->active_set[rc], reg);
            dyn_array_put(ra->inactive, tile);
        }
    }

    return false;
}

static void move_to_active(LSRA* restrict ra, Tile* tile) {
    int rc = tile_class(tile), reg = tile->assigned;
    if (set_get(&ra->active_set[rc], reg)) {
        tb_panic("v%d: interval v%d should never be forced out, we should've accomodated them in the first place", tile->id, ra->active[rc][reg]->id);
    }

    set_put(&ra->active_set[rc], reg);
    ra->active[rc][reg] = tile;
}

////////////////////////////////
// Sorting unhandled list
////////////////////////////////
static size_t partition(Tile** tiles, ptrdiff_t lo, ptrdiff_t hi) {
    int pivot = tile_start(tiles[(hi - lo) / 2 + lo]); // middle

    ptrdiff_t i = lo - 1, j = hi + 1;
    for (;;) {
        // Move the left index to the right at least once and while the element at
        // the left index is less than the pivot
        do { i += 1; } while (tile_start(tiles[i]) > pivot);

        // Move the right index to the left at least once and while the element at
        // the right index is greater than the pivot
        do { j -= 1; } while (tile_start(tiles[j]) < pivot);

        // If the indices crossed, return
        if (i >= j) return j;

        // Swap the elements at the left and right indices
        SWAP(Tile*, tiles[i], tiles[j]);
    }
}

static void cuiksort_defs(Tile** tiles, ptrdiff_t lo, ptrdiff_t hi) {
    if (lo >= 0 && hi >= 0 && lo < hi) {
        // get pivot
        size_t p = partition(tiles, lo, hi);

        // sort both sides
        cuiksort_defs(tiles, lo, p);
        cuiksort_defs(tiles, p + 1, hi);
    }
}
