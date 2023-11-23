// Linear scan register allocator:
//   https://ssw.jku.at/Research/Papers/Wimmer04Master/Wimmer04Master.pdf
#include "codegen.h"

static void cuiksort_defs(Tile** tiles, ptrdiff_t lo, ptrdiff_t hi);

typedef struct {
    DynArray(Tile*) unhandled;

    // spill slots will be used later, new intervals will alias spill slots
    // with whatever they might've been spilled from.
    int* spills;

    Tile* fixed[MAX_REG_CLASSES];
} LSRA;

static int tile_start(Tile* t) { return t->ranges[t->range_count - 1].start; }
static int tile_end(Tile* t)   { return t->ranges[1].end; }

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

        interval->ranges[interval->range_count++] = (LiveRange){ start, end };
    }
}

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena) {
    LSRA ra = { 0 };

    // create physical intervals
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("create physical intervals") {
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
        }
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
                            .tag = TILE_SPILL_MOVE,
                            .reg = -1, .hint = -1, .assigned = -1,
                            .mask = in_mask,
                            .time = t->time - 1,
                            .range_cap = 2, .range_count = 1,
                            .ranges = tb_platform_heap_alloc(2 * sizeof(LiveRange))
                        };
                        tmp->ranges[0] = (LiveRange){ INT_MAX, INT_MAX };
                        t->ins = tb_arena_alloc(tmp_arena, sizeof(Tile*));
                        t->in_count = 1;
                        t->ins[0].tile = in_def;
                        t->ins[0].mask = in_def->mask;
                        t->prev->next = tmp;

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

            if (1) {
                printf("  # v%-4d t=[%-4d - %4d)   ", tile->id, time, end);
                if (tile->n != NULL) {
                    print_node_sexpr(tile->n, 0);
                }
                printf("\n");
            }

        }

        __debugbreak();
    }

    // move resolver:
    //   when a split happens, all indirect paths that cross the split will have
    //   moves inserted.
    tb_todo();
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
