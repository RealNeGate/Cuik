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
    Ctx* ctx;
    TB_Arena* arena;

    int stack_usage;
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
    DynArray(LiveInterval*) unhandled;
    DynArray(LiveInterval*) inactive;

    Set active_set[MAX_REG_CLASSES];
    LiveInterval** active[MAX_REG_CLASSES];
} LSRA;

// Forward decls... yay
static void cuiksort_defs(LiveInterval** intervals, ptrdiff_t lo, ptrdiff_t hi);
static bool update_interval(LSRA* restrict ra, LiveInterval* interval, bool is_active, int time, int inactive_index);
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, LiveInterval* interval);
static LiveInterval* split_intersecting(LSRA* restrict ra, int pos, LiveInterval* interval, bool is_spill);
static void move_to_active(LSRA* restrict ra, LiveInterval* interval);

static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static const char* XMM_NAMES[] = { "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", "XMM6", "XMM7", "XMM8",  "XMM9", "XMM10", "XMM11", "XMM12", "XMM13", "XMM14", "XMM15" };
static const char* reg_name(int rg, int num) {
    return (rg == 1 ? XMM_NAMES : GPR_NAMES)[num];
}

// Helpers
static int interval_start(LiveInterval* l) { return l->ranges[l->range_count - 1].start; }
static int interval_end(LiveInterval* l)   { return l->ranges[1].end; }
static int interval_class(LiveInterval* l) { return l->mask.class; }

static int range_intersect(LiveRange* a, LiveRange* b) {
    if (b->start <= a->end && a->start <= b->end) {
        return a->start > b->start ? a->start : b->start;
    } else {
        return -1;
    }
}

static int interval_intersect(LiveInterval* a, LiveInterval* b) {
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

static void add_use_pos(LiveInterval* interval, int t, int kind) {
    UsePos u = { t, kind };
    dyn_array_put(interval->uses, u);
}

static void add_range(LiveInterval* interval, int start, int end) {
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

LiveInterval* gimme_interval_for_mask(Ctx* restrict ctx, TB_Arena* arena, LSRA* restrict ra, RegMask mask) {
    // not so fixed interval? we need a unique interval then
    LiveInterval* interval = TB_ARENA_ALLOC(arena, LiveInterval);
    *interval = (LiveInterval){
        .id = ctx->interval_count++,
        .mask = mask,
        .reg = -1,
        .assigned = -1,
        .range_cap = 2, .range_count = 1,
        .ranges = tb_platform_heap_alloc(4 * sizeof(LiveRange))
    };
    interval->ranges[0] = (LiveRange){ INT_MAX, INT_MAX };
    return interval;
}

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena) {
    LSRA ra = { .ctx = ctx, .arena = arena };

    // build intervals from dataflow
    CUIK_TIMED_BLOCK("build intervals") {
        FOREACH_REVERSE_N(i, 0, ctx->bb_count) {
            MachineBB* mbb = &ctx->machine_bbs[i];

            int bb_start = mbb->start->time;
            int bb_end = mbb->end->time + 2;

            // live outs define a full range across the BB (if they're defined
            // in the block, the later reverse walk will fix that up)
            Set* live_out = &mbb->live_out;
            FOREACH_N(j, 0, (ctx->interval_count + 63) / 64) {
                uint64_t bits = live_out->data[j];
                if (bits == 0) continue;

                FOREACH_N(k, 0, 64) if (bits & (1ull << k)) {
                    add_range(ctx->id2interval[j*64 + k], bb_start, bb_end);
                }
            }

            for (Tile* t = mbb->end; t; t = t->prev) {
                LiveInterval* interval = t->interval;
                if (interval == NULL) {
                    continue;
                }

                int time = t->time;
                interval->n = t->n;

                // mark output
                if (interval->mask.mask) {
                    int class = interval->mask.class;
                    int reg = fixed_reg_mask(interval->mask.mask);
                    dyn_array_put(ra.unhandled, interval);

                    if (interval->range_count == 1) {
                        add_range(interval, time, time);
                    } else {
                        interval->ranges[interval->range_count - 1].start = time;
                    }
                }

                // mark inputs
                FOREACH_N(j, 0, t->in_count) {
                    LiveInterval* in_def = t->ins[j].src;
                    RegMask in_def_mask = in_def->mask;
                    RegMask in_mask = t->ins[j].mask;

                    int hint = fixed_reg_mask(in_mask.mask);
                    in_def->hint = hint;

                    // if the use mask is more constrained than the def, we'll make a temporary
                    if ((in_def_mask.mask & in_mask.mask) != in_def->mask.mask) {
                        assert(in_def->mask.class == in_mask.class);
                        printf("  TEMP v%d (%#04llx) -> v%d (%#04llx)\n", interval->id, in_def_mask.mask, in_def->id, in_mask.mask);

                        // construct copy (either to a fixed interval or a new masked interval)
                        Tile* tmp = TB_ARENA_ALLOC(arena, Tile);
                        *tmp = (Tile){
                            .prev = t->prev,
                            .next = t,
                            .tag = TILE_SPILL_MOVE,
                            .time = t->time - 1,
                        };
                        tmp->ins = tb_arena_alloc(tmp_arena, sizeof(Tile*));
                        tmp->in_count = 1;
                        tmp->ins[0].src  = in_def;
                        tmp->ins[0].mask = in_def_mask;
                        t->prev->next = tmp;
                        t->prev = tmp;

                        // replace use site with temporary that legalized the constraint
                        tmp->interval = gimme_interval_for_mask(ctx, arena, &ra, in_mask);
                        t->ins[j].src = tmp->interval;

                        add_range(tmp->interval, bb_start, time);
                        add_use_pos(tmp->interval, time, USE_REG);
                    } else {
                        add_range(in_def, bb_start, time);
                        add_use_pos(in_def, time, USE_REG);
                    }
                }

                // this is how we place ranges even if the value isn't being inputted from anywhere.
                uint64_t clobbers[MAX_REG_CLASSES];
                if (ctx->clobbers(ctx, t, clobbers)) {
                    FOREACH_N(i, 0, ctx->num_classes) {
                        size_t j = 0;
                        for (uint64_t bits = clobbers[i]; bits; bits >>= 1, j += 1) {
                            if (bits & 1) {
                                LiveInterval* dummy = gimme_interval_for_mask(ctx, arena, &ra, (RegMask){ i, 1 << j });
                                add_range(dummy, time, time + 1);
                                dyn_array_put(ra.unhandled, dummy);
                            }
                        }
                    }
                }
            }
        }
    }

    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        ra.num_classes = ctx->num_classes;
        ra.num_regs = ctx->num_regs;
        ra.callee_saved = ctx->callee_saved;

        FOREACH_N(i, 0, ctx->num_classes) {
            if (max_regs_in_class < ctx->num_regs[i]) {
                max_regs_in_class = ctx->num_regs[i];
            }

            ra.active_set[i] = set_create_in_arena(arena, ctx->num_regs[i]);
            ra.active[i] = tb_arena_alloc(arena, ctx->num_regs[i] * sizeof(Tile*));
            memset(ra.active[i], 0, ctx->num_regs[i] * sizeof(Tile*));
        }

        // only need enough to store for the biggest register class
        ra.free_pos  = TB_ARENA_ARR_ALLOC(tmp_arena, max_regs_in_class, int);
        ra.block_pos = TB_ARENA_ARR_ALLOC(tmp_arena, max_regs_in_class, int);
    }

    ra.spills = TB_ARENA_ALLOC(arena, ctx->interval_count * sizeof(int));
    FOREACH_N(i, 0, ctx->interval_count) {
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
            LiveInterval* interval = dyn_array_pop(ra.unhandled);

            int time = interval_start(interval);
            int end = interval_end(interval);

            assert(time != INT_MAX);
            REG_ALLOC_LOG {
                printf("  # v%-4d t=[%-4d - %4d) [%#04llx]    ", interval->id, time, end, interval->mask.mask);
                if (interval->n) {
                    print_node_sexpr(interval->n, 0);
                }
                printf("\n");
            }

            // update intervals (inactive <-> active along with expiring)
            FOREACH_N(rc, 0, ctx->num_classes) {
                FOREACH_SET(reg, ra.active_set[rc]) {
                    update_interval(&ra, ra.active[rc][reg], true, time, -1);
                }
            }

            for (size_t i = 0; i < dyn_array_length(ra.inactive);) {
                LiveInterval* inactive = ra.inactive[i];
                if (update_interval(&ra, inactive, false, time, i)) {
                    continue;
                }
                i++;
            }

            ptrdiff_t reg = interval->reg;
            if (reg < 0) {
                reg = allocate_free_reg(&ra, interval);
                assert(reg >= 0 && "despair");
            }

            // add to active set
            if (reg >= 0) {
                interval->assigned = reg;
                move_to_active(&ra, interval);
            }

            // display active set
            REG_ALLOC_LOG {
                printf("  \x1b[32m{ ");
                FOREACH_N(rc, 0, ctx->num_classes) {
                    FOREACH_SET(reg, ra.active_set[rc]) {
                        LiveInterval* l = ra.active[rc][reg];
                        printf("v%d:%s ", l->id, reg_name(rc, reg));
                    }
                }
                printf("}\x1b[0m\n");
            }
        }
    }

    // move resolver:
    //   when a split happens, all indirect paths that cross the split will have
    //   moves inserted.
    ctx->spills = ra.spills;
    ctx->stack_usage = ra.stack_usage;
}

// returns -1 if no registers are available
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval_class(interval);
    uint64_t mask = interval->mask.mask;

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
        LiveInterval* other = ra->inactive[i];
        int fp = ra->free_pos[interval->assigned];
        if (fp > 0) {
            int p = interval_intersect(interval, other);
            if (p >= 0 && p < fp) {
                ra->free_pos[other->assigned] = p;
            }
        }
    }

    // it's better in the long run to aggressively split
    int hint_reg = interval->hint;
    int highest = -1;
    if (hint_reg >= 0 && interval_end(interval) >= ra->free_pos[hint_reg]) {
        highest = -1;
    }

    // pick highest free pos
    if (highest < 0) {
        highest = 0;
        FOREACH_N(i, 1, ra->num_regs[rc]) if (ra->free_pos[i] > ra->free_pos[highest]) {
            highest = i;
        }
    }

    int pos = ra->free_pos[highest];
    if (UNLIKELY(pos == 0)) {
        // alloc failure, split
        int reg = fixed_reg_mask(interval->mask.mask);
        if (reg >= 0) {
            // split whatever is using the interval right now
            assert(set_get(&ra->active_set[interval->mask.class], reg));
            LiveInterval* active_user = ra->active[interval->mask.class][reg];

            set_remove(&ra->active_set[interval->mask.class], reg);
            split_intersecting(ra, interval_start(interval) - 1, active_user, true);
            return reg;
        } else {
            tb_todo();
            return -1;
        }
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

        if (interval_end(interval) <= pos) {
            // we can steal it completely
            REG_ALLOC_LOG printf("  #   assign to %s", reg_name(rc, highest));

            if (interval->hint >= 0) {
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
            tb_todo();
            // split_intersecting(ra, pos - 1, interval, true);
        }

        return highest;
    }
}

static void insert_split_move(LSRA* restrict ra, int t, LiveInterval* old_it, LiveInterval* new_it) {
    // find which BB
    MachineBB* mbb = NULL;
    FOREACH_N(i, 0, ra->ctx->bb_count) {
        mbb = &ra->ctx->machine_bbs[i];
        if (t > mbb->start->time) break;
    }

    Tile *prev = NULL, *curr = mbb->start;
    while (curr != NULL) {
        if (curr->time > t) {
            break;
        }
        prev = curr, curr = curr->next;
    }

    Tile* move = TB_ARENA_ALLOC(ra->arena, Tile);
    *move = (Tile){
        .prev = prev,
        .next = prev->next,
        .tag = TILE_SPILL_MOVE,
        .time = prev->time + 1,
        .interval = new_it
    };
    move->ins = tb_arena_alloc(ra->arena, sizeof(Tile*));
    move->in_count = 1;
    move->ins[0].src  = old_it;
    move->ins[0].mask = old_it->mask;
    prev->next->prev = move;
    prev->next = move;
}

static LiveInterval* split_intersecting(LSRA* restrict ra, int pos, LiveInterval* interval, bool is_spill) {
    cuikperf_region_start("split", NULL);
    int rc = interval->mask.class;

    LiveInterval* restrict new_it = TB_ARENA_ALLOC(ra->arena, LiveInterval);
    *new_it = *interval;

    assert(is_spill != interval->is_spill);
    new_it->is_spill = is_spill;

    int sp_offset = ra->spills[interval->id];
    if (is_spill) {
        if (sp_offset == 0) {
            ra->stack_usage += 8;
            ra->spills[interval->id] = sp_offset = ra->stack_usage;
        }

        REG_ALLOC_LOG printf("  \x1b[33m#   v%d: spill %s to [SP - %d] at t=%d\x1b[0m\n", interval->id, reg_name(rc, interval->assigned), sp_offset, pos);
    } else {
        assert(sp_offset != 0);
        REG_ALLOC_LOG printf("  \x1b[33m#   v%d: reload [SP - %d] at t=%d\x1b[0m\n", interval->id, sp_offset, pos);
    }

    // split lifetime
    new_it->assigned = new_it->reg = -1;
    new_it->uses = NULL;
    new_it->split_kid = NULL;
    new_it->range_count = 0;

    assert(interval->split_kid == NULL && "cannot spill while spilled");
    interval->split_kid = new_it;

    if (!is_spill) {
        // since the split is starting at pos and pos is at the top of the
        // unhandled list... we can push this to the top wit no problem
        size_t i = 0, count = dyn_array_length(ra->unhandled);
        for (; i < count; i++) {
            if (pos > interval_start(ra->unhandled[i])) break;
        }

        // we know where to insert
        dyn_array_put(ra->unhandled, NULL);
        memmove(&ra->unhandled[i + 1], &ra->unhandled[i], (count - i) * sizeof(LiveInterval*));
        ra->unhandled[i] = new_it;
    }

    // split uses
    size_t use_count = dyn_array_length(interval->uses);
    FOREACH_REVERSE_N(i, 0, use_count) {
        size_t split_count = use_count - (i + 1);
        if (interval->uses[i].pos > pos && split_count > 0) {
            // split
            DynArray(UsePos) uses = dyn_array_create(UsePos, split_count);
            dyn_array_set_length(uses, split_count);
            memcpy(uses, &interval->uses[i + 1], split_count * sizeof(UsePos));

            dyn_array_set_length(interval->uses, i + 1);
            new_it->uses = interval->uses;
            interval->uses = uses;
            break;
        }
    }

    // split ranges
    size_t end = interval->range_count;
    FOREACH_REVERSE_N(i, 1, end) {
        LiveRange* range = &interval->ranges[i];
        if (range->end > pos) {
            bool clean_split = pos < range->start;

            LiveRange old = interval->ranges[interval->active_range];

            new_it->range_count = new_it->range_cap = i + 1;
            new_it->active_range = new_it->range_count - 1;
            new_it->ranges = interval->ranges;

            // move interval up, also insert INT_MAX and potentially
            size_t start = new_it->range_count - !clean_split;

            interval->range_count = interval->range_cap = (end - start) + 1;
            interval->ranges = tb_platform_heap_alloc(interval->range_count * sizeof(LiveRange));
            interval->active_range -= start - 1;
            interval->ranges[0] = (LiveRange){ INT_MAX, INT_MAX };

            FOREACH_N(j, start, end) {
                assert(j - start + 1 < interval->range_count);
                interval->ranges[j - start + 1] = new_it->ranges[j];
            }

            assert(interval->ranges[interval->active_range].start == old.start);
            assert(interval->ranges[interval->active_range].end == old.end);

            if (range->start <= pos) {
                interval->ranges[1].end = pos;
                new_it->ranges[new_it->range_count - 1].start = pos;
            }
            break;
        }
    }
    assert(new_it->range_count != 0);

    // insert move (the control flow aware moves are inserted later)
    insert_split_move(ra, pos, interval, new_it);

    // reload before next use
    if (is_spill) {
        FOREACH_REVERSE_N(i, 0, dyn_array_length(new_it->uses)) {
            if (new_it->uses[i].kind == USE_REG) {
                // new split
                split_intersecting(ra, new_it->uses[i].pos - 1, new_it, false);
                break;
            }
        }
    }

    cuikperf_region_end();
    return new_it;
}

// update active range to match where the position is currently
static bool update_interval(LSRA* restrict ra, LiveInterval* interval, bool is_active, int time, int inactive_index) {
    // get to the right range first
    while (interval->ranges[interval->active_range].end <= time) {
        assert(interval->active_range > 0);
        interval->active_range -= 1;
    }

    int hole_end = interval->ranges[interval->active_range].start;
    int active_end = interval->ranges[interval->active_range].end;
    bool is_now_active = time >= hole_end;

    int rc = interval_class(interval);
    int reg = interval->assigned;

    if (interval->active_range == 0) { // expired
        if (is_active) {
            REG_ALLOC_LOG printf("  #   active %s has expired at t=%d (v%d)\n", reg_name(rc, reg), interval_end(interval), interval->id);
            set_remove(&ra->active_set[rc], reg);
        } else {
            REG_ALLOC_LOG printf("  #   inactive %s has expired at t=%d (v%d)\n", reg_name(rc, reg), interval_end(interval), interval->id);
            dyn_array_remove(ra->inactive, inactive_index);
            return true;
        }
    } else if (is_now_active != is_active) { // if we moved, change which list we're in
        if (is_now_active) { // inactive -> active
            REG_ALLOC_LOG printf("  #   inactive %s is active again (until t=%d, v%d)\n", reg_name(rc, reg), active_end, interval->id);

            move_to_active(ra, interval);
            dyn_array_remove(ra->inactive, inactive_index);
            return true;
        } else { // active -> inactive
            REG_ALLOC_LOG printf("  #   active %s is going quiet for now (until t=%d, v%d)\n", reg_name(rc, reg), hole_end, interval->id);

            set_remove(&ra->active_set[rc], reg);
            dyn_array_put(ra->inactive, interval);
        }
    }

    return false;
}

static void move_to_active(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval_class(interval), reg = interval->assigned;
    if (set_get(&ra->active_set[rc], reg)) {
        tb_panic("v%d: interval v%d should never be forced out, we should've accomodated them in the first place", interval->id, ra->active[rc][reg]->id);
    }

    set_put(&ra->active_set[rc], reg);
    ra->active[rc][reg] = interval;
}

////////////////////////////////
// Sorting unhandled list
////////////////////////////////
static size_t partition(LiveInterval** intervals, ptrdiff_t lo, ptrdiff_t hi) {
    int pivot = interval_start(intervals[(hi - lo) / 2 + lo]); // middle

    ptrdiff_t i = lo - 1, j = hi + 1;
    for (;;) {
        // Move the left index to the right at least once and while the element at
        // the left index is less than the pivot
        do { i += 1; } while (interval_start(intervals[i]) > pivot);

        // Move the right index to the left at least once and while the element at
        // the right index is greater than the pivot
        do { j -= 1; } while (interval_start(intervals[j]) < pivot);

        // If the indices crossed, return
        if (i >= j) return j;

        // Swap the elements at the left and right indices
        SWAP(LiveInterval*, intervals[i], intervals[j]);
    }
}

static void cuiksort_defs(LiveInterval** intervals, ptrdiff_t lo, ptrdiff_t hi) {
    if (lo >= 0 && hi >= 0 && lo < hi) {
        // get pivot
        size_t p = partition(intervals, lo, hi);

        // sort both sides
        cuiksort_defs(intervals, lo, p);
        cuiksort_defs(intervals, p + 1, hi);
    }
}
