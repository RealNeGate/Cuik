// https://ssw.jku.at/Research/Papers/Wimmer04Master/Wimmer04Master.pdf
#define REG_ALLOC_LOG if (reg_alloc_log)

typedef struct {
    int start, end;
} LiveRange;

typedef struct {
    int pos;

    // this is used to denote folded reloads
    //
    //   # r2 is spilled
    //   add r0, r1, r2
    //
    // when we codegen, we don't need to allocate
    // a register for r2 here.
    //
    //   add r0, r1, [sp - 24]
    enum {
        USE_OUT,
        USE_REG,
        USE_MEM_OR_REG,
    } kind;
} UsePos;

struct LiveInterval {
    int reg_class;

    TB_Node* n;
    TB_X86_DataType dt;

    // results of regalloc
    int assigned;

    // register num, -1 if the interval isn't a physical reg
    int reg, hint;

    // whole interval
    int start, end;

    // spill point, -1 if there's none
    int spill, split_kid;

    // we're gonna have so much memory to clean up...
    DynArray(LiveRange) ranges;
    DynArray(UsePos) uses;
};

typedef DynArray(RegIndex) IntervalList;

typedef struct {
    TB_ABI abi;

    DynArray(LiveInterval) intervals;
    DynArray(RegIndex) inactive;
    IntervalList unhandled;
    Inst* first;

    int stack_usage;

    // time when the physical registers will be free again
    int* free_pos;
    int* use_pos;
    int* block_pos;

    Set active_set[CG_REGISTER_CLASSES];
    RegIndex active[CG_REGISTER_CLASSES][16];
} LSRA;

static LiveRange* last_range(LiveInterval* i) {
    return &i->ranges[dyn_array_length(i->ranges) - 1];
}

////////////////////////////////
// Generate intervals
////////////////////////////////
static void add_use_pos(LiveInterval* interval, int t, int kind) {
    UsePos u = { t, kind };
    dyn_array_put(interval->uses, u);
}

static void add_range(LiveInterval* interval, int start, int end) {
    LiveRange r = { start, end };
    dyn_array_put(interval->ranges, r);

    if (start < interval->start) interval->start = start;
    if (end < interval->start) interval->start = end;

    // max
    if (start > interval->end) interval->end = start;
    if (end > interval->end) interval->end = end;
}

static void reverse_bb_walk(LSRA* restrict ra, MachineBB* bb, Inst* inst) {
    Inst* next = inst->next;
    if (next && next->type != INST_LABEL) {
        reverse_bb_walk(ra, bb, next);
    }

    // mark outputs, inputs and temps
    //
    // TODO(NeGate): on x86 we can have one memory operand per instruction.
    // we shouldn't force only register uses or else we'll make spilling more
    // prominent.
    RegIndex* ops = inst->operands;
    FOREACH_N(i, 0, inst->out_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        if (interval->ranges == NULL) {
            add_range(interval, inst->time, inst->time);
        } else {
            interval->start = inst->time;
            interval->ranges[dyn_array_length(interval->ranges) - 1].start = inst->time;
        }

        add_use_pos(interval, inst->time, inst->type == IMUL ? USE_REG : USE_OUT);
    }

    FOREACH_N(i, 0, inst->in_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        add_range(interval, bb->start, inst->time);
        add_use_pos(interval, inst->time, USE_REG);
    }

    // calls use the temporaries for clobbers
    bool is_call = (inst->type == CALL || inst->type == SYSCALL);
    FOREACH_N(i, 0, inst->tmp_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        add_range(interval, inst->time, inst->time + 1);
        if (!is_call) {
            add_use_pos(interval, inst->time, USE_REG);
        }
    }
}

static bool interval_intersect(LiveInterval* a, LiveInterval* b) {
    return b->start <= a->end && a->start <= b->end;
}

#define FOREACH_SET(it, set) \
FOREACH_N(_i, 0, ((set).capacity + 63) / 64) \
for (uint64_t bits = (set).data[_i], it = _i*64; bits; bits >>= 1, it++) if (bits & 1)

static int next_use(LiveInterval* interval, int time) {
    FOREACH_N(i, 0, dyn_array_length(interval->uses)) {
        if (interval->uses[i].pos > time) {
            return interval->uses[i].pos;
        }
    }

    return INT_MAX;
}

static int covers(LiveInterval* it, int time) {
    FOREACH_N(i, 0, dyn_array_length(it->ranges)) {
        if (time >= it->ranges[i].start && time < it->ranges[i].end) {
            return it->ranges[i].end;
        }
    }

    return -1;
}

static LiveInterval* get_active(LSRA* restrict ra, int rc, int reg) {
    if (!set_get(&ra->active_set[rc], reg)) {
        return NULL;
    }

    return &ra->intervals[ra->active[rc][reg]];
}

static void insert_split_move(LSRA* restrict ra, int t, int old_reg, int new_reg) {
    Inst *prev = ra->first, *inst = prev->next;
    TB_X86_DataType dt = ra->intervals[old_reg].dt;

    while (inst != NULL) {
        if (inst->time > t) {
            break;
        }

        prev = inst, inst = inst->next;
    }

    // folded spill
    if (inst->type == MOV && inst->flags == 0 && inst->operands[0] == old_reg) {
        inst->operands[0] = new_reg;
        return;
    }

    Inst* new_inst = tb_arena_alloc(tmp_arena, sizeof(Inst) + (2 * sizeof(RegIndex)));
    *new_inst = (Inst){ .type = MOV, .dt = dt, .out_count = 1, 1 };
    new_inst->operands[0] = new_reg;
    new_inst->operands[1] = old_reg;
    new_inst->time = prev->time + 1;
    new_inst->next = prev->next;
    prev->next = new_inst;
}

static LiveInterval* split_interval_at(LSRA* restrict ra, LiveInterval* interval, int pos) {
    // skip past previous intervals
    while (interval->split_kid >= 0 && pos > interval->end) {
        interval = &ra->intervals[interval->split_kid];
    }

    return interval;
}

// any uses after `pos` after put into the new interval
static int split_intersecting(LSRA* restrict ra, int pos, LiveInterval* interval, bool is_spill) {
    if (interval->spill > 0) {
        REG_ALLOC_LOG printf("  \x1b[33m#   reload [RBP - %d] at t=%d\x1b[0m\n", interval->spill, pos);
    } else {
        // allocate stack slot
        int size = 8;
        ra->stack_usage = align_up(ra->stack_usage + size, size);

        REG_ALLOC_LOG printf("  \x1b[33m#   spill %s to [RBP - %d] at t=%d\x1b[0m\n", reg_name(interval->reg_class, interval->assigned), ra->stack_usage, pos);
    }

    // split lifetime
    LiveInterval it = *interval;
    if (is_spill) {
        it.spill = ra->stack_usage;
    } else {
        it.spill = -1;
        it.reg = -1;
    }
    it.assigned = it.reg = -1;
    it.start = pos;
    it.end = interval->end;
    it.uses = NULL;
    it.ranges = NULL;
    it.n = NULL;
    it.split_kid = -1;
    interval->end = pos;

    int old_reg = interval - ra->intervals;
    int new_reg = dyn_array_length(ra->intervals);
    interval->split_kid = new_reg;

    dyn_array_put(ra->intervals, it);
    interval = &ra->intervals[old_reg];

    if (!is_spill) {
        // since the split is starting at pos and pos is at the top of the
        // unhandled list... we can push this to the top wit no problem
        size_t i = 0, count = dyn_array_length(ra->unhandled);
        for (; i < count; i++) {
            if (pos > ra->intervals[ra->unhandled[i]].start) break;
        }

        // we know where to insert
        dyn_array_put(ra->unhandled, 0);
        memmove(&ra->unhandled[i + 1], &ra->unhandled[i], (count - i) * sizeof(RegIndex));
        ra->unhandled[i] = new_reg;
    }

    // split uses
    size_t use_count = dyn_array_length(interval->uses);
    FOREACH_REVERSE_N(i, 0, use_count) {
        if (interval->uses[i].pos > pos) {
            size_t split_count = use_count - (i + 1);
            DynArray(UsePos) uses = dyn_array_create(UsePos, split_count);
            dyn_array_set_length(uses, split_count);
            memcpy(uses, &interval->uses[i + 1], split_count * sizeof(UsePos));

            dyn_array_set_length(interval->uses, i + 1);
            it.uses = interval->uses;
            interval->uses = uses;
            break;
        }
    }

    // split ranges
    for (size_t i = 0; i < dyn_array_length(interval->ranges);) {
        LiveRange* range = &interval->ranges[i];
        if (range->start > pos) {
            dyn_array_put(it.ranges, *range);
            interval = &ra->intervals[old_reg];

            // remove-shift
            size_t shift = dyn_array_length(interval->ranges) - (i + 1);
            if (shift > 0) {
                memmove(range, range + 1, shift * sizeof(LiveRange));
            }
            dyn_array_pop(interval->ranges);
        } else if (range->end > pos) {
            // intersects pos, we need to split the range
            LiveRange r = { pos, range->end };
            range->end = pos;
            dyn_array_put(it.ranges, r);

            interval = &ra->intervals[old_reg];
            continue;
        }

        i++;
    }

    ra->intervals[new_reg] = it;

    // insert move (the control flow aware moves are inserted later)
    insert_split_move(ra, pos, old_reg, new_reg);

    // reload before next use
    if (is_spill) {
        FOREACH_REVERSE_N(i, 0, dyn_array_length(it.uses)) {
            if (it.uses[i].kind == USE_REG) {
                // new split
                split_intersecting(ra, it.uses[i].pos - 1, &ra->intervals[new_reg], false);
                break;
            }
        }
    }

    return new_reg;
}

// returns -1 if no registers are available
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval->reg_class;

    FOREACH_N(i, 0, 16) ra->free_pos[i] = INT_MAX;

    // for each active reg, set the free pos to 0
    FOREACH_SET(i, ra->active_set[rc]) {
        ra->free_pos[i] = 0;
    }

    // for each inactive which intersects current
    dyn_array_for(i, ra->inactive) {
        LiveInterval* inactive_it = &ra->intervals[ra->inactive[i]];
        if (interval_intersect(interval, inactive_it)) {
            ra->free_pos[i] = interval->start;
        }
    }

    if (rc == REG_CLASS_GPR) {
        // reserved regs
        ra->free_pos[RBP] = 0;
        ra->free_pos[RSP] = 0;
    }

    // pick highest free pos
    int highest = 0;
    FOREACH_N(i, 1, 16) if (ra->free_pos[i] > ra->free_pos[highest]) {
        highest = i;
    }

    int pos = ra->free_pos[highest];
    if (pos == 0) {
        // alloc failure
        return -1;
    } else if (pos > interval->end || pos == INT_MAX) {
        // we can steal it completely
        REG_ALLOC_LOG printf("  #   assign to %s\n", reg_name(rc, highest));
        return highest;
    } else {
        // TODO(NeGate): split current at optimal position before current
        interval->assigned = highest;
        split_intersecting(ra, pos - 1, interval, true);
        return highest;
    }
}

static ptrdiff_t allocate_blocked_reg(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval->reg_class;

    FOREACH_N(i, 0, 16) ra->block_pos[i] = INT_MAX;
    FOREACH_N(i, 0, 16) ra->use_pos[i] = INT_MAX;

    // mark non-fixed intervals
    FOREACH_SET(i, ra->active_set[rc]) {
        LiveInterval* it = &ra->intervals[ra->active[rc][i]];
        if (it->reg_class == rc && it->reg < 0) {
            ra->use_pos[i] = next_use(it, interval->start);
        }
    }

    dyn_array_for(i, ra->inactive) {
        LiveInterval* it = &ra->intervals[ra->inactive[i]];
        if (it->reg_class == rc && it->reg < 0) {
            ra->use_pos[i] = next_use(it, interval->start);
        }
    }

    // mark fixed intervals
    FOREACH_SET(i, ra->active_set[rc]) {
        LiveInterval* it = &ra->intervals[ra->active[rc][i]];
        if (it->reg_class == rc && it->reg >= 0) {
            ra->use_pos[i] = 0;
            ra->block_pos[i] = 0;
        }
    }

    dyn_array_for(i, ra->inactive) {
        LiveInterval* it = &ra->intervals[ra->inactive[i]];
        if (it->reg_class == rc && it->reg >= 0) {
            __debugbreak();
        }
    }

    if (rc == REG_CLASS_GPR) {
        // reserved regs
        ra->use_pos[RBP] = 0;
        ra->use_pos[RSP] = 0;
    }

    // pick highest use pos
    int highest = 0;
    FOREACH_N(i, 1, 16) if (ra->use_pos[i] > ra->use_pos[highest]) {
        highest = i;
    }

    int pos = ra->use_pos[highest];
    int first_use = interval->uses[dyn_array_length(interval->uses) - 1].pos;
    if (pos < first_use) {
        int size = 8;

        // spill interval
        ra->stack_usage = align_up(ra->stack_usage + size, size);
        interval->spill = ra->stack_usage;

        // split at optimal spot before first use that requires a register
        FOREACH_REVERSE_N(i, 0, dyn_array_length(interval->uses)) {
            if (interval->uses[i].pos >= pos && interval->uses[i].kind == USE_REG) {
                split_intersecting(ra, interval->uses[i].pos - 1, interval, false);
                break;
            }
        }

        return -1;
    } else {
        int split_pos = (interval->start & ~1) - 1;

        if (ra->block_pos[highest] <= interval->end && pos != INT_MAX) {
            split_intersecting(ra, pos, interval, true);
        }

        // split active or inactive interval reg
        LiveInterval* to_split = get_active(ra, rc, highest);
        if (to_split != NULL) {
            split_intersecting(ra, split_pos, to_split, true);
        }

        dyn_array_for(i, ra->inactive) {
            LiveInterval* it = &ra->intervals[ra->inactive[i]];
            if (it->reg_class == rc && it->assigned == highest) {
                split_intersecting(ra, split_pos, it, true);
            }
        }
    }

    return highest;
}

static void move_to_active(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval->reg_class, reg = interval->assigned;
    int ri = interval - ra->intervals;

    // fixed intervals will force things out of their spot if they have to
    if (set_get(&ra->active_set[rc], reg)) {
        tb_assert(interval->reg >= 0, "non-fixed interval attempted to force a register out");
        int split_pos = interval->ranges[dyn_array_length(interval->ranges) - 1].end;

        LiveInterval* old_interval = &ra->intervals[ra->active[rc][reg]];
        split_intersecting(ra, split_pos - 1, old_interval, true);
        interval = &ra->intervals[ri]; // might've resized the intervals
    }

    set_put(&ra->active_set[rc], reg);
    ra->active[rc][reg] = ri;
}

static void cuiksort_defs(LiveInterval* intervals, ptrdiff_t lo, ptrdiff_t hi, RegIndex* arr);
static int linear_scan(Ctx* restrict ctx, TB_Function* f, int stack_usage, int end) {
    LSRA ra = { .abi = f->super.module->target_abi, .first = ctx->first, .intervals = ctx->intervals, .stack_usage = stack_usage };

    FOREACH_N(i, 0, CG_REGISTER_CLASSES) {
        ra.active_set[i] = set_create_in_arena(tmp_arena, 16);
    }

    // build intervals:
    //   we also track when uses happen to aid in splitting
    MachineBBs mbbs = ctx->machine_bbs;
    size_t interval_count = dyn_array_length(ra.intervals);
    FOREACH_N(i, 0, ctx->order.count) {
        TB_Node* bb = ctx->order.traversal[i];
        MachineBB* mbb = &nl_map_get_checked(mbbs, bb);

        int bb_start = mbb->start;
        int bb_end = mbb->end + 2;

        // for anything that's live out, add the entire range
        Set* live_in = &mbb->live_in;
        Set* live_out = &mbb->live_out;
        FOREACH_N(i, 0, (interval_count + 63) / 64) {
            uint64_t bits = live_in->data[i] & live_out->data[i];
            if (bits == 0) continue;

            FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
                size_t k = (i*64) + j;
                add_range(&ra.intervals[k], bb_start, bb_end);
            }
        }

        // for all instruction in BB (in reverse), add ranges
        if (mbb->first) {
            reverse_bb_walk(&ra, mbb, mbb->first);
        }
    }

    post_callee_saved_constraints(ctx, 2, end);

    // generate unhandled interval list (sorted by starting point)
    ra.unhandled = dyn_array_create(LiveInterval*, (interval_count * 4) / 3);
    FOREACH_N(i, 0, interval_count) dyn_array_put(ra.unhandled, i);
    cuiksort_defs(ctx->intervals, 0, interval_count - 1, ra.unhandled);

    // only need enough to store for the biggest register class
    ra.free_pos  = TB_ARENA_ARR_ALLOC(tmp_arena, 16, int);
    ra.use_pos   = TB_ARENA_ARR_ALLOC(tmp_arena, 16, int);
    ra.block_pos = TB_ARENA_ARR_ALLOC(tmp_arena, 16, int);

    // linear scan main loop
    while (dyn_array_length(ra.unhandled)) {
        RegIndex ri = dyn_array_pop(ra.unhandled);
        LiveInterval* interval = &ra.intervals[ri];

        // unused interval, skip
        if (interval->ranges == NULL) continue;

        int time = interval->start;
        if (interval->reg >= 0) {
            REG_ALLOC_LOG printf("  # %-5s t=[%-4d - %4d)\n", reg_name(interval->reg_class, interval->reg), time, interval->end);
        } else if (interval->spill > 0) {
            REG_ALLOC_LOG {
                printf("  # v%-4d t=[%-4d - %4d) SPILLED [RBP - %d]\n", ri, time, interval->end, interval->spill);
            }
            continue;
        } else {
            REG_ALLOC_LOG {
                printf("  # v%-4d t=[%-4d - %4d)   ", ri, time, interval->end);
                if (interval->n != NULL) {
                    print_node_sexpr(f, interval->n, 0);
                }
                printf("\n");
            }
        }

        // expire intervals
        int rc = interval->reg_class;
        FOREACH_SET(reg, ra.active_set[rc]) {
            RegIndex active_i = ra.active[rc][reg];
            LiveInterval* it = &ra.intervals[active_i];

            if (it->end < time) {
                REG_ALLOC_LOG printf("  #   expired %s\n", reg_name(rc, reg));
                set_remove(&ra.active_set[rc], reg);
            } else if (covers(it, time) < 0) {
                // active -> inactive
                REG_ALLOC_LOG printf("  #   active %s is going quiet for now\n", reg_name(rc, reg));

                set_remove(&ra.active_set[rc], reg);
                dyn_array_put(ra.inactive, active_i);
            }
        }

        for (size_t i = 0; i < dyn_array_length(ra.inactive);) {
            LiveInterval* it = &ra.intervals[ra.inactive[i]];

            if (it->end < time) {
                REG_ALLOC_LOG printf("  #   inactive %s has expired\n", reg_name(it->reg_class, it->assigned));
                dyn_array_remove(ra.inactive, i);
                continue;
            } else {
                int end = covers(it, time);
                if (end >= 0) {
                    // inactive -> active
                    REG_ALLOC_LOG printf("  #   inactive %s is active again (until t=%d)\n", reg_name(it->reg_class, it->assigned), end);

                    // set active
                    move_to_active(&ra, it);
                    dyn_array_remove(ra.inactive, i);
                    continue;
                }
            }

            i++;
        }

        ptrdiff_t reg = interval->reg;
        if (reg < 0) {
            // try hint
            if (interval->hint >= 0) {
                LiveInterval* hint = &ra.intervals[interval->hint];
                assert(hint->reg_class == rc);

                if (!set_get(&ra.active_set[rc], hint->assigned)) {
                    REG_ALLOC_LOG printf("  #   assign to %s (HINTED)\n", reg_name(rc, hint->assigned));
                    reg = hint->assigned;
                }
            }

            // find register for virtual interval
            if (reg < 0) {
                reg = allocate_free_reg(&ra, interval);
                interval = &ra.intervals[ri]; // might've resized the intervals
            }

            // alloc failure
            if (reg < 0) {
                reg = allocate_blocked_reg(&ra, interval);
                interval = &ra.intervals[ri]; // might've resized the intervals

                tb_assert(reg >= 0, "regalloc failure");
            }
        }

        // add to active set
        if (reg >= 0) {
            interval->assigned = reg;
            move_to_active(&ra, interval);
        }

        // display active set
        REG_ALLOC_LOG {
            printf("  \x1b[32m{ ");
            FOREACH_N(rc, 0, CG_REGISTER_CLASSES) {
                FOREACH_SET(reg, ra.active_set[rc]) {
                    int id = ra.active[rc][reg];

                    if (ra.intervals[id].reg >= 0) {
                        printf("%s ", reg_name(rc, ra.intervals[id].reg));
                    } else {
                        printf("v%d:%s ", ra.active[rc][reg], reg_name(rc, reg));
                    }
                }
            }
            printf("}\x1b[0m\n");
        }
    }

    // move resolver
    FOREACH_N(i, 0, ctx->order.count) {
        MachineBB* mbb = &nl_map_get_checked(mbbs, ctx->order.traversal[i]);
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(ctx->order.traversal[i]);

        FOREACH_REVERSE_N(i, 0, r->succ_count) {
            TB_Node* bb = r->succ[i];
            MachineBB* target = &nl_map_get_checked(mbbs, bb);

            // for all live-ins, we should check if we need to insert a move
            FOREACH_SET(i, target->live_in) {
                LiveInterval* interval = &ra.intervals[i];

                // if the value changes across the edge, insert move
                LiveInterval* start = split_interval_at(&ra, interval, mbb->end);
                LiveInterval* end = split_interval_at(&ra, interval, mbb->end);

                if (start != end) {
                    __debugbreak();
                }
            }

            // the moves are inserted either at the end of block from or at the beginning of block to,
            // depending on the control flow
            // resolver.find_insert_position(from, to)

            // insert all moves in correct order (without overwriting registers that are used later)
            // resolver.resolve_mappings()
        }
    }

    // resolve all split interval references
    for (Inst* restrict inst = ra.first; inst; inst = inst->next) {
        if (inst->flags & INST_SPILL) {
            continue;
        }

        int pos = inst->time;
        FOREACH_N(i, 0, inst->out_count + inst->in_count + inst->tmp_count) {
            inst->operands[i] = split_interval_at(&ra, &ra.intervals[inst->operands[i]], pos) - ra.intervals;
        }
    }

    ctx->intervals = ra.intervals;
    return ra.stack_usage;
}

////////////////////////////////
// Sorting unhandled list
////////////////////////////////
static size_t partition(LiveInterval* intervals, ptrdiff_t lo, ptrdiff_t hi, RegIndex* arr) {
    int pivot = intervals[arr[(hi - lo) / 2 + lo]].start; // middle

    ptrdiff_t i = lo - 1, j = hi + 1;
    for (;;) {
        // Move the left index to the right at least once and while the element at
        // the left index is less than the pivot
        do { i += 1; } while (intervals[arr[i]].start > pivot);

        // Move the right index to the left at least once and while the element at
        // the right index is greater than the pivot
        do { j -= 1; } while (intervals[arr[j]].start < pivot);

        // If the indices crossed, return
        if (i >= j) return j;

        // Swap the elements at the left and right indices
        SWAP(RegIndex, arr[i], arr[j]);
    }
}

static void cuiksort_defs(LiveInterval* intervals, ptrdiff_t lo, ptrdiff_t hi, RegIndex* arr) {
    if (lo >= 0 && hi >= 0 && lo < hi) {
        // get pivot
        size_t p = partition(intervals, lo, hi, arr);

        // sort both sides
        cuiksort_defs(intervals, lo, p, arr);
        cuiksort_defs(intervals, p + 1, hi, arr);
    }
}
