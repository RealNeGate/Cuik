// https://ssw.jku.at/Research/Papers/Wimmer04Master/Wimmer04Master.pdf
#define REG_ALLOC_LOG if (1)

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
    bool needs_reg;
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

    // we're gonna have so much memory to clean up...
    DynArray(LiveRange) ranges;
    DynArray(UsePos) uses;
};

typedef struct {
    TB_ABI abi;

    DynArray(LiveInterval) intervals;
    DynArray(RegIndex) inactive;

    // time when the physical registers will be free again
    int* free_pos;
    int* use_pos;

    Set active_set[CG_REGISTER_CLASSES];
    RegIndex active[CG_REGISTER_CLASSES][16];
} LSRA;

typedef DynArray(RegIndex) IntervalList;

static LiveRange* last_range(LiveInterval* i) {
    return &i->ranges[dyn_array_length(i->ranges) - 1];
}

////////////////////////////////
// Generate intervals
////////////////////////////////
static void add_use_pos(LiveInterval* interval, int t, bool needs_reg) {
    UsePos u = { t, needs_reg };
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

    // each caller saved register introduces a range with no usage to force a spill
    if (inst->type == CALL || inst->type == SYSCALL) {
        // TODO(NeGate): move the x64 specific code out of here
        bool is_sysv = (ra->abi == TB_ABI_SYSTEMV);
        const struct ParamDescriptor* restrict desc = &param_descs[is_sysv ? 1 : 0];
        if (inst->type == SYSCALL) {
            desc = &param_descs[2];
        }

        FOREACH_N(i, 0, 16) if (desc->caller_saved_gprs & (1u << i)) {
            add_range(&ra->intervals[FIRST_GPR + i], inst->time, inst->time + 1);
        }

        FOREACH_N(i, 0, desc->caller_saved_xmms) {
            add_range(&ra->intervals[FIRST_XMM + i], inst->time, inst->time + 1);
        }
    }

    // mark outputs, inputs and temps
    //
    // TODO(NeGate): on x86 we can have one memory operand per instruction.
    // we shouldn't force only register uses or else we'll make spilling more
    // prominent.
    RegIndex* ops = inst->operands;
    FOREACH_N(i, 0, inst->out_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        interval->start = inst->time;
        if (interval->ranges == NULL) {
            dyn_array_put(interval->ranges, (LiveRange){ inst->time, inst->time });
        } else {
            interval->ranges[0].start = inst->time;
        }

        add_use_pos(interval, inst->time, true);
    }

    FOREACH_N(i, 0, inst->in_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        add_range(interval, bb->start, inst->time);
        add_use_pos(interval, inst->time, true);
    }

    FOREACH_N(i, 0, inst->tmp_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        add_range(interval, inst->time, inst->time + 1);
        add_use_pos(interval, inst->time, true);
    }
}

static bool range_intersect(LiveInterval* a, LiveInterval* b) {
    return b->start <= a->end && a->start <= b->end;
}

// returns -1 if no registers are available
static ptrdiff_t allocate_free_reg(LSRA* restrict ra, LiveInterval* interval) {
    int rc = interval->reg_class;
    if (interval->hint >= 0 && !set_get(&ra->active_set[rc], interval->hint)) {
        REG_ALLOC_LOG printf("  #   assign to %s (HINTED)\n", reg_name(rc, interval->hint));
        return interval->hint;
    }

    FOREACH_N(i, 0, 16) ra->free_pos[i] = INT_MAX;

    if (rc == REG_CLASS_GPR) {
        // reserved regs
        ra->free_pos[RBP] = 0;
        ra->free_pos[RSP] = 0;
    }

    // for each active reg, set the free pos to 0
    FOREACH_N(i, 0, (ra->active_set[rc].capacity + 63) / 64) {
        uint64_t bits = ra->active_set[rc].data[i];
        if (bits == 0) continue;

        FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
            ra->free_pos[i*64 + j] = 0;
        }
    }

    // for each inactive which intersects current
    dyn_array_for(i, ra->inactive) {
        if (range_intersect(interval, &ra->intervals[ra->inactive[i]])) {
            __debugbreak();
            ra->free_pos[i] = 0;
        }
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
    } else if (pos > interval->end) {
        // we can steal it completely
        REG_ALLOC_LOG printf("  #   assign to %s\n", reg_name(rc, highest));
        return highest;
    } else {
        // TODO(NeGate): split current at optimal position before current
        __debugbreak();
        return highest;
    }
}

static ptrdiff_t allocate_blocked_reg(LSRA* restrict ra, LiveInterval* interval) {
    FOREACH_N(i, 0, 16) ra->free_pos[i] = INT_MAX;
    FOREACH_N(i, 0, 16) ra->use_pos[i] = INT_MAX;

    return -1;
}

static bool covers(LiveInterval* it, int time) {
    FOREACH_N(i, 0, dyn_array_length(it->ranges)) {
        if (time >= it->ranges[i].start && time <= it->ranges[i].end) {
            return true;
        }
    }

    return false;
}

static void cuiksort_defs(LiveInterval* intervals, ptrdiff_t lo, ptrdiff_t hi, RegIndex* arr);
static void linear_scan(Ctx* restrict ctx, TB_Function* f) {
    LSRA ra = { .abi = f->super.module->target_abi, .intervals = ctx->intervals };

    FOREACH_N(i, 0, CG_REGISTER_CLASSES) {
        ra.active_set[i] = set_create_in_arena(&tb__arena, 16);
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

    // generate unhandled interval list (sorted by starting point)
    IntervalList unhandled = dyn_array_create(LiveInterval*, (interval_count * 4) / 3);
    FOREACH_N(i, 0, interval_count) dyn_array_put(unhandled, i);
    cuiksort_defs(ctx->intervals, 0, interval_count - 1, unhandled);

    // only need enough to store for the biggest register class
    ra.free_pos = TB_ARENA_ARR_ALLOC(&tb__arena, 16, int);
    ra.use_pos  = TB_ARENA_ARR_ALLOC(&tb__arena, 16, int);

    // linear scan main loop
    while (dyn_array_length(unhandled)) {
        RegIndex ri = dyn_array_pop(unhandled);
        LiveInterval* interval = &ra.intervals[ri];

        // unused interval, skip
        if (interval->ranges == NULL) continue;

        int time = interval->ranges[0].start;
        if (interval->reg >= 0) {
            continue;
        } else {
            REG_ALLOC_LOG {
                printf("  # v%-4d t=[%-4d - %4d)   ", ri, time, interval->end);
                print_node_sexpr(f, interval->n, 0);
                printf("\n");
            }
        }

        // expire intervals
        int rc = interval->reg_class;
        FOREACH_N(i, 0, (ra.active_set[rc].capacity + 63) / 64) {
            uint64_t bits = ra.active_set[rc].data[i];
            if (bits == 0) continue;

            FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
                int reg = i*64 + j;
                RegIndex active_i = ra.active[rc][reg];
                LiveInterval* it = &ra.intervals[active_i];

                if (it->end < time) {
                    REG_ALLOC_LOG printf("  #   expired %s\n", reg_name(rc, reg));
                    set_remove(&ra.active_set[rc], reg);
                } else if (!covers(it, time)) {
                    // active -> inactive
                    REG_ALLOC_LOG printf("  #   active %s is going quiet for now\n", reg_name(rc, reg));

                    set_remove(&ra.active_set[rc], reg);
                    dyn_array_put(ra.inactive, active_i);
                    __debugbreak();
                }
            }
        }

        for (size_t i = 0; i < dyn_array_length(ra.inactive); i++) retry: {
            LiveInterval* it = &ra.intervals[ra.inactive[i]];

            if (it->end < time) {
                REG_ALLOC_LOG printf("  #   inactive %s has expired\n", reg_name(rc, it->assigned));
                dyn_array_remove(ra.inactive, i);
                goto retry;
            } else if (covers(it, time)) {
                // inactive -> active
                REG_ALLOC_LOG printf("  #   inactive %s is active again\n", reg_name(rc, it->assigned));

                // set active
                set_put(&ra.active_set[interval->reg_class], it->assigned);
                ra.active[interval->reg_class][it->assigned] = ri;

                dyn_array_remove(ra.inactive, i);
                goto retry;
            }
        }

        // find register for interval
        ptrdiff_t reg = allocate_free_reg(&ra, interval);
        if (reg < 0) {
            // alloc failure
            __debugbreak();
        } else {
            set_put(&ra.active_set[interval->reg_class], reg);
            ra.active[interval->reg_class][reg] = ri;

            interval->assigned = reg;
        }

        // display active set
        REG_ALLOC_LOG {
            printf("  \x1b[32m{ ");
            FOREACH_N(rc, 0, CG_REGISTER_CLASSES) {
                FOREACH_N(i, 0, (ra.active_set[rc].capacity + 63) / 64) {
                    uint64_t bits = ra.active_set[rc].data[i];
                    if (bits == 0) continue;

                    FOREACH_N(j, 0, 64) if (bits & (1ull << j)) {
                        int reg = i*64 + j;
                        printf("v%d ", ra.active[rc][reg]);
                    }
                }
            }
            printf("}\x1b[0m\n");
        }
    }

    ctx->intervals = ra.intervals;
    __debugbreak();
}

////////////////////////////////
// Sorting unhandled list
////////////////////////////////
static int get_interval_start(LiveInterval* i) {
    return i->ranges ? i->ranges[0].start : 0;
}

static size_t partition(LiveInterval* intervals, ptrdiff_t lo, ptrdiff_t hi, RegIndex* arr) {
    int pivot = get_interval_start(&intervals[arr[(hi - lo) / 2 + lo]]); // middle

    ptrdiff_t i = lo - 1, j = hi + 1;
    for (;;) {
        // Move the left index to the right at least once and while the element at
        // the left index is less than the pivot
        do { i += 1; } while (get_interval_start(&intervals[arr[i]]) > pivot);

        // Move the right index to the left at least once and while the element at
        // the right index is greater than the pivot
        do { j -= 1; } while (get_interval_start(&intervals[arr[j]]) < pivot);

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
