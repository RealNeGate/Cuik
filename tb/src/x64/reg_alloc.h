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

    // spill point, -1 if there's none
    int spill;

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

    // calls use the temporaries for clobbers
    bool is_call = (inst->type == CALL || inst->type == SYSCALL);
    FOREACH_N(i, 0, inst->tmp_count) {
        LiveInterval* interval = &ra->intervals[*ops++];

        add_range(interval, inst->time, inst->time + 1);
        if (!is_call) {
            add_use_pos(interval, inst->time, true);
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

static bool covers(LiveInterval* it, int time) {
    FOREACH_N(i, 0, dyn_array_length(it->ranges)) {
        if (time >= it->ranges[i].start && time <= it->ranges[i].end) {
            return true;
        }
    }

    return false;
}

static LiveInterval* get_active(LSRA* restrict ra, int rc, int reg) {
    if (!set_get(&ra->active_set[rc], reg)) {
        return NULL;
    }

    return &ra->intervals[ra->active[rc][reg]];
}

// insert reload slot lazily to avoid register pressure
static void replace_use_in_insts(LSRA* restrict ra, int start, int end, int old_reg, int new_reg) {
    Inst *prev = ra->first, *inst = prev->next;

    int rc = ra->intervals[old_reg].reg_class;
    TB_X86_DataType dt = ra->intervals[old_reg].dt;

    int reload = -1;
    bool spill = false, writeback = false;
    while (inst != NULL) {
        if (inst->time >= start) {
            // spill value now
            if (!spill) {
                spill = true;

                Inst* new_inst = tb_arena_alloc(tmp_arena, sizeof(Inst) + (2 * sizeof(RegIndex)));
                *new_inst = (Inst){ .type = MOV, .dt = dt, .out_count = 1, 1 };
                new_inst->operands[0] = new_reg;
                new_inst->operands[1] = old_reg;
                new_inst->time = prev->time + 1;
                new_inst->next = prev->next;
                prev->next = new_inst;
            }

            // insert spill because we wrote to old_reg
            FOREACH_N(i, 0, inst->out_count) {
                if (inst->operands[i] == old_reg) {
                    inst->operands[i] = new_reg;
                    writeback = true;
                }
            }

            // insert reload slot if we need a register
            int t = prev->time + 1;
            int total = inst->out_count + inst->in_count;
            FOREACH_N(i, inst->out_count, total) {
                if (inst->operands[i] == old_reg) {
                    if (reload < 0) {
                        reload = dyn_array_length(ra->intervals);

                        // if it was forced, then we need to force the reload into the same slot
                        int assigned = ra->intervals[old_reg].reg;
                        if (assigned < 0) {
                            dyn_array_put(ra->unhandled, reload);
                        }

                        dyn_array_put_uninit(ra->intervals, 1);
                        ra->intervals[reload] = (LiveInterval){
                            .assigned = assigned, .reg = -1, .dt = dt, .reg_class = rc, .start = t, .end = t
                        };
                        add_range(&ra->intervals[reload], inst->time, inst->time);

                        // insert reload instruction
                        Inst* new_inst = tb_arena_alloc(tmp_arena, sizeof(Inst) + (2 * sizeof(RegIndex)));
                        *new_inst = (Inst){ .type = MOV, .dt = dt, .out_count = 1, 1 };
                        new_inst->operands[0] = reload;
                        new_inst->operands[1] = new_reg;
                        new_inst->time = t;
                        new_inst->next = prev->next;
                        prev->next = new_inst;

                        __debugbreak();
                    }

                    inst->operands[i] = reload;

                    // extend range
                    add_use_pos(&ra->intervals[reload], inst->time, true);
                    ra->intervals[reload].end = inst->time;
                }
            }
        } else if (inst->time > end) {
            break;
        }

        prev = inst, inst = inst->next;
    }

    // spill back our new version of the value
    if (writeback) {
        Inst* new_inst = tb_arena_alloc(tmp_arena, sizeof(Inst) + (2 * sizeof(RegIndex)));
        *new_inst = (Inst){ .type = MOV, .dt = dt, .out_count = 1, 1 };
        new_inst->operands[0] = new_reg;
        new_inst->operands[1] = old_reg;
        new_inst->time = prev->time + 1;
        new_inst->next = prev->next;
        prev->next = new_inst;
    }
}

// any uses after `pos` after put into the new interval
static int split_intersecting(LSRA* restrict ra, int pos, LiveInterval* interval) {
    // allocate stack slot
    int size = 8;
    ra->stack_usage = align_up(ra->stack_usage + size, size);

    // split lifetime
    LiveInterval it = *interval;
    it.spill = ra->stack_usage;
    it.assigned = it.reg = -1;
    it.start = pos;
    it.end = interval->end;
    it.uses = NULL;
    it.ranges = NULL;
    it.n = NULL;
    interval->end = pos;

    int old_reg = interval - ra->intervals;
    int new_reg = dyn_array_length(ra->intervals);
    dyn_array_put(ra->intervals, it);

    // since the split is starting at pos and pos is at the top of the
    // unhandled list... we can push this to the top wit no problem
    dyn_array_put(ra->unhandled, new_reg);

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
            replace_use_in_insts(ra, range->start, range->end, old_reg, new_reg);
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

            replace_use_in_insts(ra, r.start, r.end, old_reg, new_reg);
            interval = &ra->intervals[old_reg];
            continue;
        }

        i++;
    }

    ra->intervals[new_reg] = it;
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
            // max(interval.start, inactive_it.start)
            ra->free_pos[i] = interval->start > inactive_it->start ? interval->start : inactive_it->start;
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
        REG_ALLOC_LOG printf("  \x1b[33m#   split %s before t=%d\x1b[0m\n", reg_name(rc, highest), pos);
        __debugbreak();
        split_intersecting(ra, pos - 1, interval);
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
    if (pos < interval->uses[dyn_array_length(interval->uses) - 1].pos) {
        int size = 8;

        // spill interval
        ra->stack_usage = align_up(ra->stack_usage + size, size);
        interval->spill = ra->stack_usage;

        // split at optimal spot before first use that requires a register
        __debugbreak();
    } else {
        int split_pos = interval->start - 1;

        if (ra->block_pos[highest] <= interval->end && pos != INT_MAX) {
            REG_ALLOC_LOG printf("  \x1b[33m#   split %s before t=%d\x1b[0m\n", reg_name(rc, highest), pos);
            split_intersecting(ra, pos, interval);
        }

        // split active or inactive interval reg
        REG_ALLOC_LOG printf("  \x1b[33m#   split %s after t=%d\x1b[0m\n", reg_name(rc, highest), split_pos);
        LiveInterval* to_split = get_active(ra, rc, highest);
        if (to_split != NULL) {
            split_intersecting(ra, split_pos, to_split);
        }

        dyn_array_for(i, ra->inactive) {
            LiveInterval* it = &ra->intervals[ra->inactive[i]];
            if (it->reg_class == rc && it->assigned == highest) {
                split_intersecting(ra, split_pos, it);
            }
        }
    }

    return highest;
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
            REG_ALLOC_LOG {
                printf("  # %-5s t=[%-4d - %4d)\n", reg_name(interval->reg_class, interval->reg), time, interval->end);
            }
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
            } else if (!covers(it, time)) {
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
            } else if (covers(it, time)) {
                // inactive -> active
                REG_ALLOC_LOG printf("  #   inactive %s is active again\n", reg_name(it->reg_class, it->assigned));

                // set active
                set_put(&ra.active_set[it->reg_class], it->assigned);
                ra.active[it->reg_class][it->assigned] = ra.inactive[i];

                dyn_array_remove(ra.inactive, i);
                continue;
            }
            i++;
        }

        ptrdiff_t reg = interval->reg;
        if (reg < 0) {
            // try hint register if available
            if (interval->hint >= 0) {
                if (!set_get(&ra.active_set[rc], interval->hint)) {
                    REG_ALLOC_LOG printf("  #   assign to %s (HINTED)\n", reg_name(rc, interval->hint));
                    reg = interval->hint;
                } else {
                    REG_ALLOC_LOG printf("  #   HINT to %s failed (in use until t=%d)\n", reg_name(rc, interval->hint), ra.intervals[ra.active[rc][interval->hint]].end);
                }
            }

            // find register for virtual interval
            if (reg < 0) {
                reg = allocate_free_reg(&ra, interval);
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
                        int id = ra.active[rc][reg];

                        if (ra.intervals[id].reg >= 0) {
                            printf("%s ", reg_name(rc, ra.intervals[id].reg));
                        } else {
                            printf("v%d:%s ", ra.active[rc][reg], reg_name(rc, reg));
                        }
                    }
                }
            }
            printf("}\x1b[0m\n");
        }
    }

    // __debugbreak();

    ctx->intervals = ra.intervals;
    return ra.stack_usage;
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
