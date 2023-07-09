// TODO(NeGate): We should switch to Efficient global regsiter allocation, 2011
// https://arxiv.org/pdf/2011.05608.pdf
#define REG_ALLOC_LOG if (1)

// returns true if used in the next n instructions
static bool check_if_used(Ctx* restrict ctx, Inst* inst, int def_i, int n) {
    // check if it's unused for the next instruction
    for (size_t i = 0; inst && i < n; inst = inst->next, i++) {
        FOREACH_N(j, 1, 4) if (inst->regs[j] == USE(def_i)) {
            return true;
        }
    }

    return false;
}

static size_t choose_best_split(Ctx* restrict ctx, Inst* inst, int reg_class, int time) {
    FOREACH_REVERSE_N(i, 0, ctx->active_count) {
        DefIndex di = ctx->active[i];

        if (ctx->defs[di].reg_class == reg_class && ctx->defs[di].reg >= 0 && !check_if_used(ctx, inst, di, 3)) {
            return i;
        }
    }

    return 0;
}

static void insert_sorted_def(Ctx* restrict ctx, DefIndex* sorted, size_t count, int start, DefIndex di) {
    size_t i = count;
    while (i--) {
        if (ctx->defs[sorted[i]].start >= start) break;
    }

    // we know where to insert
    memmove(&sorted[i + 1], &sorted[i], (count - i) * sizeof(DefIndex));
    sorted[i + 1] = di;
}

static Inst* find_inst_at_time(Ctx* restrict ctx, int time) {
    Inst *prev = ctx->first, *inst = prev->next;

    while (inst != NULL) {
        if (inst->time >= time) {
            return prev;
        }

        prev = inst, inst = inst->next;
    }

    return NULL;
}

static int spill_register(Ctx* restrict ctx, RegAllocWorklist* worklist, Inst* spill_inst, DefIndex split_def, size_t split_active_i, int reg_class, int reg_num) {
    // insert spill
    Reload r = { TB_TYPE_I64, split_def };
    if (ctx->defs[split_def].node != NULL) {
        r.dt = ctx->defs[split_def].node->dt;
    }

    spill(ctx, spill_inst, &r);
    spill_inst = spill_inst->next;

    size_t min = SIZE_MAX;

    // keep spilled and generate reloads on the first use in any BB
    DefIndex reload_def = -1;
    int endpoint = ctx->defs[split_def].end;

    Inst *inst = spill_inst->next, *prev_inst = spill_inst;
    for (; inst; prev_inst = inst, inst = inst->next) {
        if (inst->type == INST_LABEL) reload_def = -1;
        if (inst->time > endpoint) break;

        // if it's used, refer to reload
        bool skip_next = false;
        FOREACH_REVERSE_N(j, 1, 4) if (inst->regs[j] == USE(split_def)) {
            if (inst->type == INST_MOVE && j == 1) {
                skip_next = true;
                r.old = split_def;
                spill(ctx, inst, &r);
                continue;
            } else if (reload_def < 0) {
                // spin up new def
                int t = prev_inst->time + 1;
                dyn_array_put(ctx->defs, (Def){ .start = t, .end = t, .reg = -1, .hint = reg_num });
                reload_def = dyn_array_length(ctx->defs) - 1;

                // place new definition into worklist sorted
                dyn_array_put_uninit(*worklist, 1);
                insert_sorted_def(ctx, *worklist, dyn_array_length(*worklist) - 1, t, reload_def);

                // generate reload before this instruction
                r.old = reload_def;
                reload(ctx, prev_inst, &r, j);
            }

            inst->regs[j] = USE(reload_def);
            ctx->defs[reload_def].end = inst->time + (j == 1 ? 0 : 1);
        }

        if (inst->regs[0] == split_def && reload_def != split_def) {
            // spill and discard our reload spot (if applies)
            r.old = inst->regs[0];
            spill(ctx, inst, &r);
            reload_def = -1;
            skip_next = true;
        }

        // if we're in the clobber list, invalidate the reload_def
        if (inst->regs[0] >= 0 && ctx->defs[inst->regs[0]].clobbers) {
            Clobbers* clobbers = ctx->defs[inst->regs[0]].clobbers;

            FOREACH_N(i, 0, clobbers->count) {
                if (clobbers->_[i].class == reg_class && clobbers->_[i].num == reg_num) {
                    reload_def = -1;
                    break;
                }
            }
        }

        if (skip_next) {
            // skip this instruction to avoid infinite spills
            prev_inst = inst, inst = inst->next;
        }
    }

    set_remove(&ctx->used_regs[reg_class], reg_num);
    remove_active(ctx, split_active_i);
    return reg_num;
}

static const char* reg_name(int rg, int num) {
    return (rg == REG_CLASS_XMM ? XMM_NAMES : GPR_NAMES)[num];
}

static ptrdiff_t find_active_from_reg(Ctx* restrict ctx, int reg_class, int reg) {
    FOREACH_N(i, 0, ctx->active_count) {
        Def* d = &ctx->defs[ctx->active[i]];
        if (d->reg_class == reg_class && d->reg == reg) {
            return i;
        }
    }

    return -1;
}

static size_t evict(Ctx* restrict ctx, RegAllocWorklist* worklist, DefIndex di, int reg_class, int reg_num, int time) {
    if (!set_get(&ctx->used_regs[reg_class], reg_num)) {
        return false;
    }

    REG_ALLOC_LOG printf("  \x1b[32m#   evict %s\x1b[0m\n", reg_name(reg_class, reg_num));

    ptrdiff_t spill_active_i = find_active_from_reg(ctx, reg_class, reg_num);
    assert(spill_active_i >= 0 && "We both know a register in use and don't know who's doing it...");

    DefIndex spill_def = ctx->active[spill_active_i];
    Inst* inst = find_inst_at_time(ctx, ctx->defs[spill_def].start);

    // we need to spill this sometime between 'time' and the initial
    // definition, we're trying to push it as far as possible but we
    // don't cross the definition BB to do so.
    while (inst->next != NULL) {
        Inst* next = inst->next;

        if (next->type == INST_LABEL || wont_spill_around(next->type)) {
            // stop before a BB or terminator
            break;
        } else if (next->time >= time) {
            // it's now or never
            break;
        }

        inst = next;
    }

    spill_register(ctx, worklist, inst, spill_def, spill_active_i, reg_class, reg_num);
    return true;
}

static void reg_alloc(Ctx* restrict ctx, TB_Function* f, RegAllocWorklist worklist) {
    Set reg_masks[CG_REGISTER_CLASSES];
    FOREACH_N(i, 0, CG_REGISTER_CLASSES){
        reg_masks[i] = set_create_in_arena(&tb__arena, ctx->used_regs[i].capacity);
    }

    // linear scan main loop
    //
    // the worklist is sorted such that we pop the lowest start time.
    // splits will introduce new definitions to the top.
    while (dyn_array_length(worklist) > 0) {
        DefIndex di = dyn_array_pop(worklist);
        Def* d = &ctx->defs[di];

        Def* next_clobber = NULL;
        if (dyn_array_length(ctx->clobbers) > 0) {
            next_clobber = &ctx->defs[ctx->clobbers[dyn_array_length(ctx->clobbers) - 1]];

            // if they don't intersect, stop caring about it
            if (!(d->start <= next_clobber->end && next_clobber->start <= d->end)) {
                next_clobber = NULL;
            }
        }

        int time = d->start;
        REG_ALLOC_LOG {
            printf("  \x1b[32m# D%zu t=[%d,%d) ", di, time, d->end);
            if (d->node) printf("%p %s", d->node, tb_node_get_name(d->node));
            printf("\x1b[0m\n");
        }

        // expire old intervals
        for (size_t j = 0; j < ctx->active_count;) {
            Def* k = &ctx->defs[ctx->active[j]];
            if (k->end > time) break;

            if (k->reg >= 0) {
                // move from active to handled
                REG_ALLOC_LOG printf("  \x1b[32m#   kill %s\x1b[0m\n", reg_name(k->reg_class, k->reg));

                set_remove(&ctx->used_regs[k->reg_class], k->reg);
                remove_active(ctx, j);
            } else {
                j++;
            }
        }

        // clobbering will evict registers it needs
        if (d->clobbers) {
            FOREACH_N(j, 0, d->clobbers->count) {
                evict(ctx, &worklist, di, d->clobbers->_[j].class, d->clobbers->_[j].num, time);
                d = &ctx->defs[di];
            }

            d = &ctx->defs[di];

            next_clobber = NULL;

            // find the clobber, delete it
            size_t top = dyn_array_length(ctx->clobbers) - 1;
            if (d == &ctx->defs[ctx->clobbers[top]]) {
                dyn_array_pop(ctx->clobbers);
            }
        }

        // find register for current
        int rc = d->reg_class;
        ptrdiff_t reg_num = -1;
        if (d->reg >= 0) {
            // pre-colored, we make room for it
            if (evict(ctx, &worklist, di, rc, d->reg, time)) {
                d = &ctx->defs[di];
            }

            reg_num = d->reg;
            REG_ALLOC_LOG printf("  \x1b[32m#   forced assign %s\x1b[0m\n", reg_name(rc, reg_num));
        } else if (d->hint >= 0 && !set_get(&ctx->used_regs[rc], d->hint)) {
            reg_num = d->hint;
            REG_ALLOC_LOG printf("  \x1b[32m#   hinted assign %s\x1b[0m\n", reg_name(rc, reg_num));
        } else {
            if (next_clobber != NULL) {
                set_copy(&reg_masks[rc], &ctx->used_regs[rc]);

                // avoid the registers which finna get clobbered
                Clobbers* c = next_clobber->clobbers;
                FOREACH_N(j, 0, c->count) {
                    if (c->_[j].class == rc) {
                        set_put(&reg_masks[rc], c->_[j].num);
                    }
                }

                // try clobberless path
                reg_num = set_pop_any(&reg_masks[rc]);
            }

            // try normal path
            if (reg_num < 0) {
                reg_num = set_pop_any(&ctx->used_regs[rc]);
            }

            if (reg_num < 0) {
                // choose who to spill
                Inst* spill_inst = find_inst_at_time(ctx, time);
                size_t split_i = choose_best_split(ctx, spill_inst, rc, time);

                DefIndex spill_def = ctx->active[split_i];

                reg_num = spill_register(ctx, &worklist, spill_inst, spill_def, split_i, rc, ctx->defs[spill_def].reg);
                d = &ctx->defs[di];
            }

            REG_ALLOC_LOG printf("  \x1b[32m#   assign %s\x1b[0m\n", reg_name(rc, reg_num));
        }

        finna_use_reg(ctx, rc, reg_num);

        set_put(&ctx->used_regs[rc], reg_num);
        add_active(ctx, di);
        d->reg = reg_num;
    }
}
