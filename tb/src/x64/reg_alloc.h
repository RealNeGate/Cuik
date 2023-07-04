// TODO(NeGate): We should switch to Efficient global regsiter allocation, 2011
// https://arxiv.org/pdf/2011.05608.pdf

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

static size_t choose_best_split(Ctx* restrict ctx, TB_Function* f, Inst* inst, int time) {
    FOREACH_REVERSE_N(i, 0, ctx->active_count) {
        DefIndex di = ctx->active[i];

        if (ctx->defs[di].reg >= 0 && !check_if_used(ctx, inst, di, 3)) {
            return i;
        }
    }

    return 0;
}

static Inst* insert_reload_site(Ctx* restrict ctx, TB_Function* f, Inst* inst, size_t def_i) {
    // skip first sequence
    inst = inst->next;

    for (; inst; inst = inst->next) {
        if (inst->type == INST_LABEL) {
            return NULL;
        }

        FOREACH_N(j, 1, 4) if (inst->regs[j] == USE(def_i)) {
            return inst;
        }
    }

    return NULL;
}

static Inst* find_inst_at_time(Ctx* restrict ctx, TB_Function* f, int time) {
    Inst *prev = ctx->first, *inst = prev->next;

    while (inst != NULL) {
        if (inst->time >= time) {
            return prev;
        }

        prev = inst, inst = inst->next;
    }

    return NULL;
}

static ptrdiff_t spill_register(Ctx* restrict ctx, DefIndex* sorted, Inst* spill_inst, DefIndex split_def, size_t split_active_i, int reg_class, int reg_num) {
    // insert spill
    Reload r = { TB_TYPE_I64, split_def };
    if (ctx->defs[split_def].node != NULL) {
        r.dt = ctx->defs[split_def].node->dt;
    }

    spill(ctx, spill_inst, &r);
    spill_inst = spill_inst->next;

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
            if (reload_def < 0) {
                if (inst->type == X86_INST_MOVE && j == 1) {
                    skip_next = true;
                    r.old = split_def;
                    spill(ctx, inst, &r);
                    continue;
                } else {
                    // spin up new def
                    int t = prev_inst->time + 1;
                    dyn_array_put(ctx->defs, (Def){ .start = t, .end = t, .reg = -1, .hint = reg_num });
                    reload_def = dyn_array_length(ctx->defs) - 1;

                    // insert into sort
                    insert_sorted_def(ctx, sorted, reload_def, t, reload_def);

                    // generate reload before this instruction
                    assert(prev_inst);
                    r.old = reload_def;
                    reload(ctx, prev_inst, &r);
                }
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

static bool evict(Ctx* restrict ctx, TB_Function* f, DefIndex di, int reg_class, int reg_num, DefIndex* sorted, int time) {
    if (!set_get(&ctx->used_regs[reg_class], reg_num)) {
        return false;
    }

    ASM printf("  \x1b[32m#   evict %s\x1b[0m\n", reg_name(reg_class, reg_num));

    ptrdiff_t spill_active_i = find_active_from_reg(ctx, reg_class, reg_num);
    assert(spill_active_i >= 0 && "We both know a register in use and don't know who's doing it...");

    DefIndex spill_def = ctx->active[spill_active_i];
    Inst* inst = find_inst_at_time(ctx, f, ctx->defs[spill_def].start);

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

    spill_register(ctx, sorted, inst, spill_def, spill_active_i, reg_class, reg_num);
    return true;
}

static void expire_intervals(Ctx* restrict ctx, DefIndex* sorted, int time) {
    for (size_t j = 0; j < ctx->active_count;) {
        Def* k = &ctx->defs[ctx->active[j]];
        if (k->end > time) break;

        if (k->reg >= 0) {
            // move from active to handled
            ASM printf("  \x1b[32m#   kill %s\x1b[0m\n", reg_name(k->reg_class, k->reg));

            set_remove(&ctx->used_regs[k->reg_class], k->reg);
            remove_active(ctx, j);
        } else {
            j++;
        }
    }
}

static void reg_alloc(Ctx* restrict ctx, TB_Function* f, DefIndex* sorted) {
    #if 0
    size_t def_count = dyn_array_length(ctx->defs);
    Arena* arena = &tb__arena;

    Set active        = set_create_in_arena(arena, def_count);
    Set visited       = set_create_in_arena(arena, def_count);
    Set live_outs     = set_create_in_arena(arena, def_count);
    Set future_active = set_create_in_arena(arena, def_count);

    FOREACH_N(i, 0, ctx->order.count) {
        MachineBB* mbb = &nl_map_get_checked(seq_bb, ctx->order.traversal[i]);
        Set* live_in = mbb->live_in;

    }

    #else
    // linear scan main loop
    for (size_t i = 0; i < dyn_array_length(ctx->defs); i++) {
        DefIndex di = sorted[i];
        Def* d = &ctx->defs[di];

        // pre-colored, we must work around these
        if (d->complete) continue;

        int time = d->start;
        ASM {
            printf("  \x1b[32m# D%zu t=[%d,%d) ", di, time, d->end);
            if (d->node) printf("%p", d->node);
            printf("\x1b[0m\n");
        }

        expire_intervals(ctx, sorted, time);

        // clobbering will evict registers it needs
        if (d->clobbers) {
            FOREACH_N(i, 0, d->clobbers->count) {
                evict(ctx, f, di, d->clobbers->_[i].class, d->clobbers->_[i].num, sorted, time);
            }

            // we need to re-eval this element because something got inserted into this slot
            if (di != sorted[i]) {
                i -= 1;
            }
            d = &ctx->defs[di];
        }

        // find register for current
        ptrdiff_t reg_num = -1;
        if (d->reg >= 0) {
            if (evict(ctx, f, di, d->reg_class, d->reg, sorted, time)) {
                // we need to re-eval this element because something got inserted into this slot
                if (di != sorted[i]) {
                    i -= 1;
                }
                d = &ctx->defs[di];
            }

            reg_num = d->reg;
            ASM printf("  \x1b[32m#   forced assign %s\x1b[0m\n", reg_name(d->reg_class, reg_num));
        } else if (d->hint >= 0 && !set_get(&ctx->used_regs[d->reg_class], d->hint)) {
            reg_num = d->hint;
            ASM printf("  \x1b[32m#   hinted assign %s\x1b[0m\n", reg_name(d->reg_class, reg_num));
        } else {
            reg_num = set_pop_any(&ctx->used_regs[d->reg_class]);

            if (reg_num < 0) {
                tb_todo();
                // choose who to spill
                /*Inst* spill_inst = find_inst_at_time(ctx, f, time);
                size_t split_i = choose_best_split(ctx, f, spill_inst, time);

                reg_num = spill_register(ctx, f, sorted, di, time, spill_inst, split_i);

                // we need to re-eval this element because something got inserted into this slot
                if (di != sorted[i]) {
                    i -= 1;
                }
                d = &ctx->defs[di];*/
            }

            ASM printf("  \x1b[32m#   assign %s\x1b[0m\n", reg_name(d->reg_class, reg_num));
        }

        gonna_use_reg(ctx, d->reg_class, reg_num);

        set_put(&ctx->used_regs[d->reg_class], reg_num);
        add_active(ctx, di);
        d->complete = true;
        d->reg = reg_num;
    }
    #endif
}

