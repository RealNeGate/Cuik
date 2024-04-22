// Linear scan register allocator:
//   https://ssw.jku.at/Research/Papers/Wimmer04Master/Wimmer04Master.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>

#define FOREACH_SET(it, set) \
FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    int id;
    RegMask* mask;
} AllocFailure;

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;
    int* fixed;

    NL_Table fwd_table;
    DynArray(int) spills;

    int* order;
    Set active;
    Set future_active;
    Set live_out;
} LSRA;

#define BND(arr, i, limit) ((i) >= (limit) ? abort() : 0, arr)[i]

static bool allocate_loop(Ctx* restrict ctx, LSRA* restrict ra, TB_Arena* arena);

// Helpers
static RegMask* constraint_in(Ctx* ctx, TB_Node* n, int i) {
    ctx->constraint(ctx, n, ctx->ins);
    return ctx->ins[i];
}

static void spill_entire_lifetime(Ctx* ctx, VReg* to_spill, RegMask* spill_mask, bool conflict) {
    cuikperf_region_start("spill vreg", NULL);

    TB_Function* f = ctx->f;
    TB_Node* n = to_spill->n;
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m# v%zu: spill    (%%%u)\x1b[0m\n", to_spill - ctx->vregs, n->gvn));

    to_spill->mask = spill_mask;

    TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);

    // don't want weird pointer invalidation crap
    size_t user_count = n->user_count;
    TB_User* users = tb_arena_alloc(f->tmp_arena, n->user_count * sizeof(TB_User));
    memcpy(users, n->users, n->user_count * sizeof(TB_User));

    // aggressive reload
    for (size_t i = 0; i < user_count; i++) {
        TB_Node* use_n = USERN(&users[i]);
        int use_i      = USERI(&users[i]);
        // it's never in[0] lmao
        if (use_i == 0) { continue; }

        RegMask* in_mask = constraint_in(ctx, use_n, use_i);
        if (conflict) {
            RegMask* intersect = tb__reg_mask_meet(ctx, in_mask, spill_mask);
            if (intersect != &TB_REG_EMPTY) { continue; }
        }

        assert(in_mask != NULL);

        // reload per use site
        TB_Node* reload_n = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
        set_input(f, use_n, reload_n, use_i);
        set_input(f, reload_n, n, 1);
        TB_NODE_SET_EXTRA(reload_n, TB_NodeMachCopy, .def = in_mask, .use = spill_mask);

        // schedule the split right before use
        tb__insert_before(ctx, ctx->f, reload_n, use_n);
        VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n);
        reload_vreg->mask = in_mask;

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   v%zu: reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n->gvn));
    }
    tb_arena_restore(f->tmp_arena, sp);
    cuikperf_region_end();
}

static bool reg_mask_may_intersect(RegMask* a, RegMask* b) {
    if (a == b) {
        return true;
    } else if (a->class == REG_CLASS_STK) {
        return b->may_spill || (b->class == REG_CLASS_STK && b->mask[0] == 0);
    } else if (b->class == REG_CLASS_STK) {
        return a->may_spill || (a->class == REG_CLASS_STK && a->mask[0] == 0);
    } else if (a->class != b->class) {
        return false;
    }

    assert(a->count == b->count);
    FOR_N(i, 0, a->count) {
        if ((a->mask[i] & b->mask[i]) != 0) {
            return true;
        }
    }

    return false;
}

static bool vreg_is_fixed(Ctx* restrict ctx, LSRA* restrict ra, int id) {
    int class = ctx->vregs[id].mask->class;
    uint32_t base = id - ra->fixed[class];
    return base < ctx->num_regs[class];
}

static void dump_sched(Ctx* restrict ctx) {
    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        printf("BB %zu:\n", i);
        aarray_for(i, mbb->items) {
            printf("  ");
            tb_print_dumb_node(NULL, mbb->items[i]);
            printf("\n");
        }
    }
}

// static const char* GPR_NAMES[] = { "X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8",  "X9", "X10", "X11", "X12", "X13", "X14", "X15" };
static const char* GPR_NAMES[] = { "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI", "R8",  "R9", "R10", "R11", "R12", "R13", "R14", "R15" };
static void print_reg_name(int rg, int num) {
    if (rg == 1) {
        // printf("R%d", num);
        printf("%s", GPR_NAMES[num]);
    } else if (rg == 2) {
        printf("XMM%d", num);
    } else if (rg == REG_CLASS_STK) {
        printf("[sp + %d]", num*8);
    } else {
        tb_todo();
    }
}

static void lsra_print_vreg(Ctx* restrict ctx, LSRA* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4lld cost=%.2f ", vreg - ctx->vregs, cost);
    tb__print_regmask(vreg->mask);
    printf("\n");
    if (vreg->n) {
        printf("#   ");
        tb_print_dumb_node(NULL, vreg->n);
        printf("\n");
    }
}

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena) {
    LSRA ra = { .ctx = ctx, .arena = arena };
    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        ra.fixed    = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));

        FOR_N(i, 0, ctx->num_classes) {
            size_t count = ctx->num_regs[i];
            if (max_regs_in_class < count) {
                max_regs_in_class = count;
            }

            int base = aarray_length(ctx->vregs);
            FOR_N(j, 0, count) {
                RegMask* mask = intern_regmask(ctx, i, false, i == 0 ? j : 1ull << j);
                aarray_push(ctx->vregs, (VReg){
                        .class = i,
                        .assigned = j,
                        .mask = mask,
                        .spill_cost = INFINITY,
                    });
            }
            ra.fixed[i] = base;
        }
        ra.num_regs  = ctx->num_regs;
        assert(max_regs_in_class < 64 && "TODO: we assume some 64bit masks in places lol");
    }

    // probably gonna throw into the arena later but the important bit is that
    // new nodes inherit liveness from some other node.
    //
    // new GVN -> old GVN
    ra.fwd_table = nl_table_alloc(32);
    ra.spills = dyn_array_create(int, 32);

    // create timeline & insert moves
    CUIK_TIMED_BLOCK("insert legalizing moves") {
        FOR_N(i, 0, ctx->bb_count) {
            MachineBB* mbb = &ctx->machine_bbs[i];
            size_t j = 0; // we do insert things while iterating
            for (; j < aarray_length(mbb->items); j++) {
                TB_Node* n = mbb->items[j];
                int tmp_count = ctx->tmp_count(ctx, n);

                RegMask** ins = ctx->ins;
                ctx->constraint(ctx, n, ins);

                // insert input copies (temporaries & clobbers never introduce
                // these so we're safe don't check those)
                size_t in_count = n->input_count;
                FOR_N(k, 1, in_count) if (n->inputs[k]) {
                    TB_Node* in = n->inputs[k];
                    RegMask* in_mask = ins[k];
                    if (in_mask == &TB_REG_EMPTY) { continue; }

                    VReg* in_def = node_vreg(ctx, in);

                    int hint = fixed_reg_mask(in_mask);
                    if (hint >= 0 && in_def->mask->class == in_mask->class) {
                        in_def->hint_vreg = ra.fixed[in_mask->class] + hint;
                    }

                    // we resolve use-def conflicts with a spill move, either when:
                    //   * the use and def classes don't match.
                    //   * the use mask is more constrained than the def.
                    //   * it's on both ends to avoid stretching fixed intervals.
                    bool both_fixed = hint >= 0 && reg_mask_eq(in_def->mask, in_mask);
                    if (tb__reg_mask_less(ctx, in_def->mask, in_mask) || both_fixed) {
                        RegMask* in_def_mask = in_def->mask;
                        if (both_fixed) {
                            in_def_mask = ctx->normie_mask[in_def->mask->class];
                        }

                        TB_OPTDEBUG(REGALLOC)(printf("  TEMP "), tb__print_regmask(in_def_mask), printf(" -> "), tb__print_regmask(in_mask), printf("\n"));

                        // construct copy (either to a fixed interval or a new masked interval)
                        TB_Node* tmp = tb_alloc_node(f, TB_MACH_COPY, in->dt, 2, sizeof(TB_NodeMachCopy));
                        set_input(f, tmp, in, 1);
                        set_input(f, n, tmp,  k);
                        TB_NODE_SET_EXTRA(tmp, TB_NodeMachCopy, .def = in_mask, .use = in_def_mask);

                        // schedule the split right before use
                        tb__insert_before(ctx, f, tmp, n);
                        if (hint >= 0) {
                            assert(hint < ctx->num_regs[in_mask->class]);
                            int fixed_vreg = ra.fixed[in_mask->class] + hint;
                            aarray_insert(ctx->vreg_map, tmp->gvn, fixed_vreg);
                        } else {
                            VReg* tmp_vreg = tb__set_node_vreg(ctx, tmp);
                            tmp_vreg->mask = in_mask;
                        }
                        j += 1;
                    }
                }

                int vreg_id = ctx->vreg_map[n->gvn];
                if (tmp_count > 0) {
                    // used for clobbers/scratch but more importantly they're not bound to a node.
                    Tmps* tmps  = tb_arena_alloc(arena, sizeof(Tmps) + tmp_count*sizeof(int));
                    tmps->count = tmp_count;
                    nl_table_put(&ctx->tmps_map, n, tmps);

                    FOR_N(k, in_count, in_count + tmp_count) {
                        RegMask* in_mask = ins[k];
                        assert(in_mask != &TB_REG_EMPTY);

                        int fixed = fixed_reg_mask(in_mask);
                        if (fixed >= 0) {
                            // insert new range to the existing vreg
                            tmps->elems[k - in_count] = ra.fixed[in_mask->class] + fixed;
                        } else {
                            tmps->elems[k - in_count] = aarray_length(ctx->vregs);
                            aarray_push(ctx->vregs, (VReg){ .n = n, .mask = in_mask, .assigned = -1, .spill_cost = INFINITY });
                        }
                    }
                }

                if (vreg_id > 0) {
                    VReg* vreg = &ctx->vregs[vreg_id];
                    RegMask* def_mask = vreg->mask;

                    vreg->spill_cost = NAN;

                    // if we're writing to a fixed interval, insert copy
                    // such that we only guarentee a fixed location at the
                    // def site.
                    int reg = fixed_reg_mask(def_mask);
                    if (reg >= 0 && n->type != TB_MACH_COPY) {
                        int fixed_vreg = ra.fixed[def_mask->class] + reg;
                        aarray_insert(ctx->vreg_map, n->gvn, fixed_vreg);

                        // construct copy (either to a fixed interval or a new masked interval)
                        TB_Node* tmp = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                        subsume_node2(f, n, tmp);
                        set_input(f, tmp, n, 1);
                        TB_NODE_SET_EXTRA(tmp, TB_NodeMachCopy, .def = ctx->normie_mask[def_mask->class], .use = def_mask);

                        // schedule the split right after def
                        tb__insert_after(ctx, f, tmp, n);
                        VReg* tmp_vreg = tb__set_node_vreg(ctx, tmp);
                        tmp_vreg->mask = ctx->normie_mask[def_mask->class];
                        tmp_vreg->hint_vreg = fixed_vreg;

                        nl_table_put(&ra.fwd_table, (void*) (uintptr_t) n->gvn, (void*) (uintptr_t) tmp->gvn);
                        j += 1;
                    }
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("allocate") {
        int rounds = 0;
        int old_spills = ctx->num_spills;

        for (;;) {
            #if TB_OPTDEBUG_REGALLOC
            rounds += 1;
            printf("###############################\n");
            printf("#  ROUND %-4d                 #\n", rounds);
            printf("###############################\n");
            #endif

            TB_ArenaSavepoint sp = tb_arena_save(arena);
            if (allocate_loop(ctx, &ra, arena)) {
                break;
            }

            // spill & retry
            __debugbreak();

            ctx->num_spills = old_spills;
            tb_arena_restore(arena, sp);
        }
    }

    nl_table_free(ra.fwd_table);
}

static bool interfere_in_block(Ctx* restrict ctx, LSRA* restrict ra, TB_Node* lhs, TB_Node* rhs, TB_BasicBlock* block) {
    bool lhs_live_out = set_get(&block->live_out, lhs->gvn);
    bool rhs_live_out = set_get(&block->live_out, rhs->gvn);
    if (lhs_live_out && rhs_live_out) {
        return true;
    } else if (!lhs_live_out && !rhs_live_out) {
        TB_Node *first = lhs, *last = rhs;
        if (ra->order[lhs->gvn] > ra->order[rhs->gvn]) {
            first = rhs, last = lhs;
        }

        FOR_USERS(u, first) {
            TB_Node* un = USERN(u);
            if (block == ctx->f->scheduled[un->gvn] && ra->order[un->gvn] > ra->order[rhs->gvn]) {
                return true;
            }
        }
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
        }

        FOR_USERS(u, lhs) {
            TB_Node* un = USERN(u);
            if (block == ctx->f->scheduled[un->gvn] && ra->order[un->gvn] > ra->order[rhs->gvn]) {
                return true;
            }
        }
    }

    return false;
}

static bool interfere(Ctx* restrict ctx, LSRA* restrict ra, TB_Node* lhs, TB_Node* rhs) {
    TB_BasicBlock* lhs_block = ctx->f->scheduled[lhs->gvn];
    TB_BasicBlock* rhs_block = ctx->f->scheduled[rhs->gvn];

    if (lhs_block == rhs_block) {
        // TODO(NeGate): collect stats, this is supposed to be a relatively common (which i believe but how common?)
        return interfere_in_block(ctx, ra, lhs, rhs, lhs_block);
    }

    return interfere_in_block(ctx, ra, lhs, rhs, lhs_block)
        || interfere_in_block(ctx, ra, rhs, lhs, rhs_block);
}

static bool allocate_reg(Ctx* restrict ctx, LSRA* restrict ra, int vreg_id) {
    if (set_get(&ra->future_active, vreg_id)) {
        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, vreg_id);
        set_put(&ra->active, vreg_id);
        return true;
    }

    VReg* vreg = &ctx->vregs[vreg_id];

    #if TB_OPTDEBUG_REGALLOC
    lsra_print_vreg(ctx, ra, vreg);
    #endif

    RegMask* mask = vreg->mask;
    assert(mask->count == 1);

    uint64_t in_use = ~mask->mask[0];
    FOREACH_SET(i, ra->active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class && interfere(ctx, ra, vreg->n, other->n)) {
            in_use |= (1ull << other->assigned);
        }
    }

    if (in_use == UINT64_MAX) {
        return false;
    }

    vreg->class = mask->class;

    int hint_reg = vreg->hint_vreg >= 0
        && ctx->vregs[vreg->hint_vreg].class == mask->class
        ?  ctx->vregs[vreg->hint_vreg].assigned
        :  -1;

    if (hint_reg >= 0 && (in_use & (1ull << hint_reg)) == 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf(" (HINTED)\n"));
        vreg->assigned = hint_reg;
    } else {
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        vreg->assigned = tb_ffs64(~in_use) - 1;
    }
    return true;
}

static int last_use_in_bb(LSRA* restrict ra, TB_BasicBlock* bb, TB_Node* n) {
    int l = 0;
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (l < ra->order[un->gvn]) {
            l = ra->order[un->gvn];
        }
    }
    return l;
}

// returns false on failure
static bool allocate_loop(Ctx* restrict ctx, LSRA* restrict ra, TB_Arena* arena) {
    ra->active        = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->future_active = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->live_out      = set_create_in_arena(arena, ctx->f->node_count);

    // unlike linear scan, we only need order information no blocks.
    ra->order = tb_arena_alloc(arena, ctx->f->node_count * sizeof(int));

    TB_BasicBlock** scheduled = ctx->f->scheduled;

    int timeline = 0;
    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        for (size_t j = 0; j < aarray_length(mbb->items); j++) {
            TB_Node* n = mbb->items[j];
            ra->order[n->gvn] = timeline++;
        }
    }

    dump_sched(ctx);

    cuikperf_region_start("main loop", NULL);
    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb    = &ctx->machine_bbs[i];
        TB_BasicBlock* bb = mbb->bb;
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        __debugbreak();

        // expire intervals for block:
        //   for any values which die this block, pause them
        //   if later blocks will have them live-in and if
        //   not, kill them.
        FOREACH_SET(j, ra->live_out) if (!set_get(live_in, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id == 0) { continue; }

            bool pause = false;
            FOR_N(k, i + 1, ctx->bb_count) {
                MachineBB* other = &ctx->machine_bbs[k];
                if (set_get(&other->bb->live_in, j)) {
                    // move to future active
                    set_put(&ra->future_active, vreg_id);
                    pause = true;
                    break;
                }
            }

            if (pause) {
                TB_OPTDEBUG(REGALLOC)(printf("#   pause V%-4d\n", vreg_id));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
            }
            set_remove(&ra->active, vreg_id);
        }

        // start intervals:
        FOREACH_SET(j, *live_in) if (!set_get(&ra->live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0 && !allocate_reg(ctx, ra, vreg_id)) {
                cuikperf_region_end();
                return false;
            }
        }

        for (size_t j = 0; j < aarray_length(mbb->items); j++) {
            TB_Node* n = mbb->items[j];
            int def_t  = ra->order[n->gvn];

            // expire intervals for node
            FOR_N(k, 1, n->input_count) {
                TB_Node* in = n->inputs[k];
                if (in == NULL) { continue; }

                int vreg_id = ctx->vreg_map[in->gvn];
                if (vreg_id == 0) {
                    set_remove(&ra->live_out, in->gvn);
                    continue;
                }

                if (!set_get(&ra->live_out, in->gvn)) {
                    // duplicate input
                    continue;
                }

                int last_use = last_use_in_bb(ra, bb, in);
                if (set_get(&ra->live_out, in->gvn)) {
                    if (scheduled[in->gvn] != mbb->bb &&
                        // j has no later uses than i
                        last_use < def_t &&
                        // j is defined after i
                        ra->order[in->gvn] > def_t) {
                        // liveness hole within cur
                        set_remove(&ra->live_out, in->gvn);
                        set_remove(&ra->active, vreg_id);
                        set_put(&ra->future_active, vreg_id);
                    }
                    continue;
                }

                set_remove(&ra->live_out, in->gvn);

                bool pause = false;
                if (last_use > def_t) {
                    pause = true;
                } else {
                    FOR_N(k, i + 1, ctx->bb_count) {
                        MachineBB* other = &ctx->machine_bbs[k];
                        if (set_get(&other->bb->live_in, in->gvn)) {
                            // move to future active
                            set_put(&ra->future_active, vreg_id);
                            pause = true;
                            break;
                        }
                    }
                }

                if (pause) {
                    TB_OPTDEBUG(REGALLOC)(printf("#   pause V%-4d\n", vreg_id));
                } else {
                    TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
                }
                set_remove(&ra->active, vreg_id);
            }

            // allocate free register
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                if (!allocate_reg(ctx, ra, vreg_id)) {
                    cuikperf_region_end();
                    return false;
                }
                set_put(&ra->live_out, vreg_id);
            }
        }
    }

    cuikperf_region_end();
    return true;
}
