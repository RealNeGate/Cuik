// Efficient global register allocation (2020):
//   https://arxiv.org/pdf/2011.05608.pdf
#include "codegen.h"
#include <limits.h>
#include <float.h>

#define FOREACH_SET(it, set) \
FOR_N(_i, 0, ((set).capacity + 63) / 64) FOR_BIT(it, _i*64, (set).data[_i])

typedef struct {
    Ctx* ctx;
    TB_Arena* arena;

    int num_classes;
    int* num_regs;
    int* fixed;

    DynArray(int) spills;

    int* order;
    Set active;
    Set future_active;
    Set live_out;

    uint64_t* in_use;
} Rogers;

#define BND(arr, i, limit) ((i) >= (limit) ? abort() : 0, arr)[i]

static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena);
static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs);

// Helpers
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

static void redo_dataflow(Ctx* restrict ctx, TB_Arena* arena) {
    TB_Function* f = ctx->f;
    // dump_sched(ctx);

    TB_Node** rpo_nodes = f->worklist->items;
    size_t bb_count     = ctx->cfg.block_count;
    FOR_N(i, 0, bb_count) {
        TB_Node* n = rpo_nodes[i];
        TB_BasicBlock* bb = f->scheduled[n->gvn];
        bb->live_in  = set_create_in_arena(arena, f->node_count);
        bb->live_out = set_create_in_arena(arena, f->node_count);
    }
    tb_dataflow(f, arena, ctx->cfg, rpo_nodes);
}

static RegMask* constraint_in(Ctx* ctx, TB_Node* n, int i) {
    ctx->constraint(ctx, n, ctx->ins);
    return ctx->ins[i];
}

static void rematerialize(Ctx* ctx, TB_Node* n) {
    // assert(n->input_count == 1 && "for now remat only happens for simple stuff like constants");

    size_t extra = extra_bytes(n);
    TB_Function* f = ctx->f;
    TB_Node* root = f->root_node;
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
        assert(use_i != 0);
        RegMask* in_mask = constraint_in(ctx, use_n, use_i);

        // remat per use site
        TB_Node* remat = tb_alloc_node(f, n->type, n->dt, n->input_count, extra);
        memcpy(remat->extra, n->extra, extra);
        FOR_N(j, 0, n->input_count) if (n->inputs[j]) {
            remat->inputs[j] = n->inputs[j];
            add_user(f, remat, n->inputs[j], j);
        }

        set_input(f, use_n, remat, use_i);

        // schedule the split right before use
        tb__insert_before(ctx, ctx->f, remat, use_n);
        VReg* reload_vreg = tb__set_node_vreg(ctx, remat);

        // reloads are unlikely to spill... but not impossible
        reload_vreg->spill_bias = 1e10;

        RegMask* remat_mask = ctx->constraint(ctx, remat, NULL);
        reload_vreg->mask = tb__reg_mask_meet(ctx, in_mask, remat_mask);
        assert(reload_vreg->mask != &TB_REG_EMPTY && "TODO hard split from rematerializing");

        // if it's remat'ing a copy, we should edit the def mask to match the use
        if (remat->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(remat);
            cpy->def = reload_vreg->mask;
        }

        TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   v%zu: remat (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, remat->gvn));
    }
    tb_arena_restore(f->tmp_arena, sp);

    // delete the original def
    ctx->vreg_map[n->gvn] = 0;
    tb__remove_node(ctx, f, n);
    tb_kill_node(f, n);
}

static void spill_entire_lifetime(Ctx* ctx, VReg* to_spill, RegMask* spill_mask, bool conflict) {
    TB_Function* f = ctx->f;
    TB_Node* n = to_spill->n;
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   v%zu: spill  (%%%u)\x1b[0m\n", to_spill - ctx->vregs, n->gvn));

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

        // if it's already a machine copy, inserting an extra one is useless
        if (use_n->type == TB_MACH_COPY) {
            TB_NodeMachCopy* cpy = TB_NODE_GET_EXTRA(use_n);
            cpy->use = spill_mask;
            continue;
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
}

static void better_spill_range(Ctx* ctx, Rogers* restrict ra, VReg* to_spill, RegMask* spill_mask, size_t old_node_count) {
    TB_Function* f = ctx->f;
    TB_Node* n = to_spill->n;
    TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   v%zu: spill  (%%%u)\x1b[0m\n", to_spill - ctx->vregs, n->gvn));

    to_spill->mask = spill_mask;

    size_t bb_count = ctx->bb_count;
    TB_ArenaSavepoint sp = tb_arena_save(f->tmp_arena);

    int* reload_t = tb_arena_alloc(f->tmp_arena, bb_count * sizeof(int));
    FOR_N(i, 0, bb_count) { reload_t[i] = 0; }

    TB_BasicBlock** scheduled = f->scheduled;
    FOR_USERS(u, n) {
        TB_BasicBlock* bb = scheduled[USERN(u)->gvn];
        int use_t         = ra->order[USERN(u)->gvn];

        // earliest point within the BB
        if (reload_t[bb->id] == 0 || use_t < reload_t[bb->id]) {
            reload_t[bb->id] = use_t;
        }
    }

    TB_Node** reload_n = tb_arena_alloc(f->tmp_arena, bb_count * sizeof(TB_Node*));

    // insert reload nodes in each relevant BB
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] > 0) {
            // reload per use site
            reload_n[i] = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
            set_input(f, reload_n[i], n, 1);
            TB_NODE_SET_EXTRA(reload_n[i], TB_NodeMachCopy, .def = NULL, .use = spill_mask);

            TB_Node* at = NULL;
            TB_BasicBlock* bb = f->scheduled[f->worklist->items[i]->gvn];
            MachineBB* mbb = &ctx->machine_bbs[bb->order];
            FOR_N(j, 0, aarray_length(mbb->items)) {
                TB_Node* n = mbb->items[j];
                if (n->gvn < old_node_count && ra->order[n->gvn] == reload_t[i]) {
                    at = n;
                    break;
                }
            }
            assert(at != NULL);

            // schedule the split right before use
            assert(at->gvn != n->gvn);
            tb__insert_before(ctx, ctx->f, reload_n[i], at);
            VReg* reload_vreg = tb__set_node_vreg(ctx, reload_n[i]);

            TB_OPTDEBUG(REGALLOC)(printf("\x1b[33m#   v%zu: reload (%%%u)\x1b[0m\n", reload_vreg - ctx->vregs, reload_n[i]->gvn));
        }
    }

    for (size_t i = 0; i < n->user_count;) {
        TB_User* u        = &n->users[i];
        TB_BasicBlock* bb = f->scheduled[USERN(u)->gvn];

        TB_Node* reload = reload_n[bb->id];
        if (USERN(u) != reload) {
            set_input(f, USERN(u), reload, USERI(u));
        } else {
            i += 1;
        }
    }

    // insert mask that's ok
    FOR_N(i, 0, bb_count) {
        if (reload_t[i] > 0) {
            TB_Node* reload   = reload_n[i];
            VReg* reload_vreg = &ctx->vregs[ctx->vreg_map[reload->gvn]];
            assert(reload->user_count > 0);

            // this process might introduce hard-splits
            bool split = false;
            int class  = -1;
            FOR_USERS(u, reload) {
                RegMask* in_mask = constraint_in(ctx, USERN(u), USERI(u));
                RegMask* new_mask = tb__reg_mask_meet(ctx, reload_vreg->mask, in_mask);
                if (reload_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                    TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", reload_vreg - ctx->vregs));
                    split = true;
                }
                reload_vreg->mask = new_mask;
            }

            if (split) {
                // construct separate reloads
                assert(reload->type == TB_MACH_COPY);
                rematerialize(ctx, reload);
            } else {
                TB_NODE_SET_EXTRA(reload_n[i], TB_NodeMachCopy, .def = reload_vreg->mask, .use = spill_mask);
            }
        }
    }

    tb_arena_restore(f->tmp_arena, sp);
}

#if 0
// SS is short for spill state, it's used in one function so i don't
// care about the names.
enum { SS_UNDEF, SS_SPILLED, SS_RELOADED, SS_PHI };
static const char* ss_names[] = { "undef  ", "spill  ", "reload", "phi   " };

typedef struct {
    uint8_t start, body, end;
} SplitState;

int ss_meet(int a, int b) {
    if (a == b) { return a; }
    if (a == SS_UNDEF) { return b; }
    if (b == SS_UNDEF) { return a; }
    return SS_PHI;
}

static void split_vregs(Ctx* ctx, TB_Arena* arena, DynArray(int) spills) {
    int num_spills = dyn_array_length(spills);
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    size_t bb_count = ctx->bb_count;
    SplitState* split_state = tb_arena_alloc(arena, num_spills * bb_count * sizeof(SplitState));
    memset(split_state, 0, num_spills * bb_count * sizeof(SplitState));

    // initial requirements
    TB_BasicBlock** scheduled = ctx->f->scheduled;
    dyn_array_for(i, spills) {
        TB_Node* n = ctx->vregs[spills[i]].n;

        // mark all uses as requiring a reload (this isn't *necessarily* true but whatever)
        FOR_USERS(u, n) {
            TB_BasicBlock* use_bb = scheduled[USERN(u)->gvn];
            split_state[use_bb->id*num_spills + i].body = SS_RELOADED;
            split_state[use_bb->id*num_spills + i].end  = SS_RELOADED;
        }

        TB_BasicBlock* def_bb = scheduled[n->gvn];
        if (split_state[def_bb->id*num_spills + i].body != SS_RELOADED) {
            split_state[def_bb->id*num_spills + i].body = SS_SPILLED;
        }
        split_state[def_bb->id*num_spills + i].end = SS_SPILLED;
    }

    __debugbreak();
    dump_sched(ctx);

    // flow pass (find out which blocks need phis and which
    // unconditionally use one version of the value)
    CUIK_TIMED_BLOCK("find phis") {
        bool changes = false;
        TB_Node** rpo_nodes = ctx->f->worklist->items;

        do {
            #if 1
            printf("START: ");
            FOR_N(i, 0, bb_count) { printf("%s ", ss_names[split_state[i].start]); }
            printf("\nBODY:  ");
            FOR_N(i, 0, bb_count) { printf("%s ", ss_names[split_state[i].body]); }
            printf("\nEND:   ");
            FOR_N(i, 0, bb_count) { printf("%s ", ss_names[split_state[i].end]); }
            printf("\n\n");
            #endif

            changes = false;
            FOR_N(i, 0, bb_count) {
                TB_Node* n = rpo_nodes[i];
                TB_BasicBlock* bb = scheduled[n->gvn];

                TB_ArenaSavepoint sp = tb_arena_save(arena);

                int pred_count = (n->type == TB_PROJ && n->inputs[0]->type == TB_ROOT) ? 0 : n->input_count;
                int* preds = tb_arena_alloc(arena, n->input_count * sizeof(int));
                FOR_N(j, 0, pred_count) {
                    TB_Node* pred = cfg_get_pred(&ctx->cfg, n, j);
                    preds[i] = scheduled[pred->gvn]->id;
                }

                // transfer function for this wacky flow pass
                FOR_N(j, 0, num_spills) {
                    SplitState* ss = &split_state[i*num_spills + j];
                    if (ss->end != SS_SPILLED) {
                        // meet over all paths
                        uint8_t mop = SS_UNDEF;
                        FOR_N(k, 0, pred_count) {
                            mop = ss_meet(mop, split_state[preds[k]*num_spills + j].end);
                        }

                        if (ss->start != mop) {
                            ss->start = mop;
                            changes = true;
                        }

                        // body didn't have a fixed use? ok let's just flow our start value
                        if (ss->body == SS_UNDEF) {
                            ss->body = ss->end = mop;
                        }
                    }
                }
                tb_arena_restore(arena, sp);
            }
        } while (changes);
    }

    TB_Node** spill_node = tb_arena_alloc(arena, num_spills * bb_count * sizeof(SplitState));

    FOR_N(i, 0, bb_count) {
        FOR_N(j, 0, num_spills) {
            SplitState* ss = &split_state[i*num_spills + j];
            TB_Node* body  = 0;

            // copy on entry
            TB_Node* start = 0;
            if (ss->start != ss->body) {
                __debugbreak();
            }

            // copy on exit
            if (ss->body != ss->end) {
                __debugbreak();
            }
        }
    }

    __debugbreak();
    tb_arena_restore(arena, sp);
}
#endif

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

static bool vreg_is_fixed(Ctx* restrict ctx, Rogers* restrict ra, int id) {
    int class = ctx->vregs[id].mask->class;
    return id >= ra->fixed[class] && id < ra->fixed[class] + ctx->num_regs[class];
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
        if (num >= STACK_BASE_REG_NAMES) {
            num -= STACK_BASE_REG_NAMES;
            printf("[bp - %d]", num*8);
        } else {
            printf("[sp + %d]", num*8);
        }
    } else {
        tb_todo();
    }
}

static void rogers_print_vreg(Ctx* restrict ctx, Rogers* restrict ra, VReg* vreg) {
    float cost = get_spill_cost(ctx, vreg);
    printf("# V%-4lld cost=%.2f ", vreg - ctx->vregs, cost);
    tb__print_regmask(vreg->mask);
    printf("\n");
}

void tb__rogers(Ctx* restrict ctx, TB_Arena* arena) {
    Rogers ra = { .ctx = ctx, .arena = arena };
    TB_Function* f = ctx->f;
    size_t node_count = f->node_count;

    TB_Worklist* ws = f->worklist;

    // i keep the nodes for the BB entries at the bottom of the worklist at this time so
    // ideally don't kill that info. visited bits are gg tho
    size_t old = dyn_array_length(ws->items);
    worklist_clear_visited(ws);

    // creating fixed vregs which coalesce all fixed reg uses
    // so i can more easily tell when things are asking for them.
    int max_regs_in_class = 0;
    CUIK_TIMED_BLOCK("pre-pass on fixed intervals") {
        ra.fixed  = tb_arena_alloc(arena, ctx->num_classes * sizeof(int));
        ra.in_use = tb_arena_alloc(arena, ctx->num_classes * sizeof(uint64_t));

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
        assert(max_regs_in_class <= 64 && "TODO: we assume some 64bit masks in places lol");
    }

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

                    VReg* in_vreg = node_vreg(ctx, in);
                    int hint = fixed_reg_mask(in_mask);
                    if (hint >= 0 && in_vreg->mask->class == in_mask->class) {
                        in_vreg->hint_vreg = ra.fixed[in_mask->class] + hint;
                    }

                    // intersect use masks with the vreg's mask, if it becomes empty we've
                    // got a hard-split (not necessarily spilling to the stack)
                    RegMask* new_mask = tb__reg_mask_meet(ctx, in_vreg->mask, ins[k]);
                    if (in_vreg->mask != &TB_REG_EMPTY && new_mask == &TB_REG_EMPTY) {
                        TB_OPTDEBUG(REGALLOC)(printf("HARD-SPLIT on V%td\n", in_vreg - ctx->vregs));
                        dyn_array_put(ra.spills, in_vreg - ctx->vregs);
                    }
                    in_vreg->mask = new_mask;
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

                    // once we have our
                    if (n->type == TB_MACH_COPY && n->inputs[1]->type == TB_PHI) {
                        worklist_push(ws, n);
                    }

                    // if we're writing to a fixed interval, insert copy
                    // such that we only guarentee a fixed location at the
                    // def site.
                    /*int reg = fixed_reg_mask(def_mask);
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
                        j += 1;
                    }*/
                }
            }
        }
    }

    bool changes = false;

    // we aggressively place phi copies, let's filter some of them out
    if (dyn_array_length(ws->items) > old) {
        cuikperf_region_start("phi coalesce", NULL);
        TB_ArenaSavepoint sp = tb_arena_save(arena);
        compute_ordinals(ctx, &ra, arena);

        // this avoids the subsume_node calls adding nodes to the list, they'd
        // do this if you remove nodes such that they get DCE'd
        f->worklist = NULL;

        while (dyn_array_length(ws->items) > old) {
            TB_Node* n = worklist_pop(ws);
            if (!interfere(ctx, &ra, n, n->inputs[1])) {
                // delete copy
                tb__remove_node(ctx, f, n);
                subsume_node(f, n, n->inputs[1]);
                changes = true;
            }
        }

        // RA calls might add dead nodes but we don't care
        f->worklist = ws;

        tb_arena_restore(f->tmp_arena, sp);
        cuikperf_region_end();
    }

    // resolving hard-splits
    if (dyn_array_length(ra.spills) > 0) {
        cuikperf_region_start("hard splits", NULL);
        // insert hard split code
        FOR_N(i, 0, dyn_array_length(ra.spills)) {
            VReg* vreg = &ctx->vregs[ra.spills[i]];
            RegMask* mask = ctx->constraint(ctx, vreg->n, NULL);
            spill_entire_lifetime(ctx, vreg, mask, true);
        }
        dyn_array_clear(ra.spills);
        cuikperf_region_end();
        changes = true;
    }

    if (changes) {
        // recompute liveness
        redo_dataflow(ctx, arena);
    }

    int rounds = 0;
    int old_spills = ctx->num_spills;

    cuikperf_region_start("allocate", NULL);
    for (;;) {
        #if TB_OPTDEBUG_REGALLOC
        rounds += 1;
        printf("###############################\n");
        printf("#  ROUND %-4d                 #\n", rounds);
        printf("###############################\n");
        #endif

        TB_ArenaSavepoint sp = tb_arena_save(arena);

        cuikperf_region_start("main loop", NULL);
        bool success = allocate_loop(ctx, &ra, arena);
        cuikperf_region_end();

        if (success) {
            break;
        }

        // undo assignments
        FOR_N(i, 1, aarray_length(ctx->vregs)) {
            if (!vreg_is_fixed(ctx, &ra, i) && ctx->vregs[i].mask->class != REG_CLASS_STK) {
                ctx->vregs[i].class = 0;
                ctx->vregs[i].assigned = -1;
            }
        }

        cuikperf_region_start("insert spills", NULL);
        assert(dyn_array_length(ra.spills) == 1);

        int vreg_id = ra.spills[0];
        TB_Node* n  = ctx->vregs[vreg_id].n;
        RegMask* vreg_mask = ctx->vregs[vreg_id].mask;

        // rematerialization candidates will delete the original def and for now, they'll
        // reload per use site (although we might wanna coalesce some later on).
        size_t old_node_count = f->node_count;
        if (can_remat(ctx, n)) {
            rematerialize(ctx, n);
        } else {
            #if 0
            split_vregs(ctx, arena, ra.spills);
            #else
            RegMask* old_rm = ctx->vregs[vreg_id].mask;

            // if the old mask was really tight, let's loosen
            // it without necessarily spilling to the stack.
            RegMask* spill_rm = intern_regmask(ctx, 1, true, 0);
            /*if (fixed_reg_mask(old_rm) >= 0) {
                spill_rm = ctx->normie_mask[old_rm->class];
            } else {
                spill_rm = intern_regmask(ctx, 1, true, 0);
            }*/

            VReg* spill_vreg = NULL;
            if (n->type == TB_PHI) {
                ctx->vregs[vreg_id].spill_cost = INFINITY;
                better_spill_range(ctx, &ra, &ctx->vregs[vreg_id], spill_rm, old_node_count);
            } else {
                TB_Node* spill_n = tb_alloc_node(f, TB_MACH_COPY, n->dt, 2, sizeof(TB_NodeMachCopy));
                subsume_node2(f, n, spill_n);
                set_input(f, spill_n, n, 1);
                TB_NODE_SET_EXTRA(spill_n, TB_NodeMachCopy, .def = spill_rm, .use = vreg_mask);
                tb__insert_after(ctx, f, spill_n, n);

                ctx->vregs[vreg_id].mask = ctx->constraint(ctx, n, NULL);
                ctx->vregs[vreg_id].spill_cost = NAN;

                spill_vreg = tb__set_node_vreg(ctx, spill_n);
                spill_vreg->spill_cost = INFINITY;
                better_spill_range(ctx, &ra, spill_vreg, spill_rm, old_node_count);
            }
            #endif
        }
        cuikperf_region_end();

        ctx->num_spills = old_spills;
        dyn_array_clear(ra.spills);
        tb_arena_restore(arena, sp);

        // recompute liveness
        redo_dataflow(ctx, arena);
    }
    cuikperf_region_end();
}

static TB_Node* phi_move_in_block(TB_BasicBlock** scheduled, TB_BasicBlock* block, TB_Node* phi) {
    FOR_N(i, 1, phi->input_count) {
        if (scheduled[phi->inputs[i]->gvn] == block) {
            return phi->inputs[i];
        }
    }
    return NULL;
}

static bool interfere_in_block(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs, TB_BasicBlock* block) {
    assert(lhs != rhs && "i... why?");

    // phis might have a liveness hole in the middle
    bool lhs_live_out = set_get(&block->live_out, lhs->gvn);
    bool rhs_live_out = set_get(&block->live_out, rhs->gvn);
    if (lhs->type == TB_PHI || rhs->type == TB_PHI) {
        TB_Node *phi = rhs, *other = lhs;
        if (lhs->type == TB_PHI && rhs->type != TB_PHI) {
            phi = lhs, other = rhs;
        }

        if (set_get(&block->live_out, phi->gvn)) {
            TB_Node* move = phi_move_in_block(ctx->f->scheduled, block, phi);

            // some phis have two ranges within a block
            //
            //   a = phi ..., c  OLD
            //   b = a + 1
            //   c = move b      NEW
            //
            // the phis themselves refer to the value in it's latest state, so after the TB_MACH_MOVE
            // that's gonna be the "next" state. we insert copies and coalesce them away such that by
            // this point in the pipeline, a phi CANNOT be referenced by the "bottom half", only the
            // potential copy can.
            if (move) {
                // because we split the lifetimes, the rest of the function should treat
                // the phi as not being live out.
                if (lhs == phi) { lhs_live_out = false; }
                else { rhs_live_out = false; }
            }
        }
    }

    if (lhs_live_out && rhs_live_out) {
        return true;
    } else if (!lhs_live_out && !rhs_live_out) {
        TB_Node *first = lhs, *last = rhs;
        if (ra->order[lhs->gvn] > ra->order[rhs->gvn]) {
            first = rhs, last = lhs;
        }

        FOR_USERS(u, first) {
            TB_Node* un = USERN(u);
            if (block == ctx->f->scheduled[un->gvn] && ra->order[un->gvn] > ra->order[last->gvn]) {
                return true;
            }
        }
    } else {
        if (lhs_live_out) {
            SWAP(TB_Node*, lhs, rhs);
        }

        TB_BasicBlock* rhs_block = ctx->f->scheduled[rhs->gvn];
        FOR_USERS(u, lhs) {
            TB_Node* un = USERN(u);
            if (rhs_block == ctx->f->scheduled[un->gvn] && ra->order[un->gvn] > ra->order[rhs->gvn]) {
                return true;
            }
        }
    }

    return false;
}

static bool interfere(Ctx* restrict ctx, Rogers* restrict ra, TB_Node* lhs, TB_Node* rhs) {
    TB_BasicBlock* lhs_block = ctx->f->scheduled[lhs->gvn];
    TB_BasicBlock* rhs_block = ctx->f->scheduled[rhs->gvn];

    if (lhs_block == rhs_block) {
        return interfere_in_block(ctx, ra, lhs, rhs, lhs_block);
    }

    return interfere_in_block(ctx, ra, lhs, rhs, lhs_block)
        || interfere_in_block(ctx, ra, rhs, lhs, rhs_block);
}

static int last_use_in_bb(TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* n) {
    int l = 0;
    FOR_USERS(u, n) {
        TB_Node* un = USERN(u);
        if (scheduled[un->gvn] == bb && l < ra->order[un->gvn]) {
            l = ra->order[un->gvn];
        }
    }
    return l;
}

static bool interfere_with_def(TB_BasicBlock** scheduled, Rogers* restrict ra, TB_BasicBlock* bb, TB_Node* def, TB_Node* other) {
    // live-in & live-out => INTERFERE
    // _______ & live-out => INTERFERE if their.def_t < our.def_t
    // live-in & ________ => INTERFERE if our.def_t   < their.last_use
    if (set_get(&bb->live_in, other->gvn)) {
        if (set_get(&bb->live_out, other->gvn)) {
            return true;
        }

        int def_t = ra->order[def->gvn];
        FOR_USERS(u, other) {
            TB_Node* un = USERN(u);
            if (scheduled[un->gvn] == bb && ra->order[un->gvn] > def_t) {
                return true;
            }
        }
    }

    return false;
}

static bool allocate_reg(Ctx* restrict ctx, Rogers* restrict ra, int vreg_id, uint64_t in_use) {
    if (set_get(&ra->future_active, vreg_id)) {
        TB_OPTDEBUG(REGALLOC)(printf("#   woke up V%d\n", vreg_id));

        // we're done with some lifetime hole, time to lock in
        set_remove(&ra->future_active, vreg_id);
        set_put(&ra->active, vreg_id);
        return true;
    }

    VReg* vreg = &ctx->vregs[vreg_id];
    #if TB_OPTDEBUG_REGALLOC
    rogers_print_vreg(ctx, ra, vreg);
    #endif

    if (vreg->assigned >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
        set_put(&ra->active, vreg_id);
        return true;
    } else if (reg_mask_is_spill(vreg->mask)) {
        int empty_slot = ctx->num_spills++;
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to SPILL%u\n", empty_slot));
        vreg->class    = REG_CLASS_STK;
        vreg->assigned = STACK_BASE_REG_NAMES + empty_slot;
        set_put(&ra->active, vreg_id);
        return true;
    } else if (vreg->mask->class == REG_CLASS_STK) {
        // fixed stack slot (args or param passing)
        TB_OPTDEBUG(REGALLOC)(printf("#   fixed as STK%"PRId64"\n", vreg->mask->mask[0]));
        vreg->class    = REG_CLASS_STK;
        vreg->assigned = vreg->mask->mask[0];
        set_put(&ra->active, vreg_id);
        return true;
    }

    RegMask* mask = vreg->mask;
    in_use |= ~mask->mask[0];

    cuikperf_region_start("interference", NULL);
    dyn_array_clear(ra->spills);

    // there's some cases where we'd rather spill ourselves over anything else we interfere
    // with, for instance with constants. another case is that we've got an overly tight constraint
    // and a spill would loosen that
    dyn_array_put(ra->spills, vreg_id);

    TB_OPTDEBUG(REGALLOC)(printf("#   "));
    FOREACH_SET(i, ra->active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class) {
            TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
            in_use |= (1ull << other->assigned);
            dyn_array_put(ra->spills, i);
        }
    }

    FOREACH_SET(i, ra->future_active) {
        VReg* other = &ctx->vregs[i];
        if (other->class == mask->class && interfere(ctx, ra, vreg->n, other->n)) {
            TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) future interferes as ", i, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
            in_use |= (1ull << other->assigned);
            dyn_array_put(ra->spills, i);
        }
    }

    // 2 address ops will interfere with their own inputs (except for
    // shared dst/src)
    TB_Node* n = vreg->n;
    int hint_vreg = vreg->hint_vreg;
    int shared_edge = ctx->node_2addr(n);
    if (shared_edge >= 0) {
        assert(shared_edge < n->input_count);
        FOR_N(k, 1, n->input_count) if (k != shared_edge) {
            TB_Node* in = n->inputs[k];
            VReg* in_vreg = node_vreg(ctx, in);
            if (in_vreg && in_vreg->class == mask->class) {
                TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", in_vreg - ctx->vregs, in->gvn), print_reg_name(in_vreg->class, in_vreg->assigned), printf("; "));
                in_use |= (1ull << in_vreg->assigned);
                dyn_array_put(ra->spills, in_vreg - ctx->vregs);
            }
        }

        hint_vreg = ctx->vreg_map[n->inputs[shared_edge]->gvn];
    }
    TB_OPTDEBUG(REGALLOC)(printf("\n"));
    cuikperf_region_end();

    TB_OPTDEBUG(REGALLOC)(printf("#   available: %#"PRIx64"\n", ~in_use));
    if (in_use == UINT64_MAX) {
        return false;
    }

    vreg->class = mask->class;

    int hint_reg = hint_vreg > 0
        && ctx->vregs[hint_vreg].class == mask->class
        ?  ctx->vregs[hint_vreg].assigned
        :  -1;

    if (hint_reg >= 0) {
        TB_OPTDEBUG(REGALLOC)(printf("#   hint as V%d (", hint_vreg), print_reg_name(vreg->class, hint_reg), printf(")\n"));
    }

    if (hint_reg >= 0 && (in_use & (1ull << hint_reg)) == 0) {
        vreg->assigned = hint_reg;
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf(" (HINTED)\n"));
    } else {
        vreg->assigned = tb_ffs64(~in_use) - 1;
        TB_OPTDEBUG(REGALLOC)(printf("#   assigned to "), print_reg_name(vreg->class, vreg->assigned), printf("\n"));
    }
    set_put(&ra->active, vreg_id);
    return true;
}

static void choose_decent_spill(Ctx* restrict ctx, Rogers* restrict ra, VReg* attempted_vreg, int useful_class, uint64_t useful_spill) {
    cuikperf_region_start("choose spill", NULL);

    bool can_spill_self = true;
    if (attempted_vreg && fixed_reg_mask(attempted_vreg->mask) >= 0) {
        // we can only spill ourselves if that meant loosening the vreg's mask
        RegMask* def = ctx->constraint(ctx, attempted_vreg->n, NULL);
        can_spill_self = (attempted_vreg->mask != def);
    }

    int best_spill = -1;
    float best_score = INFINITY;
    dyn_array_for(i, ra->spills) {
        int vreg_id = ra->spills[i];
        VReg* vreg  = &ctx->vregs[vreg_id];

        if (attempted_vreg == vreg) {
            if (!can_spill_self) {
                continue;
            }
        } else {
            // skip live values which we couldn't use to accomodate our RA failure
            if (vreg->class != useful_class || (useful_spill & (1ull << vreg->assigned)) == 0) {
                continue;
            }
        }

        float score = get_spill_cost(ctx, vreg);
        if (score < best_score) {
            if (best_spill >= 0) {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is a better spill! V%d (%f is better than %f)\n", vreg_id, best_spill, score, best_score));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#     V%d is... one of the spills of all time! %f\n", vreg_id, score));
            }
            best_score = score;
            best_spill = vreg_id;
        }
    }
    dyn_array_clear(ra->spills);
    cuikperf_region_end();

    assert(best_spill >= 0);
    dyn_array_put(ra->spills, best_spill);
}

static void compute_ordinals(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    // unlike linear scan, we only need order information no blocks.
    ra->order = tb_arena_alloc(arena, ctx->f->node_count * sizeof(int));

    int timeline = 1;
    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb = &ctx->machine_bbs[i];
        for (size_t j = 0; j < aarray_length(mbb->items); j++) {
            TB_Node* n = mbb->items[j];
            ra->order[n->gvn] = timeline++;
        }
    }
}

// returns false on failure
static bool allocate_loop(Ctx* restrict ctx, Rogers* restrict ra, TB_Arena* arena) {
    ra->active        = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->future_active = set_create_in_arena(arena, aarray_length(ctx->vregs));
    ra->live_out      = set_create_in_arena(arena, ctx->f->node_count);

    TB_BasicBlock** scheduled = ctx->f->scheduled;
    TB_Node* root = ctx->f->root_node;
    compute_ordinals(ctx, ra, arena);

    FOR_N(i, 0, ctx->bb_count) {
        MachineBB* mbb    = &ctx->machine_bbs[i];
        TB_BasicBlock* bb = mbb->bb;
        Set* live_in      = &bb->live_in;
        Set* live_out     = &bb->live_out;

        // expire intervals for block:
        //   for any values which die this block, pause them
        //   if later blocks will have them live-in and if
        //   not, kill them.
        FOREACH_SET(j, ra->live_out) if (!set_get(live_in, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id == 0) { continue; }

            bool pause = false;
            FOR_N(k, i, ctx->bb_count) {
                MachineBB* other = &ctx->machine_bbs[k];
                if (set_get(&other->bb->live_in, j)) {
                    // move to future active
                    set_put(&ra->future_active, vreg_id);
                    pause = true;
                    break;
                }
            }

            if (pause) {
                TB_OPTDEBUG(REGALLOC)(printf("#   sleep  V%-4d\n", vreg_id));
            } else {
                TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
            }
            set_remove(&ra->active, vreg_id);
            set_remove(&ra->live_out, j);
        }

        // start intervals:
        FOREACH_SET(j, *live_in) if (!set_get(&ra->live_out, j)) {
            int vreg_id = ctx->vreg_map[j];
            if (vreg_id > 0) {
                if (!allocate_reg(ctx, ra, vreg_id, 0)) {
                    RegMask* mask = ctx->vregs[vreg_id].mask;
                    choose_decent_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);
                    return false;
                }
            }
        }

        #if TB_OPTDEBUG_REGALLOC
        printf("#   live on BB start: ");
        FOREACH_SET(k, ra->live_out) if (ctx->vreg_map[k] > 0) {
            VReg* other = &ctx->vregs[ctx->vreg_map[k]];
            printf("V%d:", ctx->vreg_map[k]), print_reg_name(other->class, other->assigned), printf(" ");
        }
        printf("\n");
        #endif

        for (size_t j = 0; j < aarray_length(mbb->items); j++) {
            TB_Node* n = mbb->items[j];
            int def_t  = ra->order[n->gvn];
            if (is_proj(n) && n->inputs[0] != root) {
                continue;
            }

            #if TB_OPTDEBUG_REGALLOC
            printf("\n");
            printf("# ===========================\n");
            printf("# ");
            tb_print_dumb_node(NULL, n);
            printf("\n");
            #endif

            // expire intervals for node
            if (n->type != TB_PHI) {
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

                    int last_use = last_use_in_bb(scheduled, ra, bb, in);
                    if (set_get(&bb->live_out, in->gvn)) {
                        // it's live out so it flows to the end of the block
                        if (in->type == TB_PHI) {
                            TB_Node* move = phi_move_in_block(ctx->f->scheduled, bb, in);

                            // once we've past the move, we can't *really* refer to the phi so it's paused
                            if (move && last_use <= def_t) {
                                // liveness hole within cur
                                TB_OPTDEBUG(REGALLOC)(printf("#   pause  V%-4d (for block)\n", vreg_id));
                                set_remove(&ra->live_out, in->gvn);
                                set_remove(&ra->active, vreg_id);
                                set_put(&ra->future_active, vreg_id);
                            }
                        }
                        continue;
                    }

                    set_remove(&ra->live_out, in->gvn);

                    bool pause = false;
                    if (last_use > def_t) {
                        pause = true;
                    } else {
                        FOR_N(k, bb->id, ctx->bb_count) {
                            MachineBB* other = &ctx->machine_bbs[k];
                            if (set_get(&other->bb->live_in, in->gvn)) {
                                // move to future active
                                pause = true;
                                break;
                            }
                        }
                    }

                    if (pause) {
                        TB_OPTDEBUG(REGALLOC)(printf("#   pause  V%-4d\n", vreg_id));
                        set_put(&ra->future_active, vreg_id);
                    } else {
                        TB_OPTDEBUG(REGALLOC)(printf("#   expire V%-4d\n", vreg_id));
                    }
                    set_remove(&ra->active, vreg_id);
                }
            }

            // tmps only interfere with the live_out (which is a subset of the active set)
            Tmps* tmps = nl_table_get(&ctx->tmps_map, n);
            FOR_N(k, 0, ctx->num_classes) { ra->in_use[k] = 0; }
            if (tmps != NULL) {
                dyn_array_clear(ra->spills);

                // compute in_use for all classes but also only if they interfere with the def site (not the def's
                // entire lifetime), this is a subset of the active set.
                TB_OPTDEBUG(REGALLOC)(printf("#   "));
                FOR_N(i, 0, (aarray_length(ctx->vregs) + 63) / 64) {
                    uint64_t bits = ra->active.data[i] | ra->future_active.data[i];
                    size_t j = i*64;
                    while (bits) {
                        VReg* other = &ctx->vregs[j];
                        if ((bits & 1) && other->class > 0 && interfere_with_def(scheduled, ra, bb, n, other->n)) {
                            TB_OPTDEBUG(REGALLOC)(printf("V%zu (%%%u) interferes as ", j, other->n->gvn), print_reg_name(other->class, other->assigned), printf("; "));
                            ra->in_use[other->class] |= (1ull << other->assigned);
                            dyn_array_put(ra->spills, j);
                        }
                        bits >>= 1, j += 1;
                    }
                }
                TB_OPTDEBUG(REGALLOC)(printf("\n"));

                // temporaries interfere with each other (although in practice, they'll be fixed
                // so it doesn't matter)
                FOR_N(k, 0, tmps->count) {
                    VReg* tmp_vreg    = &ctx->vregs[tmps->elems[k]];
                    RegMask* tmp_mask = tmp_vreg->mask;

                    TB_OPTDEBUG(REGALLOC)(printf("#   TMP V%d ", tmps->elems[k]), tb__print_regmask(tmp_mask), printf("\n"));

                    uint64_t in_use = ra->in_use[tmp_mask->class] | ~tmp_mask->mask[0];
                    if (in_use == UINT64_MAX) {
                        choose_decent_spill(ctx, ra, NULL, tmp_mask->class, tmp_mask->mask[0]);
                        return false;
                    }

                    TB_OPTDEBUG(REGALLOC)(printf("#     available: %#"PRIx64"\n", ~in_use));
                    tmp_vreg->class    = tmp_mask->class;
                    tmp_vreg->assigned = tb_ffs64(~in_use) - 1;
                    ra->in_use[tmp_mask->class] |= (1ull << tmp_vreg->assigned);
                    TB_OPTDEBUG(REGALLOC)(printf("#     assigned to "), print_reg_name(tmp_vreg->class, tmp_vreg->assigned), printf("\n"));
                }

                // temporaries don't interfere with the output tho
                FOR_N(k, 0, ctx->num_classes) { ra->in_use[k] = 0; }
            }

            // allocate free register
            int vreg_id = ctx->vreg_map[n->gvn];
            if (vreg_id > 0) {
                int class = ctx->vregs[vreg_id].mask->class;
                if (!allocate_reg(ctx, ra, vreg_id, ra->in_use[class])) {
                    RegMask* mask = ctx->vregs[vreg_id].mask;
                    choose_decent_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);
                    return false;
                }

                TB_Node* def = ctx->vregs[vreg_id].n;
                set_put(&ra->live_out, def->gvn);
            } else if (n->dt.type == TB_TAG_TUPLE) {
                // allocate projections
                FOR_USERS(u, n) if (is_proj(USERN(u))) {
                    TB_Node* un = USERN(u);
                    vreg_id = ctx->vreg_map[un->gvn];
                    if (vreg_id > 0) {
                        int class = ctx->vregs[vreg_id].mask->class;
                        if (!allocate_reg(ctx, ra, vreg_id, ra->in_use[class])) {
                            RegMask* mask = ctx->vregs[vreg_id].mask;
                            choose_decent_spill(ctx, ra, &ctx->vregs[vreg_id], mask->class, mask->mask[0]);
                            return false;
                        }

                        set_put(&ra->live_out, un->gvn);
                    }
                }
            }
        }
    }
    return true;
}

