// See codegen.h for more details, this is the implementation file for it, each target
// will include this to define their own copy of the codegen.
#include "codegen.h"

#define COMMENT(...) (e->has_comments ? tb_emit_comment(e, &ctx->f->tmp_arena, __VA_ARGS__) : (void)0)

// Instruction selection:
//   returns an equivalent but machine-friendly node (one you're willing to
//   use during RA & emit).
static TB_Node* node_isel(Ctx* restrict ctx, TB_Function* f, TB_Node* n);

// RA constraints:
//   TODO
static RegMask* node_constraint(Ctx* restrict ctx, TB_Node* n, RegMask** ins);
//   these are input edges to a node which do not have a register bound, they
//   can be used to define clobbers or scratch depending on if they're later
//   defined as fixed RegMasks or not.
static int node_tmp_count(Ctx* restrict ctx, TB_Node* n);
//   when we represent 2addr ops in the SSA, we define which edge is potentially
//   shared such that lifetimes don't freak out about the fact that there's a
//   hypothetical move before the op (which means that it'll interfere with its
//   own inputs).
//
//   return -1 if it's not a 2addr and anything else to represent which edge
//   doesn't alias the dst.
static int node_2addr(TB_Node* n);
//   returns true if it's possible to rematerialize rather than spill this node.
static bool node_remat(TB_Node* n);

// Code emit:
//   finally write bytes, this is done post-RA so you're expected to use the VReg data
//   to know which regs you were assigned.
static void bundle_emit(Ctx* restrict ctx, TB_CGEmitter* e, Bundle* bundle);
//   this is where I recommend emitting prologue bytes.
static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n);
//   this is called AFTER the emitting of epilogues, those are emitted as normal
//   nodes. An example use of mine is NOP padding, idk do whatever, emitting is done.
static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e);
//   called at the start of each BB, it's mostly for bookkeeping about where labels
//   are placed.
static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb);
//   these two sequential nodes might fit into the same bundle (given they're input-independent)
static bool fits_as_bundle(Ctx* restrict ctx, TB_Node* a, TB_Node* b);

// Scheduling bits:
//   simple latency until the results of a node are useful (list scheduler will
//   generally prioritize dispatching higher latency ops first).
static int node_latency(TB_Function* f, TB_Node* n, TB_Node* end);
//   on VLIWs it's important that we keep track of which functional units a specific
//   node can even run on, use the bits to represent that (at most you can make 64
//   functional units in the current design but i don't think i need more than 10 rn)
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n);

static void init_ctx(Ctx* restrict ctx, TB_ABI abi);
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end);

// Helpers
static void dump_sched(Ctx* restrict ctx) {
    FOR_N(i, 0, ctx->bb_count) {
        TB_BasicBlock* bb = &ctx->cfg.blocks[i];
        printf("BB %zu:\n", i);
        aarray_for(i, bb->items) {
            printf("  ");
            tb_print_dumb_node(NULL, bb->items[i]);
            printf("\n");
        }
    }
}

static void flush_bundle(Ctx* restrict ctx, TB_CGEmitter* restrict e, Bundle* b) {
    #if TB_OPTDEBUG_CODEGEN
    printf("  ========= BUNDLE (%d) =========\n", b->count);
    FOR_N(i, 0, b->count) {
        TB_Node* n = b->arr[i];
        int def_id = ctx->vreg_map[n->gvn];
        VReg* vreg = def_id > 0 ? &ctx->vregs[def_id] : NULL;

        printf("  "), tb_print_dumb_node(NULL, n), printf("\n");

        if (vreg) {
            printf("    OUT    = %s:R%d \x1b[32m# VREG=%d\x1b[0m\n", reg_class_name(vreg->class), vreg->assigned, def_id);
        }

        FOR_N(j, 1, n->input_count) {
            TB_Node* in = n->inputs[j];
            int in_id = in ? ctx->vreg_map[in->gvn] : 0;
            if (in_id > 0) {
                VReg* other = &ctx->vregs[in_id];
                printf("    IN[%zu]  = %s:R%d\n", j, reg_class_name(other->class), other->assigned);
            }
        }

        Tmps* tmps = nl_table_get(&ctx->tmps_map, n);
        if (tmps) {
            FOR_N(j, 0, tmps->count) {
                VReg* other = &ctx->vregs[tmps->elems[j]];
                printf("    TMP[%zu] = %s:R%d\n", j, reg_class_name(other->class), other->assigned);
            }
        }
    }
    #endif
    bundle_emit(ctx, e, b);
    b->count = 0;
}

static void log_phase_end(TB_Function* f, size_t og_size, const char* label) {
    log_debug("%s: tmp_arena=%.1f KiB, ir_arena=%.1f KiB (post %s)", f->super.name, tb_arena_current_size(&f->tmp_arena) / 1024.0f, (tb_arena_current_size(&f->arena) - og_size) / 1024.0f, label);
}

static void compile_function(TB_Function* restrict f, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, TB_Arena* code_arena, bool emit_asm) {
    cuikperf_region_start("compile", f->super.name);
    TB_OPTDEBUG(CODEGEN)(tb_print_dumb(f));

    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    #ifndef NDEBUG
    f->arena.allocs = f->arena.alloc_bytes = 0;
    f->tmp_arena.allocs = f->tmp_arena.alloc_bytes = 0;
    #endif

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .tmp_count   = node_tmp_count,
        .constraint  = node_constraint,
        .node_2addr  = node_2addr,
        .remat       = node_remat,
        .num_classes = REG_CLASS_COUNT,
        .emit = {
            .output = func_out,
            .arena = &f->tmp_arena,
            .has_comments = true,
        }
    };

    if (features == NULL) {
        ctx.features = (TB_FeatureSet){ 0 };
    } else {
        ctx.features = *features;
    }

    init_ctx(&ctx, f->super.module->target_abi);
    TB_Worklist* restrict ws = f->worklist;

    size_t og_size = tb_arena_current_size(&f->arena);

    ctx.mask_intern = nl_hashset_alloc(200);
    nl_hashset_put2(&ctx.mask_intern, &TB_REG_EMPTY, rm_hash, rm_compare);

    CUIK_TIMED_BLOCK("isel") {
        log_debug("%s: tmp_arena=%.1f KiB (pre-isel)", f->super.name, tb_arena_current_size(&f->tmp_arena) / 1024.0f);

        // pointer math around stack slots will refer to this
        ctx.frame_ptr = tb_alloc_node(f, TB_MACH_FRAME_PTR, TB_TYPE_PTR, 1, 0);
        set_input(f, ctx.frame_ptr, f->root_node, 0);
        ctx.frame_ptr = tb_opt_gvn_node(f, ctx.frame_ptr);

        TB_ArenaSavepoint pins_sp = tb_arena_save(&f->tmp_arena);
        ArenaArray(TB_Node*) pins = aarray_create(&f->tmp_arena, TB_Node*, (f->node_count / 32) + 16);

        TB_Worklist walker_ws = { 0 };
        worklist_alloc(&walker_ws, f->node_count);

        ctx.walker_ws = &walker_ws;

        // find all nodes
        worklist_clear(ws);
        worklist_push(ws, f->root_node);
        for (size_t i = 0; i < dyn_array_length(ws->items); i++) {
            TB_Node* n = ws->items[i];
            if (tb_node_is_pinned(n) && !is_proj(n)) {
                aarray_push(pins, n);
            }

            FOR_USERS(u, n) { worklist_push(ws, USERN(u)); }
        }

        // greedy instruction selector does bottom-up rewrites from the pinned nodes
        worklist_clear(ws);
        aarray_for(i, pins) {
            TB_Node* pin_n = pins[i];

            TB_OPTDEBUG(ISEL)(printf("PIN    t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, pin_n), printf("\n"));
            worklist_push(&walker_ws, pin_n);

            while (dyn_array_length(walker_ws.items) > 0) {
                TB_Node* n = dyn_array_pop(walker_ws.items);
                TB_OPTDEBUG(ISEL)(printf("  ISEL t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n));

                if (!is_proj(n) && n->user_count == 0) {
                    TB_OPTDEBUG(ISEL)(printf(" => \x1b[31mKILL\x1b[0m\n"));
                    worklist_push(ws, n);
                    continue;
                }

                // memory out nodes are *notorious* for having unused loads, we wanna prune these
                if (n->dt.type == TB_TAG_MEMORY) {
                    FOR_USERS(u, n) {
                        if (USERN(u)->user_count == 0) {
                            worklist_push(ws, USERN(u));
                        }
                    }
                }

                tb__gvn_remove(f, n);

                // replace with machine op
                TB_Node* k = node_isel(&ctx, f, n);
                if (k && k != n) {
                    // we can GVN machine nodes :)
                    k = tb_opt_gvn_node(f, k);

                    TB_OPTDEBUG(ISEL)(printf(" => \x1b[32m"), tb_print_dumb_node(NULL, k), printf("\x1b[0m"));

                    subsume_node(f, n, k);

                    // don't walk the replacement
                    worklist_test_n_set(&walker_ws, k);
                    n = k;
                }
                TB_OPTDEBUG(ISEL)(printf("\n"));

                // replace all input edges
                FOR_REV_N(i, 0, n->input_count) if (n->inputs[i]) {
                    worklist_push(&walker_ws, n->inputs[i]);
                }

                // mark phis if we're on a region
                if (cfg_is_region(n)) {
                    FOR_USERS(u, n) if (USERN(u)->type == TB_PHI) {
                        worklist_push(&walker_ws, USERN(u));
                    }
                }
            }
        }
        worklist_free(&walker_ws);

        if (ctx.frame_ptr->user_count == 0) {
            tb_kill_node(f, ctx.frame_ptr);
            ctx.frame_ptr = NULL;
        }

        // dead node elim
        CUIK_TIMED_BLOCK("dead node elim") {
            for (TB_Node* n; n = worklist_pop(ws), n;) {
                if (n->user_count == 0 && !is_proj(n)) {
                    TB_OPTDEBUG(ISEL)(printf("  ISEL t=%d? ", ++f->stats.time), tb_print_dumb_node(NULL, n), printf(" => \x1b[31mKILL\x1b[0m\n"));
                    tb_kill_node(f, n);
                }
            }
        }

        tb_arena_restore(&f->tmp_arena, pins_sp);
        log_phase_end(f, og_size, "isel");
    }

    TB_CFG cfg;
    CUIK_TIMED_BLOCK("global sched") {
        // we're gonna build a bunch of compact tables... they're only
        // compact if we didn't spend like 40% of our value numbers on dead shit.
        #if !TB_OPTDEBUG_ISEL
        tb_renumber_nodes(f, ws);
        #endif

        TB_OPTDEBUG(CODEGEN)(tb_print_dumb(f));
        // TB_OPTDEBUG(CODEGEN)(tb_print(f));

        ctx.cfg = cfg = tb_compute_cfg(f, ws, &f->tmp_arena, true);
        tb_compute_synthetic_loop_freq(f, &cfg);
        tb_global_schedule(f, ws, cfg, false, true, node_latency);

        log_phase_end(f, og_size, "GCM");
    }

    size_t bb_count = aarray_length(cfg.blocks);

    size_t node_count = f->node_count;
    size_t vreg_cap = 0;
    CUIK_TIMED_BLOCK("local schedule") {
        ctx.vreg_map = aarray_create(&f->arena, int, tb_next_pow2(f->node_count + 16));
        aarray_set_length(ctx.vreg_map, f->node_count);

        FOR_N(i, 0, f->node_count) { ctx.vreg_map[i] = 0; }

        int max_ins = 0;
        size_t vreg_count = 1; // 0 is reserved as the NULL vreg
        TB_ASSERT(dyn_array_length(ws->items) == 0);
        FOR_N(i, 0, bb_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];

            // compute local schedule
            CUIK_TIMED_BLOCK("local sched") {
                // tb_greedy_scheduler(f, &cfg, ws, NULL, bb);
                tb_list_scheduler(f, &cfg, ws, NULL, bb, node_latency, node_unit_mask, FUNCTIONAL_UNIT_COUNT);
            }

            // a bit of slack for spills
            size_t item_count = dyn_array_length(ws->items);
            ArenaArray(TB_Node*) items = aarray_create(&f->arena, TB_Node*, item_count + 16);
            aarray_set_length(items, item_count);

            // copy out sched
            FOR_N(i, 0, item_count) {
                TB_Node* n = ws->items[i];
                items[i] = n;

                // if there's a def, let's make a vreg
                RegMask* def_mask = node_constraint(&ctx, n, NULL);

                int ins = n->input_count + node_tmp_count(&ctx, n);
                if (ins > max_ins) { max_ins = ins; }

                // these ops are guarenteed to fit since new nodes aren't being added and the
                // size was fine when we started.
                TB_ASSERT(n->gvn < aarray_length(ctx.vreg_map));

                int vreg_id = 0;
                if (def_mask != &TB_REG_EMPTY) {
                    if (n->type == TB_MACH_MOVE) {
                        TB_ASSERT(single_use(n));
                        TB_ASSERT(USERN(n->users)->type == TB_PHI);

                        // these are phi moves, they should share the vreg of phi
                        TB_Node* phi = USERN(n->users);
                        if (ctx.vreg_map[phi->gvn] == 0) {
                            ctx.vreg_map[phi->gvn] = vreg_id = vreg_count++;
                        } else {
                            vreg_id = ctx.vreg_map[phi->gvn];
                        }
                    } else if (n->type == TB_PHI && ctx.vreg_map[n->gvn] > 0) {
                        vreg_id = ctx.vreg_map[n->gvn];
                    } else {
                        vreg_id = vreg_count++;
                    }
                }
                ctx.vreg_map[n->gvn] = vreg_id;
            }
            dyn_array_clear(ws->items);
            bb->items = items;
        }
        ctx.bb_count = bb_count;
        ctx.ins = tb_arena_alloc(&f->tmp_arena, max_ins * sizeof(RegMask*));

        // ops with temporaries are *relatively* uncommon (mostly calls)
        ctx.tmps_map = nl_table_alloc((vreg_count / 16) + 4);

        log_phase_end(f, og_size, "local-sched & ra constraints");

        // setup for the next phase
        ctx.vregs = aarray_create(&f->arena, VReg, tb_next_pow2(vreg_count + 16));
        aarray_set_length(ctx.vregs, vreg_count);
    }

    CUIK_TIMED_BLOCK("gather RA constraints") {
        FOR_N(i, 0, bb_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            TB_OPTDEBUG(CODEGEN)(printf("BB %zu (freq=%.2f)\n", i, bb->freq));

            aarray_for(j, bb->items) {
                TB_Node* n = bb->items[j];
                int vreg_id = ctx.vreg_map[n->gvn];

                // all vreg writes here are in-bounds but later work will grow vregs
                // so don't be assuming it everywhere.
                TB_ASSERT(vreg_id >= 0 && vreg_id < aarray_length(ctx.vregs));

                #if TB_OPTDEBUG_CODEGEN
                int tmps = node_tmp_count(&ctx, n);
                RegMask* def_mask = node_constraint(&ctx, n, ctx.ins);

                printf("  "), tb_print_dumb_node(NULL, n), printf("\n");
                if (vreg_id > 0) {
                    printf("    OUT    = "), tb__print_regmask(def_mask), printf(" \x1b[32m# VREG=%d\x1b[0m\n", vreg_id);
                }

                FOR_N(j, 1, n->input_count) {
                    if (n->inputs[j] && ctx.ins[j] != &TB_REG_EMPTY) {
                        printf("    IN[%zu]  = ", j), tb__print_regmask(ctx.ins[j]), printf(" %%%d\n", n->inputs[j]->gvn);
                    }
                }

                FOR_N(j, n->input_count, n->input_count + tmps) {
                    printf("    TMP[%zu] = ", j), tb__print_regmask(ctx.ins[j]), printf("\n");
                }
                #endif

                if (vreg_id > 0 && n->type != TB_MACH_MOVE) {
                    RegMask* def_mask = node_constraint(&ctx, n, NULL);
                    ctx.vregs[vreg_id] = (VReg){ .n = n, .mask = def_mask, .assigned = -1, .spill_cost = NAN };
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("regalloc") {
        tb__rogers(&ctx, &f->tmp_arena);

        worklist_clear(ws);
        nl_hashset_free(ctx.mask_intern);

        log_phase_end(f, og_size, "RA");
    }

    int* final_order = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(int));
    CUIK_TIMED_BLOCK("BB scheduling") {
        // any empty projection blocks will mark which block they want you to jump to, if the
        // block "forwards" to itself then it's not actually forwarding.
        FOR_N(i, 0, bb_count) {
            TB_BasicBlock* bb = &cfg.blocks[i];
            if (aarray_length(bb->items) == 1 && cfg_is_cproj(bb->items[0])) {
                TB_Node* next = cfg_next_control(bb->items[0]);
                TB_BasicBlock* succ = nl_map_get_checked(cfg.node_to_block, next);
                bb->fwd = succ - cfg.blocks;
            } else {
                bb->fwd = i;

                // insert jump for the fallthru case, this matters most for the platforms which can
                // bundle jumps (branch delay slots and VLIWs)
                if (!cfg_is_terminator(bb->end)) {
                    TB_Node* jmp = tb_alloc_node(f, TB_MACH_JUMP, TB_TYPE_CONTROL, 1, 0);

                    TB_User* succ = cfg_next_user(bb->end);
                    set_input(f, USERN(succ), jmp, USERI(succ));
                    set_input(f, jmp, bb->end, 0);
                    bb->end = jmp;

                    aarray_push(bb->items, jmp);
                }
            }
        }

        bb_placement_rpo(&f->tmp_arena, &cfg, final_order);
    }

    CUIK_TIMED_BLOCK("emit") {
        // most functions are probably decently small, it's ok tho if it needs to
        // resize it can do that pretty quickly
        ctx.emit.capacity = 1024;
        ctx.emit.data = tb_arena_alloc(code_arena, ctx.emit.capacity);

        // allocate more stuff now that we've run stats on the IR
        ctx.emit.label_count = bb_count;
        ctx.emit.labels = tb_arena_alloc(&f->tmp_arena, bb_count * sizeof(uint32_t));
        memset(ctx.emit.labels, 0, bb_count * sizeof(uint32_t));

        TB_CGEmitter* e = &ctx.emit;
        pre_emit(&ctx, e, f->root_node);

        Bundle bundle;
        bundle.count = 0;
        bundle.arr = tb_arena_alloc(&f->tmp_arena, BUNDLE_INST_MAX * sizeof(Bundle));

        FOR_N(i, 0, bb_count) {
            int id = final_order[i];
            TB_BasicBlock* bb = &cfg.blocks[id];

            on_basic_block(&ctx, e, id);

            // block is empty and every use is jump-threaded
            if (id != bb->fwd) { continue; }

            ctx.fallthrough = INT_MAX;
            for (int j = i + 1; j < bb_count; j++) {
                int next = final_order[j];
                if (cfg.blocks[next].fwd == next) {
                    ctx.fallthrough = next;
                    break;
                }
            }
            ctx.current_emit_bb = bb;
            ctx.current_emit_bb_pos = GET_CODE_POS(e);

            // mark label
            TB_OPTDEBUG(CODEGEN)(printf("BB %d\n", id));

            TB_Node* prev_n = NULL;
            aarray_for(i, bb->items) {
                TB_Node* n = bb->items[i];

                // TODO(NeGate): we should be checking if the bundle can support the resources we're asking for.
                // an example is that one bundle might only be able to do 2 memory operations so "fits_as_bundle"
                // would return true for two stores but trying to stretch things to 3 would force a split.
                bool legal = false;
                if (bundle.count > 0 && bundle.count < BUNDLE_INST_MAX) {
                    if (i > 1 && fits_as_bundle(&ctx, bundle.arr[bundle.count - 1], n)) {
                        legal = true;

                        // if n refers to prev then we're dependent, if not then we can't
                        // be, just because there's nothing between them that could make
                        // the connection more indirect.
                        TB_Node* prev = bundle.arr[bundle.count - 1];
                        FOR_N(i, 0, n->input_count) {
                            if (n->inputs[i] == prev) {
                                legal = false;
                                break;
                            }
                        }
                    }
                }

                // flush bundle
                if (!legal && bundle.count > 0) {
                    flush_bundle(&ctx, e, &bundle);
                }

                bundle.arr[bundle.count++] = n;
            }

            // TODO(NeGate): the bundle might want to join with the terminator goto
            if (bundle.count > 0) {
                flush_bundle(&ctx, e, &bundle);
            }
        }

        post_emit(&ctx, e);
        log_phase_end(f, og_size, "emit");

        // Fill jump table entries
        CUIK_TIMED_BLOCK("jump tables") {
            dyn_array_for(i, ctx.jump_table_patches) {
                uint32_t target = ctx.emit.labels[ctx.jump_table_patches[i].target];
                TB_ASSERT((target & 0x80000000) && "target label wasn't resolved... what?");
                *ctx.jump_table_patches[i].pos = target & ~0x80000000;
            }
        }

        nl_table_free(ctx.tmps_map);
    }

    if (ctx.locations) {
        ctx.locations[0].pos = 0;
    }

    // trim code arena (it fits in a single chunk so just arena free the top)
    code_arena->top->avail = (char*) &ctx.emit.data[ctx.emit.count];
    tb_arena_realign(code_arena);

    // TODO(NeGate): move the assembly output to code arena
    if (emit_asm) CUIK_TIMED_BLOCK("dissassembly") {
        dyn_array_for(i, ctx.debug_stack_slots) {
            TB_StackSlot* s = &ctx.debug_stack_slots[i];
            EMITA(&ctx.emit, "// %s = [rsp + %d]\n", s->name, s->storage.offset);
        }
        EMITA(&ctx.emit, "%s:\n", f->super.name);

        Disasm d = {
            func_out->first_patch,
            ctx.locations,
            &ctx.locations[dyn_array_length(ctx.locations)],
            ctx.emit.comment_head,
        };

        if (ctx.prologue_length) {
            disassemble(&ctx.emit, &d, -1, 0, ctx.prologue_length);
        }

        FOR_N(i, 0, bb_count) {
            int id = final_order[i];
            if (id != cfg.blocks[id].fwd) { continue; }

            uint32_t start = ctx.emit.labels[id] & ~0x80000000;
            uint32_t end   = ctx.emit.count;
            if (i + 1 < bb_count) {
                end = ctx.emit.labels[final_order[i + 1]] & ~0x80000000;
            }

            TB_CGEmitter* e = &ctx.emit;
            {
                tb_asm_print(e, ".bb%d:", id);
                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[32m"));
                tb_asm_print(e, " # Freq: %f", cfg.blocks[id].freq);
                TB_OPTDEBUG(ANSI)(tb_asm_print(e, "\x1b[0m"));
                tb_asm_print(e, "\n");
            }
            disassemble(e, &d, id, start, end);
        }
    }

    // cleanup memory
    tb_free_cfg(&cfg);
    cuikperf_region_end();

    #ifndef NDEBUG
    log_debug("%s: total allocs on ir_arena=%.1f KiB", f->super.name, f->arena.alloc_bytes / 1024.0f);
    log_debug("%s: total allocs on tmp_arena=%.1f KiB", f->super.name, f->tmp_arena.alloc_bytes / 1024.0f);
    log_debug("%s: code_arena=%.1f KiB", f->super.name, tb_arena_current_size(code_arena) / 1024.0f);
    #endif

    tb_arena_clear(&f->tmp_arena);

    f->scheduled_n = 0;
    f->scheduled = NULL;

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->locations = ctx.locations;
    func_out->stack_slots = ctx.debug_stack_slots;
    func_out->stack_usage = ctx.stack_usage;
    func_out->prologue_length = ctx.prologue_length;
    func_out->epilogue_length = ctx.epilogue_length;
    func_out->nop_pads = ctx.nop_pads;
}
