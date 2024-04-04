// See codegen.h for more details, this is the implementation file for it, each target
// will include this to define their own copy of the codegen.
#include "codegen.h"

#define COMMENT(...) (e->has_comments ? tb_emit_comment(e, ctx->f->tmp_arena, __VA_ARGS__) : (void)0)

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
//   returns true if it clobbers the FLAGS register.
static bool node_flags(Ctx* ctx, TB_Node* n);
//   when we represent 2addr ops in the SSA, we define which edge is potentially
//   shared such that lifetimes don't freak out about the fact that there's a
//   hypothetical move before the op (which means that it'll interfere with its
//   own inputs).
//
//   return -1 if it's not a 2addr and anything else to represent which edge
//   doesn't alias the dst.
static int node_2addr(TB_Node* n);

// Code emit:
//   finally write bytes, this is done post-RA so you're expected to use the VReg data
//   to know which regs you were assigned.
static void node_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n, VReg* vreg);
//   i never actually add "goto" nodes to the graph, they're just implied and this is
//   the function responsible for that.
static void emit_goto(Ctx* ctx, TB_CGEmitter* e, MachineBB* succ);
//   this is where I recommend emitting prologue bytes.
static void pre_emit(Ctx* restrict ctx, TB_CGEmitter* e, TB_Node* n);
//   this is called AFTER the emitting of epilogues, those are emitted as normal
//   nodes. An example use of mine is NOP padding, idk do whatever, emitting is done.
static void post_emit(Ctx* restrict ctx, TB_CGEmitter* e);
//   called at the start of each BB, it's mostly for bookkeeping about where labels
//   are placed.
static void on_basic_block(Ctx* restrict ctx, TB_CGEmitter* e, int bb);

// Scheduling bits:
//   simple latency until the results of a node are useful (list scheduler will
//   generally prioritize dispatching higher latency ops first).
static int node_latency(TB_Function* f, TB_Node* n);
//   on VLIWs it's important that we keep track of which functional units a specific
//   node can even run on, use the bits to represent that (at most you can make 64
//   functional units in the current design but i don't think i need more than 10 rn)
static uint64_t node_unit_mask(TB_Function* f, TB_Node* n);

static void init_ctx(Ctx* restrict ctx, TB_ABI abi);
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end);

static const char* reg_class_name(int class) {
    switch (class) {
        case 0: return "STK";
        case 1: return "FLAGS";
        case 2: return "GPR";
        case 3: return "XMM";
        default: return NULL;
    }
}

void tb__print_regmask(RegMask* mask) {
    assert(mask->count == 1 && "TODO");
    if (mask->class == REG_CLASS_STK) {
        if (mask->mask[0] == 0) {
            printf("[any spill]");
        } else {
            printf("[SP + %"PRId64"]", mask->mask[0]*8);
        }
    } else if (mask->class == REG_CLASS_FLAGS) {
        if (mask->mask[0] == 0) {
            printf("[SPILL]");
        } else {
            printf("[FLAGS]");
        }
    } else {
        int i = 0;
        bool comma = false;
        uint64_t bits = mask->mask[0];

        printf("[%s:", reg_class_name(mask->class));
        while (bits) {
            // skip zeros
            int skip = __builtin_ffs(bits) - 1;
            i += skip, bits >>= skip;

            if (!comma) {
                comma = true;
            } else {
                printf(", ");
            }

            // find sequence of ones
            int len = __builtin_ffs(~bits) - 1;
            printf("R%d", i);
            if (len > 1) {
                printf(" .. R%d", i+len-1);
            }

            // skip ones
            bits >>= len, i += len;
        }

        if (mask->may_spill) {
            printf(" | SPILL");
        }
        printf("]");
    }
}

static void log_phase_end(TB_Function* f, size_t og_size, const char* label) {
    log_debug("%s: tmp_arena=%.1f KiB, ir_arena=%.1f KiB (post %s)", f->super.name, tb_arena_current_size(f->tmp_arena) / 1024.0f, (tb_arena_current_size(f->arena) - og_size) / 1024.0f, label);
}

static void compile_function(TB_Function* restrict f, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, TB_Arena* code_arena, bool emit_asm) {
    TB_OPTDEBUG(CODEGEN)(tb_print(f, f->tmp_arena));

    TB_Arena* arena = f->tmp_arena;
    TB_ArenaSavepoint sp = tb_arena_save(arena);

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .tmp_count   = node_tmp_count,
        .constraint  = node_constraint,
        .node_2addr  = node_2addr,
        .flags       = node_flags,
        .num_classes = REG_CLASS_COUNT,
        .emit = {
            .output = func_out,
            .arena = arena,
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

    // legalize step takes out any of our 16bit and 8bit math ops
    // tb_pass_legalize(p, f->super.module->target_arch);
    size_t og_size = tb_arena_current_size(f->arena);

    ctx.mask_intern = nl_hashset_alloc(200);
    nl_hashset_put2(&ctx.mask_intern, &TB_REG_EMPTY, rm_hash, rm_compare);

    CUIK_TIMED_BLOCK("isel") {
        log_debug("%s: tmp_arena=%.1f KiB (pre-isel)", f->super.name, tb_arena_current_size(arena) / 1024.0f);

        TB_Worklist walker_ws = { 0 };
        worklist_alloc(&walker_ws, f->node_count);
        worklist_clear(ws);

        // pointer math around stack slots will refer to this
        ctx.frame_ptr = tb_alloc_node(f, TB_MACH_FRAME_PTR, TB_TYPE_PTR, 1, 0);
        set_input(f, ctx.frame_ptr, f->root_node, 0);

        // bottom-up rewrite:
        //   we keep the visited bits set once we've rewritten a node, unlike most worklist usage
        //   which unsets a bit once it's popped from the items array.
        CUIK_TIMED_BLOCK("rewriting") {
            worklist_push(&walker_ws, f->root_node);

            while (dyn_array_length(walker_ws.items) > 0) {
                TB_Node* n = dyn_array_pop(walker_ws.items);

                // replace with machine op
                TB_Node* k = node_isel(&ctx, f, n);
                if (k && k != n) {
                    subsume_node(f, n, k);

                    // we could run GVN on machine ops :)
                    k = tb_opt_gvn_node(f, k);

                    // don't walk the replacement
                    worklist_test_n_set(&walker_ws, k);
                    n = k;
                }

                // replace all input edges
                FOR_REV_N(i, 0, n->input_count) if (n->inputs[i]) {
                    worklist_push(&walker_ws, n->inputs[i]);
                }
            }
            worklist_free(&walker_ws);
        }

        if (ctx.frame_ptr->users == NULL) {
            tb_kill_node(f, ctx.frame_ptr);
        }

        // dead node elim
        CUIK_TIMED_BLOCK("dead node elim") {
            for (TB_Node* n; n = worklist_pop(ws), n;) {
                if (n->user_count == 0) { tb_kill_node(f, n); }
            }
        }

        log_phase_end(f, og_size, "isel");
    }

    TB_CFG cfg;
    CUIK_TIMED_BLOCK("global sched") {
        // we're gonna build a bunch of compact tables... they're only
        // compact if we didn't spend like 40% of our value numbers on dead shit.
        tb_renumber_nodes(f, ws);

        TB_OPTDEBUG(CODEGEN)(tb_print_dumb(f, false));
        TB_OPTDEBUG(CODEGEN)(tb_print(f, arena));

        cfg = tb_compute_rpo(f, ws);
        tb_global_schedule(f, ws, cfg, true, node_latency);

        log_phase_end(f, og_size, "GCM");
    }

    int bb_count = 0;
    MachineBB* restrict machine_bbs = tb_arena_alloc(arena, cfg.block_count * sizeof(MachineBB));
    TB_Node** bbs = ws->items;

    int stop_bb = -1;
    CUIK_TIMED_BLOCK("BB scheduling") {
        size_t cap = ((cfg.block_count * 4) / 3);
        ctx.node_to_bb.exp = 64 - __builtin_clzll((cap < 4 ? 4 : cap) - 1);
        ctx.node_to_bb.entries = tb_arena_alloc(arena, (1u << ctx.node_to_bb.exp) * sizeof(NodeToBB));
        memset(ctx.node_to_bb.entries, 0, (1u << ctx.node_to_bb.exp) * sizeof(NodeToBB));

        // define all PHIs early and sort BB order
        FOR_N(i, 0, cfg.block_count) {
            TB_Node* end = nl_map_get_checked(cfg.node_to_block, bbs[i]).end;
            if (end->type == TB_RETURN) {
                stop_bb = i;
            } else {
                machine_bbs[bb_count++] = (MachineBB){ i };
            }
        }

        // enter END block at the... end
        if (stop_bb >= 0) {
            machine_bbs[bb_count++] = (MachineBB){ stop_bb };
        }

        log_phase_end(f, og_size, "BB-sched");
    }

    size_t node_count = f->node_count;
    size_t vreg_cap = 0;
    CUIK_TIMED_BLOCK("local schedule") {
        // zero out the root & callgraph node
        ctx.vreg_map = aarray_create(arena, int, tb_next_pow2(f->node_count + 16));
        aarray_set_length(ctx.vreg_map, f->node_count);

        FOR_N(i, 0, f->node_count) { ctx.vreg_map[i] = 0; }
        ctx.vreg_map[0] = 0;
        ctx.vreg_map[f->root_node->inputs[0]->gvn] = 0;

        int max_ins = 0;
        size_t vreg_count = 1; // 0 is reserved as the NULL vreg
        assert(dyn_array_length(ws->items) == cfg.block_count);
        FOR_N(i, 0, bb_count) {
            int bbid = machine_bbs[i].id;
            TB_Node* bb_start = bbs[bbid];
            TB_BasicBlock* bb = f->scheduled[bb_start->gvn];

            bb->order = i;
            node_to_bb_put(&ctx, bb_start, &machine_bbs[i]);

            // compute local schedule
            size_t base = dyn_array_length(ws->items);

            // tb_greedy_scheduler(f, &cfg, ws, NULL, bb);
            CUIK_TIMED_BLOCK("local sched") {
                tb_list_scheduler(f, &cfg, ws, NULL, bb, node_latency, node_unit_mask, FUNCTIONAL_UNIT_COUNT);
            }

            // a bit of slack for spills
            size_t item_count = dyn_array_length(ws->items) - base;
            ArenaArray(TB_Node*) items = aarray_create(arena, TB_Node*, item_count + 16);
            aarray_set_length(items, item_count);

            // copy out sched
            FOR_N(i, 0, item_count) {
                TB_Node* n = ws->items[cfg.block_count + i];
                items[i] = n;

                // if there's a def, let's make a vreg
                RegMask* def_mask = node_constraint(&ctx, n, NULL);

                int ins = n->input_count + node_tmp_count(&ctx, n);
                if (ins > max_ins) { max_ins = ins; }

                // these ops are guarenteed to fit since new nodes aren't being added and the
                // size was fine when we started.
                assert(n->gvn < aarray_length(ctx.vreg_map));

                int vreg_id = 0;
                if (def_mask != &TB_REG_EMPTY) {
                    if (n->type == TB_MACH_MOVE) {
                        assert(single_use(n));
                        assert(USERN(n->users)->type == TB_PHI);

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

            dyn_array_set_length(ws->items, base);

            machine_bbs[i].n = bb_start;
            machine_bbs[i].end_n = bb->end;
            machine_bbs[i].items = items;
        }
        ctx.bb_count = bb_count;
        ctx.machine_bbs = machine_bbs;
        ctx.ins = tb_arena_alloc(arena, max_ins * sizeof(RegMask*));

        // ops with temporaries are *relatively* uncommon (mostly calls)
        ctx.tmps_map = nl_table_alloc((vreg_count / 16) + 4);

        log_phase_end(f, og_size, "local-sched & ra constraints");

        // setup for the next phase
        ctx.vregs = aarray_create(arena, VReg, tb_next_pow2(vreg_count + 16));
        aarray_set_length(ctx.vregs, vreg_count);
    }

    CUIK_TIMED_BLOCK("gather RA constraints") {
        FOR_N(i, 0, bb_count) {
            MachineBB* mbb = &ctx.machine_bbs[i];
            TB_OPTDEBUG(CODEGEN)(printf("BB %zu\n", i));

            aarray_for(j, mbb->items) {
                TB_Node* n = mbb->items[j];
                int vreg_id = ctx.vreg_map[n->gvn];

                // all vreg writes here are in-bounds but later work will grow vregs
                // so don't be assuming it everywhere.
                assert(vreg_id >= 0 && vreg_id < aarray_length(ctx.vregs));

                #if TB_OPTDEBUG_CODEGEN
                int tmps = node_tmp_count(&ctx, n);
                RegMask* def_mask = node_constraint(&ctx, n, ctx.ins);

                printf("  "), tb_print_dumb_node(NULL, n), printf("\n");
                if (vreg_id > 0) {
                    printf("    OUT    = "), tb__print_regmask(def_mask), printf(" \x1b[32m# VREG=%d\x1b[0m\n", vreg_id);
                }

                FOR_N(j, 1, n->input_count) {
                    if (n->inputs[j] && ctx.ins[j] != &TB_REG_EMPTY) {
                        printf("    IN[%zu]  = ", j), tb__print_regmask(ctx.ins[j]), printf(" v%d\n", n->inputs[j]->gvn);
                    }
                }

                FOR_N(j, n->input_count, n->input_count + tmps) {
                    printf("    TMP[%zu] = ", j), tb__print_regmask(ctx.ins[j]), printf("\n");
                }

                if (node_flags(&ctx, n)) { printf("    CLOBBER FLAGS\n"); }
                #endif

                if (vreg_id > 0 && n->type != TB_MACH_MOVE) {
                    RegMask* def_mask = node_constraint(&ctx, n, NULL);
                    ctx.vregs[vreg_id] = (VReg){ .n = n, .mask = def_mask, .assigned = -1 };
                }
            }
        }
    }

    CUIK_TIMED_BLOCK("regalloc") {
        // tb__chaitin(&ctx, arena);
        tb__lsra(&ctx, arena);
        nl_hashset_free(ctx.mask_intern);

        log_phase_end(f, og_size, "RA");
    }

    CUIK_TIMED_BLOCK("emit") {
        // allocate entire top of the code arena (we'll trim it later if possible)
        ctx.emit.capacity = code_arena->high_point - code_arena->watermark;
        ctx.emit.data = tb_arena_alloc(code_arena, ctx.emit.capacity);

        // allocate more stuff now that we've run stats on the IR
        ctx.emit.label_count = cfg.block_count;
        ctx.emit.labels = tb_arena_alloc(arena, cfg.block_count * sizeof(uint32_t));
        memset(ctx.emit.labels, 0, cfg.block_count * sizeof(uint32_t));

        TB_CGEmitter* e = &ctx.emit;
        pre_emit(&ctx, e, f->root_node);

        FOR_N(i, 0, bb_count) {
            MachineBB* mbb = &machine_bbs[i];
            int bbid = mbb->id;

            if (i + 1 < bb_count) {
                ctx.fallthrough = machine_bbs[i + 1].id;
            } else {
                ctx.fallthrough = INT_MAX;
            }
            ctx.current_emit_bb = mbb;
            ctx.current_emit_bb_pos = GET_CODE_POS(e);

            // line info is BB-local
            TB_NodeLocation* last_loc = NULL;

            // mark label
            on_basic_block(&ctx, e, bbid);
            TB_OPTDEBUG(CODEGEN)(printf("BB %d\n", bbid));

            aarray_for(i, mbb->items) {
                TB_Node* n = mbb->items[i];
                int def_id = ctx.vreg_map[n->gvn];
                VReg* vreg = def_id > 0 ? &ctx.vregs[def_id] : NULL;

                #if TB_OPTDEBUG_CODEGEN
                printf("  "), tb_print_dumb_node(NULL, n), printf("\n");

                if (vreg) {
                    printf("    OUT    = %s:R%d\n", reg_class_name(vreg->class), vreg->assigned);
                }

                FOR_N(j, 1, n->input_count) {
                    TB_Node* in = n->inputs[j];
                    int in_id = in ? ctx.vreg_map[in->gvn] : 0;
                    if (in_id > 0) {
                        VReg* vreg = &ctx.vregs[in_id];
                        printf("    IN[%zu]  = %s:R%d\n", j, reg_class_name(vreg->class), vreg->assigned);
                    }
                }

                Tmps* tmps = nl_table_get(&ctx.tmps_map, n);
                if (tmps) {
                    FOR_N(j, 0, tmps->count) {
                        VReg* vreg = &ctx.vregs[tmps->elems[j]];
                        printf("    TMP[%zu] = %s:R%d\n", j, reg_class_name(vreg->class), vreg->assigned);
                    }
                }
                #endif

                node_emit(&ctx, e, n, vreg);
            }

            if (!cfg_is_terminator(mbb->end_n)) {
                MachineBB* succ = node_to_bb(&ctx, cfg_next_control(mbb->end_n));
                emit_goto(&ctx, e, succ);
            }
        }

        post_emit(&ctx, e);
        log_phase_end(f, og_size, "emit");

        // Fill jump table entries
        CUIK_TIMED_BLOCK("jump tables") {
            dyn_array_for(i, ctx.jump_table_patches) {
                uint32_t target = ctx.emit.labels[ctx.jump_table_patches[i].target];
                assert((target & 0x80000000) && "target label wasn't resolved... what?");
                *ctx.jump_table_patches[i].pos = target & ~0x80000000;
            }
        }

        nl_table_free(ctx.tmps_map);
    }

    if (ctx.locations) {
        ctx.locations[0].pos = 0;
    }

    // trim code arena (it fits in a single chunk so just arena free the top)
    code_arena->watermark = (char*) &ctx.emit.data[ctx.emit.count];
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
            int bbid = machine_bbs[i].id;
            TB_Node* bb = bbs[bbid];

            uint32_t start = ctx.emit.labels[bbid] & ~0x80000000;
            uint32_t end   = ctx.emit.count;
            if (i + 1 < bb_count) {
                end = ctx.emit.labels[machine_bbs[i + 1].id] & ~0x80000000;
            }

            disassemble(&ctx.emit, &d, bbid, start, end);
        }
    }

    // cleanup memory
    tb_free_cfg(&cfg);

    log_debug("%s: code_arena=%.1f KiB", f->super.name, tb_arena_current_size(code_arena) / 1024.0f);
    tb_arena_restore(arena, sp);
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

static void get_data_type_size(TB_DataType dt, size_t* out_size, size_t* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // above 64bits we really dont care that much about natural alignment
            bool is_big_int = dt.data > 64;

            // round up bits to a byte
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data - 1);

            *out_size  = ((bits+7) / 8);
            *out_align = is_big_int ? 8 : ((dt.data + 7) / 8);
            break;
        }
        case TB_FLOAT32: *out_size = *out_align = 4; break;
        case TB_FLOAT64: *out_size = *out_align = 8; break;
        case TB_PTR:     *out_size = *out_align = 8; break;
        default: tb_unreachable();
    }
}
