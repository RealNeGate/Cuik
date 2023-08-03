#include "../passes.h"
#include "../codegen/emitter.h"
#include <inttypes.h>
#include <log.h>

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

enum {
    INST_LABEL = 1024,
    INST_BRANCH,
    INST_LINE,
};

typedef struct Inst Inst;

// the first set of indices are reserved for physical registers, the
// rest are allocated as virtual registers.
typedef int RegIndex;

typedef struct MachineBB {
    Inst* first;

    int start, end;

    // local live sets
    Set gen, kill;
    // global
    Set live_in, live_out;
} MachineBB;

typedef struct MachineReg {
    uint8_t class, num;
} MachineReg;

typedef struct {
    TB_Node* n;

    // if is usually -1 unless there's weird parallel copies
    int val, tmp;
} PhiVal;

typedef struct LiveInterval LiveInterval;
typedef NL_Map(TB_Node*, MachineBB) MachineBBs;

typedef struct {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_ABI target_abi;

    int caller_usage;
    TB_Node* fallthrough;
    TB_PostorderWalk order;

    TB_Passes* p;
    TB_Dominators doms;

    // Scheduling
    NL_HashSet visited;
    NL_Map(TB_Node*, int) uses;

    // Regalloc
    DynArray(LiveInterval) intervals;

    // temporary but still precious
    DynArray(PhiVal) phi_vals;

    // machine output sequences
    Inst *first, *last;
    MachineBBs machine_bbs;

    // Line info
    DynArray(TB_Line) lines;
    TB_FileID last_file;
    int last_line;

    // Stack
    uint32_t stack_usage;
    NL_Map(TB_Node*, int) stack_slots;
    DynArray(TB_StackSlot) debug_stack_slots;

    uint64_t regs_to_save;

    // current value table
    NL_Map(TB_Node*, RegIndex) values;

    TB_SafepointKey* safepoints;
} Ctx;

static bool empty_bb(TB_Node* n) {
    TB_Node* end = TB_NODE_GET_EXTRA_T(n, TB_NodeRegion)->end;
    return end->type == TB_RET && end->input_count == 1 && end->inputs[0] == n;
}

static bool fits_into_int8(uint64_t x) {
    int8_t y = x & 0xFF;
    return (int64_t)y == x;
}

static bool fits_into_int32(uint64_t x) {
    /*int64_t y = ((int32_t) x);
    uint32_t hi = y >> 32ull;
    return hi == 0 || hi == 0xFFFFFFFF;*/
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

static size_t emit_prologue(Ctx* restrict ctx);
static size_t emit_epilogue(Ctx* restrict ctx);
static ptrdiff_t alloc_free_reg(Ctx* restrict ctx, int reg_class);

static TB_X86_DataType legalize(TB_DataType dt);
static bool is_terminator_or_label(int type);
static bool wont_spill_around(int type);
static int classify_reg_class(TB_DataType dt);
static int isel(Ctx* restrict ctx, TB_Node* n);

static void finna_use_reg(Ctx* restrict ctx, int reg_class, int reg_num);
static void emit_code(Ctx* restrict ctx, TB_FunctionOutput* restrict func_out);

#define ISEL(n) isel(ctx, n)

static void add_debug_local(Ctx* restrict ctx, TB_Node* n, int pos) {
    // could be costly if you had more than like 2-3 attributes per stack slot... which you
    // wouldn't do right?
    for (TB_Attrib* a = n->first_attrib; a != NULL; a = a->next) {
        if (a->type == TB_ATTRIB_VARIABLE) {
            TB_StackSlot s = {
                .position = pos,
                .storage_type = a->var.storage,
                .name = a->var.name,
            };
            dyn_array_put(ctx->debug_stack_slots, s);
            break;
        }
    }
}

static const char* reg_name(int rg, int num) {
    return (rg == REG_CLASS_XMM ? XMM_NAMES : GPR_NAMES)[num];
}

////////////////////////////////
// Instructions
////////////////////////////////
typedef enum {
    INST_LOCK   = 1,
    INST_REP    = 2,
    INST_REPNE  = 4,

    // uses memory operand for storing
    INST_STORE  = 8,

    // operands
    INST_MEM    = 16,
    INST_GLOBAL = 32,  // operand to TB_Symbol*
    INST_NODE   = 64,  // operand to TB_Node*
    INST_ATTRIB = 128, // operand to TB_Attrib*
    INST_IMM    = 256, // operand in imm
    INST_ABS    = 512, // operand in abs

    // memory op
    INST_INDEXED = 1024,
} InstFlags;

struct Inst {
    Inst* next;

    // prefixes
    InstType type;
    InstFlags flags;

    TB_X86_DataType dt;
    int time;

    union {
        int32_t imm;
        uint64_t abs;
        TB_Symbol* s;
        TB_Node* n;
        TB_Attrib* a;
    };

    int32_t disp;

    uint8_t scale;
    uint8_t out_count, in_count, tmp_count;

    // operands all go after the instruction in memory.
    //
    //    RegIndex outs[out_count];
    //    RegIndex ins[in_count];
    //    RegIndex tmps[tmp_count];
    //
    RegIndex operands[];
};

// generic instructions
static Inst* inst_label(TB_Node* n) {
    Inst* i = TB_ARENA_ALLOC(&tb__arena, Inst);
    *i = (Inst){ .type = INST_LABEL, .flags = INST_NODE, .n = n };
    return i;
}

static Inst* inst_line(TB_Attrib* a) {
    Inst* i = TB_ARENA_ALLOC(&tb__arena, Inst);
    *i = (Inst){ .type = INST_LINE, .flags = INST_ATTRIB, .a = a };
    return i;
}

#define SUBMIT(i) append_inst(ctx, i)
static void append_inst(Ctx* restrict ctx, Inst* inst) {
    if (ctx->last == NULL) {
        ctx->first = ctx->last = inst;
    } else {
        ctx->last->next = inst;
        ctx->last = inst;
    }
}

static Inst* alloc_inst(int type, TB_DataType dt, int outs, int ins, int tmps) {
    int total = outs + ins + tmps;
    Inst* i = tb_arena_alloc(&tb__arena, sizeof(Inst) + (total * sizeof(RegIndex)));
    *i = (Inst){ .type = type, .dt = legalize(dt), .out_count = outs, ins, tmps };
    return i;
}

static Inst* inst_goto(TB_Node* target) {
    Inst* i = alloc_inst(INST_BRANCH, TB_TYPE_VOID, 0, 0, 0);
    i->flags = INST_NODE;
    i->n = target;
    return i;
}

static Inst* inst_move(TB_DataType dt, RegIndex dst, RegIndex src) {
    Inst* i = alloc_inst(MOV, dt, 1, 1, 0);
    i->operands[0] = dst;
    i->operands[1] = src;
    return i;
}

static Inst* inst_op_abs(int type, TB_DataType dt, RegIndex dst, uint64_t imm) {
    Inst* i = alloc_inst(type, dt, 1, 0, 0);
    i->flags = INST_ABS;
    i->operands[0] = dst;
    i->abs = imm;
    return i;
}

static Inst* inst_op_rm(int type, TB_DataType dt, RegIndex dst, RegIndex base, RegIndex index, Scale scale, int32_t disp) {
    Inst* i = alloc_inst(type, dt, 1, index >= 0 ? 2 : 1, 0);
    i->flags = INST_MEM | (index >= 0 ? INST_INDEXED : 0);
    i->operands[0] = dst;
    i->operands[1] = base;
    if (index >= 0) {
        i->operands[2] = index;
    }
    i->disp = disp;
    i->scale = scale;
    return i;
}

static Inst* inst_op_mr(int type, TB_DataType dt, RegIndex base, RegIndex index, Scale scale, int32_t disp, RegIndex src) {
    Inst* i = alloc_inst(type, dt, 1, index >= 0 ? 2 : 1, 0);
    i->flags = INST_MEM | (index >= 0 ? INST_INDEXED : 0) | INST_STORE;
    if (index >= 0) {
        i->operands[0] = base;
        i->operands[1] = index;
        i->operands[2] = src;
    } else {
        i->operands[0] = base;
        i->operands[1] = src;
    }
    i->disp = disp;
    i->scale = scale;
    return i;
}

static Inst* inst_op_ri(int type, TB_DataType dt, RegIndex dst, RegIndex src, int32_t imm) {
    Inst* i = alloc_inst(type, dt, 1, 1, 0);
    i->flags = INST_IMM;
    i->operands[0] = dst;
    i->operands[1] = src;
    i->imm = imm;
    return i;
}

static Inst* inst_op_rr(int type, TB_DataType dt, RegIndex dst, RegIndex lhs, RegIndex rhs) {
    Inst* i = alloc_inst(type, dt, 1, 2, 0);
    i->operands[0] = dst;
    i->operands[1] = lhs;
    i->operands[2] = rhs;
    return i;
}

static Inst* inst_op_imm(int type, TB_DataType dt, RegIndex dst, int32_t imm) {
    Inst* i = alloc_inst(type, dt, 1, 0, 0);
    i->flags = INST_IMM;
    i->operands[0] = dst;
    i->imm = imm;
    return i;
}

////////////////////////////////
// Register allocation
////////////////////////////////
#include "reg_alloc.h"
#include "fancy_reg_alloc.h"

#define DEF(n, dt) alloc_vreg(ctx, n, dt)
static int alloc_vreg(Ctx* restrict ctx, TB_Node* n, TB_DataType dt) {
    int i = dyn_array_length(ctx->intervals);
    dyn_array_put(ctx->intervals, (LiveInterval){ .reg_class = classify_reg_class(dt), .n = n, .reg = -1, .hint = -1, .assigned = -1, .dt = legalize(dt), .start = INT_MAX });
    return i;
}

static void hint_reg(Ctx* restrict ctx, int i, int phys_reg) {
    ctx->intervals[i].hint = phys_reg;
}

////////////////////////////////
// Data flow analysis
////////////////////////////////
static void liveness(Ctx* restrict ctx, TB_Function* f) {
    size_t interval_count = dyn_array_length(ctx->intervals);
    TB_Arena* arena = &tb__arena;

    // find BB boundaries in sequences
    MachineBBs seq_bb = NULL;
    nl_map_create(seq_bb, ctx->order.count);

    FOREACH_N(i, 0, ctx->order.count) {
        MachineBB bb = {
            .gen = set_create_in_arena(arena, interval_count), .kill = set_create_in_arena(arena, interval_count),
            .live_in = set_create_in_arena(arena, interval_count), .live_out = set_create_in_arena(arena, interval_count)
        };

        nl_map_put(seq_bb, ctx->order.traversal[i], bb);
    }

    // generate local live sets
    if (ctx->first) {
        Inst* restrict inst = ctx->first;
        assert(inst->type == INST_LABEL);

        // initial label
        MachineBB* mbb = &nl_map_get_checked(seq_bb, f->start_node);
        mbb->first = inst;
        mbb->start = 2;
        inst = inst->next;

        TB_Node* bb = f->start_node;
        int timeline = 2;
        for (; inst; inst = inst->next) {
            if (inst->type == INST_LABEL) {
                nl_map_get_checked(seq_bb, bb).end = timeline;
                timeline += 2; // reserved two extra spaces at the end of the BB

                tb_assert(inst->flags & INST_NODE, "label instruction has no TB_Node* for the region");
                bb = inst->n;
                mbb = &nl_map_get_checked(seq_bb, bb);
                mbb->first = inst->next;
                mbb->start = timeline;
            }

            Set* restrict gen = &mbb->gen;
            Set* restrict kill = &mbb->kill;

            inst->time = timeline;
            timeline += 2;

            RegIndex* ins = inst->operands + inst->out_count;
            FOREACH_N(i, 0, inst->in_count) {
                if (!set_get(kill, ins[i])) {
                    set_put(gen, ins[i]);
                }
            }

            RegIndex* outs = inst->operands;
            FOREACH_N(i, 0, inst->out_count) {
                set_put(kill, outs[i]);
            }
        }

        mbb->end = timeline;
    }

    // generate global live sets
    bool changes;
    do {
        changes = false;

        FOREACH_REVERSE_N(i, 0, ctx->order.count) {
            TB_Node* bb = ctx->order.traversal[i];
            TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
            MachineBB* mbb = &nl_map_get_checked(seq_bb, bb);

            set_clear(&mbb->live_out);

            // walk all successors
            FOREACH_N(i, 0, r->succ_count) {
                // union with successor's lives
                MachineBB* succ = &nl_map_get_checked(seq_bb, r->succ[i]);
                set_union(&mbb->live_out, &succ->live_in);
            }

            Set* restrict live_in = &mbb->live_in;
            Set* restrict live_out = &mbb->live_out;
            Set* restrict kill = &mbb->kill;
            Set* restrict gen = &mbb->gen;

            // live_in = (live_out - live_kill) U live_gen
            FOREACH_N(i, 0, (interval_count + 63) / 64) {
                uint64_t new_val = (live_out->data[i] & ~kill->data[i]) | gen->data[i];

                changes |= (live_in->data[i] != new_val);
                live_in->data[i] = new_val;
            }
        }
    } while (changes);

    ctx->machine_bbs = seq_bb;
}

////////////////////////////////
// Early scheduling
////////////////////////////////
// schedule nodes below any of their pinned dependencies
static bool is_pinned(TB_Node* n) {
    return (n->type >= TB_START && n->type <= TB_TRAP) || n->type == TB_LOAD || n->type == TB_PHI;
}

static void schedule_early(Ctx* restrict ctx, TB_Node* n) {
    if (!nl_hashset_put(&ctx->visited, n)) {
        // already visited
        return;
    }

    // mark as visited (also track use count here)
    size_t use_count = 0;
    for (User* use = find_users(ctx->p, n); use; use = use->next) use_count++;
    nl_map_put(ctx->uses, n, use_count);

    if (n->inputs[0] == NULL) {
        set_input(ctx->p, n, ctx->f->start_node, 0);
    }

    FOREACH_N(i, 1, n->input_count) {
        schedule_early(ctx, n->inputs[i]);

        // choose deepest block
        TB_Node* aa = tb_get_parent_region(n->inputs[0]);
        TB_Node* bb = tb_get_parent_region(n->inputs[i]->inputs[0]);

        if (aa == bb) {
            // we need to measure dom depth within a block now
            //
            // aa
            // VV     a is greater than b
            // bb
            TB_Node* a = n->inputs[0];
            TB_Node* b = n->inputs[i]->inputs[0];

            for (;;) {
                if (a == b) {
                    // a dominates b, we can't be having that
                    set_input(ctx->p, n, n->inputs[i]->inputs[0], 0);
                    break;
    	        }

                if (b->type == TB_START || b->type == TB_REGION) break;
                b = b->inputs[0];
            }
        } else if (dom_depth(ctx->doms, aa) < dom_depth(ctx->doms, bb)) {
            set_input(ctx->p, n, n->inputs[i]->inputs[0], 0);
        }
    }
}

static void schedule_region(Ctx* restrict ctx, TB_Node* n) {
    TB_Node* parent = n->inputs[0];
    if (parent->type != TB_START && parent->type != TB_REGION) {
        schedule_region(ctx, parent);
    }

    FOREACH_N(i, 1, n->input_count) {
        schedule_early(ctx, n->inputs[i]);
    }
}

////////////////////////////////
// Late scheduling
////////////////////////////////
// schedule nodes such that they appear the least common
// ancestor to all their users
static TB_Node* walk_up(TB_Dominators doms, TB_Node* a, TB_Node* b) {
    // if a is deeper, line it up with b
    int bdom = dom_depth(doms, b);
    while (a->input_count > 0) {
        TB_Node* aa = tb_get_parent_region(a);
        ptrdiff_t search = nl_map_get(doms._, aa);
        assert(search >= 0);

        if (doms._[search].v.depth >= bdom) break;
        a = doms._[search].v.node;
    }

    return a;
}

static TB_Node* find_lca(TB_Dominators doms, TB_Node* a, TB_Node* b) {
    if (a == NULL) return b;

    // line both up
    a = walk_up(doms, a, b);
    b = walk_up(doms, b, a);

    while (a != b) {
        a = idom(doms, a);
        b = idom(doms, b);
    }

    return a;
}

static void schedule_late(Ctx* restrict ctx, TB_Node* n) {
    // uses doubles as the visited map for this function
    if (!nl_hashset_put(&ctx->visited, n) || is_pinned(n)) {
        // already visited
        return;
    }

    // we're gonna find the least common ancestor
    TB_Node* lca = NULL;
    for (User* use = find_users(ctx->p, n); use; use = use->next) {
        if (use->slot == 0) continue;

        TB_Node* y = use->n;
        schedule_late(ctx, y);

        TB_Node* use_block = tb_get_parent_region(y->inputs[0]);
        if (y->type == TB_PHI) {
            ptrdiff_t j = -1;
            for (; j < y->input_count; j++) {
                if (y->inputs[j] == n) {
                    break;
                }
            }
            assert(j >= 0);

            use_block = tb_get_parent_region(use_block->inputs[j - 1]);
        }

        lca = find_lca(ctx->doms, lca, use_block);
    }

    set_input(ctx->p, n, lca, 0);
}

// We'll be using this for late schedling
static void postorder_all_nodes(Ctx* restrict ctx, DynArray(TB_Node*)* worklist, TB_Node* n) {
    if (!nl_hashset_put(&ctx->visited, n)) {
        return;
    }

    // walk successors first
    for (User* use = find_users(ctx->p, n); use; use = use->next) {
        postorder_all_nodes(ctx, worklist, use->n);
    }

    dyn_array_put(*worklist, n);
}

static bool use(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->uses, n);
    if (search < 0) {
        return false;
    }

    ctx->uses[search].v -= 1;
    return ctx->uses[search].v == 0;
}

static void fake_unuse(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->uses, n);
    if (search >= 0) {
        ctx->uses[search].v += 1;
    }
}

static void isel_region(Ctx* restrict ctx, TB_Node* control, TB_Node* next) {
    if (control->type != TB_START && control->type != TB_REGION) {
        isel_region(ctx, control->inputs[0], control);
    }

    // set line info
    for (TB_Attrib* a = control->first_attrib; a; a = a->next) if (a->type == TB_ATTRIB_LOCATION) {
        // check if it's changed
        if (ctx->last_file != a->loc.file || ctx->last_line != a->loc.line) {
            ctx->last_file = a->loc.file;
            ctx->last_line = a->loc.line;

            SUBMIT(inst_line(a));
        }
    }

    isel(ctx, control);
}

static void fence(Ctx* restrict ctx, TB_Node* self) {
    for (User* use = find_users(ctx->p, self->inputs[0]); use; use = use->next) {
        TB_Node* n = use->n;

        // make sure to not queue 'next' node
        ptrdiff_t search;
        if (n != self && (search = nl_map_get(ctx->uses, n)) >= 0) {
            // other than these two everything else can be loosely placed
            if (ctx->uses[search].v > 0 && n->type == TB_LOAD) {
                isel(ctx, n);
            }
        }
    }
}

static void fence_last(Ctx* restrict ctx, TB_Node* self, TB_Node* ignore) {
    DynArray(PhiVal) phi_vals = ctx->phi_vals;
    dyn_array_clear(phi_vals);

    if (ignore->type == TB_BRANCH) {
        ptrdiff_t index = -1;
        TB_Node* dst = NULL;

        TB_NodeRegion* r = TB_NODE_GET_EXTRA(self);
        FOREACH_N(i, 0, r->succ_count) {
            dst = r->succ[i];

            // find predecessor index and do that edge
            FOREACH_N(j, 0, dst->input_count) {
                TB_Node* pred = tb_get_parent_region(dst->inputs[j]);

                if (pred == self) {
                    index = j;
                    break;
                }
            }
        }

        assert(index >= 0);
        size_t phi_count = 0;
        for (User* use = find_users(ctx->p, dst); use; use = use->next) {
            TB_Node* n = use->n;
            if (n->type != TB_PHI) continue;

            // allocate virtual register
            ptrdiff_t search = nl_map_get(ctx->values, n);
            PhiVal p = { n, -1, -1 };
            if (search < 0) {
                p.val = DEF(n, n->dt);
                nl_map_put(ctx->values, n, p.val);
            } else {
                p.val = ctx->values[search].v;
            }

            // evaluate PHI but don't writeback yet
            p.tmp = DEF(n, n->dt);
            int src = ISEL(n->inputs[1 + index]);
            SUBMIT(inst_move(n->dt, p.tmp, src));

            dyn_array_put(phi_vals, p);
            phi_count++;
        }
    }

    for (User* use = find_users(ctx->p, self); use; use = use->next) {
        TB_Node* n = use->n;

        // make sure to not queue 'next' node
        ptrdiff_t search;
        if (n != ignore && (search = nl_map_get(ctx->uses, n)) >= 0) {
            if (ctx->uses[search].v > 0 &&
                n->type != TB_PHI &&
                n->type != TB_INTEGER_CONST &&
                // n->type != TB_FLOAT32_CONST &&
                // n->type != TB_FLOAT64_CONST &&
                n->type != TB_MEMBER_ACCESS &&
                !(n->type == TB_PROJ && n->inputs[0]->type == TB_START) &&
                n->type != TB_LOCAL &&
                n->type != TB_GET_SYMBOL_ADDRESS) {
                isel(ctx, n);
            }
        }
    }

    // writeback PHI results now
    dyn_array_for(i, phi_vals) {
        TB_Node* n = phi_vals[i].n;
        SUBMIT(inst_move(n->dt, phi_vals[i].val, phi_vals[i].tmp));
    }
    ctx->phi_vals = phi_vals;
}

// Codegen through here is done in phases
static void compile_function(TB_Passes* restrict p, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, bool emit_asm) {
    TB_Function* restrict f = p->f;
    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .p = p,
        .doms = p->doms,
        .target_abi = f->super.module->target_abi,
        .safepoints = f->safepoint_count ? tb_platform_heap_alloc(f->safepoint_count * sizeof(TB_SafepointKey)) : NULL,
        .emit = {
            .f = f,
            .emit_asm = emit_asm,
            .output = func_out,
            .data = out,
            .capacity = out_capacity,
        }
    };

    // BB scheduling:
    //   we run through BBs in a reverse postorder walk, currently
    //   there's no reodering based on branch weights (since we don't
    //   do those but if we did that would go here.
    ctx.order = tb_function_get_postorder(f);
    assert(ctx.order.traversal[ctx.order.count - 1] == f->start_node && "Codegen must always schedule entry BB first");

    nl_map_create(ctx.values, f->node_count);
    nl_map_create(ctx.uses, f->node_count);

    // Generate intervals for physical registers
    FOREACH_N(i, FIRST_GPR, FIRST_GPR + 16) {
        dyn_array_put(ctx.intervals, (LiveInterval){ .reg_class = REG_CLASS_GPR, .dt = TB_X86_TYPE_QWORD, .reg = i, .assigned = i });
    }

    FOREACH_N(i, FIRST_XMM, FIRST_XMM + 16) {
        dyn_array_put(ctx.intervals, (LiveInterval){ .reg_class = REG_CLASS_XMM, .dt = TB_X86_TYPE_XMMWORD, .reg = i, .assigned = i });
    }

    // allocate more stuff now that we've run stats on the IR
    ctx.emit.return_label = 0;
    nl_map_create(ctx.emit.labels, f->control_node_count);
    nl_map_create(ctx.stack_slots, 8);
    dyn_array_create(ctx.debug_stack_slots, 8);

    // Scheduling: "Global Code Motion Global Value Numbering", Cliff Click 1995
    //   https://courses.cs.washington.edu/courses/cse501/06wi/reading/click-pldi95.pdf
    CUIK_TIMED_BLOCK("schedule") {
        ctx.visited = nl_hashset_alloc(f->node_count);
        FOREACH_REVERSE_N(i, 0, ctx.order.count) {
            TB_Node* bb = ctx.order.traversal[i];

            // schedule all pinned instructions
            schedule_region(&ctx, TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end);
        }

        // generate instruction list we can walk
        DynArray(TB_Node*) worklist = NULL;

        nl_hashset_clear(&ctx.visited);
        postorder_all_nodes(&ctx, &worklist, f->start_node);

        // move nodes closer to their usage site
        nl_hashset_clear(&ctx.visited);
        FOREACH_REVERSE_N(i, 0, dyn_array_length(worklist)) {
            TB_Node* n = worklist[i];

            if (is_pinned(n)) {
                nl_hashset_put(&ctx.visited, n);
                for (User* use = find_users(ctx.p, n); use; use = use->next) {
                    schedule_late(&ctx, use->n);
                }
            } else if (n->input_count == 1) {
                // this is gonna usually be the constants
                schedule_late(&ctx, worklist[i]);
            }
        }

        dyn_array_destroy(worklist);
        nl_hashset_free(ctx.visited);
    }

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    CUIK_TIMED_BLOCK("isel") {
        FOREACH_REVERSE_N(i, 0, ctx.order.count) {
            TB_Node* bb = ctx.order.traversal[i];
            nl_map_put(ctx.emit.labels, bb, 0);

            // mark fallthrough
            ctx.fallthrough = i > 0 ? ctx.order.traversal[i - 1] : NULL;
            if (bb) {
                append_inst(&ctx, inst_label(bb));
            }

            TB_Node* end = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end;
            isel_region(&ctx, end, NULL);
        }
    }

    EMITA(&ctx.emit, "%s:\n", f->super.name);
    {
        CUIK_TIMED_BLOCK("data flow") {
            liveness(&ctx, f);
        }

        // we can in theory have other regalloc solutions and eventually will put
        // graph coloring here.
        CUIK_TIMED_BLOCK("reg alloc") {
            linear_scan(&ctx, f);
        }

        // Arch-specific: convert instruction buffer into actual instructions
        CUIK_TIMED_BLOCK("emit code") {
            emit_code(&ctx, func_out);
        }
    }

    nl_map_free(ctx.emit.labels);
    nl_map_free(ctx.values);
    nl_map_free(ctx.machine_bbs);
    nl_map_free(ctx.uses);
    dyn_array_destroy(ctx.phi_vals);

    if (dyn_array_length(ctx.lines)) {
        ctx.lines[0].pos = 0;
    }

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->stack_usage = ctx.stack_usage;
    func_out->prologue_epilogue_metadata = ctx.regs_to_save;
    func_out->lines = ctx.lines;
    func_out->safepoints = ctx.safepoints;
    func_out->stack_slots = ctx.debug_stack_slots;

    tb_function_free_postorder(&ctx.order);
    tb_arena_clear(&tb__arena);
    nl_map_free(ctx.stack_slots);
}

static void get_data_type_size(TB_DataType dt, size_t* out_size, size_t* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // above 64bits we really dont care that much about natural alignment
            bool is_big_int = dt.data > 64;

            // round up bits to a byte
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data - 1);

            *out_size  = ((bits+7) / 8) << dt.width;
            *out_align = is_big_int ? 8 : ((dt.data + 7) / 8);
            break;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            *out_size = s << dt.width;
            *out_align = s;
            break;
        }
        case TB_PTR: {
            *out_size = 8;
            *out_align = 8;
            break;
        }
        default: tb_unreachable();
    }
}

