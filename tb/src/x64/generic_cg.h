#include "../passes.h"
#include "../codegen/emitter.h"
#include <inttypes.h>
#include <log.h>

static thread_local bool reg_alloc_log;

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

enum {
    INST_LABEL = 1024,
    INST_LINE,

    // inline machine code
    INST_INLINE,

    // marks the terminator
    INST_TERMINATOR,
    INST_EPILOGUE,

    // this is where parameters come from
    INST_ENTRY,

    //    XORPS xmm0, xmm0
    // or XOR   eax,  eax
    INST_ZERO,
};

typedef struct Inst Inst;

// the first set of indices are reserved for physical registers, the
// rest are allocated as virtual registers.
typedef int RegIndex;
_Static_assert(sizeof(TB_PhysicalReg) == sizeof(RegIndex), "these should be the same");

typedef struct MachineBB {
    Inst* first;

    int start, end;
    int terminator;

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
    DynArray(TB_Location) locations;
    TB_SourceFile* last_file;
    int last_line, last_column;

    // Stack
    uint32_t stack_usage;
    NL_Map(TB_Node*, int) stack_slots;
    DynArray(TB_StackSlot) debug_stack_slots;

    uint64_t regs_to_save;

    // current value table
    NL_Map(TB_Node*, RegIndex) values;

    TB_SafepointKey* safepoints;
} Ctx;

static bool fits_into_int8(uint64_t x) {
    int8_t y = x & 0xFF;
    return (int64_t)y == x;
}

static bool fits_into_int32(uint64_t x) {
    uint32_t hi = x >> 32ull;
    return hi == 0 || hi == 0xFFFFFFFF;
}

static size_t emit_prologue(Ctx* restrict ctx);
static size_t emit_epilogue(Ctx* restrict ctx);
static ptrdiff_t alloc_free_reg(Ctx* restrict ctx, int reg_class);
static void init_regalloc(Ctx* restrict ctx);

static TB_X86_DataType legalize(TB_DataType dt);
static bool is_terminator(int type);
static bool wont_spill_around(int type);
static int classify_reg_class(TB_DataType dt);
static int isel(Ctx* restrict ctx, TB_Node* n);

static void emit_code(Ctx* restrict ctx, TB_FunctionOutput* restrict func_out);
static void mark_callee_saved_constraints(Ctx* restrict ctx, uint64_t callee_saved[CG_REGISTER_CLASSES]);

#define ISEL(n) isel(ctx, n)

static void add_debug_local(Ctx* restrict ctx, TB_Node* n, int pos) {
    // could be costly if you had more than like 2-3 attributes per stack slot... which you
    // wouldn't do right?
    dyn_array_for(i, n->attribs) {
        TB_Attrib* a = &n->attribs[i];
        if (a->tag == TB_ATTRIB_VARIABLE) {
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

    // operands
    INST_MEM    = 16,
    INST_GLOBAL = 32,  // operand to TB_Symbol*
    INST_NODE   = 64,  // operand to TB_Node*
    INST_ATTRIB = 128, // operand to TB_Attrib*
    INST_IMM    = 256, // operand in imm
    INST_ABS    = 512, // operand in abs

    // memory op
    INST_INDEXED = 1024,
    INST_SPILL   = 2048,
} InstFlags;

struct Inst {
    Inst* next;

    // prefixes
    InstType type;
    InstFlags flags;

    TB_X86_DataType dt;
    int time, mem_slot;

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
    Inst* i = TB_ARENA_ALLOC(tmp_arena, Inst);
    *i = (Inst){ .type = INST_LABEL, .flags = INST_NODE, .n = n };
    return i;
}

static Inst* inst_line(TB_Attrib* a) {
    Inst* i = TB_ARENA_ALLOC(tmp_arena, Inst);
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
    Inst* i = tb_arena_alloc(tmp_arena, sizeof(Inst) + (total * sizeof(RegIndex)));
    *i = (Inst){ .type = type, .dt = legalize(dt), .out_count = outs, ins, tmps };
    return i;
}

static Inst* inst_move(TB_DataType dt, RegIndex dst, RegIndex src) {
    int machine_dt = legalize(dt);

    Inst* i = tb_arena_alloc(tmp_arena, sizeof(Inst) + (2 * sizeof(RegIndex)));
    *i = (Inst){ .type = machine_dt >= TB_X86_TYPE_SSE_SS ? FP_MOV : MOV, .dt = machine_dt, .out_count = 1, 1 };
    i->operands[0] = dst;
    i->operands[1] = src;
    return i;
}

static Inst* inst_op_global(int type, TB_DataType dt, RegIndex dst, TB_Symbol* s) {
    Inst* i = alloc_inst(type, dt, 1, 1, 0);
    i->flags = INST_GLOBAL;
    i->mem_slot = 1;
    i->operands[0] = dst;
    i->operands[1] = RSP;
    i->s = s;
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
    i->mem_slot = 1;
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
    Inst* i = alloc_inst(type, dt, 0, index >= 0 ? 3 : 2, 0);
    i->flags = INST_MEM | (index >= 0 ? INST_INDEXED : 0);
    i->mem_slot = 0;
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

static Inst* inst_op_rri(int type, TB_DataType dt, RegIndex dst, RegIndex src, int32_t imm) {
    Inst* i = alloc_inst(type, dt, 1, 1, 0);
    i->flags = INST_IMM;
    i->operands[0] = dst;
    i->operands[1] = src;
    i->imm = imm;
    return i;
}

static Inst* inst_op_rrr(int type, TB_DataType dt, RegIndex dst, RegIndex lhs, RegIndex rhs) {
    Inst* i = alloc_inst(type, dt, 1, 2, 0);
    i->operands[0] = dst;
    i->operands[1] = lhs;
    i->operands[2] = rhs;
    return i;
}

static Inst* inst_op_rri_tmp(int type, TB_DataType dt, RegIndex dst, RegIndex src, int32_t imm, RegIndex tmp) {
    Inst* i = alloc_inst(type, dt, 1, 1, 1);
    i->flags = INST_IMM;
    i->operands[0] = dst;
    i->operands[1] = src;
    i->operands[2] = tmp;
    i->imm = imm;
    return i;
}

static Inst* inst_op_rrr_tmp(int type, TB_DataType dt, RegIndex dst, RegIndex lhs, RegIndex rhs, RegIndex tmp) {
    Inst* i = alloc_inst(type, dt, 1, 2, 1);
    i->operands[0] = dst;
    i->operands[1] = lhs;
    i->operands[2] = rhs;
    i->operands[3] = tmp;
    return i;
}

static Inst* inst_op_imm(int type, TB_DataType dt, RegIndex dst, int32_t imm) {
    Inst* i = alloc_inst(type, dt, 1, 0, 0);
    i->flags = INST_IMM;
    i->operands[0] = dst;
    i->imm = imm;
    return i;
}

static Inst* inst_op_ri(int type, TB_DataType dt, RegIndex src, int32_t imm) {
    Inst* i = alloc_inst(type, dt, 0, 1, 0);
    i->flags = INST_IMM;
    i->operands[0] = src;
    i->imm = imm;
    return i;
}

static Inst* inst_op_r(int type, TB_DataType dt, RegIndex dst) {
    Inst* i = alloc_inst(type, dt, 1, 0, 0);
    i->operands[0] = dst;
    return i;
}

static Inst* inst_op_rr(int type, TB_DataType dt, RegIndex dst, RegIndex src) {
    Inst* i = alloc_inst(type, dt, 1, 1, 0);
    i->operands[0] = dst;
    i->operands[1] = src;
    return i;
}

static Inst* inst_op_rr_no_dst(int type, TB_DataType dt, RegIndex lhs, RegIndex rhs) {
    Inst* i = alloc_inst(type, dt, 0, 2, 0);
    i->operands[0] = lhs;
    i->operands[1] = rhs;
    return i;
}

static Inst* inst_op_zero(TB_DataType dt, RegIndex dst) {
    Inst* i = alloc_inst(INST_ZERO, dt, 1, 0, 0);
    i->operands[0] = dst;
    return i;
}

////////////////////////////////
// Register allocation
////////////////////////////////
#include "reg_alloc.h"

#define DEF(n, dt) alloc_vreg(ctx, n, dt)
static int alloc_vreg(Ctx* restrict ctx, TB_Node* n, TB_DataType dt) {
    int i = dyn_array_length(ctx->intervals);
    dyn_array_put(ctx->intervals, (LiveInterval){ .reg_class = classify_reg_class(dt), .n = n, .reg = -1, .hint = -1, .assigned = -1, .dt = legalize(dt), .start = INT_MAX, .split_kid = -1 });
    return i;
}

static void hint_reg(Ctx* restrict ctx, int i, int j) {
    if (ctx->intervals[i].hint < 0) {
        ctx->intervals[i].hint = j;
    }
}

////////////////////////////////
// Data flow analysis
////////////////////////////////
static int liveness(Ctx* restrict ctx, TB_Function* f) {
    size_t interval_count = dyn_array_length(ctx->intervals);
    TB_Arena* arena = tmp_arena;

    // find BB boundaries in sequences
    MachineBBs seq_bb = NULL;
    nl_map_create(seq_bb, ctx->order.count);

    FOREACH_N(i, 0, ctx->order.count) {
        MachineBB bb = {
            .gen = set_create_in_arena(arena, interval_count),
            .kill = set_create_in_arena(arena, interval_count),
            .live_in = set_create_in_arena(arena, interval_count),
            .live_out = set_create_in_arena(arena, interval_count)
        };

        nl_map_put(seq_bb, ctx->order.traversal[i], bb);
    }

    // generate local live sets
    int timeline = 4, epilogue = -1;
    CUIK_TIMED_BLOCK("local liveness") {
        if (ctx->first) {
            Inst* restrict inst = ctx->first;
            assert(inst->type == INST_LABEL);

            // initial label
            MachineBB* mbb = &nl_map_get_checked(seq_bb, f->start_node);
            mbb->first = inst;
            mbb->start = 2;
            inst->time = 2;
            inst = inst->next;

            TB_Node* bb = f->start_node;
            for (; inst; inst = inst->next) {
                if (inst->type == INST_LABEL) {
                    nl_map_get_checked(seq_bb, bb).end = timeline;
                    timeline += 2; // reserved two extra spaces at the end of the BB

                    tb_assert(inst->flags & INST_NODE, "label instruction has no TB_Node* for the region");
                    bb = inst->n;
                    mbb = &nl_map_get_checked(seq_bb, bb);
                    mbb->first = inst->next;
                    mbb->start = timeline;
                } else if (is_terminator(inst->type) && mbb->terminator == 0) {
                    mbb->terminator = timeline;
                } else if (inst->type == INST_EPILOGUE) {
                    epilogue = timeline;
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
    }

    // generate global live sets
    DynArray(TB_Node*) worklist = dyn_array_create(TB_Node*, ctx->order.count);

    // all nodes go into the worklist
    FOREACH_N(i, 0, ctx->order.count) {
        dyn_array_put(worklist, ctx->order.traversal[i]);
    }

    Set tmp_out = set_create_in_arena(arena, interval_count);
    while (dyn_array_length(worklist)) CUIK_TIMED_BLOCK("global iter") {
        TB_Node* bb = dyn_array_pop(worklist);
        TB_NodeRegion* r = TB_NODE_GET_EXTRA(bb);
        MachineBB* mbb = &nl_map_get_checked(seq_bb, bb);

        // walk all successors
        set_clear(&tmp_out);
        if (r->end->type == TB_BRANCH) {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
            FOREACH_N(i, 0, br->succ_count) {
                // union with successor's lives
                MachineBB* succ = &nl_map_get_checked(seq_bb, br->succ[i]);
                set_union(&tmp_out, &succ->live_in);
            }
        }

        bool changes = false;

        // copy to live_out but also check for changes, if
        // there's changes we need to put the successors in
        // the worklist.
        Set* restrict live_out = &mbb->live_out;
        FOREACH_N(i, 0, (interval_count + 63) / 64) {
            if (live_out->data[i] != tmp_out.data[i]) {
                live_out->data[i] = tmp_out.data[i];
                changes = true;
            }
        }

        if (changes && r->end->type == TB_BRANCH) {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
            FOREACH_N(i, 0, br->succ_count) {
                dyn_array_put(worklist, br->succ[i]);
            }
        }

        Set* restrict live_in = &mbb->live_in;
        Set* restrict kill = &mbb->kill;
        Set* restrict gen = &mbb->gen;

        // live_in = (live_out - live_kill) U live_gen
        FOREACH_N(i, 0, (interval_count + 63) / 64) {
            live_in->data[i] = (live_out->data[i] & ~kill->data[i]) | gen->data[i];
        }
    }

    dyn_array_destroy(worklist);
    ctx->machine_bbs = seq_bb;

    assert(epilogue >= 0);
    return epilogue;
}

static void visit_uses(Ctx* restrict ctx, NL_HashSet* visited, TB_Node* n) {
    if (!nl_hashset_put(visited, n)) {
        return;
    }

    FOREACH_REVERSE_N(i, 0, n->input_count) if (n->inputs[i]) {
        tb_assert(n->inputs[i], "empty input... in this economy?");
        visit_uses(ctx, visited, n->inputs[i]);
    }

    // track use count
    size_t use_count = 0;
    for (User* use = find_users(ctx->p, n); use; use = use->next) {
        use_count++;
    }
    nl_map_put(ctx->uses, n, use_count);
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

static bool on_last_use(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->uses, n);
    return search >= 0 ? ctx->uses[search].v == 1 : false;
}

static bool on_2nd_to_last_use(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->uses, n);
    return search >= 0 ? ctx->uses[search].v == 2 : false;
}

static bool has_users(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->uses, n);
    return search >= 0 ? ctx->uses[search].v > 0 : false;
}

static void isel_region(Ctx* restrict ctx, TB_Node* control, TB_Node* next) {
    if (control->type != TB_START && control->type != TB_REGION) {
        isel_region(ctx, control->inputs[0], control);
    }

    // set line info
    dyn_array_for(i, control->attribs) {
        TB_Attrib* a = &control->attribs[i];

        // check if it's changed
        if (a->tag == TB_ATTRIB_LOCATION && (ctx->last_file != a->loc.file || ctx->last_line != a->loc.line || ctx->last_column != a->loc.column)) {
            ctx->last_file = a->loc.file;
            ctx->last_line = a->loc.line;
            ctx->last_column = a->loc.column;

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
        if (r->end->type == TB_BRANCH) {
            TB_NodeBranch* br = TB_NODE_GET_EXTRA(r->end);
            FOREACH_N(i, 0, br->succ_count) {
                dst = br->succ[i];

                // find predecessor index and do that edge
                FOREACH_N(j, 0, dst->input_count) {
                    TB_Node* pred = tb_get_parent_region(dst->inputs[j]);

                    if (pred == self) {
                        index = j;
                        break;
                    }
                }
            }
        }

        assert(index >= 0);
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
            p.tmp = DEF(NULL, n->dt);

            int src = isel(ctx, n->inputs[1 + index]);
            hint_reg(ctx, p.tmp, src);
            SUBMIT(inst_move(n->dt, p.tmp, src));

            dyn_array_put(phi_vals, p);
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
                n->type != TB_SYMBOL) {
                isel(ctx, n);
            }
        }
    }

    // writeback PHI results now
    dyn_array_for(i, phi_vals) {
        TB_Node* n = phi_vals[i].n;

        hint_reg(ctx, phi_vals[i].val, phi_vals[i].tmp);
        SUBMIT(inst_move(n->dt, phi_vals[i].val, phi_vals[i].tmp));
    }
    ctx->phi_vals = phi_vals;
}

// Codegen through here is done in phases
static void compile_function(TB_Passes* restrict p, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, bool emit_asm) {
    verify_tmp_arena(p);

    TB_Function* restrict f = p->f;

    // gives every single node a control edge so we know when they
    // need to schedule
    tb_pass_schedule(p);

    DO_IF(TB_OPTDEBUG_PEEP)(log_debug("%s: starting codegen with %d nodes", f->super.name, f->node_count));

    reg_alloc_log = strcmp(f->super.name, "stbi__parse_huffman_block") == 0;
    if (reg_alloc_log) {
        printf("\n\n\n");
        tb_pass_print(p);
    } else {
        emit_asm = false;
    }

    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .p = p,
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
    CUIK_TIMED_BLOCK("postorder") {
        ctx.order = tb_function_get_postorder(f);
        assert(ctx.order.traversal[ctx.order.count - 1] == f->start_node && "Codegen must always schedule entry BB first");
    }

    nl_map_create(ctx.values, f->node_count);
    init_regalloc(&ctx);

    CUIK_TIMED_BLOCK("uses") {
        NL_HashSet visited = nl_hashset_arena_alloc(tmp_arena, f->node_count);
        nl_map_create(ctx.uses, f->node_count);
        FOREACH_REVERSE_N(i, 0, ctx.order.count) {
            visit_uses(&ctx, &visited, TB_NODE_GET_EXTRA_T(ctx.order.traversal[i], TB_NodeRegion)->end);
        }
        assert(visited.count <= f->node_count);
        nl_hashset_free(visited);
    }

    // allocate more stuff now that we've run stats on the IR
    ctx.emit.return_label = 0;
    nl_map_create(ctx.emit.labels, ctx.order.count);
    nl_map_create(ctx.stack_slots, 8);
    dyn_array_create(ctx.debug_stack_slots, 8);

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    CUIK_TIMED_BLOCK("isel") {
        TB_Node* stop_node = f->stop_node;
        TB_Node* stop_bb = tb_get_parent_region(stop_node);

        bool has_stop = false;
        FOREACH_REVERSE_N(i, 0, ctx.order.count) {
            TB_Node* bb = ctx.order.traversal[i];
            nl_map_put(ctx.emit.labels, bb, 0);

            if (bb != stop_bb) {
                // mark fallthrough
                ctx.fallthrough = i > 0 ? ctx.order.traversal[i - 1] : NULL;
                if (ctx.fallthrough == stop_bb) ctx.fallthrough = NULL;

                append_inst(&ctx, inst_label(bb));
                TB_Node* end = TB_NODE_GET_EXTRA_T(bb, TB_NodeRegion)->end;
                isel_region(&ctx, end, NULL);
            } else {
                has_stop = true;
            }
        }

        // always schedule the STOP node here
        if (has_stop) {
            // mark fallthrough
            ctx.fallthrough = NULL;

            append_inst(&ctx, inst_label(stop_bb));
            TB_Node* end = TB_NODE_GET_EXTRA_T(stop_bb, TB_NodeRegion)->end;
            isel_region(&ctx, end, NULL);
        } else {
            // liveness expects one but we don't really have shit to put down there... it's never reached
            append_inst(&ctx, alloc_inst(INST_EPILOGUE, TB_TYPE_VOID, 0, 0, 0));
        }
    }

    EMITA(&ctx.emit, "%s:\n", f->super.name);
    {
        int end;
        CUIK_TIMED_BLOCK("data flow") {
            end = liveness(&ctx, f);
        }

        // we can in theory have other regalloc solutions and eventually will put
        // graph coloring here.
        ctx.stack_usage = linear_scan(&ctx, f, ctx.stack_usage, end);

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

    if (dyn_array_length(ctx.locations)) {
        ctx.locations[0].pos = 0;
    }

    // we're done, clean up
    func_out->asm_out = ctx.emit.head_asm;
    func_out->code = ctx.emit.data;
    func_out->code_size = ctx.emit.count;
    func_out->stack_usage = ctx.stack_usage;
    func_out->locations = ctx.locations;
    func_out->safepoints = ctx.safepoints;
    func_out->stack_slots = ctx.debug_stack_slots;

    tb_function_free_postorder(&ctx.order);
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
