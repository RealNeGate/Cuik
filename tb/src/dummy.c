#if 0
#include "tb_internal.h"
#include "codegen/emitter.h"

#if 1
#include <tb_x64.h>
#include "x64/x64.h"

enum {
    CG_REGISTER_CLASSES = 2
};

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM
};

typedef enum X86_InstType {
    //   dst = COPY src
    X86_INST_COPY = -3,
    X86_INST_MOVE = -4,
} X86_InstType;

// for memory operands imm[0] is two fields:
//   top 32bits is scale, bottom 32bits is displacement
typedef enum X86_OperandLayout {
    X86_OP_NONE,

    // label
    X86_OP_L,
    // global
    X86_OP_G,

    // integer unary
    X86_OP_R,
    X86_OP_M,
    X86_OP_I,
    X86_OP_A,

    // integer binary ops
    X86_OP_RR,
    X86_OP_RI,
    X86_OP_MI,
    X86_OP_RM,
    X86_OP_MR,
} X86_OperandLayout;

typedef enum {
    INST_LOCK   = 1,
    INST_REP    = 2,
    INST_REPNE  = 4,
} InstPrefix;

typedef struct Inst Inst;
struct Inst {
    Inst* next;

    // prefixes
    InstPrefix prefix : 16;
    InstType type : 16;

    X86_OperandLayout layout;
    TB_X86_DataType data_type;
    int time;

    // virtual registers (-1 means none, -2 and lower is VREGs, 0+ is normal registers)
    //
    //   regs[0] is a destination
    int regs[4];
    uint64_t imm[2];
};

typedef struct {
    TB_DataType dt;

    int old;
    int stack_pos;
} Reload;
#endif

typedef uint32_t ValueRef;

typedef struct MachineBB {
    Inst* first;

    // on the timeline/slot indices
    int start, end;

    // local live sets
    Set gen, kill;
    // global
    Set live_in, live_out;
} MachineBB;

typedef struct MachineReg {
    uint8_t class, num;
} MachineReg;

typedef struct Clobbers {
    int count;
    MachineReg _[];
} Clobbers;

typedef ptrdiff_t DefIndex;
typedef struct Def {
    TB_Node* node;

    // lifetime
    int start, end;

    // regalloc
    int16_t complete, hint;
    int16_t reg_class, reg;

    // when we preallocate a definition we
    // specify here which definition must
    // be completed for it to be free again
    DefIndex live_until;

    // once the def is live, these registers are clobbered
    Clobbers* clobbers;
} Def;

typedef struct {
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;
    TB_ABI target_abi;

    // machine output sequences
    Inst *first, *last;
    DynArray(Def) defs;
    DynArray(Reload) reloads;
    DynArray(TB_Node*) in_bound;
    MachineBB* seq_bb;

    // Stack
    uint32_t stack_usage;
    NL_Map(TB_Node*, int) stack_slots;

    // Reg alloc
    DefIndex* active;
    size_t active_count;

    Set used_regs[CG_REGISTER_CLASSES];

    // current value table
    NL_Map(TB_Node*, ValueRef) values;
} Ctx;

// *out_mask of 0 means no mask
static TB_X86_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_INT || dt.type == TB_PTR);
    if (dt.type == TB_PTR) return *out_mask = 0, TB_X86_TYPE_QWORD;

    TB_X86_DataType t = TB_X86_TYPE_NONE;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = TB_X86_TYPE_BYTE;
    else if (dt.data <= 16) bits = 16, t = TB_X86_TYPE_WORD;
    else if (dt.data <= 32) bits = 32, t = TB_X86_TYPE_DWORD;
    else if (dt.data <= 64) bits = 64, t = TB_X86_TYPE_QWORD;

    uint64_t mask = ~UINT64_C(0) >> (64 - bits);
    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

static TB_X86_DataType legalize_int2(TB_DataType dt) {
    uint64_t m;
    return legalize_int(dt, &m);
}

static Inst inst_copy(TB_DataType dt, int lhs, int rhs) {
    return (Inst){
        .type = (int) X86_INST_COPY,
        .layout = X86_OP_RR,
        .data_type = legalize_int2(dt),
        .regs = { lhs, rhs }
    };
}

#define GET_VAL(n) nl_map_get_checked(ctx->values, n)
#define DEF(n, rg) put_def(ctx, n, rg)
static int put_def(Ctx* restrict ctx, TB_Node* n, int reg_class) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .reg_class = reg_class, .reg = -1, .hint = -1 });
    return i;
}

#define DEF_HINTED(n, rg, hint) put_def_hinted(ctx, n, rg, hint)
static int put_def_hinted(Ctx* restrict ctx, TB_Node* n, int reg_class, int hint) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .end = INT_MIN, .reg_class = reg_class, .reg = -1, .hint = hint });
    return i;
}

#define DEF_FORCED(n, rg, reg, live_until) put_def_forced(ctx, n, rg, reg, live_until)
static int put_def_forced(Ctx* restrict ctx, TB_Node* n, int reg_class, int reg, int live_until) {
    int i = dyn_array_length(ctx->defs);
    dyn_array_put(ctx->defs, (Def){ .node = n, .start = INT_MAX, .end = INT_MIN, .reg_class = reg_class, .reg = reg, .live_until = live_until });
    return i;
}

#define SUBMIT(i) append_inst(ctx, i)
static void append_inst(Ctx* restrict ctx, Inst i) {
    Inst* new_inst = ARENA_ALLOC(&tb__arena, Inst);
    *new_inst = i;

    if (ctx->last == NULL) {
        ctx->first = ctx->last = new_inst;
    } else {
        ctx->last->next = new_inst;
        ctx->last = new_inst;
    }
}

static ValueRef isel(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->values, n);
    if (search >= 0) {
        return ctx->values[search].v;
    }

    switch (n->type) {
        case TB_START: {
            TB_NodeStart* start = TB_NODE_GET_EXTRA(n);
            const TB_FunctionPrototype* restrict proto = ctx->f->prototype;

            // Handle known parameters
            FOREACH_N(i, 0, proto->param_count) {
                TB_Node* proj = start->projs[i];

                // copy from parameter
                ValueRef v = DEF_HINTED(proj, REG_CLASS_GPR, WIN64_GPR_PARAMETERS[i]);
                ctx->defs[v].start = -100 + i;
                SUBMIT(inst_copy(proj->dt, v, WIN64_GPR_PARAMETERS[i]));

                nl_map_put(ctx->values, proj, v);
            }

            // Handle unknown parameters (if we have varargs)
            if (proto->has_varargs) {
                tb_todo();
            }
            return 0;
        }

        case TB_LOCAL: {
            return 0;
        }

        case TB_PROJ: {
            // process root
            isel(ctx, n->inputs[0]);

            search = nl_map_get(ctx->values, n);
            assert(search >= 0);

            return ctx->values[search].v;
        }

        default: tb_todo();
    }
}

static void schedule_effect(Ctx* restrict ctx, TB_Node* n) {
    if (n->input_count > 0 && n->inputs[0]->type != TB_REGION) {
        schedule_effect(ctx, n->inputs[0]);
    }

    switch (n->type) {
        case TB_START:
        case TB_REGION: {
            printf("label %p: # %d preds\n", n, n->input_count);
            break;
        }

        case TB_STORE: {
            ValueRef src = isel(ctx, n->inputs[2]);
            ValueRef addr = isel(ctx, n->inputs[1]);

            break;
        }

        default: tb_todo();
    }

    printf("  %s\n", tb_node_get_name(n));

    /*FOREACH_N(i, 0, n->input_count) if (n->inputs[i] != NULL) { TB_Node* in = 0;
    }*/
}

static TB_FunctionOutput compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity) {
    tb_function_print(f, tb_default_print_callback, stdout);

    TB_PostorderWalk order = tb_function_get_postorder(f);
    Ctx ctx = {
        .module = f->super.module,
        .f = f,
        .target_abi = f->super.module->target_abi,
        .emit = {
            .f = f,
            .data = out,
            .capacity = out_capacity,
        }
    };

    ctx.used_regs[0] = set_create(16);
    ctx.used_regs[1] = set_create(16);
    set_put(&ctx.used_regs[0], RBP), set_put(&ctx.used_regs[0], RSP);

    // This deals with certain ABI details
    // like parameter passing.
    isel(&ctx, f->start_node);

    // we first order all the effect nodes, from there
    // they'll do instruction selection to decide how
    // their non-effect nodes are used.
    FOREACH_N(i, 0, order.count) {
        TB_Node* region = order.traversal[i].start;
        TB_Node* terminator = order.traversal[i].end;

        schedule_effect(&ctx, terminator);
    }

    __debugbreak();
    return (TB_FunctionOutput){ 0 };
}

static size_t emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static size_t emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static void emit_win64eh_unwind_info(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t saved, uint64_t stack_usage) {

}

static void get_data_type_size(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align) {
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

ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .emit_win64eh_unwind_info = emit_win64eh_unwind_info,

    #if 0
    .emit_call_patches        = emit_call_patches,
    #endif

    .get_data_type_size       = get_data_type_size,
    .emit_prologue            = emit_prologue,
    .emit_epilogue            = emit_epilogue,

    .fast_path = compile_function,
    //.complex_path = x64_complex_compile_function
};
#endif
