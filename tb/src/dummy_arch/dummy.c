#include "../tb_internal.h"
#include "../codegen/emitter.h"

#include "../x64/x64.h"

enum {
    CG_VAL_UNRESOLVED = 0,
    CG_VAL_FLAGS      = 1,
    CG_VAL_REGISTER   = 2,
};

enum {
    CG_REGISTER_CLASSES = 2
};

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

typedef enum X86_DataType {
    X86_TYPE_NONE = 0,

    X86_TYPE_BYTE,     // 1
    X86_TYPE_WORD,     // 2
    X86_TYPE_DWORD,    // 4
    X86_TYPE_QWORD,    // 8

    X86_TYPE_PBYTE,   // int8 x 16 = 16
    X86_TYPE_PWORD,   // int16 x 8 = 16
    X86_TYPE_PDWORD,  // int32 x 4 = 16
    X86_TYPE_PQWORD,  // int64 x 2 = 16

    X86_TYPE_SSE_SS,  // float32 x 1 = 4
    X86_TYPE_SSE_SD,  // float64 x 1 = 8
    X86_TYPE_SSE_PS,  // float32 x 4 = 16
    X86_TYPE_SSE_PD,  // float64 x 2 = 16

    X86_TYPE_XMMWORD, // the generic idea of them
} X86_DataType;

typedef enum X86_InstType {
    X86_MOV,

    X86_AND,
    X86_ADD,
} X86_InstType;

typedef enum X86_InstrFlags {
    // uses xmm registers for the reg array
    X86_INSTR_XMMREG = (1u << 0u),

    // r/m is a memory operand
    X86_INSTR_USE_MEMOP = (1u << 1u),

    // r/m is a rip-relative address (X86_INSTR_USE_MEMOP is always set when this is set)
    X86_INSTR_USE_RIPMEM = (1u << 2u),

    // LOCK prefix is present
    X86_INSTR_LOCK = (1u << 3u),

    // uses a signed immediate
    X86_INSTR_IMMEDIATE = (1u << 4u),

    // absolute means it's using the 64bit immediate (cannot be applied while a memory operand is active)
    X86_INSTR_ABSOLUTE = (1u << 5u),

    // set if the r/m can be found on the right hand side
    X86_INSTR_DIRECTION = (1u << 6u),

    // uses the second data type because the instruction is weird like MOVSX or MOVZX
    X86_INSTR_TWO_DATA_TYPES = (1u << 7u)
} X86_InstrFlags;

typedef struct X86_Inst {
    X86_InstType type;

    X86_DataType data_type  : 8;
    X86_DataType data_type2 : 8;
    // X86_Segment segment  : 8;
    X86_InstrFlags flags    : 8;
    uint8_t length;

    // normal operands
    int8_t regs[4];

    // immediate operand
    //   imm for INSTR_IMMEDIATE
    //   abs for INSTR_ABSOLUTE
    union {
        int32_t  imm;
        uint64_t abs;
    };

    // memory operand
    struct {
        int8_t base;
        int8_t index;
        Scale scale;
        int32_t disp;
    } mem;
} X86_Inst;

// per instruction selection
enum {
    CG_MAX_DEFS  = 8,
    CG_MAX_INSTS = 8,
};

typedef struct {
    TB_Node* last_use;
    int ordinal;
} Liveness;

typedef struct {
    TB_Node* key;
    Val val;
} ValueDesc;

typedef struct {
    int start, end;
} LiveInterval;

typedef struct Def {
    TB_Node* n;

    // if live_in and we want to make a mutable copy, we use
    // this to define what we copy from.
    TB_Node* based;

    // which pool of registers to grab from
    int reg_class;

    // live_in
    bool live_in, live_out;

    // resolved value
    int reg;
} Def;

typedef struct Ctx {
    TB_CGEmitter emit;

    // for panic-based error handling
    jmp_buf restore_point;

    // some analysis
    NL_Map(TB_Node*, Liveness) intervals;

    // Reg alloc
    TB_Node** active;
    size_t active_count;

    Set free_regs[CG_REGISTER_CLASSES];

    // instruction queue
    size_t inst_count, def_count;
    Def defs[CG_MAX_DEFS];
    X86_Inst insts[CG_MAX_INSTS];

    // current value
    size_t values_count, values_exp;
    ValueDesc* values;
} Ctx;

static LiveInterval get_bb_interval(Ctx* restrict ctx, TB_Function* f, TB_Label bb) {
    ptrdiff_t search = nl_map_get(ctx->intervals, f->bbs[bb].start);
    assert(search >= 0);

    ptrdiff_t search2 = nl_map_get(ctx->intervals, f->bbs[bb].end);
    assert(search2 >= 0);

    LiveInterval li;
    li.start = ctx->intervals[search].v.ordinal;
    li.end = ctx->intervals[search2].v.ordinal;

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

static LiveInterval get_live_interval(Ctx* restrict ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->intervals, n);
    assert(search >= 0);

    TB_Node* last_use = ctx->intervals[search].v.last_use;
    ptrdiff_t search2 = nl_map_get(ctx->intervals, last_use);
    assert(search2 >= 0);

    LiveInterval li;
    li.start = ctx->intervals[search].v.ordinal;
    li.end = ctx->intervals[search2].v.ordinal;

    if (li.start > li.end) {
        tb_swap(int, li.start, li.end);
    }
    return li;
}

#define GET_VAL(n) (*get_val(ctx, n))
static Val* get_val(Ctx* restrict ctx, TB_Node* n) {
    uint32_t hash = (((uintptr_t) n) * 11400714819323198485llu) >> 32u;

    for (size_t i = hash;;) {
        // hash table lookup
        uint32_t mask = (1 << ctx->values_exp) - 1;
        uint32_t step = (hash >> (32 - ctx->values_exp)) | 1;
        i = (i + step) & mask;

        if (ctx->values[i].key == NULL) {
            assert(ctx->values_count < (1u << ctx->values_exp));

            // new slot
            ctx->values_count++;
            ctx->values[i].key = n;
            ctx->values[i].val = (Val){ 0 };
            return &ctx->values[i].val;
        } else if (ctx->values[i].key == n) {
            return &ctx->values[i].val;
        }
    }
}

static bool fits_into_int32(uint64_t x) {
    int32_t y = x & 0xFFFFFFFF;
    return (int64_t)y == x;
}

#define SUBMIT_INST(op, ...) (ctx->insts[ctx->inst_count++] = (X86_Inst){ X86_ ## op, __VA_ARGS__ })
#define DEF(n, ...) (ctx->defs[ctx->def_count] = (Def){ n, __VA_ARGS__ }, ctx->def_count++)

// references an allocated
#define REF(x) (-(x))

// *out_mask of 0 means no mask
static X86_DataType legalize_int(TB_DataType dt, uint64_t* out_mask) {
    assert(dt.type == TB_INT || dt.type == TB_PTR);
    if (dt.type == TB_PTR) return *out_mask = 0, X86_TYPE_QWORD;

    X86_DataType t = X86_TYPE_NONE;
    int bits = 0;

    if (dt.data <= 8) bits = 8, t = X86_TYPE_BYTE;
    else if (dt.data <= 16) bits = 16, t = X86_TYPE_WORD;
    else if (dt.data <= 32) bits = 32, t = X86_TYPE_DWORD;
    else if (dt.data <= 64) bits = 64, t = X86_TYPE_QWORD;

    assert(bits != 0 && "TODO: large int support");
    uint64_t mask = ~UINT64_C(0) >> (64 - dt.data);

    *out_mask = (dt.data == bits) ? 0 : mask;
    return t;
}

enum {
    REG_CLASS_GPR,
    REG_CLASS_XMM
};

static Val isel(Ctx* restrict ctx, TB_Node* n) {
    switch (n->type) {
        case TB_INTEGER_CONST: {
            TB_NodeInt* i = TB_NODE_GET_EXTRA(n);
            assert(i->num_words == 1);

            uint64_t x = i->words[0];
            assert(fits_into_int32(x));
            return val_imm(n->dt, x);
        }
        case TB_ADD: {
            uint64_t mask;
            X86_DataType t = legalize_int(n->dt, &mask);

            int dst = DEF(n, .based = n->inputs[0], .reg_class = REG_CLASS_GPR, .live_in = true, .live_out = true);
            Val* b = &GET_VAL(n->inputs[1]);
            if (b->type == VAL_IMM) {
                SUBMIT_INST(ADD, .data_type = t, .flags = X86_INSTR_IMMEDIATE, .regs[0] = REF(dst), .imm = b->imm);
            } else {
                int other = DEF(n->inputs[1], .reg_class = REG_CLASS_GPR);
                SUBMIT_INST(ADD, .data_type = t, .regs[0] = REF(dst), .regs[1] = REF(other));
            }

            if (mask) SUBMIT_INST(AND, .data_type = t, .regs[0] = REF(dst));
            return val_gpr(n->dt, REF(dst));
        }
        default: tb_todo();
    }
}

static void remove_active(Ctx* restrict ctx, size_t i, TB_Node* n) {
    if (i + 1 == ctx->active_count) {
        memmove(&ctx->active[i], &ctx->active[i + 1], ctx->active_count - 1);
    }
}

static Val alloc_reg(Ctx* restrict ctx, TB_Function* f, TB_Node* n, int reg_class) {
    LiveInterval r_li = get_live_interval(ctx, n);

    if (ctx->active_count == ctx->free_regs[reg_class].capacity) {
        FOREACH_N(i, 0, ctx->active_count) {
            TB_Node* spill = ctx->active[i];
            Val* spill_v = &GET_VAL(spill);

            if (spill_v->type == CG_VAL_REGISTER + reg_class) {
                // GAD_FN(explicit_steal)(ctx, f, n, spill_reg, i);
                tb_todo();
            }
        }

        tb_unreachable();
        return (Val){ 0 };
    } else {
        ptrdiff_t reg_num = set_pop_any(&ctx->free_regs[reg_class]);
        assert(reg_num >= 0);

        Val v = {
            .type = CG_VAL_REGISTER + reg_class,
            .r = n, .dt = n->dt, .reg = reg_num
        };

        GET_VAL(n) = v;
        set_put(&ctx->free_regs[reg_class], reg_num);

        // mark register as to be saved
        // ctx->regs_to_save[reg_class] |= (1u << reg_num) & ctx->callee_saved[reg_class];

        // insert by increasing end point
        // TODO(NeGate): do binary insert since the array is sorted
        FOREACH_REVERSE_N(i, 0, ctx->active_count) {
            LiveInterval k_li = get_live_interval(ctx, ctx->active[i]);
            if (k_li.end > r_li.end) continue;

            memmove(ctx->active, ctx->active[i + 1], (ctx->active_count - i) * sizeof(TB_Node*));
            ctx->active[i + 1] = n;
            ctx->active_count += 1;
        }

        // insert at the end
        ctx->active[ctx->active_count++] = n;
        return v;
    }
}

static void trap(Ctx* restrict ctx, int code) {
    longjmp(ctx->restore_point, code);
}

// Codegen through here is done in phases
static TB_FunctionOutput compile_function(TB_Function* restrict f, const TB_FeatureSet* features, uint8_t* out, size_t out_capacity, size_t local_thread_id) {
    TB_TemporaryStorage* tls = tb_tls_allocate();
    tb_function_print(f, tb_default_print_callback, stdout, false);

    Ctx* restrict ctx = arena_alloc(&tb__arena, sizeof(Ctx), _Alignof(Ctx));
    *ctx = (Ctx){
        .emit = {
            .f = f,
            .data = out,
            .capacity = out_capacity,
            // .labels = ARENA_ARR_ALLOC(&tb__arena, f->bb_count, uint32_t),
            // .label_patches = ARENA_ARR_ALLOC(&tb__arena, tally.label_patch_count, LabelPatch),
            // .ret_patches = ARENA_ARR_ALLOC(&tb__arena, tally.return_count, ReturnPatch),
        }
    };

    // Setup restore point so the codegen can safely exit
    int result_code = setjmp(ctx->restore_point);
    if (result_code != 0) {
        // we had an error
        arena_clear(&tb__arena);
        return (TB_FunctionOutput){ .result = result_code };
    }

    ctx->free_regs[0] = set_create(16);
    ctx->free_regs[1] = set_create(16);

    set_put(&ctx->free_regs[0], RBP), set_put(&ctx->free_regs[0], RSP);

    // Live intervals:
    //   We compute this for register allocation along
    //   with the "ordinals" which act as our timeline.
    int counter = 0, bb_count = 0, label_patch_count = 0;
    TB_FOR_BASIC_BLOCK(bb, f) {
        TB_FOR_NODE(n, f, bb) {
            // create new entry with no last user
            Liveness l = { .ordinal = counter++ };
            nl_map_put(ctx->intervals, n, l);

            if (n->type == TB_BRANCH) {
                label_patch_count += 1 + TB_NODE_GET_EXTRA_T(n, TB_NodeBranch)->count;
            }

            TB_FOR_INPUT_IN_NODE(in, n) {
                // extend last user
                ptrdiff_t search = nl_map_get(ctx->intervals, *in);
                assert(search >= 0 && "use before define?");

                ctx->intervals[search].v.last_use = n;
            }
        }
    }

    // allocate more stuff now that we've run stats on the IR
    ctx->emit.labels = ARENA_ARR_ALLOC(&tb__arena, f->bb_count, uint32_t);
    ctx->emit.label_patches = ARENA_ARR_ALLOC(&tb__arena, tally.label_patch_count, LabelPatch);
    ctx->emit.ret_patches = ARENA_ARR_ALLOC(&tb__arena, tally.return_count, ReturnPatch);

    // allocate values map and active, for linear scan
    size_t ht_cap = tb_next_pow2((counter * 4) / 3) - 1;
    size_t ht_exp = tb_ffs(ht_cap - 1);

    ctx->values_exp = ht_exp;
    ctx->values = arena_alloc(&tb__arena, ht_cap * sizeof(ValueDesc), _Alignof(ValueDesc));
    ctx->active = arena_alloc(&tb__arena, counter * sizeof(TB_Node*), _Alignof(TB_Node*));

    // BB scheduling:
    //   we run through BBs in a reverse postorder walk, currently
    //   there's no reodering based on branch weights (since we don't
    //   do those but if we did that would go here.
    TB_PostorderWalk walk = {
        .visited = tb_tls_push(tls, f->bb_count * sizeof(bool)),
        .traversal = tb_tls_push(tls, f->bb_count * sizeof(TB_Node*)),
    };
    tb_function_get_postorder_explicit(f, &walk);
    assert(walk.traversal[walk.count - 1] == 0 && "Codegen must always schedule entry BB first");

    // Instruction selection:
    //   we just decide which instructions to emit, which operands are
    //   fixed and which need allocation. For now regalloc is handled
    //   immediately but in theory it could be delayed until all selection
    //   is done.
    FOREACH_REVERSE_N(i, 0, walk.count) {
        TB_Label bb = walk.traversal[i];
        TB_Node* bb_end = f->bbs[bb].end;

        // mark BB start
        // ctx->emit.labels[bb] = GET_CODE_POS(&ctx->emit);

        // doesn't iterate over the terminator
        for (TB_Node* n = f->bbs[bb].start; n != bb_end; n = n->next) {
            // selection
            switch (n->type) {
                case TB_NULL:
                case TB_PARAM:
                case TB_LOCAL:
                case TB_PARAM_ADDR:
                case TB_PHI:
                goto skip;

                case TB_LINE_INFO: {
                    /*f->lines[f->line_count++] = (TB_Line) {
                        .file = n->line_info.file,
                        .line = n->line_info.line,
                        .pos = GET_CODE_POS(&ctx->emit)
                    };*/
                    goto skip;
                }

                default:
                assert(ctx->def_count == 0);

                Val v = isel(ctx, n);
                GET_VAL(n) = v;
                break;
            }

            // linear scan alloc
            //   expired old intervals
            LiveInterval r_li = get_live_interval(ctx, n);
            FOREACH_N(i, 0, ctx->active_count) {
                TB_Node* k = ctx->active[i];
                LiveInterval k_li = get_live_interval(ctx, k);
                if (k_li.end >= r_li.start) break;

                Val* v = &GET_VAL(k);
                if (v->type >= CG_VAL_REGISTER && v->type < CG_VAL_REGISTER + CG_REGISTER_CLASSES) {
                    set_remove(&ctx->free_regs[v->type - CG_VAL_REGISTER], v->reg);
                }
                remove_active(ctx, i, k);
            }
            //   allocate new regs
            FOREACH_N(i, 0, ctx->def_count) {
                Def* d = &ctx->defs[i];

                // copied from somewhere else
                if (d->based) {
                    Val* src = &GET_VAL(d->based);

                    // allocate copy
                    Val copy = alloc_reg(ctx, f, d->n, d->reg_class);
                    (void)src;
                    (void)copy;
                    __debugbreak();
                }
                tb_todo();
            }
            ctx->def_count = 0;

            // emit sequences
            FOREACH_N(i, 0, ctx->inst_count) {
                tb_todo();
            }
            ctx->inst_count = 0;

            skip:;
        }

        // handle terminator
        printf("L%d\n", bb);

        // GAD_FN(eval_bb)(ctx, f, bb, i > 0 ? walk.traversal[i - 1] : 0);
    }

    EMIT1(&ctx->emit, 0xC3);

    //  Label patching: we make sure any local labels
    //


    // we're done, clean up
    TB_FunctionOutput func_out = {
        .linkage = f->linkage,
        .code = ctx->emit.data,
        .code_size = ctx->emit.count,
        // .stack_usage = ctx->stack_usage,
        // .prologue_epilogue_metadata = ctx->regs_to_save[0],
        // .stack_slots = ctx->stack_slots
    };

    arena_clear(&tb__arena);
    __debugbreak();
    return func_out;
}

size_t emit_prologue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

size_t emit_epilogue(uint8_t* out, uint64_t saved, uint64_t stack_usage) {
    return 0;
}

static size_t emit_call_patches(TB_Module* restrict m) {
    return 0;
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__dummy_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .emit_call_patches  = emit_call_patches,
    .get_data_type_size = get_data_type_size,
    .emit_prologue      = emit_prologue,
    .emit_epilogue      = emit_epilogue,

    .fast_path = compile_function,
    //.complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
