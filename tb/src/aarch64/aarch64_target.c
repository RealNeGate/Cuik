// NOTE(NeGate): THIS IS VERY INCOMPLETE
#include "../tb_internal.h"
#include "../emitter.h"
#include "aarch64_emitter.h"

enum {
    // register classes
    REG_CLASS_GPR,
    REG_CLASS_COUNT,
};

#include "../codegen_impl.h"

static void init_ctx(Ctx* restrict ctx, TB_ABI abi) {
    ctx->sched = greedy_scheduler;
    ctx->regalloc = tb__lsra;

    ctx->num_regs[0] = 32;
}

static RegMask in_reg_mask(Ctx* restrict ctx, Tile* tile, TB_Node* n, int i) {
    tb_todo();
}

static RegMask isel_node(Ctx* restrict ctx, Tile* dst, TB_Node* n) {
    tb_todo();
}

static bool clobbers(Ctx* restrict ctx, Tile* t, uint64_t clobbers[MAX_REG_CLASSES]) {
    return false;
}

static void emit_tile(Ctx* restrict ctx, TB_CGEmitter* e, Tile* t) {
    tb_todo();
}

#define E(fmt, ...) tb_asm_print(e, fmt, ## __VA_ARGS__)
static void disassemble(TB_CGEmitter* e, Disasm* restrict d, int bb, size_t pos, size_t end) {
    if (bb >= 0) {
        E(".bb%d:\n", bb);
    }
}
#undef E

ICodeGen tb__aarch64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,
    .emit_win64eh_unwind_info = NULL,
    .emit_call_patches  = NULL,
    .get_data_type_size = get_data_type_size,
    .compile_function   = compile_function,
};
