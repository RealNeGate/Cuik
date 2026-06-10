#include "../emitter.h"
#include "../tb_internal.h"
#include <string.h>
#include <ctype.h>

static void global_init() {}

typedef enum GPR {
    ZR,
    AT,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    GP,
    SP,
    FP,
    RA,
    GPR_NONE = -1,
} GPR;

typedef enum FPR {
    F0,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    F25,
    F26,
    F27,
    F28,
    F29,
    F30,
    F31,
    FPR_NONE = -1,
} FPR;

enum {
    REG_CLASS_GPR = 1,
    REG_CLASS_FPR = 2,
    REG_CLASS_COUNT,
};

enum {
    BUNDLE_INST_MAX = 1,
};

#include "../codegen_impl.h"

static void mach_dsl_init(Ctx* restrict ctx, TB_ABI abi) {
    ctx->num_regs[REG_CLASS_GPR] = 32;
    ctx->num_regs[REG_CLASS_FPR] = 32;
}
static bool mach_is_subpat(int type) {
    switch (type) {
        default: return false;
    }
}

static int mach_is_operand(int type) {
    switch (type) {
        default: return 0;
    }
}

enum {
    RULE_NONE,
};

static void mach_indent(int depth) {
    while (depth--) { printf("  "); }
}

#if 0
#define DFA_LOG(depth, n, fmt, ...) (mach_indent(depth), printf(fmt, __VA_ARGS__), printf(": "), tb_print_dumb_node(NULL, n), printf("\n"))
#define DFA_LOG2(depth, fmt, ...) (mach_indent(depth), printf(fmt, __VA_ARGS__), printf("\n"))
#else
#define DFA_LOG(depth, n, fmt, ...)
#define DFA_LOG2(depth, fmt, ...)
#endif

static int node_isel_op_group(MatchRuleID id) {
    switch (id) {
        default: return RULE_NONE;
    }
}

#define RULE(x) mach_dfa_rule_ ## x(ctx, f, n, depth)
static MatchRuleID node_isel_raw(Ctx* ctx, TB_Function* f, Set* shared, TB_Node* n, int depth) {
    MatchRuleID k;
    switch (n->type) {
    }
    return 0;
}

static MatchRule match_rules[] = {
    NULL,
};
#undef ACCEPT
