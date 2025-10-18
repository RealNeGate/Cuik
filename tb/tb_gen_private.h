#pragma once

extern const uint64_t tb_node_flags[];
extern const uint16_t* tb_node_subtypes[];

typedef enum TB_NodeFlagsEnum {
    NODE_PINNED               = 1ull << 0,
    NODE_END                  = 1ull << 1,
    NODE_TERMINATOR           = 1ull << 2,
    NODE_CTRL                 = 1ull << 3,
    NODE_FORK_CTRL            = 1ull << 4,
    NODE_EFFECT               = 1ull << 5,
    NODE_MEMORY_IN            = 1ull << 6,
    NODE_MEMORY_OUT           = 1ull << 7,
    NODE_SAFEPOINT            = 1ull << 8,
    NODE_NO_GVN               = 1ull << 9,
    NODE_COMPARE              = 1ull << 10,
} TB_NodeFlagsEnum;

static bool tb_node_is_pinned(TB_Node* n) { return tb_node_flags[n->type] & NODE_PINNED; }
static bool tb_node_is_end(TB_Node* n) { return tb_node_flags[n->type] & NODE_END; }
static bool tb_node_is_terminator(TB_Node* n) { return tb_node_flags[n->type] & NODE_TERMINATOR; }
static bool tb_node_is_ctrl(TB_Node* n) { return tb_node_flags[n->type] & NODE_CTRL; }
static bool tb_node_is_fork_ctrl(TB_Node* n) { return tb_node_flags[n->type] & NODE_FORK_CTRL; }
static bool tb_node_is_effect(TB_Node* n) { return tb_node_flags[n->type] & NODE_EFFECT; }
static bool tb_node_is_memory_in(TB_Node* n) { return tb_node_flags[n->type] & NODE_MEMORY_IN; }
static bool tb_node_is_memory_out(TB_Node* n) { return tb_node_flags[n->type] & NODE_MEMORY_OUT; }
static bool tb_node_is_safepoint(TB_Node* n) { return tb_node_flags[n->type] & NODE_SAFEPOINT; }
static bool tb_node_is_no_gvn(TB_Node* n) { return tb_node_flags[n->type] & NODE_NO_GVN; }
static bool tb_node_is_compare(TB_Node* n) { return tb_node_flags[n->type] & NODE_COMPARE; }

#define NODE_ISA(s, t) tb_node_isa((s)->type, TB_ ## t)
static bool tb_node_is_x86(TB_Node* n) { return n->type >= 110 && n->type < 154; }

#if 1 // namespace: x86
typedef enum Cond {
    O = 0,
    NO = 1,
    B = 2,
    NB = 3,
    E = 4,
    NE = 5,
    BE = 6,
    A = 7,
    S = 8,
    NS = 9,
    P = 10,
    NP = 11,
    L = 12,
    GE = 13,
    LE = 14,
    G = 15,
} Cond;

typedef struct X86MemOp {
union {
    float prob;
struct {
    int32_t disp;
    int32_t imm;
};

};

    uint8_t mode : 2;
    uint8_t scale : 2;
    uint8_t cond : 4;
    uint8_t flags;
    TB_DataType extra_dt;
} X86MemOp;

typedef struct X86Call {
    TB_NodeSafepoint super;
    TB_FunctionPrototype* proto;
} X86Call;

enum {
    OP_IMMEDIATE = 1,
    OP_INDEXED = 2,
};

enum {
    MODE_REG = 0,
    MODE_LD = 1,
    MODE_ST = 2,
};

#endif

#ifndef TB_GEN_IMPL
TB_NodeFlagsEnum tb_node_get_flags(TB_Node* n);
const char* tb_node_get_name(TB_NodeTypeEnum n_type);
size_t tb_node_extra_bytes(TB_NodeTypeEnum n_type);
bool tb_node_isa(TB_NodeTypeEnum s, TB_NodeTypeEnum t);
#else
bool tb_node_isa(TB_NodeTypeEnum s, TB_NodeTypeEnum t) {
    const uint16_t* ss = tb_node_subtypes[s];
    if (ss == NULL) {
        return t == s;
    }
    int t_depth = tb_node_subtypes[t] ? tb_node_subtypes[t][0] : 1;
    return t_depth <= ss[0] ? t == ss[t_depth] : false;
}

const uint16_t* tb_node_subtypes[TB_NODE_TYPE_MAX] = {
    [TB_BRANCH_PROJ]         = (uint16_t[]){ 2, 4, 5 },
    [TB_MACH_PROJ]           = (uint16_t[]){ 2, 4, 6 },
    [TB_NATURAL_LOOP]        = (uint16_t[]){ 2, 13, 14 },
    [TB_AFFINE_LOOP]         = (uint16_t[]){ 3, 13, 14, 15 },
    [TB_AFFINE_LATCH]        = (uint16_t[]){ 2, 18, 19 },
    [TB_SYSCALL]             = (uint16_t[]){ 2, 27, 28 },
    [TB_TAILCALL]            = (uint16_t[]){ 2, 27, 29 },
    [TB_x86_jcc]             = (uint16_t[]){ 2, 18, 115 },
    [TB_x86_call]            = (uint16_t[]){ 2, 27, 121 },
};

const uint64_t tb_node_flags[TB_NODE_TYPE_MAX] = {
    [TB_PROJ]                = NODE_PINNED,
    [TB_BRANCH_PROJ]         = NODE_PINNED,
    [TB_MACH_PROJ]           = NODE_PINNED,
    [TB_ROOT]                = NODE_END|NODE_PINNED|NODE_TERMINATOR,
    [TB_RETURN]              = NODE_END|NODE_PINNED|NODE_TERMINATOR,
    [TB_REGION]              = NODE_CTRL|NODE_PINNED,
    [TB_NATURAL_LOOP]        = NODE_CTRL|NODE_PINNED,
    [TB_AFFINE_LOOP]         = NODE_CTRL|NODE_PINNED,
    [TB_PHI]                 = NODE_PINNED,
    [TB_IF]                  = NODE_CTRL|NODE_FORK_CTRL|NODE_PINNED|NODE_TERMINATOR,
    [TB_AFFINE_LATCH]        = NODE_CTRL|NODE_FORK_CTRL|NODE_PINNED|NODE_TERMINATOR,
    [TB_NEVER_BRANCH]        = NODE_CTRL|NODE_FORK_CTRL|NODE_PINNED|NODE_TERMINATOR,
    [TB_DEBUGBREAK]          = NODE_CTRL|NODE_EFFECT|NODE_MEMORY_IN|NODE_PINNED,
    [TB_TRAP]                = NODE_CTRL|NODE_EFFECT|NODE_END|NODE_MEMORY_IN|NODE_PINNED|NODE_TERMINATOR,
    [TB_UNREACHABLE]         = NODE_CTRL|NODE_EFFECT|NODE_END|NODE_MEMORY_IN|NODE_PINNED|NODE_TERMINATOR,
    [TB_DEAD_STORE]          = NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED,
    [TB_ENTRY_FORK]          = NODE_CTRL|NODE_FORK_CTRL|NODE_PINNED|NODE_TERMINATOR,
    [TB_CALL]                = NODE_CTRL|NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED|NODE_SAFEPOINT,
    [TB_SYSCALL]             = NODE_CTRL|NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED|NODE_SAFEPOINT,
    [TB_TAILCALL]            = NODE_CTRL|NODE_EFFECT|NODE_END|NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED|NODE_SAFEPOINT,
    [TB_DEBUG_LOCATION]      = NODE_CTRL|NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED,
    [TB_LOAD]                = NODE_MEMORY_IN,
    [TB_STORE]               = NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_MEMCPY]              = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_MEMSET]              = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_LOAD]         = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_XCHG]         = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_ADD]          = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_AND]          = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_XOR]          = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_OR]           = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_PTROFF]       = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_ATOMIC_CAS]          = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_HARD_BARRIER]        = NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT,
    [TB_LOCAL]               = NODE_NO_GVN,
    [TB_CMP_EQ]              = NODE_COMPARE,
    [TB_CMP_NE]              = NODE_COMPARE,
    [TB_CMP_ULT]             = NODE_COMPARE,
    [TB_CMP_ULE]             = NODE_COMPARE,
    [TB_CMP_SLT]             = NODE_COMPARE,
    [TB_CMP_SLE]             = NODE_COMPARE,
    [TB_CMP_FLT]             = NODE_COMPARE,
    [TB_CMP_FLE]             = NODE_COMPARE,
    [TB_BLACKHOLE]           = NODE_CTRL|NODE_EFFECT,
    [TB_MACH_FRAME_PTR]      = NODE_PINNED,
    [TB_MACH_TEMP]           = NODE_NO_GVN,
    [TB_x86_jcc]             = NODE_CTRL|NODE_FORK_CTRL|NODE_PINNED|NODE_TERMINATOR,
    [TB_x86_jmp_MULTI]       = NODE_CTRL|NODE_FORK_CTRL|NODE_TERMINATOR,
    [TB_x86_call]            = NODE_CTRL|NODE_EFFECT|NODE_MEMORY_IN|NODE_MEMORY_OUT|NODE_PINNED|NODE_SAFEPOINT,
};

TB_NodeFlagsEnum tb_node_get_flags(TB_Node* n) {
    TB_ASSERT(n->type < TB_NODE_TYPE_MAX);
    return tb_node_flags[n->type];
}

const char* tb_node_get_name(TB_NodeTypeEnum n_type) {
    switch (n_type) {
        case TB_NULL: return "null";
        case TB_ICONST: return "iconst";
        case TB_F32CONST: return "f32const";
        case TB_F64CONST: return "f64const";
        case TB_PROJ: return "proj";
        case TB_BRANCH_PROJ: return "branch_proj";
        case TB_MACH_PROJ: return "mach_proj";
        case TB_POISON: return "poison";
        case TB_CYCLE_COUNTER: return "cycle_counter";
        case TB_PREFETCH: return "prefetch";
        case TB_SYMBOL_TABLE: return "symbol_table";
        case TB_ROOT: return "root";
        case TB_RETURN: return "return";
        case TB_REGION: return "region";
        case TB_NATURAL_LOOP: return "natural_loop";
        case TB_AFFINE_LOOP: return "affine_loop";
        case TB_PHI: return "phi";
        case TB_BRANCH: return "branch";
        case TB_IF: return "if";
        case TB_AFFINE_LATCH: return "affine_latch";
        case TB_NEVER_BRANCH: return "never_branch";
        case TB_DEBUGBREAK: return "debugbreak";
        case TB_TRAP: return "trap";
        case TB_UNREACHABLE: return "unreachable";
        case TB_DEAD: return "dead";
        case TB_DEAD_STORE: return "dead_store";
        case TB_ENTRY_FORK: return "entry_fork";
        case TB_CALL: return "call";
        case TB_SYSCALL: return "syscall";
        case TB_TAILCALL: return "tailcall";
        case TB_DEBUG_LOCATION: return "debug_location";
        case TB_CALLGRAPH: return "callgraph";
        case TB_DEBUG_SCOPES: return "debug_scopes";
        case TB_SPLITMEM: return "splitmem";
        case TB_MERGEMEM: return "mergemem";
        case TB_LOAD: return "load";
        case TB_STORE: return "store";
        case TB_MEMCPY: return "memcpy";
        case TB_MEMSET: return "memset";
        case TB_ATOMIC_LOAD: return "atomic_load";
        case TB_ATOMIC_XCHG: return "atomic_xchg";
        case TB_ATOMIC_ADD: return "atomic_add";
        case TB_ATOMIC_AND: return "atomic_and";
        case TB_ATOMIC_XOR: return "atomic_xor";
        case TB_ATOMIC_OR: return "atomic_or";
        case TB_ATOMIC_PTROFF: return "atomic_ptroff";
        case TB_ATOMIC_CAS: return "atomic_cas";
        case TB_HARD_BARRIER: return "hard_barrier";
        case TB_LOCAL: return "local";
        case TB_SYMBOL: return "symbol";
        case TB_PTR_OFFSET: return "ptr_offset";
        case TB_TRUNCATE: return "truncate";
        case TB_FLOAT_TRUNC: return "float_trunc";
        case TB_FLOAT_EXT: return "float_ext";
        case TB_SIGN_EXT: return "sign_ext";
        case TB_ZERO_EXT: return "zero_ext";
        case TB_UINT2FLOAT: return "uint2float";
        case TB_FLOAT2UINT: return "float2uint";
        case TB_INT2FLOAT: return "int2float";
        case TB_FLOAT2INT: return "float2int";
        case TB_BITCAST: return "bitcast";
        case TB_SELECT: return "select";
        case TB_BSWAP: return "bswap";
        case TB_CLZ: return "clz";
        case TB_CTZ: return "ctz";
        case TB_POPCNT: return "popcnt";
        case TB_FNEG: return "fneg";
        case TB_AND: return "and";
        case TB_OR: return "or";
        case TB_XOR: return "xor";
        case TB_ADD: return "add";
        case TB_SUB: return "sub";
        case TB_MUL: return "mul";
        case TB_SHL: return "shl";
        case TB_SHR: return "shr";
        case TB_SAR: return "sar";
        case TB_ROL: return "rol";
        case TB_ROR: return "ror";
        case TB_UDIV: return "udiv";
        case TB_SDIV: return "sdiv";
        case TB_UMOD: return "umod";
        case TB_SMOD: return "smod";
        case TB_FADD: return "fadd";
        case TB_FSUB: return "fsub";
        case TB_FMUL: return "fmul";
        case TB_FDIV: return "fdiv";
        case TB_FMIN: return "fmin";
        case TB_FMAX: return "fmax";
        case TB_CMP_EQ: return "cmp_eq";
        case TB_CMP_NE: return "cmp_ne";
        case TB_CMP_ULT: return "cmp_ult";
        case TB_CMP_ULE: return "cmp_ule";
        case TB_CMP_SLT: return "cmp_slt";
        case TB_CMP_SLE: return "cmp_sle";
        case TB_CMP_FLT: return "cmp_flt";
        case TB_CMP_FLE: return "cmp_fle";
        case TB_FSQRT: return "fsqrt";
        case TB_FRAME_PTR: return "frame_ptr";
        case TB_BLACKHOLE: return "blackhole";
        case TB_SMULPAIR: return "smulpair";
        case TB_UMULPAIR: return "umulpair";
        case TB_VBROADCAST: return "vbroadcast";
        case TB_VSHUFFLE: return "vshuffle";
        case TB_VA_START: return "va_start";
        case TB_MACH_COPY: return "mach_copy";
        case TB_MACH_JUMP: return "mach_jump";
        case TB_MACH_FRAME_PTR: return "mach_frame_ptr";
        case TB_MACH_JIT_THREAD_PTR: return "mach_jit_thread_ptr";
        case TB_MACH_SYMBOL: return "mach_symbol";
        case TB_MACH_TEMP: return "mach_temp";
        case TB_x86_MEMORY: return "x86_memory";
        case TB_x86_COND: return "x86_cond";
        case TB_x86_cmp: return "x86_cmp";
        case TB_x86_test: return "x86_test";
        case TB_x86_ucomi: return "x86_ucomi";
        case TB_x86_jcc: return "x86_jcc";
        case TB_x86_jmp_MULTI: return "x86_jmp_multi";
        case TB_x86_setcc: return "x86_setcc";
        case TB_x86_cmovcc: return "x86_cmovcc";
        case TB_x86_bt: return "x86_bt";
        case TB_x86_adc: return "x86_adc";
        case TB_x86_call: return "x86_call";
        case TB_x86_shlx: return "x86_shlx";
        case TB_x86_shrx: return "x86_shrx";
        case TB_x86_sarx: return "x86_sarx";
        case TB_x86_mov: return "x86_mov";
        case TB_x86_lea: return "x86_lea";
        case TB_x86_add: return "x86_add";
        case TB_x86_or: return "x86_or";
        case TB_x86_and: return "x86_and";
        case TB_x86_sub: return "x86_sub";
        case TB_x86_xor: return "x86_xor";
        case TB_x86_imul: return "x86_imul";
        case TB_x86_shl: return "x86_shl";
        case TB_x86_shr: return "x86_shr";
        case TB_x86_sar: return "x86_sar";
        case TB_x86_rol: return "x86_rol";
        case TB_x86_ror: return "x86_ror";
        case TB_x86_div: return "x86_div";
        case TB_x86_idiv: return "x86_idiv";
        case TB_x86_vmov: return "x86_vmov";
        case TB_x86_vzero: return "x86_vzero";
        case TB_x86_vadd: return "x86_vadd";
        case TB_x86_vsub: return "x86_vsub";
        case TB_x86_vmul: return "x86_vmul";
        case TB_x86_vdiv: return "x86_vdiv";
        case TB_x86_vmax: return "x86_vmax";
        case TB_x86_vmin: return "x86_vmin";
        case TB_x86_movsx8: return "x86_movsx8";
        case TB_x86_movzx8: return "x86_movzx8";
        case TB_x86_movsx16: return "x86_movsx16";
        case TB_x86_movzx16: return "x86_movzx16";
        case TB_x86_movsx32: return "x86_movsx32";
        case TB_x86_vxor: return "x86_vxor";
        default: tb_todo(); return NULL;
    }
}

size_t tb_node_extra_bytes(TB_NodeTypeEnum n_type) {
    switch (n_type) {
        case TB_NULL: return 0;
        case TB_ICONST: return sizeof(TB_NodeInt);
        case TB_F32CONST: return sizeof(TB_NodeFloat32);
        case TB_F64CONST: return sizeof(TB_NodeFloat64);
        case TB_PROJ: return sizeof(TB_NodeProj);
        case TB_BRANCH_PROJ: return sizeof(TB_NodeBranchProj);
        case TB_MACH_PROJ: return 0;
        case TB_POISON: return 0;
        case TB_CYCLE_COUNTER: return 0;
        case TB_PREFETCH: return sizeof(TB_NodePrefetch);
        case TB_SYMBOL_TABLE: return sizeof(TB_NodeSymbolTable);
        case TB_ROOT: return 0;
        case TB_RETURN: return 0;
        case TB_REGION: return 0;
        case TB_NATURAL_LOOP: return 0;
        case TB_AFFINE_LOOP: return 0;
        case TB_PHI: return 0;
        case TB_BRANCH: return sizeof(TB_NodeBranch);
        case TB_IF: return sizeof(TB_NodeIf);
        case TB_AFFINE_LATCH: return 0;
        case TB_NEVER_BRANCH: return 0;
        case TB_DEBUGBREAK: return 0;
        case TB_TRAP: return 0;
        case TB_UNREACHABLE: return 0;
        case TB_DEAD: return 0;
        case TB_DEAD_STORE: return sizeof(TB_NodeMemAccess);
        case TB_ENTRY_FORK: return 0;
        case TB_CALL: return sizeof(TB_NodeCall);
        case TB_SYSCALL: return 0;
        case TB_TAILCALL: return 0;
        case TB_DEBUG_LOCATION: return sizeof(TB_NodeDbgLoc);
        case TB_CALLGRAPH: return 0;
        case TB_DEBUG_SCOPES: return 0;
        case TB_SPLITMEM: return 0;
        case TB_MERGEMEM: return 0;
        case TB_LOAD: return sizeof(TB_NodeMemAccess);
        case TB_STORE: return sizeof(TB_NodeMemAccess);
        case TB_MEMCPY: return sizeof(TB_NodeMemAccess);
        case TB_MEMSET: return sizeof(TB_NodeMemAccess);
        case TB_ATOMIC_LOAD: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_XCHG: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_ADD: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_AND: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_XOR: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_OR: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_PTROFF: return sizeof(TB_NodeAtomic);
        case TB_ATOMIC_CAS: return sizeof(TB_NodeAtomic);
        case TB_HARD_BARRIER: return 0;
        case TB_LOCAL: return sizeof(TB_NodeLocal);
        case TB_SYMBOL: return sizeof(TB_NodeSymbol);
        case TB_PTR_OFFSET: return 0;
        case TB_TRUNCATE: return 0;
        case TB_FLOAT_TRUNC: return 0;
        case TB_FLOAT_EXT: return 0;
        case TB_SIGN_EXT: return 0;
        case TB_ZERO_EXT: return 0;
        case TB_UINT2FLOAT: return 0;
        case TB_FLOAT2UINT: return 0;
        case TB_INT2FLOAT: return 0;
        case TB_FLOAT2INT: return 0;
        case TB_BITCAST: return 0;
        case TB_SELECT: return 0;
        case TB_BSWAP: return 0;
        case TB_CLZ: return 0;
        case TB_CTZ: return 0;
        case TB_POPCNT: return 0;
        case TB_FNEG: return 0;
        case TB_AND: return sizeof(TB_NodeBinopInt);
        case TB_OR: return sizeof(TB_NodeBinopInt);
        case TB_XOR: return sizeof(TB_NodeBinopInt);
        case TB_ADD: return sizeof(TB_NodeBinopInt);
        case TB_SUB: return sizeof(TB_NodeBinopInt);
        case TB_MUL: return sizeof(TB_NodeBinopInt);
        case TB_SHL: return sizeof(TB_NodeBinopInt);
        case TB_SHR: return sizeof(TB_NodeBinopInt);
        case TB_SAR: return sizeof(TB_NodeBinopInt);
        case TB_ROL: return sizeof(TB_NodeBinopInt);
        case TB_ROR: return sizeof(TB_NodeBinopInt);
        case TB_UDIV: return sizeof(TB_NodeBinopInt);
        case TB_SDIV: return sizeof(TB_NodeBinopInt);
        case TB_UMOD: return sizeof(TB_NodeBinopInt);
        case TB_SMOD: return sizeof(TB_NodeBinopInt);
        case TB_FADD: return 0;
        case TB_FSUB: return 0;
        case TB_FMUL: return 0;
        case TB_FDIV: return 0;
        case TB_FMIN: return 0;
        case TB_FMAX: return 0;
        case TB_CMP_EQ: return sizeof(TB_NodeCompare);
        case TB_CMP_NE: return sizeof(TB_NodeCompare);
        case TB_CMP_ULT: return sizeof(TB_NodeCompare);
        case TB_CMP_ULE: return sizeof(TB_NodeCompare);
        case TB_CMP_SLT: return sizeof(TB_NodeCompare);
        case TB_CMP_SLE: return sizeof(TB_NodeCompare);
        case TB_CMP_FLT: return sizeof(TB_NodeCompare);
        case TB_CMP_FLE: return sizeof(TB_NodeCompare);
        case TB_FSQRT: return 0;
        case TB_FRAME_PTR: return 0;
        case TB_BLACKHOLE: return 0;
        case TB_SMULPAIR: return 0;
        case TB_UMULPAIR: return 0;
        case TB_VBROADCAST: return 0;
        case TB_VSHUFFLE: return 0;
        case TB_VA_START: return 0;
        case TB_MACH_COPY: return sizeof(TB_NodeMachCopy);
        case TB_MACH_JUMP: return 0;
        case TB_MACH_FRAME_PTR: return 0;
        case TB_MACH_JIT_THREAD_PTR: return 0;
        case TB_MACH_SYMBOL: return sizeof(TB_NodeMachSymbol);
        case TB_MACH_TEMP: return sizeof(TB_NodeMachTemp);
        case TB_x86_MEMORY: return sizeof(X86MemOp);
        case TB_x86_COND: return sizeof(X86MemOp);
        case TB_x86_cmp: return sizeof(X86MemOp);
        case TB_x86_test: return sizeof(X86MemOp);
        case TB_x86_ucomi: return sizeof(X86MemOp);
        case TB_x86_jcc: return sizeof(X86MemOp);
        case TB_x86_jmp_MULTI: return sizeof(X86MemOp);
        case TB_x86_setcc: return sizeof(X86MemOp);
        case TB_x86_cmovcc: return sizeof(X86MemOp);
        case TB_x86_bt: return sizeof(X86MemOp);
        case TB_x86_adc: return sizeof(X86MemOp);
        case TB_x86_call: return sizeof(X86Call);
        case TB_x86_shlx: return sizeof(X86MemOp);
        case TB_x86_shrx: return sizeof(X86MemOp);
        case TB_x86_sarx: return sizeof(X86MemOp);
        case TB_x86_mov: return sizeof(X86MemOp);
        case TB_x86_lea: return sizeof(X86MemOp);
        case TB_x86_add: return sizeof(X86MemOp);
        case TB_x86_or: return sizeof(X86MemOp);
        case TB_x86_and: return sizeof(X86MemOp);
        case TB_x86_sub: return sizeof(X86MemOp);
        case TB_x86_xor: return sizeof(X86MemOp);
        case TB_x86_imul: return sizeof(X86MemOp);
        case TB_x86_shl: return sizeof(X86MemOp);
        case TB_x86_shr: return sizeof(X86MemOp);
        case TB_x86_sar: return sizeof(X86MemOp);
        case TB_x86_rol: return sizeof(X86MemOp);
        case TB_x86_ror: return sizeof(X86MemOp);
        case TB_x86_div: return sizeof(X86MemOp);
        case TB_x86_idiv: return sizeof(X86MemOp);
        case TB_x86_vmov: return sizeof(X86MemOp);
        case TB_x86_vzero: return sizeof(X86MemOp);
        case TB_x86_vadd: return sizeof(X86MemOp);
        case TB_x86_vsub: return sizeof(X86MemOp);
        case TB_x86_vmul: return sizeof(X86MemOp);
        case TB_x86_vdiv: return sizeof(X86MemOp);
        case TB_x86_vmax: return sizeof(X86MemOp);
        case TB_x86_vmin: return sizeof(X86MemOp);
        case TB_x86_movsx8: return sizeof(X86MemOp);
        case TB_x86_movzx8: return sizeof(X86MemOp);
        case TB_x86_movsx16: return sizeof(X86MemOp);
        case TB_x86_movzx16: return sizeof(X86MemOp);
        case TB_x86_movsx32: return sizeof(X86MemOp);
        case TB_x86_vxor: return sizeof(X86MemOp);
        default: tb_todo(); return 0;
    }
}
#endif