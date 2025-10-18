(namespace x86

    (arch_features
        // (_parse-prefix tb_rv_parse_feature_prefix)
        //                ^^^^^^
        //             char* example(char* str, TB_RV_FeatureSet* feats) {
        //               if (*str == 'i' && ++str) TB_RV_FeatureSet__set(feats, TB_RV_FEATURE_I);
        //               if (*str == 'm' && ++str) TB_RV_FeatureSet__set(feats, TB_RV_FEATURE_M);
        //               if (*str == 'a' && ++str) TB_RV_FeatureSet__set(feats, TB_RV_FEATURE_A);
        //               if (*str == 'c' && ++str) TB_RV_FeatureSet__set(feats, TB_RV_FEATURE_C);
        //               // ...
        //               return str;
        //             }
        // (a)            feature a
        // (a => x y z)   feature a, which implies x, y, z

        (sse2)
        (sse3 => sse2)
        (sse41 => sse3)
        (sse42 => sse3)

        (popcnt)
        (lzcnt)

        (clmul)
        (f16c)

        (bmi1)
        (bmi2)

        (avx)
        (avx2 => avx)

        // profiles
        ("x86_64-v1" => sse2)
        ("x86_64-v2" => "x86_64-v1" sse3 sse41 sse42 popcnt)
        ("x86_64-v3" => "x86_64-v2" avx avx2 bmi1 bmi2 lzcnt)
        ("x86_64-v4" => "x86_64-v3")
    )

    // *******************************
    // registers
    // *******************************
    (reg_class "GPR"
        RAX RCX RDX RBX RSP RBP RSI RDI
        R8  R9  R10 R11 R12 R13 R14 R15
    )

    (reg_class "XMM"
        XMM0 XMM1 XMM2  XMM3  XMM4  XMM5  XMM6  XMM7
        XMM8 XMM9 XMM10 XMM11 XMM12 XMM13 XMM14 XMM15
    )

    (reg_class "FLAGS")

    (enum Cond
        O NO B NB E NE BE A
        S NS P NP L GE LE G
    )

    // node with X86MemOp (mov, add, and...) will have this layout of inputs:
    //   [1] mem
    //   [2] base (or first src)
    //   [3] idx
    //   [4] val (only if flags' HAS_IMMEDIATE is unset)
    (struct X86MemOp
        (___ (union ___
            (prob float)
            (___ (struct ___
                (disp int32_t)
                (imm  int32_t)
            ))
        ))

        (mode  (bits uint8_t 2))
        (scale (bits uint8_t 2))
        (cond  (bits uint8_t 4))
        (flags uint8_t)

        (extra_dt TB_DataType)
    )

    (struct X86Call
        (super TB_NodeSafepoint super)
        (proto (ptr TB_FunctionPrototype))
    )

    // *******************************
    // memory operands
    // *******************************
    // Unnamed C enum
    (enum ___
        (OP_IMMEDIATE 1)
        (OP_INDEXED   2)
    )

    // Unnamed C enum
    (enum ___ MODE_REG MODE_LD MODE_ST)

    (node MEMORY extra=X86MemOp)

    (op_group MEMORY x86_MEMORY)
    (op_group COND   x86_COND)
    (op_group IARITH x86_add x86_sub x86_and x86_xor x86_or)

    // [base + index]
    pat (PTR_OFFSET ___ $base $index)
    => (x86_MEMORY flags=OP_INDEXED $base $index scale=0)

    // [base + disp]
    pat (PTR_OFFSET ___ $base ($imm: ICONST ...))
    where "fits_into_int32(TB_TYPE_I64, $imm)"
    => (x86_MEMORY $base disp="as_int32($imm)")

    // [base + index + disp]
    pat (PTR_OFFSET ___ $base (ADD ___ $index ($imm: ICONST ...)))
    where "fits_into_int32(TB_TYPE_I64, $imm)"
    => (x86_MEMORY $base $index flags=OP_INDEXED disp="as_int32($imm)")

    // [base + index*scale + disp]
    pat (PTR_OFFSET ___ $base (ADD ___ (SHL ___ $index ($scale: ICONST ...)) ($disp: ICONST ...)))
    where "fits_into_scale($scale) && fits_into_int32(TB_TYPE_I64, $disp)"
    => (x86_MEMORY flags=OP_INDEXED $base $index scale="as_int32($scale)" disp="as_int32($disp)")

    // [base + index*scale]
    pat (PTR_OFFSET ___ $base (SHL ___ $index ($scale: ICONST ...)))
    where "fits_into_scale($scale)"
    => (x86_MEMORY flags=OP_INDEXED $base $index scale="as_int32($scale)")

    // [base + index*scale + disp]
    pat (PTR_OFFSET ___ $base (ADD ___ (SHL ___ $index ($scale: ICONST ...)) ($disp: ICONST ...)))
    where "fits_into_scale($scale) && fits_into_int32(TB_TYPE_I64, $disp)"
    => (x86_MEMORY flags=OP_INDEXED $base $index scale="as_int32($scale)" disp="as_int32($disp)")

    pat (LOCAL $root stack_pos=$disp) => (x86_MEMORY "ctx->frame_ptr" disp=$disp)

    // [symbol]
    pat (SYMBOL $root sym=$sym) => (x86_MEMORY (MACH_SYMBOL $root sym=$sym))

    // [symbol + offset]
    pat (PTR_OFFSET ___ (SYMBOL $root sym=$sym) $offset)
    => (x86_MEMORY (x86_lea dt=TB_TYPE_PTR ___ ___ (MACH_SYMBOL $root sym=$sym) mode=MODE_LD) $offset flags=OP_INDEXED)

    // [symbol + disp]
    pat (PTR_OFFSET ___ (SYMBOL $root sym=$sym) ($disp: ICONST ...))
    where "fits_into_int32(TB_TYPE_I64, $disp)"
    => (x86_MEMORY (MACH_SYMBOL $root sym=$sym) disp="as_int32($disp)")

    // using an lea as a memory operand is kinda useful... swag :p
    subpat (x86_lea ___ ___ $base disp=$disp) => (x86_MEMORY $base disp=$disp)
    subpat (x86_lea ___ ___ $base $index scale=$scale disp=$disp flags=$flags) where "$flags==OP_INDEXED" => (x86_MEMORY $base $index flags=OP_INDEXED scale=$scale disp=$disp)

    // [rsp + offset]
    pat (PTR_OFFSET ___ (MEMORY (MACH_FRAME_PTR ...) flags=$flags disp=$disp) ($addend: ICONST ...))
    => (x86_MEMORY "ctx->frame_ptr" flags=$flags disp="$disp + as_int32($addend)")

    // *******************************
    // FLAGS
    // *******************************
    (node COND extra=X86MemOp)
    (node cmp extra=X86MemOp)
    (node test extra=X86MemOp)
    (node ucomi extra=X86MemOp)

    // compares
    #define CMP_OP(op_name, cc) \
    pat (op_name cmp_dt=$dt ___ $lhs $rhs) where "TB_IS_BOOL_INT_PTR($dt)" => (x86_COND dt=TB_TYPE_I8 (x86_cmp dt=TB_TYPE_I64 ___ ___ $rhs $lhs extra_dt=$dt) cond=cc) \
    pat (op_name cmp_dt=$dt ___ $lhs ($rhs: ICONST ...)) where "TB_IS_BOOL_INT_PTR($dt) && fits_into_int32($dt, $rhs)" => (x86_COND dt=TB_TYPE_I8 (x86_cmp dt=TB_TYPE_I64 ___ ___ $lhs flags="OP_IMMEDIATE" imm="as_int32($rhs)" extra_dt=$dt) cond=cc)

    CMP_OP(CMP_SLE, LE)
    CMP_OP(CMP_SLT, L)
    CMP_OP(CMP_ULE, BE)
    CMP_OP(CMP_ULT, B)
    CMP_OP(CMP_EQ,  E)
    CMP_OP(CMP_NE,  NE)

    pat (CMP_FLT cmp_dt=$dt ___ $lhs $rhs) => (x86_COND dt=TB_TYPE_I8 (x86_ucomi dt=TB_TYPE_I64 ___ ___ $rhs $lhs extra_dt=$dt) cond=B)
    pat (CMP_FLE cmp_dt=$dt ___ $lhs $rhs) => (x86_COND dt=TB_TYPE_I8 (x86_ucomi dt=TB_TYPE_I64 ___ ___ $rhs $lhs extra_dt=$dt) cond=BE)
    pat (CMP_EQ  cmp_dt=$dt ___ $lhs $rhs) where "TB_IS_FLOAT_TYPE($dt)" => (x86_COND dt=TB_TYPE_I8 (x86_ucomi dt=TB_TYPE_I64 ___ ___ $rhs $lhs extra_dt=$dt) cond=E)
    pat (CMP_NE  cmp_dt=$dt ___ $lhs $rhs) where "TB_IS_FLOAT_TYPE($dt)" => (x86_COND dt=TB_TYPE_I8 (x86_ucomi dt=TB_TYPE_I64 ___ ___ $rhs $lhs extra_dt=$dt) cond=NE)

    pat (CMP_EQ cmp_dt=$dt ___ $lhs ($rhs: ICONST ...)) where "TB_IS_INT_OR_PTR($dt) && TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0" => (x86_COND dt=TB_TYPE_I8 (x86_test dt=TB_TYPE_I64 ___ ___ $lhs $lhs extra_dt=$dt) cond=E)
    pat (CMP_NE cmp_dt=$dt ___ $lhs ($rhs: ICONST ...)) where "TB_IS_INT_OR_PTR($dt) && TB_NODE_GET_EXTRA_T($rhs, TB_NodeInt)->value == 0" => (x86_COND dt=TB_TYPE_I8 (x86_test dt=TB_TYPE_I64 ___ ___ $lhs $lhs extra_dt=$dt) cond=NE)

    (node jcc parent=IF extra=X86MemOp)
    (node jmp_MULTI CTRL TERMINATOR FORK_CTRL extra=X86MemOp)

    /* canonicalize the if-shaped branches */
    pat (IF ...) => "isel_if_branch(ctx, f, n)"
    pat (AFFINE_LATCH ...) => "isel_if_branch(ctx, f, n)"

    /* standard two-way branches */
    pat (IF dt=$dt $ctrl (COND $cmp cond=$cond) prob=$prob) => (x86_jcc dt=TB_TYPE_TUPLE $ctrl $cmp cond=$cond prob=$prob)
    pat (IF dt=$dt $ctrl $x prob=$prob) => (x86_jcc dt=TB_TYPE_TUPLE $ctrl (x86_test dt=TB_TYPE_I64 ___ ___ $x $x extra_dt="n->inputs[1]->dt") cond=NE prob=$prob)
    pat (IF dt=$dt $ctrl (LOAD $ctrl2 $mem ($x: MEMORY ... flags=$flags)) prob=$prob) => (x86_jcc dt=TB_TYPE_TUPLE $ctrl (x86_cmp dt=TB_TYPE_I64 ___ ___ $x mode="MODE_LD" flags="$flags | OP_IMMEDIATE" extra_dt="n->inputs[1]->dt") cond=NE prob=$prob)
    pat (IF dt=$dt $ctrl (AND ___ $x $y) prob=$prob) => (x86_jcc dt=TB_TYPE_TUPLE $ctrl (x86_test dt=TB_TYPE_I64 ___ ___ $x $y extra_dt="n->inputs[1]->dt") cond=NE prob=$prob)
    /* multi-way branches */ \
    pat (BRANCH dt=$dt $ctrl $x) => "isel_multi_way_branch(ctx, f, n)"

    // if we don't get rid of the "condition" by merging it with something
    // else then it's being directly used.
    (node setcc extra=X86MemOp)
    pat (x86_COND $cmp cond=$cond) => (x86_setcc dt=TB_TYPE_I8 ___ ___ $cmp cond=$cond)

    (node cmovcc extra=X86MemOp)
    pat (SELECT dt=$dt ___ $pred $a $b) => (x86_cmovcc dt=$dt ___ ___ $b $a (x86_test dt=TB_TYPE_I64 ___ ___ $pred $pred extra_dt="n->inputs[1]->dt") cond=NE)
    pat (SELECT dt=$dt ___ (COND $cmp cond=$cond) $a $b) => (x86_cmovcc dt=$dt ___ ___ $b $a $cmp cond=$cond)

    (node bt extra=X86MemOp)
    pat (AND dt=$dt ___ (SHL ___ ($con: ICONST ...) $y) $x) where "as_int32($con) == 1" => (x86_COND dt=TB_TYPE_I8 (x86_bt dt=TB_TYPE_I64 ___ ___ $y $x extra_dt=$dt) cond=B)

    (node adc extra=X86MemOp)
    pat (x86_cmovcc dt=$dt ___ ___ $x (ADD ___ $y ($con: ICONST ...)) $pred) where "as_int32($con) == 1 && TB_NODE_GET_EXTRA_T(n, X86MemOp)->cond == B" => (x86_adc dt=$dt ___ ___ $x $pred flags="OP_IMMEDIATE" imm="0")

    // IARITH X, Y
    // set,s A
    /* subpat (SHR ___ ($src: IARITH) ($con: ICONST ...))
    where "as_int32($con) == 63"
    => (x86_COND dt=TB_TYPE_I8 (MACH_PROJ dt=TB_TYPE_I64 $src def="ctx->normie_mask[REG_CLASS_FLAGS]" index=0) cond=cc)
    */

    // *******************************
    // misc
    // *******************************
    (node call parent=CALL extra=X86Call)
    pat (CALL $ctrl $mem (SYMBOL $root sym=$sym) $REST) => (x86_call dt=TB_TYPE_TUPLE $ctrl $mem (MACH_SYMBOL $root sym=$sym) $REST proto="TB_NODE_GET_EXTRA_T(n, TB_NodeCall)->proto")
    pat (CALL $ctrl $mem $target $REST) => (x86_call dt=TB_TYPE_TUPLE $ctrl $mem $target $REST proto="TB_NODE_GET_EXTRA_T(n, TB_NodeCall)->proto")

    pat (VA_START ...) => "isel_va_start(ctx, f, n)"

    // *******************************
    // BMI2
    // *******************************
    #define BMI2_OP(op_name, mnemonic) \
    (node mnemonic extra=X86MemOp) \
    pat (op_name dt=$dt ___ $lhs $rhs) require "bmi2" => (x86_ ## mnemonic dt=$dt ___ ___ $lhs $rhs) \
    pat (op_name dt=$dt ___ (LOAD $ctrl $mem ($lhs: MEMORY ...)) $rhs) require "bmi2" => (x86_ ## mnemonic dt=$dt $ctrl $mem $lhs $rhs mode=MODE_LD) \

    BMI2_OP(SHL, shlx)
    BMI2_OP(SHR, shrx)
    BMI2_OP(SAR, sarx)

    // *******************************
    // integer ops
    // *******************************
    (node mov extra=X86MemOp)
    (node lea extra=X86MemOp)

    pat (VA_START ...) => "isel_va_start(ctx, f, n)"
    pat ($lhs: x86_MEMORY ...) => (x86_lea dt=TB_TYPE_PTR ___ ___ $lhs mode=MODE_LD)

    pat (LOAD dt=$dt $ctrl $mem $addr) where "TB_IS_INT_OR_PTR0($dt)" => (x86_mov dt=$dt $ctrl $mem $addr mode=MODE_LD)
    pat (LOAD dt=$dt $ctrl $mem ($addr: MEMORY ...)) where "TB_IS_INT_OR_PTR0($dt)" => (x86_mov dt=$dt $ctrl $mem $addr mode=MODE_LD)

    // Load barriers
    pat (LOAD dt=$dt $ctrl $mem $addr)
    where "$dt.type == TB_TAG_PTR && $dt.elem_or_addrspace != 0"
    => (x86_mov dt=$dt $ctrl $mem $addr (x86_mov dt=TB_TYPE_PTR ___ ___ (MACH_SYMBOL "f->root_node" sym="ctx->module->ccgc.phase_control") mode=MODE_LD) mode=MODE_LD)

    pat (LOAD dt=$dt $ctrl $mem ($addr: MEMORY ...))
    where "$dt.type == TB_TAG_PTR && $dt.elem_or_addrspace != 0"
    => (x86_mov dt=$dt $ctrl $mem $addr (x86_mov dt=TB_TYPE_PTR ___ ___ (MACH_SYMBOL "f->root_node" sym="ctx->module->ccgc.phase_control") mode=MODE_LD) mode=MODE_LD)

    pat (STORE $ctrl $mem $addr $val) where "TB_IS_INT_OR_PTR($val->dt)" => (x86_mov dt=TB_TYPE_MEMORY $ctrl $mem $addr $val mode=MODE_ST extra_dt="n->inputs[3]->dt")
    pat (STORE $ctrl $mem ($lhs: MEMORY ...) $val) where "TB_IS_INT_OR_PTR($val->dt)" => (x86_mov dt=TB_TYPE_MEMORY $ctrl $mem $lhs $val mode=MODE_ST extra_dt="n->inputs[3]->dt")
    pat (STORE $ctrl $mem ($lhs: MEMORY ... flags=$flags) ($val: ICONST ...)) where "TB_IS_INT_OR_PTR($val->dt) && fits_into_int32($val->dt, $val)" => (x86_mov dt=TB_TYPE_MEMORY $ctrl $mem $lhs flags="$flags | OP_IMMEDIATE" mode=MODE_ST extra_dt="n->inputs[3]->dt" imm="as_int32($val)")

    #define NORMIE_OP(op_name, mnemonic) \
    (node mnemonic extra=X86MemOp) \
    pat (op_name dt=$dt ___ $lhs $rhs) => (x86_ ## mnemonic dt=$dt ___ ___ $rhs $lhs) \
    pat (op_name dt=$dt ___ $lhs (LOAD $ctrl $mem ($rhs: MEMORY ...))) => (x86_ ## mnemonic dt=$dt $ctrl $mem $rhs $lhs mode=MODE_LD) \
    pat (op_name dt=$dt ___ $lhs ($rhs: ICONST ...)) where "fits_into_int32($dt, $rhs)" => (x86_ ## mnemonic dt=$dt ___ ___ $lhs flags="OP_IMMEDIATE" imm="as_int32($rhs)") \
    pat (op_name dt=$dt ___ $lhs ($rhs: ICONST ...)) => (x86_ ## mnemonic dt=$dt ___ ___ $rhs $lhs)

    #define NORMIE_OP8(op_name, mnemonic) \
    (node mnemonic extra=X86MemOp) \
    pat (op_name dt=$dt ___ $lhs $rhs) => (x86_ ## mnemonic dt=$dt ___ ___ $rhs $lhs) \
    pat (op_name dt=$dt ___ $lhs ($rhs: ICONST ...)) where "fits_into_uint8($dt, $rhs)" => (x86_ ## mnemonic dt=$dt ___ ___ $lhs flags="OP_IMMEDIATE" imm="as_int32($rhs)")

    NORMIE_OP(ADD, add)
    NORMIE_OP(OR,  or)
    NORMIE_OP(AND, and)
    NORMIE_OP(SUB, sub)
    NORMIE_OP(XOR, xor)
    NORMIE_OP(MUL, imul)

    // shifts
    NORMIE_OP8(SHL, shl)
    NORMIE_OP8(SHR, shr)
    NORMIE_OP8(SAR, sar)
    NORMIE_OP8(ROL, rol)
    NORMIE_OP8(ROR, ror)

    (node div extra=X86MemOp)
    (node idiv extra=X86MemOp)

    pat (UDIV dt=$dt ___ $lhs $rhs) => (PROJ dt=$dt (x86_div dt=TB_TYPE_TUPLE ___ ___ $rhs $lhs extra_dt=$dt) index=0)
    pat (SDIV dt=$dt ___ $lhs $rhs) => (PROJ dt=$dt (x86_div dt=TB_TYPE_TUPLE ___ ___ $rhs $lhs extra_dt=$dt) index=0)
    pat (UMOD dt=$dt ___ $lhs $rhs) => (PROJ dt=$dt (x86_div dt=TB_TYPE_TUPLE ___ ___ $rhs $lhs extra_dt=$dt) index=1)
    pat (SMOD dt=$dt ___ $lhs $rhs) => (PROJ dt=$dt (x86_div dt=TB_TYPE_TUPLE ___ ___ $rhs $lhs extra_dt=$dt) index=1)

    // *******************************
    // float ops
    // *******************************
    (node vmov extra=X86MemOp)
    (node vzero extra=X86MemOp)

    pat (LOAD dt=$dt $ctrl $mem $addr) where "TB_IS_FLOAT_TYPE($dt) || TB_IS_VECTOR_TYPE($dt)" => (x86_vmov dt=$dt $ctrl $mem $addr mode=MODE_LD)
    pat (LOAD dt=$dt $ctrl $mem ($addr: MEMORY ...)) where "TB_IS_FLOAT_TYPE($dt) || TB_IS_VECTOR_TYPE($dt)" => (x86_vmov dt=$dt $ctrl $mem $addr mode=MODE_LD)
    pat (STORE $ctrl $mem $addr $val) where "TB_IS_FLOAT_TYPE(n->inputs[3]->dt) || TB_IS_VECTOR_TYPE(n->inputs[3]->dt)" => (x86_vmov dt=TB_TYPE_MEMORY $ctrl $mem $addr $val mode=MODE_ST extra_dt="n->inputs[3]->dt")
    pat (STORE $ctrl $mem ($lhs: MEMORY ...) $val) where "TB_IS_FLOAT_TYPE(n->inputs[3]->dt) || TB_IS_VECTOR_TYPE(n->inputs[3]->dt)" => (x86_vmov dt=TB_TYPE_MEMORY $ctrl $mem $lhs $val mode=MODE_ST extra_dt="n->inputs[3]->dt")

    // float constants goop
    pat (F32CONST $root) where "is_float_zero(n)" => (x86_vzero dt=TB_TYPE_F32 $root)
    pat (F32CONST $root) => (x86_vmov dt=TB_TYPE_F32 ___ ___ (MACH_SYMBOL $root sym="gimme_float_sym(ctx->module, n)") mode=MODE_LD)

    pat (F64CONST $root) where "is_float_zero(n)" => (x86_vzero dt=TB_TYPE_F64 $root)
    pat (F64CONST $root) => (x86_vmov dt=TB_TYPE_F64 ___ ___ (MACH_SYMBOL $root sym="gimme_float_sym(ctx->module, n)") mode=MODE_LD)

    pat (VBROADCAST dt=$dt ___ ($x: F32CONST $root)) where "is_float_zero($x)" => (x86_vzero dt=$dt $root)
    pat (VBROADCAST dt=$dt ___ ($x: F32CONST $root)) => (x86_vmov dt=$dt ___ ___ (MACH_SYMBOL $root sym="gimme_float_sym(ctx->module, n)") mode=MODE_LD)

    #define FLOATIE_OP(op_name, mnemonic) \
    (node mnemonic extra=X86MemOp) \
    pat (op_name dt=$dt ___ $lhs $rhs) => (x86_ ## mnemonic dt=$dt ___ ___ $rhs $lhs)

    FLOATIE_OP(FADD, vadd)
    FLOATIE_OP(FSUB, vsub)
    FLOATIE_OP(FMUL, vmul)
    FLOATIE_OP(FDIV, vdiv)
    FLOATIE_OP(FMAX, vmax)
    FLOATIE_OP(FMIN, vmin)

    // *******************************
    // casting
    // *******************************
    (node movsx8 extra=X86MemOp)
    (node movzx8 extra=X86MemOp)
    (node movsx16 extra=X86MemOp)
    (node movzx16 extra=X86MemOp)
    (node movsx32 extra=X86MemOp)

    (node vxor extra=X86MemOp)
    pat (FNEG dt=$dt ___ $src)
    => (x86_vxor dt=$dt ___ ___ (MACH_SYMBOL "f->root_node" sym="gimme_float_neg_zero(ctx->module, $dt)") $src mode=MODE_LD)

    pat (BITCAST dt=$dt ___ $src)
    => (MACH_COPY dt=$dt ___ $src def="ctx->normie_mask[TB_IS_FLOAT_TYPE(n->dt) ? REG_CLASS_XMM : REG_CLASS_GPR]" use="ctx->normie_mask[TB_IS_FLOAT_TYPE($src->dt) ? REG_CLASS_XMM : REG_CLASS_GPR]")

    pat (TRUNCATE dt=$dt ___ $src)
    => (MACH_COPY dt=$dt ___ $src def="ctx->normie_mask[REG_CLASS_GPR]" use="ctx->normie_mask[REG_CLASS_GPR]")

    // 32 -> 64
    pat (SIGN_EXT dt=$dt ___ $src) where "$dt.type == TB_TAG_I64 && $src->dt.type == TB_TAG_I32" => (x86_movsx32 dt=TB_TYPE_I64 ___ ___ $src extra_dt="TB_TYPE_I64")
    pat (ZERO_EXT dt=$dt ___ $src) where "$dt.type == TB_TAG_I64 && $src->dt.type == TB_TAG_I32" => (x86_mov     dt=TB_TYPE_I64 ___ ___ $src extra_dt="TB_TYPE_I32")

    // 1/8 -> 32/64
    pat (SIGN_EXT dt=$dt ___ $src)
    where "($dt.type == TB_TAG_I16 || $dt.type == TB_TAG_I32 || $dt.type == TB_TAG_I64) && ($src->dt.type == TB_TAG_I8 || $src->dt.type == TB_TAG_BOOL)"
    => (x86_movsx8 dt=$dt ___ ___ $src extra_dt="$src->dt")

    // 1/8 -> 16/32/64
    pat (ZERO_EXT dt=$dt ___ $src)
    where "($dt.type == TB_TAG_I16 || $dt.type == TB_TAG_I32 || $dt.type == TB_TAG_I64) && ($src->dt.type == TB_TAG_I8 || $src->dt.type == TB_TAG_BOOL)"
    => (x86_movzx8 dt=$dt ___ ___ $src extra_dt="TB_TYPE_I32")

    // 1/8 -> 16/32/64
    pat (ZERO_EXT dt=$dt ___ ($src: LOAD $ctrl $mem $addr))
    where "($dt.type == TB_TAG_I16 || $dt.type == TB_TAG_I32 || $dt.type == TB_TAG_I64) && ($src->dt.type == TB_TAG_I8 || $src->dt.type == TB_TAG_BOOL)"
    => (x86_movzx8 dt=$dt $ctrl $mem $addr mode=MODE_LD extra_dt="TB_TYPE_I32")

    // 1/8 -> 16/32/64
    pat (ZERO_EXT dt=$dt ___ ($src: LOAD $ctrl $mem ($addr: MEMORY ...)))
    where "($dt.type == TB_TAG_I16 || $dt.type == TB_TAG_I32 || $dt.type == TB_TAG_I64) && ($src->dt.type == TB_TAG_I8 || $src->dt.type == TB_TAG_BOOL)"
    => (x86_movzx8 dt=$dt $ctrl $mem $addr mode=MODE_LD extra_dt="TB_TYPE_I32")

    pat (ZERO_EXT dt=$dt ___ $src)
    where "$dt.type == TB_TAG_I8 && $src->dt.type == TB_TAG_BOOL"
    => (MACH_COPY dt=$dt ___ $src def="ctx->normie_mask[REG_CLASS_GPR]" use="ctx->normie_mask[REG_CLASS_GPR]")

    // 16 -> 32
    pat (SIGN_EXT dt=$dt ___ $src) where "$src->dt.type == TB_TAG_I16" => (x86_movsx16 dt=$dt ___ ___ $src extra_dt="TB_TYPE_I16")
    pat (ZERO_EXT dt=$dt ___ $src) where "$src->dt.type == TB_TAG_I16" => (x86_movzx16 dt=$dt ___ ___ $src extra_dt="TB_TYPE_I16")
)

#if 0
(pipeline A57
    (unit ALU0   2) // MAC capable
    (unit ALU1   2) // DIV capable
    (unit Branch 2)
    (unit LdSt   2)
    // Float ports
    (unit FP0    4)
    (unit FP1    4)

    (class AluOp (___ ___ $rhs  $lhs) (2 ALU0 ALU1))
    (class FpOp  (___ ___ $rhs  $lhs) (2 FP0  FP1))
    (class Div   (___ ___ $rhs  $lhs) (10 ALU1))
)
#endif
