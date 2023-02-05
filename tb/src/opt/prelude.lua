local ffi = require 'ffi'
local bitop = require 'bit'
local C = ffi.C

ffi.cdef[[
typedef int TB_Reg;
typedef int TB_Label;

typedef enum {
    TB_ASSUME_NSW,
    TB_ASSUME_NUW,
    TB_CAN_WRAP,
    TB_SIGNED_TRAP_ON_WRAP,
    TB_UNSIGNED_TRAP_ON_WRAP,
    TB_SATURATED_UNSIGNED,
    TB_SATURATED_SIGNED
} TB_ArithmaticBehavior;

typedef struct {
    TB_Reg label, val;
} TB_PhiInput;

typedef enum TB_MemoryOrder {
    TB_MEM_ORDER_RELAXED,
    TB_MEM_ORDER_CONSUME,
    TB_MEM_ORDER_ACQUIRE,
    TB_MEM_ORDER_RELEASE,
    TB_MEM_ORDER_ACQ_REL,
    TB_MEM_ORDER_SEQ_CST,
} TB_MemoryOrder;

typedef enum TB_DataTypeEnum {
    TB_INT,
    TB_FLOAT,
    TB_PTR,
} TB_DataTypeEnum;

typedef enum TB_FloatFormat {
    // IEEE 754 floats
    TB_FLT_32, TB_FLT_64
} TB_FloatFormat;

typedef union TB_DataType {
    struct {
        uint16_t type  : 2;
        // 2^N where N is the width value.
        // Only integers and floats can be wide.
        uint16_t width : 3;

        // for integers it's the bitwidth
        uint16_t data  : 11;
    };
    uint16_t raw;
} TB_DataType;

// represents byte counts
typedef uint32_t TB_CharUnits;

typedef unsigned int TB_AttributeID;
typedef unsigned int TB_FileID;
typedef unsigned int TB_FunctionID;

typedef struct TB_Module            TB_Module;
typedef struct TB_External          TB_External;
typedef struct TB_Global            TB_Global;
typedef struct TB_Initializer       TB_Initializer;
typedef struct TB_Function          TB_Function;
typedef struct TB_AttribList        TB_AttribList;
typedef struct TB_FunctionPrototype TB_FunctionPrototype;

typedef struct {
    TB_Function* func;
    TB_Reg reg;
} TB_NodeRef;

typedef enum TB_NodeTypeEnum {
    TB_NULL = 0,

    /* metadata */
    TB_LINE_INFO,
    TB_KEEPALIVE,
    TB_POISON,

    TB_ICALL, /* internal use only, inline call */
    TB_CALL,  /* standard function call */
    TB_VCALL, /* virtual call */
    TB_ECALL, /* extern call */

    /* Memory operations */
    TB_STORE,

    TB_MEMCLR,
    TB_MEMCPY,
    TB_MEMSET,
    TB_MEMCMP,
    TB_INITIALIZE,

    /* Atomics */
    TB_ATOMIC_TEST_AND_SET,
    TB_ATOMIC_CLEAR,

    TB_ATOMIC_XCHG,
    TB_ATOMIC_ADD,
    TB_ATOMIC_SUB,
    TB_ATOMIC_AND,
    TB_ATOMIC_XOR,
    TB_ATOMIC_OR,

    TB_ATOMIC_CMPXCHG, /* These are always bundled together */
    TB_ATOMIC_CMPXCHG2,
    TB_DEBUGBREAK,

    /* Terminators */
    TB_LABEL,
    TB_GOTO,
    TB_SWITCH,
    TB_IF,
    TB_RET,
    TB_TRAP,
    TB_UNREACHABLE,

    /* Load */
    TB_LOAD,

    /* Pointers */
    TB_LOCAL,
    TB_PARAM_ADDR,

    TB_PARAM,
    TB_FUNC_ADDRESS,
    TB_EXTERN_ADDRESS,
    TB_GLOBAL_ADDRESS,

    TB_MEMBER_ACCESS,
    TB_ARRAY_ACCESS,

    /* Immediates */
    TB_INTEGER_CONST,
    TB_FLOAT_CONST,
    TB_STRING_CONST,

    /* Conversions */
    TB_TRUNCATE,
    TB_FLOAT_EXT,
    TB_SIGN_EXT,
    TB_ZERO_EXT,
    TB_INT2PTR,
    TB_PTR2INT,
    TB_UINT2FLOAT,
    TB_FLOAT2UINT,
    TB_INT2FLOAT,
    TB_FLOAT2INT,
    TB_BITCAST,

    /* Select */
    TB_SELECT,

    /* Unary operations */
    TB_NOT,
    TB_NEG,

    /* Integer arithmatic */
    TB_AND,
    TB_OR,
    TB_XOR,
    TB_ADD,
    TB_SUB,
    TB_MUL,

    TB_SHL,
    TB_SHR,
    TB_SAR,
    TB_UDIV,
    TB_SDIV,
    TB_UMOD,
    TB_SMOD,

    /* Float arithmatic */
    TB_FADD,
    TB_FSUB,
    TB_FMUL,
    TB_FDIV,

    /* Comparisons */
    TB_CMP_EQ,
    TB_CMP_NE,
    TB_CMP_SLT,
    TB_CMP_SLE,
    TB_CMP_ULT,
    TB_CMP_ULE,
    TB_CMP_FLT,
    TB_CMP_FLE,

    /* PHI */
    // NOTE(NeGate): phi1 and phi2 are just to avoid
    // using extra space for the common cases
    TB_PHI1,
    TB_PHI2,
    TB_PHIN,

    // NOTE(NeGate): only used internally, if you
    // see one in normal IR things went wrong in
    // an optimization pass
    TB_PASS,

    // variadic
    TB_VA_START,

    // x86 intrinsics
    TB_X86INTRIN_LDMXCSR,
    TB_X86INTRIN_STMXCSR,
    TB_X86INTRIN_SQRT,
    TB_X86INTRIN_RSQRT,
} TB_NodeTypeEnum;
typedef uint8_t TB_NodeType;

typedef struct TB_Node {
    TB_NodeType    type;
    TB_DataType    dt;
    TB_Reg         next;
    TB_AttribList* first_attrib;

    union {
        struct TB_NodeInt {
            size_t num_words;
            union {
                uint64_t single_word;
                uint64_t* words;
            };
        } integer;
        struct TB_NodeFloat {
            double value;
        } flt;
        struct TB_NodeString {
            size_t length;
            const char* data;
        } string;
        struct TB_NodeFunction {
            const TB_Function* value;
        } func;
        struct TB_NodeExtern {
            const TB_External* value;
        } external;
        struct TB_NodeGlobal {
            const TB_Global* value;
        } global;
        struct TB_NodeLine {
            TB_FileID file;
            int line;
        } line_info;
        struct TB_NodeMemberAccess {
            TB_Reg  base;
            int32_t offset;
        } member_access;
        struct TB_NodeArrayAccess {
            TB_Reg base;
            TB_Reg index;
            TB_CharUnits stride;
        } array_access;
        struct TB_NodePtrdiff {
            TB_Reg a;
            TB_Reg b;
            TB_CharUnits stride;
        } ptrdiff;
        struct TB_NodeParam {
            uint32_t id;
            TB_CharUnits size;
        } param;
        struct TB_NodeParamAddr {
            TB_Reg param;

            TB_CharUnits size;
            TB_CharUnits alignment;
        } param_addr;
        struct TB_NodeLocal {
            TB_CharUnits size;
            TB_CharUnits alignment;
        } local;
        struct TB_NodeUnary {
            TB_Reg src;
        } unary;
        struct TB_NodeIArith {
            TB_Reg a;
            TB_Reg b;
            TB_ArithmaticBehavior arith_behavior;
        } i_arith;
        struct TB_NodeFArith {
            TB_Reg a;
            TB_Reg b;
        } f_arith;
        struct TB_NodeCompare {
            TB_Reg a;
            TB_Reg b;
            TB_DataType dt;
        } cmp;
        struct TB_NodeSelect {
            TB_Reg a;
            TB_Reg b;
            TB_Reg cond;
        } select;
        struct TB_NodeLoad {
            TB_Reg address;
            // this is only here to make load and store
            // payloads match in data layout... just because
            TB_Reg _;
            TB_CharUnits alignment;
            bool is_volatile;
        } load;
        struct TB_NodeStore {
            TB_Reg address;
            TB_Reg value;
            TB_CharUnits alignment;
            bool is_volatile;
        } store;
        struct TB_NodeAtomicRMW {
            TB_Reg addr;
            TB_Reg src;
            TB_MemoryOrder order;

            // NOTE(NeGate): this is used for fail
            TB_MemoryOrder order2;
        } atomic;
        struct TB_NodeReturn {
            TB_Reg value;
        } ret;
        struct TB_NodePass {
            TB_Reg value;
        } pass;
        struct TB_NodePhi1 {
            TB_PhiInput inputs[1];
        } phi1;
        struct TB_NodePhi2 {
            TB_PhiInput inputs[2];
        } phi2;
        struct TB_NodePhi {
            size_t count;
            TB_PhiInput* inputs;
        } phi;
        struct TB_NodeLabel {
            TB_Label id;
            TB_Reg   terminator;
        } label;
        struct TB_NodeIf {
            TB_Reg cond;
            TB_Label if_true;
            TB_Label if_false;
        } if_;
        struct TB_NodeGoto {
            TB_Label label;
        } goto_;
        struct TB_NodeExternCall {
            int param_start, param_end;
            const TB_External* target;
        } ecall;
        struct TB_NodeDynamicCall {
            int param_start, param_end;
            TB_Reg target;
        } vcall;
        struct TB_NodeFunctionCall {
            int param_start, param_end;
            const TB_Function* target;
        } call;
        struct TB_NodeSwitch {
            TB_Reg key;
            TB_Label default_label;
            int entries_start, entries_end;
        } switch_;
        struct TB_NodeMemoryOp {
            TB_Reg dst;
            TB_Reg src;
            TB_Reg size;
            TB_CharUnits align;
        } mem_op;
        struct TB_NodeMemoryClear {
            TB_Reg dst;
            TB_CharUnits size;
            TB_CharUnits align;
        } clear;
        struct TB_NodeInitialize {
            TB_Reg addr;
            TB_Initializer* src;
        } init;
    };
} TB_Node;

TB_NodeRef tb__first_node(TB_Function* f);
TB_NodeRef tb__next_node(TB_NodeRef r);
TB_Node* tb__nodes(TB_Function* f);
void tb__print_func(TB_Function* f);
bool tb__is_iconst(TB_Function* f, TB_Reg r, uint64_t imm);
bool tb__is_izero(TB_Function* f, TB_Reg r);

]]

tb = {}

function tb.all_nodes_iter(f)
	local node = C.tb__first_node(f)

	return function ()
		if node.reg ~= 0 then
			local old = node
			node = C.tb__next_node(node)
			return old
		else
			return nil
		end
	end
end

-- node type checking
function tb.is_iadd(n) return C.tb__nodes(n.func)[n.reg].type == C.TB_ADD end
function tb.is_isub(n) return C.tb__nodes(n.func)[n.reg].type == C.TB_SUB end
function tb.is_imul(n) return C.tb__nodes(n.func)[n.reg].type == C.TB_MUL end
function tb.is_izero(n) return C.tb__is_izero(n.func, n.reg) end
function tb.is_iconst(n, imm) return C.tb__is_iconst(n.func, n.reg, imm) end

-- node categories
function tb.is_idiv(n)
	local ty = C.tb__nodes(n.func)[n.reg].type
	return ty == C.TB_SDIV or ty == C.TB_UDIV
end

function tb.is_int_binop(n)
	local ty = C.tb__nodes(n.func)[n.reg].type
	return ty >= C.TB_AND and ty <= C.TB_SMOD
end

-- node accessors
-- this one applies to integer, float and
-- relational (comparisons) binary operators
function tb.get_binops(n)
	local nn = C.tb__nodes(n.func)[n.reg]
	return { func=n.func, reg=nn.i_arith.a }, { func=n.func, reg=nn.i_arith.b }
end

-- setters
function tb.set_pass(from, to)
	print("PASS "..from.reg.." -> "..to.reg);
	local r = C.tb__nodes(from.func)[from.reg]
	r.type = C.TB_PASS
	r.pass.value = to.reg
end

function tb.set_poison(n)
	print("POISON "..n.reg);
	C.tb__nodes(n.func)[n.reg].type = C.TB_POISON
end

function tb.kill(n)
	C.tb__nodes(n.func)[n.reg].type = C.TB_NULL
end

function tb.print_func(f)
	C.tb__print_func(f)
end
