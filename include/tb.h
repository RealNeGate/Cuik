#ifndef TB_CORE_H
#define TB_CORE_H

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
    #endif

    // https://semver.org/
    #define TB_VERSION_MAJOR 0
    #define TB_VERSION_MINOR 2
    #define TB_VERSION_PATCH 0

    #define TB_API extern

    // These are flags
    typedef enum TB_ArithmaticBehavior {
        TB_ARITHMATIC_NSW = 1,
        TB_ARITHMATIC_NUW = 2,
    } TB_ArithmaticBehavior;

    typedef enum TB_DebugFormat {
        TB_DEBUGFMT_NONE,

        TB_DEBUGFMT_DWARF,
        TB_DEBUGFMT_CODEVIEW,

        TB_DEBUGFMT_COLINPILLED
    } TB_DebugFormat;

    typedef enum TB_Arch {
        TB_ARCH_UNKNOWN,

        TB_ARCH_X86_64,
        // unsupported but planned
        TB_ARCH_AARCH64
    } TB_Arch;

    typedef enum TB_System {
        TB_SYSTEM_WINDOWS,
        TB_SYSTEM_LINUX,
        TB_SYSTEM_MACOS,

        // Not supported yet
        TB_SYSTEM_ANDROID,

        TB_SYSTEM_MAX,
    } TB_System;

    typedef enum TB_ABI {
        // Used on 64bit Windows platforms
        TB_ABI_WIN64,

        // Used on Mac, BSD and Linux platforms
        TB_ABI_SYSTEMV,
    } TB_ABI;

    typedef enum TB_OutputFlavor {
        TB_FLAVOR_OBJECT,     // .o  .obj
        TB_FLAVOR_SHARED,     // .so .dll
        TB_FLAVOR_STATIC,     // .a  .lib
        TB_FLAVOR_EXECUTABLE, //     .exe
    } TB_OutputFlavor;

    typedef enum TB_CallingConv {
        TB_CDECL,
        TB_STDCALL
    } TB_CallingConv;

    typedef struct TB_FeatureSet {
        struct {
            bool sse3 : 1;

            bool popcnt : 1;
            bool lzcnt : 1;
            bool sse41 : 1;
            bool sse42 : 1;

            bool clmul : 1;
            bool f16c : 1;

            bool bmi1 : 1;
            bool bmi2 : 1;

            bool avx : 1;
            bool avx2 : 1;
        } x64;
        struct {
            bool bf16 : 1;
        } aarch64;
    } TB_FeatureSet;

    typedef enum TB_BranchHint {
        TB_BRANCH_HINT_NONE,
        TB_BRANCH_HINT_LIKELY,
        TB_BRANCH_HINT_UNLIKELY
    } TB_BranchHint;

    typedef enum TB_Linkage {
        TB_LINKAGE_PUBLIC,
        TB_LINKAGE_PRIVATE
    } TB_Linkage;

    typedef enum TB_StorageClass {
        // data is the default way to handle storage, this is usually
        // passed onto the .data section
        TB_STORAGE_DATA,

        // Thread local storage will have the values defined per thread
        // and only accessible within that thread
        TB_STORAGE_TLS
    } TB_StorageClass;

    typedef enum TB_MemoryOrder {
        TB_MEM_ORDER_RELAXED,
        TB_MEM_ORDER_CONSUME,
        TB_MEM_ORDER_ACQUIRE,
        TB_MEM_ORDER_RELEASE,
        TB_MEM_ORDER_ACQ_REL,
        TB_MEM_ORDER_SEQ_CST,
    } TB_MemoryOrder;

    typedef enum TB_ISelMode {
        // FastISel
        TB_ISEL_FAST,
        TB_ISEL_COMPLEX
    } TB_ISelMode;

    typedef enum TB_DataTypeEnum {
        // Integers, note void is an i0 and bool is an i1
        //   i(0-2047)
        TB_INT,
        // Floating point numbers
        //   f{32,64}
        TB_FLOAT,
        // Pointers
        //   ptr(0-2047)
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
    static_assert(sizeof(TB_DataType) == sizeof(uint16_t), "TB_DataType isn't 16bits...");

    // classify data types
    #define TB_IS_VOID_TYPE(x)     ((x).type == TB_INT && (x).data == 0)
    #define TB_IS_BOOL_TYPE(x)     ((x).type == TB_INT && (x).data == 1)
    #define TB_IS_INTEGER_TYPE(x)  ((x).type == TB_INT)
    #define TB_IS_FLOAT_TYPE(x)    ((x).type == TB_FLOAT)
    #define TB_IS_POINTER_TYPE(x)  ((x).type == TB_PTR)

    // accessors
    #define TB_GET_INT_BITWIDTH(x) ((x).data)
    #define TB_GET_FLOAT_FORMAT(x) ((x).data)
    #define TB_GET_PTR_ADDRSPACE(x) ((x).data)

    typedef enum TB_NodeTypeEnum {
        TB_NULL = 0,

        /* metadata */
        TB_LINE_INFO,
        TB_KEEPALIVE,
        TB_POISON,

        TB_ICALL, /* internal use only, inline call */
        TB_CALL,  /* standard function call */
        TB_SCALL, /* system call */
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

        TB_ATOMIC_LOAD,
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
        TB_UNREACHABLE,
        TB_TRAP,

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
        TB_FLOAT32_CONST,
        TB_FLOAT64_CONST,
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

    #define TB_IS_NODE_SIDE_EFFECT(type) ((type) >= TB_LINE_INFO && (type) <= TB_DEBUGBREAK)
    #define TB_IS_NODE_TERMINATOR(type)  ((type) >= TB_LABEL && (type) <= TB_TRAP)

    typedef int TB_Label;

    typedef struct {
        int32_t  key;
        TB_Label value;
    } TB_SwitchEntry;

    // just represents some region of bytes, usually in file parsing crap
    typedef struct {
        size_t length;
        uint8_t* data;
    } TB_Slice;

    // represents byte counts
    typedef uint32_t TB_CharUnits;

    typedef unsigned int TB_AttributeID;
    typedef unsigned int TB_FileID;
    typedef unsigned int TB_FunctionID;

    typedef struct TB_Module            TB_Module;
    typedef struct TB_External          TB_External;
    typedef struct TB_Global            TB_Global;
    typedef struct TB_DebugType         TB_DebugType;
    typedef struct TB_Initializer       TB_Initializer;
    typedef struct TB_Function          TB_Function;
    typedef struct TB_AttribList        TB_AttribList;
    typedef struct TB_FunctionPrototype TB_FunctionPrototype;

    // references to a node within a TB_Function
    // these are virtual registers so they don't necessarily
    // map to any hardware but instead represent some operation
    typedef int TB_Reg, TB_Register;

    #define TB_NULL_REG ((TB_Reg)0)
    #define TB_REG_MAX  ((TB_Reg)INT_MAX)

    typedef struct {
        TB_Reg label, val;
    } TB_PhiInput;

    typedef struct TB_Node {
        TB_NodeType    type;
        TB_DataType    dt;
        TB_Reg         next;
        TB_AttribList* first_attrib;

        union {
            uint8_t raw_operands[16];
            struct TB_NodeInt {
                size_t num_words;
                union {
                    uint64_t single_word;
                    uint64_t* words;
                };
            } integer;
            struct TB_NodeFloat32 {
                float value;
            } flt32;
            struct TB_NodeFloat64 {
                double value;
            } flt64;
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
                const char* name;
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
            struct TB_NodeSysCall {
                int param_start, param_end;
                TB_Reg target;
            } scall;
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
    static_assert(sizeof(TB_Node) <= 32, "sizeof(TB_Node) <= 32");

    // represents the atomic cmpxchg result since it's two values
    typedef struct {
        TB_Reg success;
        TB_Reg old_value;
    } TB_CmpXchgResult;

    typedef struct TB_Loop {
        // refers to another entry in TB_LoopInfo... unless it's -1
        ptrdiff_t parent_loop;

        TB_Label header;
        TB_Label backedge;

        size_t body_count;
        TB_Label* body;
    } TB_Loop;

    typedef struct TB_LoopInfo {
        size_t count;
        TB_Loop* loops;
    } TB_LoopInfo;

    typedef struct TB_Predeccesors {
        int* count;
        TB_Label** preds;
    } TB_Predeccesors;

    typedef enum {
        TB_OBJECT_RELOC_NONE, // how?

        // Target independent
        TB_OBJECT_RELOC_ADDR32,
        TB_OBJECT_RELOC_ADDR64, // unsupported on 32bit platforms
        TB_OBJECT_RELOC_SECREL,
        TB_OBJECT_RELOC_SECTION,

        // COFF only
        TB_OBJECT_RELOC_ADDR32NB, // Relative virtual address

        // x64 only
        TB_OBJECT_RELOC_REL32,   // relative 32bit displacement
        TB_OBJECT_RELOC_REL32_1, //   plus 1
        TB_OBJECT_RELOC_REL32_2, //   plus 2
        TB_OBJECT_RELOC_REL32_3, //   and so on
        TB_OBJECT_RELOC_REL32_4, //   ...
        TB_OBJECT_RELOC_REL32_5,

        // Aarch64 only
        TB_OBJECT_RELOC_BRANCH26, // 26bit displacement for B and BL instructions
        TB_OBJECT_RELOC_REL21,    // for ADR instructions

        // TODO(NeGate): fill in the rest of this later
    } TB_ObjectRelocType;

    typedef struct {
        TB_ObjectRelocType type;
        uint32_t symbol_index;
        size_t virtual_address;
        size_t addend;
    } TB_ObjectReloc;

    typedef struct {
        TB_Slice name;

        size_t virtual_address;
        size_t virtual_size;

        // You can have a virtual size without having a raw
        // data size, that's how the BSS section works
        TB_Slice raw_data;

        size_t relocation_count;
        TB_ObjectReloc* relocations;
    } TB_ObjectSection;

    typedef enum {
        TB_OBJECT_SYMBOL_SECTION
    } TB_ObjectSymbolType;

    typedef struct {
        TB_ObjectSymbolType type;
        TB_Slice name;

        // this is zeroed out by the loader and left for the user to do crap with
        void* user_data;
    } TB_ObjectSymbol;

    typedef enum {
        TB_OBJECT_FILE_UNKNOWN,

        TB_OBJECT_FILE_COFF,
        TB_OBJECT_FILE_ELF64
    } TB_ObjectFileType;

    typedef struct {
        TB_ObjectFileType type;
        TB_Arch           arch;

        size_t           symbol_count;
        TB_ObjectSymbol* symbols;

        size_t           section_count;
        TB_ObjectSection sections[];
    } TB_ObjectFile;

    typedef struct {
        const char* libname;
        const char* name;
    } TB_ArchiveImport;

    typedef struct {
        size_t  object_file_count;

        // import table
        size_t import_count;
        TB_ArchiveImport* imports;

        // Name table maps to the object files directly
        char**   object_file_names;
        TB_Slice object_files[];
    } TB_ArchiveFile;

    typedef struct {
        // list of the object/archive files
        size_t input_count;
        const char** inputs;

        // list of the linker search paths
        size_t search_dir_count;
        const char** search_dirs;
    } TB_LinkerInput;

    typedef struct {
        enum {
            TB_EXPORT_PACKET_NONE,

            // allocates a region of memory
            //
            TB_EXPORT_PACKET_ALLOC,

            TB_EXPORT_PACKET_WRITE,
        } type;
        union {
            struct {
                // input
                size_t request_size;

                // output
                void* memory;
            } alloc;
            struct {
                // input
                size_t length;
                const uint8_t* data;
            } write;
        };
    } TB_ModuleExportPacket;

    typedef struct TB_ModuleExporter {
        // top secret stuff
        void* state;

        bool (*next)(TB_Module* m, void* exporter, TB_ModuleExportPacket* packet);
    } TB_ModuleExporter;

    // *******************************
    // Public macros
    // *******************************
    #ifdef __cplusplus

    #define TB_TYPE_VOID    TB_DataType{ { TB_INT,   0, 0 } }
    #define TB_TYPE_I8      TB_DataType{ { TB_INT,   0, 8 } }
    #define TB_TYPE_I16     TB_DataType{ { TB_INT,   0, 16 } }
    #define TB_TYPE_I32     TB_DataType{ { TB_INT,   0, 32 } }
    #define TB_TYPE_I64     TB_DataType{ { TB_INT,   0, 64 } }
    #define TB_TYPE_F32     TB_DataType{ { TB_FLOAT, 0, TB_FLT_32 } }
    #define TB_TYPE_F64     TB_DataType{ { TB_FLOAT, 0, TB_FLT_64 } }
    #define TB_TYPE_BOOL    TB_DataType{ { TB_INT,   0, 1 } }
    #define TB_TYPE_PTR     TB_DataType{ { TB_PTR,   0, 0 } }
    #define TB_TYPE_PTRN(N) TB_DataType{ { TB_PTR,   0, (N) } }

    #else

    #define TB_TYPE_VOID (TB_DataType){ { TB_INT,   0, 0 } }
    #define TB_TYPE_I8   (TB_DataType){ { TB_INT,   0, 8 } }
    #define TB_TYPE_I16  (TB_DataType){ { TB_INT,   0, 16 } }
    #define TB_TYPE_I32  (TB_DataType){ { TB_INT,   0, 32 } }
    #define TB_TYPE_I64  (TB_DataType){ { TB_INT,   0, 64 } }
    #define TB_TYPE_F32  (TB_DataType){ { TB_FLOAT, 0, TB_FLT_32 } }
    #define TB_TYPE_F64  (TB_DataType){ { TB_FLOAT, 0, TB_FLT_64 } }
    #define TB_TYPE_BOOL (TB_DataType){ { TB_INT,   0, 1 } }
    #define TB_TYPE_PTR  (TB_DataType){ { TB_PTR,   0, 0 } }
    #define TB_TYPE_PTRN(N) (TB_DataType){ { TB_PTR,  0, (N) } }

    #endif

    typedef void (*TB_PrintCallback)(void* user_data, const char* fmt, ...);

    ////////////////////////////////
    // Module management
    ////////////////////////////////
    // Creates a module with the correct target and settings
    TB_API TB_Module* tb_module_create(TB_Arch target_arch, TB_System target_system, TB_DebugFormat debug_fmt, const TB_FeatureSet* features);

    // Creates a module but defaults on the architecture and system based on the host machine
    TB_API TB_Module* tb_module_create_for_host(TB_DebugFormat debug_fmt, const TB_FeatureSet* features);

    // Validates IR & compiles the function into machine code.
    // For isel_mode, TB_ISEL_FAST will compile faster but worse codegen
    // TB_ISEL_COMPLEX will compile slower but better codegen
    //
    // returns false if it fails.
    TB_API bool tb_module_compile_function(TB_Module* m, TB_Function* f, TB_ISelMode isel_mode);
    TB_API bool tb_module_compile_functions(TB_Module* m, size_t count, TB_Function* funcs, TB_ISelMode isel_mode);

    // Frees all resources for the TB_Module and it's functions, globals and
    // compiled code.
    TB_API void tb_module_destroy(TB_Module* m);

    // When targetting windows & thread local storage, you'll need to bind a tls index
    // which is usually just a global that the runtime support has initialized, if you
    // dont and the tls_index is used, it'll crash
    TB_API void tb_module_set_tls_index(TB_Module* m, TB_External* e);

    TB_API TB_ModuleExporter tb_make_exporter(TB_Module* m, TB_OutputFlavor flavor);
    #define tb_exporter_next(m, exporter, packet) ((exporter).next(m, (exporter).state, packet))

    TB_API bool tb_exporter_to_file(TB_Module* m, TB_ModuleExporter exporter, const char* filepath);
    TB_API uint8_t* tb_exporter_to_buffer(TB_Module* m, TB_ModuleExporter exporter, size_t* length);

    // For isel_mode, TB_ISEL_FAST will compile faster but worse codegen
    // TB_ISEL_COMPLEX will compile slower but better codegen
    TB_API void tb_module_export_jit(TB_Module* m, TB_ISelMode isel_mode);

    typedef struct TB_FunctionIter {
        // public
        TB_Function* f;

        // internal
        TB_Module* module_;
        size_t index_;
    } TB_FunctionIter;

    #define TB_FOR_FUNCTIONS(it, module) for (TB_FunctionIter it = { .module_ = (module) }; tb_next_function(&it);)
    TB_API TB_FunctionIter tb_function_iter(TB_Module* m);
    TB_API bool tb_next_function(TB_FunctionIter* it);

    typedef struct TB_FunctionBatchIter {
        // public
        TB_Function* start;
        size_t count;

        // internal
        TB_Module* module_;
        size_t index_;
    } TB_FunctionBatchIter;

    #define TB_FOR_FUNCTION_BATCH(it, module) for (TB_FunctionBatchIter it = { .module_ = (module) }; tb_next_function_batch(&it);)
    TB_API size_t tb_estimate_function_batch_count(TB_Module* m);
    TB_API TB_FunctionBatchIter tb_function_batch_iter(TB_Module* m);
    TB_API bool tb_next_function_batch(TB_FunctionBatchIter* it);

    typedef struct TB_ExternalIter {
        // public
        TB_External* e;

        // internal
        TB_Module* module_;
        void* p_;
        size_t a_, b_, c_;
    } TB_ExternalIter;

    #define TB_FOR_EXTERNALS(it, module) for (TB_ExternalIter it = tb_external_iter(module); tb_next_external(&it);)
    TB_API TB_ExternalIter tb_external_iter(TB_Module* m);
    TB_API bool tb_next_external(TB_ExternalIter* it);

    TB_API const char* tb_extern_get_name(TB_External* e);

    // this is used JIT scenarios to tell the compiler what externals map to
    TB_API void tb_extern_bind_ptr(TB_External* e, void* ptr);

    TB_API TB_External* tb_extern_create(TB_Module* m, const char* name);
    TB_API TB_FileID tb_file_create(TB_Module* m, const char* path);

    // Called once you're done with TB operations on a thread (or i guess when it's
    // about to be killed :p), not calling it can only result in leaks on that thread
    // and calling it too early will result in TB potentially reallocating it but there's
    // should be no crashes from this, just potential slowdown or higher than expected memory
    // usage.
    TB_API void tb_free_thread_resources(void);

    ////////////////////////////////
    // Function Prototypes
    ////////////////////////////////
    // creates a function prototype used to define a function's parameters and
    // return type.
    //
    // function prototypes do not get freed individually and last for the entire run
    // of the backend, they can also be reused for multiple functions which have
    // matching signatures.
    TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv conv, TB_DataType return_dt, int num_params, bool has_varargs);

    // adds a parameter to the function prototype, TB doesn't support struct
    // parameters so the frontend must lower them to pointers or any other type
    // depending on their preferred ABI.
    TB_API void tb_prototype_add_param(TB_FunctionPrototype* p, TB_DataType dt);

    // same as tb_prototype_add_param(...) but takes an array
    TB_API void tb_prototype_add_params(TB_FunctionPrototype* p, size_t count, const TB_DataType* dt);

    ////////////////////////////////
    // Constant Initializers
    ////////////////////////////////
    // NOTE: the max objects is a cap and thus it can be bigger than the actual
    // number used.
    TB_API TB_Initializer* tb_initializer_create(TB_Module* m, size_t size, size_t align, size_t max_objects);

    // returns a buffer which the user can fill to then have represented in the
    // initializer
    TB_API void* tb_initializer_add_region(TB_Module* m, TB_Initializer* id, size_t offset, size_t size);

    // places a relocation for a global at offset, the size of the relocation
    // depends on the pointer size
    TB_API void tb_initializer_add_global(TB_Module* m, TB_Initializer* id, size_t offset, const TB_Global* global);
    TB_API void tb_initializer_add_function(TB_Module* m, TB_Initializer* id, size_t offset, const TB_Function* func);
    TB_API void tb_initializer_add_extern(TB_Module* m, TB_Initializer* id, size_t offset, const TB_External* external);

    ////////////////////////////////
    // Constant Initializers
    ////////////////////////////////
    TB_API TB_Global* tb_global_create(TB_Module* m, const char* name, TB_StorageClass storage, TB_Linkage linkage);
    TB_API void tb_global_set_initializer(TB_Module* m, TB_Global* global, TB_Initializer* initializer);

    ////////////////////////////////
    // Function Attributes
    ////////////////////////////////
    // These are parts of a function that describe metadata for instructions

    // restrict is applied onto loads and store operations meaning that the pointer
    // being accessed doesn't alias with any of the other pointers within the scope
    TB_API TB_AttributeID tb_function_attrib_restrict(TB_Function* f, TB_AttributeID scope);

    // This defines a some scope which can have a set of restrict pointers defined
    // in it
    TB_API TB_AttributeID tb_function_attrib_scope(TB_Function* f, TB_AttributeID parent_scope);

    // places an attribute on a function, note that there's no limit to how many
    // registers can share an attribute
    TB_API void tb_function_append_attrib(TB_Function* f, TB_Reg r, TB_AttributeID a);

    ////////////////////////////////
    // Debug info Generation
    ////////////////////////////////
    TB_API const TB_DebugType* tb_debug_get_void(TB_Module* m);
    TB_API const TB_DebugType* tb_debug_get_integer(TB_Module* m, bool is_signed, int bits);
    TB_API const TB_DebugType* tb_debug_get_float(TB_Module* m, TB_FloatFormat fmt);
    TB_API const TB_DebugType* tb_debug_create_ptr(TB_Module* m, const TB_DebugType* base);
    TB_API const TB_DebugType* tb_debug_create_array(TB_Module* m, const TB_DebugType* base, size_t count);

    ////////////////////////////////
    // IR access
    ////////////////////////////////
    typedef struct TB_NodeInputIter {
        TB_Reg r;

        // private
        int index_;
        TB_Reg parent_;
    } TB_NodeInputIter;

    #define TB_FOR_INPUT_IN_REG(it, f, parent) for (TB_NodeInputIter it = { .parent_ = (parent) }; tb_next_node_input(f, &it);)
    #define TB_FOR_INPUT_IN_NODE(it, f, parent) for (TB_NodeInputIter it = { .parent_ = (parent) - f->nodes }; tb_next_node_input(f, &it);)

    TB_API TB_NodeInputIter tb_node_input_iter(TB_Reg r);
    TB_API bool tb_next_node_input(const TB_Function* f, TB_NodeInputIter* iter);

    ////////////////////////////////
    // Function IR Generation
    ////////////////////////////////
    // the user_data is expected to be a valid FILE*
    TB_API void tb_default_print_callback(void* user_data, const char* fmt, ...);

    // this only allows for power of two vector types
    TB_API TB_DataType tb_vector_type(TB_DataTypeEnum type, int width);

    TB_API TB_Function* tb_function_create(TB_Module* m, const char* name, TB_Linkage linkage);

    TB_API void* tb_function_get_jit_pos(TB_Function* f);

    TB_API void tb_function_set_name(TB_Function* f, const char* name);
    TB_API const char* tb_function_get_name(TB_Function* f);

    TB_API void tb_function_set_prototype(TB_Function* f, const TB_FunctionPrototype* p);
    TB_API const TB_FunctionPrototype* tb_function_get_prototype(TB_Function* f);

    TB_API TB_Reg tb_function_insert(TB_Function* f, TB_Reg r, const TB_Node n);
    TB_API TB_Reg tb_function_set(TB_Function* f, TB_Reg r, const TB_Node n);
    TB_API TB_Reg tb_function_append(TB_Function* f, const TB_Node n);

    TB_API void tb_function_print(TB_Function* f, TB_PrintCallback callback, void* user_data);
    TB_API void tb_function_print2(TB_Function* f, TB_PrintCallback callback, void* user_data, bool display_nops);
    TB_API void tb_function_free(TB_Function* f);

    TB_API TB_Label tb_inst_get_current_label(TB_Function* f);
    TB_API void tb_inst_loc(TB_Function* f, TB_FileID file, int line);

    TB_API void tb_inst_set_scope(TB_Function* f, TB_AttributeID scope);
    TB_API TB_AttributeID tb_inst_get_scope(TB_Function* f);

    TB_API void tb_inst_unreachable(TB_Function* f);
    TB_API void tb_inst_debugbreak(TB_Function* f);
    TB_API void tb_inst_trap(TB_Function* f);
    TB_API void tb_inst_keep_alive(TB_Function* f, TB_Reg src);
    TB_API TB_Reg tb_inst_poison(TB_Function* f);

    TB_API TB_Reg tb_inst_param(TB_Function* f, int param_id);
    TB_API TB_Reg tb_inst_param_addr(TB_Function* f, int param_id);

    TB_API TB_Reg tb_inst_fpxt(TB_Function* f, TB_Reg src, TB_DataType dt);
    TB_API TB_Reg tb_inst_sxt(TB_Function* f, TB_Reg src, TB_DataType dt);
    TB_API TB_Reg tb_inst_zxt(TB_Function* f, TB_Reg src, TB_DataType dt);
    TB_API TB_Reg tb_inst_trunc(TB_Function* f, TB_Reg src, TB_DataType dt);
    TB_API TB_Reg tb_inst_int2ptr(TB_Function* f, TB_Reg src);
    TB_API TB_Reg tb_inst_ptr2int(TB_Function* f, TB_Reg src, TB_DataType dt);
    TB_API TB_Reg tb_inst_int2float(TB_Function* f, TB_Reg src, TB_DataType dt, bool is_signed);
    TB_API TB_Reg tb_inst_float2int(TB_Function* f, TB_Reg src, TB_DataType dt, bool is_signed);
    TB_API TB_Reg tb_inst_bitcast(TB_Function* f, TB_Reg src, TB_DataType dt);

    TB_API TB_Reg tb_inst_local(TB_Function* f, uint32_t size, TB_CharUnits align, const char* name);
    TB_API TB_Reg tb_inst_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits align);
    TB_API void tb_inst_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits align);

    TB_API TB_Reg tb_inst_volatile_load(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_CharUnits alignment);
    TB_API void tb_inst_volatile_store(TB_Function* f, TB_DataType dt, TB_Reg addr, TB_Reg val, TB_CharUnits alignment);

    TB_API TB_Reg tb_inst_bool(TB_Function* f, bool imm);
    TB_API TB_Reg tb_inst_ptr(TB_Function* f, uint64_t imm);
    TB_API TB_Reg tb_inst_sint(TB_Function* f, TB_DataType dt, int64_t imm);
    TB_API TB_Reg tb_inst_uint(TB_Function* f, TB_DataType dt, uint64_t imm);
    TB_API TB_Reg tb_inst_float32(TB_Function* f, float imm);
    TB_API TB_Reg tb_inst_float64(TB_Function* f, double imm);
    TB_API TB_Reg tb_inst_cstring(TB_Function* f, const char* str);
    TB_API TB_Reg tb_inst_string(TB_Function* f, size_t len, const char* str);

    // Applies an initializer to a memory region
    TB_API void tb_inst_initialize_mem(TB_Function* f, TB_Reg addr, TB_Initializer* src);

    // Broadcasts 'val' across 'count' elements starting 'dst'
    TB_API void tb_inst_memset(TB_Function* f, TB_Reg dst, TB_Reg val, TB_Reg count, TB_CharUnits align);

    // performs a copy of 'count' elements from one memory location to another
    // both locations cannot overlap.
    TB_API void tb_inst_memcpy(TB_Function* f, TB_Reg dst, TB_Reg src, TB_Reg count, TB_CharUnits align);

    // Clears a memory region to zeroes
    TB_API void tb_inst_memclr(TB_Function* f, TB_Reg addr, TB_CharUnits size, TB_CharUnits align);

    // result = base + (index * stride)
    TB_API TB_Reg tb_inst_array_access(TB_Function* f, TB_Reg base, TB_Reg index, uint32_t stride);

    // result = base + offset
    // where base is a pointer
    TB_API TB_Reg tb_inst_member_access(TB_Function* f, TB_Reg base, int32_t offset);

    TB_API TB_Reg tb_inst_get_func_address(TB_Function* f, const TB_Function* target);
    TB_API TB_Reg tb_inst_get_extern_address(TB_Function* f, const TB_External* target);
    TB_API TB_Reg tb_inst_get_global_address(TB_Function* f, const TB_Global* target);

    // Performs a conditional select between two values, if the operation is
    // performed wide then the cond is expected to be the same type as a and b where
    // the condition is resolved as true if the MSB (per component) is 1.
    //
    // result = cond ? a : b
    // a, b must match in type
    TB_API TB_Reg tb_inst_select(TB_Function* f, TB_Reg cond, TB_Reg a, TB_Reg b);

    // Integer arithmatic
    TB_API TB_Reg tb_inst_add(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
    TB_API TB_Reg tb_inst_sub(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
    TB_API TB_Reg tb_inst_mul(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
    TB_API TB_Reg tb_inst_div(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
    TB_API TB_Reg tb_inst_mod(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);

    // Bitwise operations
    TB_API TB_Reg tb_inst_not(TB_Function* f, TB_Reg n);
    TB_API TB_Reg tb_inst_neg(TB_Function* f, TB_Reg n);
    TB_API TB_Reg tb_inst_and(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_or(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_xor(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_sar(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_shl(TB_Function* f, TB_Reg a, TB_Reg b, TB_ArithmaticBehavior arith_behavior);
    TB_API TB_Reg tb_inst_shr(TB_Function* f, TB_Reg a, TB_Reg b);

    // Atomics
    // By default you can use TB_MEM_ORDER_SEQ_CST for the memory order to get
    // correct but possibly slower results on certain platforms (those with relaxed
    // memory models).
    TB_API TB_Reg tb_inst_atomic_test_and_set(TB_Function* f, TB_Reg addr, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_clear(TB_Function* f, TB_Reg addr, TB_MemoryOrder order);

    // Must be aligned to the natural alignment of dt
    TB_API TB_Reg tb_inst_atomic_load(TB_Function* f, TB_Reg addr, TB_DataType dt, TB_MemoryOrder order);

    // All atomic operations here return the old value and the operations are
    // performed in the same data type as 'src' with alignment of 'addr' being
    // the natural alignment of 'src'
    TB_API TB_Reg tb_inst_atomic_xchg(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_add(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_sub(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_and(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_xor(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);
    TB_API TB_Reg tb_inst_atomic_or(TB_Function* f, TB_Reg addr, TB_Reg src, TB_MemoryOrder order);

    // if (*addr == expected) {
    //   old_value = atomic_xchg(addr, desired);
    //   return { true, old_value };
    // } else {
    //   return { false };
    // }
    TB_API TB_CmpXchgResult tb_inst_atomic_cmpxchg(TB_Function* f, TB_Reg addr, TB_Reg expected, TB_Reg desired, TB_MemoryOrder succ, TB_MemoryOrder fail);

    // Float math
    TB_API TB_Reg tb_inst_fadd(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_fsub(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_fmul(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_fdiv(TB_Function* f, TB_Reg a, TB_Reg b);

    // Comparisons
    TB_API TB_Reg tb_inst_cmp_eq(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_cmp_ne(TB_Function* f, TB_Reg a, TB_Reg b);

    TB_API TB_Reg tb_inst_cmp_ilt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
    TB_API TB_Reg tb_inst_cmp_ile(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
    TB_API TB_Reg tb_inst_cmp_igt(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);
    TB_API TB_Reg tb_inst_cmp_ige(TB_Function* f, TB_Reg a, TB_Reg b, bool signedness);

    TB_API TB_Reg tb_inst_cmp_flt(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_cmp_fle(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_cmp_fgt(TB_Function* f, TB_Reg a, TB_Reg b);
    TB_API TB_Reg tb_inst_cmp_fge(TB_Function* f, TB_Reg a, TB_Reg b);

    // General intrinsics
    TB_API TB_Reg tb_inst_va_start(TB_Function* f, TB_Reg a);

    // x86 Intrinsics
    TB_API TB_Reg tb_inst_x86_ldmxcsr(TB_Function* f, TB_Reg a);
    TB_API TB_Reg tb_inst_x86_stmxcsr(TB_Function* f);
    TB_API TB_Reg tb_inst_x86_sqrt(TB_Function* f, TB_Reg a);
    TB_API TB_Reg tb_inst_x86_rsqrt(TB_Function* f, TB_Reg a);

    // Control flow
    TB_API TB_Reg tb_inst_call(TB_Function* f, TB_DataType dt, const TB_Function* target, size_t param_count, const TB_Reg* params);
    TB_API TB_Reg tb_inst_syscall(TB_Function* f, TB_DataType dt, TB_Reg syscall_num, size_t param_count, const TB_Reg* params);
    TB_API TB_Reg tb_inst_vcall(TB_Function* f, TB_DataType dt, TB_Reg target, size_t param_count, const TB_Reg* params);
    TB_API TB_Reg tb_inst_ecall(TB_Function* f, TB_DataType dt, const TB_External* target, size_t param_count, const TB_Reg* params);

    TB_API TB_Label tb_inst_new_label_id(TB_Function* f);
    TB_API TB_Reg tb_inst_phi2(TB_Function* f, TB_Label a_label, TB_Reg a, TB_Label b_label, TB_Reg b);
    TB_API TB_Reg tb_inst_label(TB_Function* f, TB_Label id);
    TB_API void tb_inst_goto(TB_Function* f, TB_Label id);
    TB_API TB_Reg tb_inst_if(TB_Function* f, TB_Reg cond, TB_Label if_true, TB_Label if_false);
    TB_API void tb_inst_switch(TB_Function* f, TB_DataType dt, TB_Reg key, TB_Label default_label, size_t entry_count, const TB_SwitchEntry* entries);
    TB_API void tb_inst_ret(TB_Function* f, TB_Reg value);

    ////////////////////////////////
    // Optimizer
    ////////////////////////////////
    typedef struct TB_Pass {
        // the pass modes tell us what things the pass can modify
        // and what it's being scheduled to run on
        enum TB_PassMode {
            // basic blocks
            TB_BASIC_BLOCK_PASS,

            // loops' basic blocks
            TB_LOOP_PASS,

            // this is where a majority of optimizations live
            // applied to all functions in a module but any execution
            // shall not affect functions outside of the one it's applied on
            TB_FUNCTION_PASS,

            // unstructured and applied to the entire module
            // can modify the entire module data
            TB_MODULE_PASS,
        } mode;
        const char* name;

        // if l_state is not NULL, it'll run the lua function described
        void* l_state;
        union {
            bool(*bb_run)(TB_Function* f, TB_Reg bb);
            bool(*loop_run)(TB_Function* f, const TB_Loop* l);
            bool(*func_run)(TB_Function* f);
            bool(*mod_run)(TB_Module* m);
        };
    } TB_Pass;

    // Applies optimizations to the entire module
    TB_API bool tb_module_optimize(TB_Module* m, size_t pass_count, const TB_Pass passes[]);

    TB_API TB_Pass tb_opt_load_lua_pass(const char* path, enum TB_PassMode mode);
    TB_API void tb_opt_unload_lua_pass(TB_Pass* p);

    // analysis
    TB_API TB_Predeccesors tb_get_predeccesors(TB_Function* f);

    typedef struct {
        size_t count;

        // max size is label_count
        TB_Reg* traversal;

        // NOTE: you can free visited once the postorder calculation is complete
        // max size is label_count
        bool* visited;
    } TB_PostorderWalk;

    // Allocates from the heap and requires freeing with tb_function_free_postorder
    TB_API TB_PostorderWalk tb_function_get_postorder(TB_Function* f);
    TB_API void tb_function_free_postorder(TB_PostorderWalk* walk);

    // Doesn't allocate memory for you
    TB_API void tb_function_get_postorder_explicit(TB_Function* f, TB_PostorderWalk* walk);

    // if out_doms is NULL it'll only return the dominator array length (it's just the label count really)
    TB_API size_t tb_get_dominators(TB_Function* f, TB_Predeccesors p, TB_Label* out_doms);
    TB_API bool tb_is_dominated_by(TB_Label* doms, TB_Label expected_dom, TB_Label bb);

    TB_API TB_LoopInfo tb_get_loop_info(TB_Function* f, TB_Predeccesors preds, TB_Label* doms);
    TB_API void tb_free_loop_info(TB_LoopInfo loops);

    ////////////////////////////////
    // Transformation pass library
    ////////////////////////////////
    // function level
    TB_API TB_Pass tb_opt_hoist_locals(void);
    TB_API TB_Pass tb_opt_merge_rets(void);
    TB_API TB_Pass tb_opt_instcombine(void);
    TB_API TB_Pass tb_opt_subexpr_elim(void);
    TB_API TB_Pass tb_opt_remove_pass_nodes(void);
    TB_API TB_Pass tb_opt_mem2reg(void);
    TB_API TB_Pass tb_opt_compact_dead_regs(void);
    TB_API TB_Pass tb_opt_dead_block_elim(void);
    TB_API TB_Pass tb_opt_dead_expr_elim(void);
    TB_API TB_Pass tb_opt_load_store_elim(void);

    // module level
    // TB_API TB_Pass tb_opt_inline(void);

    ////////////////////////////////
    // IR access
    ////////////////////////////////
    TB_API TB_FunctionID tb_function_get_id(TB_Module* m, TB_Function* f);
    TB_API TB_Function* tb_function_from_id(TB_Module* m, TB_FunctionID id);

    TB_API TB_Reg tb_node_get_last_register(TB_Function* f);
    TB_API TB_Reg tb_node_get_previous(TB_Function* f, TB_Reg at);

    TB_API TB_Node* tb_function_get_node(TB_Function* f, TB_Reg r);
    TB_API int tb_function_get_label_count(TB_Function* f);

    TB_API bool tb_node_is_constant_zero(TB_Function* f, TB_Reg r);

    // Returns the size and alignment of a LOCAL node, both must
    // be valid addresses
    TB_API void tb_get_function_get_local_info(TB_Function* f, TB_Reg r, int* size, int* align);

    TB_API bool tb_node_is_phi_node(TB_Function* f, TB_Reg r);
    TB_API int tb_node_get_phi_width(TB_Function* f, TB_Reg r);
    TB_API TB_PhiInput* tb_node_get_phi_inputs(TB_Function* f, TB_Reg r);

    // is an IF node?
    TB_API bool tb_node_is_conditional(TB_Function* f, TB_Reg r);

    // is an IF, GOTO, RET, SWITCH, or LABEL node?
    TB_API bool tb_node_is_terminator(TB_Function* f, TB_Reg r);

    TB_API bool tb_node_is_label(TB_Function* f, TB_Reg r);

    TB_API TB_Reg tb_node_store_get_address(TB_Function* f, TB_Reg r);
    TB_API TB_Reg tb_node_store_get_value(TB_Function* f, TB_Reg r);

    TB_API TB_Reg tb_node_load_get_address(TB_Function* f, TB_Reg r);

    // These work for any floating point, comparison, or integer arithmatic ops
    TB_API TB_Reg tb_node_arith_get_left(TB_Function* f, TB_Reg r);
    TB_API TB_Reg tb_node_arith_get_right(TB_Function* f, TB_Reg r);

    ////////////////////////////////
    // Objects
    ////////////////////////////////
    TB_ArchiveFile* tb_archive_parse_lib(const TB_Slice file);
    void tb_archive_free(TB_ArchiveFile* archive);

    TB_ObjectFile* tb_object_parse_coff(const TB_Slice file);
    void tb_object_free(TB_ObjectFile* obj);

    #ifdef __cplusplus
}
#endif
#endif /* TB_CORE_H */