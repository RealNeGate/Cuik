// Glossary (because i don't know where else to put it)
//   IR   - Intermediate Representation
//   SoN  - Sea Of Nodes (https://www.oracle.com/technetwork/java/javase/tech/c2-ir95-150110.pdf)
//   SSA  - Single Static Assignment
//   VN   - Value Number
//   GVN  - Global Value Numbering
//   CSE  - Common Subexpression Elimination
//   CFG  - Control Flow Graph
//   DSE  - Dead Store Elimination
//   GCM  - Global Code Motion
//   GCF  - Global Congruence Finding
//   SROA - Scalar Replacement Of Aggregates
//   CCP  - Conditional Constant Propagation
//   SCCP - Sparse Conditional Constant Propagation
//   RPO  - Reverse PostOrder
//   RA   - Register Allocation
//   BB   - Basic Block
//   ZTC  - Zero Trip Count
//   MAF  - Monotone Analysis Framework
//   SCC  - Strongly Connected Components
//   MOP  - Meet Over all Paths
//   IPO  - InterProcedural Optimizations
//   RPC  - Return Program Counter
//   SLP  - Superword-Level Parallelism
#ifndef TB_CORE_H
#define TB_CORE_H

#include <prelude.h>

#define TB_PACKED_USERS 1

#ifndef TB_API
#  ifdef __cplusplus
#    define TB_EXTERN extern "C"
#  else
#    define TB_EXTERN
#  endif
#  ifdef TB_DLL
#    ifdef TB_IMPORT_DLL
#      define TB_API TB_EXTERN __declspec(dllimport)
#    else
#      define TB_API TB_EXTERN __declspec(dllexport)
#    endif
#  else
#    define TB_API TB_EXTERN
#  endif
#endif

// These are flags
typedef enum TB_ArithmeticBehavior {
    TB_ARITHMATIC_NONE = 0,
    TB_ARITHMATIC_NSW  = 1,
    TB_ARITHMATIC_NUW  = 2,
} TB_ArithmeticBehavior;

typedef enum TB_DebugFormat {
    TB_DEBUGFMT_NONE,

    TB_DEBUGFMT_DWARF,
    TB_DEBUGFMT_CODEVIEW,

    TB_DEBUGFMT_SDG,
} TB_DebugFormat;

typedef enum TB_CallingConv {
    TB_CDECL,
    TB_STDCALL,

    // clobbers all regs
    TB_TRAPCALL,
} TB_CallingConv;

typedef enum TB_FeatureSet_Generic {
    TB_FEATURE_FRAME_PTR  = (1u << 0u),
    TB_FEATURE_STACK_MAPS = (1u << 1u),
} TB_FeatureSet_Generic;

typedef enum TB_Linkage {
    TB_LINKAGE_PUBLIC,
    TB_LINKAGE_PRIVATE
} TB_Linkage;

typedef enum {
    TB_COMDAT_NONE,
    TB_COMDAT_MATCH_ANY,
} TB_ComdatType;

typedef enum TB_MemoryOrder {
    // atomic ops, unordered
    TB_MEM_ORDER_RELAXED,

    // acquire for loads:
    //   loads/stores from after this load cannot be reordered
    //   after this load.
    //
    // release for stores:
    //   loads/stores from before this store on this thread
    //   can't be reordered after this store.
    TB_MEM_ORDER_ACQ_REL,

    // acquire, release and total order across threads.
    TB_MEM_ORDER_SEQ_CST,
} TB_MemoryOrder;

typedef enum TB_DataTypeEnum {
    TB_TAG_VOID,
    // Boolean
    TB_TAG_BOOL,
    // Integers
    TB_TAG_I8,
    TB_TAG_I16,
    TB_TAG_I32,
    TB_TAG_I64,
    // Pointers
    TB_TAG_PTR,
    // Floating point numbers
    TB_TAG_F32,
    TB_TAG_F64,
    // SIMD vectors, note that not all sizes are supported on all
    // platforms (unlike every other type)
    TB_TAG_V64,
    TB_TAG_V128,
    TB_TAG_V256,
    TB_TAG_V512,
    // Control token
    TB_TAG_CONTROL,
    // memory effects (and I/O), you can think of it like a giant magic table of
    // all memory addressed with pointers.
    TB_TAG_MEMORY,
    // Tuples, these cannot be used in memory ops, just accessed via projections
    TB_TAG_TUPLE,
} TB_DataTypeEnum;

typedef union TB_DataType {
    struct {
        uint8_t type : 4;
        // for vectors, it's the element type.
        // for pointers, it's the address space (there's currently only one but
        // once GCs and funky hardware are introduced this will matter).
        uint8_t elem_or_addrspace : 4;
    };
    uint8_t raw;
} TB_DataType;
static_assert(sizeof(TB_DataType) == 1, "im expecting this to be a byte");

// classify data types
#define TB_IS_VOID_TYPE(x)     ((x).type == TB_TAG_VOID)
#define TB_IS_BOOL_TYPE(x)     ((x).type == TB_TAG_BOOL)
#define TB_IS_INTEGER_TYPE(x)  ((x).type >= TB_TAG_I8  && (x).type <= TB_TAG_I64)
#define TB_IS_FLOAT_TYPE(x)    ((x).type == TB_TAG_F32 || (x).type == TB_TAG_F64)
#define TB_IS_VECTOR_TYPE(x)   ((x).type >= TB_TAG_V64 && (x).type <= TB_TAG_V512)
#define TB_IS_POINTER_TYPE(x)  ((x).type == TB_TAG_PTR)
#define TB_IS_SCALAR_TYPE(x)   ((x).type <= TB_TAG_F64)
#define TB_IS_INT_OR_PTR(x)    ((x).type >= TB_TAG_I8  && (x).type <= TB_TAG_PTR)
#define TB_IS_INT_OR_PTR0(x)   (((x).type >= TB_TAG_I8  && (x).type <= TB_TAG_I64) || ((x).type == TB_TAG_PTR && (x).elem_or_addrspace == 0))
#define TB_IS_BOOL_INT_PTR(x)  ((x).type >= TB_TAG_BOOL && (x).type <= TB_TAG_PTR)

// accessors
#define TB_GET_INT_BITWIDTH(x) ((x).data)
#define TB_GET_FLOAT_FORMAT(x) ((x).data)
#define TB_GET_PTR_ADDRSPACE(x) ((x).data)

#include "tb_gen_public.h"

typedef struct TB_FeatureSet {
    uint64_t gen; // TB_FeatureSet_Generic
    union {
        #ifdef TB_HAS_X64
        TB_X86_FeatureSet x86;
        #endif
    };
} TB_FeatureSet;

// represents byte counts
typedef uint32_t TB_CharUnits;

// will get interned so each TB_Module has a unique identifier for the source file
typedef struct {
    // used by the debug info export
    int id;

    size_t len;
    uint8_t path[];
} TB_SourceFile;

typedef struct TB_Location {
    TB_SourceFile* file;
    int line, column;
    uint32_t pos;
} TB_Location;

// SO refers to shared objects which mean either shared libraries (.so or .dll)
// or executables (.exe or ELF executables)
typedef enum {
    // exports to the rest of the shared object
    TB_EXTERNAL_SO_LOCAL,

    // exports outside of the shared object
    TB_EXTERNAL_SO_EXPORT,
} TB_ExternalType;

typedef struct TB_Global            TB_Global;
typedef struct TB_External          TB_External;
typedef struct TB_Function          TB_Function;

typedef struct TB_Module            TB_Module;
typedef struct TB_DebugType         TB_DebugType;
typedef struct TB_ModuleSection     TB_ModuleSection;
typedef struct TB_FunctionPrototype TB_FunctionPrototype;

// TODO(NeGate): get rid of the lack of namespace here
typedef struct RegMask RegMask;

enum { TB_MODULE_SECTION_NONE = -1 };
typedef int32_t TB_ModuleSectionHandle;
typedef struct TB_Attrib TB_Attrib;

// target-specific, just a unique ID for the registers
typedef int TB_PhysicalReg;

// Thread local module state
typedef struct TB_ThreadInfo TB_ThreadInfo;

typedef enum {
    TB_SYMBOL_NONE,
    TB_SYMBOL_EXTERNAL,
    TB_SYMBOL_GLOBAL,
    TB_SYMBOL_FUNCTION,
    TB_SYMBOL_DEAD,
    TB_SYMBOL_MAX,
} TB_SymbolTag;

// Refers generically to objects within a module
//
// TB_Function, TB_Global, and TB_External are all subtypes of TB_Symbol
// and thus are safely allowed to cast into a symbol for operations.
typedef struct TB_Symbol {
    TB_SymbolTag tag;
    TB_Linkage linkage;

    // which thread info it's tied to (we may need to remove it, this
    // is used for that)
    TB_ThreadInfo* info;

    size_t name_length;
    char* name;

    // It's kinda a weird circular reference but yea
    TB_Module* module;

    // helpful for sorting and getting consistent builds
    uint64_t ordinal;

    union {
        // if we're JITing then this maps to the address of the symbol
        void* address;
        size_t symbol_id;
    };

    // after this point it's tag-specific storage
} TB_Symbol;

typedef struct TB_Node TB_Node;

typedef struct {
    #if TB_PACKED_USERS
    uint64_t _n    : 48;
    uint64_t _slot : 16;
    #else
    TB_Node* _n;
    int _slot;
    #endif
} TB_User;

struct TB_Node {
    TB_NodeType type;
    TB_DataType dt;

    uint16_t input_cap;
    uint16_t input_count;

    uint16_t user_cap;
    uint16_t user_count;

    // makes it easier to track in graph walks
    uint32_t gvn;
    // def-use edges, unordered
    TB_User* users;
    // ordered use-def edges, jolly ol' semantics.
    //   after input_count (and up to input_cap) goes an unordered set of nodes which
    //   act as extra deps, this is where anti-deps and other scheduling related edges
    //   are placed. stole this trick from Cliff... ok if you look at my compiler impl
    //   stuff it's either gonna be like trad compiler stuff, Cnile, LLVM or Cliff that's
    //   just how i learned :p
    TB_Node** inputs;

    char extra[];
};

// These are the extra data in specific nodes
#define TB_NODE_GET_EXTRA(n)         ((void*) n->extra)
#define TB_NODE_GET_EXTRA_T(n, T)    ((T*) (n)->extra)
#define TB_NODE_SET_EXTRA(n, T, ...) (*((T*) (n)->extra) = (T){ __VA_ARGS__ })

// this represents switch (many targets), if (one target)
typedef struct { // TB_BRANCH
    uint64_t total_hits;
    size_t succ_count;
} TB_NodeBranch;

typedef struct { // TB_IF
    float prob; // probability of hitting the true case
} TB_NodeIf;

typedef struct { // TB_MACH_COPY
    RegMask* use;
    RegMask* def;
} TB_NodeMachCopy;

typedef struct { // TB_MACH_TEMP
    RegMask* def;
} TB_NodeMachTemp;

typedef struct { // TB_PROJ
    int index;
} TB_NodeProj;

typedef struct { // TB_SYMBOL_TABLE
    bool complete;
} TB_NodeSymbolTable;

typedef struct { // TB_MACH_PROJ
    int index;
    RegMask* def;
} TB_NodeMachProj;

typedef struct { // TB_MACH_SYMBOL
    TB_Symbol* sym;
} TB_NodeMachSymbol;

typedef struct { // TB_BRANCH_PROJ
    int index;
    uint64_t taken;
    int64_t key;
} TB_NodeBranchProj;

typedef struct { // TB_ICONST
    uint64_t value;
} TB_NodeInt;

typedef struct { // any compare operator
    TB_DataType cmp_dt;
} TB_NodeCompare;

typedef struct { // any integer binary operator
    TB_ArithmeticBehavior ab;
} TB_NodeBinopInt;

typedef struct {
    TB_CharUnits align;
} TB_NodeMemAccess;

typedef struct { // TB_DEBUG_LOCATION
    TB_SourceFile* file;
    int line, column;
} TB_NodeDbgLoc;

typedef struct {
    int level;
} TB_NodePrefetch;

typedef struct {
    TB_CharUnits size, align;

    bool has_split;

    // used when machine-ifying it
    int stack_pos;

    // dbg info
    char* name;
    TB_DebugType* type;
} TB_NodeLocal;

typedef struct {
    float value;
} TB_NodeFloat32;

typedef struct {
    double value;
} TB_NodeFloat64;

typedef struct {
    TB_Symbol* sym;
} TB_NodeSymbol;

typedef struct {
    TB_MemoryOrder order;
    TB_MemoryOrder order2;
} TB_NodeAtomic;

typedef struct {
    void* userdata;
    int saved_val_count;
} TB_NodeSafepoint;

typedef struct {
    TB_NodeSafepoint super;
    TB_FunctionPrototype* proto;
    int proj_count;
} TB_NodeCall;

typedef struct {
    TB_NodeSafepoint super;
    TB_FunctionPrototype* proto;
} TB_NodeTailcall;

typedef struct {
    int width;
    int indices[0];
} TB_NodeVShuffle;

typedef struct {
    const char* tag;

    // used for IR building
    TB_Node *mem_in;
} TB_NodeRegion;

typedef struct TB_MultiOutput {
    size_t count;
    union {
        // count = 1
        TB_Node* single;
        // count > 1
        TB_Node** multiple;
    };
} TB_MultiOutput;
#define TB_MULTI_OUTPUT(o) ((o).count > 1 ? (o).multiple : &(o).single)

typedef struct {
    int64_t key;
    TB_Node* value;
} TB_SwitchEntry;

typedef struct {
    // if the top bit is set, we're referring to a
    // physical register.
    uint32_t value;
    // user-defined reference type data
    void* metadata;
} TB_StackMapEntry;

typedef struct TB_Safepoint {
    TB_Function* func;
    TB_Node* node; // type == TB_SAFEPOINT
    void* userdata;

    uint32_t ip; // relative to the function body.

    size_t count;
    uint64_t has_refs[];
} TB_Safepoint;

typedef enum {
    TB_MODULE_SECTION_WRITE = 1,
    TB_MODULE_SECTION_EXEC  = 2,
    TB_MODULE_SECTION_TLS   = 4,
} TB_ModuleSectionFlags;

typedef void (*TB_InlineAsmRA)(TB_Node* n, void* ctx);

// This is the function that'll emit bytes from a TB_INLINE_ASM node
typedef size_t (*TB_InlineAsmEmit)(TB_Node* n, void* ctx, size_t out_cap, uint8_t* out);

typedef struct {
    void* ctx;
    TB_InlineAsmRA   ra;
    TB_InlineAsmEmit emit;
} TB_NodeInlineAsm;

// *******************************
// Public macros
// *******************************
#ifdef __cplusplus

#define TB_TYPE_TUPLE   TB_DataType{ { TB_TAG_TUPLE      } }
#define TB_TYPE_CONTROL TB_DataType{ { TB_TAG_CONTROL    } }
#define TB_TYPE_VOID    TB_DataType{ { TB_TAG_VOID       } }
#define TB_TYPE_BOOL    TB_DataType{ { TB_TAG_BOOL       } }
#define TB_TYPE_I8      TB_DataType{ { TB_TAG_I8         } }
#define TB_TYPE_I16     TB_DataType{ { TB_TAG_I16        } }
#define TB_TYPE_I32     TB_DataType{ { TB_TAG_I32        } }
#define TB_TYPE_I64     TB_DataType{ { TB_TAG_I64        } }
#define TB_TYPE_F32     TB_DataType{ { TB_TAG_F32        } }
#define TB_TYPE_F64     TB_DataType{ { TB_TAG_F64        } }
#define TB_TYPE_PTR     TB_DataType{ { TB_TAG_PTR        } }
#define TB_TYPE_MEMORY  TB_DataType{ { TB_TAG_MEMORY, 0  } }
#define TB_TYPE_PTRN(N) TB_DataType{ { TB_TAG_PTR,   (N) } }

#else

#define TB_TYPE_TUPLE   (TB_DataType){ { TB_TAG_TUPLE      } }
#define TB_TYPE_CONTROL (TB_DataType){ { TB_TAG_CONTROL    } }
#define TB_TYPE_VOID    (TB_DataType){ { TB_TAG_VOID       } }
#define TB_TYPE_BOOL    (TB_DataType){ { TB_TAG_BOOL       } }
#define TB_TYPE_I8      (TB_DataType){ { TB_TAG_I8         } }
#define TB_TYPE_I16     (TB_DataType){ { TB_TAG_I16        } }
#define TB_TYPE_I32     (TB_DataType){ { TB_TAG_I32        } }
#define TB_TYPE_I64     (TB_DataType){ { TB_TAG_I64        } }
#define TB_TYPE_F32     (TB_DataType){ { TB_TAG_F32        } }
#define TB_TYPE_F64     (TB_DataType){ { TB_TAG_F64        } }
#define TB_TYPE_PTR     (TB_DataType){ { TB_TAG_PTR        } }
#define TB_TYPE_MEMORY  (TB_DataType){ { TB_TAG_MEMORY, 0  } }
#define TB_TYPE_PTRN(N) (TB_DataType){ { TB_TAG_PTR,   (N) } }

#endif

typedef enum TB_FunctionAttribs {
    TB_FUNCTION_NOINLINE = 1,
} TB_FunctionAttribs;

// defined in common/arena.h
#ifndef TB_OPAQUE_ARENA_DEF
#define TB_OPAQUE_ARENA_DEF
typedef struct TB_ArenaChunk TB_ArenaChunk;
typedef struct {
    TB_ArenaChunk* top;

    #ifndef NDEBUG
    const char* tag;
    uint32_t allocs;
    uint32_t alloc_bytes;
    #endif
} TB_Arena;

typedef struct TB_ArenaSavepoint {
    TB_ArenaChunk* top;
    char* avail;
} TB_ArenaSavepoint;
#endif // TB_OPAQUE_ARENA_DEF

TB_API void tb_arena_create(TB_Arena* restrict arena, const char* optional_tag);
TB_API void tb_arena_destroy(TB_Arena* restrict arena);
TB_API void tb_arena_clear(TB_Arena* restrict arena);
TB_API bool tb_arena_is_empty(TB_Arena* restrict arena);
TB_API TB_ArenaSavepoint tb_arena_save(TB_Arena* arena);
TB_API void tb_arena_restore(TB_Arena* arena, TB_ArenaSavepoint sp);

////////////////////////////////
// Module management
////////////////////////////////
// Creates a module with the correct target and settings
TB_API TB_Module* tb_module_create(TB_Arch arch, TB_System sys, bool is_jit);

// Creates a module but defaults on the architecture and system based on the host machine
TB_API TB_Module* tb_module_create_for_host(bool is_jit);

// Frees all resources for the TB_Module and it's functions, globals and
// compiled code.
TB_API void tb_module_destroy(TB_Module* m);

TB_API void tb_module_use_cc_gc(TB_Module* m, TB_Symbol* phase_control_sym);

// When targetting windows & thread local storage, you'll need to bind a tls index
// which is usually just a global that the runtime support has initialized, if you
// dont and the tls_index is used, it'll crash
TB_API void tb_module_set_tls_index(TB_Module* m, ptrdiff_t len, const char* name);

TB_API void tb_module_enable_chkstk(TB_Module* m);

// not thread-safe
TB_API TB_ModuleSectionHandle tb_module_create_section(TB_Module* m, ptrdiff_t len, const char* name, TB_ModuleSectionFlags flags, TB_ComdatType comdat);

////////////////////////////////
// Compiled code introspection
////////////////////////////////
enum { TB_ASSEMBLY_CHUNK_CAP = 4*1024 - sizeof(size_t[2]) };

typedef struct TB_Assembly TB_Assembly;
struct TB_Assembly {
    TB_Assembly* next;

    // nice chunk of text here
    size_t length;
    char data[];
};

// this is where the machine code and other relevant pieces go.
typedef struct TB_FunctionOutput TB_FunctionOutput;

TB_API void tb_output_print_asm(TB_FunctionOutput* out, FILE* fp);

TB_API uint8_t* tb_output_get_code(TB_FunctionOutput* out, size_t* out_length);

// returns NULL if there's no line info
TB_API TB_Location* tb_output_get_locations(TB_FunctionOutput* out, size_t* out_count);

// returns NULL if no assembly was generated
TB_API TB_Assembly* tb_output_get_asm(TB_FunctionOutput* out);

// this is relative to the start of the function (the start of the prologue)
TB_API TB_Safepoint* tb_safepoint_get(TB_Function* f, uint32_t relative_ip);

////////////////////////////////
// Disassembler
////////////////////////////////
typedef struct TB_Disasm {
    void* ctx;

    // Input stream
    size_t in_len;
    size_t in_curr;
    const uint8_t* in;

    // Output stream
    size_t out_len;
    size_t out_curr;
    char* out;

    // symbol_handler will be called any time there's a constant
    // or offset which the disassembler can pretty print.
    //
    // field_pos is measured in bits because RISC processors like to have ranges in funky places
    bool (*symbol_handler)(struct TB_Disasm* disasm, int inst_length, uint64_t field, int field_pos, int field_len, bool is_offset);
} TB_Disasm;

TB_API bool tb_disasm_outf(TB_Disasm* disasm, const char* fmt, ...);
TB_API ptrdiff_t tb_disasm_print(TB_Arch arch, TB_Disasm* disasm, bool has_relocs);

////////////////////////////////
// JIT compilation
////////////////////////////////
typedef struct TB_JIT TB_JIT;

#ifdef EMSCRIPTEN
TB_API void* tb_jit_wasm_obj(TB_Arena* arena, TB_Function* f);
#else
typedef struct TB_Stacklet TB_Stacklet;

// passing 0 to jit_heap_capacity will default to 4MiB
TB_API TB_JIT* tb_jit_begin(TB_Module* m, size_t jit_heap_capacity);
TB_API void* tb_jit_place_function(TB_JIT* jit, TB_Function* f);
TB_API void* tb_jit_place_global(TB_JIT* jit, TB_Global* g);
TB_API void* tb_jit_alloc_obj(TB_JIT* jit, size_t size, size_t align);
TB_API void tb_jit_free_obj(TB_JIT* jit, void* ptr);
TB_API void tb_jit_dump_heap(TB_JIT* jit);
TB_API void tb_jit_end(TB_JIT* jit);

typedef struct {
    void* tag;
    uint32_t offset;
} TB_ResolvedAddr;

typedef void (*TB_JIT_ExceptionHandler)(TB_JIT* jit, TB_Stacklet* stack, void* sp, void* pc);

TB_API void* tb_jit_resolve_addr(TB_JIT* jit, void* ptr, uint32_t* offset);
TB_API void* tb_jit_get_code_ptr(TB_Function* f);

// you can take an tag an allocation, fresh space for random userdata :)
TB_API void tb_jit_tag_object(TB_JIT* jit, void* ptr, void* tag);

// Debugger stuff
TB_API TB_Stacklet* tb_jit_thread_create(TB_JIT* jit, size_t ud_size, size_t stack_limit);

// offsetof user_data in the TB_Stacklet
TB_API size_t tb_jit_thread_userdata(void);

TB_API bool tb_jit_thread_call(TB_Stacklet* stacklet, void* pc, uint64_t* ret, size_t arg_count, void** args);
#endif

////////////////////////////////
// Exporter
////////////////////////////////
// Export buffers are generated in chunks because it's easier, usually the
// chunks are "massive" (representing some connected piece of the buffer)
// but they don't have to be.
typedef struct TB_ExportChunk TB_ExportChunk;
struct TB_ExportChunk {
    TB_ExportChunk* next;
    size_t pos, size;
    uint8_t data[];
};

typedef struct {
    size_t total;
    TB_ExportChunk *head, *tail;
} TB_ExportBuffer;

TB_API TB_ExportBuffer tb_module_object_export(TB_Module* m, TB_Arena* dst_arena, TB_DebugFormat debug_fmt);
TB_API bool tb_export_buffer_to_file(TB_ExportBuffer buffer, const char* path);

////////////////////////////////
// Symbols
////////////////////////////////
TB_API TB_Symbol* tb_extern_create(TB_Module* m, ptrdiff_t len, const char* name, TB_ExternalType type);

TB_API TB_SourceFile* tb_get_source_file(TB_Module* m, ptrdiff_t len, const char* path);

////////////////////////////////
// Function Prototypes
////////////////////////////////
typedef struct TB_PrototypeParam {
    TB_DataType dt;
    TB_DebugType* debug_type;

    // does not apply for returns
    const char* name;
} TB_PrototypeParam;

struct TB_FunctionPrototype {
    // header
    TB_CallingConv call_conv;
    uint16_t return_count, param_count;
    bool has_varargs;

    // params are directly followed by returns
    TB_PrototypeParam params[];
};
#define TB_PROTOTYPE_RETURNS(p) ((p)->params + (p)->param_count)

// creates a function prototype used to define a function's parameters and returns.
//
// function prototypes do not get freed individually and last for the entire run
// of the backend, they can also be reused for multiple functions which have
// matching signatures.
TB_API TB_FunctionPrototype* tb_prototype_create(TB_Module* m, TB_CallingConv cc, size_t param_count, const TB_PrototypeParam* params, size_t return_count, const TB_PrototypeParam* returns, bool has_varargs);
TB_API TB_FunctionPrototype* tb_prototype_from_dbg(TB_Module* m, TB_DebugType* dbg);

// used for ABI parameter passing
typedef enum {
    // needs a direct value
    TB_PASSING_DIRECT,

    // needs an address to the value
    TB_PASSING_INDIRECT,

    // doesn't use this parameter
    TB_PASSING_IGNORE,
} TB_PassingRule;

TB_API TB_PassingRule tb_get_passing_rule_from_dbg(TB_Module* mod, TB_DebugType* param_type, bool is_return);

////////////////////////////////
// Globals
////////////////////////////////
TB_API TB_Global* tb_global_create(TB_Module* m, ptrdiff_t len, const char* name, TB_DebugType* dbg_type, TB_Linkage linkage);

// allocate space for the global
TB_API void tb_global_set_storage(TB_Module* m, TB_ModuleSectionHandle section, TB_Global* global, size_t size, size_t align, size_t max_objects);

// returns a buffer which the user can fill to then have represented in the initializer
TB_API void* tb_global_add_region(TB_Module* m, TB_Global* global, size_t offset, size_t size);

// places a relocation for a global at offset, the size of the relocation
// depends on the pointer size
TB_API void tb_global_add_symbol_reloc(TB_Module* m, TB_Global* global, size_t offset, TB_Symbol* symbol);

TB_API TB_ModuleSectionHandle tb_module_get_text(TB_Module* m);
TB_API TB_ModuleSectionHandle tb_module_get_rdata(TB_Module* m);
TB_API TB_ModuleSectionHandle tb_module_get_data(TB_Module* m);
TB_API TB_ModuleSectionHandle tb_module_get_tls(TB_Module* m);

////////////////////////////////
// Function Attributes
////////////////////////////////
// These are parts of a function that describe metadata for instructions
TB_API void tb_function_attrib_variable(TB_Function* f, TB_Node* n, TB_Node* parent, ptrdiff_t len, const char* name, TB_DebugType* type);
TB_API void tb_function_attrib_scope(TB_Function* f, TB_Node* n, TB_Node* parent);

////////////////////////////////
// Debug info Generation
////////////////////////////////
TB_API TB_DebugType* tb_debug_get_void(TB_Module* m);
TB_API TB_DebugType* tb_debug_get_bool(TB_Module* m);
TB_API TB_DebugType* tb_debug_get_integer(TB_Module* m, bool is_signed, int bits);
TB_API TB_DebugType* tb_debug_get_float32(TB_Module* m);
TB_API TB_DebugType* tb_debug_get_float64(TB_Module* m);
TB_API TB_DebugType* tb_debug_create_ptr(TB_Module* m, TB_DebugType* base);
TB_API TB_DebugType* tb_debug_create_array(TB_Module* m, TB_DebugType* base, size_t count);
TB_API TB_DebugType* tb_debug_create_alias(TB_Module* m, TB_DebugType* base, ptrdiff_t len, const char* tag);
TB_API TB_DebugType* tb_debug_create_struct(TB_Module* m, ptrdiff_t len, const char* tag);
TB_API TB_DebugType* tb_debug_create_union(TB_Module* m, ptrdiff_t len, const char* tag);
TB_API TB_DebugType* tb_debug_create_field(TB_Module* m, TB_DebugType* type, ptrdiff_t len, const char* name, TB_CharUnits offset);

// returns the array you need to fill with fields
TB_API TB_DebugType** tb_debug_record_begin(TB_Module* m, TB_DebugType* type, size_t count);
TB_API void tb_debug_record_end(TB_DebugType* type, TB_CharUnits size, TB_CharUnits align);

TB_API TB_DebugType* tb_debug_create_func(TB_Module* m, TB_CallingConv cc, size_t param_count, size_t return_count, bool has_varargs);

TB_API TB_DebugType* tb_debug_field_type(TB_DebugType* type);

TB_API size_t tb_debug_func_return_count(TB_DebugType* type);
TB_API size_t tb_debug_func_param_count(TB_DebugType* type);

// you'll need to fill these if you make a function
TB_API TB_DebugType** tb_debug_func_params(TB_DebugType* type);
TB_API TB_DebugType** tb_debug_func_returns(TB_DebugType* type);

////////////////////////////////
// Symbols
////////////////////////////////
// returns NULL if the tag doesn't match
TB_API TB_Function* tb_symbol_as_function(TB_Symbol* s);
TB_API TB_External* tb_symbol_as_external(TB_Symbol* s);
TB_API TB_Global* tb_symbol_as_global(TB_Symbol* s);

////////////////////////////////
// Function IR Generation
////////////////////////////////
TB_API TB_DataType tb_data_type_ptr_int(TB_Module* m);
TB_API void tb_get_data_type_size(TB_Module* mod, TB_DataType dt, size_t* size, size_t* align);

// if section is NULL, default to .text
TB_API TB_Function* tb_function_create(TB_Module* m, ptrdiff_t len, const char* name, TB_Linkage linkage);
TB_API void tb_function_set_features(TB_Function* f, const TB_FeatureSet* features);
TB_API void tb_function_set_attrs(TB_Function* f, TB_FunctionAttribs attrs);
TB_API void tb_function_destroy(TB_Function* f);

// returns 0 on success
TB_API int tb_features_parse(TB_FeatureSet* out, TB_Module* m, const char* str);

TB_API TB_Arena* tb_function_get_arena(TB_Function* f, int i);

// if len is -1, it's null terminated
TB_API void tb_symbol_set_name(TB_Symbol* s, ptrdiff_t len, const char* name);

TB_API void tb_symbol_bind_ptr(TB_Symbol* s, void* ptr);
TB_API const char* tb_symbol_get_name(TB_Symbol* s);

// if arena is NULL, defaults to module arena which is freed on tb_free_thread_resources
TB_API void tb_function_set_prototype(TB_Function* f, TB_ModuleSectionHandle section, TB_FunctionPrototype* p);
TB_API TB_FunctionPrototype* tb_function_get_prototype(TB_Function* f);

TB_API const char* tb_node_get_name(TB_NodeTypeEnum n_type);

////////////////////////////////
// Optimizer API
////////////////////////////////
// To avoid allocs, you can make a worklist and keep it across multiple functions so long
// as they're not trying to use it at the same time.
typedef struct TB_Worklist TB_Worklist;

TB_API TB_Worklist* tb_worklist_alloc(void);
TB_API void tb_worklist_free(TB_Worklist* ws);

// if you decide during tb_opt that you wanna preserve the types, this is how you'd later free them.
TB_API void tb_opt_free_types(TB_Function* f);

// this will allocate the worklist, you can free worklist once you're done with analysis/transforms.
TB_API void tb_opt_push_all_nodes(TB_Function* f);
TB_API void tb_opt_dump_stats(TB_Function* f);

// returns GVN on a new node, returning either the same node or a duplicate node 'k'.
// it deletes 'n' if it's a duplicate btw.
TB_API TB_Node* tb_opt_gvn_node(TB_Function* f, TB_Node* n);
// returns isomorphic node that's run it's peepholes.
TB_API TB_Node* tb_opt_peep_node(TB_Function* f, TB_Node* n);

// Uses the two function arenas pretty heavily, may even flip their purposes (as a form
// of GC compacting)
//
// returns true if any graph rewrites were performed.
TB_API bool tb_opt(TB_Function* f, TB_Worklist* ws, bool preserve_types);

// print in SSA-CFG looking form (with BB params for the phis), if tmp is NULL it'll use the
// function's tmp arena
TB_API void tb_print(TB_Function* f);
TB_API void tb_print_dumb(TB_Function* f);
TB_API void tb_print_svg(TB_Function* f);

uint64_t tb_interpret(TB_Function* f, TB_Worklist* ws, uint64_t* params);

typedef enum {
    // Ian Rogers style allocator:
    //   "Efficient global register allocation" (2020)
    TB_RA_ROGERS,

    // Briggs-Chaitin style allocator:
    //   "Register Allocation via Graph Coloring" (1992)
    TB_RA_BRIGGS,
} TB_CodegenRA;

// codegen:
//   output goes at the top of the code_arena, feel free to place multiple functions
//   into the same code arena (although arenas aren't thread-safe you'll want one per thread
//   at least)
//
//   if code_arena is NULL, the IR arena will be used.
TB_API TB_FunctionOutput* tb_codegen(TB_Function* f, TB_CodegenRA ra, TB_Worklist* ws, TB_Arena* code_arena, bool emit_asm);

// interprocedural optimizer iter
typedef struct TPool TPool;
TB_API bool tb_module_ipo(TB_Module* m, TPool* pool);

// global perf stats
TB_API void tb_dump_stats(void);

////////////////////////////////
// Cooler IR building
////////////////////////////////
typedef struct TB_GraphBuilder TB_GraphBuilder;
enum { TB_GRAPH_BUILDER_PARAMS = 0 };

// if ws != NULL, i'll run the peepholes while you're constructing nodes. why? because it
// avoids making junk nodes before they become a problem for memory bandwidth.
TB_API TB_GraphBuilder* tb_builder_enter(TB_Function* f, TB_ModuleSectionHandle section, TB_FunctionPrototype* proto, TB_Worklist* ws);

// parameter's addresses are available through the tb_builder_param_addr, they're not tracked as mutable vars.
TB_API TB_GraphBuilder* tb_builder_enter_from_dbg(TB_Function* f, TB_ModuleSectionHandle section, TB_DebugType* dbg, TB_Worklist* ws);

TB_API void tb_builder_exit(TB_GraphBuilder* g);
TB_API TB_Node* tb_builder_param_addr(TB_GraphBuilder* g, int i);

TB_API TB_Node* tb_builder_bool(TB_GraphBuilder* g, bool x);
TB_API TB_Node* tb_builder_uint(TB_GraphBuilder* g, TB_DataType dt, uint64_t x);
TB_API TB_Node* tb_builder_sint(TB_GraphBuilder* g, TB_DataType dt, int64_t x);
TB_API TB_Node* tb_builder_float32(TB_GraphBuilder* g, float imm);
TB_API TB_Node* tb_builder_float64(TB_GraphBuilder* g, double imm);
TB_API TB_Node* tb_builder_symbol(TB_GraphBuilder* g, TB_Symbol* sym);
TB_API TB_Node* tb_builder_string(TB_GraphBuilder* g, ptrdiff_t len, const char* str);

// works with type: AND, OR, XOR, ADD, SUB, MUL, SHL, SHR, SAR, ROL, ROR, UDIV, SDIV, UMOD, SMOD.
// note that arithmetic behavior is irrelevant for some of the operations (but 0 is always a good default).
TB_API TB_Node* tb_builder_binop_int(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b, TB_ArithmeticBehavior ab);
TB_API TB_Node* tb_builder_binop_float(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b);

TB_API TB_Node* tb_builder_select(TB_GraphBuilder* g, TB_Node* cond, TB_Node* a, TB_Node* b);
TB_API TB_Node* tb_builder_cast(TB_GraphBuilder* g, TB_DataType dt, int type, TB_Node* src);

// ( a -- b )
TB_API TB_Node* tb_builder_unary(TB_GraphBuilder* g, int type, TB_Node* src);
TB_API TB_Node* tb_builder_va_start(TB_GraphBuilder* g, TB_Node* src);

TB_API TB_Node* tb_builder_neg(TB_GraphBuilder* g, TB_Node* src);
TB_API TB_Node* tb_builder_not(TB_GraphBuilder* g, TB_Node* src);

// ( a b -- c )
TB_API TB_Node* tb_builder_cmp(TB_GraphBuilder* g, int type, TB_Node* a, TB_Node* b);

// pointer arithmetic
//   base + index*stride
TB_API TB_Node* tb_builder_ptr_array(TB_GraphBuilder* g, TB_Node* base, TB_Node* index, int64_t stride);
//   base + offset
TB_API TB_Node* tb_builder_ptr_member(TB_GraphBuilder* g, TB_Node* base, int64_t offset);

// memory
TB_API TB_Node* tb_builder_load(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_DataType dt, TB_Node* addr, TB_CharUnits align, bool is_volatile);
TB_API TB_Node* tb_builder_store(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* addr, TB_Node* val, TB_CharUnits align, bool is_volatile);
TB_API void tb_builder_memcpy(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* src, TB_Node* size, TB_CharUnits align, bool is_volatile);
TB_API void tb_builder_memset(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* val, TB_Node* size, TB_CharUnits align, bool is_volatile);
TB_API void tb_builder_memzero(TB_GraphBuilder* g, int mem_var, bool ctrl_dep, TB_Node* dst, TB_Node* size, TB_CharUnits align, bool is_volatile);

// returns initially loaded value
TB_API TB_Node* tb_builder_atomic_rmw(TB_GraphBuilder* g, int mem_var, int op, TB_Node* addr, TB_Node* val, TB_MemoryOrder order);
TB_API TB_Node* tb_builder_atomic_load(TB_GraphBuilder* g, int mem_var, TB_DataType dt, TB_Node* addr, TB_MemoryOrder order);

// splitting/merging:
//   splits the 'in_mem' variable, this writes over in_mem with a split
//   and 'split_count' number of extra paths at 'variable[returned value]'
TB_API int tb_builder_split_mem(TB_GraphBuilder* g, int in_mem, int split_count, TB_Node** out_split);
//   this will merge the memory effects back into out_mem, split_vars being the result of a tb_builder_split_mem(...)
TB_API void tb_builder_merge_mem(TB_GraphBuilder* g, int out_mem, int split_count, int split_vars, TB_Node* split);

TB_API void tb_builder_loc(TB_GraphBuilder* g, int mem_var, TB_SourceFile* file, int line, int column);

// function call
TB_API TB_Node** tb_builder_call(TB_GraphBuilder* g, TB_FunctionPrototype* proto, int mem_var, TB_Node* target, int arg_count, TB_Node** args);
TB_API TB_Node* tb_builder_syscall(TB_GraphBuilder* g, TB_DataType dt, int mem_var, TB_Node* target, int arg_count, TB_Node** args);

// locals (variables but as stack vars)
TB_API TB_Node* tb_builder_local(TB_GraphBuilder* g, TB_CharUnits size, TB_CharUnits align);
TB_API void tb_builder_local_dbg(TB_GraphBuilder* g, TB_Node* n, ptrdiff_t len, const char* name, TB_DebugType* type);

TB_API TB_Node* tb_builder_frame_ptr(TB_GraphBuilder* g);
TB_API TB_Node* tb_builder_jit_thread_ptr(TB_GraphBuilder* g);

// variables:
//   just gives you the ability to construct mutable names, from
//   there we just slot in the phis and such for you :)
TB_API int tb_builder_decl(TB_GraphBuilder* g, TB_Node* label);
TB_API TB_Node* tb_builder_get_var(TB_GraphBuilder* g, int id);
TB_API void tb_builder_set_var(TB_GraphBuilder* g, int id, TB_Node* v);

// control flow primitives:
//   makes a region we can jump to (generally for forward jumps)
TB_API TB_Node* tb_builder_label_make(TB_GraphBuilder* g);
TB_API TB_Node* tb_builder_label_make2(TB_GraphBuilder* g, TB_Node* label, bool has_backward_jumps);
//   once a label is complete you can no longer insert jumps to it, the phis
//   are placed and you can then insert code into the label's body.
TB_API void tb_builder_label_complete(TB_GraphBuilder* g, TB_Node* label);
//   begin building on the label (has to be completed now), returns old label
TB_API TB_Node* tb_builder_label_set(TB_GraphBuilder* g, TB_Node* label);
//   just makes a label from an existing label (used when making the loop body defs)
TB_API TB_Node* tb_builder_label_clone(TB_GraphBuilder* g, TB_Node* label);
//   active label
TB_API TB_Node* tb_builder_label_get(TB_GraphBuilder* g);
//   number of predecessors at that point in time
TB_API int tb_builder_label_pred_count(TB_GraphBuilder* g, TB_Node* label);
//   kill node
TB_API void tb_builder_label_kill(TB_GraphBuilder* g, TB_Node* label);
//   writes to the paths array the symbol tables for the branch.
//   [0] is the true case and [1] is the false case.
TB_API TB_Node* tb_builder_if(TB_GraphBuilder* g, TB_Node* cond, TB_Node* paths[2]);
//   begins empty switch statement, we can add cases as we go.
//   returns the symbol table we use to instatiate the cases.
TB_API TB_Node* tb_builder_switch(TB_GraphBuilder* g, TB_Node* cond);
//   returns the symbol table for the newly created case.
TB_API TB_Node* tb_builder_def_case(TB_GraphBuilder* g, TB_Node* br_syms, int prob);
//   returns the symbol table for the newly created case.
TB_API TB_Node* tb_builder_key_case(TB_GraphBuilder* g, TB_Node* br_syms, uint64_t key, int prob);
//   unconditional jump to target
TB_API void tb_builder_br(TB_GraphBuilder* g, TB_Node* target);
//   forward and backward branch target
TB_API TB_Node* tb_builder_loop(TB_GraphBuilder* g);
//   explicit phi construction
TB_API TB_Node* tb_builder_phi(TB_GraphBuilder* g, int val_count, TB_Node** vals);

// technically TB has multiple returns, in practice it's like 2 regs before
// ABI runs out of shit.
TB_API void tb_builder_ret(TB_GraphBuilder* g, int mem_var, int arg_count, TB_Node** args);
TB_API void tb_builder_unreachable(TB_GraphBuilder* g, int mem_var);
TB_API void tb_builder_trap(TB_GraphBuilder* g, int mem_var);
TB_API void tb_builder_debugbreak(TB_GraphBuilder* g, int mem_var);
//   every arg has their lifetime stretched to this point.
TB_API void tb_builder_blackhole(TB_GraphBuilder* g, int arg_count, TB_Node** args);

// allows you to define multiple entry points
TB_API void tb_builder_entry_fork(TB_GraphBuilder* g, int count, TB_Node* paths[]);

// general intrinsics
TB_API TB_Node* tb_builder_cycle_counter(TB_GraphBuilder* g);
TB_API TB_Node* tb_builder_prefetch(TB_GraphBuilder* g, TB_Node* addr, int level);

// x86 Intrinsics
TB_API TB_Node* tb_builder_x86_ldmxcsr(TB_GraphBuilder* g, TB_Node* a);
TB_API TB_Node* tb_builder_x86_stmxcsr(TB_GraphBuilder* g);

////////////////////////////////
// IR access
////////////////////////////////
TB_API bool tb_node_is_constant_non_zero(TB_Node* n);
TB_API bool tb_node_is_constant_zero(TB_Node* n);

#endif /* TB_CORE_H */
