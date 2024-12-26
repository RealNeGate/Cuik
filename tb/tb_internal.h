#pragma once

// Windows likes it's secure functions, i kinda do too
// but only sometimes and this isn't one of them
#if defined(_WIN32) && !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "tb.h"
#include "tb_formats.h"
#include <stdalign.h>

#if defined(_MSC_VER) && !defined(__clang__)
#include <immintrin.h>
#define thread_local __declspec(thread)
#define alignas(x) __declspec(align(x))
#else
#define thread_local _Thread_local
#endif

////////////////////////////////
// Random toggles
////////////////////////////////
#define TB_OPTDEBUG_STATS     0
#define TB_OPTDEBUG_PASSES    0
#define TB_OPTDEBUG_PEEP      0
#define TB_OPTDEBUG_SCCP      0
#define TB_OPTDEBUG_LOOP      0
#define TB_OPTDEBUG_SROA      0
#define TB_OPTDEBUG_GCM       0
#define TB_OPTDEBUG_MEM2REG   0
#define TB_OPTDEBUG_ISEL      0
#define TB_OPTDEBUG_CODEGEN   0
#define TB_OPTDEBUG_DATAFLOW  0
#define TB_OPTDEBUG_PLACEMENT 0
#define TB_OPTDEBUG_INLINE    0
#define TB_OPTDEBUG_REGALLOC  1
#define TB_OPTDEBUG_SLP       0
#define TB_OPTDEBUG_GVN       0
#define TB_OPTDEBUG_COMPACT   0
#define TB_OPTDEBUG_SCHEDULE  0
// for toggling ANSI colors
#define TB_OPTDEBUG_ANSI      1

#define TB_OPTDEBUG(cond) CONCAT(DO_IF_, CONCAT(TB_OPTDEBUG_, cond))

#define DO_IF(cond) CONCAT(DO_IF_, cond)
#define DO_IF_0(...)
#define DO_IF_1(...) __VA_ARGS__

#ifndef _WIN32
// NOTE(NeGate): I love how we assume that if it's not windows
// its just posix, these are the only options i guess
#include <pthread.h>
#include <sys/mman.h>
#include <unistd.h>
#endif

#include "tb_platform.h"
#include "dyn_array.h"
#include "arena_array.h"
#include "builtins.h"

#define NL_HASH_MAP_INLINE
#include <hash_map.h>
#include <new_hash_map.h>

#define NBHS_REALLOC cuik_realloc
#include <nbhs.h>

#include <perf.h>
#include <log.h>

#define TB_MIN(x, y) ((x) < (y) ? (x) : (y))
#define TB_MAX(x, y) ((x) > (y) ? (x) : (y))

#ifndef CONCAT
#define CONCAT_(x, y) x ## y
#define CONCAT(x, y) CONCAT_(x, y)
#endif

#include <arena.h>
#include "set.h"

#include <threads.h>
#include <stdatomic.h>

#define TB_DATA_TYPE_EQUALS(a, b) ((a).raw == (b).raw)

#ifdef CONFIG_HAS_LINKER
typedef struct TB_LinkerSectionPiece TB_LinkerSectionPiece;
#endif

// i love my linked lists don't i?
typedef struct TB_SymbolPatch TB_SymbolPatch;
struct TB_SymbolPatch {
    TB_SymbolPatch* next;
    uint32_t pos;
    bool internal; // handled already by the code gen's emit_call_patches
    TB_Symbol* target;
};

struct TB_External {
    TB_Symbol super;
    TB_ExternalType type;

    void* thunk; // JIT will cache a thunk here because it's helpful

    // if non-NULL, the external was resolved
    _Atomic(TB_Symbol*) resolved;
};

typedef struct TB_InitObj {
    enum {
        TB_INIT_OBJ_REGION,
        TB_INIT_OBJ_RELOC,
    } type;
    TB_CharUnits offset;
    union {
        struct {
            TB_CharUnits size;
            const void* ptr;
        } region;

        TB_Symbol* reloc;
    };
} TB_InitObj;

struct TB_Global {
    TB_Symbol super;
    TB_ModuleSectionHandle parent;

    // layout stuff
    void* address; // JIT-only
    uint32_t pos;
    TB_CharUnits size, align;

    // debug info
    TB_DebugType* dbg_type;

    // contents
    uint32_t obj_count, obj_capacity;
    TB_InitObj* objects;
};

struct TB_DebugType {
    enum {
        TB_DEBUG_TYPE_VOID,
        TB_DEBUG_TYPE_BOOL,

        TB_DEBUG_TYPE_UINT,
        TB_DEBUG_TYPE_INT,
        TB_DEBUG_TYPE_FLOAT32,
        TB_DEBUG_TYPE_FLOAT64,

        TB_DEBUG_TYPE_ARRAY,
        TB_DEBUG_TYPE_POINTER,

        // special types
        TB_DEBUG_TYPE_ALIAS,
        TB_DEBUG_TYPE_FIELD,

        // aggregates
        // TODO(NeGate): apparently codeview has cool vector and matrix types... yea
        TB_DEBUG_TYPE_STRUCT,
        TB_DEBUG_TYPE_UNION,

        TB_DEBUG_TYPE_FUNCTION,
    } tag;

    // debug-info target specific data
    union {
        struct {
            uint16_t type_id;
            uint16_t type_id_fwd; // used by records to manage forward decls
        };
    };

    // tag specific
    union {
        int int_bits;
        TB_DebugType* ptr_to;
        struct {
            TB_DebugType* base;
            size_t count;
        } array;
        struct {
            size_t len;
            char* name;
            TB_DebugType* type;
        } alias;
        struct {
            size_t len;
            char* name;
            TB_CharUnits offset;
            TB_DebugType* type;
        } field;
        struct TB_DebugTypeRecord {
            size_t len;
            char* tag;
            TB_CharUnits size, align;

            size_t count;
            TB_DebugType** members;
        } record;
        struct TB_DebugTypeFunc {
            TB_CallingConv cc;
            bool has_varargs;

            size_t param_count, return_count;
            TB_DebugType** params;
            TB_DebugType** returns;
        } func;
    };
};

// TODO(NeGate): support complex variable descriptions
// currently we only support stack relative
typedef struct {
    int32_t offset;
} TB_DebugValue;

typedef struct TB_StackSlot {
    const char* name;
    TB_DebugType* type;
    TB_DebugValue storage;
} TB_StackSlot;

typedef struct TB_Comdat {
    TB_ComdatType type;
    uint32_t reloc_count;
} TB_Comdat;

typedef struct COFF_UnwindInfo COFF_UnwindInfo;
typedef struct ICodeGen ICodeGen;

// it's a lattice element i'm not fucking typing that shit tho
typedef struct Lattice Lattice;

typedef struct TB_FunctionOutput {
    TB_Function* parent;
    TB_ModuleSectionHandle section;

    TB_Linkage linkage;

    uint8_t prologue_length;
    uint8_t epilogue_length;
    uint8_t nop_pads;

    TB_Assembly* asm_out;

    // if there's a TB_ENTRY_FORK, we've got multiple entries and this
    // holds the offsets. if entry_offsets is NULL, the only offset is 0.
    uint32_t* entry_offsets;

    uint64_t ordinal;
    uint64_t stack_usage;

    uint8_t* code;
    uint32_t code_size;
    uint32_t code_pos; // relative to the export-specific text section

    // export-specific
    union {
        uint32_t win32_unwind_info;
        uint32_t wasm_type;
    };

    // Part of the debug info
    DynArray(TB_Location) locations;
    ArenaArray(TB_Safepoint*) safepoints;

    // Relocations
    uint32_t patch_pos;
    uint32_t patch_count;

    TB_SymbolPatch* first_patch;
    TB_SymbolPatch* last_patch;

    DynArray(TB_StackSlot) stack_slots;
} TB_FunctionOutput;

struct TB_Worklist {
    DynArray(TB_Node*) items;

    // uses gvn as key
    size_t visited_cap; // in words
    uint64_t* visited;
};

typedef struct TB_LoopTree TB_LoopTree;
typedef struct TB_BasicBlock TB_BasicBlock;

struct TB_LoopTree {
    TB_LoopTree* parent;
    TB_LoopTree* next; // next sibling
    TB_LoopTree* kid;

    // start-of-BB nodes btw
    TB_Node* header;
    TB_Node* backedge;

    bool is_natural;
    int id;
    int depth;
};

// we have analysis stuff for computing BBs from our graphs, these aren't
// kept around at all times like an SSA-CFG compiler.
struct TB_BasicBlock {
    TB_Node* start;
    TB_Node* end;

    int pre_index;

    // during codegen, if the block is empty this is the
    // successive block where we'll actually point to
    int fwd;

    float freq;
    int dom_depth;
    TB_BasicBlock* dom;

    // dataflow
    Set gen, kill;
    Set live_in, live_out;

    // all nodes in the BB, usually unordered (when produced by GCM)
    // but becomes ordered during codegen
    ArenaArray(TB_Node*) items;
};

typedef struct TB_CFG {
    // not actually a loop lmao, i just need something at the
    // top of the loop tree
    TB_LoopTree* root_loop;

    ArenaArray(TB_LoopTree*) loops;
    ArenaArray(TB_BasicBlock) blocks; // RPO ordering
    NL_Map(TB_Node*, TB_BasicBlock*) node_to_block;
} TB_CFG;

typedef enum {
    IND_NE, IND_SLT, IND_SLE, IND_ULT, IND_ULE,
} TB_IndVarPredicate;

typedef struct {
    TB_Node* cond;
    TB_Node* phi;
    int64_t step;

    // neutral limit:
    //   while (ind != limit) IND_NE
    // forwards limit:
    //   while (ind <= limit) IND_LE
    //   while (ind <  limit) IND_LT
    // backwards limit:
    //   while (limit <= ind) IND_LE
    //   while (limit <  ind) IND_LT
    TB_IndVarPredicate pred;
    bool backwards;

    // end_cond is NULL, we're exitting based on some constant
    TB_Node* end_cond;
    uint64_t end_const;
} TB_InductionVar;

struct TB_Function {
    TB_Symbol super;
    TB_ModuleSectionHandle section;

    TB_DebugType* dbg_type;
    TB_FunctionPrototype* prototype;

    // raw parameters
    size_t param_count;
    TB_Node** params;

    // stores nodes, user lists & lattice elems.
    TB_Arena arena;
    // all the random allocs within passes
    TB_Arena tmp_arena;

    // used for the copy-compact decision making, not perfectly accurate
    size_t dead_node_bytes;

    size_t node_count;
    TB_Node* root_node;

    // for legacy builder
    TB_Trace trace;
    TB_Node* last_loc;

    // Optimizer related data
    struct {
        // how we track duplicates for GVN, it's possible to run while building the IR.
        NL_HashSet gvn_nodes;

        // it's what the peepholes are iterating on
        TB_Worklist* worklist;

        // track a lattice per node (basically all get one so a compact array works)
        size_t type_cap;
        Lattice** types;

        // some xforms like removing branches can
        // invalidate the loop tree.
        bool invalidated_loops;

        // we throw the results of scheduling here:
        //   [value number] -> TB_BasicBlock*
        size_t scheduled_n;
        TB_BasicBlock** scheduled;

        // used during loop opts mostly, it's just SoN doms
        size_t doms_n;
        TB_Node** doms;

        // nice stats
        struct {
            #if TB_OPTDEBUG_PEEP || TB_OPTDEBUG_SCCP || TB_OPTDEBUG_ISEL
            int time;
            #endif

            #if TB_OPTDEBUG_STATS
            int initial;
            int gvn_hit, gvn_tries;
            int *peeps, *identities, *rewrites, *constants, *opto_constants, *killed;
            #endif
        } stats;
    };

    // Compilation output
    union {
        void* compiled_pos;
        size_t compiled_symbol_id;
    };

    TB_FunctionOutput* output;
};

struct TB_ModuleSection {
    char* name;

    #ifdef CONFIG_HAS_LINKER
    TB_LinkerSectionPiece* piece;
    #endif

    int section_num;
    TB_ModuleSectionFlags flags;

    TB_Comdat comdat;

    // export-specific
    uint32_t export_flags;
    uint32_t name_pos;
    COFF_UnwindInfo* unwind;

    // this isn't computed until export time
    uint32_t raw_data_pos;
    uint32_t total_size;
    uint32_t reloc_count;
    uint32_t reloc_pos;

    DynArray(TB_Global*) globals;
    DynArray(TB_FunctionOutput*) funcs;
};

typedef struct {
    int len;
    char data[16];
} SmallConst;

// only next_in_module is ever mutated on multiple threads (when first attached)
struct TB_ThreadInfo {
    void* owner;
    _Atomic(TB_ThreadInfo*) next_in_module;

    TB_ThreadInfo* prev;
    TB_ThreadInfo* next;

    mtx_t* lock;

    // used for moving the start of the
    // linked list forward.
    TB_ThreadInfo** chain;

    TB_Arena perm_arena;
    TB_Arena tmp_arena;
};

typedef struct {
    size_t count;
    TB_External** data;
} ExportList;

struct TB_Module {
    bool is_jit;
    bool visited; // used by the linker
    ICodeGen* codegen;

    atomic_flag is_tls_defined;

    // we have a global lock since the arena can be accessed
    // from any thread.
    mtx_t lock;

    // thread info
    _Atomic(TB_ThreadInfo*) first_info_in_module;

    // small constants are interned because they
    // come up a lot.
    NL_Map(SmallConst, TB_Global*) global_interns;

    TB_ABI target_abi;
    TB_Arch target_arch;
    TB_System target_system;
    TB_FeatureSet features;
    ExportList exports;

    // TB_Symbol*
    NBHS symbols;

    // This is a hack for windows since they've got this idea
    // of a _tls_index
    TB_Symbol* tls_index_extern;
    TB_Symbol* chkstk_extern;

    // interning lattice
    NBHS lattice_elements;

    _Atomic uint32_t uses_chkstk;
    _Atomic uint32_t compiled_function_count;

    // needs to be locked with 'TB_Module.lock'
    NL_Strmap(TB_SourceFile*) files;

    // unused by the JIT
    DynArray(TB_ModuleSection) sections;
};

typedef struct {
    size_t length;
    TB_ObjectSection* data;
} TB_SectionGroup;

enum {
    // part of the SoN's embedded CFG, generally produce more CONTROL
    // and taken in CONTROL (with the exception of the entry and exits).
    NODE_CTRL       = 1,
    // CFG node with no successors
    NODE_END        = 2,
    // CFG node which terminates a BB (usually branch or exit)
    NODE_TERMINATOR = 4,
    // tuple nodes which may produce several control edges
    NODE_FORK_CTRL  = 8,
    // uses TB_BRANCH_PROJ for the cprojs
    NODE_BRANCH     = 16,
    // has potential memory input in inputs[1]
    NODE_MEMORY_IN  = 32,
    // has memory outputs
    NODE_MEMORY_OUT = 64,
    // the first few bytes of extra are TB_NodeSafepoint
    NODE_SAFEPOINT  = 128,
    // cannot be scheduled late
    NODE_PINNED     = 256,
};

struct ICodeGen {
    // what does CHAR_BIT mean on said platform
    int minimum_addressable_size, pointer_size;

    // Mach nodes info
    bool (*can_gvn)(TB_Node* n);
    uint32_t (*flags)(TB_Node* n);
    size_t (*extra_bytes)(TB_Node* n);
    const char* (*node_name)(int n_type);
    void (*print_extra)(TB_Node* n);

    // return the number of non-local patches
    size_t (*emit_call_patches)(TB_Module* restrict m, TB_FunctionOutput* out_f);
    // NULLable if doesn't apply
    void (*emit_win64eh_unwind_info)(TB_Emitter* e, TB_FunctionOutput* out_f, uint64_t stack_usage);
    void (*compile_function)(TB_Function* f, TB_FunctionOutput* restrict func_out, const TB_FeatureSet* features, TB_Arena* code, bool emit_asm);
};

// All debug formats i know of boil down to adding some extra sections to the object file
typedef struct {
    const char* name;

    bool (*supported_target)(TB_Module* m);
    int (*number_of_debug_sections)(TB_Module* m);

    // functions are laid out linearly based on their function IDs and
    // thus function_sym_start tells you what the starting point is in the symbol table
    TB_SectionGroup (*generate_debug_info)(TB_Module* m, TB_Arena* arena);
} IDebugFormat;

#define TB_FITS_INTO(T,x) ((x) == (T)(x))

#ifndef _MSC_VER
#define __debugbreak() __builtin_debugtrap()
#endif

// tb_todo means it's something we fill in later
// tb_unreachable means it's logically impossible to reach
// tb_assume means we assume some expression cannot be false
//
// in debug builds these are all checked and tb_todo is some sort of trap
#if defined(_MSC_VER) && !defined(__clang__)
#if TB_DEBUG_BUILD
#define tb_todo()            (assert(0 && "TODO"), __assume(0))
#define tb_unreachable()     (assert(0), __assume(0), 0)
#else
#define tb_todo()            abort()
#define tb_unreachable()     (__assume(0), 0)
#endif
#else
#if TB_DEBUG_BUILD
#define tb_todo()            __builtin_trap()
#define tb_unreachable()     (assert(0), 0)
#else
#define tb_todo()            __builtin_trap()
#define tb_unreachable()     (__builtin_unreachable(), 0)
#endif
#endif

#ifndef NDEBUG
#define TB_ASSERT_MSG(cond, ...) ((cond) ? 0 : (printf("\n" __FILE__ ":" STR(__LINE__) ": assertion failed: " #cond "\n  "), printf(__VA_ARGS__), __builtin_trap(), 0))
#define TB_ASSERT(cond)          ((cond) ? 0 : (printf("\n" __FILE__ ":" STR(__LINE__) ": assertion failed: " #cond "\n  "), __builtin_trap(), 0))
#else
#define TB_ASSERT_MSG(cond, ...) ((cond) ? 0 : (__builtin_unreachable(), 0))
#define TB_ASSERT(cond)          ((cond) ? 0 : (__builtin_unreachable(), 0))
#endif

#if defined(_WIN32) && !defined(__GNUC__)
#define tb_panic(...)                     \
do {                                      \
    printf(__VA_ARGS__);                  \
    __fastfail(FAST_FAIL_FATAL_APP_EXIT); \
} while (0)
#else
#define tb_panic(...)                     \
do {                                      \
    printf(__VA_ARGS__);                  \
    abort();                              \
} while (0)
#endif

#ifndef COUNTOF
#define COUNTOF(...) (sizeof(__VA_ARGS__) / sizeof((__VA_ARGS__)[0]))
#endif

TB_ThreadInfo* tb_thread_info(TB_Module* m);

inline static uint64_t align_up(uint64_t a, uint64_t b) {
    return a + (b - (a % b)) % b;
}

// NOTE(NeGate): Considers 0 as a power of two
inline static bool tb_is_power_of_two(uint64_t x) {
    return (x & (x - 1)) == 0;
}

////////////////////////////////
// HELPER FUNCTIONS
////////////////////////////////
#ifdef _MSC_VER
#define TB_LIKELY(x)   (!!(x))
#define TB_UNLIKELY(x) (!!(x))
#else
#define TB_LIKELY(x)   __builtin_expect(!!(x), 1)
#define TB_UNLIKELY(x) __builtin_expect(!!(x), 0)
#endif

// for more consistent hashing than a pointer
uint32_t tb__node_hash(void* a);
bool tb__node_cmp(void* a, void* b);

TB_Node* tb_alloc_node_dyn(TB_Function* f, int type, TB_DataType dt, int input_count, int input_cap, size_t extra);
TB_Node* tb_alloc_node(TB_Function* f, int type, TB_DataType dt, int input_count, size_t extra);
TB_Node* tb__make_proj(TB_Function* f, TB_DataType dt, TB_Node* src, int index);
void tb_kill_node(TB_Function* f, TB_Node* n);

ExportList tb_module_layout_sections(TB_Module* m);

////////////////////////////////
// EXPORTER HELPER
////////////////////////////////
size_t tb_helper_write_section(TB_Module* m, size_t write_pos, TB_ModuleSection* section, uint8_t* output, uint32_t pos);
size_t tb__layout_relocations(TB_Module* m, DynArray(TB_ModuleSection) sections, const ICodeGen* restrict code_gen, size_t output_size, size_t reloc_size);

TB_ExportChunk* tb_export_make_chunk(TB_Arena* arena, size_t size);
void tb_export_append_chunk(TB_ExportBuffer* buffer, TB_ExportChunk* c);

////////////////////////////////
// ANALYSIS
////////////////////////////////
void set_input(TB_Function* f, TB_Node* n, TB_Node* in, int slot);
void add_input_late(TB_Function* f, TB_Node* n, TB_Node* in);
void add_user(TB_Function* f, TB_Node* n, TB_Node* in, int slot);

// extra edges go after n->input_count, are unordered and NULL just means free space
void tb_node_add_extra(TB_Function* f, TB_Node* n, TB_Node* in);
void tb_node_clear_extras(TB_Function* f, TB_Node* n);

TB_Node* tb__gvn(TB_Function* f, TB_Node* n, size_t extra);

TB_Symbol* tb_symbol_alloc(TB_Module* m, TB_SymbolTag tag, ptrdiff_t len, const char* name, size_t size);

void tb_emit_symbol_patch(TB_FunctionOutput* func_out, TB_Symbol* target, size_t pos);
TB_Global* tb__small_data_intern(TB_Module* m, size_t len, const void* data);
void tb__lattice_init(TB_Module* m);

// out_bytes needs at least 16 bytes
void tb__md5sum(uint8_t* out_bytes, uint8_t* initial_msg, size_t initial_len);

uint64_t tb__sxt(uint64_t src, uint64_t src_bits, uint64_t dst_bits);

char* tb__arena_strdup(TB_Module* m, ptrdiff_t len, const char* src);

static bool is_same_location(TB_Location* a, TB_Location* b) {
    return a->file == b->file && a->line == b->line && a->column == b->column;
}

static TB_Arena* get_temporary_arena(TB_Module* key) {
    return &tb_thread_info(key)->tmp_arena;
}

static TB_Arena* get_permanent_arena(TB_Module* key) {
    return &tb_thread_info(key)->perm_arena;
}
