// This is the general codegen framework, it combines together the various components in
// the target dependent part, if you see any of these TODOs and wanna contribute, those
// are good places to go.
//
// This can be broken down into a few steps:
//
//   GCM => Init => Per BB: { Local sched, Tiling } => Dataflow => Regalloc => Emit
//
// Here's where you can find more info on these:
//
//   * GCM: handled by the tb_pass_schedule (TODO ways to skew global scheduling).
//
//   * Init: This is where you can process ABI details and fill in certain important details
//     for later like ctx.sched which is the scheduler, and ctx.regalloc which is the register
//     allocator.
//
//   * Local sched: handled by ctx.sched, it's just a function you can replace with your
//     own based on whatever you wanna achieve, for now i've only got the topo sort greedy
//     sched (TODO implement a latency-based list sched will be a thing)
//
//   * Tiling: handled by isel_node, by default we walk the BB backwards building one tile
//     per node, you can grow a tile to multiple nodes by tagging it differently and to disable
//     the codegen of a node by itself later you can call fold_node(ctx, n) on it.
//
//   * Regalloc: TODO
//
//   * Emit: handled by emit_tile, writes out the bytes now that we've completed regalloc.
//
#pragma once

#include "opt/passes.h"
#include "emitter.h"

enum {
    // all we can fit into 3bits, but also... 8 classes is a lot.
    //
    // * x86 has two currently: GPR and Vector.
    //
    MAX_REG_CLASSES = 8
};

// represents a set of registers, usually for register constraints
typedef struct {
    uint64_t class : 3;
    uint64_t mask  : 61;
} RegMask;
#define REGMASK(c, m) (RegMask){ REG_CLASS_ ## c, m }
#define REGEMPTY (RegMask){ 0 }

typedef struct {
    int start, end;
} LiveRange;

typedef struct {
    int pos;

    // this is used to denote folded reloads
    //
    //   # r2 is spilled
    //   add r0, r1, r2
    //
    // when we codegen, we don't need to allocate
    // a register for r2 here.
    //
    //   add r0, r1, [sp - 24]
    enum {
        USE_OUT,
        USE_REG,
        USE_MEM_OR_REG,
    } kind;
} UsePos;

typedef enum {
    // single node tile
    TILE_NORMAL,
    // SoN doesn't need explicit
    TILE_GOTO,
    // used by regalloc to spill/reload
    TILE_SPILL_MOVE,
    // used by regalloc to rematerialize immediates
    TILE_RELOAD_IMM,
    // place all target-dependent tiles here:
    //   ...
} TileTag;

// represents a pattern of instructions
typedef struct Tile {
    struct Tile* prev;
    struct Tile* next;

    uint8_t tag;

    // regalloc concerns
    int8_t assigned;
    int8_t hint;
    // register num, -1 if the interval isn't a physical reg
    int8_t reg;

    RegMask mask;
    int id; // used by live sets

    TB_Node* n;

    union {
        // tag = TILE_GOTO, this is the successor
        TB_Node* succ;

        // tag = TILE_SPILL_MOVE, this is the tile we're copying from.
        struct Tile* src;
    };

    struct {
        int time;
        int active_range;

        // spill info
        struct Tile* og; // initial tile
        int spill;

        DynArray(UsePos) uses;

        int range_cap, range_count;
        LiveRange* ranges;
    };
} Tile;

typedef struct {
    int id;

    TB_Node* end_n;

    Tile* start;
    Tile* end;

    // dataflow
    Set gen, kill;
    Set live_in, live_out;
} MachineBB;

typedef struct {
    TB_SymbolPatch* patch;
    TB_Location* loc;
    TB_Location* end;
} Disasm;

// Static-sized hash map
typedef struct {
    TB_Node* k;
    MachineBB* v;
} NodeToBB;

typedef struct Ctx Ctx;
typedef void (*TB_RegAlloc)(Ctx* restrict ctx, TB_Arena* arena);

struct Ctx {
    TB_Passes* p;
    TB_CGEmitter emit;

    TB_Module* module;
    TB_Function* f;

    // user-provided details
    TB_Scheduler sched;
    TB_RegAlloc regalloc;

    // target-dependent index
    int abi_index;
    int fallthrough;
    int prologue_length;

    // TB_Node* -> MachineBB*
    struct {
        size_t exp;
        NodeToBB* entries;
    } node_to_bb;

    // Basic blocks
    int bb_count;
    MachineBB* machine_bbs;

    // Values
    Set folded;
    Tile** values; // values[n->gvn]

    // Regalloc
    int stack_usage;
    int num_classes;
    int num_regs[MAX_REG_CLASSES];

    // Line info
    DynArray(TB_Location) locations;
};

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena);
