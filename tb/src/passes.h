#pragma once
#include "tb_internal.h"

#define TB_OPTDEBUG_PEEP 1
#define TB_OPTDEBUG_LOOP 0
#define TB_OPTDEBUG_MEM2REG 1

#define DO_IF(cond) CONCAT(DO_IF_, cond)
#define DO_IF_0(...)
#define DO_IF_1(...) __VA_ARGS__

typedef NL_HashSet TB_FrontierSet;
typedef NL_Map(TB_Node*, TB_FrontierSet) TB_DominanceFrontiers;

typedef struct {
    size_t count;
    TB_Node** traversal;
    NL_Map(TB_Node*, char) visited;
} TB_PostorderWalk;

typedef struct User User;
struct User {
    User* next;
    TB_Node* n;
    int slot;
};

struct TB_Passes {
    TB_Function* f;

    // we really shouldn't be doing this...
    bool cfg_dirty;

    // we use this to verify that we're on the same thread
    // for the entire duration of the TB_Passes.
    TB_ThreadInfo* pinned_thread;

    DynArray(TB_Node*) worklist;
    NL_HashSet visited;

    TB_PostorderWalk order;

    // we wanna track locals because it's nice and easy
    DynArray(TB_Node*) locals;

    // this is used to do CSE
    NL_HashSet cse_nodes;

    // debug shit:
    TB_Node* error_n;

    // outgoing edges are incrementally updated every time we
    // run a rewrite rule
    NL_Map(TB_Node*, User*) users;
};

////////////////////////////////
// CFG analysis
////////////////////////////////
// shorthand because we use it a lot
static TB_Node* idom(TB_Node* n) {
    return TB_NODE_GET_EXTRA_T(n, TB_NodeRegion)->dom;
}

static int dom_depth(TB_Node* n) {
    if (n == NULL) {
        return 0;
    }

    while (n->type != TB_REGION && n->type != TB_START) {
        n = n->inputs[0];
    }

    return TB_NODE_GET_EXTRA_T(n, TB_NodeRegion)->dom_depth;
}

void tb_compute_dominators(TB_Function* f, TB_PostorderWalk order);

// Allocates from the heap and requires freeing with tb_function_free_postorder
TB_PostorderWalk tb_function_get_postorder(TB_Function* f);
void tb_function_free_postorder(TB_PostorderWalk* walk);

extern thread_local TB_Arena* tmp_arena;

void tb_pass_ensure_empty(TB_Passes* p);

void verify_tmp_arena(TB_Passes* p);
User* find_users(TB_Passes* restrict p, TB_Node* n);
void set_input(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot);
