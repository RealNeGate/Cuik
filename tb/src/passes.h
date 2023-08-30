#pragma once
#include "tb_internal.h"

#define TB_OPTDEBUG_PEEP 1
#define TB_OPTDEBUG_LOOP 0
#define TB_OPTDEBUG_MEM2REG 0

#define DO_IF(cond) CONCAT(DO_IF_, cond)
#define DO_IF_0(...)
#define DO_IF_1(...) __VA_ARGS__

typedef struct User User;
struct User {
    User* next;
    TB_Node* n;
    int slot;
};

struct TB_Passes {
    TB_Function* f;

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

extern thread_local TB_Arena* tmp_arena;

void verify_tmp_arena(TB_Passes* p);
User* find_users(TB_Passes* restrict p, TB_Node* n);
void set_input(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot);

