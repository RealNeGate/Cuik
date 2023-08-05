#pragma once
#include "tb_internal.h"

#define TB_OPTDEBUG_PEEP 0
#define TB_OPTDEBUG_LOOP 0
#define TB_OPTDEBUG_MEM2REG 1

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

    // some function state is changed for the duration of the
    // optimizer run, we need to track that to reset it
    TB_Attrib* old_line_attrib;
    TB_Arena* old_arena;

    DynArray(TB_Node*) queue;
    NL_HashSet visited;

    TB_PostorderWalk order;
    TB_Dominators doms;

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

User* find_users(TB_Passes* restrict p, TB_Node* n);
void set_input(TB_Passes* restrict p, TB_Node* n, TB_Node* in, int slot);

