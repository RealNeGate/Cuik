#include "../tb_internal.h"

typedef struct {
    // personal to each family and represents either a stack slot
    // or register on the target machine
    uint32_t value : 28;

    // represents what kind of virtual register it is (e.g. GPR or vector
    // registers on x64)
    uint32_t family : 4;
} TreeVReg;

typedef struct TreeNode {
    // 0 means it's a sync point and operands[0] is the actual node
    // and operands[1] is a next sync point in the chain
    TB_Reg reg;

    // if use_count>1 it's a shared point which means it evaluates once
    // and stores that result into the vreg waiting for others to access
    // it.
    int use_count;
    TreeVReg vreg;

    struct TreeNode* operands[2];
} TreeNode;

typedef struct TreeNodePage {
    struct TreeNodePage* next;
    size_t count;

    TreeNode nodes[170];
} TreeNodePage;
// static_assert(sizeof(TreeNodePage) == 4096, "sizeof(TreeNodePage) != 4096");

typedef struct {
    // TODO(NeGate): implement some sort of structure
    // for finding shared points quicker
    TreeNodePage* base;
    TreeNodePage* top;

    TreeNode* first_link;
    TreeNode* last_link;
} TreeNodeArena;

// the eval tree represents a target agnostic system for evaluating the IR
// for machine code gen, you can essentially walk from the first node which
// is always a root node and schedule codegen based on simple peepholes and
// tree walkers.
TreeNode* tb_tree_generate(TreeNodeArena* arena, TB_Function* f, TB_Reg* use_count, TB_Label bb);
void tb_tree_clear(TreeNodeArena* arena);
void tb_tree_free(TreeNodeArena* arena);
void tb_tree_print(TreeNode* node);

inline static bool tb_tree_vreg_equal(const TreeVReg* a, const TreeVReg* b) {
    return (a->family == b->family && a->value == b->value);
}
