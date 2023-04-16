#include "tb_internal.h"

typedef struct {
    TB_Emitter nodes;
    NL_Map(TB_Node*, uint32_t) file_offset_map;
} SerializeCtx;

static uint32_t emit_node(SerializeCtx* ctx, TB_Node* n) {
    ptrdiff_t search = nl_map_get(ctx->file_offset_map, n);
    if (search >= 0) return ctx->file_offset_map[search].v;

    // write header (we'll override the next pointer later)
    return tb_outs(&ctx->nodes, tb_node_get_expected_size(n), n);
}

static void emit_function(TB_Function* f) {
    SerializeCtx ctx = { 0 };

    // we just walk all nodes and lay them out however
    TB_FOR_BASIC_BLOCK(bb, f) {
        uint32_t prev = 0;
        TB_FOR_NODE(n, f, bb) {
            uint32_t curr = emit_node(&ctx, n);

            // stitch next pointer
            if (prev) tb_patch8b(&ctx.nodes, prev, curr);
            prev = curr + offsetof(TB_Node, next);
        }
    }

    tb_todo();

    // generate basic block listing
    TB_FOR_BASIC_BLOCK(bb, f) {

    }
}

// Serialize
TB_Exports tb_module_export_bytecode(TB_Module* m) {
    TB_FOR_FUNCTIONS(f, m) {

        // emits bytecode
        emit_function(f);
    }

    return (TB_Exports){ 0 };
}
