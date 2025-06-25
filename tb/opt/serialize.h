
#define TB_IR_BLOB_VERSION 1

typedef struct {
    char magic[4];
    uint32_t version;
    uint32_t node_count;
    uint32_t dict[];
} IRBlob_Header;

typedef struct {
    TB_NodeType type;
    TB_DataType dt;
    uint16_t input_count;
    uint16_t extra;
    uint32_t inputs[];
} IRBlob_Node;

typedef struct {
    size_t cnt, cap;
    uint8_t* data;
} IRBlob;

void tb_print_from_blob(IRBlob blob) {
    IRBlob_Header* header = (IRBlob_Header*) blob.data;
    bool bad_magic = memcmp(header->magic, "TBIR", 4) != 0;

    printf("FILE:\n");
    printf("  MAGIC:   '%.*s'%s\n", 4, header->magic, bad_magic ? " (BAD!!!)" : "");
    printf("  VERSION: %u%s\n", header->version, header->version != TB_IR_BLOB_VERSION ? " (BAD!!!)" : "");
    printf("  COUNT:   %u\n", header->node_count);
    printf("  NODES:\n");
    if (bad_magic || header->version != TB_IR_BLOB_VERSION) {
        return;
    }

    FOR_N(i, 0, header->node_count) {
        IRBlob_Node* node = (IRBlob_Node*) &blob.data[header->dict[i]];

        printf("  %%%-4zu: ", i);

        int l = print_type(node->dt);
        FOR_N(i, l, 5) { printf(" "); }

        printf(" = %s ( ", tb_node_get_name(node->type));
        FOR_N(j, 0, node->input_count) {
            if (node->inputs[j] == UINT32_MAX) {
                printf("___ ");
            } else {
                printf("%%%u ", node->inputs[j]);
            }
        }

        printf(")");
        if (node->extra) {
            printf(" || ");

            size_t off = header->dict[i] + sizeof(IRBlob_Node) + node->input_count*4;
            uint32_t* extra = (uint32_t*) &blob.data[off];
            FOR_N(i, 0, node->extra / 4) {
                printf("0x%08"PRIX32" ", extra[i]);
            }
        }
        printf("\n");
    }
}

IRBlob tb_save_to_blob(TB_Function* f) {
    int used   = 0;
    int* dict  = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));
    int* remap = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));
    FOR_N(i, 0, f->node_count) {
        dict[i] = -1;
    }

    TB_Worklist ws = { 0 };
    worklist_alloc(&ws, f->node_count);

    worklist_push(&ws, f->root_node);
    for (size_t i = 0; i < dyn_array_length(ws.items); i++) {
        TB_Node* n = ws.items[i];
        FOR_USERS(u, n) { worklist_push(&ws, USERN(u)); }

        dict[i] = used;
        remap[n->gvn] = i;

        size_t extra = extra_bytes(n);
        TB_ASSERT(extra < 65536);

        used += sizeof(IRBlob_Node);
        used += n->input_count*4;
        used += (extra + 3) & -4;
    }

    // IR dictionary: ID -> Offset
    size_t dict_size = sizeof(IRBlob_Header) + dyn_array_length(ws.items)*4;

    IRBlob blob;
    blob.cap = dict_size + used;
    blob.cnt = dict_size;
    blob.data = tb_arena_alloc(&f->tmp_arena, used);

    IRBlob_Header* header = (IRBlob_Header*) blob.data;
    memcpy(header->magic, "TBIR", 4);
    header->version = TB_IR_BLOB_VERSION;
    header->node_count = dyn_array_length(ws.items);

    for (size_t i = 0; i < dyn_array_length(ws.items); i++) {
        TB_Node* n = ws.items[i];
        header->dict[i] = dict_size + dict[i];

        TB_ASSERT(dict_size + dict[i] == blob.cnt);
        IRBlob_Node* node = (IRBlob_Node*) &blob.data[blob.cnt];
        blob.cnt += sizeof(IRBlob_Node) + n->input_count*4;

        size_t extra = extra_bytes(n);
        node->type = n->type;
        node->dt = n->dt;
        node->input_count = n->input_count;
        node->extra = extra;
        FOR_N(j, 0, n->input_count) {
            node->inputs[j] = n->inputs[j] ? remap[n->inputs[j]->gvn] : UINT32_MAX;
        }

        memcpy(&blob.data[blob.cnt], n->extra, extra);
        blob.cnt += (extra + 3) & -4;
    }
    TB_ASSERT(blob.cnt == blob.cap);
    tb_print_from_blob(blob);
    __debugbreak();
    return blob;
}

void tb_load_from_blob(TB_Function* f) {

}

