
typedef struct {
    int rank;
    int pos;

    float per_in_w;
    float per_proj_w;
} NodeLayout;

static _Thread_local int* svg_width_per_rank;
static _Thread_local NodeLayout* svg_layout;
static _Thread_local int svg_node_count;
static _Thread_local TB_Node** svg_nodes;

static int rank_walk(TB_Function* f, TB_Node* n) {
    if (svg_layout[n->gvn].rank != 0) {
        return svg_layout[n->gvn].rank;
    }

    // visited but unranked
    svg_layout[n->gvn].rank = -1;

    // our rank will be one above our inputs
    int rank = 0;
    FOR_N(i, 0, n->input_count) if (n->inputs[i]) {
        int u_rank = rank_walk(f, n->inputs[i]);
        if (u_rank > rank) { rank = u_rank; }
    }

    // we don't layout projections or phis
    if (is_proj(n) || n->type == TB_PHI) { return -1; }
    if (rank < 0) { rank = 0; }

    rank += 1;

    svg_layout[n->gvn].rank = rank;
    svg_layout[n->gvn].pos  = svg_width_per_rank[rank]++;

    tb_print_dumb_node(NULL, n);
    printf(" // rank=%d\n", rank);

    svg_nodes[svg_node_count++] = n;
    return rank;
}

static void node_layout(TB_Node* n, NodeLayout* l, float dst[2]) {
    float full_width = svg_width_per_rank[l->rank] * 120.0f;
    dst[0] = 500.0f + ((l->pos * 120.0f) - (full_width*0.5f));
    dst[1] = 50.0f  + l->rank * 120.0f;

    if (cfg_is_control(n)) {
        dst[0] = 200.0f;
    }
}

void tb_print_svg(TB_Function* f) {
    // the temp arena might've been freed, let's restore it
    if (f->tmp_arena.top == NULL) {
        tb_arena_create(&f->tmp_arena, "Tmp");
    }

    TB_ArenaSavepoint sp = tb_arena_save(&f->tmp_arena);
    svg_layout = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(NodeLayout));
    svg_width_per_rank = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(int));
    svg_nodes = tb_arena_alloc(&f->tmp_arena, f->node_count * sizeof(TB_Node*));
    svg_node_count = 0;

    FOR_N(i, 0, f->node_count) { svg_layout[i].rank = 0, svg_layout[i].pos = 0; }
    FOR_N(i, 0, f->node_count) { svg_width_per_rank[i] = 0; }

    printf("\n\n");

    svg_layout[f->root_node->gvn].rank = -1;
    rank_walk(f, f->root_node->inputs[1]);
    // place the root all the way at the top
    svg_layout[f->root_node->gvn].rank = 0;
    svg_layout[f->root_node->gvn].pos  = 0;
    svg_width_per_rank[0] = 1;
    svg_nodes[svg_node_count++] = f->root_node;

    __debugbreak();

    FILE* fp = fopen("out.svg", "wb");

    fprintf(fp, "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n");
    fprintf(fp, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n");
    fprintf(fp, "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"1000pt\" height=\"1000pt\" viewBox=\"0.00 0.00 1000 1000\">\n");

    float w = 100.0f, h = 50.0f;
    FOR_REV_N(i, 0, svg_node_count) {
        TB_Node* n = svg_nodes[i];
        const char* name = tb_node_get_name(n->type);

        NodeLayout* l = &svg_layout[n->gvn];
        float xy[2];
        node_layout(n, l, xy);

        l->per_in_w = w / (n->input_count - 1);

        fprintf(fp, "  <g class=\"node\">\n");
        if (n->type != TB_ROOT) {
            FOR_N(j, 0, n->input_count - 1) {
                fprintf(fp, "    <rect fill=\"#e8b36d\" stroke=\"black\" x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\"/>\n", xy[0] + j*l->per_in_w,xy[1]-10.0f,l->per_in_w,10.0f);
            }
        }
        fprintf(fp, "    <rect fill=\"#e8b36d\" stroke=\"black\" x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\"/>\n", xy[0],xy[1],w,h);
        if (n->dt.type == TB_TAG_TUPLE) {
            int proj_count = 0;
            FOR_USERS(u, n) {
                if (is_proj(USERN(u)) && USERN(u)->dt.type != TB_TAG_CONTROL) { proj_count++; }
            }

            float per_proj_w = w / proj_count;
            FOR_N(j, 0, proj_count) {
                fprintf(fp, "    <rect fill=\"#e8b36d\" stroke=\"black\" x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\"/>\n", xy[0] + j*per_proj_w,xy[1]+h,per_proj_w,10.0f);
            }
            l->per_proj_w = per_proj_w;
        }
        fprintf(fp, "    <text text-anchor=\"middle\" x=\"%f\" y=\"%f\" font-family=\"Consolas\" font-size=\"14.00\" fill=\"#000000\">%u %s</text>", xy[0] + w*0.5f, xy[1] + h*0.5f + 7.0f, n->gvn, name);
        fprintf(fp, "  </g>\n");
    }

    FOR_REV_N(i, 0, svg_node_count) {
        TB_Node* n = svg_nodes[i];
        if (n->type == TB_ROOT) { continue; }

        float xy[2];
        NodeLayout* l = &svg_layout[n->gvn];
        node_layout(n, l, xy);

        float inputs_w = n->input_count*15.0f;
        FOR_N(j, 1, n->input_count) if (n->inputs[j]) {
            TB_Node* in = n->inputs[j];

            // classify edges
            const char* color = "black";
            if (in->dt.type == TB_TAG_MEMORY)  { color = "blue"; }

            // we don't care about the edge that constants and friends have to the root
            if (in->type == TB_ROOT) { continue; }

            // projections & phis as a little piece of their parent node
            float xy2[2];
            if (is_proj(in)) {
                int idx = TB_NODE_GET_EXTRA_T(in, TB_NodeProj)->index;

                in = in->inputs[0];
                node_layout(in, &svg_layout[in->gvn], xy2);

                xy2[0] += (idx + 0.5f)*svg_layout[in->gvn].per_proj_w;
                xy2[1] += 10.0f;
            } else if (in->type == TB_PHI) {
                in = in->inputs[0];
                node_layout(in, &svg_layout[in->gvn], xy2);
                xy2[0] += w*0.5f;
            } else {
                node_layout(in, &svg_layout[in->gvn], xy2);
                xy2[0] += w*0.5f;
            }

            float xx = xy[0] + (j-0.5f)*l->per_in_w;
            fprintf(fp, "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"%s\"/>", xx, xy[1] - 10.0f, xy2[0], xy2[1] + h, color);
        }

        if (n->inputs[0]) {
            if (cfg_is_control(n) && n->type != TB_ROOT) {
                TB_Node* ctrl = n->inputs[0];
                float off = h;
                if (is_proj(ctrl)) { ctrl = ctrl->inputs[0], off += 10.0f; }

                float xy2[2];
                node_layout(ctrl, &svg_layout[ctrl->gvn], xy2);

                fprintf(fp, "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"red\"/>", xy[0], xy[1] - 10.0f, 200.0f, xy2[1] + off);
            } else {
                fprintf(fp, "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" stroke=\"red\"/>", xy[0], xy[1] + h*0.5f, 200.0f, xy[1] + h*0.5f);
            }
        }
    }
    fprintf(fp, "</svg>\n");
    fclose(fp);

    tb_arena_restore(&f->tmp_arena, sp);
}
