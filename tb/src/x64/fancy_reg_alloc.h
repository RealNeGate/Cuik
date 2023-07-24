// linear scan allocator with some improvements around lifetime holes,
// based on Efficient global register allocation, 2011:
//   https://arxiv.org/pdf/2011.05608.pdf
typedef struct {
    size_t def_count;
    Set future_active;
    Set active;
    Set live_out;
} FancyLSRA;

static void fancy_allocate(Ctx* restrict ctx, FancyLSRA* restrict ra, DefIndex di) {
    if (set_get(&ra->future_active, di)) {
        set_remove(&ra->future_active, di);
        set_put(&ra->active, di);
    }
}

static void fancy_lsra(Ctx* restrict ctx, TB_Function* f, RegAllocWorklist worklist) {
    // future active starts off referring to precolored definitions
    FancyLSRA ra = { .def_count     = dyn_array_length(ctx->defs) };
    ra.future_active = set_create_in_arena(&tb__arena, ra.def_count);
    ra.active        = set_create_in_arena(&tb__arena, ra.def_count);
    ra.live_out      = set_create_in_arena(&tb__arena, ra.def_count);

    FOREACH_REVERSE_N(i, 0, dyn_array_length(worklist)) {
        if (ctx->defs[worklist[i]].reg >= 0) set_put(&ra.future_active, i);
    }

    size_t current_bb = 0;

    // walk the CFG, in it's scheduled form
    //
    // because the basic block numbers are in order, we don't need to
    // store a visited set, it's literally just whatever BBs are lower than
    // current_bb.
    Inst* restrict inst = ctx->first;
    while (inst != NULL) {
        TB_Node* bb = (TB_Node*) inst->imm[0];
        assert(bb == ctx->order.traversal[current_bb]);
        MachineBB* mbb = &nl_map_get_checked(ctx->machine_bbs, bb);

        Set* live_in = &mbb->live_in;

        // expire intervals between basic blocks
        //
        //   for live_out & ~live_in do
        //     for block in not visited do
        //       if i is live-in(block) then
        //         pause i
        //         continue outer loop
        //       end
        //     end
        //
        //     free i
        //   end
        uint64_t m;
        FOREACH_N(i, 0, (ra.def_count + 63) / 64) if (m = ra.live_out.data[i] & ~live_in->data[i], m != 0) {
            FOREACH_N(j, 0, 64) if (m & (1ull << j)) {
                size_t k = i*64 + j;

                FOREACH_N(block, current_bb, ctx->order.count) {
                    MachineBB* other_mbb = &nl_map_get_checked(ctx->machine_bbs, ctx->order.traversal[block]);

                    if (set_get(&other_mbb->live_in, k)) {
                        // pause (moving active -> future active)
                        set_remove(&ra.active, k);
                        set_put(&ra.future_active, k);
                        goto outer_loop;
                    }
                }

                // free
                set_remove(&ra.active, k);
                outer_loop:;
            }
        }

        // start intervals
        //   for live_in & ~live_out do
        //     allocate i
        FOREACH_N(i, 0, (ra.def_count + 63) / 64) if (m = live_in->data[i] & ~ra.live_out.data[i], m != 0) {
            FOREACH_N(j, 0, 64) if (m & (1ull << j)) {
                size_t k = i*64 + j;

                tb_todo();
            }
        }

        for (; inst; inst = inst->next) {
            tb_todo();
        }

        current_bb++;
    }
}
