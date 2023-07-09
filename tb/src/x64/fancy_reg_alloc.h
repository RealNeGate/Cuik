static void fancy_allocate(Ctx* restrict ctx, Set* active, Set* future_active, DefIndex di) {
    if (set_get(future_active, di)) {
        set_remove(future_active, di);
        set_put(active, di);
    }
}

static void fancy_reg_alloc(Ctx* restrict ctx, TB_Function* f, RegAllocWorklist worklist) {
    size_t def_count = dyn_array_length(ctx->defs);

    // future active starts off referring to precolored definitions
    Set future_active = set_create_in_arena(&tb__arena, def_count);
    FOREACH_REVERSE_N(i, 0, dyn_array_length(worklist)) {
        if (ctx->defs[worklist[i]].reg >= 0) set_put(&future_active, i);
    }

    Set active = set_create_in_arena(&tb__arena, def_count);
    Set visited = set_create_in_arena(&tb__arena, def_count);
    Set live_out = set_create_in_arena(&tb__arena, def_count);

    size_t current_bb = 0;

    // walk the CFG, in it's scheduled form
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
        FOREACH_N(i, 0, (def_count + 63) / 64) if (m = live_out.data[i] & ~live_in->data[i], m != 0) {
            FOREACH_N(j, 0, 64) if (m & (1ull << j)) {
                size_t k = i*64 + j;

                FOREACH_N(block, current_bb, ctx->order.count) {
                    MachineBB* other_mbb = &nl_map_get_checked(ctx->machine_bbs, ctx->order.traversal[block]);

                    if (set_get(&other_mbb->live_in, k)) {
                        // pause (moving active -> future active)
                        set_remove(&active, k);
                        set_put(&future_active, k);
                        goto outer_loop;
                    }
                }

                // free
                set_remove(&active, k);
                outer_loop:;
            }
        }

        // start intervals
        //   for live_in & ~live_out do
        //     allocate i
        FOREACH_N(i, 0, (def_count + 63) / 64) if (m = live_in->data[i] & ~live_out.data[i], m != 0) {
            FOREACH_N(j, 0, 64) if (m & (1ull << j)) {
                size_t k = i*64 + j;

                __debugbreak();
            }
        }

        for (; inst; inst = inst->next) {
            __debugbreak();
        }

        current_bb++;
    }
}
