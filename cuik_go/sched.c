
static TB_Stacklet* first_task;
TB_Stacklet* sched_g_first(void) {
    return first_task;
}

Sched_G* sched_g_get(TB_Stacklet* stacklet) {
    return (Sched_G*) ((char*) stacklet + tb_jit_thread_userdata());
}

TB_Stacklet* go_spawn(TB_JIT* jit) {
    TB_Stacklet* stack = tb_jit_thread_create(jit, sizeof(Sched_G), 16384);
    Sched_G* g = sched_g_get(stack);

    // attach to scheduler
    g->next = first_task;
    first_task = stack;
    return stack;
}

static void (*go_stuff)(Slice*, Slice*);

static void foobar(void) {
    Slice a = { gc_alloc(1024, 0), 1024/8, 1024/8 };
    Slice b = { gc_alloc(1024, 0), 1024/8, 1024/8 };

    // initialize data
    double* aa = gc_rawptr(a.base);
    double* bb = gc_rawptr(b.base);
    FOR_N(i, 0, 1024/8) {
        aa[i] = i*0.25;
        bb[i] = i*0.5 - 6.0;
        // printf("[%zu] %f, %f\n", i, aa[i], bb[i]);
    }

    for (;;) {
        // printf("go_stuff(%p, %p)\n", gc_rawptr(a.base), gc_rawptr(b.base));
        go_stuff(&a, &b);
    }
}

// This thread mostly just waits for timer interrupts
static int sched_main(void* arg) {
    for (;;) {

    }
}

// This thread maps to an OS-thread, it's main job is just run goroutines
static int sched_n_main(void* arg) {
    marklist_n = gc_marklist_init();
    tb_jit_thread_call(first_task, foobar, NULL, 0, NULL);
    printf("Done!\n");
    return 0;
}

TB_Stacklet* c_checkpoint(TB_Stacklet* stack) {
    uint64_t start = cuik_time_in_nanos();
    Sched_G* g = sched_g_get(stack);
    g->pause = 0;

    void** rsp = (void**) g->state.gprs[4];
    void* rpc  = rsp[1];

    TB_Safepoint* sfpt;
    while (sfpt = tb_jit_get_safepoint(jit, rpc), sfpt) {
        TB_Function* f = sfpt->func;

        printf("CHECKPOINT: rsp = %p, [rsp+8] = %p\n", rsp, rpc);
        printf("  %s+%#x\n", ((TB_Symbol*) f)->name, sfpt->ip);
        FOR_N(i, 0, sfpt->count) {
            printf("    R%-2u : %4u\n", (sfpt->refs[i] >> 8) & 0xFF, sfpt->refs[i] & 0xFF);
        }

        // pop frame
        rsp += sfpt->frame_size / 8;
        rpc = rsp[0];
    }

    // notify that we've done our root scanning
    gc_checkpoint_time += cuik_time_in_nanos() - start;
    gc_checkpoint_trigger++;
    return stack;
}

