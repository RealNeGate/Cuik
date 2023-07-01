#ifdef CUIK_USE_TB
enum {
    IRGEN_TASK_BATCH_SIZE = 8192,
    TB_TASK_BATCH_SIZE = 8192,
};

typedef struct {
    TB_Module* mod;
    TB_Function* start;
    Futex* remaining;

    void* ctx;
    CuikSched_PerFunction func;
} PerFunction;

static void per_func_task(void* arg) {
    PerFunction task = *((PerFunction*) arg);
    TB_Function* f = task.start;

    for (size_t i = 0; i < TB_TASK_BATCH_SIZE && f != NULL; i++) {
        task.func(task.mod, f, task.ctx);
        f = tb_next_function(f);
    }
    futex_dec(task.remaining);
}

void cuiksched_per_function(Cuik_IThreadpool* restrict thread_pool, TB_Module* mod, void* ctx, CuikSched_PerFunction func) {
    if (thread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        size_t capacity = (tb_module_get_function_count(mod) + TB_TASK_BATCH_SIZE - 1) / TB_TASK_BATCH_SIZE;
        Futex remaining = capacity;

        TB_Function* f = tb_first_function(mod);
        while (f != NULL) {
            PerFunction task = { .mod = mod, .start = f, .remaining = &remaining, .ctx = ctx, .func = func };
            CUIK_CALL(thread_pool, submit, per_func_task, sizeof(task), &task);

            // skip to the next batch, this is kinda slow but
            // a submittion happened already so there's work
            // being done elsewhere... ideally
            for (size_t i = 0; f != NULL && i < TB_TASK_BATCH_SIZE; i++) {
                f = tb_next_function(f);
            }
        }

        futex_wait_eq(&remaining, 0);
        #else
        fprintf(stderr, "Please compile with -DCUIK_ALLOW_THREADS if you wanna spin up threads");
        abort();
        #endif /* CUIK_ALLOW_THREADS */
    } else {
        TB_FOR_FUNCTIONS(f, mod) {
            func(mod, f, ctx);
        }
    }
}
#endif