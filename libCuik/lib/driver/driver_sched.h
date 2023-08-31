#ifdef CUIK_USE_TB
typedef struct {
    TB_Module* mod;
    TB_Function* start;
    Futex* remaining;

    int batch_size;

    void* ctx;
    CuikSched_PerFunction func;
} PerFunction;

static void per_func_task(void* arg) {
    PerFunction task = *((PerFunction*) arg);
    TB_Function* f = task.start;

    for (size_t i = 0; i < task.batch_size && f != NULL; i++) {
        task.func(task.mod, f, task.ctx);
        f = tb_next_function(f);
    }
    futex_dec(task.remaining);
}

static size_t good_batch_size(size_t n, size_t jobs) {
    // we cap out at 8192 for the batch size but when there's less input
    // we might pick something which can get some good division of labor.
    //
    // each thread is gonna get 4 batches so: job_count / (N * 4)
    /*size_t batch_size = jobs / (n * 4);
    if (batch_size < 128) return 100;
    if (batch_size > 8192) return 8192;

    // next power of two
    return 1ull << (64ull - __builtin_clzll(batch_size - 1ull));*/

    return 64;
}

void cuiksched_per_function(Cuik_IThreadpool* restrict thread_pool, int num_threads, TB_Module* mod, void* ctx, CuikSched_PerFunction func) {
    if (thread_pool != NULL) {
        #if CUIK_ALLOW_THREADS
        size_t func_count = tb_module_get_function_count(mod);

        size_t batch_size = good_batch_size(num_threads, func_count);
        size_t capacity = (func_count + batch_size - 1) / batch_size;
        Futex remaining = capacity;

        TB_Function* f = tb_first_function(mod);
        while (f != NULL) {
            PerFunction task = { .mod = mod, .start = f, .remaining = &remaining, .batch_size = batch_size, .ctx = ctx, .func = func };
            CUIK_CALL(thread_pool, submit, per_func_task, sizeof(task), &task);

            // skip to the next batch, this is kinda slow but
            // a submittion happened already so there's work
            // being done elsewhere... ideally
            for (size_t i = 0; f != NULL && i < batch_size; i++) {
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