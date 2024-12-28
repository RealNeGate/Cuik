
#ifdef CONFIG_HAS_TB
typedef struct {
    Futex* done;

    size_t count;
    TB_Function** arr;

    void* arg;

    CuikSched_PerFunction func;
} PerFunction;

static void per_func_task(TPool* tp, void** arg) {
    PerFunction task = *((PerFunction*) arg[0]);
    for (size_t i = 0; i < task.count; i++) {
        task.func(task.arr[i], task.arg);
    }

    atomic_fetch_sub(task.done, 1);
    futex_signal(task.done);
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

    return 8192;
}

void cuiksched_per_function(TPool* tp, CompilationUnit* cu, TB_Module* mod, void* arg, CuikSched_PerFunction func) {
    if (tp != NULL) {
        Futex done = 0;
        size_t count = 0;

        PerFunction task = { .done = &done, .arg = arg, .func = func };
        size_t func_count = dyn_array_length(cu->worklist);

        size_t batch_size = 10000;
        for (size_t i = 0; i < func_count; i += batch_size) {
            size_t end = i + batch_size;
            if (end >= func_count) end = func_count;

            // btw the struct gets copied by the thread pool
            task.count = end - i;
            task.arr = &cu->worklist[i];

            // leaking!!! also bad allocation pattern!!!
            PerFunction* t = cuik_malloc(sizeof(PerFunction));
            *t = task;

            tpool_add_task(tp, per_func_task, t);
            count++;
        }

        futex_wait_eq(&done, count);
    } else {
        size_t func_count = dyn_array_length(cu->worklist);
        for (size_t i = 0; i < func_count; i++) {
            func(cu->worklist[i], arg);
        }
    }
}
#endif
