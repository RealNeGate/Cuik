#ifdef CUIK_USE_TB
typedef struct {
    Futex* remaining;

    TB_Function* f;
    void* arg;

    CuikSched_PerFunction func;
} PerFunction;

static void per_func_task(void* arg) {
    PerFunction task = *((PerFunction*) arg);
    task.func(task.f, task.arg);

    atomic_fetch_sub(task.remaining, 1);
    futex_signal(task.remaining);
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

void cuiksched_per_function(Cuik_IThreadpool* restrict thread_pool, int num_threads, TB_Module* mod, void* arg, CuikSched_PerFunction func) {
    TB_SymbolIter it = tb_symbol_iter(mod);
    if (thread_pool != NULL) {
        Futex remaining = 0;
        size_t count = 0;

        PerFunction task = { .remaining = &remaining, .arg = arg, .func = func };

        TB_Symbol* sym;
        while (sym = tb_symbol_iter_next(&it), sym) if (sym->tag == TB_SYMBOL_FUNCTION) {
            task.f = (TB_Function*) sym;
            CUIK_CALL(thread_pool, submit, per_func_task, sizeof(task), &task);
            count++;
        }

        futex_wait_eq(&remaining, count);
    } else {
        TB_Symbol* sym;
        while (sym = tb_symbol_iter_next(&it), sym) if (sym->tag == TB_SYMBOL_FUNCTION) {
            func((TB_Function*) sym, arg);
        }
    }
}
#endif

