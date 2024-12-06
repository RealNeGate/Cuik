#pragma once
#include <stdint.h>
#include <stdatomic.h>

#define TPool_Thread_Local _Thread_local
#define TPool_Atomic _Atomic

#if defined(__APPLE__) || defined(_WIN32)
typedef TPool_Atomic int64_t TPool_Futex;
#else
typedef TPool_Atomic int32_t TPool_Futex;
#endif

typedef struct TPool_Thread TPool_Thread;
typedef struct TPool TPool;

typedef void tpool_task_proc(TPool* pool, void** args);

typedef struct TPool_Task {
    tpool_task_proc *do_work;
    void *args[3];
} TPool_Task;

struct TPool {
    struct TPool_Thread *threads;

    int thread_count;
    TPool_Atomic bool running;

    TPool_Futex tasks_available;
    TPool_Futex tasks_left;
};

void tpool_init(TPool *pool, int child_thread_count);
void tpool_add_task(TPool *pool, tpool_task_proc* fn, void* val);
void tpool_add_task2(TPool *pool, tpool_task_proc* fn, int arg_count, void** args);
void tpool_wait(TPool *pool);
void tpool_destroy(TPool *pool);

