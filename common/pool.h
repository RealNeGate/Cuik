#pragma once
#include <stdint.h>
#include <stdatomic.h>
#include "futex.h"

#define TPool_Thread_Local _Thread_local
#define TPool_Atomic _Atomic

typedef struct TPool_Thread TPool_Thread;
typedef struct TPool TPool;
typedef struct TPool_ReadReq TPool_ReadReq;

typedef void tpool_task_proc(TPool* pool, void** args);
typedef void tpool_io_task_proc(TPool* pool, TPool_ReadReq* req);

typedef struct TPool_Task {
    tpool_task_proc *do_work;
    void *args[3];
} TPool_Task;

struct TPool_ReadReq {
    int fd;
    size_t offset, size;
    void* data;

    tpool_io_task_proc* do_work;
    void* args[3];
};

struct TPool {
    struct TPool_Thread *threads;

    int thread_count;
    TPool_Atomic bool running;
    TPool_Atomic uint64_t last_broadcast_tick;

    TPool_Atomic uint64_t sleeping_tasks;

    Futex tasks_available;
    Futex tasks_left;
};

void tpool_init(TPool *pool, int child_thread_count);
void tpool_add_task(TPool *pool, tpool_task_proc* fn, void* val);
void tpool_add_task2(TPool *pool, tpool_task_proc* fn, int arg_count, void** args);
void tpool_wait(TPool *pool);
void tpool_destroy(TPool *pool);

int tpool_num_threads(TPool *pool);

void tpool_io_prep_all(TPool *pool);
void tpool_io_prep(TPool* pool);
void tpool_io_read(TPool* pool, int fd, size_t offset, size_t size, void* data, tpool_io_task_proc* fn, void* arg0, void* arg1, void* arg2);

// Called within an I/O task to forward tasks to the worker threads
void tpool_io_forward(TPool *pool, bool hi_prio, tpool_task_proc* fn, int arg_count, void** args);

void tpool_wait_for_jobs(TPool *pool, Futex* done, Futex* count);
void tpool_wait_for_jobs2(TPool *pool, Futex* done, int64_t count);
