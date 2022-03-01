#pragma once
#include <threads.h>
#include <stdbool.h>
#include "queue.h"

typedef struct threadpool_t threadpool_t;

typedef struct {
    void (*fn)(void*);
    void *arg;
} work_t;

/* Creates a threadpool with the specified number of workers and queue size. 
   Returns the threadpool created, or null if the number of workers or queue size is equal to 0. */
threadpool_t* threadpool_create(size_t worker_count, size_t workqueue_size);

/* Enqueue an action to the threadpool's internal queue. */
void threadpool_enqueue(threadpool_t *threadpool, void (*fn)(void*), void *arg);

/* Wait for all workers in the thread pool to finish, and all tasks in the internal queue to be handled. */
void threadpool_join(threadpool_t *threadpool);

/* Free the threadpool back to the heap. */
void threadpool_free(threadpool_t *threadpool);
