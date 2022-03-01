#include "threadpool.h"

struct threadpool_t {
    queue_t* workqueue;
    thrd_t* workers;
    size_t worker_count;
    cnd_t cnd;
    mtx_t mtx;
    bool complete;
};

static int threadpool_thread(void* arg) {
    threadpool_t* threadpool = arg;
    while (true) {
        mtx_lock(&threadpool->mtx);
        if (!(threadpool->complete || threadpool->workqueue->size > 0)) {
            cnd_wait(&threadpool->cnd, &threadpool->mtx);
        }
		
        if (threadpool->workqueue->size > 0) {
            work_t work;
            queue_pop(threadpool->workqueue, &work);
            mtx_unlock(&threadpool->mtx);
            work.fn(work.arg);
        } else if (threadpool->complete) {
            mtx_unlock(&threadpool->mtx);
            break;
        }
    }
	
    return 0;
}

/* Creates a threadpool with the specified number of workers and queue size. 
   Returns the threadpool created, or null if the number of workers or queue size is equal to 0. */
threadpool_t* threadpool_create(size_t worker_count, size_t workqueue_size) {
    if (worker_count == 0 || workqueue_size == 0) return NULL;
	
    threadpool_t *threadpool = malloc(sizeof(*threadpool));
    threadpool->workqueue = queue_create(workqueue_size, sizeof(work_t));
    threadpool->workers = malloc(worker_count * sizeof(thrd_t));
    threadpool->worker_count = worker_count;
    threadpool->complete = false;
	
    cnd_init(&threadpool->cnd);
    mtx_init(&threadpool->mtx, mtx_plain);
	
    for (int i = 0; i < worker_count; i++) {
        thrd_create(&threadpool->workers[i], threadpool_thread, threadpool);
    }
	
    return threadpool;
}

/* Enqueue an action to the threadpool's internal queue. */
void threadpool_enqueue(threadpool_t* threadpool, void (*fn)(void*), void *arg) {
    work_t work = { fn, arg };
    mtx_lock(&threadpool->mtx);
    queue_push(threadpool->workqueue, &work);
    cnd_signal(&threadpool->cnd);
    mtx_unlock(&threadpool->mtx);
}

/* Wait for all workers in the thread pool to finish, and all tasks in the internal queue to be handled. */
void threadpool_join(threadpool_t* threadpool) {   
    mtx_lock(&threadpool->mtx);
    threadpool->complete = true;
    cnd_broadcast(&threadpool->cnd);
    mtx_unlock(&threadpool->mtx);
	
    for (int i = 0; i < threadpool->worker_count; i++) {
        thrd_join(threadpool->workers[i], NULL);
    }
}

/* Free the threadpool back to the heap. */
void threadpool_free(threadpool_t* threadpool) {
    queue_free(threadpool->workqueue);
    cnd_destroy(&threadpool->cnd);
    mtx_destroy(&threadpool->mtx);
    free(threadpool->workers);
    free(threadpool);
}
