#ifndef THREADPOOL_H
#define THREADPOOL_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct threadpool_t threadpool_t;
typedef void work_routine(void*);

typedef struct {
    work_routine* fn;
    char arg[56];
} work_t;

threadpool_t* threadpool_create(size_t worker_count, size_t workqueue_size);
void threadpool_submit(threadpool_t* threadpool, work_routine fn, size_t arg_size, void* arg);
void threadpool_wait(threadpool_t* threadpool);
void threadpool_work_one_job(threadpool_t* threadpool);
void threadpool_work_while_wait(threadpool_t* threadpool);
void threadpool_free(threadpool_t* threadpool);
int threadpool_get_thread_count(threadpool_t* threadpool);

// Kinda OOP-y but i don't care, we can use callbacks as a treat.
#ifdef HAS_CUIK_CLASS
Cuik_IThreadpool threadpool_create_class(int threads, int workqueue_size);
#endif /* HAS_CUIK_CLASS */
#endif /* THREADPOOL_H */



// https://github.com/skeeto/scratch/blob/master/misc/queue.c
#ifdef THREADPOOL_IMPL
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <threads.h>
#include <stdatomic.h>

#ifndef _WIN32
#include <semaphore.h>

#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#endif

#include <cuik.h>

// HACK(NeGate): i wanna call tb_free_thread_resources on thread exit...
extern void tb_free_thread_resources(void);
extern void spallperf__start_thread(void);
extern void spallperf__stop_thread(void);

typedef _Atomic uint32_t atomic_uint32_t;

// 1 << QEXP is the size of the queue per thread
#define QEXP 6

struct threadpool_t {
    atomic_bool running;

    // this skeeto guy is kinda sick wit it
    atomic_uint32_t queue;
    atomic_uint32_t jobs_done;

    int thread_count;
    work_t* work;
    thrd_t* threads;

    #ifdef _WIN32
    HANDLE sem;
    #else
    sem_t sem;
    #endif
};

static work_t* ask_for_work(threadpool_t* threadpool, uint32_t* save) {
    uint32_t r = *save = threadpool->queue;
    uint32_t mask = (1u << QEXP) - 1;
    uint32_t head = r & mask;
    uint32_t tail = (r >> 16) & mask;

    return head != tail ? &threadpool->work[tail] : NULL;
}

static bool do_work(threadpool_t* threadpool) {
    uint32_t save;
    work_t* job = NULL;

    do {
        job = ask_for_work(threadpool, &save);
        if (job == NULL) {
            // take a nap if we ain't find shit
            return true;
        }

        // don't continue until we successfully commited the queue pop
    } while (!atomic_compare_exchange_strong(&threadpool->queue, &save, save + 0x10000));

    job->fn(job->arg);
    threadpool->jobs_done -= 1;
    return false;
}

static int threadpool_thread(void* arg) {
    threadpool_t* threadpool = arg;
    spallperf__start_thread();

    CUIK_TIMED_BLOCK("thread") {
        while (threadpool->running) {
            if (do_work(threadpool)) {
                #ifdef _WIN32
                WaitForSingleObjectEx(threadpool->sem, -1, false); // wait for jobs
                #else
                sem_wait(&threadpool->sem);
                #endif
            }
        }
    }

    spallperf__stop_thread();
    tb_free_thread_resources();
    cuik_free_thread_resources();
    return 0;
}

threadpool_t* threadpool_create(size_t worker_count, size_t workqueue_size) {
    if (worker_count == 0 || workqueue_size == 0) {
        return NULL;
    }

    if ((workqueue_size & (workqueue_size - 1)) != 0) {
        return NULL;
    }

    threadpool_t* threadpool = malloc(sizeof(threadpool_t));
    *threadpool = (threadpool_t){ 0 };

    threadpool->work = malloc(workqueue_size * sizeof(work_t));
    threadpool->threads = malloc(worker_count * sizeof(thrd_t));
    threadpool->thread_count = worker_count;
    threadpool->running = true;

    #if _WIN32
    threadpool->sem = CreateSemaphoreExA(0, worker_count, worker_count, 0, 0, SEMAPHORE_ALL_ACCESS);
    #else
    if (sem_init(&threadpool->sem, 0 /* shared between threads */, worker_count) != 0) {
        fprintf(stderr, "error: could not create semaphore!\n");
        return NULL;
    }
    #endif

    for (int i = 0; i < worker_count; i++) {
        if (thrd_create(&threadpool->threads[i], threadpool_thread, threadpool) != thrd_success) {
            fprintf(stderr, "error: could not create worker threads!\n");
            return NULL;
        }
    }

    return threadpool;
}

void threadpool_submit(threadpool_t* threadpool, work_routine fn, size_t arg_size, void* arg) {
    ptrdiff_t i = 0;
    for (;;) {
        // might wanna change the memory order on this atomic op
        uint32_t r = threadpool->queue;

        uint32_t mask = (1u << QEXP) - 1;
        uint32_t head = r & mask;
        uint32_t tail = (r >> 16) & mask;
        uint32_t next = (head + 1u) & mask;
        if (r & 0x8000) { // avoid overflow on commit
            threadpool->queue &= ~0x8000;
        }

        // it don't fit...
        if (next != tail) {
            i = head;
            break;
        }
    }

    assert(arg_size < sizeof(threadpool->work[i].arg));
    threadpool->work[i].fn = fn;
    memcpy(threadpool->work[i].arg, arg, arg_size);

    threadpool->jobs_done += 1;
    threadpool->queue += 1;

    #ifdef _WIN32
    ReleaseSemaphore(threadpool->sem, 1, 0);
    #else
    sem_post(&threadpool->sem);
    #endif
}

void threadpool_work_one_job(threadpool_t* threadpool) {
    do_work(threadpool);
}

void threadpool_work_while_wait(threadpool_t* threadpool) {
    while (threadpool->jobs_done > 0) {
        if (do_work(threadpool)) {
            thrd_yield();
        }
    }
}

void threadpool_wait(threadpool_t* threadpool) {
    while (threadpool->jobs_done > 0) {
        thrd_yield();
    }
}

void threadpool_free(threadpool_t* threadpool) {
    threadpool->running = false;

    #ifdef _WIN32
    ReleaseSemaphore(threadpool->sem, threadpool->thread_count, 0);
    WaitForMultipleObjects(threadpool->thread_count, threadpool->threads, TRUE, INFINITE);

    for (int i = 0; i < threadpool->thread_count; i++) {
        thrd_join(threadpool->threads[i], NULL);
    }
    CloseHandle(threadpool->sem);
    #else
    for (int i = 0; i < threadpool->thread_count; i++) {
        thrd_join(threadpool->threads[i], NULL);
    }

    // wake everyone
    for (size_t i = 0; i < threadpool->thread_count; i++) {
        sem_post(&threadpool->sem);
    }
    sem_destroy(&threadpool->sem);
    #endif

    free(threadpool->threads);
    free(threadpool->work);
    free(threadpool);
}

int threadpool_get_thread_count(threadpool_t* threadpool) {
    return threadpool->thread_count;
}

#ifdef HAS_CUIK_CLASS
static void threadpool__submit(void* user_data, void fn(void*), size_t arg_size, void* arg) {
    threadpool_submit((threadpool_t*) user_data, fn, arg_size, arg);
}

static void threadpool__work_one_job(void* user_data) {
    threadpool_work_one_job((threadpool_t*) user_data);
}

Cuik_IThreadpool threadpool_create_class(int worker_count, int workqueue_size) {
    threadpool_t* tp = threadpool_create(worker_count, workqueue_size);

    return (Cuik_IThreadpool){
        .user_data = tp,
        .submit = threadpool__submit,
        .work_one_job = threadpool__work_one_job
    };
}

void threadpool_destroy_class(Cuik_IThreadpool* thread_pool) {
    if (thread_pool) threadpool_free(thread_pool->user_data);
}
#endif /* HAS_CUIK_CLASS */
#endif /* THREADPOOL_IMPL */
