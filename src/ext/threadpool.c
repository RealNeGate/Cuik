#include "threadpool.h"
#include "threads.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdatomic.h>

struct threadpool_t {
	atomic_bool running;
	atomic_uint read_pointer; // Read
	atomic_uint write_pointer; // Write
	
	atomic_uint completion_goal;
	atomic_uint completion_count;
	
	int thread_count;
	unsigned int queue_size_mask;
	
#if _WIN32
	HANDLE sem;
#else
#error "Implemented unix based semaphore"
#endif
	thrd_t* threads;
	
	work_t* work;
};

static bool do_work(threadpool_t* threadpool) {
	uint32_t read_ptr = threadpool->read_pointer;
	uint32_t new_read_ptr = (read_ptr + 1) & threadpool->queue_size_mask;
	
	if (read_ptr != threadpool->write_pointer) {
		if (atomic_compare_exchange_strong(&threadpool->read_pointer, &read_ptr, new_read_ptr)) {
			threadpool->work[read_ptr].fn(threadpool->work[read_ptr].arg);
			threadpool->completion_count++;
		}
		
		return false;
	}
	
	return true;
}

static int threadpool_thread(void* arg) {
    threadpool_t* threadpool = arg;
	
	while (threadpool->running) {
		if (do_work(threadpool)) {
#if _WIN32
			WaitForSingleObjectEx(threadpool->sem, -1, false); // wait for jobs
#endif
		}
	}
	
    return 0;
}

threadpool_t* threadpool_create(size_t worker_count, size_t workqueue_size) {
    if (worker_count == 0 || workqueue_size == 0) return NULL;
	if ((workqueue_size & (workqueue_size - 1)) != 0) return NULL;
	
    threadpool_t* threadpool = calloc(1, sizeof(threadpool_t));
    threadpool->work = malloc(workqueue_size * sizeof(work_t));
    threadpool->threads = malloc(worker_count * sizeof(thrd_t));
    threadpool->thread_count = worker_count;
    threadpool->running = true;
	threadpool->queue_size_mask = workqueue_size - 1;
	
#if _WIN32
	threadpool->sem = CreateSemaphoreExA(0, worker_count, worker_count, 0, 0, SEMAPHORE_ALL_ACCESS);
#endif
	
    for (int i = 0; i < worker_count; i++) {
        thrd_create(&threadpool->threads[i], threadpool_thread, threadpool);
    }
	
    return threadpool;
}

void threadpool_submit(threadpool_t* threadpool, work_routine fn, void* arg) {
	uint32_t write_ptr = threadpool->write_pointer;
	uint32_t new_write_ptr = (write_ptr + 1) & threadpool->queue_size_mask;
	while (new_write_ptr == threadpool->read_pointer) {
		// TODO: Stall until the jobs are complete.
	}
	
	threadpool->work[write_ptr] = (work_t) {
		.fn = fn,
		.arg = arg
	};
	
	threadpool->completion_goal++;
	threadpool->write_pointer = new_write_ptr;
	
#if _WIN32
	ReleaseSemaphore(threadpool->sem, 1, 0);
#endif
}

void threadpool_wait(threadpool_t* threadpool) {
	while (threadpool->completion_goal != threadpool->completion_count) {
		thrd_yield();
	}
	
	threadpool->completion_goal = 0;
	threadpool->completion_count = 0;
}

void threadpool_free(threadpool_t* threadpool) {
	threadpool->running = false;
#if _WIN32
	ReleaseSemaphore(threadpool->sem, threadpool->thread_count, 0);
#endif
	
	for (int i = 0; i < threadpool->thread_count; i++) {
		thrd_join(threadpool->threads[i], NULL);
	}
	
#if _WIN32
	CloseHandle(threadpool->sem);
#endif
	
	free(threadpool->threads);
	free(threadpool->work);
	free(threadpool);
}
