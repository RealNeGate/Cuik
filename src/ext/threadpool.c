#include "threadpool.h"
#include "threads.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdatomic.h>

#ifndef _WIN32
#include <semaphore.h>

#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#endif

#include <timer.h>
#include <arena.h>

// HACK(NeGate): i wanna call tb_free_thread_resources on thread exit...
extern void tb_free_thread_resources(void);

struct threadpool_t {
	atomic_bool running;
	atomic_uint read_pointer; // Read
	atomic_uint write_pointer; // Write

	atomic_uint completion_goal;
	atomic_uint completion_count;

	int thread_count;
	unsigned int queue_size_mask;

#ifdef _WIN32
	HANDLE sem;
#else
	sem_t  sem;
#endif

	thrd_t* threads;
	work_t* work;
	mtx_t   mutex;
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

	timed_block("thread") {
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

	tb_free_thread_resources();
	arena_free(&thread_arena);
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
#else
	if (sem_init(&threadpool->sem, 0 /* shared between threads */, worker_count) != 0) {
		fprintf(stderr, "error: could not create semaphore!\n");
		abort();
	}
#endif

    for (int i = 0; i < worker_count; i++) {
        if (thrd_create(&threadpool->threads[i], threadpool_thread, threadpool) != thrd_success) {
			fprintf(stderr, "error: could not create worker threads!\n");
			abort();
		}
    }

	mtx_init(&threadpool->mutex, mtx_plain);

    return threadpool;
}

void threadpool_submit(threadpool_t* threadpool, work_routine fn, void* arg) {
	mtx_lock(&threadpool->mutex);
	{
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
	}
	mtx_unlock(&threadpool->mutex);

#ifdef _WIN32
	ReleaseSemaphore(threadpool->sem, 1, 0);
#else
	sem_post(&threadpool->sem);
#endif
}

void threadpool_work_while_wait(threadpool_t* threadpool) {
	while (threadpool->completion_goal != threadpool->completion_count) {
		if (do_work(threadpool)) {
			thrd_yield();
		}
	}

	threadpool->completion_goal = 0;
	threadpool->completion_count = 0;
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

#ifdef _WIN32
	ReleaseSemaphore(threadpool->sem, threadpool->thread_count, 0);
#else
	// wake everyone
	for (size_t i = 0; i < threadpool->thread_count; i++) {
		sem_post(&threadpool->sem);
	}
#endif

#ifdef _WIN32
	WaitForMultipleObjects(threadpool->thread_count, threadpool->threads, TRUE, INFINITE);

	for (int i = 0; i < threadpool->thread_count; i++) CloseHandle(threadpool->threads[i]);
	CloseHandle(threadpool->sem);
#else
	for (int i = 0; i < threadpool->thread_count; i++) {
		thrd_join(threadpool->threads[i], NULL);
	}
	sem_destroy(&threadpool->sem);
#endif

	mtx_destroy(&threadpool->mutex);
	free(threadpool->threads);
	free(threadpool->work);
	free(threadpool);
}

int threadpool_get_thread_count(threadpool_t* threadpool) {
	return threadpool->thread_count;
}
