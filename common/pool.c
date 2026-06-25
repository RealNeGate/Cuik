// MIT License
//
// Copyright (c) 2022 Colin Davidson
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// https://github.com/colrdavidson/workpool/blob/main/pool.h
#include "pool.h"

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

#ifdef CUIK_USE_SPALL_AUTO
#include "spall_native_auto.h"
#endif

// cross-platform thread wrappers, because microsoft couldn't be arsed to take 5 seconds and
// do this and save all the junior devs and codebases everywhere from this pile of nonsense.
#if defined(__linux__) || defined(__APPLE__)

#include <stdatomic.h>
#include <unistd.h>
#include <errno.h>

typedef pthread_t TPool_ThreadHandle;

#define tpool_thread_start(t) pthread_create(&(t)->thread, NULL, )
#define tpool_thread_end(t)   pthread_join((t)->thread, NULL)

#elif defined(_WIN32)

#include <windows.h>
#include <process.h>

typedef ptrdiff_t ssize_t;
#endif

#include <stdatomic.h>

#define TPOOL_LOAD(val) atomic_load(&val)
#define TPOOL_CAS(addr, expected, desired) atomic_compare_exchange_weak(addr, &expected, desired)
#define TPOOL_ATOMIC_FUTEX_INC(val) (atomic_fetch_add_explicit(&val, 1, memory_order_acquire))
#define TPOOL_ATOMIC_FUTEX_DEC(val) (atomic_fetch_sub_explicit(&val, 1, memory_order_acquire))
#define __debugbreak() __builtin_trap()

// cross-platform futex, because we can't just have nice things. All the popular platforms have them under the hood,
// but giving them to users? NO! Users are too stupid to have nice things, save them for the fedora-wearing elite.
#if defined(__linux__)

#include <linux/futex.h>
#include <sys/syscall.h>

void _tpool_signal(TPool_Futex *addr) {
    int ret = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, NULL, NULL, 0);
    if (ret == -1) {
        perror("Futex wake");
        __debugbreak();
    }
}

void _tpool_broadcast(TPool_Futex *addr) {
    int ret = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, INT32_MAX, NULL, NULL, 0);
    if (ret == -1) {
        perror("Futex wake");
        __debugbreak();
    }
}

void _tpool_wait(TPool_Futex *addr, TPool_Futex val) {
    for (;;) {
        int ret = syscall(SYS_futex, addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL, 0);
        if (ret == -1) {
            if (errno != EAGAIN) {
                perror("Futex wait");
                __debugbreak();
            } else {
                return;
            }
        } else if (ret == 0) {
            if (*addr != val) {
                return;
            }
        }
    }
}

#elif defined(__APPLE__)

#define UL_COMPARE_AND_WAIT	0x00000001
#define ULF_WAKE_ALL        0x00000100
#define ULF_NO_ERRNO        0x01000000

/* timeout is specified in microseconds */
int __ulock_wait(uint32_t operation, void *addr, uint64_t value, uint32_t timeout);
int __ulock_wake(uint32_t operation, void *addr, uint64_t wake_value);

void _tpool_signal(TPool_Futex *addr) {
    for (;;) {
        int ret = __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, 0);
        if (ret >= 0) {
            return;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }
        printf("futex wake fail?\n");
        __debugbreak();
    }
}

void _tpool_broadcast(TPool_Futex *addr) {
    for (;;) {
        int ret = __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO | ULF_WAKE_ALL, addr, 0);
        if (ret >= 0) {
            return;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }
        printf("futex wake fail?\n");
        __debugbreak();
    }
}

void _tpool_wait(TPool_Futex *addr, TPool_Futex val) {
    for (;;) {
        int ret = __ulock_wait(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, val, 0);
        if (ret >= 0) {
            if (*addr != val) {
                return;
            }
            continue;
        }
        ret = -ret;
        if (ret == EINTR || ret == EFAULT) {
            continue;
        }
        if (ret == ENOENT) {
            return;
        }

        printf("futex wait fail?\n");
        __debugbreak();
    }
}

#elif defined(_WIN32)
void _tpool_signal(TPool_Futex *addr) {
    WakeByAddressSingle((void *)addr);
}

void _tpool_broadcast(TPool_Futex *addr) {
    WakeByAddressAll((void *)addr);
}

void _tpool_wait(TPool_Futex *addr, TPool_Futex val) {
    for (;;) {
        int ret = WaitOnAddress(addr, (void *)&val, sizeof(val), INFINITE);
        if (*addr != val) break;
    }
}

#elif defined(__FreeBSD__)

#include <sys/types.h>
#include <sys/umtx.h>

void _tpool_signal(TPool_Futex *addr) {
    _umtx_op(addr, UMTX_OP_WAKE, 1, 0, 0);
}

void _tpool_broadcast(TPool_Futex *addr) {
    _umtx_op(addr, UMTX_OP_WAKE, INT32_MAX, 0, 0);
}

void _tpool_wait(TPool_Futex *addr, TPool_Futex val) {
    for (;;) {
        int ret = _umtx_op(addr, UMTX_OP_WAIT_UINT, val, 0, NULL);
        if (ret == 0) {
            if (errno == ETIMEDOUT || errno == EINTR) {
                continue;
            }

            perror("Futex wait");
            __debugbreak();
        } else if (ret == 0) {
            if (*addr != val) {
                return;
            }
        }
    }
}

#elif defined(__OpenBSD__)

#include <sys/futex.h>

void _tpool_signal(TPool_Futex *addr) {
    for (;;) {
        int ret = futex(addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, NULL, NULL);
        if (ret == -1) {
            if (errno == ETIMEDOUT || errno == EINTR) {
                continue;
            }

            perror("Futex wake");
            __debugbreak();
        } else if (ret == 1) {
            return;
        }
    }
}

void _tpool_broadcast(TPool_Futex *addr) {
    for (;;) {
        int ret = futex(addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, INT32_MAX, NULL, NULL);
        if (ret == -1) {
            if (errno == ETIMEDOUT || errno == EINTR) {
                continue;
            }

            perror("Futex wake");
            __debugbreak();
        } else if (ret > 0) {
            return;
        }
    }
}

void _tpool_wait(TPool_Futex *addr, TPool_Futex val) {
    for (;;) {
        int ret = futex(addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL);
        if (ret == -1) {
            if (*addr != val) {
                return;
            }

            if (errno == ETIMEDOUT || errno == EINTR) {
                continue;
            }

            perror("Futex wait");
            __debugbreak();
        }
    }
}

#endif

TPool_Thread_Local int tpool_current_thread_idx;

#define GRAB_SUCCESS 0
#define GRAB_EMPTY   1
#define GRAB_FAILED  2

typedef struct {
    TPool_Atomic ssize_t size;
    TPool_Task *buffer;
} TPool_RingBuffer;

typedef struct {
    TPool_Atomic ssize_t top;
    TPool_Atomic ssize_t bottom;

    TPool_Atomic(TPool_RingBuffer *) ring;
} TPool_Queue;

typedef struct TPool_Thread {
    thrd_t thread;
    int idx;

    TPool_Queue queue;
    struct TPool *pool;
} TPool_Thread;

TPool_RingBuffer *tpool_ring_make(ssize_t size) {
    TPool_RingBuffer *ring = cuik_malloc(sizeof(TPool_RingBuffer));
    ring->size = size;
    ring->buffer = cuik_calloc(ring->size, sizeof(TPool_Task));
    return ring;
}

TPool_Queue tpool_queue_make(ssize_t size) {
    TPool_Queue d = {};
    TPool_RingBuffer *ring = tpool_ring_make(size);
    atomic_store(&d.ring, ring);
    return d;
}

void tpool_queue_delete(TPool_Queue *q) {
    cuik_free(q->ring->buffer);
    cuik_free(q->ring);
}

TPool_RingBuffer *tpool_ring_grow(TPool_RingBuffer *ring, ssize_t bottom, ssize_t top) {
    TPool_RingBuffer *new_ring = tpool_ring_make(ring->size * 2);
    for (ssize_t i = top; i < bottom; i++) {
        new_ring->buffer[i % new_ring->size] = ring->buffer[i % ring->size];
    }
    return new_ring;
}

void _thread_init(TPool *pool, TPool_Thread *thread, int idx) {
    thread->queue = tpool_queue_make(1 << 1);
    thread->pool = pool;
    thread->idx = idx;
}

void _tpool_queue_push(TPool_Thread *thread, tpool_task_proc* fn, int val_count, void** val) {
    assert(val_count <= 3);
    ssize_t bot                = atomic_load_explicit(&thread->queue.bottom, memory_order_relaxed);
    ssize_t top                = atomic_load_explicit(&thread->queue.top,    memory_order_acquire);
    TPool_RingBuffer *cur_ring = atomic_load_explicit(&thread->queue.ring,   memory_order_relaxed);

    ssize_t size = bot - top;
    if (size > (cur_ring->size - 1)) {
        // Queue is full
        thread->queue.ring = tpool_ring_grow(thread->queue.ring, bot, top);
        cur_ring = atomic_load_explicit(&thread->queue.ring, memory_order_relaxed);
    }

    cur_ring->buffer[bot % cur_ring->size].do_work = fn;
    memcpy(&cur_ring->buffer[bot % cur_ring->size].args, val, val_count * sizeof(void*));

    atomic_thread_fence(memory_order_release);
    atomic_store_explicit(&thread->queue.bottom, bot + 1, memory_order_relaxed);

    TPOOL_ATOMIC_FUTEX_INC(thread->pool->tasks_left);
    TPOOL_ATOMIC_FUTEX_INC(thread->pool->tasks_available);
    _tpool_broadcast(&thread->pool->tasks_available);
}

int _tpool_queue_take(TPool_Thread *thread, TPool_Task *task) {
    ssize_t bot = atomic_load_explicit(&thread->queue.bottom, memory_order_relaxed) - 1;
    TPool_RingBuffer *cur_ring = atomic_load_explicit(&thread->queue.ring, memory_order_relaxed);
    atomic_store_explicit(&thread->queue.bottom, bot, memory_order_relaxed);
    atomic_thread_fence(memory_order_seq_cst);

    ssize_t top = atomic_load_explicit(&thread->queue.top, memory_order_relaxed);
    if (top <= bot) {
        // Queue is not empty

        *task = cur_ring->buffer[bot % cur_ring->size];
        if (top == bot) {
            // Only one entry left in queue
            if (!atomic_compare_exchange_strong_explicit(&thread->queue.top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
                // Race failed
                atomic_store_explicit(&thread->queue.bottom, bot + 1, memory_order_relaxed);
                return GRAB_EMPTY;
            }

            atomic_store_explicit(&thread->queue.bottom, bot + 1, memory_order_relaxed);
            return GRAB_SUCCESS;
        }

        // We got a task without hitting a race
        return GRAB_SUCCESS;
    } else {
        // Queue is empty
        atomic_store_explicit(&thread->queue.bottom, bot + 1, memory_order_relaxed);
        return GRAB_EMPTY;
    }
}

int _tpool_queue_steal(TPool_Thread *thread, TPool_Task *task) {
    ssize_t top = atomic_load_explicit(&thread->queue.top, memory_order_acquire);
    atomic_thread_fence(memory_order_seq_cst);
    ssize_t bot = atomic_load_explicit(&thread->queue.bottom, memory_order_acquire);

    int ret = GRAB_EMPTY;
    if (top < bot) {
        // Queue is not empty
        TPool_RingBuffer *cur_ring = atomic_load_explicit(&thread->queue.ring, memory_order_consume);
        *task = cur_ring->buffer[top % cur_ring->size];

        if (!atomic_compare_exchange_strong_explicit(&thread->queue.top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
            // Race failed
            ret = GRAB_FAILED;
        } else {
            ret = GRAB_SUCCESS;
        }
    }
    return ret;
}

void cuikperf_thread_start(void);
void cuikperf_thread_stop(void);
void cuikperf_region_start(const char* label, const char* extra);
void cuikperf_region_end(void);

int _tpool_worker(void *ptr) {
    TPool_Task task;
    TPool_Thread *current_thread = (TPool_Thread *)ptr;
    tpool_current_thread_idx = current_thread->idx;
    TPool *pool = current_thread->pool;

    cuikperf_thread_start();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_init(tpool_current_thread_idx, SPALL_DEFAULT_BUFFER_SIZE);
    #endif

    for (;;) {
        work_start:
        if (!pool->running) {
            break;
        }

        // If we've got tasks to process, work through them
        size_t finished_tasks = 0;
        while (!_tpool_queue_take(current_thread, &task)) {
            task.do_work(pool, task.args);
            TPOOL_ATOMIC_FUTEX_DEC(pool->tasks_left);

            finished_tasks += 1;
        }
        if (finished_tasks > 0 && !TPOOL_LOAD(pool->tasks_left)) {
            _tpool_signal(&pool->tasks_left);
        }

        // If there's still work somewhere and we don't have it, steal it
        if (TPOOL_LOAD(pool->tasks_left)) {
            int idx = current_thread->idx;
            for (int i = 0; i < pool->thread_count; i++) {
                if (!TPOOL_LOAD(pool->tasks_left)) {
                    break;
                }

                idx = (idx + 1) % pool->thread_count;
                TPool_Thread *thread = &pool->threads[idx];

                TPool_Task task;
                int ret = _tpool_queue_steal(thread, &task);
                if (ret == GRAB_FAILED) {
                    goto work_start;
                } else if (ret == GRAB_EMPTY) {
                    continue;
                }

                task.do_work(pool, task.args);
                TPOOL_ATOMIC_FUTEX_DEC(pool->tasks_left);

                if (!TPOOL_LOAD(pool->tasks_left)) {
                    _tpool_signal(&pool->tasks_left);
                }

                goto work_start;
            }
        }

        // if we've done all our work, and there's nothing to steal, go to sleep
        int32_t state = TPOOL_LOAD(pool->tasks_available);
        if (!pool->running) { break; }
        _tpool_wait(&pool->tasks_available, state);
    }

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    #endif

    cuikperf_thread_stop();
    return 0;
}

void tpool_add_task(TPool *pool, tpool_task_proc* fn, void* val) {
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];
    _tpool_queue_push(current_thread, fn, 1, &val);
}

void tpool_add_task2(TPool *pool, tpool_task_proc* fn, int arg_count, void** args) {
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];
    _tpool_queue_push(current_thread, fn, arg_count, args);
}

void tpool_wait(TPool *pool) {
    TPool_Task task;
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];

    while (TPOOL_LOAD(pool->tasks_left)) {

        // if we've got tasks on our queue, run them
        while (!_tpool_queue_take(current_thread, &task)) {
            task.do_work(pool, task.args);
            TPOOL_ATOMIC_FUTEX_DEC(pool->tasks_left);
        }


        // is this mem-barriered enough?
        // This *must* be executed in this order, so the futex wakes immediately
        // if rem_tasks has changed since we checked last, otherwise the program
        // will permanently sleep
        TPool_Futex rem_tasks = TPOOL_LOAD(pool->tasks_left);
        if (!rem_tasks) {
            break;
        }

        _tpool_wait(&pool->tasks_left, rem_tasks);
    }

}

void tpool_init(TPool *pool, int child_thread_count) {
    int thread_count = child_thread_count + 1;
    pool->thread_count = thread_count;
    pool->threads = cuik_malloc(sizeof(TPool_Thread) * pool->thread_count);

    pool->running = true;

    // setup the main thread
    _thread_init(pool, &pool->threads[0], 0);
    tpool_current_thread_idx = 0;

    for (int i = 1; i < pool->thread_count; i++) {
        _thread_init(pool, &pool->threads[i], i);
        thrd_create(&pool->threads[i].thread, _tpool_worker, &pool->threads[i]);
    }
}

void tpool_destroy(TPool *pool) {
    pool->running = false;
    for (int i = 1; i < pool->thread_count; i++) {
        TPOOL_ATOMIC_FUTEX_INC(pool->tasks_available);
        _tpool_broadcast(&pool->tasks_available);
        thrd_join(pool->threads[i].thread, NULL);
    }
    for (int i = 0; i < pool->thread_count; i++) {
        tpool_queue_delete(&pool->threads[i].queue);
    }

    cuik_free(pool->threads);
}
