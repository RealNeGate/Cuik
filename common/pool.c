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
#include "common.h"
#include "futex.h"
#include <stdatomic.h>

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

#ifdef CUIK_USE_SPALL_AUTO
#include "spall_native_auto.h"
#endif

#ifdef CUIK_USE_URING
#include <liburing.h>
#endif

// cross-platform thread wrappers, because microsoft couldn't be arsed to take 5 seconds and
// do this and save all the junior devs and codebases everywhere from this pile of nonsense.
#if defined(__linux__) || defined(__APPLE__)
#include <unistd.h>
#include <errno.h>
#elif defined(_WIN32)
#include <windows.h>
#include <process.h>
typedef ptrdiff_t ssize_t;
#endif

#define TPOOL_LOAD(val) atomic_load(&val)
#define TPOOL_CAS_STRONG(addr, expected, desired) atomic_compare_exchange_strong(addr, expected, desired)
#define TPOOL_CAS(addr, expected, desired) atomic_compare_exchange_weak(addr, &expected, desired)
#define TPOOL_ATOMIC_FUTEX_INC(val) (atomic_fetch_add_explicit(&val, 1, memory_order_acquire))
#define TPOOL_ATOMIC_FUTEX_DEC(val) (atomic_fetch_sub_explicit(&val, 1, memory_order_acquire))
#define __debugbreak() __builtin_debugtrap()

enum {
    POOL_IO_DEPTH = 64,
};

TPool_Thread_Local bool tpool_is_pool_thread = false;
TPool_Thread_Local int tpool_current_thread_idx = -1;

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

typedef struct {
    _Alignas(64) Futex io_head;
    _Alignas(64) Futex io_tail;
    _Alignas(64) uint64_t io_head_cache;
    _Alignas(64) TPool_ReadReq entries[POOL_IO_DEPTH];
} TPool_IOQueue;

typedef struct TPool_Thread {
    thrd_t thread;
    int idx;

    TPool_Queue queue;
    struct TPool *pool;

    // A pair thread is used to submit async I/O requests to, it's
    // only created whenever that happens because I really only need
    // it for the TB linker
    bool has_pair;
    thrd_t pair_thread;

    #ifdef CUIK_USE_URING
    struct io_uring io_ring;

    _Alignas(64) _Atomic(uint64_t) used_entries;
    _Alignas(64) TPool_ReadReq entries[POOL_IO_DEPTH];
    #else
    TPool_IOQueue* io_submit;
    #endif

    uint64_t curr_submit_i;
    uint64_t last_submit_i;
    uint64_t last_submit_t;

    // These are where completed I/O tasks go, other threads could steal from here.
    TPool_Queue io_complete_lo;
    TPool_Queue io_complete_hi;

    tpool_task_proc* last_run;
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
    thread->queue = tpool_queue_make(32);
    thread->pool = pool;
    thread->idx = idx;
}

uint64_t cuik_time_in_nanos(void);
void cuikperf_region_start(const char* fmt, const char* extra);
void cuikperf_region_end(void);

void _tpool_queue_push(TPool *pool, TPool_Queue *queue, tpool_task_proc* fn, int val_count, void** val) {
    assert(val_count <= 3);
    ssize_t bot                = atomic_load_explicit(&queue->bottom, memory_order_relaxed);
    ssize_t top                = atomic_load_explicit(&queue->top,    memory_order_acquire);
    TPool_RingBuffer *cur_ring = atomic_load_explicit(&queue->ring,   memory_order_relaxed);

    TPOOL_ATOMIC_FUTEX_INC(pool->tasks_left);
    TPOOL_ATOMIC_FUTEX_INC(pool->tasks_available);

    ssize_t size = bot - top;
    if (size > (cur_ring->size - 1)) {
        // Queue is full
        cuikperf_region_start("resize", NULL);
        queue->ring = tpool_ring_grow(queue->ring, bot, top);
        cur_ring = atomic_load_explicit(&queue->ring, memory_order_relaxed);
        cuikperf_region_end();
    }

    cur_ring->buffer[bot % cur_ring->size].do_work = fn;
    memcpy(&cur_ring->buffer[bot % cur_ring->size].args, val, val_count * sizeof(void*));

    atomic_thread_fence(memory_order_release);
    atomic_store_explicit(&queue->bottom, bot + 1, memory_order_relaxed);

    #if 1
    // Only broadcast if there's sleepers, this is probably not the best way to handle ngl
    uint64_t sleepers = pool->sleeping_tasks;
    if (sleepers != 0 && atomic_compare_exchange_strong(&pool->sleeping_tasks, &sleepers, 0)) {
        #if 0
        // rate limit the broadcast to avoid spending too much time in the OS.
        // We're gonna do one broadcast every 10us for now.
        uint64_t now_ticks = cuik_time_in_nanos() / 10000;
        if (pool->last_broadcast_tick != now_ticks) {
            pool->last_broadcast_tick = now_ticks;

            cuikperf_region_start("BROADCAST", NULL);
            futex_broadcast(&pool->tasks_available);
            cuikperf_region_end();
        }
        #endif

        cuikperf_region_start("BROADCAST", NULL);
        futex_broadcast(&pool->tasks_available);
        cuikperf_region_end();
    }
    #endif
}

static int _tpool_queue_take(TPool_Queue *queue, TPool_Task *task) {
    ssize_t bot = atomic_load_explicit(&queue->bottom, memory_order_relaxed) - 1;
    TPool_RingBuffer *cur_ring = atomic_load_explicit(&queue->ring, memory_order_relaxed);
    atomic_store_explicit(&queue->bottom, bot, memory_order_relaxed);
    atomic_thread_fence(memory_order_seq_cst);

    ssize_t top = atomic_load_explicit(&queue->top, memory_order_relaxed);
    if (top <= bot) {
        // Queue is not empty
        *task = cur_ring->buffer[bot % cur_ring->size];
        if (top == bot) {
            // Only one entry left in queue
            if (!atomic_compare_exchange_strong_explicit(&queue->top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
                // Race failed
                atomic_store_explicit(&queue->bottom, bot + 1, memory_order_relaxed);
                return GRAB_EMPTY;
            }

            atomic_store_explicit(&queue->bottom, bot + 1, memory_order_relaxed);
            return GRAB_SUCCESS;
        }

        // We got a task without hitting a race
        return GRAB_SUCCESS;
    } else {
        // Queue is empty
        atomic_store_explicit(&queue->bottom, bot + 1, memory_order_relaxed);
        return GRAB_EMPTY;
    }
}

static int _tpool_queue_steal(TPool_Queue *queue, TPool_Task *task) {
    ssize_t top = atomic_load_explicit(&queue->top, memory_order_acquire);
    atomic_thread_fence(memory_order_seq_cst);
    ssize_t bot = atomic_load_explicit(&queue->bottom, memory_order_acquire);

    int ret = GRAB_EMPTY;
    if (top < bot) {
        // Queue is not empty
        TPool_RingBuffer *cur_ring = atomic_load_explicit(&queue->ring, memory_order_consume);
        *task = cur_ring->buffer[top % cur_ring->size];

        if (!atomic_compare_exchange_strong_explicit(&queue->top, &top, top + 1, memory_order_seq_cst, memory_order_relaxed)) {
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

static int WAKEUP_TOKEN;
int _tpool_io_worker(void *ptr) {
    TPool_Thread* current_thread = (TPool_Thread*) ptr;
    tpool_current_thread_idx = current_thread->idx;
    TPool *pool = current_thread->pool;

    cuikperf_thread_start();
    cuikperf_region_start("I/O", NULL);

    const uint64_t top_bit = 1ull << 63ull;

    #ifdef CUIK_USE_URING
    struct io_uring* ring = &current_thread->io_ring;
    #else
    TPool_IOQueue* queue = current_thread->io_submit;
    #endif

    size_t stats_requests = 0;
    size_t stats_requested_size = 0;

    uint64_t t = 0;
    while (pool->running) {
        #ifdef CUIK_USE_URING
        cuikperf_region_end();

        // consume and process all the I/O completion responses
        struct io_uring_cqe* cqe;
        int ret = io_uring_wait_cqe(ring, &cqe);
        if (ret == -4) {
            continue;
        }

        cuikperf_region_start("I/O", NULL);
        if (ret < 0) {
            printf("error: io_uring_wait_cqe!");
            abort();
        }

        unsigned int head = 0, i = 0;
        io_uring_for_each_cqe(ring, head, cqe) {
            if (io_uring_cqe_get_data(cqe) == &WAKEUP_TOKEN) {
                break;
            }
            if (cqe->res < 0) {
                printf("error: io_uring_wait_cqe! %s\n", strerror(-cqe->res));
                abort();
            }

            // completions come in out of order, so we track a bitmap
            // rather than a queue like the pread impl.
            TPool_ReadReq req;
            TPool_ReadReq* src = io_uring_cqe_get_data(cqe);
            size_t req_i = src - current_thread->entries;
            memcpy(&req, src, sizeof(req));

            // free up slot now
            current_thread->used_entries &= ~(1ull << req_i);
            futex_signal(&current_thread->used_entries);

            // I/O response
            req.do_work(pool, &req);
            i++;
        }
        io_uring_cq_advance(ring, i);
        #else
        // wait for new requests
        uint64_t h = queue->io_head;
        while (h == t) {
            // refresh cache
            t = queue->io_tail;
            if (h == t) {
                // CAS to set the sleep bit on, if we succeed then we
                // sleep, if we fail work must've just been inserted.
                if (TPOOL_CAS_STRONG(&queue->io_tail, &t, t | top_bit)) {
                    cuikperf_region_end();
                    futex_wait(&queue->io_tail, t);
                    cuikperf_region_start("I/O", NULL);

                    // update tail to unflip the sleep bit
                    t = atomic_fetch_xor(&queue->io_tail, top_bit) ^ top_bit;
                    assert((t & top_bit) == 0);
                }
            }

            if (pool->running == 0) {
                goto done;
            }
        }

        // wait for reads to finish
        #if 0
        cuikperf_region_start("touch", NULL);
        TPool_ReadReq req = queue->entries[h % POOL_IO_DEPTH];
        // int res = mprotect(req.data, req.size, PROT_READ);
        volatile char* ptr = req.data;
        for (size_t i = 0; i < req.size; i += 4096) {
            int x = ptr[i];
        }
        stats_requested_size += req.size, stats_requests += 1;
        cuikperf_region_end();
        #else
        cuikperf_region_start("pread", NULL);
        TPool_ReadReq req = queue->entries[h % POOL_IO_DEPTH];
        pread(req.fd, req.data, req.size, req.offset);
        stats_requested_size += req.size, stats_requests += 1;
        cuikperf_region_end();
        #endif

        queue->io_head += 1;
        futex_signal(&queue->io_head);

        // Ideally the user understands that this is for
        // very low latency responses so they shouldn't block
        // it up for long.
        req.do_work(pool, &req);
        #endif
    }
    done:
    printf("STATS %.3f KiB | %zu\n", stats_requested_size / 1024.0f, stats_requests);
    cuikperf_region_end();
    cuikperf_thread_stop();
    return 0;
}

static size_t estimate_io_load(TPool_Thread* current_thread) {
    #ifdef CUIK_USE_URING
    return 0;
    #else
    if (current_thread->io_submit == NULL) {
        return 0;
    }

    const uint64_t top_bit = 1ull << 63ull;
    uint64_t next_t = current_thread->io_submit->io_tail & ~top_bit;
    return next_t - current_thread->io_submit->io_head;
    #endif
}

static void try_submit_io(TPool* pool, TPool_Thread* thread, bool force) {
    #if CUIK_USE_URING
    if (thread->has_pair) {
        uint64_t t = cuik_time_in_nanos();
        uint64_t elapsed = t - thread->last_submit_t;
        if ((elapsed > 100000 || force) && thread->curr_submit_i != thread->last_submit_i) {
            cuikperf_region_start("uring_submit", NULL);
            io_uring_submit(&thread->io_ring);
            thread->last_submit_t = t;
            thread->last_submit_i = thread->curr_submit_i;
            cuikperf_region_end();
        }
    }
    #endif
}

static void issue_load(TPool* pool, TPool_Thread* current_thread) {
    work_start:
    if (!pool->running) {
        return;
    }

    int64_t tasks_left = TPOOL_LOAD(pool->tasks_left);

    TPool_Task task;
    size_t old_finished;
    size_t finished_tasks = 0;
    do {
        old_finished = finished_tasks;

        // I/O completion tasks go into two queues, one is for short-lived tasks which are
        // likely to spawn more I/O jobs. These should run first, the other camp is
        // longer-lived tasks which should wait until I/O is saturated.
        //
        // If the I/O thread is currently backed up, don't run high priority tasks
        // since those are meant to issue more requests.
        uint64_t est = estimate_io_load(current_thread);
        while (est <= (POOL_IO_DEPTH * 3) / 4 && _tpool_queue_steal(&current_thread->io_complete_hi, &task) == GRAB_SUCCESS) {
            task.do_work(pool, task.args);
            est = estimate_io_load(current_thread);
            finished_tasks += 1;
        }

        if (_tpool_queue_take(&current_thread->queue, &task) == GRAB_SUCCESS) {
            task.do_work(pool, task.args);
            finished_tasks += 1;
        }

        if (_tpool_queue_steal(&current_thread->io_complete_lo, &task) == GRAB_SUCCESS) {
            task.do_work(pool, task.args);
            finished_tasks += 1;
        }
    } while (old_finished != finished_tasks);

    if (finished_tasks > 0) {
        tasks_left = atomic_fetch_sub(&pool->tasks_left, finished_tasks) - finished_tasks;
        assert(tasks_left >= 0);

        if (tasks_left == 0) {
            futex_signal(&pool->tasks_left);
        }
    }

    // If there's still work somewhere and we don't have it, steal it.
    if (TPOOL_LOAD(pool->tasks_left)) {
        bool dirty;
        do {
            dirty = false;

            int idx = current_thread->idx;
            for (int i = 0; i < pool->thread_count; i++) {
                if (!TPOOL_LOAD(pool->tasks_left)) {
                    break;
                }

                idx = (idx + 1) % pool->thread_count;
                TPool_Thread *thread = &pool->threads[idx];

                TPool_Task task;
                int ret = _tpool_queue_steal(&thread->queue, &task);
                dirty |= (ret == GRAB_FAILED);

                if (ret == GRAB_FAILED || ret == GRAB_EMPTY) {
                    ret = _tpool_queue_steal(&thread->io_complete_hi, &task);
                    dirty |= (ret == GRAB_FAILED);
                }

                if (ret == GRAB_FAILED || ret == GRAB_EMPTY) {
                    ret = _tpool_queue_steal(&thread->io_complete_lo, &task);
                    dirty |= (ret == GRAB_FAILED);
                }

                // fail twice? we'll try to find another thread, it might
                // make sense to prioritize certain thread scan orderings
                if (ret == GRAB_FAILED || ret == GRAB_EMPTY) {
                    continue;
                }

                task.do_work(pool, task.args);
                if (TPOOL_ATOMIC_FUTEX_DEC(pool->tasks_left) == 1) {
                    futex_signal(&pool->tasks_left);
                }
                goto work_start;
            }
        } while (dirty);
    }

    try_submit_io(pool, current_thread, true);
}

int _tpool_worker(void *ptr) {
    TPool_Task task;
    TPool_Thread *current_thread = (TPool_Thread *)ptr;
    tpool_current_thread_idx = current_thread->idx;
    tpool_is_pool_thread = true;
    TPool *pool = current_thread->pool;

    cuikperf_thread_start();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_init(tpool_current_thread_idx, SPALL_DEFAULT_BUFFER_SIZE);
    #endif

    cuikperf_region_start("Worker", NULL);
    for (;;) {
        issue_load(pool, current_thread);

        // if we've done all our work, and there's nothing to steal, go to sleep
        int32_t state = TPOOL_LOAD(pool->tasks_available);
        if (!pool->running) { break; }

        pool->sleeping_tasks |= 1ull << (tpool_current_thread_idx % 64ull);
        cuikperf_region_end();
        futex_wait(&pool->tasks_available, state);
        cuikperf_region_start("Worker", NULL);
    }
    cuikperf_region_end();

    #ifdef CUIK_USE_SPALL_AUTO
    spall_auto_thread_quit();
    #endif

    cuikperf_thread_stop();
    return 0;
}

static void tpool_init_io(TPool* pool, TPool_Thread* thread) {
    // lazy init of I/O thread
    if (!thread->has_pair) {
        thread->has_pair = true;
        thread->io_complete_lo = tpool_queue_make(64);
        thread->io_complete_hi = tpool_queue_make(64);

        #ifdef CUIK_USE_URING
        int ret = io_uring_queue_init(64, &thread->io_ring, 0);
        assert(ret == 0);
        #else
        thread->io_submit = cuik_aligned_alloc(sizeof(TPool_IOQueue), alignof(TPool_IOQueue));
        memset(thread->io_submit, 0, sizeof(TPool_IOQueue));
        #endif

        thrd_create(&thread->pair_thread, _tpool_io_worker, thread);
    }
}

void tpool_io_prep(TPool* pool) {
    assert(tpool_is_pool_thread);
    tpool_init_io(pool, &pool->threads[tpool_current_thread_idx]);
}

void tpool_io_read(TPool* pool, int fd, size_t offset, size_t size, void* data, tpool_io_task_proc* fn, void* arg0, void* arg1, void* arg2) {
    tpool_io_prep(pool);

    assert((offset & 511) == 0);
    assert((size   & 511) == 0);

    TPool_Thread* thread = &pool->threads[tpool_current_thread_idx];
    const uint64_t top_bit = 1ull << 63ull;

    #ifdef CUIK_USE_URING
    struct io_uring* ring = &thread->io_ring;
    struct io_uring_sqe* sqe = io_uring_get_sqe(ring);
    while (sqe == NULL) {
        cuikperf_region_start("sqe_pressure", NULL);
        io_uring_submit_and_wait(ring, 1);
        sqe = io_uring_get_sqe(ring);
        cuikperf_region_end();
    }

    uint64_t curr = thread->used_entries;
    for (;;) {
        if (curr == UINT64_MAX) {
            cuikperf_region_start("slot_pressure", NULL);
            io_uring_submit_and_wait(ring, 1);
            futex_wait(&thread->used_entries, curr);
            curr = thread->used_entries;
            cuikperf_region_end();
        }

        uint64_t free_i = __builtin_ffsll(~curr) - 1;
        uint64_t next   = curr | (1ull << free_i);
        if (atomic_compare_exchange_strong(&thread->used_entries, &curr, next)) {
            thread->entries[free_i] = (TPool_ReadReq){ fd, offset, size, data, fn, { arg0, arg1, arg2 } };
            atomic_thread_fence(memory_order_release);

            io_uring_prep_read(sqe, fd, data, size, offset);
            io_uring_sqe_set_data(sqe, &thread->entries[free_i]);

            // printf("PUSH %zu\n", free_i);
            thread->curr_submit_i++;
            try_submit_io(pool, thread, false);
            break;
        }
    }
    #else
    TPool_IOQueue* queue = thread->io_submit;
    uint64_t t = queue->io_tail;
    uint64_t next_t = (t & ~top_bit) + 1;

    // wait for the queue to empty up
    uint64_t h;
    while (h = queue->io_head_cache, next_t - h > POOL_IO_DEPTH - 1) {
        // refresh cache
        h = queue->io_head_cache = queue->io_head;

        if (next_t - h > POOL_IO_DEPTH - 1) {
            cuikperf_region_start("pressure", NULL);
            futex_wait(&queue->io_head, h);
            h = queue->io_head_cache = queue->io_head;
            cuikperf_region_end();
        }
    }

    queue->entries[t % POOL_IO_DEPTH] = (TPool_ReadReq){ fd, offset, size, data, fn, { arg0, arg1, arg2 } };
    atomic_thread_fence(memory_order_release);
    // since we're only incrementing this will preserve the sleep bit, given
    // we don't add 9 quintillion I/O requests I guess?
    atomic_fetch_add(&queue->io_tail, 1);
    // only signal the futex if the I/O thread is sleeping
    if (t & top_bit) {
        cuikperf_region_start("signal", NULL);
        futex_signal(&queue->io_tail);
        cuikperf_region_end();
    }
    #endif
}

void tpool_add_task(TPool *pool, tpool_task_proc* fn, void* val) {
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];
    _tpool_queue_push(pool, &current_thread->queue, fn, 1, &val);
}

void tpool_add_task2(TPool *pool, tpool_task_proc* fn, int arg_count, void** args) {
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];
    _tpool_queue_push(pool, &current_thread->queue, fn, arg_count, args);
}

void tpool_io_forward(TPool *pool, bool hi_prio, tpool_task_proc* fn, int arg_count, void** args) {
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];
    TPool_Queue* queue = hi_prio ? &current_thread->io_complete_hi : &current_thread->io_complete_lo;
    _tpool_queue_push(pool, queue, fn, arg_count, args);
}

void tpool_wait_for_jobs(TPool *pool, Futex* done, Futex* count) {
    TPool_Thread* thread = &pool->threads[tpool_current_thread_idx];

    FutexV c = *count;
    while (c = *count, *done != c) {
        FutexV avail = TPOOL_LOAD(pool->tasks_available);
        issue_load(pool, thread);

        uint64_t rem_tasks = pool->tasks_left;

        c = *count;
        if (*done == c) {
            break;
        }

        // assert(rem_tasks != 0);
        // futex_wait(&pool->tasks_left, rem_tasks);
    }
}

void tpool_wait_for_jobs2(TPool *pool, Futex* done, int64_t count) {
    TPool_Thread* thread = &pool->threads[tpool_current_thread_idx];
    while (*done != count) {
        FutexV avail = TPOOL_LOAD(pool->tasks_available);
        issue_load(pool, thread);

        pool->sleeping_tasks |= 1ull << (tpool_current_thread_idx % 64ull);
        futex_wait(&pool->tasks_available, avail);
    }
}

void tpool_wait(TPool *pool) {
    TPool_Task task;
    TPool_Thread *current_thread = &pool->threads[tpool_current_thread_idx];

    while (TPOOL_LOAD(pool->tasks_left)) {
        // if we've got tasks on our queue, run them
        while (!_tpool_queue_take(&current_thread->queue, &task)) {
            task.do_work(pool, task.args);
            TPOOL_ATOMIC_FUTEX_DEC(pool->tasks_left);
        }

        // is this mem-barriered enough?
        // This *must* be executed in this order, so the futex wakes immediately
        // if rem_tasks has changed since we checked last, otherwise the program
        // will permanently sleep
        Futex rem_tasks = TPOOL_LOAD(pool->tasks_left);
        if (!rem_tasks) {
            break;
        }

        futex_wait(&pool->tasks_left, rem_tasks);
    }

}

int tpool_num_threads(TPool *pool) {
    return pool->thread_count;
}

void tpool_init(TPool *pool, int child_thread_count) {
    cuikperf_region_start("tpool_init", NULL);

    int thread_count = child_thread_count + 1;
    pool->thread_count = thread_count;
    pool->threads = cuik_calloc(pool->thread_count, sizeof(TPool_Thread));

    pool->running = true;

    // setup the main thread
    _thread_init(pool, &pool->threads[0], 0);
    tpool_current_thread_idx = 0;
    tpool_is_pool_thread = true;

    for (int i = 1; i < pool->thread_count; i++) {
        _thread_init(pool, &pool->threads[i], i);
        thrd_create(&pool->threads[i].thread, _tpool_worker, &pool->threads[i]);
    }
    cuikperf_region_end();
}

void tpool_io_prep_all(TPool *pool) {
}

void tpool_destroy(TPool *pool) {
    cuikperf_region_start("tpool_destroy", NULL);

    pool->running = false;
    for (int i = 0; i < pool->thread_count; i++) {
        #ifdef CUIK_USE_URING
        if (pool->threads[i].has_pair) {
            struct io_uring* ring = &pool->threads[i].io_ring;
            struct io_uring_sqe *sqe = io_uring_get_sqe(ring);
            if (sqe != NULL) {
                io_uring_prep_nop(sqe);
                io_uring_sqe_set_data(sqe, &WAKEUP_TOKEN);
                io_uring_submit(ring);
            }
        }
        #else
        if (pool->threads[i].has_pair) {
            pool->threads[i].io_submit->io_tail = -1;
            futex_signal(&pool->threads[i].io_submit->io_tail);
        }
        #endif
    }
    for (int i = 1; i < pool->thread_count; i++) {
        TPOOL_ATOMIC_FUTEX_INC(pool->tasks_available);
        futex_broadcast(&pool->tasks_available);
        thrd_join(pool->threads[i].thread, NULL);
        if (pool->threads[i].has_pair) {
            thrd_join(pool->threads[i].pair_thread, NULL);
        }
    }
    if (pool->threads[0].has_pair) {
        thrd_join(pool->threads[0].pair_thread, NULL);
    }
    for (int i = 0; i < pool->thread_count; i++) {
        tpool_queue_delete(&pool->threads[i].queue);
    }

    cuik_free(pool->threads);
    cuikperf_region_end();
}
