/*
 * Copyright (c) 2020 rxi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Modified by NeGate for lovely reasons
 */
#include "log.h"
#include <inttypes.h>

#if CUIK_ALLOW_THREADS
#include <threads.h>
#endif

#ifdef _WIN32
#ifdef _POSIX_C_SOURCE
__declspec(dllimport) unsigned int GetCurrentThreadId(void);
#else
__declspec(dllimport) unsigned long GetCurrentThreadId(void);
#endif
#else
#include <unistd.h>
#endif

#define MAX_CALLBACKS 32

typedef struct {
    log_LogFn fn;
    void *udata;
    int level;
} Callback;

static struct {
    once_flag init;
    int level;
    bool quiet;
    Callback callbacks[MAX_CALLBACKS];
} L;

// https://synergy.cs.vt.edu/pubs/papers/scogland-queues-icpe15.pdf
enum {
    LOG_BUFFER_CAPACITY = 2048,

    QUEUE_SIZE = 64,
    MAX_ID = UINT32_MAX / (QUEUE_SIZE*2),
};

static struct {
    _Atomic uint32_t head, tail;
    _Atomic Futex ids[QUEUE_SIZE];
    struct {
        void* base;
        size_t len;
        Futex* status;
    } items[QUEUE_SIZE];
} queue;

static const char *level_strings[] = {
    "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

#ifdef LOG_USE_COLOR
static const char *level_colors[] = {
    "\x1b[94m", "\x1b[36m", "\x1b[32m", "\x1b[33m", "\x1b[31m", "\x1b[35m"
};
#endif

static uint64_t log_time_start;

// Each thread has a buffer which is split into two halves (double-buffering)
static thread_local bool log_buffer_to_write;
static thread_local int log_buffer_used;
static thread_local char* log_buffer;

static thread_local Futex log_buffer_writer_status;

// Auxillary thread just does writing so that our threads aren't disturbed
static thrd_t aux_thread;

static void log_enqueue(char* dst);

static uint64_t get_nanos(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return ((uint64_t)ts.tv_sec * UINT64_C(1000000000)) + ts.tv_nsec;
}

const char* log_level_string(int level) {
    return level_strings[level];
}

void log_set_level(int level) {
    L.level = level;
}

void log_set_quiet(bool enable) {
    L.quiet = enable;
}

static _Atomic bool log_running = true;
static Futex log_done = 0;
static int aux_work(void* arg) {
    while (log_running) {
        uint32_t ticket = atomic_fetch_add(&queue.head, 1);
        uint32_t target = ticket % QUEUE_SIZE;
        uint32_t id = ((ticket / QUEUE_SIZE) * 2) + 1;
        // wait for slot to be ready
        int64_t old;
        while (old = queue.ids[target], old != id) {
            // wait for changes
            futex_wait(&queue.ids[target], old);
            // did log_thread_flush cancel waiting? ok let's just dip out then
            if (!log_running) {
                goto dip;
            }
        }
        // read the slice we're gonna write out (we can write after we've freed up the queue slot)
        char* base    = queue.items[target].base;
        size_t len    = queue.items[target].len;
        Futex* status = queue.items[target].status;
        // slot's empty again
        atomic_store(&queue.ids[target], (id+1) % MAX_ID);
        futex_broadcast(&queue.ids[target]);
        assert(base[len-1] == '\n');
        fwrite(base, len, 1, stdout);
        // notify that we're done with writing (so that the thread can reuse it)
        *status = 0;
        futex_signal(status);
    }

    dip:
    log_done = 1;
    futex_signal(&log_done);

    return 0;
}

static void log_thread_flush(void* ptr) {
    // wait for all the pending aux thread writes, then pop off
    if (log_done == 0) {
        if (atomic_compare_exchange_strong(&log_running, &(bool){ true }, false)) {
            // now we forcibly wake up every id
            FOR_N(i, 0, QUEUE_SIZE) {
                queue.ids[i] = 0;
                futex_broadcast(&queue.ids[i]);
            }
        }

        futex_wait(&log_done, 0);
    }

    char* dst = &log_buffer[log_buffer_to_write ? LOG_BUFFER_CAPACITY : 0];
    fwrite(dst, log_buffer_used, 1, stdout);
}

static tss_t log_exit_key;
static void log_init(void) {
    log_time_start = get_nanos() / 1000;
    thrd_create(&aux_thread, aux_work, NULL);

    if (tss_create(&log_exit_key, log_thread_flush) != thrd_success) {
        abort();
    }
}

int log_add_callback(log_LogFn fn, void *udata, int level) {
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        if (!L.callbacks[i].fn) {
            L.callbacks[i] = (Callback) { fn, udata, level };
            return 0;
        }
    }
    return -1;
}

void log_log(int level, const char *file, int line, const char *fmt, ...) {
    if (level < L.level) {
        return;
    }

    #if _WIN32
    int tid = GetCurrentThreadId();
    #else
    int tid = getpid();
    #endif

    call_once(&L.init, log_init);

    if (log_buffer == NULL) {
        log_buffer = cuik__valloc(2 * LOG_BUFFER_CAPACITY);
        tss_set(log_exit_key, log_buffer);
    }

    for (;;) {
        uint64_t time = (get_nanos() / 1000) - log_time_start;
        char* dst = &log_buffer[log_buffer_to_write ? LOG_BUFFER_CAPACITY : 0];

        // begin writing to log buffer (if we can't complete the entire line we'll revert it).
        // this way all log buffer writes contain only complete lines.
        int used = log_buffer_used;
        #ifdef LOG_USE_COLOR
        int len = snprintf(
            &dst[used], LOG_BUFFER_CAPACITY - used, "T%-5d %-10.4f %s%-5s\x1b[0m \x1b[90m%s:%d:\x1b[0m ",
            tid, time / 1000000.0, level_colors[level], level_strings[level], file, line
        );
        #else
        int len = snprintf(
            &dst[used], LOG_BUFFER_CAPACITY - used, "T%-5d %-10.4f %-5s %s:%d: ",
            tid, time / 1000000.0, level_strings[level], file, line
        );
        #endif

        if (len >= 0 && len < LOG_BUFFER_CAPACITY - used) {
            used += len;

            va_list ap;
            va_start(ap, fmt);
            len = vsnprintf(&dst[used], LOG_BUFFER_CAPACITY - used, fmt, ap);
            va_end(ap);

            if (len >= 0 && len+1 < LOG_BUFFER_CAPACITY - used) {
                dst[used + len] = '\n';

                // success log write, we're happy now
                log_buffer_used = used + len + 1;
                log_enqueue(dst);
                return;
            }
        }

        log_enqueue(dst);
    }
}

void log_flush(void) {
    char* dst = &log_buffer[log_buffer_to_write ? LOG_BUFFER_CAPACITY : 0];
    log_enqueue(dst);
}

static void log_enqueue(char* dst) {
    // we need this to be 0 for us to queue up more IO, that way we know that the chunk
    // we flip to is ready for use.
    futex_wait_eq(&log_buffer_writer_status, 0);
    log_buffer_writer_status = 1;

    // queue up this chunk
    uint32_t ticket = atomic_fetch_add(&queue.tail, 1);
    uint32_t target = ticket % QUEUE_SIZE;
    uint32_t id = (ticket / QUEUE_SIZE) * 2;
    // wait for slot to open up
    futex_wait_eq(&queue.ids[target], id);
    // pretty sure this doesn't need to be atomic, until "target"
    // is changed these writes shouldn't need to be acknowledged.
    queue.items[target].base   = dst;
    queue.items[target].len    = log_buffer_used;
    queue.items[target].status = &log_buffer_writer_status;
    // writer thread can now see what we enqueued
    atomic_store(&queue.ids[target], (id+1) % MAX_ID);
    futex_broadcast(&queue.ids[target]);

    log_buffer_used = 0;
    log_buffer_to_write = !log_buffer_to_write;
}
