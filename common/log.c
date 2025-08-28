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
 * Modified by NeGate to enforce C11 mutexes
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

    void *udata;
    #if CUIK_ALLOW_THREADS
    mtx_t lock;
    #endif
    int level;
    bool quiet;
    Callback callbacks[MAX_CALLBACKS];
} L;

static const char *level_strings[] = {
    "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

#ifdef LOG_USE_COLOR
static const char *level_colors[] = {
    "\x1b[94m", "\x1b[36m", "\x1b[32m", "\x1b[33m", "\x1b[31m", "\x1b[35m"
};
#endif

static uint64_t log_time_start;

static uint64_t get_nanos(void) {
    struct timespec ts;
    timespec_get(&ts, TIME_UTC);
    return ((uint64_t)ts.tv_sec * UINT64_C(1000000000)) + ts.tv_nsec;
}

static void stdout_callback(log_Event *ev) {
    #ifdef LOG_USE_COLOR
    fprintf(
        ev->udata, "Thread-%d %.4f s %s%-5s\x1b[0m \x1b[90m%s:%d:\x1b[0m ",
        ev->tid, ev->time / 1000000.0, level_colors[ev->level], level_strings[ev->level],
        ev->file, ev->line);
    #else
    fprintf(
        ev->udata, "Thread-%d %.4f s %-5s %s:%d: ",
        ev->tid, ev->time / 1000000.0, level_strings[ev->level], ev->file, ev->line);
    #endif
    vfprintf(ev->udata, ev->fmt, ev->ap);
    fprintf(ev->udata, "\n");
    fflush(ev->udata);
}

static void file_callback(log_Event *ev) {
    fprintf(
        ev->udata, "Thread-%d %.4f s %-5s %s:%d: ",
        ev->tid, ev->time / 1000000.0, level_strings[ev->level], ev->file, ev->line);
    vfprintf(ev->udata, ev->fmt, ev->ap);
    fprintf(ev->udata, "\n");
    fflush(ev->udata);
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

int log_add_callback(log_LogFn fn, void *udata, int level) {
    for (int i = 0; i < MAX_CALLBACKS; i++) {
        if (!L.callbacks[i].fn) {
            L.callbacks[i] = (Callback) { fn, udata, level };
            return 0;
        }
    }
    return -1;
}

int log_add_fp(FILE *fp, int level) {
    return log_add_callback(file_callback, fp, level);
}

static void init_event(log_Event *ev, void *udata) {
    if (!ev->time) {
        ev->time = (get_nanos() / 1000) - log_time_start;
    }
    ev->udata = udata;
}

static void log_init(void) {
    log_time_start = get_nanos() / 1000;
    #if CUIK_ALLOW_THREADS
    mtx_init(&L.lock, mtx_plain);
    #endif
}

void log_log(int level, const char *file, int line, const char *fmt, ...) {
    log_Event ev = {
        .fmt   = fmt,
        .file  = file,
        .line  = line,
        .level = level,
        #if _WIN32
        .tid = GetCurrentThreadId(),
        #else
        .tid = getpid(),
        #endif
    };

    call_once(&L.init, log_init);
    #if CUIK_ALLOW_THREADS
    mtx_lock(&L.lock);
    #endif

    if (!L.quiet && level >= L.level) {
        init_event(&ev, stderr);
        va_start(ev.ap, fmt);
        stdout_callback(&ev);
        va_end(ev.ap);
    }

    for (int i = 0; i < MAX_CALLBACKS && L.callbacks[i].fn; i++) {
        Callback *cb = &L.callbacks[i];
        if (level >= cb->level) {
            init_event(&ev, cb->udata);
            va_start(ev.ap, fmt);
            cb->fn(&ev);
            va_end(ev.ap);
        }
    }

    #if CUIK_ALLOW_THREADS
    mtx_unlock(&L.lock);
    #endif
}
