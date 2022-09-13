// SPDX-FileCopyrightText: © 2022 Phillip Trudeau-Tavara <pmttavara@protonmail.com>
// SPDX-License-Identifier: 0BSD

/* TODO

Core API:

  - Completely contextless; you pass in params to begin()/end(), get a packed begin/end struct
      - Simple, handmade, user has full control and full responsibility

Optional Helper APIs:

  - Buffered-writing API
      - Caller allocates and stores a buffer for multiple events
      - begin()/end() writes chunks to the buffer
      - Function invokes a callback when the buffer is full and needs flushing
          - Can a callback be avoided? The function indicates when the buffer must be flushed?

  - Compression API: would require a mutexed lockable context (yuck...)
      - Either using a ZIP library, a name cache + TIDPID cache, or both (but ZIP is likely more than enough!!!)
      - begin()/end() writes compressed chunks to a caller-determined destination
          - The destination can be the buffered-writing API or a custom user destination
      - Ultimately need to take a lock with some granularity... can that be the caller's responsibility?

  - fopen()/fwrite() API: requires a context (no mutex needed, since fwrite() takes a lock)
      - begin()/end() writes chunks to a FILE*
          - before writing them to disk, the chunks can optionally be sent through the compression API
              - is this opt-in or opt-out?
          - the write to disk can optionally use the buffered writing API


Example Threaded Implementation:
    enum { RING_BUFFER_SIZE = 65536 };
    struct Event {
        uintptr_t is_end; // same size as const char* to avoid padding bytes
        const char *name;
        double when;
    };
    struct RegisteredThread {
        uint32_t pid;
        uint32_t tid;
        _Atomic uint64_t read_head;
        _Atomic uint64_t write_head;
#ifdef DYNAMIC_STRINGS
        void *allocator_userdata;
#endif
        Event events[RING_BUFFER_SIZE];
    };
    struct ProfileContext {
        Semaphore recording;
        Semaphore any_thread_has_written;
        bool never_drop_events;
        Mutex mutex; {
            RegisteredThread **registered_threads;
            int n_threads;
            SpallContext ctx;
        }
    };

    ProfileContext profile_init(, bool start_recording) {
        
    }

    void output_profile(ProfileContext *profile) {
        mutex_lock(&profile->mutex); {
            for (auto &thread : registered_threads) {
                if (thread.read_head <= thread.write_head - (RING_BUFFER_SIZE - 1)) {
                    printf(!"Ring tear. Increase the ring buffer size! :(");
                    SpallTraceBeginTidPid(&profile->ctx, "Ring tear. Increase the ring buffer size! :(", event.when, thread.tid, thread.pid);
                    thread.read_head = 0xffffffffffffffffull;
                    continue; // TODO: depth recovery
                }
                while (thread.read_head < thread.write_head) {
                    Event event = thread.events[thread.read_head & (RING_BUFFER_SIZE - 1)];
                    if (!event.is_end) {
                        SpallTraceBeginTidPid(&profile->ctx, event.name, event.when, thread.tid, thread.pid);
                    } else {
                        SpallTraceEndTidPid(&profile->ctx, event.when, thread.tid, thread.pid);
                    }
                    ++thread.read_head; // atomic
                }
            }
        }
        mutex_unlock(&profile->mutex);
        SpallFlush();
    }

    int output_thread(void *userdata) {
        ProfileContext *profile = (ProfileContext *)userdata;
        while (true) {
            wait_for_semaphore_forever(profile->recording);
            wait_for_semaphore_forever(profile->any_thread_has_written);
            output_profile(profile);
        }
    }

    // note: /Ob1 is how you would get FORCE_INLINE to work on msvc in debug mode
    inline void trace_begin(ProfileContext *profile, RegisteredThread *thread, const char *name) {
        if (UNLIKELY(profile->never_drop_events & profile->recording)) { // note: bitwise and could reduce branch predict slots?
            if (UNLIKELY(thread.read_head <= thread.write_head - (RING_BUFFER_SIZE - 1))) {
                output_profile(profile);
            }
        }
        Event *event = &thread->events[thread->write_head & (RING_BUFFER_SIZE - 1)];
#ifdef DYNAMIC_STRINGS
        if (LIKELY(event->name)) SPALL_FREE(event->name, thread->allocator_userdata);
        name = SPALL_STRDUP(name, thread->allocator_userdata);
#endif
        *event = { false, thread->thread_depth++, name, __rdtsc() };
        ++thread->write_head; // atomic
        signal_semaphore(profile->any_thread_has_written);
    }
    inline void trace_end(ProfileContext *profile, RegisteredThread *thread, EventID id) {
        Event *event = &thread->events[thread->write_head & (RING_BUFFER_SIZE - 1)];
        thread->events[thread->write_head & (RING_BUFFER_SIZE - 1)] = { true, id, thread->events[thread->write_head & (RING_BUFFER_SIZE - 1)].name, __rdtsc() };
        ++thread->write_head; // atomic
        signal_semaphore(profile->any_thread_has_written);
    }

    RegisteredThread *thread_init(ProfileContext *profile, u32 pid, u32 tid, u8 ring_buffer_size_power) {
        // handle = CreateSemaphoreA()
        RegisteredThread *result = array_calloc_and_append(&profile->registered_threads, &profile->n_threads);
    }
    void thread_quit(RegisteredThread *thread) {
        output_profile(profile);
        array_remove_unordered_and_free(&profile->registered_threads, thread);
    }
*/

#if defined(FLINT_BUFFER_PROFILING) && !defined(FLINT_BUFFER_PROFILING_GET_TIME)
#error "You must #define FLINT_BUFFER_PROFILING_GET_TIME() to profile buffer flushes."
#endif

#ifndef FLINT_H
#define FLINT_H

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#pragma pack(push, 1)

typedef struct FlintHeader {
    uint64_t magic_header; // = 0x0BADF00D
    uint64_t version; // = 0
    double timestamp_unit;
    uint64_t must_be_0;
} FlintHeader;

typedef struct FlintString {
    uint8_t length;
    char bytes[1];
} FlintString;

enum {
    FlintEventType_Invalid             = 0,
    FlintEventType_Custom_Data         = 1, // Basic readers can skip this.

    FlintEventType_Complete            = 2,
    FlintEventType_Begin               = 3,
    FlintEventType_End                 = 4,
    FlintEventType_Instant             = 5,

    FlintEventType_Overwrite_Timestamp = 6, // Retroactively change timestamp units - useful for incrementally improving RDTSC frequency.
    FlintEventType_Update_Checksum     = 7, // Verify rolling checksum. Basic readers/writers can ignore/omit this.
};

typedef struct FlintCompleteEvent {
    uint8_t type; // = FlintEventType_Complete
    uint32_t pid;
    uint32_t tid;
    double when;
    double duration;
    FlintString name;
} FlintCompleteEvent;

typedef struct FlintBeginEvent {
    uint8_t type; // = FlintEventType_Begin
    uint32_t pid;
    uint32_t tid;
    double when;
    FlintString name;
} FlintBeginEvent;

typedef struct FlintEndEvent {
    uint8_t type; // = FlintEventType_End
    uint32_t pid;
    uint32_t tid;
    double when;
} FlintEndEvent;

typedef struct FlintCompleteEventMax {
    FlintCompleteEvent event;
    char name_bytes[254];
} FlintCompleteEventMax;

typedef struct FlintBeginEventMax {
    FlintBeginEvent event;
    char name_bytes[254];
} FlintBeginEventMax;

#pragma pack(pop)

typedef struct FlintProfile {
    FILE *file;
    double timestamp_unit;
    uint64_t is_json;
} FlintProfile;

typedef struct FlintRecentString {
    char *pointer;
    uint8_t length;
} FlintRecentString;

// Important!: If you are writing Begin/End events, then do NOT write
//             events for the same PID + TID pair on different buffers!!!
typedef struct FlintBuffer {
    void *data;
    size_t length;

    // Internal data - don't assign this
    size_t head;
    FlintProfile *ctx;
    uint8_t recent_string_index;
    FlintRecentString recent_strings[256]; // ring buffer
    // TODO: uint32_t last_tid;
    // TODO: uint32_t last_pid;
} FlintBuffer;

#ifdef __cplusplus
extern "C" {
#endif

// Profile context
FlintProfile FlintInit    (const char *filename, double timestamp_unit);
FlintProfile FlintInitJson(const char *filename, double timestamp_unit);
void         FlintQuit    (FlintProfile *ctx);

bool FlintFlush(FlintProfile *ctx);

// Buffer API
extern FlintBuffer FlintSingleThreadedBuffer;

bool FlintBufferInit (FlintProfile *ctx, FlintBuffer *wb);
bool FlintBufferQuit (FlintProfile *ctx, FlintBuffer *wb);

bool FlintBufferFlush(FlintProfile *ctx, FlintBuffer *wb);

// Begin events
bool FlintTraceBegin         (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name);
bool FlintTraceBeginTid      (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, uint32_t tid);
bool FlintTraceBeginLen      (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len);
bool FlintTraceBeginLenTid   (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len, uint32_t tid);
bool FlintTraceBeginTidPid   (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name,                       uint32_t tid, uint32_t pid);
bool FlintTraceBeginLenTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len, uint32_t tid, uint32_t pid);

// End events
bool FlintTraceEnd      (FlintProfile *ctx, FlintBuffer *wb, double when);
bool FlintTraceEndTid   (FlintProfile *ctx, FlintBuffer *wb, double when, uint32_t tid);
bool FlintTraceEndTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, uint32_t tid, uint32_t pid);

// Complete events
bool FlintTraceComplete         (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name);
bool FlintTraceCompleteTid      (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, uint32_t tid);
bool FlintTraceCompleteLen      (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len);
bool FlintTraceCompleteLenTid   (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len, uint32_t tid);
bool FlintTraceCompleteTidPid   (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name,                       uint32_t tid, uint32_t pid);
bool FlintTraceCompleteLenTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len, uint32_t tid, uint32_t pid);

#ifdef __cplusplus
}
#endif

#endif // FLINT_H

#ifdef FLINT_IMPLEMENTATION
#ifndef FLINT_IMPLEMENTED
#define FLINT_IMPLEMENTED

#ifdef __cplusplus
extern "C" {
#endif

#ifdef FLINT_BUFFER_PROFILING
#define FLINT_BUFFER_PROFILE_BEGIN() double time_begin = (FLINT_BUFFER_PROFILING_GET_TIME());
#define FLINT_BUFFER_PROFILE_END(name) \
    do { \
        double time_end = (FLINT_BUFFER_PROFILING_GET_TIME()); \
        if (!FlintTraceCompleteTid(ctx, NULL, time_begin, time_end - time_begin, "" name "", (uintptr_t)wb->data % 1000000 + 4000000000)) return false; \
    } while (0)
#else
#define FLINT_BUFFER_PROFILE_BEGIN() do {} while (0)
#define FLINT_BUFFER_PROFILE_END(name) do { (void)("" name ""); } while (0)
#endif

extern char FlintSingleThreadedBufferData[];
char FlintSingleThreadedBufferData[1 << 16];
FlintBuffer FlintSingleThreadedBuffer = {FlintSingleThreadedBufferData, sizeof(FlintSingleThreadedBufferData)};

static bool Flint__FileWrite(FILE *f, void *p, size_t n) {
    // if (feof(f)) return false;
    if (ferror(f)) return false;
    if (fwrite(p, n, 1, f) != 1) return false;
    return true;
}

static void Flint__BufferPushString(FlintBuffer *wb, size_t n, signed long name_len) {
    FlintRecentString recent_string = {0};
    // precon: wb
    // precon: n > 0
    // precon: name_len > 0
    // precon: name_len < n
    // precon: name_len <= 255
    if (wb->head + n > wb->length) return; // will (try to) flush or do an unbuffered write, so don't push a string
    recent_string.pointer = (char *)wb->data + wb->head + n - name_len;
    recent_string.length = (uint8_t)name_len;
    wb->recent_strings[wb->recent_string_index++] = recent_string;
}

// returns -1 or a backreference number in [0, 255] -- telling you how many "strings ago" the string can be found (0 is the most recent string)
static int Flint__BufferFindString(FlintBuffer *wb, char *name, signed long name_len) {
    // precon: wb
    // precon: wb->data
    // precon: name
    // precon: name_len > 0
    // precon: name_len <= 255
    for (int i = 0; i < 256; i++) {
        unsigned int index = ((wb->recent_string_index - 1) - i + 256) % 256;
        FlintRecentString recent_string = wb->recent_strings[index];
        if (!recent_string.pointer) return -1; // early-out: NULLs mean the ring buffer isn't even full yet
        if (recent_string.length == name_len && memcmp(name, recent_string.pointer, recent_string.length) == 0) {
            return i;
        }
    }
    return -1;
}

static bool Flint__BufferFlush(FlintProfile *ctx, FlintBuffer *wb) {
    // precon: wb
    // precon: wb->data
    // precon: wb->head <= wb->length
    if (wb->ctx != ctx) return false; // Buffer must be bound to this context (or to NULL)
    if (wb->head && ctx) {
        if (!ctx->file) return false;
        FLINT_BUFFER_PROFILE_BEGIN();
        if (!Flint__FileWrite(ctx->file, wb->data, wb->head)) return false;
        FLINT_BUFFER_PROFILE_END("Buffer Flush");
    }
    wb->head = 0;
    wb->recent_string_index = 0;
    memset(wb->recent_strings, 0, sizeof(wb->recent_strings));
    return true;
}

static bool Flint__BufferWrite(FlintProfile *ctx, FlintBuffer *wb, void *p, size_t n) {
    // precon: !wb || wb->head < wb->length
    // precon: ctx->file
    if (!wb) return Flint__FileWrite(ctx->file, p, n);
    if (wb->head + n > wb->length && !Flint__BufferFlush(ctx, wb)) return false;
    if (n > wb->length) {
        FLINT_BUFFER_PROFILE_BEGIN();
        if (!Flint__FileWrite(ctx->file, p, n)) return false;
        FLINT_BUFFER_PROFILE_END("Unbuffered Write");
        return true;
    }
    memcpy((char *)wb->data + wb->head, p, n);
    wb->head += n;
    return true;
}

bool FlintBufferFlush(FlintProfile *ctx, FlintBuffer *wb) {
    if (!wb) return false;
    if (!wb->data) return false;
    if (!Flint__BufferFlush(ctx, wb)) return false;
    return true;
}

bool FlintBufferInit(FlintProfile *ctx, FlintBuffer *wb) {
    if (!FlintBufferFlush(NULL, wb)) return false;
    wb->ctx = ctx;
    return true;
}
bool FlintBufferQuit(FlintProfile *ctx, FlintBuffer *wb) {
    if (!FlintBufferFlush(ctx, wb)) return false;
    wb->ctx = NULL;
    return true;
}

bool FlintBufferAbort(FlintBuffer *wb) {
    if (!wb) return false;
    wb->ctx = NULL;
    if (!Flint__BufferFlush(NULL, wb)) return false;
    return true;
}

static FlintProfile Flint__Init(const char *filename, double timestamp_unit, bool is_json) {
    FlintProfile ctx;
    memset(&ctx, 0, sizeof(ctx));
    if (timestamp_unit < 0) return ctx;
    if (!filename) return ctx;
    ctx.file = fopen(filename, "wb"); // TODO: handle utf8 and long paths on windows
    if (!ctx.file) { FlintQuit(&ctx); return ctx; }
    ctx.timestamp_unit = timestamp_unit;
    ctx.is_json = is_json;
    if (ctx.is_json) {
        if (fprintf(ctx.file, "{\"traceEvents\":[\n") <= 0) { FlintQuit(&ctx); return ctx; }
    } else {
        FlintHeader header;
        header.magic_header = 0x0BADF00D;
        header.version = 0;
        header.timestamp_unit = timestamp_unit;
        header.must_be_0 = 0;
        if (!Flint__FileWrite(ctx.file, &header, sizeof(header))) { FlintQuit(&ctx); return ctx; }
    }
    return ctx;
}

FlintProfile FlintInit    (const char *filename, double timestamp_unit) { return Flint__Init(filename, timestamp_unit, false); }
FlintProfile FlintInitJson(const char *filename, double timestamp_unit) { return Flint__Init(filename, timestamp_unit,  true); }

void FlintQuit(FlintProfile *ctx) {
    if (!ctx) return;
    if (ctx->file) {
        if (ctx->is_json) {
            fseek(ctx->file, -2, SEEK_CUR); // seek back to overwrite trailing comma
            fprintf(ctx->file, "\n]}\n");
        }
        fflush(ctx->file);
        fclose(ctx->file);
    }
    memset(ctx, 0, sizeof(*ctx));
}

bool FlintFlush(FlintProfile *ctx) {
    if (!ctx) return false;
    if (!ctx->file) return false;
    if (fflush(ctx->file)) return false;
    return true;
}

bool FlintTraceBeginLenTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len, uint32_t tid, uint32_t pid) {
    FlintBeginEventMax ev;
    if (!ctx) return false;
    if (!name) return false;
    if (!ctx->file) return false;
    // if (ctx->times_are_u64) return false;
    if (name_len <= 0) return false;
    if (name_len > 255) name_len = 255; // will be interpreted as truncated in the app (?)
    ev.event.type = FlintEventType_Begin;
    ev.event.pid = pid;
    ev.event.tid = tid;
    ev.event.when = when;
    ev.event.name.length = (uint8_t)name_len;
    memcpy(ev.event.name.bytes, name, (uint8_t)name_len);
    if (ctx->is_json) {
        if (fprintf(ctx->file,
                    "{\"name\":\"%.*s\",\"ph\":\"B\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
                    (int)ev.event.name.length, ev.event.name.bytes,
                    ev.event.pid,
                    ev.event.tid,
                    ev.event.when * ctx->timestamp_unit)
            <= 0) return false;
    } else {
        if (!Flint__BufferWrite(ctx, wb, &ev, sizeof(FlintBeginEvent) + (uint8_t)name_len - 1)) return false;
    }
    return true;
}
bool FlintTraceBeginTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, uint32_t tid, uint32_t pid) {
    unsigned long name_len;
    if (!name) return false;
    name_len = strlen(name);
    if (!name_len) return false;
    return FlintTraceBeginLenTidPid(ctx, wb, when, name, (signed long)name_len, tid, pid);
}
bool FlintTraceBeginLenTid(FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len, uint32_t tid) { return FlintTraceBeginLenTidPid(ctx, wb, when, name, name_len, tid, 0); }
bool FlintTraceBeginLen   (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, signed long name_len)               { return FlintTraceBeginLenTidPid(ctx, wb, when, name, name_len,   0, 0); }
bool FlintTraceBeginTid   (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name, uint32_t tid)                       { return FlintTraceBeginTidPid   (ctx, wb, when, name,           tid, 0); }
bool FlintTraceBegin      (FlintProfile *ctx, FlintBuffer *wb, double when, const char *name)                                     { return FlintTraceBeginTidPid   (ctx, wb, when, name,             0, 0); }

bool FlintTraceCompleteLenTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len, uint32_t tid, uint32_t pid) {
    FlintCompleteEventMax ev;
    if (!ctx) return false;
    if (!name) return false;
    if (!ctx->file) return false;
    // if (ctx->times_are_u64) return false;
    if (name_len <= 0) return false;
    if (name_len > 255) name_len = 255; // will be interpreted as truncated in the app (?)
    ev.event.type = FlintEventType_Complete;
    ev.event.pid = pid;
    ev.event.tid = tid;
    ev.event.when = when;
    ev.event.duration = duration;
    ev.event.name.length = (uint8_t)name_len;
    memcpy(ev.event.name.bytes, name, (uint8_t)name_len);
    if (ctx->is_json) {
        if (fprintf(ctx->file,
                    "{\"name\":\"%.*s\",\"ph\":\"X\",\"pid\":%u,\"tid\":%u,\"ts\":%f,\"dur\":%f},\n",
                    (int)ev.event.name.length, ev.event.name.bytes,
                    ev.event.pid,
                    ev.event.tid,
                    ev.event.when * ctx->timestamp_unit,
                    ev.event.duration * ctx->timestamp_unit)
            <= 0) return false;
    } else {
        if (!Flint__BufferWrite(ctx, wb, &ev, sizeof(FlintCompleteEvent) + (uint8_t)name_len - 1)) return false;
    }
    return true;
}
bool FlintTraceCompleteTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, uint32_t tid, uint32_t pid) {
    unsigned long name_len;
    if (!name) return false;
    name_len = strlen(name);
    if (!name_len) return false;
    return FlintTraceCompleteLenTidPid(ctx, wb, when, duration, name, (signed long)name_len, tid, pid);
}
bool FlintTraceCompleteLenTid(FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len, uint32_t tid) { return FlintTraceCompleteLenTidPid(ctx, wb, when, duration, name, name_len, tid, 0); }
bool FlintTraceCompleteLen   (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, signed long name_len)               { return FlintTraceCompleteLenTidPid(ctx, wb, when, duration, name, name_len,   0, 0); }
bool FlintTraceCompleteTid   (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name, uint32_t tid)                       { return FlintTraceCompleteTidPid   (ctx, wb, when, duration, name,           tid, 0); }
bool FlintTraceComplete      (FlintProfile *ctx, FlintBuffer *wb, double when, double duration, const char *name)                                     { return FlintTraceCompleteTidPid   (ctx, wb, when, duration, name,             0, 0); }

bool FlintTraceEndTidPid(FlintProfile *ctx, FlintBuffer *wb, double when, uint32_t tid, uint32_t pid) {
    FlintEndEvent ev;
    if (!ctx) return false;
    if (!ctx->file) return false;
    // if (ctx->times_are_u64) return false;
    ev.type = FlintEventType_End;
    ev.pid = pid;
    ev.tid = tid;
    ev.when = when;
    if (ctx->is_json) {
        if (fprintf(ctx->file,
                    "{\"ph\":\"E\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
                    ev.pid,
                    ev.tid,
                    ev.when * ctx->timestamp_unit)
            <= 0) return false;
    } else {
        if (!Flint__BufferWrite(ctx, wb, &ev, sizeof(ev))) return false;
    }
    return true;
}

bool FlintTraceEndTid(FlintProfile *ctx, FlintBuffer *wb, double when, uint32_t tid) { return FlintTraceEndTidPid(ctx, wb, when, tid, 0); }
bool FlintTraceEnd   (FlintProfile *ctx, FlintBuffer *wb, double when)               { return FlintTraceEndTidPid(ctx, wb, when,   0, 0); }

#ifdef __cplusplus
}
#endif

#endif // FLINT_IMPLEMENTED
#endif // FLINT_IMPLEMENTATION

/*
Zero-Clause BSD (0BSD)

Copyright (c) 2022, Phillip Trudeau-Tavara
All rights reserved.

Permission to use, copy, modify, and/or distribute this software
for any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/
