// SPDX-FileCopyrightText: © 2022 Phillip Trudeau-Tavara <pmttavara@protonmail.com>
// SPDX-License-Identifier: 0BSD

/*

TODO: Core API:

  - Completely contextless; you pass in params to begin()/end(), get a packed begin/end struct
      - Simple, handmade, user has full control and full responsibility

TODO: Optional Helper APIs:

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

*/

#ifndef SPALL_H
#define SPALL_H

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define SPALL_MIN(a, b) (((a) < (b)) ? (a) : (b))

#pragma pack(push, 1)

typedef struct SpallHeader {
    uint64_t magic_header; // = 0x0BADF00D
    uint64_t version; // = 1
    double timestamp_unit;
    uint64_t must_be_0;
} SpallHeader;

enum {
    SpallEventType_Invalid             = 0,
    SpallEventType_Custom_Data         = 1, // Basic readers can skip this.
    SpallEventType_StreamOver          = 2,

    SpallEventType_Begin               = 3,
    SpallEventType_End                 = 4,
    SpallEventType_Instant             = 5,

    SpallEventType_Overwrite_Timestamp = 6, // Retroactively change timestamp units - useful for incrementally improving RDTSC frequency.
};

typedef struct SpallBeginEvent {
    uint8_t type; // = SpallEventType_Begin
    uint8_t category;

    uint32_t pid;
    uint32_t tid;
    double when;

    uint8_t name_length;
    uint8_t args_length;

    // char name[1];
    // char args[1];
} SpallBeginEvent;

typedef struct SpallBeginEventMax {
    SpallBeginEvent event;
    char name_bytes[255];
    char args_bytes[255];
} SpallBeginEventMax;

typedef struct SpallEndEvent {
    uint8_t type; // = SpallEventType_End
    uint32_t pid;
    uint32_t tid;
    double when;
} SpallEndEvent;

#pragma pack(pop)

typedef struct SpallProfile {
    double timestamp_unit;
    bool (*write)(struct SpallProfile *self, const void *data, size_t length);
    bool (*flush)(struct SpallProfile *self);
    void (*close)(struct SpallProfile *self);
    union {
        FILE *file;
        void *userdata;
    };
} SpallProfile;

// Important!: If you are writing Begin/End events, then do NOT write
//             events for the same PID + TID pair on different buffers!!!
typedef struct SpallBuffer {
    void *data;
    size_t length;

    // Internal data - don't assign this
    size_t head;
    SpallProfile *ctx;
} SpallBuffer;

#ifdef __cplusplus
extern "C" {
    #endif

    // Profile context
    SpallProfile SpallInit    (const char *filename, double timestamp_unit);
    void         SpallQuit    (SpallProfile *ctx);

    bool SpallFlush(SpallProfile *ctx);

    // Buffer API
    extern SpallBuffer SpallSingleThreadedBuffer;

    bool SpallBufferInit (SpallProfile *ctx, SpallBuffer *wb);
    bool SpallBufferQuit (SpallProfile *ctx, SpallBuffer *wb);

    bool SpallBufferFlush(SpallProfile *ctx, SpallBuffer *wb);

    // Begin events
    bool SpallTraceBeginLen      (SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, double when);
    bool SpallTraceBeginLenTid   (SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, uint32_t tid, double when);
    bool SpallTraceBeginLenTidPid(SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, uint32_t tid, uint32_t pid, double when);

    // End events
    bool SpallTraceEnd      (SpallProfile *ctx, SpallBuffer *wb, double when);
    bool SpallTraceEndTid   (SpallProfile *ctx, SpallBuffer *wb, uint32_t tid, double when);
    bool SpallTraceEndTidPid(SpallProfile *ctx, SpallBuffer *wb, uint32_t tid, uint32_t pid, double when);

    #ifdef __cplusplus
}
#endif

#endif // SPALL_H

#ifdef SPALL_IMPLEMENTATION
#ifndef SPALL_IMPLEMENTED
#define SPALL_IMPLEMENTED

#ifdef __cplusplus
extern "C" {
    #endif

    #if defined(SPALL_BUFFER_PROFILING) && !defined(SPALL_BUFFER_PROFILING_GET_TIME)
    #error "You must #define SPALL_BUFFER_PROFILING_GET_TIME() to profile buffer flushes."
    #endif

    #ifdef SPALL_BUFFER_PROFILING
    static void Spall__BufferProfile(SpallProfile *ctx, SpallBuffer *wb, double spall_time_begin, double spall_time_end, const char *name, int name_len) {
        // precon: ctx
        // precon: ctx->write
        char temp_buffer_data[2048];
        SpallBuffer temp_buffer = { temp_buffer_data, sizeof(temp_buffer_data) };
        if (!SpallTraceBeginLenTidPid(ctx, &temp_buffer, name, sizeof(name) - 1, (uint32_t)(uintptr_t)wb->data, 4222222222, spall_time_begin)) return;
        if (!SpallTraceEndTidPid(ctx, &temp_buffer, (uint32_t)(uintptr_t)wb->data, 4222222222, spall_time_end)) return;
        if (ctx->write) ctx->write(ctx, temp_buffer_data, temp_buffer.head);
    }
    #define SPALL_BUFFER_PROFILE_BEGIN() double spall_time_begin = (SPALL_BUFFER_PROFILING_GET_TIME())
    // Don't call this with anything other than a string literal
    #define SPALL_BUFFER_PROFILE_END(name) Spall__BufferProfile(ctx, wb, spall_time_begin, (SPALL_BUFFER_PROFILING_GET_TIME()), "" name "", sizeof("" name "") - 1)
    #else
    #define SPALL_BUFFER_PROFILE_BEGIN()
    #define SPALL_BUFFER_PROFILE_END(name)
    #endif

    extern char SpallSingleThreadedBufferData[];
    char SpallSingleThreadedBufferData[1 << 16];
    SpallBuffer SpallSingleThreadedBuffer = {SpallSingleThreadedBufferData, sizeof(SpallSingleThreadedBufferData)};

    static bool Spall__FileWrite(SpallProfile *ctx, const void *p, size_t n) {
        if (!ctx->file) return false;
        #ifdef SPALL_DEBUG
        if (feof(ctx->file)) return false;
        if (ferror(ctx->file)) return false;
        #endif

        if (fwrite(p, n, 1, ctx->file) != 1) return false;
        return true;
    }
    static bool Spall__FileFlush(SpallProfile *ctx) {
        if (!ctx->file) return false;
        if (fflush(ctx->file)) return false;
        return true;
    }
    static void Spall__FileClose(SpallProfile *ctx) {
        if (!ctx->file) return;

        #ifdef SPALL_JSON
        #ifdef SPALL_DEBUG
        if (!feof(ctx->file) && !ferror(ctx->file))
            #endif
        {
            fseek(ctx->file, -2, SEEK_CUR); // seek back to overwrite trailing comma
            fwrite("\n]}\n", sizeof("\n]}\n") - 1, 1, ctx->file);
        }
        #endif
        fflush(ctx->file);
        fclose(ctx->file);
        ctx->file = NULL;
    }

    static bool Spall__BufferFlush(SpallProfile *ctx, SpallBuffer *wb) {
        // precon: wb
        // precon: wb->data
        // precon: wb->head <= wb->length
        // precon: !ctx || ctx->write
        #ifdef SPALL_DEBUG
        if (wb->ctx != ctx) return false; // Buffer must be bound to this context (or to NULL)
        #endif

        if (wb->head && ctx) {
            SPALL_BUFFER_PROFILE_BEGIN();
            if (!ctx->write || !ctx->write(ctx, wb->data, wb->head)) return false;
            SPALL_BUFFER_PROFILE_END("Buffer Flush");
        }
        wb->head = 0;
        return true;
    }

    static bool Spall__BufferWrite(SpallProfile *ctx, SpallBuffer *wb, void *p, size_t n) {
        // precon: !wb || wb->head < wb->length
        // precon: !ctx || ctx->write
        if (!wb) return ctx->write && ctx->write(ctx, p, n);
        #ifdef SPALL_DEBUG
        if (wb->ctx != ctx) return false; // Buffer must be bound to this context (or to NULL)
        #endif
        if (wb->head + n > wb->length && !Spall__BufferFlush(ctx, wb)) return false;
        if (n > wb->length) {
            SPALL_BUFFER_PROFILE_BEGIN();
            if (!ctx->write || !ctx->write(ctx, p, n)) return false;
            SPALL_BUFFER_PROFILE_END("Unbuffered Write");
            return true;
        }
        memcpy((char *)wb->data + wb->head, p, n);
        wb->head += n;
        return true;
    }

    bool SpallBufferFlush(SpallProfile *ctx, SpallBuffer *wb) {
        #ifdef SPALL_DEBUG
        if (!wb) return false;
        if (!wb->data) return false;
        #endif

        if (!Spall__BufferFlush(ctx, wb)) return false;
        return true;
    }

    bool SpallBufferInit(SpallProfile *ctx, SpallBuffer *wb) {
        if (!SpallBufferFlush(NULL, wb)) return false;
        wb->ctx = ctx;
        return true;
    }
    bool SpallBufferQuit(SpallProfile *ctx, SpallBuffer *wb) {
        if (!SpallBufferFlush(ctx, wb)) return false;
        wb->ctx = NULL;
        return true;
    }

    bool SpallBufferAbort(SpallBuffer *wb) {
        if (!wb) return false;
        wb->ctx = NULL;
        if (!Spall__BufferFlush(NULL, wb)) return false;
        return true;
    }

    static SpallProfile Spall__Init(const char *filename, double timestamp_unit) {
        SpallProfile ctx;
        memset(&ctx, 0, sizeof(ctx));
        if (timestamp_unit < 0) return ctx;
        if (!filename) return ctx;
        ctx.file = fopen(filename, "wb"); // TODO: handle utf8 and long paths on windows
        ctx.write = Spall__FileWrite;
        ctx.flush = Spall__FileFlush;
        ctx.close = Spall__FileClose;
        if (!ctx.file) { SpallQuit(&ctx); return ctx; }
        ctx.timestamp_unit = timestamp_unit;

        #ifdef SPALL_JSON
        if (!ctx.write(&ctx, "{\"traceEvents\":[\n", sizeof("{\"traceEvents\":[\n") - 1)) { SpallQuit(&ctx); return ctx; }
        #else
        SpallHeader header;
        header.magic_header = 0x0BADF00D;
        header.version = 1;
        header.timestamp_unit = timestamp_unit;
        header.must_be_0 = 0;
        if (!ctx.write(&ctx, &header, sizeof(header))) { SpallQuit(&ctx); return ctx; }
        #endif

        return ctx;
    }

    SpallProfile SpallInit    (const char *filename, double timestamp_unit) { return Spall__Init(filename, timestamp_unit); }

    void SpallQuit(SpallProfile *ctx) {
        if (!ctx) return;
        if (ctx->close) ctx->close(ctx);

        memset(ctx, 0, sizeof(*ctx));
    }

    bool SpallFlush(SpallProfile *ctx) {
        #ifdef SPALL_DEBUG
        if (!ctx) return false;
        if (!ctx->file) return false;
        #endif

        if (!ctx->flush || !ctx->flush(ctx)) return false;
        return true;
    }

    bool SpallTraceBeginLenArgsTidPid(SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, const char *args, signed long args_len, uint32_t tid, uint32_t pid, double when) {
        SpallBeginEventMax ev;

        #ifdef SPALL_DEBUG
        if (!ctx) return false;
        if (!name) return false;
        if (!ctx->file) return false;
        if (name_len <= 0) return false;
        #endif
        name_len = SPALL_MIN(name_len, 255); // will be interpreted as truncated in the app (?)
        args_len = SPALL_MIN(args_len, 255); // will be interpreted as truncated in the app (?)

        ev.event.type = SpallEventType_Begin;
        ev.event.category = 0;
        ev.event.pid = pid;
        ev.event.tid = tid;
        ev.event.when = when;
        ev.event.name_length = (uint8_t)name_len;
        ev.event.args_length = (uint8_t)args_len;
        char *args_bytes = ev.name_bytes + name_len;
        memcpy(ev.name_bytes, name, (uint8_t)name_len);
        memcpy(args_bytes, args, (uint8_t)args_len);

        #ifdef SPALL_JSON
        char buf[1024];
        int buf_len = snprintf(buf, sizeof(buf),
            "{\"args\":\"%.*s\",\"name\":\"%.*s\",\"ph\":\"B\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
            (int)ev.event.args_length, args_bytes,
            (int)ev.event.name_length, ev.name_bytes,
            ev.event.pid,
            ev.event.tid,
            ev.event.when * ctx->timestamp_unit);
        if (buf_len <= 0) return false;
        if (buf_len >= sizeof(buf)) return false;
        if (!Spall__BufferWrite(ctx, wb, buf, buf_len)) return false;
        #else
        if (!Spall__BufferWrite(ctx, wb, &ev, sizeof(SpallBeginEvent) + (uint8_t)name_len + (uint8_t)args_len)) return false;
        #endif

        return true;
    }

    bool SpallTraceBeginLenTidPid(SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, uint32_t tid, uint32_t pid, double when) { return SpallTraceBeginLenArgsTidPid(ctx, wb, name, name_len, "", 0, tid, 0, when); }
    bool SpallTraceBeginLenTid(SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, uint32_t tid, double when) { return SpallTraceBeginLenArgsTidPid(ctx, wb, name, name_len, "", 0, tid, 0, when); }
    bool SpallTraceBeginLen   (SpallProfile *ctx, SpallBuffer *wb, const char *name, signed long name_len, double when)               { return SpallTraceBeginLenArgsTidPid(ctx, wb, name, name_len, "", 0,  0, 0, when); }

    bool SpallTraceEndTidPid(SpallProfile *ctx, SpallBuffer *wb, uint32_t tid, uint32_t pid, double when) {
        SpallEndEvent ev;

        #ifdef SPALL_DEBUG
        if (!ctx) return false;
        if (!ctx->file) return false;
        #endif

        ev.type = SpallEventType_End;
        ev.pid = pid;
        ev.tid = tid;
        ev.when = when;

        #ifdef SPALL_JSON
        char buf[512];
        int buf_len = snprintf(buf, sizeof(buf),
            "{\"ph\":\"E\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
            ev.pid,
            ev.tid,
            ev.when * ctx->timestamp_unit);
        if (buf_len <= 0) return false;
        if (buf_len >= sizeof(buf)) return false;
        if (!Spall__BufferWrite(ctx, wb, buf, buf_len)) return false;
        #else
        if (!Spall__BufferWrite(ctx, wb, &ev, sizeof(ev))) return false;
        #endif

        return true;
    }

    bool SpallTraceEndTid(SpallProfile *ctx, SpallBuffer *wb, uint32_t tid, double when) { return SpallTraceEndTidPid(ctx, wb, tid, 0, when); }
    bool SpallTraceEnd   (SpallProfile *ctx, SpallBuffer *wb, double when)               { return SpallTraceEndTidPid(ctx, wb,   0, 0, when); }

    #ifdef __cplusplus
}
#endif

#endif // SPALL_IMPLEMENTED
#endif // SPALL_IMPLEMENTATION

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
