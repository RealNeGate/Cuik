// SPDX-FileCopyrightText: © 2022 Phillip Trudeau-Tavara <pmttavara@protonmail.com>
// SPDX-License-Identifier: 0BSD

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#ifndef __cplusplus
#include <threads.h>
#endif

#pragma pack(push, 1)
typedef struct FlintHeader {
    uint64_t magic_header; // = 0x0BADF00D
    uint64_t version; // = 0 to start
    double timestamp_unit;
    uint8_t name_cache_power; // must be between 0 and 16 inclusive, defaults to 10
} FlintHeader;

typedef struct FlintTime { double floating; } FlintTime;
typedef struct FlintString {
    uint8_t length;
    char bytes[1];
} FlintString;

typedef enum FlintEventType {
    FlintType_Begin,
    FlintType_End,
    FlintType_BeginCacheHit,
} FlintEventType;

typedef struct FlintBeginEvent {
    uint8_t type; // = FlintType_Begin
    uint32_t pid;
    uint32_t tid;
    FlintTime when;
    FlintString name;
} FlintBeginEvent;

typedef struct FlintBeginCacheHitEvent {
    uint8_t type; // = FlintType_BeginCacheHit
    uint32_t pid;
    uint32_t tid;
    FlintTime when;
    uint16_t name_slot;
} FlintBeginCacheHitEvent;

typedef struct FlintEndEvent {
    uint8_t type; // = FlintType_End
    uint32_t pid;
    uint32_t tid;
    FlintTime when;
} FlintEndEvent;

typedef struct FlintBeginEventMax {
    FlintBeginEvent event;
    char name_bytes[254];
} FlintBeginEventMax;

#pragma pack(pop)

typedef struct FlintContext {
    FILE *file;
    double timestamp_unit;
    bool is_json;
} FlintContext;

typedef struct NameCacheEntry {
    uint32_t hash;
    uint32_t pid;
    uint32_t tid;
    uint8_t name_len;
    char name[255];
} NameCacheEntry;

enum { NAME_CACHE_POWER = 10 };
thread_local NameCacheEntry name_cache[1 << NAME_CACHE_POWER];

inline bool FlintFlush(FlintContext *ctx) {
    if (!ctx) return false;
    if (!ctx->file) return false;
    if (fflush(ctx->file)) return false;
    return true;
}

inline void FlintQuit(FlintContext *ctx) {
    if (!ctx) return;
    if (ctx->file) {
        if (ctx->is_json) {
            fseek(ctx->file, -2, SEEK_CUR); // seek back to overwrite trailing comma
            fprintf(ctx->file, "\n]}\n");
        }
        fclose(ctx->file);
    }
    memset(ctx, 0, sizeof(*ctx));
}

inline FlintContext FlintInit_Impl(const char *filename, double timestamp_unit, bool is_json) {
    FlintContext ctx;
    memset(&ctx, 0, sizeof(ctx));
    if (!filename) return ctx;
    ctx.file = fopen(filename, "wb"); // TODO: handle utf8 on windows
    ctx.timestamp_unit = timestamp_unit;
    ctx.is_json = is_json;
    if (!ctx.file) {
        FlintQuit(&ctx);
        return ctx;
    }
    if (ctx.is_json) {
        if (fprintf(ctx.file, "{\"traceEvents\":[\n") <= 0) {
            FlintQuit(&ctx);
            return ctx;
        }
        if (fflush(ctx.file)) {
            FlintQuit(&ctx);
            return ctx;
        }
    } else {
        FlintHeader header;
        header.magic_header = 0x0BADF00D;
        header.version = 0;
        header.timestamp_unit = timestamp_unit;
        header.name_cache_power = NAME_CACHE_POWER;
        if (fwrite(&header, sizeof(header), 1, ctx.file) != 1) {
            FlintQuit(&ctx);
            return ctx;
        }
    }
    memset(name_cache, 0, sizeof(name_cache));
    return ctx;
}

inline FlintContext FlintInitJson(const char *filename, double timestamp_unit) { return FlintInit_Impl(filename, timestamp_unit,  true); }
inline FlintContext FlintInit    (const char *filename, double timestamp_unit) { return FlintInit_Impl(filename, timestamp_unit, false); }

static inline uint32_t murmur32(const char *key, uint32_t len, uint32_t seed) {
    uint32_t c1 = 0xcc9e2d51;
    uint32_t c2 = 0x1b873593;
    uint32_t r1 = 15;
    uint32_t r2 = 13;
    uint32_t m = 5;
    uint32_t n = 0xe6546b64;
    uint32_t h = 0;
    uint32_t k = 0;
    uint8_t *d = (uint8_t *) key; // 32 bit extract from `key'
    const uint32_t *chunks = NULL;
    const uint8_t *tail = NULL; // tail - last 8 bytes
    int i = 0;
    int l = len / 4; // chunk length

    h = seed;

    chunks = (const uint32_t *) (d + l * 4); // body
    tail = (const uint8_t *) (d + l * 4); // last 8 byte chunk of `key'

    // for each 4 byte chunk of `key'
    for (i = -l; i != 0; ++i) {
        // next 4 byte chunk of `key'
        k = chunks[i];

        // encode next 4 byte chunk of `key'
        k *= c1;
        k = (k << r1) | (k >> (32 - r1));
        k *= c2;

        // append to hash
        h ^= k;
        h = (h << r2) | (h >> (32 - r2));
        h = h * m + n;
    }

    k = 0;

    // remainder
    switch (len & 3) { // `len % 4'
        case 3: k ^= (tail[2] << 16);
        case 2: k ^= (tail[1] << 8);

        case 1:
        k ^= tail[0];
        k *= c1;
        k = (k << r1) | (k >> (32 - r1));
        k *= c2;
        h ^= k;
    }

    h ^= len;

    h ^= (h >> 16);
    h *= 0x85ebca6b;
    h ^= (h >> 13);
    h *= 0xc2b2ae35;
    h ^= (h >> 16);

    return h;
}

static inline uint32_t hash_entry(const char *name, uint8_t name_len, uint32_t tid, uint32_t pid) {
    uint32_t result = murmur32(name, name_len, 2166136261);
    result = murmur32((char *)&pid, 4, result);
    result = murmur32((char *)&tid, 4, result);
    result = murmur32((char *)&name_len, 1, result);
    return result;
}

static bool use_cache = false; // @Hack

int string_payload_length = 0;

// Caller has to take a lock if multiple threads submit a specific TID-PID combination.
inline bool FlintTraceBeginLenTidPid(FlintContext *ctx, double when, const char *name, signed long name_len, uint32_t tid, uint32_t pid) {
    if (!ctx) return false;
    if (!name) return false;
    if (!ctx->file) return false;
    if (feof(ctx->file)) return false;
    if (ferror(ctx->file)) return false;
    // if (ctx->times_are_u64) return false;
    if (name_len <= 0) return false;
    if (name_len > 255) name_len = 255; // will be interpreted as truncated in the app (?)

    if (ctx->is_json) {
        if (fprintf(ctx->file,
                "{\"name\":\"%.*s\",\"ph\":\"B\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
                (int)name_len, name,
                pid,
                tid,
                when * ctx->timestamp_unit)
            <= 0) return false;
    } else {
        if (0) {
            uint32_t hash = hash_entry(name, name_len, tid, pid);
            int slot = hash & ((1 << NAME_CACHE_POWER) - 1);
            bool hit = false;
            NameCacheEntry nce = name_cache[slot];
            if (nce.hash == hash && nce.tid == tid && nce.pid == pid && nce.name_len == name_len && !memcmp(nce.name, name, name_len)) {
                hit = true;
            }
            if (hit) {
                // Write cache hit position
                FlintBeginCacheHitEvent ev;
                ev.type = FlintType_BeginCacheHit;
                ev.pid = pid;
                ev.tid = tid;
                ev.when.floating = when;
                ev.name_slot = slot;
                if (fwrite(&ev, sizeof(ev), 1, ctx->file) != 1) return false;
            } else {
                // Write new literal
                FlintBeginEventMax ev;
                ev.event.type = FlintType_Begin;
                ev.event.pid = pid;
                ev.event.tid = tid;
                ev.event.when.floating = when;
                ev.event.name.length = (uint8_t)name_len;
                memcpy(ev.event.name.bytes, name, name_len);
                if (fwrite(&ev, sizeof(FlintBeginEvent) + name_len - 1, 1, ctx->file) != 1) return false;
                string_payload_length += name_len - 1;

                // Overwrite hash entry if longer
                NameCacheEntry entry = {hash, pid, tid, (uint8_t)name_len};
                memcpy(entry.name, name, name_len);
                name_cache[slot] = entry;
            }

        } else {
            FlintBeginEventMax ev;
            ev.event.type = FlintType_Begin;
            ev.event.pid = pid;
            ev.event.tid = tid;
            ev.event.when.floating = when;
            ev.event.name.length = (uint8_t)name_len;
            memcpy(ev.event.name.bytes, name, name_len);
            if (fwrite(&ev, sizeof(FlintBeginEvent) + name_len - 1, 1, ctx->file) != 1) return false;
            string_payload_length += name_len - 1;
        }

    }
    return true;
}
inline bool FlintTraceBeginTidPid(FlintContext *ctx, double when, const char *name, uint32_t tid, uint32_t pid) {
    unsigned long name_len;
    if (!name) return false;
    name_len = strlen(name);
    if (!name_len) return false;
    return FlintTraceBeginLenTidPid(ctx, when, name, (signed long)name_len, tid, pid);
}
inline bool FlintTraceBeginLenTid(FlintContext *ctx, double when, const char *name, signed long name_len, uint32_t tid) { return FlintTraceBeginLenTidPid(ctx, when, name, name_len, tid, 0); }
inline bool FlintTraceBeginLen   (FlintContext *ctx, double when, const char *name, signed long name_len)               { return FlintTraceBeginLenTidPid(ctx, when, name, name_len,   0, 0); }
inline bool FlintTraceBeginTid   (FlintContext *ctx, double when, const char *name, uint32_t tid)                       { return FlintTraceBeginTidPid   (ctx, when, name,           tid, 0); }
inline bool FlintTraceBegin      (FlintContext *ctx, double when, const char *name)                                     { return FlintTraceBeginTidPid   (ctx, when, name,             0, 0); }

inline bool FlintTraceEndTidPid(FlintContext *ctx, double when, uint32_t tid, uint32_t pid) {
    FlintEndEvent ev;
    if (!ctx) return false;
    if (!ctx->file) return false;
    if (feof(ctx->file)) return false;
    if (ferror(ctx->file)) return false;
    // if (ctx->times_are_u64) return false;
    ev.type = FlintType_End;
    ev.pid = pid;
    ev.tid = tid;
    ev.when.floating = when;
    if (ctx->is_json) {
        if (fprintf(ctx->file,
                "{\"ph\":\"E\",\"pid\":%u,\"tid\":%u,\"ts\":%f},\n",
                ev.pid,
                ev.tid,
                ev.when.floating * ctx->timestamp_unit)
            <= 0) return false;
    } else {
        if (fwrite(&ev, sizeof(ev), 1, ctx->file) != 1) return false;
    }
    return true;
}
inline bool FlintTraceEndTid(FlintContext *ctx, double when, uint32_t tid) { return FlintTraceEndTidPid(ctx, when, tid, 0); }
inline bool FlintTraceEnd   (FlintContext *ctx, double when)               { return FlintTraceEndTidPid(ctx, when,   0, 0); }

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
