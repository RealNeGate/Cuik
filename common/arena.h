#pragma once
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef TB_API
#  ifdef __cplusplus
#    define TB_EXTERN extern "C"
#  else
#    define TB_EXTERN
#  endif
#  ifdef TB_DLL
#    ifdef TB_IMPORT_DLL
#      define TB_API TB_EXTERN __declspec(dllimport)
#    else
#      define TB_API TB_EXTERN __declspec(dllexport)
#    endif
#  else
#    define TB_API TB_EXTERN
#  endif
#endif

enum {
    // usually chunks are small unless it asks for a lot of memory (big arrays usually)
    TB_ARENA_NORMAL_CHUNK_SIZE = 32 * 1024,
    // this is the backing data for chunks, if we've got 32KiB chunks in 2MiB blocks that's 64 chunks per block.
    TB_ARENA_BLOCK_SIZE = 2 * 1024 * 1024,
    // just a decent alignment amount, in practice 8 would prolly be fine
    TB_ARENA_ALIGNMENT  = 16,
};

typedef struct TB_ArenaFreeList TB_ArenaFreeList;
struct TB_ArenaFreeList {
    TB_ArenaFreeList* next;
    size_t size;
    char data[];
};

typedef struct TB_ArenaChunk TB_ArenaChunk;
struct TB_ArenaChunk {
    TB_ArenaChunk* prev;
    char* avail;
    char* limit;
    char* pad;

    char data[];
};

#ifndef TB_OPAQUE_ARENA_DEF
#define TB_OPAQUE_ARENA_DEF
typedef struct TB_ArenaChunk TB_ArenaChunk;
typedef struct {
    TB_ArenaChunk* top;

    #ifndef NDEBUG
    const char* tag;
    uint32_t allocs;
    uint32_t alloc_bytes;
    #endif
} TB_Arena;

typedef struct TB_ArenaSavepoint {
    TB_ArenaChunk* top;
    char* avail;
} TB_ArenaSavepoint;
#endif // TB_OPAQUE_ARENA_DEF

#define TB_ARENA_ALLOC(arena, T) tb_arena_alloc(arena, sizeof(T))
#define TB_ARENA_ARR_ALLOC(arena, count, T) tb_arena_alloc(arena, (count) * sizeof(T))

TB_API void tb_arena_create(TB_Arena* restrict arena, const char* optional_tag);
TB_API void tb_arena_destroy(TB_Arena* restrict arena);
TB_API void tb_arena_clear(TB_Arena* restrict arena);

TB_API void* tb_arena_unaligned_alloc(TB_Arena* restrict arena, size_t size);
TB_API void* tb_arena_alloc(TB_Arena* restrict arena, size_t size);

TB_API void* tb_arena_realloc(TB_Arena* restrict arena, void* old, size_t old_size, size_t size);

// return false on failure
TB_API bool tb_arena_free(TB_Arena* restrict arena, void* ptr, size_t size);
TB_API void tb_arena_pop(TB_Arena* restrict arena, void* ptr, size_t size);

// in case you wanna mix unaligned and aligned arenas
TB_API void tb_arena_realign(TB_Arena* restrict arena);

TB_API size_t tb_arena_chunk_size(TB_ArenaChunk* arena);
TB_API size_t tb_arena_current_size(TB_Arena* arena);

// savepoints
TB_API TB_ArenaSavepoint tb_arena_base(TB_Arena* arena);
TB_API TB_ArenaSavepoint tb_arena_save(TB_Arena* arena);
TB_API void tb_arena_restore(TB_Arena* arena, TB_ArenaSavepoint sp);
TB_API bool tb_arena_is_top(TB_Arena* arena, TB_ArenaSavepoint sp);
