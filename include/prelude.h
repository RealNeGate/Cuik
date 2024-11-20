#pragma once

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

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

// just represents some region of bytes, usually in file parsing crap
typedef struct {
    const uint8_t* data;
    size_t length;
} TB_Slice;

typedef enum TB_WindowsSubsystem {
    TB_WIN_SUBSYSTEM_UNKNOWN,
    TB_WIN_SUBSYSTEM_WINDOWS,
    TB_WIN_SUBSYSTEM_CONSOLE,
    TB_WIN_SUBSYSTEM_EFI_APP,
} TB_WindowsSubsystem;

typedef enum TB_OutputFlavor {
    TB_FLAVOR_OBJECT,     // .o  .obj
    TB_FLAVOR_SHARED,     // .so .dll
    TB_FLAVOR_STATIC,     // .a  .lib
    TB_FLAVOR_EXECUTABLE, //     .exe
} TB_OutputFlavor;

typedef enum {
    TB_EXECUTABLE_UNKNOWN,
    TB_EXECUTABLE_PE,
    TB_EXECUTABLE_ELF,
} TB_ExecutableType;

typedef enum TB_Arch {
    TB_ARCH_UNKNOWN,

    TB_ARCH_X86_64,
    TB_ARCH_AARCH64,

    // they're almost identical so might as well do both.
    TB_ARCH_MIPS32,
    TB_ARCH_MIPS64,

    TB_ARCH_WASM32,

    TB_ARCH_MAX,
} TB_Arch;

typedef enum TB_System {
    TB_SYSTEM_WINDOWS,
    TB_SYSTEM_LINUX,
    TB_SYSTEM_MACOS,
    TB_SYSTEM_ANDROID, // Not supported yet
    TB_SYSTEM_WASM,

    TB_SYSTEM_MAX,
} TB_System;

typedef enum TB_ABI {
    // Used on 64bit Windows platforms
    TB_ABI_WIN64,

    // Used on Mac, BSD and Linux platforms
    TB_ABI_SYSTEMV,
} TB_ABI;

typedef struct TB_Emitter {
    size_t capacity, count;
    uint8_t* data;
} TB_Emitter;

void* tb_out_reserve(TB_Emitter* o, size_t count);
void tb_out_commit(TB_Emitter* o, size_t count);

// reserves & commits
void* tb_out_grab(TB_Emitter* o, size_t count);
size_t tb_out_grab_i(TB_Emitter* o, size_t count);
size_t tb_out_get_pos(TB_Emitter* o, void* p);

// Adds null terminator onto the end and returns the starting position of the string
size_t tb_outstr_nul_UNSAFE(TB_Emitter* o, const char* str);

void tb_out1b_UNSAFE(TB_Emitter* o, uint8_t i);
void tb_out4b_UNSAFE(TB_Emitter* o, uint32_t i);
void tb_outstr_UNSAFE(TB_Emitter* o, const char* str);
void tb_outs_UNSAFE(TB_Emitter* o, size_t len, const void* str);
size_t tb_outs(TB_Emitter* o, size_t len, const void* str);
void* tb_out_get(TB_Emitter* o, size_t pos);

// fills region with zeros
void tb_out_zero(TB_Emitter* o, size_t len);

void tb_out1b(TB_Emitter* o, uint8_t i);
void tb_out2b(TB_Emitter* o, uint16_t i);
void tb_out4b(TB_Emitter* o, uint32_t i);
void tb_out8b(TB_Emitter* o, uint64_t i);
void tb_patch1b(TB_Emitter* o, uint32_t pos, uint8_t i);
void tb_patch2b(TB_Emitter* o, uint32_t pos, uint16_t i);
void tb_patch4b(TB_Emitter* o, uint32_t pos, uint32_t i);
void tb_patch8b(TB_Emitter* o, uint32_t pos, uint64_t i);

uint8_t  tb_get1b(TB_Emitter* o, uint32_t pos);
uint16_t tb_get2b(TB_Emitter* o, uint32_t pos);
uint32_t tb_get4b(TB_Emitter* o, uint32_t pos);

