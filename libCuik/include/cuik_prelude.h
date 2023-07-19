// All Cuik includes use this file
#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#ifdef CUIK_DLL
#  ifdef CUIK_IMPORT_DLL
#    define CUIK_API __declspec(dllimport)
#  else
#    define CUIK_API __declspec(dllexport)
#  endif
#else
#  define CUIK_API
#endif

// hacky
#if !defined(CUIK_USE_TB) && !defined(TB_CORE_H)
typedef enum TB_WindowsSubsystem {
    TB_WIN_SUBSYSTEM_UNKNOWN,

    TB_WIN_SUBSYSTEM_WINDOWS,
    TB_WIN_SUBSYSTEM_CONSOLE,
    TB_WIN_SUBSYSTEM_EFI_APP,
} TB_WindowsSubsystem;
#endif

#ifndef ARENA_H
#define ARENA_H

typedef struct ArenaSegment ArenaSegment;
typedef struct {
    struct ArenaSegment* base;
    struct ArenaSegment* top;
} Arena;

#endif
