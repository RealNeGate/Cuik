#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "tb_formats.h"

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

////////////////////////////////
// Linker exporter
////////////////////////////////
// This is used to export shared objects or executables
typedef struct TB_Linker TB_Linker;
typedef struct TB_LinkerSection TB_LinkerSection;
typedef struct TB_LinkerSectionPiece TB_LinkerSectionPiece;

// tp can be NULL, it just means we're single-threaded
TB_API TB_Linker* tb_linker_create(TB_ExecutableType type, TB_Arch arch, TPool* tp);
TB_API bool tb_linker_export(TB_Linker* l, const char* file_name);

// wait for any pending jobs to finish
TB_API void tb_linker_barrier(TB_Linker* l);

// windows only
TB_API void tb_linker_set_subsystem(TB_Linker* l, TB_WindowsSubsystem subsystem);

TB_API void tb_linker_set_entrypoint(TB_Linker* l, const char* name);

TB_API void tb_linker_add_libpath(TB_Linker* l, const char* path);

// Links compiled module into output
TB_API void tb_linker_append_module(TB_Linker* l, TB_Module* m);

// Adds object file to output
TB_API void tb_linker_append_object(TB_Linker* l, const char* file_name);

// Adds static library to output
//   this can include imports (wrappers for DLL symbols) along with
//   normal sections.
TB_API void tb_linker_append_library(TB_Linker* l, const char* file_name);
