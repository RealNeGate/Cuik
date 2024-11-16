// All Cuik includes use this file
#pragma once
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pool.h>

#ifdef CUIK_DLL
#  ifdef CUIK_IMPORT_DLL
#    define CUIK_API __declspec(dllimport)
#  else
#    define CUIK_API __declspec(dllexport)
#  endif
#else
#  define CUIK_API
#endif

