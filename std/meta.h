// DO NOT INCLUDE UNLESS YOU'RE THE CUIK METAPROGRAM 
#pragma once
#include <stddef.h>

typedef int ExprIndex;
typedef struct TUnit TUnit;

typedef ExprIndex CuikSugar(TUnit* t, ExprIndex e, size_t arg_count, ExprIndex* args);
typedef void CuikMetaEntrypoint(TUnit* t);

void __meta_bind_sugar(const char* name, CuikSugar* sugar);

#ifndef CUIK_SHORT_NAMES
#define bind_sugar(name, sugar) __meta_bind_sugar
#endif /* CUIK_SHORT_NAMES */
