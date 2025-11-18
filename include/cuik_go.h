#pragma once
#include "cuik_prelude.h"

typedef struct CuikGo_WordPage CuikGo_WordPage;

typedef struct {
    // file system stuff
    Cuikpp_LocateFile locate;
    Cuikpp_GetFile fs;
    void* user_data;
    bool case_insensitive;

    // parser
    size_t used;
    CuikGo_WordPage* base;
    CuikGo_WordPage* curr;
} CuikGo_Parser;

void cuikgo_parse_file(CuikGo_Parser* ctx, Cuik_Path* filepath);
