#pragma once
#include "cuik_prelude.h"

typedef struct {
    // file system stuff
    Cuikpp_LocateFile locate;
    Cuikpp_GetFile fs;
    void* user_data;
    bool case_insensitive;

    // parser
} CuikGo_Parser;

void cuikgo_parse_file(CuikGo_Parser* ctx, Cuik_Path* filepath);
