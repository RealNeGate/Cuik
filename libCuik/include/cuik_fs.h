#pragma once
#include "cuik_prelude.h"

typedef struct Cuik_File Cuik_File;

// if case_insensitive is set on a case sensitive and there's multiple files conflicting
// the behavior is unspecified (it will pick *a* file)
//
// returns NULL if it failed to find the file
Cuik_File* cuikfs_open(const char* path, bool write);
void cuikfs_close(Cuik_File* file);
bool cuikfs_get_length(Cuik_File* file, size_t* out_length);
bool cuikfs_read(Cuik_File* file, void* data, size_t count);

bool cuikfs_canonicalize(Cuik_Path* out, const char* path, bool case_insensitive);