#pragma once
#include "common.h"

void* arena_alloc(size_t size, size_t align);
void arena_free();
