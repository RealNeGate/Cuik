#pragma once
#include "common.h"

void* allocate_virtual_memory(size_t sz);
void free_virtual_memory(void* ptr);
