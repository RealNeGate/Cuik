#pragma once
#include "common.h"
#include "lexer.h"

extern char compiler_directory[260];

TokenStream preprocess_translation_unit(const char* filepath);
