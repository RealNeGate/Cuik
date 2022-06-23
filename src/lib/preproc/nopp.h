#pragma once
#include "lexer.h"
#include <big_array.h>
#include <common.h>

// literally just converts the lexer input into a raw token stream
TokenStream nopp_invoke(Lexer* restrict l);
