#pragma once
#include "common.h"
#include "arena.h"
#include "parser.h"
#include "tb/tb.h"
#include "stb_ds.h"

extern TB_Module* mod;

// stage 1 is forward declarations
void gen_ir_stage1(TopLevel tl);

// stage 2 is definitions
void gen_ir_stage2(TopLevel tl);

