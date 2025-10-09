#pragma once
#include <stdint.h>
#include <stdatomic.h>

typedef struct {
    uint64_t gprs[16];
    uint8_t fxsave[512];
    uint64_t cookie;
} CPUState;

typedef struct Sched_G {
    _Atomic(uint64_t) pause;
    uint64_t pad;

    CPUState state;
    TB_Stacklet* next;
} Sched_G;

TB_Stacklet* sched_g_first(void);
Sched_G* sched_g_get(TB_Stacklet* stacklet);
