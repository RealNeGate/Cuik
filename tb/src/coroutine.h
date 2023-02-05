// Simple coroutine design
#pragma once

#ifndef CONCAT
#define CONCAT_(x, y) x ## y
#define CONCAT(x, y) CONCAT_(x, y)
#endif

#define CO_DONE()          return 0
#define CO_YIELD(co, code) do { co->state = __LINE__; return code; case __LINE__:; } while (0)
#define CO_SCOPE(co)       switch (co->state)
#define CO_START()         case 0:

// the iterations need to yield so we need to place the state
// into the coroutine
#define CO_FOREACH_N(co, it, start_, count_)                           \
for (                                                                  \
    ptrdiff_t it = (co->it = 0, co->CONCAT(it, _limit) = (count_), 0); \
    it = (co->it), it < co->CONCAT(it, _limit); co->it++               \
)
