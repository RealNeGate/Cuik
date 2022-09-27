// Simple coroutine design
#pragma once

#define CO_DONE()          return 0
#define CO_YIELD(co, code) do { co->state = __LINE__; return code; case __LINE__:; } while (0)
#define CO_SCOPE(co)       switch (co->state)
#define CO_START()         case 0:
