#pragma once

#include <stdint.h>

#if defined(_WIN32)
  #include <windows.h>
  static inline uint32_t thread_id32(void) {
    return (uint32_t)GetCurrentThreadId();
  }
#else
  #include <pthread.h>
  #include <stdint.h>

  static inline uint32_t thread_id32(void) {
    uintptr_t p = (uintptr_t)pthread_self();
    // Mix high/low bits; good enough for IDs/logging.
    return (uint32_t)(p ^ (p >> 32));
  }
#endif
