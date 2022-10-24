// This setup is based on the Clang builtin table.
// we describe the function prototype with the types, return type first then
// the parameters all next to each other, here's the symbols:
//
//   *   follows some type, represents a pointer
//   v   void
//   b   bool
//   c   char
//   s   short
//   i   int
//   l   long (64bit)
//   !   single arbitrary parameter
//   .   variadic parameters
//
// gcc/clang
X(__builtin_expect, "vb")
X(__builtin_trap, "vv")

// cuik internal
X(__c11_atomic_compare_exchange_strong, "v.")
X(__c11_atomic_thread_fence, "vi")
X(__c11_atomic_signal_fence, "vi")
X(__c11_atomic_is_lock_free, "iz")
X(__c11_atomic_load, "v.")
X(__c11_atomic_store, "v.")
X(__c11_atomic_exchange, "v.")
X(__c11_atomic_compare_exchange_strong, "v.")
X(__c11_atomic_compare_exchange_weak, "v.")
X(__c11_atomic_fetch_add, "v.")
X(__c11_atomic_fetch_sub, "v.")
X(__c11_atomic_fetch_or, "v.")
X(__c11_atomic_fetch_xor, "v.")
X(__c11_atomic_fetch_and, "v.")

// microsoft extensions
X(__assume, "vb")
X(__debugbreak, "vv")
X(__va_start, "vc*.")
X(_umul128, "llll*")
X(_mul128, "llll*")
X(_InterlockedExchange, "ii*i")
X(_InterlockedCompareExchange, "ii*ii")
#undef X
