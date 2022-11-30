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
//   l   long
//   L   long long
//   !   single arbitrary parameter
//   .   variadic parameters
//
// gcc/clang
X(__builtin_expect, "vb")
X(__builtin_trap, "vv")
X(__builtin_clz, "ii")
X(__builtin_mul_overflow, "v.")

X(__builtin_unreachable, "v")

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
X(_byteswap_ulong, "ii")

X(__assume, "vb")
X(__debugbreak, "vv")
X(__va_start, "vc**.")
X(_umul128, "LLLL*")
X(_mul128, "LLLL*")
X(_InterlockedExchange, "ll*l")
X(_InterlockedCompareExchange, "ll*ll")
#undef X
