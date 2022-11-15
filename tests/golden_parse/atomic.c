typedef _Atomic(int) atomic_int;
typedef _Atomic int atomic_int;
typedef _Atomic _Atomic _Atomic(int) atomic_int;

typedef const int const_int;

typedef const atomic_int const_atomic_int;
typedef _Atomic const int const_atomic_int;
typedef const _Atomic int const_atomic_int;
typedef const _Atomic(int) const_atomic_int;
typedef _Atomic const_int const_atomic_int;

typedef int *_Atomic atomic_int_ptr;
typedef _Atomic(int *) atomic_int_ptr;

typedef int _Atomic *int_atomic_ptr;
typedef _Atomic(int) *int_atomic_ptr;

typedef int int_fn();
typedef _Atomic int atomic_int_array[3];

_Atomic struct S { int n; };

struct S
_Atomic atomic_s_no_missing_semicolon;

int *const _Atomic atomic_return_type();
