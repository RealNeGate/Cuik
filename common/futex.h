#ifndef FUTEX_H
#define FUTEX_H

#ifdef __APPLE__
typedef _Atomic int32_t Futex;
#else
typedef _Atomic int64_t Futex;
#endif

void futex_dec(Futex* f);
void futex_signal(Futex* f);
void futex_broadcast(Futex* f);
void futex_wait(Futex* f, Futex val); // leaves if *f != val
void futex_wait_eq(Futex* f, Futex val); // leaves if *f == val

#endif /* FUTEX_H */
