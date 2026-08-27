clang cs3.c -g -o a.out -ftime-trace -ftime-trace-granularity=0 -I /usr/include/csmith/ -Wno-constant-conversion -Wno-unused -Wno-tautological-constant-out-of-range-compare
cuik cs3.c -o b.out -I /usr/include/csmith/

./a.out 1 > foo.txt
./b.out 1 > bar.txt

diff foo.txt bar.txt
