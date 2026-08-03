clang cs1.c -g -o a.out -I /home/linuxbrew/.linuxbrew/include/csmith-2.3.0/ -Wno-constant-conversion -Wno-unused -Wno-tautological-constant-out-of-range-compare
cuik cs1.c -o b.out -I /home/linuxbrew/.linuxbrew/include/csmith-2.3.0/

./a.out 1 > foo.txt
./b.out 1 > bar.txt


