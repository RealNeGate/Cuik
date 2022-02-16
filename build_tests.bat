call vcvars64

set clang_settings=-march=haswell -maes -O2 -DNDEBUG -Werror -Wall -Wno-gnu-designator -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

mkdir build

clang %clang_settings% src/front/lexer_test.c src/front/lexer.c src/arena.c -Isrc -o build/lexer_test.exe -lole32 -lAdvapi32 -lOleAut32

rem build\lexer_test test tests\the_pile\sqlite3.c tests\the_pile\sqlite3_gold.txt
