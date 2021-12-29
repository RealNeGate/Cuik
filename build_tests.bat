call vcvars64

set clang_settings=-march=haswell -maes -O0 -Werror -Wall -Wno-gnu-designator -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

mkdir build

clang %clang_settings% src/lexer_test.c src/lexer.c -o build/lexer_test.exe -lole32 -lAdvapi32 -lOleAut32

.\build\lexer_test test tests\the_pile\sqlite3.c tests\the_pile\sqlite3_gold.txt
