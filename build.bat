call vcvars64

set clang_settings=-march=haswell -O0 -Werror -Wall -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set cuik_source_files=src/main.c ^
	src/lexer.c ^
	src/tls.c ^
	src/types.c ^
	src/parser.c ^
	src/ir_gen.c ^
	src/memory_win32.c

mkdir build
clang %clang_settings% %cuik_source_files% tinybackend.lib -o build/cuik.exe
