call vcvars64

set clang_settings=-march=haswell -maes -O2 -DNDEBUG -Werror -Wall -Wno-gnu-designator -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set cuik_source_files=src/main.c ^
	src/preproc.c ^
	src/lexer.c ^
	src/tls.c ^
	src/types.c ^
	src/arena.c ^
	src/atoms.c ^
	src/parser.c ^
	src/ir_gen.c ^
	src/linker.c ^
	src/microsoft_craziness.cpp ^
	ext/threads_msvc.c

mkdir build
clang %clang_settings% %cuik_source_files% tinybackend.lib -o build/cuik.exe -lole32 -lAdvapi32 -lOleAut32
