call vcvars64

set clang_settings=-march=haswell -maes -O0 -Werror -Wall -Wno-gnu-designator -Wno-unused-function -g -gcodeview -D_CRT_SECURE_NO_WARNINGS

set cuik_source_files=src/main.c ^
	src/tls.c ^
	src/arena.c ^
	src/targets/x64.c ^
	src/front/preproc.c ^
	src/front/lexer.c ^
	src/front/types.c ^
	src/front/atoms.c ^
	src/front/parser.c ^
	src/front/const_eval.c ^
	src/mid/sema.c ^
	src/back/ir_gen.c ^
	src/back/linker.c ^
	src/back/microsoft_craziness.cpp ^
	src/ext/stb_ds.c ^
	src/ext/threads_msvc.c

mkdir build
clang %clang_settings% %cuik_source_files% tinybackend.lib -Isrc -o build/cuik.exe -lole32 -lAdvapi32 -lOleAut32
