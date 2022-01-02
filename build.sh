c_settings="-march=haswell -maes -std=gnu11 -DCUIK_NEEDS_SAFE_FUNCTIONS -O2 -DNDEBUG -Werror -Wall -Wno-unused-function -g"

cuik_source_files="src/main.c
	src/tls.c
	src/arena.c
	src/front/preproc.c
	src/front/lexer.c
	src/front/types.c
	src/front/atoms.c
	src/front/parser.c
	src/back/ir_gen.c
	src/back/linker.c
	src/ext/stb_ds.c"

gcc $c_settings $cuik_source_files tinybackend.a -lpthread -fPIC -Isrc -o build/cuik
