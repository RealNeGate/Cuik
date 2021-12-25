c_settings="-march=haswell -maes -std=gnu11 -DCUIK_NEEDS_SAFE_FUNCTIONS -O2 -DNDEBUG -Werror -Wall -Wno-unused-function -g"

cuik_source_files="src/main.c
	src/preproc.c
	src/lexer.c
	src/tls.c
	src/types.c
	src/atoms.c
	src/parser.c
	src/ir_gen.c
	src/linker.c
	src/tree_printer.c"

gcc $c_settings $cuik_source_files tinybackend.a -lpthread -fPIC -o build/cuik
