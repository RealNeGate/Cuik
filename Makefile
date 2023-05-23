# requires GNU make
LD      = lld-link
LDFLAGS = -dll -debug onecore.lib msvcrt.lib libcmt.lib

CC      = clang
CFLAGS  = -g -msse4 -I common \
	-Wall -Werror -Wno-unused -Wno-deprecated-pragma \
	-DTB_USE_MIMALLOC -DCUIK_USE_MIMALLOC -I mimalloc/include \
	-DCUIK_ALLOW_THREADS -I c11threads -D_CRT_SECURE_NO_WARNINGS

OBJS := bin/common.o bin/mimalloc.o bin/threads.o

EXE_EXT = .exe

ifdef DRIVER
	CUIK = 1
	TB   = 1
	OBJS += bin/main_driver.o
endif

ifdef CUIK
	CFLAGS += -I libCuik/include -DMINIZ_NO_MALLOC
	OBJS += bin/cuik.o bin/msvc.o bin/gnu.o bin/freestanding.o
endif

ifdef TB
	CFLAGS += -DCUIK_USE_TB -I tb/include
	OBJS += bin/tb.o bin/tb_x64.o
endif

all: cuik.exe

cuik.exe: $(OBJS)
	$(LD) $(LDFLAGS) $^ -out:$@

# we generate the lexer DFA via a metaprogram
bin/lexgen$(EXE_EXT): libCuik/meta/lexgen.c
	$(CC) $< -O1 -o $@
libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h: bin/lexgen$(EXE_EXT)
	bin/lexgen$(EXE_EXT)

# package freestanding headers into C file
bin/hexembed$(EXE_EXT): libCuik/meta/hexembed.c
	$(CC) $< -O1 -o $@
bin/freestanding.c: crt/include/*.h | bin/hexembed$(EXE_EXT)
	bin/hexembed$(EXE_EXT) $^
bin/freestanding.o: bin/freestanding.c
	$(CC) $(CFLAGS) $< -c -o $@

# compile source files
bin/threads.o: c11threads/threads_msvc.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/main_driver.o: main/main_driver.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/mimalloc.o: mimalloc/src/static.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/cuik.o: libCuik/lib/libcuik.c libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h
	$(CC) $(CFLAGS) $< -c -o $@
bin/msvc.o: libCuik/lib/toolchains/msvc.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/gnu.o: libCuik/lib/toolchains/gnu.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/darwin.o: libCuik/lib/toolchains/darwin.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/tb_x64.o: tb/src/x64/x64.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/common.o: common/common.c
	$(CC) $(CFLAGS) $< -c -o $@
bin/tb.o: tb/src/libtb.c
	$(CC) $(CFLAGS) $< -c -o $@

# just delete everything in the bin directory
clean:
	rm -f bin/*.o
	rm -f libCuik/lib/preproc/keywords.h
	rm -f libCuik/lib/preproc/dfa.h
	rm -f bin/freestanding.c
