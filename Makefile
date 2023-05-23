#####################################
# requires GNU make
#
# OPTIONS
#
#   OPT      do optimized builds
#
#   SHARED   generate cuik as a dynamic library
#
#   CUIK     compile libcuik
#
#   TB       compile TB
#
#   DRIVER   compile CLI driver (will use both CUIK and TB)
#
#####################################
CC = clang
LD = lld-link

OBJS    = bin/common.o bin/mimalloc.o
LDFLAGS = -debug onecore.lib msvcrt.lib libcmt.lib
CFLAGS  = -g -msse4 -I common -Wall -Werror -Wno-unused -Wno-deprecated-pragma \
          -DTB_USE_MIMALLOC -DCUIK_USE_MIMALLOC -I mimalloc/include -DCUIK_ALLOW_THREADS

ifdef OPT
	CFLAGS += -O2 -DNDEBUG
endif

ifeq ($(OS),Windows_NT)
	AR = lib
	EXE_EXT = .exe
	CFLAGS += -I c11threads -D_CRT_SECURE_NO_WARNINGS
	OBJS += bin/threads.o

	# figure out what the output is
	ifdef SHARED
		OUTPUT = cuik.dll
		LDFLAGS += /dll
	else
		# this is before the DRIVER enables TB or CUIK so we
		# use this to detect what the library should be
		OUTPUT = cuik.exe

		ifdef TB
			OUTPUT = tb.lib
		endif

		ifdef CUIK
			OUTPUT = cuik.lib
		endif
	endif
else
	AR = ar

	# figure out what the output is
	ifdef SHARED
		OUTPUT = cuik.so
		LDFLAGS += -shared
	else
		# this is before the DRIVER enables TB or CUIK so we
		# use this to detect what the library should be
		OUTPUT = cuik

		ifdef CUIK
			OUTPUT = cuik.a
		endif

		ifdef TB
			OUTPUT = tb.a
		endif
	endif
endif

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

.PHONY: all
all: $(OUTPUT)

# windows side of output
cuik.exe: $(OBJS)
	$(LD) $(LDFLAGS) $^ /out:$@
%.dll: $(OBJS)
	$(LD) /dll $(LDFLAGS) $^ /out:$@
%.lib: $(OBJS)
	$(AR) /nologo /out:$@ $^

# gnu side of output
cuik: $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $@
%.so: $(OBJS)
	$(LD) $(LDFLAGS) $^ -o $@
%.a: $(OBJS)
	$(AR) -rcs $@ $^

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
