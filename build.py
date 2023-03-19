import glob
import os
import platform
import subprocess
from pathlib import Path

# Config
optimize = False
use_tb = True
use_libcuik = True
use_main_driver = True
use_asan = False
use_mimalloc = True

# Figure out C and linker flags
cflags = "-g -msse4 -Wall -Werror -Wno-unused -I common -DCUIK_ALLOW_THREADS"

if use_libcuik:  cflags += " -I libCuik/include"
if use_mimalloc: cflags += " -DCUIK_USE_MIMALLOC -I mimalloc/include"
if use_tb:       cflags += " -I tb/include -DCUIK_USE_TB"
if use_asan:     cflags += " -fsanitize=address"
if optimize:     cflags += " -O2 -DNDEBUG"

system = platform.system()
if system == "Windows":
	ld = "lld-link"
	ldflags = "-debug -defaultlib:libcmt -defaultlib:oldnames"
	lib_ext = ".lib"
	exe_ext = ".exe"
	cflags += " -I c11threads -D_CRT_SECURE_NO_WARNINGS"
elif system == "Darwin":
	ld = "lld.ld"
	exe_ext = "-g -lc"
	lib_ext = ".a"
	cflags += " -I c11threads -Wno-deprecated-declarations"
else:
	ld = "ld.lld"
	ldflags = "-g -lc -lm -lthreads"
	exe_ext = ""
	lib_ext = ".a"

sources = []
sources.append("common/common.c")

if use_libcuik:         sources.append("libCuik/lib/libcuik.c")
if use_mimalloc:        sources.append("mimalloc/src/static.c")

if system == "Windows":
	sources.append("c11threads/threads_msvc.c")
	sources.append("libCuik/lib/toolchains/msvc.c")
	sources.append("libCuik/lib/back/microsoft_craziness.c")

if system == "Darwin":
	sources.append("c11threads/threads_posix.c")

if use_tb:
	for path in Path("tb/src").rglob("*.c"):
		sources.append(str(path))

# main driver
if use_main_driver:
	sources.append("main/src/main_driver.c")

# generate ninja file
objs = []
ninja = open('build.ninja', 'w')
ninja.write(f"""
cflags = {cflags}
ldflags = {ldflags}

rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = CC $in $out

rule link
  command = {ld} $in $ldflags /out:$out
  description = LINK $out

rule meta_cc
  depfile = $out.d
  command = clang $in -MD -MF $out.d -o $out
  description = CC $in $out

rule lexgen
  command = lexgen{exe_ext} $out
  description = LEXGEN

rule embed_files
  command = $in
  description = EMBED

rule embed_cc
  command = clang $in -c -o $out
  description = CC $in $out

""")

# lexer generator metaprogram
ninja.write(f"build lexgen{exe_ext}: meta_cc libCuik/meta/lexgen.c\n")
ninja.write(f"build libCuik/lib/preproc/dfa.h: lexgen lexgen{exe_ext} libCuik/meta/lexgen.c\n")

# package freestanding headers into C file
freestanding_headers = ""
for f in glob.glob("crt/include/*.h"):
	freestanding_headers += ' ' + f.replace('\\', '/')

ninja.write(f"build hexembed{exe_ext}: meta_cc libCuik/meta/hexembed.c\n")
ninja.write(f"build libCuik/freestanding.c: embed_files hexembed{exe_ext} {freestanding_headers}\n")

ninja.write("build bin/freestanding.o: embed_cc libCuik/freestanding.c\n")
objs.append("bin/freestanding.o")

for pattern in sources:
	for f in glob.glob(pattern):
		f = f.replace('\\', '/')
		obj = os.path.basename(f).replace('.c', '.o')
		ninja.write(f"build bin/{obj}: cc {f}\n")
		objs.append("bin/"+obj)

ninja.write(f"build cuik{exe_ext}: link {' '.join(objs)}\n")
ninja.close()

exit(subprocess.call(['ninja']))
