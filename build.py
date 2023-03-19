#!/usr/bin/env python3
import glob
import os
import platform
import subprocess
import argparse
from pathlib import Path

parser = argparse.ArgumentParser(description='Compiles Cuik + friends')
parser.add_argument('-opt', action='store_true', help='optimize output')
parser.add_argument('-driver', action='store_true', help='compile main driver')
parser.add_argument('-libcuik', action='store_true', help='compile libCuik')
parser.add_argument('-tb', action='store_true', help='compiles TB')
parser.add_argument('-asan', action='store_true', help='compile with ASAN')
parser.add_argument('-autospall', action='store_true', help='instrument code with SpallAuto')

args = parser.parse_args()
mimalloc = True

if args.driver:
	args.tb = True
	args.libcuik = True

# Figure out C and linker flags
cflags = "-g -msse4 -Wall -Werror -Wno-unused -I common -DCUIK_ALLOW_THREADS"

if mimalloc:       cflags += " -DCUIK_USE_MIMALLOC -I mimalloc/include"
if args.libcuik:   cflags += " -I libCuik/include"
if args.tb:        cflags += " -DCUIK_USE_TB -I tb/include"
if args.asan:      cflags += " -fsanitize=address"
if args.opt:       cflags += " -O2 -DNDEBUG"
if args.autospall: cflags += " -DCUIK_USE_SPALL_AUTO -finstrument-functions-after-inlining"

system = platform.system()
if system == "Windows":
	ld = "lld-link"
	ldflags = "/debug /defaultlib:libcmt /defaultlib:oldnames"
	lib_ext = ".lib"
	exe_ext = ".exe"
	cflags += " -I c11threads -D_CRT_SECURE_NO_WARNINGS"
elif system == "Darwin":
	ld = "lld.ld"
	ld_flags = "-g -lc"
	exe_ext = ""
	lib_ext = ".a"
	cflags += " -I c11threads -Wno-deprecated-declarations"
else:
	ld = "ld.lld"
	ldflags = "-g -lc -lm -lthreads"
	exe_ext = ""
	lib_ext = ".a"

sources = []
sources.append("common/common.c")

if args.libcuik: sources.append("libCuik/lib/libcuik.c")
if mimalloc:     sources.append("mimalloc/src/static.c")

if system == "Windows":
	sources.append("c11threads/threads_msvc.c")
	if args.libcuik:
		sources.append("libCuik/lib/toolchains/msvc.c")
		sources.append("libCuik/lib/back/microsoft_craziness.c")

if system == "Darwin":
	sources.append("c11threads/threads_posix.c")

if args.tb:
	for path in Path("tb/src").rglob("*.c"):
		sources.append(str(path))

# main driver
if args.driver:
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
  command = clang $in -o $out
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

if system == "Windows":
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

# lexer generator metaprogram
if args.libcuik:
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

# compile final executable (or library)
if not args.driver and args.libcuik:
	ninja.write(f"build libcuik{lib_ext}: lib {' '.join(objs)}\n")
elif not args.driver and args.tb:
	ninja.write(f"build tb{lib_ext}: lib {' '.join(objs)}\n")
else:
	ninja.write(f"build cuik{exe_ext}: link {' '.join(objs)}\n")

ninja.close()

exit(subprocess.call(['ninja']))
