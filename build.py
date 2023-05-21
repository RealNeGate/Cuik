#!/usr/bin/env python3
import glob
import os
import platform
import subprocess
import argparse
from pathlib import Path

# the inspector uses Raylib, be sure to change this path if you're gonna use it
raylib_path = "C:/raylib"

parser = argparse.ArgumentParser(description='Compiles Cuik + friends')
parser.add_argument('-shared', action='store_true', help='build libCuik as shared object')
parser.add_argument('-opt', action='store_true', help='optimize output')
parser.add_argument('-driver', action='store_true', help='compile main driver')
parser.add_argument('-inspector', action='store_true', help='compile inspector')
parser.add_argument('-libcuik', action='store_true', help='compile libCuik')
parser.add_argument('-tb', action='store_true', help='compiles TB')
parser.add_argument('-asan', action='store_true', help='compile with ASAN')
parser.add_argument('-autospall', action='store_true', help='instrument code with SpallAuto')

args = parser.parse_args()
args.driver = True
# args.shared = True

mimalloc = True

if args.inspector or args.driver or args.shared:
	args.tb = True
	args.libcuik = True

# Figure out C and linker flags
ldflags = ""
cflags = "-g -msse4 -Wall -Werror -Wno-unused -Wno-deprecated-pragma -I common -DCUIK_ALLOW_THREADS -D_MT -U_DLL"

if mimalloc:       cflags += " -DTB_USE_MIMALLOC -DCUIK_USE_MIMALLOC -I mimalloc/include"
if args.libcuik:   cflags += " -I libCuik/include -DMINIZ_NO_MALLOC"
if args.tb:        cflags += " -DCUIK_USE_TB -I tb/include"
if args.asan:      cflags += " -fsanitize=address"
if args.opt:       cflags += " -O2 -DNDEBUG"
if args.shared:    cflags += " -DCUIK_DLL -DTB_DLL"
if args.autospall: cflags += " -DCUIK_USE_SPALL_AUTO -finstrument-functions-after-inlining"

if args.inspector:
	cflags += f" -I {raylib_path}/include"
	ldflags += f" {raylib_path}/lib/raylib.lib /nodefaultlib:libcmt"

system = platform.system()
if system == "Windows":
	if args.shared:
		ldflags += " -dll "

	if args.asan:
		ld = "clang -fuse-ld=lld-link -fsanitize=address"
		ldflags += " -o "
	else:
		ld = "lld-link"
		ldflags += " -debug onecore.lib msvcrt.lib libcmt.lib -out:"

	lib_ext = ".lib"
	exe_ext = ".exe"
	dll_ext = ".dll"
	cflags += " -I c11threads -D_CRT_SECURE_NO_WARNINGS"
elif system == "Darwin":
	ld = "lld.ld"
	ldflags += " -g -lc -o"

	exe_ext = ""
	lib_ext = ".a"
	dll_ext = ".so"
	cflags += " -std=gnu11 -I c11threads -Wno-deprecated-declarations"
else:
	ld = "ld.lld"
	ldflags += " -g -lc -lm -lthreads -o "
	exe_ext = ""
	lib_ext = ".a"
	dll_ext = ".so"

sources = []
sources.append("common/common.c")

if args.libcuik: sources.append("libCuik/lib/libcuik.c")
if mimalloc:     sources.append("mimalloc/src/static.c")

if system == "Windows":
	sources.append("c11threads/threads_msvc.c")
	if args.libcuik:
		sources.append("libCuik/lib/toolchains/msvc.c")
		sources.append("libCuik/lib/toolchains/gnu.c")
		sources.append("libCuik/lib/toolchains/darwin.c")
		sources.append("libCuik/lib/back/microsoft_craziness.c")

if system == "Darwin":
	sources.append("c11threads/threads_posix.c")

if args.tb:
	sources.append("tb/src/tb.c")
	# debug formats
	sources.append("tb/src/debug/cv/cv.c")
	sources.append("tb/src/debug/fut/fut.c")
	# architectures
	sources.append("tb/src/x64/x64.c")
	# objects
	sources.append("tb/src/objects/coff.c")
	sources.append("tb/src/objects/coff_parse.c")
	sources.append("tb/src/objects/elf64.c")
	sources.append("tb/src/objects/macho.c")
	# linker
	sources.append("tb/src/linker/pe.c")
	sources.append("tb/src/linker/elf.c")

if args.inspector:
	sources.append("inspector/main.c")

# main driver
if args.driver:
	sources.append("main/*.c")

# generate ninja file
objs = []
ninja = open('build.ninja', 'w')
ninja.write(f"""
cflags = {cflags}
ldflags = {ldflags}

rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = CC $out

rule meta_cc
  command = clang $in -o $out
  description = CC $in $out

rule lexgen
  command = ./lexgen{exe_ext}
  description = LEXGEN

rule embed_cc
  command = clang $in -c -o $out
  description = CC $out

""")

# the end of ldflags should define the out flag
if system == "Windows":
	ninja.write(f"""
rule embed_files
  command = $in
  description = EMBED

rule link
  command = {ld} $in $ldflags$out
  description = LINK $out

rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	ninja.write(f"""
rule embed_files
  command = ./$in
  description = EMBED

rule link
  command = {ld} $in $ldflags$out
  description = LINK $out

rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

# lexer generator metaprogram
if args.libcuik:
	ninja.write(f"build lexgen{exe_ext}: meta_cc libCuik/meta/lexgen.c\n")
	ninja.write(f"build libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h: lexgen lexgen{exe_ext} libCuik/meta/lexgen.c\n")

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
		ninja.write(f"build bin/{obj}: cc {f} | libCuik/lib/preproc/keywords.h libCuik/lib/preproc/dfa.h\n")
		objs.append("bin/"+obj)

# compile final executable (or library)
if args.driver or args.shared:
	ext = dll_ext if (args.shared) else exe_ext
	ninja.write(f"build cuik{ext}: link {' '.join(objs)}\n")
elif args.inspector:
	ninja.write(f"build inspector{exe_ext}: link {' '.join(objs)}\n")
elif args.libcuik:
	ninja.write(f"build libcuik{lib_ext}: lib {' '.join(objs)}\n")
elif args.tb:
	ninja.write(f"build tb{lib_ext}: lib {' '.join(objs)}\n")

ninja.close()

exit(subprocess.call(['ninja']))
