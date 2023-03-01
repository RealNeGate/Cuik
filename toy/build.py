#!/usr/bin/env python3
import glob
import os
import sys
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles toy')
parser.add_argument('-opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('-asan', action='store_true', help='instrument code with the AddressSanitizer')
args = parser.parse_args()

#######################################
# Handle dependencies
#######################################
tb_args = [sys.executable, 'build.py', '-mimalloc']

if args.opt:
	tb_args.append('-opt')

if args.asan:
	tb_args.append('-asan')

subprocess.check_call(tb_args, shell=True, cwd="../tb")

#######################################
# link everything together
#######################################
system = platform.system()
ninja  = open('build.ninja', 'w')

if system == "Windows":
	ldflags = " -fuse-ld=lld-link"
else:
	ldflags = " -fuse-ld=lld"

cflags = "-g -Wall -Werror -Wno-unused-function"
cflags += " -I ../common/ -I ../tb/include"
cflags += " -DCUIK_ALLOW_THREADS"

if args.opt:
	cflags  += " -O2 -DNDEBUG"

# windows' CRT doesn't support c11 threads so we provide a fallback
if system == "Windows":
	lib_ext = ".lib"
	exe_ext = ".exe"
	cflags += " -I ../c11threads -D_CRT_SECURE_NO_WARNINGS"
elif system == "Darwin":
	exe_ext = ""
	lib_ext = ".a"
	cflags += " -I ../c11threads"
	cflags += " -Wno-deprecated-declarations"
else:
	exe_ext = ""
	lib_ext = ".a"

# write some rules
ninja.write(f"""
cflags = {cflags}
ldflags = {ldflags}

rule mimalloc
  depfile = $out.d
  command = clang $in -I ../mimalloc/include -DMI_SECURE=1 -g -O2 -MD -MF $out.d -c -o $out
  description = MIMALLOC $in $out

rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = CC $in $out

rule link
  command = clang $in $ldflags -g -o $out
  description = LINK $out

""")

# compile source files
objs = []
list = [ "src/main.c" ]

# support code (we might wanna merge it with the threads stuff)
list.append("../common/common.c")

if system == "Windows":
	list.append("../c11threads/threads_msvc.c")
elif system == "Darwin":
	list.append("../c11threads/threads_posix.c")

for f in list:
	obj = os.path.basename(f).replace('.c', '.o')
	ninja.write(f"build bin/{obj}: cc {f}\n")
	objs.append("bin/"+obj)

ninja.write(f"build bin/mimalloc.o: mimalloc ../mimalloc/src/static.c\n")
objs.append("bin/mimalloc.o")

ninja.write(f"build toy{exe_ext}: link {' '.join(objs)} ../tb/tb{lib_ext}\n")
ninja.close()

exit(subprocess.call(['ninja']))
