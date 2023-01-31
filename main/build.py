#!/usr/bin/env python3
import glob
import os
import sys
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles libCuik')
parser.add_argument('-opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('-asan', action='store_true', help='instrument code with the AddressSanitizer')
parser.add_argument('-autospall', action='store_true', help='instrument code with SpallAuto')
args = parser.parse_args()
args.opt = True

#######################################
# Handle dependencies
#######################################
tb_args = [sys.executable, 'build.py', '-mimalloc', 'x64', 'aarch64', 'wasm']
libcuik_args = [sys.executable, 'build.py', '-usetb', '-mimalloc']

if args.opt:
	tb_args.append('-opt')
	libcuik_args.append('-opt')

if args.asan:
	tb_args.append('-asan')
	libcuik_args.append('-asan')

if args.autospall:
	tb_args.append('-autospall')
	libcuik_args.append('-autospall')

subprocess.check_call(tb_args, shell=True, cwd="../tb")
subprocess.check_call(libcuik_args, shell=True, cwd="../libCuik")

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
cflags += " -I ../common/ -I ../libCuik/include -I ../tb/include"
cflags += " -DCUIK_USE_TB"
cflags += " -DCUIK_ALLOW_THREADS"

if args.asan:
	cflags += " -fsanitize=address"
	# TODO: my ASAN didn't work without me passing the lib directly, this should be commented out
	# once i figure that out
	if platform.system() == "Windows":
		ldflags += " \"C:/Program Files/LLVM/lib/clang/15.0.0/lib/windows/clang_rt.asan-x86_64.lib\""

if args.opt:
	cflags  += " -O2 -DNDEBUG"

if args.autospall:
	cflags += " -DCUIK_USE_SPALL_AUTO -finstrument-functions"

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
list = [ "src/main_driver.c" ]

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

ninja.write(f"build cuik{exe_ext}: link {' '.join(objs)} ../libCuik/libcuik{lib_ext} ../tb/tb{lib_ext}\n")
ninja.close()

exit(subprocess.call(['ninja']))