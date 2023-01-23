#!/usr/bin/env python3
import glob
import os
import sys
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles libCuik')
parser.add_argument('--opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('--asan', action='store_true', help='instrument code with the AddressSanitizer')
args = parser.parse_args()

#######################################
# Handle dependencies
#######################################
tb_args = [sys.executable, 'build.py', 'x64', 'aarch64', 'wasm']
libcuik_args = [sys.executable, 'build.py', '--usetb']

if args.opt:
	tb_args.append('--opt')
	libcuik_args.append('--opt')

if args.asan:
	tb_args.append('--asan')
	libcuik_args.append('--asan')

subprocess.check_call(tb_args, shell=True, cwd="../tilde-backend")
subprocess.check_call(libcuik_args, shell=True, cwd="../libCuik")

#######################################
# link everything together
#######################################
system = platform.system()
ninja  = open('build.ninja', 'w')

if system == "Windows":
	ldflags = " -fuse-ld=lld-link"
	ldflags = " SDL2/lib/SDL2.lib SDL2/lib/SDL2main.lib SDL2/lib/SDL2_ttf.lib -lopengl32 -lshell32 -Xlinker /subsystem:console"
else:
	ldflags = " -fuse-ld=lld"

cflags = "-g -Wall -Werror -Wno-unused-function"
cflags += " -I ../common/ -I SDL2/include -I ../libCuik/include -I ../tilde-backend/include"
cflags += " -DCUIK_USE_TB "

if args.asan:
	cflags += " -fsanitize=address"

if args.opt:
	cflags  += " -O2 -DNDEBUG"

# windows' CRT doesn't support c11 threads so we provide a fallback
if system == "Windows":
	lib_ext = ".lib"
	exe_ext = ".exe"
	cflags += " -I ../c11threads -D_CRT_SECURE_NO_WARNINGS"
	# when we're not doing ASAN, we should be using mimalloc
	if False: # not args.asan:
		cflags += " -D_DLL"
		ldflags += " ../mimalloc/out/Release/mimalloc.lib -Xlinker /include:mi_version"
		ldflags += " -nodefaultlibs -lmsvcrt -lvcruntime -lucrt"
		subprocess.call(['build_mimalloc.bat'], cwd="..\\", shell=True)
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
list = glob.glob("src/*.c")

if system == "Windows":
	list.append("../c11threads/threads_msvc.c")
elif system == "Darwin":
	list.append("../c11threads/threads_posix.c")

for f in list:
	obj = os.path.basename(f).replace('.c', '.o')
	ninja.write(f"build bin/{obj}: cc {f}\n")
	objs.append("bin/"+obj)

ninja.write(f"build inspector{exe_ext}: link {' '.join(objs)} ../libCuik/libcuik{lib_ext} ../tilde-backend/tildebackend{lib_ext}\n")
ninja.close()

exit(subprocess.call(['ninja', '-j2']))
