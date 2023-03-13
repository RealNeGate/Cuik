import glob
import os
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles TB')
parser.add_argument('-usetb', action='store_true', help='compiles with the TB integration code')
parser.add_argument('-opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('-mimalloc', action='store_true', help='use mimalloc')
parser.add_argument('-shared', action='store_true', help='compile shared object')
parser.add_argument('-asan', action='store_true', help='compile with ASAN')
parser.add_argument('-autospall', action='store_true', help='instrument code with SpallAuto')

args = parser.parse_args()
args.mimalloc = False

source_patterns = ["lib/libcuik.c"]

ninja = open('build.ninja', 'w')
cflags = "-g -I ../common/ -I include -I lib -I deps -Wall -Werror -Wno-unused-function -Wno-unused-variable"
cflags += " -DCUIK_ALLOW_THREADS"

if args.mimalloc: cflags += " -DCUIK_USE_MIMALLOC -I ../mimalloc/include"
if args.shared:   cflags += " -DCUIK_USE_DLL"
if args.opt:      cflags += " -O2 -DNDEBUG"
if args.asan:     cflags += " -fsanitize=address"
if args.usetb:    cflags += " -I ../tb/include -DCUIK_USE_TB"

if args.autospall:
	cflags += " -finstrument-functions-after-inlining"

os_name = platform.system()
if os_name == "Windows":
	source_patterns.append("lib/toolchains/msvc.c")
	source_patterns.append("lib/back/microsoft_craziness.c")
	cflags += " -ferror-limit=100 -D_CRT_SECURE_NO_WARNINGS"
	cflags += " -I ../c11threads"
	exe_ext = ".exe"
elif os_name == "Darwin":
	source_patterns.append("lib/toolchains/darwin.c")
	cflags += " -I ../c11threads"
	exe_ext = ""
else:
	exe_ext = ""

# configure architecture-specific targeting
if platform.machine() == "AMD64":
	cflags += " -msse4.2"

# package freestanding headers into object file
freestanding_headers = ""
for f in glob.glob("../crt/include/*.h"):
	freestanding_headers += ' ' + f.replace('\\', '/')

# write some rules
ninja.write(f"""
cflags = {cflags}

rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = CC $in $out

rule lexgen
  command = cmd /c clang $in -o lg{exe_ext} && lg $out
  description = LEXGEN $in $out

rule headers
  command = cmd /c clang meta/hexembed.c -o headers{exe_ext} && headers $out {freestanding_headers}
  description = HEADERS $out

rule shared_obj
  command = clang $in $ldflags -g -o $out -shared
  description = LINK $out
""")

if os_name == "Windows":
	lib_ext = ".lib"
	so_ext = ".dll"
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	lib_ext = ".a"
	so_ext = ".so"
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

ninja.write(f"build freestanding.c: headers meta/hexembed.c {freestanding_headers}\n")
ninja.write(f"build bin/freestanding.o: cc freestanding.c\n")

# compile lexer DFA
ninja.write(f"build lib/preproc/dfa.h: lexgen meta/lexgen.c\n")

# compile libCuik
objs = []
objs.append("bin/freestanding.o")

for pattern in source_patterns:
	list = glob.glob(pattern)
	for f in list:
		obj = os.path.basename(f).replace('.c', '.o')
		ninja.write(f"build bin/{obj}: cc {f}\n")
		objs.append("bin/"+obj)

if args.shared:
	ninja.write(f"build libcuik{so_ext}: shared_obj {' '.join(objs)}\n")
else:
	ninja.write(f"build libcuik{lib_ext}: lib {' '.join(objs)}\n")

ninja.close()
exit(subprocess.call(['ninja']))
